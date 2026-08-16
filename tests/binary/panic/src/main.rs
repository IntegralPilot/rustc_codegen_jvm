use std::any::Any;
use std::cell::Cell;
use std::mem::ManuallyDrop;
use std::panic::{AssertUnwindSafe, catch_unwind, panic_any, resume_unwind};
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};

static PANICKING_DROP_RAN: AtomicBool = AtomicBool::new(false);
static FIELD_AFTER_PANIC_DROPPED: AtomicBool = AtomicBool::new(false);

struct PanicsOnDrop;

impl Drop for PanicsOnDrop {
    fn drop(&mut self) {
        PANICKING_DROP_RAN.store(true, Ordering::SeqCst);
        panic!("single panic from Drop");
    }
}

struct RecordsDrop;

impl Drop for RecordsDrop {
    fn drop(&mut self) {
        FIELD_AFTER_PANIC_DROPPED.store(true, Ordering::SeqCst);
    }
}

struct PanickingAggregate(RecordsDrop);

impl Drop for PanickingAggregate {
    fn drop(&mut self) {
        PANICKING_DROP_RAN.store(true, Ordering::SeqCst);
        panic!("single panic from Drop");
    }
}

#[repr(C)]
struct FormattingFields {
    prefix: [u64; 11],
    value: usize,
    suffix: [u64; 12],
}

unsafe extern "C" {
    #[link_name = "jvm:static:java/lang/Math:addExact"]
    fn java_add_exact(left: i32, right: i32) -> i32;
}

fn move_plain_panic_payload(payload: Box<dyn Any>) -> Box<dyn Any> {
    ManuallyDrop::into_inner(ManuallyDrop::new(payload))
}

fn panicking_vec_clone_cleanup(panic_on_clone: usize) -> (u32, u32) {
    struct PanickingClone<'count> {
        drop_count: &'count AtomicU32,
        ordinary_drop_count: &'count Cell<u32>,
        panic_on_clone: bool,
    }

    impl Clone for PanickingClone<'_> {
        fn clone(&self) -> Self {
            if self.panic_on_clone {
                panic!("panic while cloning a vector element");
            }
            Self { ..*self }
        }
    }

    impl Drop for PanickingClone<'_> {
        fn drop(&mut self) {
            self.drop_count.fetch_add(1, Ordering::SeqCst);
            self.ordinary_drop_count
                .set(self.ordinary_drop_count.get() + 1);
        }
    }

    let drop_count = AtomicU32::new(0);
    let ordinary_drop_count = Cell::new(0);
    let mut values = (0..3)
        .map(|index| PanickingClone {
            drop_count: &drop_count,
            ordinary_drop_count: &ordinary_drop_count,
            panic_on_clone: index == panic_on_clone,
        })
        .collect::<Vec<_>>();

    catch_unwind(AssertUnwindSafe(move || values.extend_from_within(..)))
        .expect_err("the selected element clone must panic");
    (
        ordinary_drop_count.get(),
        drop_count.load(Ordering::SeqCst),
    )
}

fn concurrent_panicking_vec_clone_cleanup() {
    let previous_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let results = std::thread::scope(|scope| {
        let handles = (0..8)
            .map(|_| {
                scope.spawn(|| {
                    let mut mismatch = None;
                    for _ in 0..16 {
                        for panic_on_clone in 0..3 {
                            let counts = panicking_vec_clone_cleanup(panic_on_clone);
                            let expected = 3 + panic_on_clone as u32;
                            if counts != (expected, expected) {
                                mismatch = Some((panic_on_clone, counts, expected));
                                break;
                            }
                        }
                        if mismatch.is_some() {
                            break;
                        }
                    }
                    mismatch
                })
            })
            .collect::<Vec<_>>();
        handles
            .into_iter()
            .map(|handle| handle.join())
            .collect::<Vec<_>>()
    });
    std::panic::set_hook(previous_hook);

    for result in results {
        let mismatch = result.expect("panic cleanup worker must complete");
        if let Some((panic_on_clone, (ordinary, atomic), expected)) = mismatch {
            panic!(
                "Vec clone panicking at index {panic_on_clone} dropped {ordinary} ordinary and \
                 {atomic} atomic values; expected {expected}"
            );
        }
    }
}

fn main() {
    concurrent_panicking_vec_clone_cleanup();

    let plain_payload = move_plain_panic_payload(Box::new(73_u32));
    assert_eq!(plain_payload.downcast_ref::<u32>(), Some(&73));

    let fields = FormattingFields {
        prefix: [0; 11],
        value: 2809,
        suffix: [0; 12],
    };
    assert_eq!(format!("{}", fields.value), "2809");

    let literal = catch_unwind(|| panic!("literal payload"))
        .expect_err("literal panic should unwind");
    assert_eq!(
        literal.downcast_ref::<&'static str>().copied(),
        Some("literal payload")
    );

    let formatted = catch_unwind(|| {
        let value = 999_u64;
        panic!("This is a formatted panic message: {}", value);
    })
    .expect_err("panic! must unwind into catch_unwind");
    assert_eq!(
        formatted.downcast_ref::<String>().map(String::as_str),
        Some("This is a formatted panic message: 999")
    );

    let typed = catch_unwind(|| panic_any(1234_u32))
        .expect_err("panic_any must preserve its typed payload");
    assert_eq!(typed.downcast_ref::<u32>(), Some(&1234));

    let resumed = catch_unwind(AssertUnwindSafe(|| resume_unwind(typed)))
        .expect_err("resume_unwind must be caught by the next unwind boundary");
    assert_eq!(resumed.downcast_ref::<u32>(), Some(&1234));

    let unit = catch_unwind(AssertUnwindSafe(|| resume_unwind(Box::new(()))))
        .expect_err("a zero-sized panic payload must unwind");
    assert_eq!(unit.downcast_ref::<()>(), Some(&()));

    let foreign = catch_unwind(|| unsafe { java_add_exact(i32::MAX, 1) })
        .expect_err("a foreign JVM exception must unwind into catch_unwind");
    let message = foreign
        .downcast_ref::<String>()
        .expect("foreign JVM failures should carry a diagnostic String");
    assert!(message.contains("java.lang.ArithmeticException"));
    assert!(message.contains("integer overflow"));
    assert_eq!(unsafe { java_add_exact(20, 22) }, 42);
    let add_exact: unsafe extern "C" fn(i32, i32) -> i32 = java_add_exact;
    assert_eq!(unsafe { add_exact(19, 23) }, 42);

    let drop_panic = catch_unwind(|| {
        let _value = PanicsOnDrop;
    })
    .expect_err("a single panic during normal Drop must remain catchable");
    assert!(PANICKING_DROP_RAN.load(Ordering::SeqCst));
    assert_eq!(
        drop_panic.downcast_ref::<&'static str>().copied(),
        Some("single panic from Drop")
    );

    PANICKING_DROP_RAN.store(false, Ordering::SeqCst);
    FIELD_AFTER_PANIC_DROPPED.store(false, Ordering::SeqCst);
    let aggregate_drop_panic = catch_unwind(|| {
        drop(PanickingAggregate(RecordsDrop));
    })
    .expect_err("an aggregate field panic must unwind after later fields are dropped");
    assert_eq!(
        aggregate_drop_panic
            .downcast_ref::<&'static str>()
            .copied(),
        Some("single panic from Drop")
    );
    assert!(PANICKING_DROP_RAN.load(Ordering::SeqCst));
    assert!(FIELD_AFTER_PANIC_DROPPED.load(Ordering::SeqCst));

    let hook = std::panic::take_hook();
    std::panic::set_hook(hook);
}
