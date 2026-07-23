use std::any::Any;
use std::mem::ManuallyDrop;
use std::panic::{AssertUnwindSafe, catch_unwind, panic_any, resume_unwind};

#[repr(C)]
struct FormattingFields {
    prefix: [u64; 11],
    value: usize,
    suffix: [u64; 12],
}

unsafe extern "C" {
    #[link_name = "jvm:static:java/lang/Math:addExact:(II)I"]
    fn java_add_exact(left: i32, right: i32) -> i32;
}

fn move_plain_panic_payload(payload: Box<dyn Any>) -> Box<dyn Any> {
    ManuallyDrop::into_inner(ManuallyDrop::new(payload))
}

fn main() {
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

    let hook = std::panic::take_hook();
    std::panic::set_hook(hook);
}
