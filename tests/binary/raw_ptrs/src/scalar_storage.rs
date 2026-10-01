//! Escaping scalar locations retain their value and identity across boundaries.
use std::hint::black_box;

#[inline(never)]
fn replace<T>(place: &mut T, value: T) -> T {
    std::mem::replace(black_box(place), value)
}

#[inline(never)]
fn parameter(mut value: u64) -> u64 {
    assert_eq!(
        replace(&mut value, 0xfedc_ba98_7654_3210),
        0x1234_5678_9abc_def0
    );
    value
}

#[inline(never)]
fn identity(value: &mut u32) -> &mut u32 {
    value
}

pub fn run() {
    super::borrowed_slots::run();
    replaced_fields();
    inline_arrays();
    assert_eq!(parameter(0x1234_5678_9abc_def0), 0xfedc_ba98_7654_3210);
    let mut narrow = -123_i8;
    assert_eq!(replace(&mut narrow, 87), -123);
    assert_eq!(narrow, 87);
    let mut unsigned = 61_234_u16;
    assert_eq!(replace(&mut unsigned, 54_321), 61_234);
    assert_eq!(unsigned, 54_321);
    let mut half = 1.5_f16;
    assert_eq!(replace(&mut half, -2.25).to_bits(), 1.5_f16.to_bits());
    assert_eq!(half.to_bits(), (-2.25_f16).to_bits());
    let mut float = f64::from_bits(0x7ff8_0000_0000_1234);
    assert_eq!(replace(&mut float, -0.0).to_bits(), 0x7ff8_0000_0000_1234);
    assert_eq!(float.to_bits(), (-0.0_f64).to_bits());
    let mut flag = true;
    assert!(replace(&mut flag, false));
    assert!(!flag);

    let mut value = 0x1122_3344_u32;
    let pointer = black_box(identity(&mut value) as *mut u32);
    let address = pointer.expose_provenance();
    let mut update = || unsafe { *pointer = 0x5566_7788 };
    black_box(&mut update)();
    let rebuilt = std::ptr::with_exposed_provenance_mut::<u32>(address);
    unsafe {
        assert_eq!(*rebuilt, 0x5566_7788);
        *rebuilt.cast::<u8>().add(1) = 0xaa;
    }
    assert_eq!(value, 0x5566_aa88);
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        *identity(&mut value) = 42;
        black_box(false).then_some(()).unwrap();
    }));
    std::panic::set_hook(hook);
    assert!(outcome.is_err());
    assert_eq!(value, 42);
}

#[inline(never)]
fn field_read(value: &u32) -> u32 {
    *std::hint::black_box(value)
}

#[inline(never)]
fn replaced_fields() {
    #[repr(C)]
    struct Pair {
        first: u32,
        second: u32,
    }
    let mut value = Pair {
        first: 3,
        second: 5,
    };
    let root = &raw mut value;
    let field = unsafe { &raw mut (*root).second };
    assert_eq!(field_read(unsafe { &*field }), 5);
    unsafe {
        std::ptr::write(
            root,
            Pair {
                first: 7,
                second: 11,
            },
        );
    }
    assert_eq!(field_read(unsafe { &*field }), 11);
    unsafe {
        replace(&mut *field, 13);
    }
    assert_eq!(value.first, 7);
    assert_eq!(value.second, 13);
    // Materializing an exposed address later must retain the original root.
    let address = field.expose_provenance();
    unsafe {
        *std::ptr::with_exposed_provenance_mut::<u32>(address) = 17;
    }
    assert_eq!(field_read(unsafe { &*field }), 17);
}

#[inline(never)]
fn inline_arrays() {
    #[repr(C)]
    struct Arrays {
        words: [u32; 3],
        floats: [f32; 2],
    }
    let mut value = Arrays {
        words: [3, 5, 7],
        floats: [-0.0, f32::from_bits(0x7fc01234)],
    };
    let root = &raw mut value;
    let word = unsafe { &raw mut (*root).words[1] };
    unsafe {
        word.cast::<u8>().add(1).write(0xab);
    }
    assert_eq!(value.words[1], 0xab05);
    unsafe {
        (*root).words = [11, 13, 17];
    }
    assert_eq!(unsafe { word.read() }, 13);
    unsafe {
        *word = 19;
    }
    assert_eq!(value.words[1], 19);
    assert_eq!(value.floats[0].to_bits(), 0x80000000);
    assert_eq!(value.floats[1].to_bits(), 0x7fc01234);
    let address = word.expose_provenance();
    unsafe {
        std::ptr::with_exposed_provenance_mut::<u32>(address).write(23);
    }
    assert_eq!(value.words[1], 23);
}
