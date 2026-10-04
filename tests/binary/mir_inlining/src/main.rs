#![feature(allocator_ext, core_intrinsics, ptr_metadata)]
#![allow(internal_features)]

use std::{hint::black_box, ptr::Pointee, sync::Arc};

// Keep PtrMetadata MIR generic even when inlining is disabled.
#[inline(never)]
fn intrinsic_metadata<T: ?Sized>(pointer: *const T) -> <T as Pointee>::Metadata {
    std::intrinsics::ptr_metadata(pointer)
}

#[inline(never)]
fn wrapper_metadata<T: ?Sized>(pointer: *const T) -> <T as Pointee>::Metadata {
    std::ptr::metadata(pointer)
}

fn metadata() {
    let slice: &[u32] = black_box(&[10, 20, 30]);
    let text = black_box("café");
    assert_eq!(intrinsic_metadata(slice), 3);
    assert_eq!(wrapper_metadata(slice), 3);
    assert_eq!(intrinsic_metadata(text), 5);
    assert_eq!(wrapper_metadata(text), 5);
    assert_eq!(intrinsic_metadata(&[1_u8; 3]), ());

    struct Tail<T: ?Sized> {
        _head: u64,
        tail: T,
    }
    let value = Tail {
        _head: 1,
        tail: [2_u32, 3, 4],
    };
    let tail: &Tail<[u32]> = &value;
    assert_eq!(intrinsic_metadata(tail), 3);
    assert_eq!(wrapper_metadata(tail), 3);
    assert_eq!(&tail.tail, &[2, 3, 4]);
    let nested: Arc<Tail<[u32]>> = Arc::new(Tail {
        _head: 1,
        tail: [5, 6, 7],
    });
    assert_eq!(&nested.tail, &[5, 6, 7]);

    #[derive(Debug)]
    #[repr(align(32))]
    struct Aligned(#[allow(dead_code)] [u8; 47]);
    let value = black_box(Aligned([0; 47]));
    let object: &dyn std::fmt::Debug = &value;
    let direct_meta = std::ptr::metadata(black_box(object));
    assert_eq!(direct_meta.size_of(), 64);
    assert_eq!(direct_meta.align_of(), 32);
    let meta = intrinsic_metadata(object);
    assert_eq!(meta.size_of(), 64);
    assert_eq!(meta.align_of(), 32);
    assert_eq!(wrapper_metadata(object), meta);
    let value = Tail {
        _head: 1,
        tail: value,
    };
    let tail: &Tail<dyn std::fmt::Debug> = &value;
    let meta = intrinsic_metadata(tail);
    assert_eq!(meta.size_of(), 64);
    assert_eq!(meta.align_of(), 32);
    assert_eq!(wrapper_metadata(tail), meta);

    let raw = std::ptr::slice_from_raw_parts(std::ptr::without_provenance::<u32>(0x100), 3);
    assert_eq!(format!("{raw:?}"), "Pointer { addr: 0x100, metadata: 3 }");

    // Raw pointer metadata can exceed JVM array limits without accessing storage.
    let length = black_box((1_usize << 33) + 7);
    let raw = std::ptr::from_raw_parts::<str>(std::ptr::without_provenance::<()>(0x100), length);
    assert_eq!(intrinsic_metadata(raw), length);
}

fn unsized_cloning() {
    let mut a: Arc<[i32]> = Arc::new([10, 20, 30]);
    let b = a.clone();
    Arc::make_mut(&mut a)[1] = 99;
    assert_eq!(&*a, &[10, 99, 30]);
    assert_eq!(&*b, &[10, 20, 30]);
    let zeroed = Arc::<[usize], _>::new_zeroed_slice_in(5, std::alloc::System);
    let initialized = unsafe { zeroed.assume_init() };
    assert_eq!(&*initialized, &[0; 5]);

    let mut text: Arc<str> = Arc::from("abc");
    let old = text.clone();
    Arc::make_mut(&mut text).make_ascii_uppercase();
    assert_eq!(&*text, "ABC");
    assert_eq!(&*old, "abc");
}

fn non_null_trait_reference() {
    trait Value {
        fn get(&self) -> i32;
        fn set(&mut self, value: i32);
    }
    impl Value for i32 {
        fn get(&self) -> i32 {
            *self
        }
        fn set(&mut self, value: i32) {
            *self = value;
        }
    }
    let mut value = 42_i32;
    let mut pointer = std::ptr::NonNull::from(&mut value as &mut dyn Value);
    unsafe {
        assert_eq!(pointer.as_ref().get(), 42);
        pointer.as_mut().set(43);
        assert_eq!(pointer.as_ref().get(), 43);
    }
    assert_eq!(value, 43);
    let metadata = std::ptr::metadata(pointer.as_ptr());
    let mut other = 70_i32;
    let raw = std::ptr::from_raw_parts_mut::<dyn Value>(
        std::ptr::from_mut(&mut other).cast::<()>(),
        metadata,
    );
    unsafe {
        assert_eq!((&*raw).get(), 70);
        (&mut *raw).set(71);
    }
    assert_eq!(other, 71);
}

fn custom_error() {
    let error = std::io::Error::new(std::io::ErrorKind::Other, "example error");
    assert_eq!(format!("{error}"), "example error");
    assert!(format!("{error:?}").contains("example error"));
    assert_eq!(error.get_ref().unwrap().to_string(), "example error");
}

#[inline(never)]
fn trait_tail_reconstruction() {
    #[repr(C)]
    struct Record<T: ?Sized> {
        header: usize,
        value: T,
    }
    let original = Record {
        header: 17,
        value: 23_i64,
    };
    let reference: &Record<dyn std::fmt::Display> = &original;
    let metadata = std::ptr::metadata(reference);
    let other = Record {
        header: 31,
        value: 47_i64,
    };
    let rebuilt = std::ptr::from_raw_parts::<Record<dyn std::fmt::Display>>(
        std::ptr::from_ref(&other).cast::<()>(),
        metadata,
    );
    unsafe {
        assert_eq!((*rebuilt).header, 31);
        assert_eq!(format!("{}", &(*rebuilt).value), "47");
        assert_eq!(
            std::mem::size_of_val(&*rebuilt),
            std::mem::size_of_val(&other)
        );
    }
    let arc: Arc<dyn std::fmt::Display> = Arc::new(53_i64);
    let raw = Arc::into_raw(arc.clone());
    let restored = unsafe { Arc::from_raw(raw) };
    assert_eq!(restored.to_string(), "53");
    drop(restored);
    assert_eq!(arc.to_string(), "53");
}

#[inline(never)]
fn cloned_closure() {
    let text = String::from("captured");
    let closure = move || text.len();
    let cloned = closure.clone();
    assert!(closure() == 8);
    assert!(cloned() == 8);
}

#[inline(never)]
fn path_ancestors() {
    let path = std::path::Path::new(black_box("alpha/beta/gamma"));
    let parent = path.parent().unwrap();
    let expected = std::path::Path::new("alpha/beta");
    assert_eq!(parent.as_os_str().len(), 10);
    assert_eq!(parent, expected);
    let lengths: Vec<_> = path
        .ancestors()
        .take(8)
        .map(|p| p.as_os_str().len())
        .collect();
    assert_eq!(lengths, [16, 10, 5, 0]);
}

fn main() {
    metadata();
    unsized_cloning();
    non_null_trait_reference();
    custom_error();
    trait_tail_reconstruction();
    cloned_closure();
    path_ancestors();
}
