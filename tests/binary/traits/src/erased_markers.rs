use std::any::{Any, TypeId};
use std::hint::black_box;
use std::marker::PhantomData;

trait Marker {
    fn identity(&self) -> TypeId;
}
impl<T: 'static> Marker for PhantomData<T> {
    fn identity(&self) -> TypeId { TypeId::of::<T>() }
}

#[derive(Clone, Copy)]
struct Tagged<T> {
    value: u64,
    marker: PhantomData<T>,
}

#[inline(never)]
fn value<T>(tagged: Tagged<T>, _: PhantomData<T>) -> u64 { tagged.value }

#[inline(never)]
fn clone_marker<T>(value: &PhantomData<T>) -> PhantomData<T> { value.clone() }

pub fn run() {
    let byte = black_box(PhantomData::<u8>);
    assert_eq!(clone_marker(&byte), byte);
    let word = black_box(PhantomData::<u64>);
    let markers: [&dyn Marker; 2] = [&byte, &word];
    assert_eq!(markers[0].identity(), TypeId::of::<u8>());
    assert_eq!(markers[1].identity(), TypeId::of::<u64>());
    let any: &dyn Any = &byte;
    assert!(any.is::<PhantomData<u8>>());
    assert!(!any.is::<PhantomData<u64>>());
    assert!(any.downcast_ref::<PhantomData<u8>>().is_some());
    let tagged = Tagged { value: 91, marker: byte };
    assert_eq!(value(tagged, byte), 91);
    let boxed: Box<dyn Any> = Box::new(word);
    assert!(boxed.downcast::<PhantomData<u64>>().is_ok());
    let many = vec![byte; 16384];
    assert_eq!(many.len(), 16384);
    assert_eq!(many.iter().count(), 16384);
    assert_eq!(std::mem::size_of::<Tagged<u8>>(), 8);
}
