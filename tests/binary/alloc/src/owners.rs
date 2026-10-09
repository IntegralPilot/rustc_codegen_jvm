use std::alloc::{AllocError, Allocator, Global, Layout};
use std::marker::PhantomPinned;
use std::pin::Pin;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};

static DROPS: AtomicUsize = AtomicUsize::new(0);
static DEALLOCATIONS: AtomicUsize = AtomicUsize::new(0);

struct Owned(Box<u32>);

struct Container<T>(T);

impl<T> Drop for Container<T> {
    fn drop(&mut self) {
        DROPS.fetch_add(1000, SeqCst);
    }
}

#[derive(Clone, Copy)]
struct Distance(f64);

impl PartialEq for Distance {
    #[inline(never)]
    fn eq(&self, other: &Self) -> bool {
        (self.0 - other.0).abs() < 0.5
    }
}

impl Drop for Owned {
    fn drop(&mut self) {
        DROPS.fetch_add(*self.0 as usize, SeqCst);
    }
}

// Preserve user-defined equality when a value is stored as an address.
#[derive(Clone, Copy)]
struct Compared(*const u32);

impl PartialEq for Compared {
    #[inline(never)]
    fn eq(&self, other: &Self) -> bool {
        unsafe { *self.0 % 10 == *other.0 % 10 }
    }
}

#[repr(C)]
struct WithMarker {
    pointer: *const u32,
    marker: (),
}

struct AllocatorWithDrop;

impl Drop for AllocatorWithDrop {
    fn drop(&mut self) {
        DROPS.fetch_add(100, SeqCst);
    }
}

unsafe impl Allocator for AllocatorWithDrop {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        Global.allocate(layout)
    }

    unsafe fn deallocate(&self, pointer: NonNull<u8>, layout: Layout) {
        DEALLOCATIONS.fetch_add(1, SeqCst);
        unsafe { Global.deallocate(pointer, layout) }
    }
}

struct Pinned {
    address: *const Pinned,
    _marker: PhantomPinned,
}

#[inline(never)]
fn pass_pin(value: Pin<Box<Pinned>>) -> Pin<Box<Pinned>> {
    value
}

#[inline(never)]
fn pass_owner(value: Owned) -> Owned {
    value
}

#[inline(never)]
fn pass_option(value: Option<Box<[u32]>>) -> Option<Box<[u32]>> {
    value
}

#[inline(never)]
fn marker_address(value: &WithMarker) -> usize {
    &raw const value.marker as usize
}

pub fn run() {
    // Retain slice length while initializing the Rc or Arc allocation.
    #[derive(Debug, PartialEq)]
    struct Pair(u64, u64);
    let shared: std::rc::Rc<[Pair]> = (0..4).map(|i| Pair(i, i + 10)).collect();
    let atomic: std::sync::Arc<[Pair]> = (0..4).map(|i| Pair(i + 20, i + 30)).collect();
    assert_eq!(shared.len(), 4);
    assert_eq!(shared[3], Pair(3, 13));
    assert_eq!(atomic.len(), 4);
    assert_eq!(atomic[2], Pair(22, 32));
    let empty: std::sync::Arc<[Pair]> = std::iter::empty().collect();
    assert_eq!(empty.len(), 0);

    let start = DROPS.load(SeqCst);
    let owner = pass_owner(Owned(Box::new(7)));
    assert_eq!(*owner.0, 7);
    drop(owner);
    assert_eq!(DROPS.load(SeqCst), start + 7);

    assert!(pass_option(None).is_none());
    let optional = pass_option(Some(Box::new([4, 5]))).unwrap();
    assert_eq!(&*optional, &[4, 5]);
    let mut nested: Option<Option<Box<u32>>> = Some(None);
    assert!(nested.as_ref().unwrap().is_none());
    nested = Some(Some(Box::new(18)));
    assert_eq!(**nested.as_ref().unwrap().as_ref().unwrap(), 18);

    let left = 12;
    let right = 22;
    let different = 23;
    assert!(Compared(&left) == Compared(&right));
    assert!(Compared(&left) != Compared(&different));
    assert!([Compared(&left)].starts_with(&[Compared(&right)]));

    let marker = WithMarker {
        pointer: &left,
        marker: (),
    };
    assert_eq!(
        marker_address(&marker) - &raw const marker as usize,
        size_of::<usize>()
    );
    let WithMarker {
        pointer,
        marker: (),
    } = marker;
    assert_eq!(unsafe { *pointer }, left);

    let deallocations = DEALLOCATIONS.load(SeqCst);
    let custom = Box::new_in(33u64, AllocatorWithDrop);
    assert_eq!(*custom, 33);
    drop(custom);
    assert_eq!(DROPS.load(SeqCst), start + 107);
    assert_eq!(DEALLOCATIONS.load(SeqCst), deallocations + 1);

    let mut pinned = Box::pin(Pinned {
        address: std::ptr::null(),
        _marker: PhantomPinned,
    });
    let address = &*pinned as *const Pinned;
    unsafe {
        pinned.as_mut().get_unchecked_mut().address = address;
    }
    let pinned = pass_pin(pinned);
    assert_eq!(pinned.address, &*pinned as *const Pinned);

    let strong: std::rc::Rc<str> = std::rc::Rc::from("owner");
    let weak = std::rc::Rc::downgrade(&strong);
    let copy = strong.clone();
    assert_eq!(std::rc::Rc::strong_count(&strong), 2);
    assert_eq!(&*weak.upgrade().unwrap(), "owner");
    drop(copy);
    drop(strong);
    assert!(weak.upgrade().is_none());

    let mut strong: std::sync::Arc<[u32]> = std::sync::Arc::from([1, 2, 3]);
    let copy = strong.clone();
    std::sync::Arc::make_mut(&mut strong)[1] = 9;
    assert_eq!(&*strong, &[1, 9, 3]);
    assert_eq!(&*copy, &[1, 2, 3]);

    let before = DROPS.load(SeqCst);
    let mut container = Container(vec![1, 2, 3]);
    container.0.push(4);
    assert_eq!(&container.0, &[1, 2, 3, 4]);
    let erased: &dyn std::any::Any = &container;
    assert!(erased.is::<Container<Vec<i32>>>());
    assert!(!erased.is::<Vec<i32>>());
    drop(container);
    assert_eq!(DROPS.load(SeqCst), before + 1000);

    assert!(Distance(1.0) == Distance(1.25));
    assert!([Distance(1.0)].starts_with(&[Distance(1.25)]));
    let mut distance = Box::new(Distance(3.0));
    distance.0 += 0.25;
    assert_eq!(distance.0, 3.25);
}
