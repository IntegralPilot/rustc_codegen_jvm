use std::hint::black_box;
use std::ptr::NonNull;

#[derive(Debug, PartialEq)]
struct Item {
    value: i32,
    extra: i32,
}
static VALUE: Item = Item {
    value: 17,
    extra: 19,
};
static PRESENT: Option<&Item> = Some(&VALUE);
static ABSENT: Option<&Item> = None;

#[inline(never)]
fn choose<T>(value: &T, present: bool) -> Option<&T> {
    if present { Some(value) } else { None }
}

#[inline(never)]
fn replace<'a>(slot: &mut Option<&'a Item>, new: &'a Item) {
    if let Some(payload) = slot {
        *payload = new;
    }
}

pub fn run() {
    constructor_items();
    let item = black_box(Item {
        value: 23,
        extra: 29,
    });
    let other = black_box(Item {
        value: 23,
        extra: 29,
    });
    assert_eq!(black_box(Some(&item)), black_box(Some(&other)));
    assert_ne!(black_box(Some(&item)), black_box(Some(&VALUE)));
    assert_eq!(black_box(None::<&Item>), black_box(None::<&Item>));
    assert_ne!(black_box(Some(&item)), black_box(None::<&Item>));
    assert!(black_box(Some(&7i32)) < black_box(Some(&11i32)));
    assert!(black_box(None::<&i32>) < black_box(Some(&7i32)));
    assert_eq!(PRESENT.unwrap().value, 17);
    assert!(ABSENT.is_none());
    let mut slot = choose(&VALUE, black_box(true));
    replace(&mut slot, &item);
    assert_eq!(slot.take().unwrap().value, 23);
    assert!(slot.is_none());
    assert!(slot.replace(&item).is_none());
    assert!(std::ptr::eq(slot.unwrap(), &item));
    assert!(choose(&item, black_box(false)).is_none());
    let nested = black_box([None, Some(None), Some(Some(&item))]);
    assert!(nested[0].is_none());
    assert!(nested[1].unwrap().is_none());
    assert_eq!(nested[2].unwrap().unwrap().extra, 29);
    let words = black_box([2, 3, 5, 7, 11]);
    let mut iter = words.iter();
    let mut total = 0;
    while let Some(value) = iter.next() {
        total += *value;
    }
    assert_eq!(total, 28);
    assert!(iter.next().is_none());
    let mut value = black_box(31i32);
    let pointer = &mut value as *mut i32;
    unsafe {
        assert!(std::ptr::null::<i32>().as_ref().is_none());
        *pointer.as_mut().unwrap() = 37;
        assert_eq!(*pointer.as_ref().unwrap(), 37);
    }
    assert_eq!(value, 37);
    let absent = black_box(NonNull::<Item>::new(std::ptr::null_mut()));
    assert!(absent.is_none());
    let present = black_box(Some(NonNull::from(&item)));
    assert_eq!(unsafe { present.unwrap().as_ref() }.extra, 29);
    let sentinel = black_box(Some(NonNull::<Item>::dangling()));
    assert!(sentinel.is_some());
    let bits: usize = unsafe { std::mem::transmute(black_box(None::<&Item>)) };
    assert_eq!(bits, 0);
    let bits: usize = unsafe { std::mem::transmute(black_box(Some(&item))) };
    assert_eq!(bits, (&item as *const Item).addr());
    let raw: *const Item = unsafe { std::mem::transmute(black_box(Some(&item))) };
    let round_trip: Option<&Item> = unsafe { std::mem::transmute(black_box(raw)) };
    assert!(std::ptr::eq(round_trip.unwrap(), &item));
    let mut stored = black_box(vec![None, Some(&VALUE), Some(&item)]);
    assert_eq!(stored[1].unwrap().value, 17);
    replace(&mut stored[1], &item);
    assert_eq!(stored[1].unwrap().value, 23);
    assert_eq!(stored.pop().unwrap().unwrap().extra, 29);
    let empty = ();
    assert!(choose(&empty, black_box(true)).is_some());
    assert!(choose(&empty, black_box(false)).is_none());
}

fn constructor_items() {
    #[derive(Debug, PartialEq)]
    struct Header {
        index: usize,
    }
    let headers = [Header { index: 3 }, Header { index: 7 }];
    let wrapped: Vec<_> = headers.iter().map(Some).collect();
    assert_eq!(wrapped, [Some(&headers[0]), Some(&headers[1])]);
    static HEADER: Header = Header { index: 9 };
    let wrap: fn(&'static Header) -> Option<&'static Header> = std::hint::black_box(Some);
    assert_eq!(wrap(&HEADER), Some(&HEADER));
}
