use std::any::Any;
use std::cell::Cell;
use std::rc::Rc;

trait IntoValue {
    fn value(&self) -> u32;

    fn into_value(self) -> u32 {
        self.value()
    }
}

trait Blockable: IntoValue + Any {}

impl IntoValue for Option<bool> {
    fn value(&self) -> u32 {
        u32::from(self.unwrap_or(false))
    }
}

impl Blockable for Option<bool> {}

struct Owned(Rc<Cell<u32>>);

impl IntoValue for Owned {
    fn value(&self) -> u32 {
        7
    }
}

impl Blockable for Owned {}

impl Drop for Owned {
    fn drop(&mut self) {
        self.0.set(self.0.get() + 1);
    }
}

#[inline(never)]
fn erase<T: Blockable + 'static>(value: T) -> Box<dyn Blockable> {
    Box::new(value)
}

pub fn run() {
    // Typst boxes values behind a trait with a by-value supertrait method.
    let value = erase(std::hint::black_box(Some(true)));
    assert_eq!(value.value(), 1);
    let any: &dyn Any = &*value;
    assert_eq!(any.downcast_ref::<Option<bool>>(), Some(&Some(true)));

    // The ordinary method and its vtable shim need distinct bodies and ABIs.
    assert_eq!(std::hint::black_box(Some(false)).into_value(), 0);
    assert_eq!(value.into_value(), 1);

    let drops = Rc::new(Cell::new(0));
    let owned = erase(Owned(Rc::clone(&drops)));
    assert_eq!(owned.into_value(), 7);
    assert_eq!(drops.get(), 1);
}
