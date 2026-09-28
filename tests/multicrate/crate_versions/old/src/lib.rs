#[derive(Clone)]
pub struct Node(pub i32);

pub static VALUE: i32 = 11;

#[inline(never)]
pub fn version() -> i32 {
    VALUE
}

#[inline(never)]
pub fn make() -> Node {
    Node(version())
}

#[inline(never)]
pub fn clone_node<T: Clone>(value: &T) -> T {
    value.clone()
}

pub fn nodes() -> Vec<Node> {
    vec![clone_node(&make())]
}

pub trait Value {
    fn value(&self) -> i32;
}

impl Value for Node {
    fn value(&self) -> i32 {
        self.0
    }
}

pub fn boxed() -> Box<dyn Value> {
    Box::new(make())
}
