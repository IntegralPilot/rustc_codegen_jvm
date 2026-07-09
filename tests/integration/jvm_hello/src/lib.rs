pub struct Ciallo {
    pub count: i32,
    pub desc: String,
}

pub fn ciallo(_a: Ciallo) -> &'static str {
    "Hello from Rust!"
}
