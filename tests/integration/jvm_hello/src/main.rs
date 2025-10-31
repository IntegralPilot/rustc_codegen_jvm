struct Ciallo {
    pub count: i32,
    pub desc: String,
}

pub fn ciallo(a: Ciallo) -> &'static str {
    "Hello from Rust!"
}

pub fn main() {}