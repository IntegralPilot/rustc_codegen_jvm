pub struct Ciallo {
    pub count: i32,
    pub desc: &'static str,
}

pub fn ciallo(a: Ciallo) -> &'static str {
    if a.count == 233 && a.desc == "wooooooooo" {
        "Hello from Rust!"
    } else {
        "An error occured"
    }
}
