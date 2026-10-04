struct Guard;

impl Drop for Guard {
    fn drop(&mut self) {
        eprintln!("abort ran a destructor");
    }
}

fn main() {
    let _ = std::panic::catch_unwind(|| {
        let _guard = Guard;
        std::process::abort();
    });
    eprintln!("abort returned or unwound");
}
