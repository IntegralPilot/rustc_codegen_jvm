fn main() {
    let argument = std::env::args()
        .nth(1)
        .expect("the workflow test passes a program argument");
    println!(
        "cargo-jvm run: {argument}: {}",
        cargo_jvm_workflow::triple(14)
    );
}
