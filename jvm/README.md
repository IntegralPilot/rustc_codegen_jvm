# `rcj`

Attribute macros for calling foreign JVM classes from Rust.

This only works with [`rustc_codegen_jvm`](https://github.com/IntegralPilot/rustc_codegen_jvm), and is not intended for use with JNI.

## Get

### From upstream

```toml
[dependencies]
jvm = { package = "rcj", git = "https://github.com/IntegralPilot/rustc_codegen_jvm" }
```

### From crates.io

```toml
[dependencies]
jvm = { package = "rcj", version = "0.1" }
```

## Example

```rust,ignore
#![feature(extern_types)]

#[jvm::class("java.time.LocalDate", rename_all = "camelCase")]
impl LocalDate {
    #[jvm::static_method]
    pub fn of(year: i32, month: i32, day: i32) -> *mut Self {}

    // Inferred as the JVM method getYear.
    #[jvm::method]
    pub fn get_year(&self) -> i32 {}
}

#[jvm::class("Main$Counter", rename_all = "camelCase")]
impl Counter {
    #[jvm::constructor]
    pub fn new(value: i32) -> *mut Self {}

    #[jvm::field]
    pub fn value(&self) -> i32 {}

    // set_value is inferred as the field value.
    #[jvm::field]
    pub fn set_value(&mut self, value: i32) {}

    // Named options keep cross-class bindings unambiguous.
    #[jvm::static_field(class = "Main")]
    pub fn shared_count() -> i32 {}
}
```
If you want to have multiple `impl` blocks, you need to use `#[jvm::bindings]` on subsequent blocks so the type is not declared
twice:

```rust,ignore
#![feature(extern_types)]

#[jvm::class("java.lang.StringBuilder")]
impl StringBuilder {}

#[jvm::bindings(rename_all = "camelCase")]
impl StringBuilder {
    #[jvm::method]
    pub fn append_code_point(&mut self, code_point: i32) -> *mut Self {}
}
```
