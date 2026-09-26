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

#[jvm::class("Main.Counter", rename_all = "camelCase")]
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

Dots before the first capitalized name separate packages; later dots denote
nested classes. Use JVM `/` and `$` spelling for names that do not follow Java
capitalization conventions.

## Interfaces

Declare foreign interfaces with `#[jvm::interface]` so the backend knows which
invocation opcode and constant-pool entry to use without inspecting a classpath:

```rust,ignore
#![feature(extern_types)]

#[jvm::interface("java.util.Comparator", rename_all = "camelCase")]
impl Comparator {
    #[jvm::static_method]
    pub fn natural_order() -> *mut Self {}

    #[jvm::method]
    pub fn reversed(&self) -> *mut Self {}
}
```

Instance methods, including default methods, use `invokeinterface`. Static
methods use `invokestatic` with an interface method reference. Outside an
interface declaration, add `interface = true` to `#[jvm::static_method]` or to a
named `#[jvm::bindings("java.util.Comparator", interface = true)]` impl containing
static interface bindings. An explicitly different owner in such an impl keeps
its own class/interface choice.

Both declaration attributes also accept opaque unit structs and foreign types.
The raw forms are `#[link_name = "jvm:interface:java/util/Comparator"]` on an
extern type and `jvm:static-interface:<owner>:<method>[:<descriptor>]` on a static
function import. Instance imports keep `jvm:virtual:<method>[:<descriptor>]`;
the receiver's declared type determines class versus interface dispatch.

`#[jvm::constructor]` is for classes and emits `new`, `dup`, and `invokespecial`.
Interfaces cannot have constructors or instance fields. General `invokespecial`
calls to superclass or interface-default implementations are not exposed;
ordinary default-method calls use normal interface dispatch.

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
