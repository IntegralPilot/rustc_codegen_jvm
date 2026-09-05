# `rcj`

Attribute macros for calling JVM classes from Rust compiled by
[`rustc_codegen_jvm`](https://github.com/IntegralPilot/rustc_codegen_jvm).
They generate the backend's `jvm:` link names and replace placeholder function
bodies with the corresponding JVM call.

```toml
[dependencies]
jvm = { package = "rcj", git = "https://github.com/IntegralPilot/rustc_codegen_jvm" }
```

```rust,no_run
#![feature(extern_types)]
# use rcj as jvm;

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

The package is named `rcj`; the `jvm` dependency alias gives the attributes their
natural `#[jvm::...]` spelling. Member attributes may also be imported directly,
such as `use jvm::{constructor, method};` followed by `#[constructor]` or
`#[method]`.

Fully dotted names and JVM slash names both work for ordinary classes. Nested
classes can use JVM `$` syntax (`java.util.Map$Entry`), or an explicit
package/class boundary followed by dots (`java/util/Map.Entry`). An all-dot name
is treated as a package path because package dots and nested-class dots are
otherwise indistinguishable.

A `#[jvm::class("...")]` impl reuses its class name and infers member names. A
single positional argument on a member attribute is the member name; use
`class = "..."` to target another class. Explicit JVM descriptors are available
as `descriptor = "..."` for ambiguous overloads.

For an additional impl block, use `#[jvm::bindings]` so the type is not declared
twice:

```rust,no_run
# #![feature(extern_types)]
# use rcj as jvm;
# #[jvm::class("java.lang.StringBuilder")]
# impl StringBuilder {}
#[jvm::bindings(rename_all = "camelCase")]
impl StringBuilder {
    #[jvm::method]
    pub fn append_code_point(&mut self, code_point: i32) -> *mut Self {}
}
```

The attributes also work directly in foreign blocks:

```rust,no_run
#![feature(extern_types)]
# use rcj as jvm;

unsafe extern "C" {
    #[jvm::class("java.time.LocalDate")]
    type LocalDate;

    #[jvm::static_method("java.time.LocalDate", "of")]
    fn date(year: i32, month: i32, day: i32) -> *mut LocalDate;

    #[jvm::method("getYear")]
    fn year(date: &LocalDate) -> i32;
}
```

For transparent nested enum variants, use the compiler marker separately so it
does not collide with this crate's `jvm` macro namespace:

```rust
#![feature(register_tool)]
#![register_tool(jvm_codegen)]

enum Leaf {
    Value(i32),
}

enum Root {
    #[jvm_codegen::subtype]
    Leaf(Leaf),
}
```
