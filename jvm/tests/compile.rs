#![feature(extern_types)]
#![allow(dead_code)]

use jvm::{constructor, method};
use rcj as jvm;

unsafe extern "C" {
    #[jvm::class("java.lang.String")]
    type JString;

    #[jvm::class("java.time.LocalDate")]
    type JavaLocalDate;

    #[jvm::static_method("java.time.LocalDate", "of")]
    fn raw_date(year: i32, month: i32, day: i32) -> *const JavaLocalDate;

    #[jvm::method(name = "getYear")]
    fn raw_year(date: &JavaLocalDate) -> i32;

    #[jvm::constructor(class = "java.lang.String")]
    fn raw_string(bytes: *const u8) -> *mut JString;

    #[jvm::field]
    fn raw_value(string: &JString) -> i32;

    #[jvm::static_field("example.Globals", "value")]
    fn raw_global() -> i32;
}

#[jvm::class("java.time.LocalDate", rename_all = "camelCase")]
impl Date {
    #[jvm::static_method]
    fn of(year: i32, month: i32, day: i32) -> *mut Self {}

    #[jvm::static_method("parse")]
    fn parse_iso(value: &JString) -> *mut Self {}

    #[jvm::method]
    fn get_year(&self) -> i32 {}

    #[jvm::field]
    fn get_day_of_month(&self) -> i32 {}

    #[jvm::field]
    fn set_day_of_month(&mut self, value: i32) {}

    #[jvm::static_field(class = "example.Globals")]
    fn shared_value() -> i32 {}

    #[jvm::static_field(class = "example.Globals")]
    fn set_shared_value(value: i32) {}
}

#[jvm::static_method(class = "java.lang.Math", name = "max")]
fn max_i32(left: i32, right: i32) -> i32 {}

struct Helpers;

impl Helpers {
    #[jvm::static_method(class = "java.lang.System", name = "nanoTime")]
    fn nano_time() -> i64 {}
}

mod qualified {
    #[rcj::class("java.lang.StringBuilder")]
    pub struct Builder;
}

#[jvm::bindings]
impl qualified::Builder {
    #[jvm::method("length")]
    fn length(&self) -> i32 {}
}

#[jvm::class("java.lang.StringBuilder")]
impl DirectlyImportedAttributes {
    #[constructor]
    fn new() -> *mut Self {}

    #[method("length")]
    fn length(&self) -> i32 {}
}

#[test]
fn macros_expand() {}
