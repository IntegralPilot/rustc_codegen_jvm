#![feature(extern_types, register_tool)]
#![register_tool(jvm_codegen)]

#[jvm::class("java.time.LocalDate", rename_all = "camelCase")]
impl JavaLocalDate {
    #[jvm::static_method("of")]
    pub fn of(year: i32, month: i32, day: i32) -> *const Self {}

    #[jvm::method]
    pub fn get_year(&self) -> i32 {}
}

#[jvm::class("Main.State", rename_all = "camelCase")]
impl JavaState {
    #[jvm::constructor]
    pub fn new(value: i32, wide: i64) -> *mut Self {}

    #[jvm::field]
    pub fn get_value(&self) -> i32 {}

    #[jvm::field]
    pub fn set_value(&mut self, value: i32) {}

    #[jvm::field]
    pub fn get_wide(&self) -> i64 {}

    #[jvm::field]
    pub fn get_next(&self) -> *mut Self {}

    #[jvm::field]
    pub fn set_next(&mut self, next: *mut Self) {}

    #[jvm::static_field(class = "Main")]
    pub fn shared() -> i64 {}

    #[jvm::static_field(class = "Main")]
    pub fn set_shared(value: i64) {}

    #[jvm::static_field(class = "Main")]
    pub fn shared_state() -> *mut Self {}

    #[jvm::static_field(class = "Main")]
    pub fn set_shared_state(value: *mut Self) {}

    #[jvm::static_method("twice")]
    pub fn twice(value: i32) -> i32 {}
}

#[jvm::static_method(class = "java.lang.Math", name = "max")]
fn java_max(left: i32, right: i32) -> i32 {}

unsafe extern "C" {
    #[jvm::static_method("java.lang.Math", "min")]
    fn java_min(left: i32, right: i32) -> i32;
}

pub enum MacroLeaf {
    Number(i32),
    Empty,
}

pub enum MacroRoot {
    #[jvm_codegen::subtype]
    Leaf(MacroLeaf),
    Other(i32),
}

pub fn exercise() -> i64 {
    unsafe {
        let leap = JavaLocalDate::of(2024, 2, 29);
        assert_eq!((&*leap).get_year(), 2024);

        let first = JavaState::new(7, 4_000_000_000);
        assert_eq!((&*first).get_value(), 7);
        assert_eq!((&*first).get_wide(), 4_000_000_000);

        (&mut *first).set_value(13);
        assert_eq!((&*first).get_value(), 13);

        let second = JavaState::new(11, 9);
        (&mut *first).set_next(second);
        assert_eq!((&*first).get_value(), 13);
        assert_eq!((&*(&*first).get_next()).get_value(), 11);

        assert_eq!(JavaState::shared(), 21);
        JavaState::set_shared(34);
        assert_eq!(JavaState::shared(), 34);

        JavaState::set_shared_state(first);
        assert_eq!((&*JavaState::shared_state()).get_value(), 13);

        assert_eq!(JavaState::twice(6), 12);
        assert_eq!(java_max(17, 9), 17);
        assert_eq!(java_min(17, 9), 9);

        let nested = MacroRoot::Leaf(MacroLeaf::Number(5));
        let nested_value = match nested {
            MacroRoot::Leaf(MacroLeaf::Number(value)) => value,
            _ => 0,
        };

        (&*first).get_value() as i64
            + (&*(&*first).get_next()).get_value() as i64
            + JavaState::shared()
            + JavaState::twice(6) as i64
            + java_max(17, 9) as i64
            + java_min(17, 9) as i64
            + nested_value as i64
    }
}
