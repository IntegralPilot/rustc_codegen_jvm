struct Settings {
    base: i32,
    enabled: bool,
}

fn add_one(value: i32) -> i32 {
    value + 1
}

static ANSWER: i32 = 42;
static ANSWER_REF: &i32 = &ANSWER;
static SETTINGS: Settings = Settings {
    base: 40,
    enabled: true,
};
static VALUES: [i32; 4] = [3, 5, 8, 13];
static MESSAGE: &str = "static value";
static OPERATION: fn(i32) -> i32 = add_one;
const PROMOTED_VALUE: &&str = &"promoted value";
const PROMOTED_ARRAY: &[&str; 1] = core::array::from_ref(PROMOTED_VALUE);

mod nested {
    pub static OFFSET: i32 = 2;
}

fn main() {
    assert!(ANSWER == 42);
    assert!(*ANSWER_REF == ANSWER);
    assert!(SETTINGS.enabled);
    assert!(SETTINGS.base + nested::OFFSET == ANSWER);
    assert!(VALUES[0] + VALUES[1] + VALUES[2] + VALUES[3] == 29);
    assert!(MESSAGE == "static value");
    assert!(OPERATION(41) == ANSWER);
    assert!(*PROMOTED_VALUE == PROMOTED_ARRAY[0]);
    assert!(core::ptr::eq(PROMOTED_VALUE, &PROMOTED_ARRAY[0]));
}
