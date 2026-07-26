struct Settings {
    base: i32,
    enabled: bool,
}

#[repr(C)]
struct StaticBytes {
    low: u32,
    high: u32,
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
static STATIC_BYTES: StaticBytes = StaticBytes {
    low: 0x1122_3344,
    high: 0x5566_7788,
};
static VALUES: [i32; 4] = [3, 5, 8, 13];
static MESSAGE: &str = "static value";
static OPERATION: fn(i32) -> i32 = add_one;
const PROMOTED_VALUE: &&str = &"promoted value";
const PROMOTED_ARRAY: &[&str; 1] = core::array::from_ref(PROMOTED_VALUE);

#[repr(C, align(16))]
struct AssociatedConstArray {
    data: [u8; if Self::ENABLED { 16 } else { 0 }],
}

impl AssociatedConstArray {
    const ENABLED: bool = false;

    const fn new() -> Self {
        Self {
            data: [0; if Self::ENABLED { 16 } else { 0 }],
        }
    }
}

static ASSOCIATED_CONST_ARRAY: AssociatedConstArray = AssociatedConstArray::new();

trait StaticOperation: Sync {
    fn apply(&self, value: i32) -> i32;
}

struct Increment;

impl StaticOperation for Increment {
    fn apply(&self, value: i32) -> i32 {
        value + 1
    }
}

static DYNAMIC_OPERATION: &dyn StaticOperation = &Increment;

mod nested {
    pub static OFFSET: i32 = 2;
}

fn main() {
    assert!(ANSWER == 42);
    assert!(*ANSWER_REF == ANSWER);
    assert!(SETTINGS.enabled);
    assert!(SETTINGS.base + nested::OFFSET == ANSWER);
    let words = unsafe {
        core::slice::from_raw_parts(
            (&raw const STATIC_BYTES).cast::<u32>(),
            core::mem::size_of::<StaticBytes>() / core::mem::size_of::<u32>(),
        )
    };
    assert!(words == [0x1122_3344, 0x5566_7788]);
    assert!(VALUES[0] + VALUES[1] + VALUES[2] + VALUES[3] == 29);
    assert!(MESSAGE == "static value");
    assert!(OPERATION(41) == ANSWER);
    assert!(DYNAMIC_OPERATION.apply(41) == ANSWER);
    assert!(*PROMOTED_VALUE == PROMOTED_ARRAY[0]);
    assert!(core::ptr::eq(PROMOTED_VALUE, &PROMOTED_ARRAY[0]));
    assert!(ASSOCIATED_CONST_ARRAY.data.is_empty());
    assert!(core::mem::size_of_val(&ASSOCIATED_CONST_ARRAY) == 0);
}
