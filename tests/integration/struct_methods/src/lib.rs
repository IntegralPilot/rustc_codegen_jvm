#![feature(custom_inner_attributes)]
#![feature(register_tool)]
#![register_tool(jvm_codegen)]
#![jvm_codegen::export]
pub struct NamedCounter {
    pub name: &'static str,
    pub count: u32,
    pub limit: u32,
    pub enabled: bool,
}

pub struct EmptyMarker {}

pub struct OrderedConstant {
    pub z_value: u32,
    pub a_flag: bool,
}

pub const DEFAULT_PROFILE: OrderedConstant = OrderedConstant {
    z_value: 36,
    a_flag: true,
};

impl NamedCounter {
    pub fn new(name: &'static str, limit: u32) -> Self {
        NamedCounter {
            name,
            count: 0,
            limit,
            enabled: true,
        }
    }

    pub fn new_disabled(name: &'static str, limit: u32) -> Self {
        NamedCounter {
            name,
            count: 0,
            limit,
            enabled: false,
        }
    }

    pub fn get_limit(&self) -> u32 {
        self.limit
    }

    pub fn get_count(&self) -> u32 {
        self.count
    }

    pub fn finalize(&mut self) {
        self.count += 1;
    }

    pub fn finish(&mut self) {
        self.finalize();
    }

    pub fn increment(&mut self) -> bool {
        if !self.enabled {
            return false;
        }

        self.count = self.count + 1;
        true
    }
}

pub fn accept_empty_marker(_marker: EmptyMarker) {
}

pub fn default_profile() -> OrderedConstant {
    DEFAULT_PROFILE
}

pub trait Finalize {
    fn finalize(&mut self);
}

impl Finalize for u32 {
    fn finalize(&mut self) {
        *self += 1;
    }
}

#[inline(never)]
pub fn finish_trait(value: &mut dyn Finalize) {
    value.finalize();
}

pub fn finish_number() -> u32 {
    let mut number = 7;
    finish_trait(&mut number);
    number
}

struct PrivateCounter(u32);

impl PrivateCounter {
    #[inline(never)]
    fn advance(&mut self) {
        self.0 += 5;
    }
}

impl Drop for PrivateCounter {
    fn drop(&mut self) {
        assert_eq!(self.0, 12);
    }
}

pub fn private_counter() -> u32 {
    let mut counter = std::hint::black_box(PrivateCounter(7));
    counter.advance();
    counter.0
}

pub struct OptionalViewBridge;

impl OptionalViewBridge {
    pub fn has_value(&self, value: Option<&str>) -> bool {
        value.is_some()
    }

    pub fn round_trip(&self, value: Option<&'static str>) -> Option<&'static str> {
        std::hint::black_box(value)
    }
}
