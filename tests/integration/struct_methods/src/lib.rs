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
