use std::mem::ManuallyDrop;

union Storage {
    inline: ManuallyDrop<i32>,
}

impl Storage {
    #[inline(never)]
    fn from_inline(value: i32) -> Self {
        Self {
            inline: ManuallyDrop::new(value),
        }
    }

    #[inline(never)]
    fn get_inline(&self) -> i32 {
        unsafe { *self.inline + 1 }
    }

    #[inline(never)]
    fn set_inline(&mut self, value: i32) {
        self.inline = ManuallyDrop::new(value + 2);
    }
}

pub fn run() {
    let mut value = Storage::from_inline(12);
    assert_eq!(unsafe { *value.inline }, 12);
    assert_eq!(value.get_inline(), 13);
    value.set_inline(40);
    assert_eq!(unsafe { *value.inline }, 42);
    assert_eq!(value.get_inline(), 43);
}
