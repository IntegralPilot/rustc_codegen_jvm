#![no_std]

#[derive(Clone, Copy)]
pub enum OptionArgument {
    Yes,
    No,
}

#[derive(Clone, Copy)]
pub enum OptionOccurrence {
    Optional,
    Multiple,
}
