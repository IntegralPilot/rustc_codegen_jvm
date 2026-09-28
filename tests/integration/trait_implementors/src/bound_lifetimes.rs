pub struct Section<'a>(pub &'a [u8]);

pub enum Payload<'a> {
    Data(Section<'a>),
}

// Exporting this interface used to let 'a escape through the callback type.
pub trait BorrowedConsumer {
    fn consume<'a>(&self, callback: fn(Section<'a>) -> Payload<'a>, bytes: &'a [u8]) -> u8;
}
