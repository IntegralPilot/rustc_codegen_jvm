pub use ristretto_classfile::byte_reader::ByteReader;
pub use ristretto_classfile::*;

#[derive(Debug)]
pub enum Error {
    ClassFile(ristretto_classfile::Error),
    VerificationError { context: String, message: String },
}

impl std::fmt::Display for Error {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ClassFile(error) => error.fmt(formatter),
            Self::VerificationError { context, message } => {
                write!(formatter, "{context}: {message}")
            }
        }
    }
}

impl std::error::Error for Error {}

impl From<ristretto_classfile::Error> for Error {
    fn from(error: ristretto_classfile::Error) -> Self {
        Self::ClassFile(error)
    }
}

impl From<std::num::TryFromIntError> for Error {
    fn from(error: std::num::TryFromIntError) -> Self {
        Self::ClassFile(error.into())
    }
}

pub type Result<T> = std::result::Result<T, Error>;
