//! JVM-visible methods forward to one canonical Rust implementation. A recipe
//! owns ABI metadata, never a duplicate computational body.
use super::{Constant, Signature};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReceiverPointer {
    pub size: i32,
    pub alignment: i32,
    pub codec: Constant,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodForwarder {
    pub signature: Signature,
    pub target_owner: String,
    pub target_name: String,
    pub target_signature: Signature,
    pub receiver: Option<ReceiverPointer>,
    pub source_file: Option<String>,
}
