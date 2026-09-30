//! Typed SSA bodies. Identifiers are local to their owning compilation context.
mod debug;
pub use debug::*;
mod body;
mod builder;
mod fold;
pub(crate) use fold::Folded;
mod ids;
mod parameters;
mod remap;
mod types;
pub use remap::Remap;
mod verify;

pub use body::*;
pub use builder::Builder;
pub use ids::*;
pub use types::*;
pub use verify::{VerifyError, verify, verify_with_debug};

#[cfg(test)]
mod tests;

#[cfg(test)]
mod storage_tests;

mod shape;
pub use shape::ComponentShape;
