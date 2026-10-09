//! Bounded facts shared by representation and storage optimizations.
mod origins;
pub(crate) use origins::{NO_ORIGIN, origins};
mod users;
pub(crate) use users::ValueUsers;
mod arrays;
pub(crate) use arrays::{native_array_accesses, native_array_roots};
#[cfg(test)]
mod arrays_tests;
