//! Analyses are built on demand and owned by one body compilation.
mod live;
pub use live::{Live, live, live_with_roots};
