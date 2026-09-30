//! Analyses are built on demand and owned by one body compilation.
mod fields;
mod live;
pub use fields::promote_fields;
#[cfg(test)]
mod fields_tests;
pub use live::{Live, live, live_with_roots};

mod cells;
pub use cells::promote_cells;
mod tagged;
pub use tagged::decompose_tagged;

#[cfg(test)]
mod cells_tests;

mod simplify;
mod unreachable;
pub use simplify::simplify_components;
