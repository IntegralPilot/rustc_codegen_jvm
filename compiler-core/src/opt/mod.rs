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
mod views;
pub use views::decompose_views;
#[cfg(test)]
mod views_tests;

#[cfg(test)]
mod cells_tests;

mod view_abi;
pub use view_abi::{component_argument_slots, lower_component_arguments};

mod simplify;
mod unreachable;
pub use simplify::simplify_components;

fn append_value(
    body: &mut crate::ir::Body,
    op: crate::ir::Op,
    ty: crate::ir::TypeId,
) -> (crate::ir::InstId, crate::ir::ValueId) {
    use crate::ir::{Inst, InstId, Value, ValueDef, ValueId};
    let inst = InstId::new(body.instructions.len());
    let value = ValueId::new(body.values.len());
    body.values.push(Value {
        ty,
        def: ValueDef::Inst(inst),
    });
    body.instructions.push(Inst {
        op,
        result: Some(value),
    });
    (inst, value)
}
