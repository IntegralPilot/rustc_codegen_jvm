//! Analyses are built on demand and owned by one body compilation.
mod borrowed_memory;
mod field_abi;
pub use borrowed_memory::{borrowed_memory_method, lower_borrowed_memory};
#[cfg(test)]
mod borrowed_memory_tests;
mod fields;
pub use field_abi::lower_borrowed_fields;
#[cfg(test)]
mod field_abi_tests;
mod live;
pub use fields::{fold_field_paths, promote_fields};
#[cfg(test)]
mod fields_tests;
pub use live::{Live, live, live_with_roots};

mod cells;
pub use cells::promote_cells;
mod storage;
pub use storage::lower_typed_storage;
#[cfg(test)]
mod storage_tests;
mod tagged;
pub use tagged::decompose_tagged;
mod array_locations;
#[cfg(test)]
mod array_locations_tests;
pub use array_locations::lower_array_locations;
mod views;
pub use views::decompose_views;
#[cfg(test)]
mod views_tests;

#[cfg(test)]
mod cells_tests;

mod view_abi;
pub use view_abi::{component_argument_slots, lower_component_arguments};

mod addresses;
pub use addresses::decompose_addresses;

#[cfg(test)]
mod addresses_tests;

mod aggregate_joins;
mod aggregates;
mod owned_copies;
mod owned_reads;
mod value_copies;
pub use owned_reads::lower_owned_reads;
#[cfg(test)]
mod owned_reads_tests;
pub use aggregates::promote_aggregates;
#[cfg(test)]
mod aggregates_tests;

mod return_abi;
pub use return_abi::lower_component_returns;

#[cfg(test)]
mod return_abi_tests;

mod address_parts;

mod typed_addresses;
pub use typed_addresses::lower_typed_addresses;
#[cfg(test)]
mod typed_addresses_tests;

mod typed_loads;
pub use typed_loads::lower_typed_loads;
#[cfg(test)]
mod typed_loads_tests;

mod address_observers;
pub use address_observers::lower_address_observers;
#[cfg(test)]
mod address_observers_tests;

mod address_intrinsics;
pub use address_intrinsics::lower_address_intrinsics;
#[cfg(test)]
mod address_intrinsics_tests;

mod memory_copies;
pub use memory_copies::lower_memory_copies;
#[cfg(test)]
mod array_view_tests;
#[cfg(test)]
mod memory_copies_tests;

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
