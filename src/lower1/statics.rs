use crate::oomir;
use rustc_abi::Size;
use rustc_middle::ty::{Instance, TyCtxt};
use rustc_span::def_id::DefId;
use std::collections::HashMap;

use super::{
    jvm_names, operand::const_eval::read_constant_value_from_memory, types::ty_to_oomir_type,
};

fn identity(tcx: TyCtxt<'_>, def_id: DefId) -> (String, String) {
    (
        format!("{}$Static", jvm_names::class_for_def_id(tcx, def_id)),
        jvm_names::member_name(tcx.item_name(def_id).as_str()),
    )
}

pub fn static_ref_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> oomir::Constant {
    let rust_ty = tcx.type_of(def_id).skip_binder();
    let value_type = ty_to_oomir_type(rust_ty, tcx, data_types, instance);
    let ty = if matches!(value_type, oomir::Type::Array(_)) {
        value_type
    } else {
        oomir::Type::Pointer(Box::new(value_type))
    };
    let (owner_class, field_name) = identity(tcx, def_id);
    oomir::Constant::StaticRef {
        owner_class,
        field_name,
        ty,
    }
}

pub fn lower_static<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    module: &mut oomir::Module,
) -> Result<(), String> {
    let rust_ty = tcx.type_of(def_id).skip_binder();
    let instance = Instance::mono(tcx, def_id);
    let value_type = ty_to_oomir_type(rust_ty, tcx, &mut module.data_types, instance);
    let storage_type = if matches!(value_type, oomir::Type::Array(_)) {
        value_type.clone()
    } else {
        oomir::Type::Pointer(Box::new(value_type.clone()))
    };
    let allocation = tcx
        .eval_static_initializer(def_id)
        .map_err(|error| format!("could not evaluate static {def_id:?}: {error:?}"))?;
    let initializer = read_constant_value_from_memory(
        tcx,
        allocation.inner(),
        Size::ZERO,
        rust_ty,
        &mut module.data_types,
        instance,
    )?;
    let (owner_class, field_name) = identity(tcx, def_id);
    let static_value = oomir::Static {
        owner_class,
        field_name,
        storage_type,
        initializer,
        is_thread_local: tcx.is_thread_local_static(def_id),
    };
    module.statics.insert(static_value.key(), static_value);
    Ok(())
}
