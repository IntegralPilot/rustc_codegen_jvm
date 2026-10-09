use crate::lower1::context::Definitions;
use crate::oomir;
use rustc_abi::Size;
use rustc_middle::ty::{Instance, TyCtxt};
use rustc_span::def_id::DefId;

use super::{
    jvm_names,
    operand::const_eval::read_constant_value_from_memory,
    types::{
        layout_align_bytes, layout_size_bytes, pointer_memory_codec_operand, ty_to_oomir_type,
    },
};

pub(super) fn is_nested(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    matches!(
        tcx.def_kind(def_id),
        rustc_hir::def::DefKind::Static { nested: true, .. }
    )
}

fn identity(tcx: TyCtxt<'_>, def_id: DefId) -> (String, String) {
    (
        format!("{}$Static", jvm_names::class_for_def_id(tcx, def_id)),
        jvm_names::member_name(tcx.item_name(def_id).as_str()),
    )
}

pub fn static_ref_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> oomir::Constant {
    let rust_ty = data_types.normalize(tcx, tcx.type_of(def_id).skip_binder(), instance);
    let value_type = ty_to_oomir_type(rust_ty, tcx, data_types, instance);
    let ty = if rust_ty.is_array() {
        value_type
    } else {
        oomir::Type::pointer(value_type)
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
    module: &mut super::context::Module<'tcx>,
) -> Result<(), String> {
    // Synthetic statics have allocation data but no declared Rust type. Their
    // typed views are materialized by constant lowering at each reference.
    if is_nested(tcx, def_id) {
        return Ok(());
    }
    let instance = Instance::mono(tcx, def_id);
    let rust_ty = module
        .data_types
        .normalize(tcx, tcx.type_of(def_id).skip_binder(), instance);
    let value_type = ty_to_oomir_type(rust_ty, tcx, &mut module.data_types, instance);
    // Only Rust arrays use direct array static storage. Struct wrappers keep the struct borrow ABI.
    let storage_type = if rust_ty.is_array() {
        value_type.clone()
    } else {
        oomir::Type::pointer(value_type.clone())
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
    let allocation_size = layout_size_bytes(tcx, rust_ty)?;
    let allocation_alignment = layout_align_bytes(tcx, rust_ty)?;
    let allocation_codec_class_name = matches!(storage_type, oomir::Type::Pointer(_))
        .then(|| pointer_memory_codec_operand(rust_ty, tcx, &mut module.data_types, instance))
        .and_then(|codec| match codec {
            oomir::Operand::Constant(oomir::Constant::String(class_name)) => Some(class_name),
            _ => None,
        });
    let (owner_class, field_name) = identity(tcx, def_id);
    let attrs = tcx.codegen_fn_attrs(def_id);
    use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
    let static_value = oomir::Static {
        owner_class,
        field_name,
        storage_type,
        initializer,
        allocation_size,
        allocation_alignment,
        allocation_codec_class_name,
        is_thread_local: tcx.is_thread_local_static(def_id),
        is_private: !crate::java_exports::is_exported(tcx, def_id)
            && !attrs.contains_extern_indicator()
            && !attrs
                .flags
                .intersects(CodegenFnAttrFlags::USED_COMPILER | CodegenFnAttrFlags::USED_LINKER),
    };
    module.statics.insert(static_value.key(), static_value);
    Ok(())
}
