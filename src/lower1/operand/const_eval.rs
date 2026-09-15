use crate::lower1::context::Definitions;
use rustc_abi::{BackendRepr, FieldIdx, FieldsShape, Size, TagEncoding, VariantIdx, Variants};
use rustc_hash::FxHashMap as HashMap;
use rustc_middle::mir::interpret::{
    AllocId, AllocRange, Allocation, CtfeProvenance, GlobalAlloc, Pointer, Provenance, Scalar,
};
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::{
    AdtDef, EarlyBinder, FloatTy, GenericArgsRef, Instance, InstanceKind, IntTy,
    PseudoCanonicalInput, ScalarInt, ShimKind, Ty, TyCtxt, TyKind, TypingEnv, UintTy, Unnormalized,
};
use rustc_span::def_id::LOCAL_CRATE;

use super::super::{
    control_flow::rvalue::{
        ensure_closure_fn_pointer_adapter_class, ensure_fn_pointer_adapter_class, fn_pointer_target,
    },
    control_flow::trait_objects::ensure_trait_object_adapter_class_for_pointees,
    jvm_names, ty_to_oomir_type,
    types::{
        UNION_BYTES_FIELD, UNION_OBJECTS_FIELD, ensure_fn_ptr_interface, ensure_union_data_type,
        enum_variant_field_name, fn_ptr_signature_from_ty, force_define_named_adt,
        generate_adt_jvm_class_name, generate_tuple_jvm_class_name, jvm_subtype_payload_ty,
        pointer_memory_codec_operand, pointer_view_codec_operand, union_from_method_name,
    },
};
use crate::oomir;

type ConstAllocation = Allocation<CtfeProvenance>;

fn anonymous_allocation_identity(
    tcx: TyCtxt<'_>,
    value: &oomir::Constant,
    view_size: u64,
    alignment: u64,
    view_codec: &oomir::Constant,
    pointee: &oomir::Type,
) -> String {
    let hash = crate::stable_hash::short_hash_value(
        &(value, view_size, alignment, view_codec, pointee),
        16,
    );
    format!("{}::constant::{hash}", tcx.crate_name(LOCAL_CRATE))
}

fn anonymous_memory_identity(
    tcx: TyCtxt<'_>,
    allocation: &ConstAllocation,
    instance: Instance<'_>,
    storage_value: &oomir::Constant,
) -> Option<String> {
    if !allocation.provenance().ptrs().is_empty() {
        return None;
    }
    let bytes = allocation
        .inspect_with_uninit_and_ptr_outside_interpreter(0..allocation.size().bytes_usize());
    let hash = crate::stable_hash::short_hash_value(
        &(
            format!("{instance:?}"),
            bytes,
            allocation.align.bytes(),
            storage_value,
        ),
        16,
    );
    Some(format!("{}::memory::{hash}", tcx.crate_name(LOCAL_CRATE)))
}

/// Decode the optimized `ConstValue::Slice` representation. Rust uses that
/// representation for every reference whose pointee has a slice tail, not
/// merely for `&str` and `&[T]`.
pub fn read_pointer_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let ty = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let ty = tcx
        .try_normalize_erasing_regions(
            TypingEnv::fully_monomorphized(),
            rustc_middle::ty::Unnormalized::new_wip(ty),
        )
        .unwrap_or(ty);
    if let TyKind::Pat(inner, _) = ty.kind() {
        return read_pointer_constant(tcx, pointer, *inner, oomir_data_types, instance);
    }
    if let Some(field_ty) = scalar_struct_field_ty(tcx, ty)? {
        return read_pointer_constant(tcx, pointer, field_ty, oomir_data_types, instance);
    }

    match ty.kind() {
        TyKind::FnPtr(..) => {
            read_function_pointer_constant(tcx, pointer, ty, oomir_data_types, instance)
        }
        TyKind::Ref(_, inner_ty, _) if inner_ty.is_array() => {
            let value = read_pointee_constant(tcx, pointer, *inner_ty, oomir_data_types, instance)?;
            let backing = interned_pointer_for_full_allocation(
                tcx,
                pointer,
                *inner_ty,
                value.clone(),
                oomir_data_types,
                instance,
            )?
            .unwrap_or(value);
            array_reference_to_slice(tcx, *inner_ty, backing, oomir_data_types, instance)
        }
        TyKind::Ref(_, inner_ty, _) | TyKind::RawPtr(inner_ty, _) => {
            if matches!(ty.kind(), TyKind::RawPtr(..)) {
                if let Some(pointer) = anonymous_memory_pointer_constant(
                    tcx,
                    pointer,
                    *inner_ty,
                    oomir_data_types,
                    instance,
                )? {
                    return Ok(pointer);
                }
            }
            let points_directly_to_static = pointer_references_static(tcx, pointer);
            let points_directly_to_vtable = pointer_references_vtable(tcx, pointer);
            let value = read_pointee_constant(tcx, pointer, *inner_ty, oomir_data_types, instance)?;
            if points_directly_to_static
                || points_directly_to_vtable
                || inner_ty.is_str()
                || inner_ty.is_slice()
                || matches!(inner_ty.kind(), TyKind::Dynamic(..))
            {
                Ok(value)
            } else {
                pointer_constant_for_pointee(
                    tcx,
                    pointer,
                    *inner_ty,
                    value,
                    oomir_data_types,
                    instance,
                )
            }
        }
        _ => read_pointee_constant(tcx, pointer, ty, oomir_data_types, instance),
    }
}

fn pointer_references_static(tcx: TyCtxt<'_>, pointer: Pointer<CtfeProvenance>) -> bool {
    let (provenance, _) = pointer.into_raw_parts();
    provenance
        .get_alloc_id()
        .is_some_and(|alloc_id| matches!(tcx.global_alloc(alloc_id), GlobalAlloc::Static(_)))
}

fn pointer_references_vtable(tcx: TyCtxt<'_>, pointer: Pointer<CtfeProvenance>) -> bool {
    let (provenance, _) = pointer.into_raw_parts();
    provenance
        .get_alloc_id()
        .is_some_and(|alloc_id| matches!(tcx.global_alloc(alloc_id), GlobalAlloc::VTable(..)))
}

fn anonymous_memory_pointer_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    pointee_ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<Option<oomir::Constant>, String> {
    if pointee_ty.is_str()
        || pointee_ty.is_slice()
        || matches!(pointee_ty.kind(), TyKind::Dynamic(..))
    {
        return Ok(None);
    }
    let (provenance, offset) = pointer.into_raw_parts();
    let Some(alloc_id) = provenance.get_alloc_id() else {
        return Ok(None);
    };
    let GlobalAlloc::Memory(const_allocation) = tcx.global_alloc(alloc_id) else {
        return Ok(None);
    };
    let allocation = const_allocation.inner();
    if !allocation.provenance().ptrs().is_empty() {
        return Ok(None);
    }
    let pointee_layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(pointee_ty))
        .map_err(|error| format!("Could not determine constant pointee layout: {error:?}"))?;
    if allocation.size() <= pointee_layout.size {
        return Ok(None);
    }
    if offset > allocation.size() {
        return Err(format!(
            "constant pointer offset {} exceeds allocation size {}",
            offset.bytes(),
            allocation.size().bytes()
        ));
    }
    let bytes = allocation
        .inspect_with_uninit_and_ptr_outside_interpreter(0..allocation.size().bytes_usize())
        .to_vec();
    let Some(&byte) = bytes.first() else {
        return Ok(None);
    };
    let view_codec = match pointer_memory_codec_operand(pointee_ty, tcx, oomir_data_types, instance)
    {
        oomir::Operand::Constant(oomir::Constant::String(codec)) => Some(codec),
        oomir::Operand::Constant(oomir::Constant::Null(_)) => None,
        other => {
            return Err(format!(
                "Constant pointer codec was not a nullable string: {other:?}"
            ));
        }
    };
    let pointee = Box::new(ty_to_oomir_type(
        pointee_ty,
        tcx,
        oomir_data_types,
        instance,
    ));
    let identity_value = oomir::Constant::Array(
        Box::new(oomir::Type::U8),
        bytes.iter().copied().map(oomir::Constant::U8).collect(),
    );
    let identity_codec = view_codec.as_ref().map_or(
        oomir::Constant::Null(oomir::Type::Class("java/lang/String".to_string())),
        |codec| oomir::Constant::String(codec.clone()),
    );
    let identity_candidate = anonymous_memory_identity(tcx, allocation, instance, &identity_value)
        .unwrap_or_else(|| {
            anonymous_allocation_identity(
                tcx,
                &identity_value,
                pointee_layout.size.bytes(),
                allocation.align.bytes(),
                &identity_codec,
                &pointee,
            )
        });
    let identity = oomir_data_types.allocation_identity(alloc_id, identity_candidate);
    if bytes.iter().all(|candidate| *candidate == byte) {
        Ok(Some(oomir::Constant::RepeatedBytePointer {
            identity,
            byte,
            length: allocation.size().bytes(),
            offset: offset.bytes(),
            view_size: pointee_layout.size.bytes(),
            alignment: allocation.align.bytes(),
            view_codec,
            pointee,
        }))
    } else {
        Ok(Some(oomir::Constant::ByteArrayPointer {
            identity,
            bytes,
            offset: offset.bytes(),
            view_size: pointee_layout.size.bytes(),
            alignment: allocation.align.bytes(),
            view_codec,
            pointee,
        }))
    }
}

fn pointer_constant_for_pointee<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    pointee_ty: Ty<'tcx>,
    value: oomir::Constant,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    if let Some(pointer) = interned_pointer_for_full_allocation(
        tcx,
        pointer,
        pointee_ty,
        value.clone(),
        oomir_data_types,
        instance,
    )? {
        return Ok(pointer);
    }
    if let Some(pointer) = interned_pointer_for_memory_view(
        tcx,
        pointer,
        pointee_ty,
        value.clone(),
        oomir_data_types,
        instance,
    )? {
        return Ok(pointer);
    }
    let pointee_layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(pointee_ty))
        .map_err(|error| format!("Could not determine constant reference layout: {error:?}"))?;
    let codec = match pointer_memory_codec_operand(pointee_ty, tcx, oomir_data_types, instance) {
        oomir::Operand::Constant(codec) => codec,
        other => {
            return Err(format!(
                "Constant reference codec was not constant: {other:?}"
            ));
        }
    };
    Ok(oomir::Constant::Instance {
        class_name: oomir::POINTER_CLASS.to_string(),
        fields: HashMap::default(),
        params: vec![
            if oomir::Type::from_constant(&value).has_jvm_value() {
                value
            } else {
                oomir::Constant::Null(oomir::Type::Class("java/lang/Object".to_string()))
            },
            oomir::Constant::I32(
                i32::try_from(pointee_layout.size.bytes())
                    .map_err(|_| "Constant reference pointee exceeds JVM limits".to_string())?,
            ),
            codec,
        ],
        param_types: Vec::new(),
    })
}

fn interned_pointer_for_memory_view<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    pointee_ty: Ty<'tcx>,
    value: oomir::Constant,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<Option<oomir::Constant>, String> {
    let (provenance, offset) = pointer.into_raw_parts();
    let Some(alloc_id) = provenance.get_alloc_id() else {
        return Ok(None);
    };
    let GlobalAlloc::Memory(const_allocation) = tcx.global_alloc(alloc_id) else {
        return Ok(None);
    };
    let allocation = const_allocation.inner();
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(pointee_ty))
        .map_err(|error| format!("Could not determine constant pointee layout: {error:?}"))?;
    let end = offset
        .checked_add(layout.size, &tcx.data_layout)
        .ok_or_else(|| {
            format!(
                "constant pointer range overflow: offset {}, size {}",
                offset.bytes(),
                layout.size.bytes()
            )
        })?;
    if end > allocation.size() {
        return Err(format!(
            "constant pointer range {}..{} exceeds allocation size {}",
            offset.bytes(),
            end.bytes(),
            allocation.size().bytes()
        ));
    }
    let view_codec = match pointer_memory_codec_operand(pointee_ty, tcx, oomir_data_types, instance)
    {
        oomir::Operand::Constant(codec) => codec,
        other => {
            return Err(format!(
                "Constant reference codec was not constant: {other:?}"
            ));
        }
    };
    let pointee = ty_to_oomir_type(pointee_ty, tcx, oomir_data_types, instance);
    let identity_candidate = anonymous_memory_identity(tcx, allocation, instance, &value)
        .unwrap_or_else(|| {
            let hash = crate::stable_hash::short_hash_value(
                &(
                    format!("{instance:?}"),
                    format!("{alloc_id:?}"),
                    allocation.size().bytes(),
                    allocation.align.bytes(),
                ),
                16,
            );
            format!("{}::allocation::{hash}", tcx.crate_name(LOCAL_CRATE))
        });
    let identity = oomir_data_types.allocation_identity(alloc_id, identity_candidate);
    Ok(Some(oomir::Constant::InternedPointer {
        identity,
        value: Box::new(value),
        array_backed: false,
        allocation_size: allocation.size().bytes(),
        offset: offset.bytes(),
        view_size: layout.size.bytes(),
        alignment: allocation.align.bytes(),
        view_codec: Box::new(view_codec),
        pointee: Box::new(pointee),
    }))
}

fn interned_pointer_for_full_allocation<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    pointee_ty: Ty<'tcx>,
    value: oomir::Constant,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<Option<oomir::Constant>, String> {
    let (provenance, offset) = pointer.into_raw_parts();
    let Some(alloc_id) = provenance.get_alloc_id() else {
        return Ok(None);
    };
    let GlobalAlloc::Memory(const_allocation) = tcx.global_alloc(alloc_id) else {
        return Ok(None);
    };
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(pointee_ty))
        .map_err(|error| format!("Could not determine constant pointee layout: {error:?}"))?;
    let allocation = const_allocation.inner();
    if layout.size == Size::ZERO || offset != Size::ZERO || allocation.size() != layout.size {
        return Ok(None);
    }
    let view_codec = match pointer_memory_codec_operand(pointee_ty, tcx, oomir_data_types, instance)
    {
        oomir::Operand::Constant(codec) => codec,
        other => {
            return Err(format!(
                "Constant reference codec was not constant: {other:?}"
            ));
        }
    };
    let pointee = ty_to_oomir_type(pointee_ty, tcx, oomir_data_types, instance);
    let identity_candidate = anonymous_memory_identity(tcx, allocation, instance, &value)
        .unwrap_or_else(|| {
            anonymous_allocation_identity(
                tcx,
                &value,
                layout.size.bytes(),
                allocation.align.bytes(),
                &view_codec,
                &pointee,
            )
        });
    let identity = oomir_data_types.allocation_identity(alloc_id, identity_candidate);
    Ok(Some(oomir::Constant::InternedPointer {
        identity,
        value: Box::new(value),
        array_backed: false,
        allocation_size: allocation.size().bytes(),
        offset: 0,
        view_size: layout.size.bytes(),
        alignment: allocation.align.bytes(),
        view_codec: Box::new(view_codec),
        pointee: Box::new(pointee),
    }))
}

fn read_function_pointer_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let (provenance, _) = pointer.into_raw_parts();
    let alloc_id = provenance.get_alloc_id().ok_or_else(|| {
        format!(
            "Function pointer provenance {:?} has no allocation id",
            provenance
        )
    })?;
    let function_allocation = tcx.global_alloc(alloc_id);
    let GlobalAlloc::Function {
        instance: function_instance,
    } = function_allocation
    else {
        return Err(format!(
            "Function pointer of type {:?} referred to non-function allocation {:?}",
            ty, function_allocation
        ));
    };

    let signature = fn_ptr_signature_from_ty(ty, tcx, oomir_data_types, instance);
    let interface_name = ensure_fn_ptr_interface(&signature, oomir_data_types, tcx, instance);

    let closure_instance = match function_instance.def {
        InstanceKind::Item(def_id) => {
            let item_ty = function_instance.ty(tcx, TypingEnv::fully_monomorphized());
            matches!(item_ty.kind(), TyKind::Closure(..))
                .then_some(Instance::new_raw(def_id, function_instance.args))
        }
        InstanceKind::Shim(ShimKind::ClosureOnce { closure, .. }) => {
            match function_instance.args.type_at(0).kind() {
                TyKind::Closure(def_id, closure_args) => {
                    debug_assert_eq!(*def_id, closure);
                    Some(Instance::new_raw(*def_id, closure_args))
                }
                _ => None,
            }
        }
        _ => None,
    };

    let adapter_class = if let Some(closure_instance) = closure_instance {
        ensure_closure_fn_pointer_adapter_class(
            oomir_data_types,
            closure_instance,
            &signature,
            &interface_name,
            tcx,
            instance,
        )
    } else {
        let callable_target =
            fn_pointer_target(tcx, oomir_data_types, function_instance, &signature);
        ensure_fn_pointer_adapter_class(
            oomir_data_types,
            callable_target.as_ref(),
            &signature,
            &interface_name,
            tcx,
            instance,
        )
    };

    Ok(oomir::Constant::FunctionPointer {
        adapter_class,
        interface_name,
    })
}

fn read_pointer_from_memory<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
) -> Result<Pointer<CtfeProvenance>, String> {
    let pointer_size = tcx.data_layout.pointer_size();
    let ptr_range = AllocRange {
        start: offset,
        size: pointer_size,
    };
    match allocation
        .read_scalar(&tcx.data_layout, ptr_range, true)
        .map_err(|e| format!("Failed to read pointer scalar at {:?}: {:?}", offset, e))?
    {
        Scalar::Ptr(ptr, _) => Ok(ptr),
        Scalar::Int(int) => Err(format!(
            "Expected pointer scalar at {:?}, found integer {:?}",
            offset, int
        )),
    }
}

fn read_trait_object_reference_from_memory<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
    dynamic_ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let data_pointer = read_pointer_from_memory(tcx, allocation, offset)?;
    let vtable_pointer =
        read_pointer_from_memory(tcx, allocation, offset + tcx.data_layout.pointer_size())?;
    let (vtable_provenance, _) = vtable_pointer.into_raw_parts();
    let vtable_alloc_id = vtable_provenance.get_alloc_id().ok_or_else(|| {
        format!("Trait-object vtable pointer {vtable_pointer:?} has no allocation id")
    })?;
    let GlobalAlloc::VTable(concrete_ty, _) = tcx.global_alloc(vtable_alloc_id) else {
        return Err(format!(
            "Trait-object metadata pointed to non-vtable allocation {:?}",
            tcx.global_alloc(vtable_alloc_id)
        ));
    };
    let concrete_ty = EarlyBinder::bind(tcx, concrete_ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let concrete_ty = tcx
        .try_normalize_erasing_regions(
            TypingEnv::fully_monomorphized(),
            Unnormalized::new_wip(concrete_ty),
        )
        .unwrap_or(concrete_ty);
    if !matches!(dynamic_ty.kind(), TyKind::Dynamic(..)) {
        return Err(format!(
            "Trait-object reference requested for non-dynamic type {dynamic_ty:?}"
        ));
    }

    let value = read_pointee_constant(tcx, data_pointer, concrete_ty, oomir_data_types, instance)?;
    let concrete_pointer = if pointer_references_static(tcx, data_pointer) {
        value
    } else {
        pointer_constant_for_pointee(
            tcx,
            data_pointer,
            concrete_ty,
            value,
            oomir_data_types,
            instance,
        )?
    };
    let carrier_ty = oomir::Type::from_constant(&concrete_pointer);
    let oomir::Type::Interface(interface_name) =
        ty_to_oomir_type(dynamic_ty, tcx, oomir_data_types, instance)
    else {
        return Err(format!(
            "Trait-object type {dynamic_ty:?} did not map to a JVM interface"
        ));
    };
    let adapter_class = ensure_trait_object_adapter_class_for_pointees(
        concrete_ty,
        dynamic_ty,
        &carrier_ty,
        &interface_name,
        oomir_data_types,
        tcx,
        instance,
    )?;
    Ok(oomir::Constant::Instance {
        class_name: adapter_class,
        fields: HashMap::default(),
        params: vec![concrete_pointer],
        param_types: vec![carrier_ty],
    })
}

fn read_pointee_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    pointee_ty: Ty<'tcx>,
    oomir_data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let (provenance, offset) = pointer.into_raw_parts();
    let alloc_id = provenance
        .get_alloc_id()
        .ok_or_else(|| format!("Pointer provenance {:?} has no allocation id", provenance))?;

    match tcx.global_alloc(alloc_id) {
        GlobalAlloc::Memory(const_alloc) => {
            let allocation = const_alloc.inner();
            if pointee_ty.is_str() {
                read_string_from_allocation(allocation, offset, None)
            } else {
                read_constant_value_from_memory(
                    tcx,
                    allocation,
                    offset,
                    pointee_ty,
                    oomir_data_types,
                    instance,
                )
            }
        }
        GlobalAlloc::Function { instance } => {
            let func_name = tcx.def_path_str(instance.def_id());
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!("Info: Constant pointer to function: {}", func_name)
            );
            Ok(oomir::Constant::String(format!(
                "FunctionPtr({})",
                func_name
            )))
        }
        GlobalAlloc::Static(def_id) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!("Info: Constant pointer to static: {:?}", def_id)
            );
            Ok(super::super::statics::static_ref_constant(
                tcx,
                def_id,
                oomir_data_types,
                instance,
            ))
        }
        GlobalAlloc::VTable(concrete_ty, dyn_ty) => {
            let concrete_ty = EarlyBinder::bind(tcx, concrete_ty)
                .instantiate(tcx, instance.args)
                .skip_norm_wip();
            let dynamic_predicates = EarlyBinder::bind(tcx, dyn_ty)
                .instantiate(tcx, instance.args)
                .skip_norm_wip();
            let dyn_ty = Ty::new_dynamic(tcx, dynamic_predicates, tcx.lifetimes.re_erased);
            let layout = tcx
                .layout_of(TypingEnv::fully_monomorphized().as_query_input(concrete_ty))
                .map_err(|error| format!("Could not determine vtable layout: {error:?}"))?;
            let concrete_oomir_ty = ty_to_oomir_type(concrete_ty, tcx, oomir_data_types, instance);
            let pointer_oomir_ty = oomir::Type::Pointer(Box::new(concrete_oomir_ty));
            let oomir::Type::Interface(interface_name) =
                ty_to_oomir_type(dyn_ty, tcx, oomir_data_types, instance)
            else {
                return Err(format!(
                    "Vtable dynamic type does not lower to a JVM interface: {dyn_ty:?}"
                ));
            };
            let adapter_class = ensure_trait_object_adapter_class_for_pointees(
                concrete_ty,
                dyn_ty,
                &pointer_oomir_ty,
                &interface_name,
                oomir_data_types,
                tcx,
                instance,
            )?;
            let pointee_codec =
                match pointer_memory_codec_operand(concrete_ty, tcx, oomir_data_types, instance) {
                    oomir::Operand::Constant(codec) => codec,
                    other => {
                        return Err(format!(
                            "Constant vtable pointee codec was not constant: {other:?}"
                        ));
                    }
                };
            Ok(oomir::Constant::StaticCall {
                owner_class: oomir::POINTER_CLASS.to_string(),
                method_name: "constantVtableMarker".to_string(),
                args: vec![
                    oomir::Constant::String(format!("{concrete_ty:?}:{dyn_ty:?}")),
                    oomir::Constant::U64(layout.size.bytes()),
                    oomir::Constant::U64(layout.align.abi.bytes()),
                    oomir::Constant::String(adapter_class),
                    pointee_codec,
                ],
                param_types: Vec::new(),
                ty: oomir::Type::Pointer(Box::new(ty_to_oomir_type(
                    pointee_ty,
                    tcx,
                    oomir_data_types,
                    instance,
                ))),
            })
        }
        GlobalAlloc::TypeId { ty } => Err(format!("Unsupported constant pointer to TypeId {ty:?}")),
    }
}

mod views;
pub(crate) use views::*;

mod scalars;
pub(crate) use scalars::*;

mod objects;
pub(crate) use objects::*;

mod memory;
pub(crate) use memory::*;
