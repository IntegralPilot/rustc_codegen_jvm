use rustc_abi::{FieldIdx, FieldsShape, Size, TagEncoding, VariantIdx, Variants};
use rustc_middle::mir::interpret::{
    AllocId, AllocRange, Allocation, CtfeProvenance, GlobalAlloc, Pointer, Provenance, Scalar,
};
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::{
    AdtDef, EarlyBinder, FloatTy, GenericArgsRef, Instance, InstanceKind, IntTy,
    PseudoCanonicalInput, ScalarInt, ShimKind, Ty, TyCtxt, TyKind, TypingEnv, UintTy,
};
use std::collections::HashMap;

use super::super::{
    control_flow::rvalue::{
        ensure_closure_fn_pointer_adapter_class, ensure_fn_pointer_adapter_class, fn_pointer_target,
    },
    jvm_names, ty_to_oomir_type,
    types::{
        UNION_BYTES_FIELD, UNION_OBJECTS_FIELD, ensure_fn_ptr_interface, ensure_union_data_type,
        fn_ptr_signature_from_ty, generate_adt_jvm_class_name, generate_tuple_jvm_class_name,
        pointer_memory_codec_operand, should_define_named_data_type,
    },
};
use crate::oomir::{self, DataTypeMethod};

type ConstAllocation = Allocation<CtfeProvenance>;

/// Decode the optimized `ConstValue::Slice` representation. Rust uses that
/// representation for every reference whose pointee has a slice tail, not
/// merely for `&str` and `&[T]`.
pub fn read_slice_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    alloc_id: AllocId,
    len: u64,
    pointee_ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let allocation = match tcx.global_alloc(alloc_id) {
        GlobalAlloc::Memory(allocation) => allocation.inner(),
        other => {
            return Err(format!(
                "slice data referred to non-memory allocation {:?}",
                other
            ));
        }
    };

    read_slice_backed_value(
        tcx,
        allocation,
        Size::ZERO,
        len,
        pointee_ty,
        oomir_data_types,
        instance,
    )
}

fn read_slice_backed_value<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    base_offset: Size,
    len: u64,
    ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    match ty.kind() {
        TyKind::Str => read_string_from_allocation(allocation, base_offset, Some(len as usize)),
        TyKind::Slice(element_ty) => {
            let element_layout = tcx
                .layout_of(TypingEnv::fully_monomorphized().as_query_input(*element_ty))
                .map_err(|error| {
                    format!(
                        "could not get slice element layout for {:?}: {:?}",
                        element_ty, error
                    )
                })?;
            let element_type = ty_to_oomir_type(*element_ty, tcx, oomir_data_types, instance);
            let mut elements = Vec::with_capacity(len as usize);

            for index in 0..len {
                let element_offset = element_layout
                    .size
                    .checked_mul(index, &tcx.data_layout)
                    .ok_or_else(|| format!("slice offset overflow at element {}", index))?;
                elements.push(read_constant_value_from_memory(
                    tcx,
                    allocation,
                    base_offset + element_offset,
                    *element_ty,
                    oomir_data_types,
                    instance,
                )?);
            }

            Ok(oomir::Constant::Slice(Box::new(element_type), elements))
        }
        TyKind::Adt(adt_def, args) if adt_def.is_struct() && adt_def.repr().transparent() => {
            let variant = adt_def.variant(VariantIdx::from_usize(0));
            if variant.fields.len() != 1 {
                return Err(format!(
                    "transparent slice-tailed type {:?} has {} fields; only single-field wrappers are currently representable",
                    ty,
                    variant.fields.len()
                ));
            }

            let field = &variant.fields[FieldIdx::from_usize(0)];
            let field_ty = tcx
                .normalize_erasing_regions(TypingEnv::fully_monomorphized(), field.ty(tcx, args));
            let expected_tail = tcx.struct_tail_for_codegen(ty, TypingEnv::fully_monomorphized());
            let field_tail =
                tcx.struct_tail_for_codegen(field_ty, TypingEnv::fully_monomorphized());
            if field_tail != expected_tail {
                return Err(format!(
                    "transparent field {:?} does not contain the slice tail {:?}",
                    field_ty, expected_tail
                ));
            }

            let inner = read_slice_backed_value(
                tcx,
                allocation,
                base_offset,
                len,
                field_ty,
                oomir_data_types,
                instance,
            )?;
            let class_name = match ty_to_oomir_type(ty, tcx, oomir_data_types, instance) {
                oomir::Type::Class(class_name) => class_name,
                other => {
                    return Err(format!(
                        "slice-tailed wrapper {:?} mapped to non-class type {:?}",
                        ty, other
                    ));
                }
            };
            let mut fields = HashMap::new();
            fields.insert(field.ident(tcx).to_string(), inner.clone());
            Ok(oomir::Constant::Instance {
                class_name,
                fields,
                params: vec![inner],
            })
        }
        _ => Err(format!(
            "unsupported slice-backed pointee type {:?}; expected str, a slice, or a single-field transparent wrapper",
            ty
        )),
    }
}

pub fn read_scalar_int_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    scalar_int: ScalarInt,
    ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let ty = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    if tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .map(|layout| layout.is_zst())
        .unwrap_or(false)
    {
        return read_zero_sized_constant(tcx, ty, oomir_data_types, instance);
    }

    if let TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) = ty.kind()
        && matches!(
            ty_to_oomir_type(ty, tcx, oomir_data_types, instance),
            oomir::Type::Pointer(_)
        )
    {
        let layout = tcx
            .layout_of(TypingEnv::fully_monomorphized().as_query_input(*pointee))
            .map_err(|error| format!("Could not determine pointee layout for {ty:?}: {error:?}"))?;
        return Ok(oomir::Constant::Instance {
            class_name: oomir::POINTER_CLASS.to_string(),
            fields: HashMap::new(),
            params: vec![
                oomir::Constant::U64(scalar_int.to_target_usize(tcx) as u64),
                oomir::Constant::I32(
                    i32::try_from(layout.size.bytes())
                        .map_err(|_| format!("Pointee layout for {ty:?} exceeds JVM limits"))?,
                ),
            ],
        });
    }

    if let TyKind::Closure(_, closure_args) = ty.kind() {
        let class_name = match ty_to_oomir_type(ty, tcx, oomir_data_types, instance) {
            oomir::Type::Class(class_name) => class_name,
            other => {
                return Err(format!(
                    "Scalar closure {ty:?} did not map to a JVM class: {other:?}"
                ));
            }
        };
        let mut fields = HashMap::new();
        let mut params = Vec::new();
        let mut non_zst_captures = 0usize;
        for (index, capture_ty) in closure_args.as_closure().upvar_tys().iter().enumerate() {
            let capture_ty = EarlyBinder::bind(tcx, capture_ty)
                .instantiate(tcx, instance.args)
                .skip_norm_wip();
            let capture_layout = tcx
                .layout_of(TypingEnv::fully_monomorphized().as_query_input(capture_ty))
                .map_err(|error| {
                    format!(
                        "Could not determine closure capture layout for {capture_ty:?}: {error:?}"
                    )
                })?;
            let capture_jvm_ty = ty_to_oomir_type(capture_ty, tcx, oomir_data_types, instance);
            if !capture_jvm_ty.has_jvm_value() {
                continue;
            }
            let capture = if capture_layout.is_zst() {
                read_zero_sized_constant(tcx, capture_ty, oomir_data_types, instance)?
            } else {
                non_zst_captures += 1;
                read_scalar_int_constant(tcx, scalar_int, capture_ty, oomir_data_types, instance)?
            };
            fields.insert(format!("arg{index}"), capture.clone());
            params.push(capture);
        }
        if non_zst_captures != 1 {
            return Err(format!(
                "Scalar closure {ty:?} has {non_zst_captures} non-ZST captures, expected exactly one"
            ));
        }
        return Ok(oomir::Constant::Instance {
            class_name,
            fields,
            params,
        });
    }

    if let TyKind::Adt(adt_def, substs) = ty.kind() {
        if adt_def.is_enum() {
            // A scalar enum constant is the enum's physical ABI carrier, not
            // necessarily its source-level discriminant. Niche-encoded enums
            // in optimized MIR rely on both the exact bits and their width.
            // Preserve that width so value adaptation can reconstruct the enum
            // through the exact-layout codec instead of widening it to i64.
            let carrier_ty = match scalar_int.size().bytes() {
                1 => tcx.types.u8,
                2 => tcx.types.u16,
                4 => tcx.types.u32,
                8 => tcx.types.u64,
                16 => tcx.types.u128,
                size => {
                    return Err(format!(
                        "Unsupported {size}-byte scalar carrier for enum {ty:?}"
                    ));
                }
            };
            return Ok(scalar_int_to_oomir_constant(scalar_int, carrier_ty));
        }

        let adt_name = match ty_to_oomir_type(ty, tcx, oomir_data_types, instance) {
            oomir::Type::Class(class_name) => class_name,
            other => {
                return Err(format!(
                    "Expected class type for scalar ADT constant {:?}, got {:?}",
                    ty, other
                ));
            }
        };

        let variant = adt_def
            .variants()
            .iter()
            .next()
            .ok_or_else(|| format!("Transparent ADT {:?} has no variants", ty))?;
        let non_zst_fields = variant
            .fields
            .iter()
            .filter(|field_def| {
                !tcx.layout_of(PseudoCanonicalInput {
                    typing_env: TypingEnv::post_analysis(tcx, field_def.did),
                    value: field_def.ty(tcx, substs).skip_norm_wip(),
                })
                .map(|layout| layout.is_zst())
                .unwrap_or(false)
            })
            .collect::<Vec<_>>();

        if non_zst_fields.len() != 1 {
            return Err(format!(
                "Transparent ADT {:?} has {} non-ZST fields, expected exactly one",
                ty,
                non_zst_fields.len()
            ));
        }

        let field_def = non_zst_fields[0];
        let field_name = field_def.ident(tcx).name.to_string();
        let unnormalized_field_ty = field_def.ty(tcx, substs);
        let field_ty = tcx
            .try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), unnormalized_field_ty)
            .map_err(|error| {
                format!(
                    "Could not normalize constant field {} of type {:?}: {:?}",
                    field_def.ident(tcx),
                    unnormalized_field_ty,
                    error
                )
            })?;
        let inner_constant =
            read_scalar_int_constant(tcx, scalar_int, field_ty, oomir_data_types, instance)?;
        let mut fields = HashMap::new();
        fields.insert(field_name, inner_constant.clone());
        return Ok(oomir::Constant::Instance {
            class_name: adt_name,
            fields,
            params: vec![inner_constant],
        });
    }

    Ok(scalar_int_to_oomir_constant(scalar_int, ty))
}

pub fn read_zero_sized_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let ty = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .map_err(|error| format!("Could not determine ZST layout for {ty:?}: {error:?}"))?;
    if !layout.is_zst() {
        return Err(format!("Type {ty:?} is not zero-sized"));
    }

    let oomir_ty = ty_to_oomir_type(ty, tcx, oomir_data_types, instance);
    if !oomir_ty.has_jvm_value() {
        return Ok(oomir::Constant::Unit);
    }

    let make_instance = |class_name: String, named_values: Vec<(String, oomir::Constant)>| {
        let fields = named_values.iter().cloned().collect::<HashMap<_, _>>();
        let params = named_values.into_iter().map(|(_, value)| value).collect();
        oomir::Constant::Instance {
            class_name,
            fields,
            params,
        }
    };

    match ty.kind() {
        TyKind::FnDef(..) => {
            let oomir::Type::Class(class_name) = oomir_ty else {
                return Err(format!("ZST function item {ty:?} did not map to a class"));
            };
            Ok(make_instance(class_name, Vec::new()))
        }
        TyKind::Closure(_, closure_args) => {
            let oomir::Type::Class(class_name) = oomir_ty else {
                return Err(format!("ZST closure {ty:?} did not map to a class"));
            };
            let mut values = Vec::new();
            for (index, capture_ty) in closure_args.as_closure().upvar_tys().iter().enumerate() {
                let capture_ty = EarlyBinder::bind(tcx, capture_ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                let capture_jvm_ty = ty_to_oomir_type(capture_ty, tcx, oomir_data_types, instance);
                if capture_jvm_ty.has_jvm_value() {
                    values.push((
                        format!("arg{index}"),
                        read_zero_sized_constant(tcx, capture_ty, oomir_data_types, instance)?,
                    ));
                }
            }
            Ok(make_instance(class_name, values))
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_struct() => {
            let oomir::Type::Class(class_name) = oomir_ty else {
                return Err(format!("ZST struct {ty:?} did not map to a class"));
            };
            let mut values = Vec::new();
            for field in &adt_def.variant(VariantIdx::from_usize(0)).fields {
                let field_ty = EarlyBinder::bind(tcx, field.ty(tcx, substs).skip_norm_wip())
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                let field_jvm_ty = ty_to_oomir_type(field_ty, tcx, oomir_data_types, instance);
                if field_jvm_ty.has_jvm_value() {
                    values.push((
                        field.ident(tcx).to_string(),
                        read_zero_sized_constant(tcx, field_ty, oomir_data_types, instance)?,
                    ));
                }
            }
            Ok(make_instance(class_name, values))
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_union() => {
            let class_name =
                ensure_union_data_type(adt_def, substs, tcx, oomir_data_types, instance);
            let bytes = oomir::Constant::Array(Box::new(oomir::Type::I8), Vec::new());
            let objects = oomir::Constant::Array(
                Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                vec![oomir::Constant::Null(oomir::Type::Class(
                    "java/lang/Object".to_string(),
                ))],
            );
            Ok(oomir::Constant::Instance {
                class_name,
                fields: HashMap::from([
                    (UNION_BYTES_FIELD.to_string(), bytes.clone()),
                    (UNION_OBJECTS_FIELD.to_string(), objects.clone()),
                ]),
                params: vec![bytes, objects],
            })
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_enum() => {
            let Variants::Single { index } = layout.variants else {
                return Err(format!(
                    "ZST enum {ty:?} does not have one known active variant"
                ));
            };
            let oomir::Type::Class(base_class) = oomir_ty else {
                return Err(format!("ZST enum {ty:?} did not map to a class"));
            };
            let variant = adt_def.variant(index);
            let class_name = format!(
                "{}${}",
                base_class,
                jvm_names::member_name(&variant.name.to_string())
            );
            let mut values = Vec::new();
            let mut jvm_index = 0usize;
            for field in &variant.fields {
                let field_ty = EarlyBinder::bind(tcx, field.ty(tcx, substs).skip_norm_wip())
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                let field_jvm_ty = ty_to_oomir_type(field_ty, tcx, oomir_data_types, instance);
                if field_jvm_ty.has_jvm_value() {
                    values.push((
                        format!("field{jvm_index}"),
                        read_zero_sized_constant(tcx, field_ty, oomir_data_types, instance)?,
                    ));
                    jvm_index += 1;
                }
            }
            Ok(make_instance(class_name, values))
        }
        TyKind::Tuple(elements) => {
            let element_tys = elements.iter().collect::<Vec<_>>();
            if element_tys.is_empty() {
                return Ok(oomir::Constant::Unit);
            }
            let oomir::Type::Class(class_name) = oomir_ty else {
                return Err(format!("ZST tuple {ty:?} did not map to a class"));
            };
            let mut values = Vec::new();
            for (index, element_ty) in element_tys.into_iter().enumerate() {
                let element_jvm_ty = ty_to_oomir_type(element_ty, tcx, oomir_data_types, instance);
                if element_jvm_ty.has_jvm_value() {
                    values.push((
                        format!("field{index}"),
                        read_zero_sized_constant(tcx, element_ty, oomir_data_types, instance)?,
                    ));
                }
            }
            Ok(make_instance(class_name, values))
        }
        TyKind::Array(element_ty, length) => {
            let length = length
                .try_to_target_usize(tcx)
                .ok_or_else(|| format!("ZST array length is not concrete for {ty:?}"))?;
            let element_jvm_ty = ty_to_oomir_type(*element_ty, tcx, oomir_data_types, instance);
            let mut elements = Vec::with_capacity(length as usize);
            for _ in 0..length {
                elements.push(if element_jvm_ty.has_jvm_value() {
                    read_zero_sized_constant(tcx, *element_ty, oomir_data_types, instance)?
                } else {
                    oomir::Constant::Unit
                });
            }
            Ok(oomir::Constant::Array(Box::new(element_jvm_ty), elements))
        }
        _ => Err(format!("Unsupported nominal ZST constant type: {ty:?}")),
    }
}

pub fn read_pointer_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    match ty.kind() {
        TyKind::FnPtr(..) => {
            read_function_pointer_constant(tcx, pointer, ty, oomir_data_types, instance)
        }
        TyKind::Ref(_, inner_ty, _) if inner_ty.is_array() => {
            let value = read_pointee_constant(tcx, pointer, *inner_ty, oomir_data_types, instance)?;
            array_reference_to_slice(tcx, *inner_ty, value, oomir_data_types, instance)
        }
        TyKind::Ref(_, inner_ty, _) | TyKind::RawPtr(inner_ty, _) => {
            read_pointee_constant(tcx, pointer, *inner_ty, oomir_data_types, instance)
        }
        _ => read_pointee_constant(tcx, pointer, ty, oomir_data_types, instance),
    }
}

fn read_function_pointer_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
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
        let callable_target = fn_pointer_target(tcx, function_instance, &signature);
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

fn array_reference_to_slice<'tcx>(
    tcx: TyCtxt<'tcx>,
    array_ty: Ty<'tcx>,
    value: oomir::Constant,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let TyKind::Array(element_ty, _) = array_ty.kind() else {
        return Err(format!("Expected array type, found {array_ty:?}"));
    };
    let value = match value {
        oomir::Constant::Array(element_type, elements) => {
            return Ok(oomir::Constant::Slice(element_type, elements));
        }
        other => other,
    };
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(array_ty))
        .map_err(|error| format!("Could not get array layout for {array_ty:?}: {error:?}"))?;
    let FieldsShape::Array { count, .. } = layout.fields else {
        return Err(format!("Array type {array_ty:?} had layout {layout:?}"));
    };
    Ok(oomir::Constant::SliceRef {
        backing: Box::new(value),
        element_type: Box::new(ty_to_oomir_type(
            *element_ty,
            tcx,
            oomir_data_types,
            instance,
        )),
        length: count,
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

fn read_pointee_constant<'tcx>(
    tcx: TyCtxt<'tcx>,
    pointer: Pointer<CtfeProvenance>,
    pointee_ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
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
        GlobalAlloc::VTable(..) => Err("Unsupported constant pointer to vtable".to_string()),
        GlobalAlloc::TypeId { ty } => Err(format!("Unsupported constant pointer to TypeId {ty:?}")),
    }
}

fn read_str_from_fat_pointer<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
) -> Result<oomir::Constant, String> {
    let pointer_size = tcx.data_layout.pointer_size();
    let data_ptr = read_pointer_from_memory(tcx, allocation, offset)?;
    let len_range = AllocRange {
        start: offset + pointer_size,
        size: pointer_size,
    };
    let len_scalar = allocation
        .read_scalar(&tcx.data_layout, len_range, false)
        .map_err(|e| format!("Failed to read str length at {:?}: {:?}", offset, e))?;
    let len = match len_scalar {
        Scalar::Int(len) => len.to_target_usize(tcx) as usize,
        Scalar::Ptr(..) => {
            return Err(format!(
                "Expected integer str length at {:?}, found pointer",
                offset + pointer_size
            ));
        }
    };

    let (provenance, data_offset) = data_ptr.into_raw_parts();
    let alloc_id = provenance.get_alloc_id().ok_or_else(|| {
        format!(
            "String data pointer provenance {:?} has no allocation id",
            provenance
        )
    })?;
    match tcx.global_alloc(alloc_id) {
        GlobalAlloc::Memory(const_alloc) => {
            read_string_from_allocation(const_alloc.inner(), data_offset, Some(len))
        }
        other => Err(format!(
            "String data pointer referenced non-memory allocation {:?}",
            other
        )),
    }
}

fn read_slice_from_fat_pointer<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
    slice_ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let pointer_size = tcx.data_layout.pointer_size();
    let data_ptr = read_pointer_from_memory(tcx, allocation, offset)?;
    let len_scalar = allocation
        .read_scalar(
            &tcx.data_layout,
            AllocRange {
                start: offset + pointer_size,
                size: pointer_size,
            },
            false,
        )
        .map_err(|error| format!("Failed to read slice length at {:?}: {:?}", offset, error))?;
    let len = match len_scalar {
        Scalar::Int(len) => len.to_target_usize(tcx),
        Scalar::Ptr(..) => {
            return Err(format!(
                "Expected integer slice length at {:?}, found pointer",
                offset + pointer_size
            ));
        }
    };

    let (provenance, data_offset) = data_ptr.into_raw_parts();
    let alloc_id = provenance.get_alloc_id().ok_or_else(|| {
        format!(
            "Slice data pointer provenance {:?} has no allocation id",
            provenance
        )
    })?;
    match tcx.global_alloc(alloc_id) {
        GlobalAlloc::Memory(const_alloc) => read_slice_backed_value(
            tcx,
            const_alloc.inner(),
            data_offset,
            len,
            slice_ty,
            oomir_data_types,
            instance,
        ),
        other => Err(format!(
            "Slice data pointer referenced non-memory allocation {:?}",
            other
        )),
    }
}

fn read_string_from_allocation(
    allocation: &ConstAllocation,
    offset: Size,
    len: Option<usize>,
) -> Result<oomir::Constant, String> {
    let start = offset.bytes_usize();
    let alloc_size = allocation.size().bytes_usize();
    let end = match len {
        Some(len) => start
            .checked_add(len)
            .ok_or_else(|| format!("String byte range starting at {} overflowed", start))?,
        None => alloc_size,
    };
    if end > alloc_size {
        return Err(format!(
            "String byte range {}..{} is outside allocation size {}",
            start, end, alloc_size
        ));
    }

    let bytes = allocation.inspect_with_uninit_and_ptr_outside_interpreter(start..end);
    match String::from_utf8(bytes.to_vec()) {
        Ok(s) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!("Info: Successfully extracted string constant: \"{}\"", s)
            );
            Ok(oomir::Constant::Str(s))
        }
        Err(e) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "const-eval",
                format!("Warning: String bytes were not valid UTF-8: {}", e)
            );
            Ok(oomir::Constant::Str("Invalid UTF8".to_string()))
        }
    }
}

/// Reads a constant value of type `ty` from the `allocation` starting at `offset`.
pub fn read_constant_value_from_memory<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &ConstAllocation,
    offset: Size,
    ty: Ty<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let pci = TypingEnv::fully_monomorphized().as_query_input(ty);
    let layout = tcx
        .layout_of(pci)
        .map_err(|_| "Couldn't get layout.".to_string())?;

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "const-eval",
        format!(
            "Debug: Reading constant value for type {:?} at offset {:?} with layout size {:?}",
            ty, offset, layout.size
        )
    );

    match ty.kind() {
        TyKind::Pat(base_ty, _) => read_constant_value_from_memory(
            tcx,
            allocation,
            offset,
            *base_ty,
            oomir_data_types,
            instance,
        ),
        TyKind::Bool | TyKind::Char | TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_) => {
            let range = AllocRange {
                start: offset,
                size: layout.size,
            };
            // Read as ScalarInt - floats are represented by their bits
            let scalar = allocation
                .read_scalar(&tcx.data_layout, range, false)
                .map_err(|e| {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Error,
                        "const-eval",
                        format!("Error reading scalar: {:?}", e)
                    );
                    "Failed to read scalar".to_string()
                })?;
            let scalar_int = match scalar {
                Scalar::Int(int) => int,
                Scalar::Ptr(_, _) => {
                    return Err(format!(
                        "Expected scalar integer for type {:?}, found pointer",
                        ty
                    ));
                }
            };
            read_scalar_int_constant(tcx, scalar_int, ty, oomir_data_types, instance)
        }

        TyKind::Ref(_, inner_ty, _) => {
            if inner_ty.is_str() || inner_ty.is_slice() {
                let value = if inner_ty.is_str() {
                    read_str_from_fat_pointer(tcx, allocation, offset)?
                } else {
                    read_slice_from_fat_pointer(
                        tcx,
                        allocation,
                        offset,
                        *inner_ty,
                        oomir_data_types,
                        instance,
                    )?
                };
                Ok(value)
            } else {
                let ptr = read_pointer_from_memory(tcx, allocation, offset)?;
                let value = read_pointee_constant(tcx, ptr, *inner_ty, oomir_data_types, instance)?;
                if inner_ty.is_array() {
                    return array_reference_to_slice(
                        tcx,
                        *inner_ty,
                        value,
                        oomir_data_types,
                        instance,
                    );
                }
                if matches!(oomir::Type::from_constant(&value), oomir::Type::Pointer(_)) {
                    // References to statics already are their canonical stable
                    // Pointer. Wrapping that address would create Pointer<Pointer<T>>.
                    return Ok(value);
                }
                if matches!(inner_ty.kind(), TyKind::Dynamic(..)) {
                    return Ok(value);
                }
                let pointee_layout = tcx
                    .layout_of(TypingEnv::fully_monomorphized().as_query_input(*inner_ty))
                    .map_err(|error| {
                        format!("Could not determine constant reference layout: {error:?}")
                    })?;
                let codec = match pointer_memory_codec_operand(
                    *inner_ty,
                    tcx,
                    oomir_data_types,
                    instance,
                ) {
                    oomir::Operand::Constant(codec) => codec,
                    other => {
                        return Err(format!(
                            "Constant reference codec was not constant: {other:?}"
                        ));
                    }
                };
                Ok(oomir::Constant::Instance {
                    class_name: oomir::POINTER_CLASS.to_string(),
                    fields: HashMap::new(),
                    params: vec![
                        if oomir::Type::from_constant(&value).has_jvm_value() {
                            value
                        } else {
                            oomir::Constant::Null(oomir::Type::Class(
                                "java/lang/Object".to_string(),
                            ))
                        },
                        oomir::Constant::I32(i32::try_from(pointee_layout.size.bytes()).map_err(
                            |_| "Constant reference pointee exceeds JVM limits".to_string(),
                        )?),
                        codec,
                    ],
                })
            }
        }

        TyKind::RawPtr(inner_ty, _) => {
            if inner_ty.is_str() {
                read_str_from_fat_pointer(tcx, allocation, offset)
            } else if inner_ty.is_slice() {
                Err("Unsupported raw slice pointer constant".to_string())
            } else {
                let pointer_size = tcx.data_layout.pointer_size();
                let scalar = allocation
                    .read_scalar(
                        &tcx.data_layout,
                        AllocRange {
                            start: offset,
                            size: pointer_size,
                        },
                        true,
                    )
                    .map_err(|error| {
                        format!("Failed to read raw pointer {ty:?} at {offset:?}: {error:?}")
                    })?;
                match scalar {
                    Scalar::Int(address) => {
                        read_scalar_int_constant(tcx, address, ty, oomir_data_types, instance)
                    }
                    Scalar::Ptr(pointer, _) => {
                        let (provenance, pointer_offset) = pointer.into_raw_parts();
                        if provenance.get_alloc_id().is_some_and(|alloc_id| {
                            matches!(tcx.global_alloc(alloc_id), GlobalAlloc::TypeId { .. })
                        }) {
                            let pointee_layout = tcx
                                .layout_of(
                                    TypingEnv::fully_monomorphized().as_query_input(*inner_ty),
                                )
                                .map_err(|error| {
                                    format!(
                                        "Could not determine pointee layout for {ty:?}: {error:?}"
                                    )
                                })?;
                            Ok(oomir::Constant::Instance {
                                class_name: oomir::POINTER_CLASS.to_string(),
                                fields: HashMap::new(),
                                // TypeId encodes its hash bytes in the offsets of
                                // provenance-only pointers. At runtime those are
                                // ordinary exposed-address pointer bits.
                                params: vec![
                                    oomir::Constant::U64(pointer_offset.bytes()),
                                    oomir::Constant::I32(
                                        i32::try_from(pointee_layout.size.bytes()).map_err(
                                            |_| {
                                                format!(
                                                    "Pointee layout for {ty:?} exceeds JVM limits"
                                                )
                                            },
                                        )?,
                                    ),
                                ],
                            })
                        } else {
                            let value = read_pointee_constant(
                                tcx,
                                pointer,
                                *inner_ty,
                                oomir_data_types,
                                instance,
                            )?;
                            let pointee_type =
                                ty_to_oomir_type(*inner_ty, tcx, oomir_data_types, instance);
                            Ok(oomir::Constant::Array(Box::new(pointee_type), vec![value]))
                        }
                    }
                }
            }
        }

        TyKind::FnPtr(..) => {
            let pointer = read_pointer_from_memory(tcx, allocation, offset)?;
            read_function_pointer_constant(tcx, pointer, ty, oomir_data_types, instance)
        }

        TyKind::Str => Err("Unsupported type: Direct read of str from memory".to_string()),

        TyKind::Array(elem_ty, _) => {
            let FieldsShape::Array { count: len, .. } = layout.fields else {
                return Err(format!(
                    "Array type {:?} had non-array layout {:?}",
                    ty, layout
                ));
            };
            let elem_pci = TypingEnv::fully_monomorphized().as_query_input(*elem_ty);
            let elem_layout = tcx
                .layout_of(elem_pci)
                .map_err(|_| "Couldn't get element layout.".to_string())?;
            let oomir_elem_type = ty_to_oomir_type(*elem_ty, tcx, oomir_data_types, instance);

            let mut values = Vec::with_capacity(len as usize);
            for i in 0..len {
                let elem_offset =
                    offset + elem_layout.size.checked_mul(i, &tcx.data_layout).unwrap();
                let elem_const = read_constant_value_from_memory(
                    tcx,
                    allocation,
                    elem_offset,
                    *elem_ty,
                    oomir_data_types,
                    instance,
                )?;
                values.push(elem_const);
            }

            Ok(oomir::Constant::Array(Box::new(oomir_elem_type), values))
        }

        TyKind::Slice(_) => Err("Unsupported type: Direct read of slice from memory".to_string()),

        TyKind::Adt(adt_def, substs) => {
            if adt_def.is_struct() {
                handle_constant_struct(
                    tcx,
                    allocation,
                    offset,
                    layout,
                    *adt_def,
                    substs,
                    oomir_data_types,
                    instance,
                )
            } else if adt_def.is_enum() {
                handle_constant_enum(
                    tcx,
                    allocation,
                    offset,
                    ty,
                    layout,
                    *adt_def,
                    substs,
                    oomir_data_types,
                    instance,
                )
            } else if adt_def.is_union() {
                let class_name =
                    ensure_union_data_type(adt_def, substs, tcx, oomir_data_types, instance);
                let start = offset.bytes_usize();
                let end = start
                    .checked_add(layout.size.bytes_usize())
                    .ok_or_else(|| format!("Union constant range overflow for {ty:?}"))?;
                let bytes = allocation
                    .inspect_with_uninit_and_ptr_outside_interpreter(start..end)
                    .iter()
                    .map(|byte| oomir::Constant::I8(*byte as i8))
                    .collect::<Vec<_>>();
                let objects = (0..layout.size.bytes_usize())
                    .map(|_| {
                        oomir::Constant::Null(oomir::Type::Class("java/lang/Object".to_string()))
                    })
                    .collect::<Vec<_>>();
                let bytes = oomir::Constant::Array(Box::new(oomir::Type::I8), bytes);
                let objects = oomir::Constant::Array(
                    Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                    objects,
                );
                Ok(oomir::Constant::Instance {
                    class_name,
                    fields: HashMap::from([
                        (UNION_BYTES_FIELD.to_string(), bytes.clone()),
                        (UNION_OBJECTS_FIELD.to_string(), objects.clone()),
                    ]),
                    params: vec![bytes, objects],
                })
            } else {
                Err(format!("Unsupported ADT constant type: {ty:?}"))
            }
        }

        TyKind::Tuple(field_tys) => {
            if field_tys.is_empty() {
                return Ok(oomir::Constant::Unit);
            }
            let mut fields_map = HashMap::new();
            let mut params = Vec::new();
            match layout.fields {
                FieldsShape::Arbitrary { ref offsets, .. } => {
                    for (i, field_ty) in field_tys.iter().enumerate() {
                        let field_offset = offsets[FieldIdx::from_usize(i)];
                        let field_const = read_constant_value_from_memory(
                            tcx,
                            allocation,
                            offset + field_offset,
                            field_ty,
                            oomir_data_types,
                            instance,
                        )?;
                        params.push(field_const.clone());
                        fields_map.insert(format!("field{}", i), field_const);
                    }
                }
                _ => return Err("Unsupported tuple layout".to_string()),
            }
            let tuple_class_name =
                generate_tuple_jvm_class_name(field_tys, tcx, oomir_data_types, instance);
            Ok(oomir::Constant::Instance {
                class_name: tuple_class_name,
                fields: fields_map,
                params,
            })
        }

        TyKind::Closure(_, closure_args) => {
            let class_name = match ty_to_oomir_type(ty, tcx, oomir_data_types, instance) {
                oomir::Type::Class(class_name) => class_name,
                other => {
                    return Err(format!(
                        "Closure constant {ty:?} did not map to a JVM class: {other:?}"
                    ));
                }
            };
            let capture_tys = closure_args.as_closure().upvar_tys();
            let mut fields = HashMap::new();
            let mut params = Vec::new();
            for (index, capture_ty) in capture_tys.iter().enumerate() {
                let capture_offset = layout.fields.offset(index);
                let capture = read_constant_value_from_memory(
                    tcx,
                    allocation,
                    offset + capture_offset,
                    capture_ty,
                    oomir_data_types,
                    instance,
                )?;
                fields.insert(format!("arg{index}"), capture.clone());
                params.push(capture);
            }
            Ok(oomir::Constant::Instance {
                class_name,
                fields,
                params,
            })
        }

        _ => Err(format!("Unsupported constant type: {:?}", ty)),
    }
}

fn handle_constant_struct<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &rustc_middle::mir::interpret::Allocation,
    offset: Size,
    layout: TyAndLayout<'tcx>,
    adt_def: AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let variant = adt_def.variant(VariantIdx::from_usize(0)); // Structs have one variant
    let mut fields_map = HashMap::new();
    let mut params = Vec::new();

    for (i, field_def) in variant.fields.iter().enumerate() {
        let field_idx = FieldIdx::from_usize(i);
        let unnormalized_field_ty = field_def.ty(tcx, substs);
        let field_ty = tcx
            .try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), unnormalized_field_ty)
            .map_err(|error| {
                format!(
                    "Could not normalize constant field {} of type {:?}: {:?}",
                    field_def.ident(tcx),
                    unnormalized_field_ty,
                    error
                )
            })?;
        let field_offset = layout.fields.offset(field_idx.into());
        let field_name = field_def.ident(tcx).to_string();

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "const-eval",
            format!(
                "Debug: Reading struct field '{}' ({:?}) at offset {:?}",
                field_name,
                field_ty,
                offset + field_offset
            )
        );

        let field_const = read_constant_value_from_memory(
            tcx,
            allocation,
            offset + field_offset,
            field_ty,
            oomir_data_types,
            instance,
        )?;
        params.push(field_const.clone());
        fields_map.insert(field_name, field_const);
    }

    let class_name = generate_adt_jvm_class_name(&adt_def, substs, tcx, oomir_data_types, instance);

    Ok(oomir::Constant::Instance {
        class_name,
        fields: fields_map,
        params,
    })
}

fn handle_constant_enum<'tcx>(
    tcx: TyCtxt<'tcx>,
    allocation: &rustc_middle::mir::interpret::Allocation<
        rustc_middle::mir::interpret::CtfeProvenance, // Explicit provenance type
    >,
    offset: Size,
    enum_ty: Ty<'tcx>, // Keep enum_ty for context/errors if needed
    layout: TyAndLayout<'tcx>,
    adt_def: AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    oomir_data_types: &mut HashMap<String, oomir::DataType>,
    instance: Instance<'tcx>,
) -> Result<oomir::Constant, String> {
    let active_variant_idx: VariantIdx;
    match &layout.variants {
        Variants::Single { index } => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!(
                    "Debug: Enum {:?} has single variant layout (index {:?})",
                    adt_def.did(),
                    index
                )
            );
            active_variant_idx = *index;
        }

        Variants::Multiple {
            tag, // This is the Scalar layout for the tag's storage location
            tag_encoding,
            tag_field, // Index within layout.fields where the tag is stored
            variants: _variant_layouts,
        } => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!(
                    "Debug: Enum {:?} has multiple variant layout. Tag Encoding: {:?}",
                    adt_def.did(),
                    tag_encoding
                )
            );

            let tag_scalar_layout = tag;
            let tag_size = tag_scalar_layout.size(&tcx.data_layout);

            let tag_offset_in_enum = layout.fields.offset((*tag_field).into());
            let absolute_tag_offset = offset + tag_offset_in_enum;
            let absolute_tag_range = AllocRange {
                start: absolute_tag_offset,
                size: tag_size,
            };

            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!(
                    "Debug: Reading tag/niche value for {:?} (storage type {:?}, size {:?}) at offset {:?} (relative offset {:?}, tag_field index {})",
                    enum_ty,
                    tag_scalar_layout.primitive(),
                    tag_size,
                    absolute_tag_offset,
                    tag_offset_in_enum,
                    usize::from(*tag_field)
                )
            );

            let tag_scalar = allocation
                .read_scalar(&tcx.data_layout, absolute_tag_range, false)
                .map_err(|e| format!("Failed to read enum tag/niche for {:?}: {:?}", enum_ty, e))?;

            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!("Debug: Read tag scalar: {:?}", tag_scalar)
            );

            match tag_encoding {
                TagEncoding::Direct => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "const-eval",
                        "Debug: Using Direct tag encoding"
                    );
                    let tag_val = match tag_scalar {
                        Scalar::Int(int) => int,
                        Scalar::Ptr(..) => {
                            return Err(format!(
                                "Enum tag for {:?} with Direct encoding read as pointer, expected integer",
                                enum_ty
                            ));
                        }
                    };

                    if tag_val.size() != tag_size {
                        return Err(format!(
                            "Direct Tag size mismatch for {:?}: read {:?} bytes, but expected size {:?}",
                            enum_ty,
                            tag_val.size(),
                            tag_size
                        ));
                    }
                    let read_tag_bits = tag_val.to_bits(tag_size);
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "const-eval",
                        format!(
                            "Debug: Read Direct tag value: {:?}, bits: {:#x}",
                            tag_val, read_tag_bits
                        )
                    );

                    let mut found_idx = None;
                    for (v_idx, v_discr) in adt_def.discriminants(tcx) {
                        let mask = if tag_size.bits() == 128 {
                            u128::MAX
                        } else {
                            (1u128 << tag_size.bits()) - 1
                        };
                        let canonical_discr_val_masked = v_discr.val & mask;
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "const-eval",
                            format!(
                                "Debug: Comparing read_tag_bits {:#x} with variant {:?} discriminant {:#x} (masked: {:#x})",
                                read_tag_bits, v_idx, v_discr.val, canonical_discr_val_masked
                            )
                        );
                        if read_tag_bits == canonical_discr_val_masked {
                            if found_idx.is_some() {
                                return Err(format!("Ambiguous match found for enum variant"));
                            }
                            found_idx = Some(v_idx);
                            break;
                        }
                    }
                    active_variant_idx =
                        found_idx.ok_or_else(|| "No matching variant found".to_string())?;
                } // End Direct Encoding

                TagEncoding::Niche {
                    untagged_variant,
                    niche_variants,
                    niche_start,
                } => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "const-eval",
                        format!(
                            "Debug: Using Niche tag encoding. Untagged: {:?}, Niche variants: {:?}, Niche start: {:#x}",
                            untagged_variant, niche_variants, niche_start
                        )
                    );

                    let read_value_bits = match tag_scalar {
                        Scalar::Int(int) => {
                            if int.size() != tag_size {
                                return Err(format!(
                                    "Niche integer tag size mismatch for {:?}: read {:?} bytes, but expected size {:?}",
                                    enum_ty,
                                    int.size(),
                                    tag_size
                                ));
                            }
                            int.to_bits(tag_size)
                        }
                        Scalar::Ptr(ptr, _meta) => {
                            if tag_size != tcx.data_layout.pointer_size() {
                                return Err(format!(
                                    "Niche pointer tag size mismatch for {:?}: pointer size is {:?}, but tag size is {:?}",
                                    enum_ty,
                                    tcx.data_layout.pointer_size(),
                                    tag_size
                                ));
                            }
                            ptr.into_raw_parts().1.bytes() as u128
                        }
                    };
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "const-eval",
                        format!("Debug: Read Niche value bits: {:#x}", read_value_bits)
                    );

                    let tag_bits = tag_size.bits();
                    let tag_mask = if tag_bits == 128 {
                        u128::MAX
                    } else {
                        (1u128 << tag_bits) - 1
                    };
                    let relative = read_value_bits.wrapping_sub(*niche_start) & tag_mask;
                    let first = niche_variants.start.as_u32();
                    let relative_max = niche_variants.last.as_u32() - first;
                    if relative <= u128::from(relative_max) {
                        active_variant_idx = VariantIdx::from_u32(
                            first + u32::try_from(relative).expect("bounded by relative_max"),
                        );
                    } else {
                        active_variant_idx = *untagged_variant;
                    }
                }
            }

            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "const-eval",
                format!(
                    "Debug: Determined active variant index: {:?}",
                    active_variant_idx
                )
            );
        }

        Variants::Empty => {
            return Err(format!(
                "Cannot read constant value for uninhabited enum type {:?}",
                enum_ty
            ));
        }
    }

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "const-eval",
        format!(
            "Debug: Determined active variant index: {:?}",
            active_variant_idx
        )
    );

    let variant_def = adt_def.variant(active_variant_idx);

    let mut fields_map = HashMap::new();
    let mut params = Vec::new();
    for (i, field_def) in variant_def.fields.iter().enumerate() {
        let field_idx = FieldIdx::from_usize(i);
        let field_ty = field_def.ty(tcx, substs).skip_norm_wip();
        let field_oomir_ty = ty_to_oomir_type(field_ty, tcx, oomir_data_types, instance);
        if !field_oomir_ty.has_jvm_value() {
            continue;
        }

        // Sticking with the previous logic: relative offset within variant shape.
        let field_offset_in_variant_shape = match &layout.variants {
            Variants::Single { .. } => layout.fields.offset(field_idx.into()),
            Variants::Multiple {
                variants: variant_layouts,
                ..
            } => variant_layouts[active_variant_idx].field_offsets[field_idx],
            Variants::Empty => unreachable!("empty enums have no active variant fields"),
        };

        // Calculate absolute offset relative to the start of the *whole allocation* `offset`.
        let absolute_field_offset = offset + field_offset_in_variant_shape;

        let field_name = format!("field{}", fields_map.len());

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "const-eval",
            format!(
                "Debug: Reading enum variant {:?} field '{}' index {} ({:?}) at absolute offset {:?} (relative offset in variant shape: {:?})",
                variant_def.name,
                field_name,
                i,
                field_ty,
                absolute_field_offset,
                field_offset_in_variant_shape
            )
        );

        let field_const = read_constant_value_from_memory(
            tcx,
            allocation,
            absolute_field_offset, // Use the absolute offset in the allocation
            field_ty,
            oomir_data_types,
            instance,
        )?;
        params.push(field_const.clone());
        fields_map.insert(field_name, field_const);
    }

    // 4. Construct the OOMIR constant
    let base_enum_name =
        generate_adt_jvm_class_name(&adt_def, substs, tcx, oomir_data_types, instance);
    let variant_class_name = format!(
        "{}${}", // Using '$' as inner class separator is common in JVM
        base_enum_name,
        jvm_names::member_name(&variant_def.ident(tcx).to_string())
    );

    let should_define_data_type = should_define_named_data_type(tcx, adt_def.did());

    // the enum in general
    if should_define_data_type && !oomir_data_types.contains_key(&base_enum_name) {
        let mut methods = HashMap::new();
        methods.insert(
            "getVariantIdx".to_string(),
            DataTypeMethod::SimpleConstantReturn(oomir::Type::I32, None),
        );
        oomir_data_types.insert(
            base_enum_name.clone(),
            oomir::DataType::Class {
                fields: vec![], // No fields in the abstract class
                is_abstract: true,
                methods,
                super_class: None,
                interfaces: vec![],
            },
        );
    }

    // this variant
    if should_define_data_type && !oomir_data_types.contains_key(&variant_class_name) {
        let mut fields = vec![];
        for field in variant_def.fields.iter() {
            let field_type = ty_to_oomir_type(
                field.ty(tcx, substs).skip_norm_wip(),
                tcx,
                oomir_data_types,
                instance,
            );
            if field_type.has_jvm_value() {
                let field_name = format!("field{}", fields.len());
                fields.push((field_name, field_type));
            }
        }

        let mut methods = HashMap::new();
        methods.insert(
            "getVariantIdx".to_string(),
            DataTypeMethod::SimpleConstantReturn(
                oomir::Type::I32,
                Some(oomir::Constant::I32(active_variant_idx.as_u32() as i32)),
            ),
        );

        oomir_data_types.insert(
            variant_class_name.clone(),
            oomir::DataType::Class {
                fields,
                is_abstract: false,
                methods,
                super_class: Some(base_enum_name.clone()),
                interfaces: vec![],
            },
        );
    }

    Ok(oomir::Constant::Instance {
        class_name: variant_class_name,
        fields: fields_map,
        params,
    })
}

/// Converts a Rust MIR Scalar::Int into the appropriate OOMIR constant.
pub fn scalar_int_to_oomir_constant(scalar_int: ScalarInt, ty: Ty<'_>) -> oomir::Constant {
    let bits = scalar_int.to_bits(scalar_int.size());
    let bit_width = scalar_int.size().bits() as u32;
    let signed = if bit_width == 128 {
        bits as i128
    } else {
        ((bits << (128 - bit_width)) as i128) >> (128 - bit_width)
    };

    match ty.kind() {
        TyKind::Int(int_ty) => match int_ty {
            IntTy::I8 => oomir::Constant::I8(signed as i8),
            IntTy::I16 => oomir::Constant::I16(signed as i16),
            IntTy::I32 => oomir::Constant::I32(signed as i32),
            IntTy::Isize => oomir::Constant::I64(signed as i64),
            IntTy::I64 => oomir::Constant::I64(signed as i64),
            IntTy::I128 => {
                let param = oomir::Constant::String(signed.to_string());
                oomir::Constant::Instance {
                    class_name: crate::lower2::I128_CLASS.into(),
                    fields: HashMap::new(),
                    params: vec![param],
                }
            }
        },
        TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => oomir::Constant::U8(bits as u8),
            UintTy::U16 => oomir::Constant::U16(bits as u16),
            UintTy::U32 => oomir::Constant::U32(bits as u32),
            UintTy::Usize | UintTy::U64 => oomir::Constant::U64(bits as u64),
            UintTy::U128 => {
                let param = oomir::Constant::String(bits.to_string());
                oomir::Constant::Instance {
                    class_name: crate::lower2::U128_CLASS.into(),
                    fields: HashMap::new(),
                    params: vec![param],
                }
            }
        },
        TyKind::Bool => oomir::Constant::Boolean(scalar_int.try_to_bool().unwrap_or(false)),
        TyKind::Char => oomir::Constant::I32(scalar_int.to_u32() as i32),
        TyKind::Float(float_ty) => match float_ty {
            FloatTy::F16 => oomir::Constant::F16(scalar_int.to_u16()),
            FloatTy::F32 => oomir::Constant::F32(f32::from_bits(scalar_int.to_u32())),
            FloatTy::F64 => oomir::Constant::F64(f64::from_bits(scalar_int.to_u64())),
            FloatTy::F128 => {
                let bits = scalar_int.to_u128();
                oomir::Constant::Instance {
                    class_name: crate::lower2::F128_CLASS.into(),
                    fields: HashMap::new(),
                    params: vec![
                        oomir::Constant::I64((bits >> 64) as i64),
                        oomir::Constant::I64(bits as i64),
                    ],
                }
            }
        },
        TyKind::Str => oomir::Constant::Str(scalar_int.to_u64().to_string()),
        TyKind::Ref(_, inner_ty, _) => {
            scalar_int_to_oomir_constant(scalar_int.to_u64().into(), *inner_ty)
        }
        TyKind::Pat(base_ty, _) => scalar_int_to_oomir_constant(scalar_int, *base_ty),
        _ => panic!("Unsupported type for ScalarInt conversion: {:?}", ty),
    }
}
