use rustc_abi::FieldIdx;
use rustc_middle::{
    mir::{
        BinOp, Body, BorrowKind as MirBorrowKind, CastKind, Operand as MirOperand, Place,
        ProjectionElem, Rvalue, UnOp,
    },
    ty::{EarlyBinder, Instance, Ty, TyCtxt, TyKind, TypingEnv, adjustment::PointerCoercion},
};
use rustc_span::sym;
use std::collections::{HashMap, HashSet};

use super::{
    super::{
        jvm_names,
        operand::{convert_operand, get_placeholder_operand},
        place::{
            coroutine_saved_field_name, emit_instructions_to_get_on_own, emit_pointer_read,
            emit_pointer_slice_parts, emit_slice_view, get_place_type, place_to_string,
        },
        types::{
            ENUM_UNION_DISCRIMINANT_METHOD, adapt_simple_enum_operand,
            ensure_exact_transmute_helper, ensure_fn_ptr_interface, ensure_union_data_type,
            enum_union_discriminant_supported, fn_ptr_signature_from_ty,
            generate_adt_jvm_class_name, should_define_named_data_type, ty_to_oomir_type,
            union_from_method_name,
        },
    },
    checked_ops::{
        checked_arithmetic_tuple_local_name, emit_checked_arithmetic_oomir_instructions,
    },
    oomir::{self, DataTypeMethod},
    trait_objects::{carrier_needs_trait_object_adapter, ensure_trait_object_adapter_class},
};

use std::sync::atomic::{AtomicUsize, Ordering}; // For unique temp names

// Global or struct-based counter for temporary variables
static TEMP_VAR_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn generate_temp_var_name(base_name: &str) -> String {
    let count = TEMP_VAR_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}_tmp{}", jvm_names::member_name(base_name), count)
}

fn rust_layout_size_operand<'tcx>(
    ty: rustc_middle::ty::Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> oomir::Operand {
    let ty = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let size = super::super::types::layout_size_bytes(tcx, ty).unwrap_or_else(|error| {
        panic!("could not determine pointer layout size for {ty:?}: {error}")
    });
    oomir::Operand::Constant(oomir::Constant::U64(
        u64::try_from(size).expect("Rust layout size exceeds u64"),
    ))
}

fn pointer_view_size_operand<'tcx>(
    pointer_ty: rustc_middle::ty::Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> oomir::Operand {
    let pointee = match pointer_ty.kind() {
        TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
        other => panic!("expected pointer/reference type, found {other:?}"),
    };
    let pointee = EarlyBinder::bind(tcx, pointee)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let size = super::super::types::layout_size_bytes(tcx, pointee).unwrap_or_else(|error| {
        panic!("could not determine pointer view size for {pointee:?}: {error}")
    });
    oomir::Operand::Constant(oomir::Constant::U64(
        u64::try_from(size).expect("Rust pointer view layout exceeds u64"),
    ))
}

fn pointer_pointee_ty<'tcx>(pointer_ty: rustc_middle::ty::Ty<'tcx>) -> rustc_middle::ty::Ty<'tcx> {
    match pointer_ty.kind() {
        TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
        other => panic!("expected pointer/reference type, found {other:?}"),
    }
}

fn struct_tail_unsize_target_class<'tcx>(
    source_pointer_ty: rustc_middle::ty::Ty<'tcx>,
    target_pointer_ty: rustc_middle::ty::Ty<'tcx>,
    source_oomir_ty: &oomir::Type,
    target_oomir_ty: &oomir::Type,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Option<String> {
    let (source_pointee, target_pointee) =
        match (source_pointer_ty.kind(), target_pointer_ty.kind()) {
            (
                TyKind::Ref(_, source, _) | TyKind::RawPtr(source, _),
                TyKind::Ref(_, target, _) | TyKind::RawPtr(target, _),
            ) => (*source, *target),
            _ => return None,
        };
    let source_pointee = EarlyBinder::bind(tcx, source_pointee)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let target_pointee = EarlyBinder::bind(tcx, target_pointee)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let typing_env = TypingEnv::fully_monomorphized();
    let source_pointee = tcx
        .try_normalize_erasing_regions(
            typing_env,
            rustc_middle::ty::Unnormalized::new_wip(source_pointee),
        )
        .ok()?;
    let target_pointee = tcx
        .try_normalize_erasing_regions(
            typing_env,
            rustc_middle::ty::Unnormalized::new_wip(target_pointee),
        )
        .ok()?;
    let (TyKind::Adt(source_def, _), TyKind::Adt(target_def, _)) =
        (source_pointee.kind(), target_pointee.kind())
    else {
        return None;
    };
    if source_def.did() != target_def.did() || !source_def.is_struct() {
        return None;
    }
    let source_tail = tcx.struct_tail_for_codegen(source_pointee, typing_env);
    let target_tail = tcx.struct_tail_for_codegen(target_pointee, typing_env);
    if !matches!(source_tail.kind(), TyKind::Array(_, _))
        || !matches!(target_tail.kind(), TyKind::Slice(_) | TyKind::Str)
    {
        return None;
    }
    match (source_oomir_ty, target_oomir_ty) {
        (oomir::Type::Pointer(source), oomir::Type::Pointer(target))
            if matches!(source.as_ref(), oomir::Type::Class(_)) =>
        {
            let oomir::Type::Class(target_class) = target.as_ref() else {
                return None;
            };
            Some(target_class.clone())
        }
        _ => None,
    }
}

fn struct_tail_pointer_target_class<'tcx>(
    target_pointer_ty: rustc_middle::ty::Ty<'tcx>,
    target_oomir_ty: &oomir::Type,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Option<String> {
    let target_pointee = match target_pointer_ty.kind() {
        TyKind::Ref(_, target, _) | TyKind::RawPtr(target, _) => *target,
        _ => return None,
    };
    let target_pointee = normalize_unsize_ty(target_pointee, tcx, instance);
    let tail = tcx.struct_tail_for_codegen(target_pointee, TypingEnv::fully_monomorphized());
    if !matches!(tail.kind(), TyKind::Slice(_) | TyKind::Str) {
        return None;
    }
    let oomir::Type::Pointer(target) = target_oomir_ty else {
        return None;
    };
    let oomir::Type::Class(target_class) = target.as_ref() else {
        return None;
    };
    Some(target_class.clone())
}

fn normalize_unsize_ty<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Ty<'tcx> {
    let instantiated = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    tcx.try_normalize_erasing_regions(
        TypingEnv::fully_monomorphized(),
        rustc_middle::ty::Unnormalized::new_wip(instantiated),
    )
    .unwrap_or(instantiated)
}

fn emit_raw_array_pointer_unsize<'tcx>(
    source_ty: Ty<'tcx>,
    target_ty: Ty<'tcx>,
    source: oomir::Operand,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    // NonNull's field is a pattern-refined raw pointer. Pattern types retain
    // the raw pointer's runtime representation and unsizing behavior.
    let source_pointer_ty = match source_ty.kind() {
        TyKind::Pat(inner, _) => normalize_unsize_ty(*inner, tcx, instance),
        _ => source_ty,
    };
    let target_pointer_ty = match target_ty.kind() {
        TyKind::Pat(inner, _) => normalize_unsize_ty(*inner, tcx, instance),
        _ => target_ty,
    };
    let (source_pointee, target_pointee) =
        match (source_pointer_ty.kind(), target_pointer_ty.kind()) {
            (TyKind::RawPtr(source, _), TyKind::RawPtr(target, _)) => (*source, *target),
            _ => return None,
        };
    let source_pointee = normalize_unsize_ty(source_pointee, tcx, instance);
    let target_pointee = normalize_unsize_ty(target_pointee, tcx, instance);
    let source_oomir_ty = ty_to_oomir_type(source_ty, tcx, data_types, instance);
    let target_oomir_ty = ty_to_oomir_type(target_ty, tcx, data_types, instance);

    if matches!(target_pointee.kind(), TyKind::Dynamic(..))
        && matches!(source_oomir_ty, oomir::Type::Pointer(_))
        && matches!(target_oomir_ty, oomir::Type::Pointer(_))
    {
        if let Some(callable_abi) = super::super::types::callable_trait_object_abi(
            target_pointer_ty,
            tcx,
            data_types,
            instance,
        ) {
            ensure_closure_callable_bridge(
                source_pointee,
                &callable_abi,
                data_types,
                tcx,
                instance,
            );
        }
        instructions.push(oomir::Instruction::InvokeVirtual {
            dest: Some(dest.to_string()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "retype".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("self".to_string(), source_oomir_ty),
                    ("view_size".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(target_oomir_ty.clone()),
                is_static: false,
            },
            args: vec![oomir::Operand::Constant(oomir::Constant::U64(0))],
            operand: source,
        });
        return Some(oomir::Operand::Variable {
            name: dest.to_string(),
            ty: target_oomir_ty,
        });
    }

    let (TyKind::Array(source_element, length), TyKind::Slice(target_element)) =
        (source_pointee.kind(), target_pointee.kind())
    else {
        return None;
    };
    let source_element = normalize_unsize_ty(*source_element, tcx, instance);
    let target_element = normalize_unsize_ty(*target_element, tcx, instance);
    if source_element != target_element {
        return None;
    }

    if !matches!(source_oomir_ty, oomir::Type::Pointer(_))
        || !matches!(target_oomir_ty, oomir::Type::Slice(_))
    {
        return None;
    }

    let element_oomir_ty = ty_to_oomir_type(target_element, tcx, data_types, instance);
    let element_pointer_ty = oomir::Type::Pointer(Box::new(element_oomir_ty));
    let element_pointer = format!("{dest}_element_pointer");
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(element_pointer.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "retype".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), source_oomir_ty),
                ("view_size".to_string(), oomir::Type::U64),
                ("view_codec".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(element_pointer_ty.clone()),
            is_static: true,
        },
        args: vec![
            source,
            rust_layout_size_operand(target_element, tcx, instance),
            super::super::types::pointer_view_codec_operand(
                target_element,
                tcx,
                data_types,
                instance,
            ),
        ],
    });

    let length = EarlyBinder::bind(tcx, *length)
        .instantiate(tcx, instance.args)
        .skip_norm_wip()
        .try_to_target_usize(tcx)?;
    let slice_object = format!("{dest}_slice_object");
    instructions.push(oomir::Instruction::ConstructObject {
        dest: slice_object.clone(),
        class_name: oomir::SLICE_VIEW_CLASS.to_string(),
        args: vec![
            (
                oomir::Operand::Variable {
                    name: element_pointer,
                    ty: element_pointer_ty,
                },
                oomir::Type::Class("java/lang/Object".to_string()),
            ),
            (
                oomir::Operand::Constant(oomir::Constant::I32(0)),
                oomir::Type::I32,
            ),
            (
                oomir::Operand::Constant(oomir::Constant::I32(i32::try_from(length).ok()?)),
                oomir::Type::I32,
            ),
        ],
    });
    instructions.push(oomir::Instruction::Cast {
        dest: dest.to_string(),
        op: oomir::Operand::Variable {
            name: slice_object,
            ty: oomir::Type::Class(oomir::SLICE_VIEW_CLASS.to_string()),
        },
        ty: target_oomir_ty.clone(),
    });
    Some(oomir::Operand::Variable {
        name: dest.to_string(),
        ty: target_oomir_ty,
    })
}

fn emit_unsize_value<'tcx>(
    source_ty: Ty<'tcx>,
    target_ty: Ty<'tcx>,
    source: oomir::Operand,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let checkpoint = instructions.len();
    let source_ty = normalize_unsize_ty(source_ty, tcx, instance);
    let target_ty = normalize_unsize_ty(target_ty, tcx, instance);
    let source_oomir_ty = ty_to_oomir_type(source_ty, tcx, data_types, instance);
    let target_oomir_ty = ty_to_oomir_type(target_ty, tcx, data_types, instance);

    let result = if source_oomir_ty == target_oomir_ty {
        instructions.push(oomir::Instruction::Move {
            dest: dest.to_string(),
            src: source,
        });
        Some(oomir::Operand::Variable {
            name: dest.to_string(),
            ty: target_oomir_ty,
        })
    } else if let (TyKind::Adt(source_def, source_args), TyKind::Adt(target_def, target_args)) =
        (source_ty.kind(), target_ty.kind())
        && source_def.did() == target_def.did()
        && source_def.is_struct()
        && super::super::types::should_define_named_data_type(tcx, source_def.did())
        && let (oomir::Type::Class(source_class), oomir::Type::Class(target_class)) =
            (&source_oomir_ty, &target_oomir_ty)
    {
        let mut constructor_args = Vec::new();
        let mut valid = true;
        for (field_index, field) in source_def.variant(0usize.into()).fields.iter().enumerate() {
            let source_field_ty =
                normalize_unsize_ty(field.ty(tcx, source_args).skip_norm_wip(), tcx, instance);
            let target_field_ty =
                normalize_unsize_ty(field.ty(tcx, target_args).skip_norm_wip(), tcx, instance);
            let target_field_oomir_ty =
                ty_to_oomir_type(target_field_ty, tcx, data_types, instance);
            if !target_field_oomir_ty.has_jvm_value() {
                continue;
            }

            let target_is_zst = tcx
                .layout_of(TypingEnv::fully_monomorphized().as_query_input(target_field_ty))
                .is_ok_and(|layout| layout.size.bytes() == 0);
            let field_value = if target_is_zst {
                super::super::value_repr::materialize_implicit_zst(
                    target_field_ty,
                    &format!("{dest}_field_{field_index}_zst"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                )
            } else {
                let source_field_oomir_ty =
                    ty_to_oomir_type(source_field_ty, tcx, data_types, instance);
                if !source_field_oomir_ty.has_jvm_value() {
                    None
                } else {
                    let source_field_name = format!("{dest}_field_{field_index}_source");
                    instructions.push(oomir::Instruction::GetField {
                        dest: source_field_name.clone(),
                        object: source.clone(),
                        field_name: field.ident(tcx).to_string(),
                        field_ty: source_field_oomir_ty.clone(),
                        owner_class: source_class.clone(),
                    });
                    let source_field = oomir::Operand::Variable {
                        name: source_field_name,
                        ty: source_field_oomir_ty,
                    };
                    if source_field_ty == target_field_ty {
                        Some(source_field)
                    } else {
                        emit_unsize_value(
                            source_field_ty,
                            target_field_ty,
                            source_field,
                            &format!("{dest}_field_{field_index}_unsized"),
                            tcx,
                            instance,
                            data_types,
                            instructions,
                        )
                    }
                }
            };
            let Some(field_value) = field_value else {
                valid = false;
                break;
            };
            constructor_args.push((field_value, target_field_oomir_ty));
        }
        valid.then(|| {
            instructions.push(oomir::Instruction::ConstructObject {
                dest: dest.to_string(),
                class_name: target_class.clone(),
                args: constructor_args,
            });
            oomir::Operand::Variable {
                name: dest.to_string(),
                ty: target_oomir_ty.clone(),
            }
        })
    } else {
        emit_raw_array_pointer_unsize(
            source_ty,
            target_ty,
            source,
            dest,
            tcx,
            instance,
            data_types,
            instructions,
        )
    };

    if result.is_none() {
        instructions.truncate(checkpoint);
    }
    result
}

fn emit_pointer_factory(
    method_name: &str,
    args: Vec<oomir::Operand>,
    pointer_ty: &oomir::Type,
    dest: &str,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let params = match method_name {
        "cell" => vec![
            (
                "value".to_string(),
                oomir::Type::Class("java/lang/Object".to_string()),
            ),
            ("size".to_string(), oomir::Type::I32),
            ("codec".to_string(), oomir::Type::java_string()),
        ],
        "array" => vec![
            (
                "array".to_string(),
                oomir::Type::Class("java/lang/Object".to_string()),
            ),
            ("offset".to_string(), oomir::Type::I32),
            ("element_size".to_string(), oomir::Type::U64),
            ("codec".to_string(), oomir::Type::java_string()),
        ],
        "nullPointer" => Vec::new(),
        other => panic!("unknown pointer factory {other}"),
    };
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(dest.to_string()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: method_name.to_string(),
        method_ty: oomir::Signature {
            params,
            ret: Box::new(pointer_ty.clone()),
            is_static: true,
        },
        args,
    });
    oomir::Operand::Variable {
        name: dest.to_string(),
        ty: pointer_ty.clone(),
    }
}

fn emit_array_pointer(
    array_or_slice: oomir::Operand,
    index: oomir::Operand,
    element_size: oomir::Operand,
    codec: oomir::Operand,
    pointer_ty: &oomir::Type,
    dest: &str,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let source_ty = array_or_slice
        .get_type()
        .expect("array pointer source must be typed");
    if matches!(source_ty, oomir::Type::Slice(_)) {
        let index = if index.get_type() == Some(oomir::Type::U64) {
            index
        } else {
            let index_u64 = format!("{dest}_slice_index_u64");
            instructions.push(oomir::Instruction::Cast {
                dest: index_u64.clone(),
                op: index,
                ty: oomir::Type::U64,
            });
            oomir::Operand::Variable {
                name: index_u64,
                ty: oomir::Type::U64,
            }
        };
        let base_dest = format!("{dest}_slice_base");
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(base_dest.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "fromSlice".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    (
                        "slice".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    ),
                    ("element_size".to_string(), oomir::Type::U64),
                    ("codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(pointer_ty.clone()),
                is_static: true,
            },
            args: vec![array_or_slice, element_size, codec],
        });
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(dest.to_string()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "add".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), pointer_ty.clone()),
                    ("count".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(pointer_ty.clone()),
                is_static: true,
            },
            args: vec![
                oomir::Operand::Variable {
                    name: base_dest,
                    ty: pointer_ty.clone(),
                },
                index,
            ],
        });
        return oomir::Operand::Variable {
            name: dest.to_string(),
            ty: pointer_ty.clone(),
        };
    }
    emit_pointer_factory(
        "array",
        vec![array_or_slice, index, element_size, codec],
        pointer_ty,
        dest,
        instructions,
    )
}

/// Takes the address of a MIR place. Array elements retain their real backing
/// allocation; scalar/aggregate locals and fields receive a stable heap cell.
/// A dereference followed by another borrow is a reborrow and therefore keeps
/// the exact pointer object/allocation identity.
fn emit_pointer_to_place<'tcx>(
    place: &Place<'tcx>,
    pointer_ty: &oomir::Type,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    if place.projection.is_empty() && super::super::place::local_uses_stable_cell(place.local, mir)
    {
        let dest = format!("{temp_prefix}_local_ptr");
        instructions.push(oomir::Instruction::Move {
            dest: dest.clone(),
            src: oomir::Operand::Variable {
                name: super::super::place::local_cell_name(place.local),
                ty: pointer_ty.clone(),
            },
        });
        return oomir::Operand::Variable {
            name: dest,
            ty: pointer_ty.clone(),
        };
    }

    if let Some((last, prefix)) = place.projection.split_last() {
        let base_place = Place {
            local: place.local,
            projection: tcx.mk_place_elems(prefix),
        };
        match last {
            ProjectionElem::Deref => {
                let (base_name, base_instructions, base_ty) =
                    emit_instructions_to_get_on_own(&base_place, tcx, instance, mir, data_types);
                instructions.extend(base_instructions);
                let dereferences_direct_jvm_self = base_place.local.index() == 1
                    && base_place.projection.is_empty()
                    && tcx
                        .opt_associated_item(instance.def_id())
                        .is_some_and(|item| item.is_method());
                if matches!(base_ty, oomir::Type::Pointer(_)) && !dereferences_direct_jvm_self {
                    let dest = format!("{temp_prefix}_reborrow");
                    instructions.push(oomir::Instruction::Move {
                        dest: dest.clone(),
                        src: oomir::Operand::Variable {
                            name: base_name,
                            ty: base_ty,
                        },
                    });
                    return oomir::Operand::Variable {
                        name: dest,
                        ty: pointer_ty.clone(),
                    };
                }
                if matches!(base_ty, oomir::Type::Slice(_))
                    && matches!(pointer_ty, oomir::Type::Pointer(_))
                {
                    // Resolve generic projections through the monomorphized instance;
                    // querying an unresolved parameter's element type can ICE rustc.
                    let pointee_ty =
                        normalize_unsize_ty(place.ty(&mir.local_decls, tcx).ty, tcx, instance);
                    let element_ty = match pointee_ty.kind() {
                        TyKind::Array(element, _) | TyKind::Slice(element) => *element,
                        other => panic!(
                            "slice-backed dereference resolved to non-sequence pointee {other:?}"
                        ),
                    };
                    let element_pointer = emit_array_pointer(
                        oomir::Operand::Variable {
                            name: base_name,
                            ty: base_ty,
                        },
                        oomir::Operand::Constant(oomir::Constant::I32(0)),
                        rust_layout_size_operand(element_ty, tcx, instance),
                        super::super::types::pointer_view_codec_operand(
                            element_ty, tcx, data_types, instance,
                        ),
                        pointer_ty,
                        &format!("{temp_prefix}_slice_base"),
                        instructions,
                    );
                    let dest = format!("{temp_prefix}_slice_array");
                    instructions.push(oomir::Instruction::InvokeVirtual {
                        dest: Some(dest.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "retype".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                ("self".to_string(), pointer_ty.clone()),
                                ("view_size".to_string(), oomir::Type::U64),
                                ("view_codec".to_string(), oomir::Type::java_string()),
                            ],
                            ret: Box::new(pointer_ty.clone()),
                            is_static: false,
                        },
                        args: vec![
                            rust_layout_size_operand(pointee_ty, tcx, instance),
                            super::super::types::pointer_view_codec_operand(
                                pointee_ty, tcx, data_types, instance,
                            ),
                        ],
                        operand: element_pointer,
                    });
                    return oomir::Operand::Variable {
                        name: dest,
                        ty: pointer_ty.clone(),
                    };
                }
            }
            ProjectionElem::Index(index_local) => {
                let (base_name, base_instructions, base_ty) =
                    emit_instructions_to_get_on_own(&base_place, tcx, instance, mir, data_types);
                instructions.extend(base_instructions);
                if matches!(base_ty, oomir::Type::Array(_) | oomir::Type::Slice(_)) {
                    let index = convert_operand(
                        &MirOperand::Copy(Place::from(*index_local)),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        instructions,
                    );
                    return emit_array_pointer(
                        oomir::Operand::Variable {
                            name: base_name,
                            ty: base_ty,
                        },
                        index,
                        rust_layout_size_operand(place.ty(&mir.local_decls, tcx).ty, tcx, instance),
                        super::super::types::pointer_view_codec_operand(
                            place.ty(&mir.local_decls, tcx).ty,
                            tcx,
                            data_types,
                            instance,
                        ),
                        pointer_ty,
                        &format!("{temp_prefix}_array_ptr"),
                        instructions,
                    );
                }
            }
            ProjectionElem::ConstantIndex {
                offset, from_end, ..
            } => {
                let (base_name, base_instructions, base_ty) =
                    emit_instructions_to_get_on_own(&base_place, tcx, instance, mir, data_types);
                instructions.extend(base_instructions);
                if matches!(base_ty, oomir::Type::Array(_) | oomir::Type::Slice(_)) {
                    let base_operand = oomir::Operand::Variable {
                        name: base_name,
                        ty: base_ty,
                    };
                    let index = if *from_end {
                        let length_name = format!("{temp_prefix}_pointer_length");
                        instructions.push(oomir::Instruction::Length {
                            dest: length_name.clone(),
                            array: base_operand.clone(),
                        });
                        let index_name = format!("{temp_prefix}_pointer_index");
                        instructions.push(oomir::Instruction::Sub {
                            dest: index_name.clone(),
                            op1: oomir::Operand::Variable {
                                name: length_name,
                                ty: oomir::Type::I32,
                            },
                            op2: oomir::Operand::Constant(oomir::Constant::I32(*offset as i32)),
                        });
                        oomir::Operand::Variable {
                            name: index_name,
                            ty: oomir::Type::I32,
                        }
                    } else {
                        oomir::Operand::Constant(oomir::Constant::I32(*offset as i32))
                    };
                    return emit_array_pointer(
                        base_operand,
                        index,
                        rust_layout_size_operand(place.ty(&mir.local_decls, tcx).ty, tcx, instance),
                        super::super::types::pointer_view_codec_operand(
                            place.ty(&mir.local_decls, tcx).ty,
                            tcx,
                            data_types,
                            instance,
                        ),
                        pointer_ty,
                        &format!("{temp_prefix}_array_ptr"),
                        instructions,
                    );
                }
            }
            ProjectionElem::Field(field_index, _) => {
                let field_rust_ty = place.ty(&mir.local_decls, tcx).ty;
                if super::super::types::layout_size_bytes(
                    tcx,
                    EarlyBinder::bind(tcx, field_rust_ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip(),
                )
                .is_ok_and(|size| size == 0)
                {
                    // A ZST field has no byte range to project. Materialize its
                    // semantic JVM singleton/value in a zero-sized cell; all
                    // pointer arithmetic still remains stationary.
                    let (value_name, value_instructions, value_ty) =
                        emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
                    instructions.extend(value_instructions);
                    return emit_pointer_factory(
                        "cell",
                        vec![
                            oomir::Operand::Variable {
                                name: value_name,
                                ty: value_ty,
                            },
                            oomir::Operand::Constant(oomir::Constant::I32(0)),
                            oomir::Operand::Constant(oomir::Constant::Null(
                                oomir::Type::java_string(),
                            )),
                        ],
                        pointer_ty,
                        &format!("{temp_prefix}_zst_field_ptr_{}", field_index.index()),
                        instructions,
                    );
                }
                let base_rust_ty = EarlyBinder::bind(tcx, base_place.ty(&mir.local_decls, tcx).ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                // Project aggregate fields from the base Pointer so they retain
                // the allocation and byte offset of the complete Rust value.
                // Ordinary direct JVM receivers use write-through field cells.
                // Atomic<T>, identified semantically by its diagnostic item,
                // must preserve its decoded byte-storage origin so aliases use
                // the same physical atomic location and lock stripe.
                let projects_direct_instance_self = base_place.local.index() == 1
                    && matches!(base_place.projection.as_ref(), [ProjectionElem::Deref])
                    && tcx
                        .opt_associated_item(instance.def_id())
                        .is_some_and(|item| item.is_method());
                let is_atomic_receiver = matches!(
                    base_rust_ty.kind(),
                    TyKind::Adt(adt_def, _)
                        if tcx.is_diagnostic_item(sym::Atomic, adt_def.did())
                );
                let use_managed_field = (matches!(
                    base_rust_ty.kind(),
                    TyKind::Adt(adt_def, _) if adt_def.is_struct() || adt_def.is_enum()
                ) || matches!(base_rust_ty.kind(), TyKind::Coroutine(..)))
                    && matches!(pointer_ty, oomir::Type::Pointer(_));
                let managed_field = if use_managed_field {
                    // For an enum downcast the place carrier is the concrete
                    // variant class, while the Rust type remains the base ADT.
                    let base_oomir_ty = get_place_type(&base_place, mir, tcx, instance, data_types);
                    match &base_oomir_ty {
                        oomir::Type::Class(owner_class) => {
                            let coroutine_variant =
                                base_place.projection.iter().rev().find_map(|projection| {
                                    match projection {
                                        ProjectionElem::Downcast(_, variant) => Some(variant),
                                        _ => None,
                                    }
                                });
                            coroutine_variant
                                .and_then(|variant| {
                                    coroutine_saved_field_name(
                                        base_rust_ty,
                                        variant,
                                        field_index.index(),
                                        tcx,
                                    )
                                })
                                .or_else(|| {
                                    super::super::place::field_name_for_projection(
                                        owner_class,
                                        field_index.index(),
                                        base_rust_ty,
                                        tcx,
                                        data_types,
                                    )
                                    .ok()
                                })
                                .map(|field_name| (base_oomir_ty, field_name))
                        }
                        _ => None,
                    }
                } else {
                    None
                };
                if let Some((base_oomir_ty, field_name)) = managed_field {
                    let base_pointer_ty = oomir::Type::Pointer(Box::new(base_oomir_ty.clone()));
                    let base_pointer = emit_pointer_to_place(
                        &base_place,
                        &base_pointer_ty,
                        &format!("{temp_prefix}_dst_field_base"),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        instructions,
                    );
                    if matches!(base_rust_ty.kind(), TyKind::Adt(adt_def, _) if adt_def.is_struct())
                        && (!projects_direct_instance_self || is_atomic_receiver)
                    {
                        let base_layout = tcx
                            .layout_of(
                                TypingEnv::fully_monomorphized().as_query_input(base_rust_ty),
                            )
                            .unwrap_or_else(|error| {
                                panic!("could not determine struct field pointer layout: {error:?}")
                            });
                        let field_offset = base_layout
                            .fields
                            .offset((*field_index).into())
                            .bytes_usize();
                        let oomir::Type::Class(owner_class) = &base_oomir_ty else {
                            panic!("Rust struct field owner is not a JVM class");
                        };
                        let result_name = format!("{temp_prefix}_struct_field_pointer");
                        instructions.push(oomir::Instruction::InvokeVirtual {
                            dest: Some(result_name.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "projectStructField".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("self".to_string(), base_pointer_ty.clone()),
                                    ("owner_class".to_string(), oomir::Type::java_string()),
                                    ("field_name".to_string(), oomir::Type::java_string()),
                                    ("field_offset".to_string(), oomir::Type::U64),
                                    ("field_size".to_string(), oomir::Type::U64),
                                    ("field_codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(pointer_ty.clone()),
                                is_static: false,
                            },
                            args: vec![
                                oomir::Operand::Constant(oomir::Constant::String(
                                    owner_class.clone(),
                                )),
                                oomir::Operand::Constant(oomir::Constant::String(field_name)),
                                oomir::Operand::Constant(oomir::Constant::U64(
                                    u64::try_from(field_offset)
                                        .expect("Rust struct field offset exceeds u64"),
                                )),
                                rust_layout_size_operand(field_rust_ty, tcx, instance),
                                super::super::types::pointer_view_codec_operand(
                                    field_rust_ty,
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                            operand: base_pointer,
                        });
                        return oomir::Operand::Variable {
                            name: result_name,
                            ty: pointer_ty.clone(),
                        };
                    }
                    let base_value_name = format!("{temp_prefix}_dst_field_owner");
                    let base_value = emit_pointer_read(
                        base_pointer,
                        &base_oomir_ty,
                        &base_value_name,
                        instructions,
                    );
                    let result_name = format!("{temp_prefix}_dst_field_pointer");
                    instructions.push(oomir::Instruction::InvokeStatic {
                        dest: Some(result_name.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "field".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                (
                                    "owner".to_string(),
                                    oomir::Type::Class("java/lang/Object".to_string()),
                                ),
                                ("field_name".to_string(), oomir::Type::java_string()),
                                ("size".to_string(), oomir::Type::U64),
                                ("codec".to_string(), oomir::Type::java_string()),
                            ],
                            ret: Box::new(pointer_ty.clone()),
                            is_static: true,
                        },
                        args: vec![
                            base_value,
                            oomir::Operand::Constant(oomir::Constant::String(field_name)),
                            rust_layout_size_operand(field_rust_ty, tcx, instance),
                            super::super::types::pointer_view_codec_operand(
                                field_rust_ty,
                                tcx,
                                data_types,
                                instance,
                            ),
                        ],
                    });
                    return oomir::Operand::Variable {
                        name: result_name,
                        ty: pointer_ty.clone(),
                    };
                }
                let is_aggregate = matches!(base_rust_ty.kind(), TyKind::Tuple(_))
                    || matches!(
                        base_rust_ty.kind(),
                        TyKind::Adt(adt_def, _) if adt_def.is_struct() || adt_def.is_union()
                    );
                if is_aggregate {
                    let base_layout = tcx
                        .layout_of(TypingEnv::fully_monomorphized().as_query_input(base_rust_ty))
                        .unwrap_or_else(|error| {
                            panic!("could not determine aggregate field pointer layout: {error:?}")
                        });
                    let field_offset = base_layout
                        .fields
                        .offset((*field_index).into())
                        .bytes_usize();
                    let base_oomir_ty = ty_to_oomir_type(base_rust_ty, tcx, data_types, instance);
                    let base_pointer_ty = oomir::Type::Pointer(Box::new(base_oomir_ty));
                    let base_pointer = emit_pointer_to_place(
                        &base_place,
                        &base_pointer_ty,
                        &format!("{temp_prefix}_field_base"),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        instructions,
                    );
                    let offset_pointer_name = format!("{temp_prefix}_field_offset_pointer");
                    instructions.push(oomir::Instruction::InvokeStatic {
                        dest: Some(offset_pointer_name.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "byte_offset".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                ("pointer".to_string(), base_pointer_ty.clone()),
                                ("byte_count".to_string(), oomir::Type::U64),
                            ],
                            ret: Box::new(base_pointer_ty.clone()),
                            is_static: true,
                        },
                        args: vec![
                            base_pointer,
                            oomir::Operand::Constant(oomir::Constant::U64(
                                u64::try_from(field_offset)
                                    .expect("Rust aggregate field offset exceeds u64"),
                            )),
                        ],
                    });
                    let result_name = format!("{temp_prefix}_field_pointer");
                    instructions.push(oomir::Instruction::InvokeStatic {
                        dest: Some(result_name.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "retype".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                ("pointer".to_string(), base_pointer_ty),
                                ("view_size".to_string(), oomir::Type::U64),
                                ("view_codec".to_string(), oomir::Type::java_string()),
                            ],
                            ret: Box::new(pointer_ty.clone()),
                            is_static: true,
                        },
                        args: vec![
                            oomir::Operand::Variable {
                                name: offset_pointer_name,
                                ty: pointer_ty.clone(),
                            },
                            rust_layout_size_operand(
                                place.ty(&mir.local_decls, tcx).ty,
                                tcx,
                                instance,
                            ),
                            super::super::types::pointer_view_codec_operand(
                                place.ty(&mir.local_decls, tcx).ty,
                                tcx,
                                data_types,
                                instance,
                            ),
                        ],
                    });
                    return oomir::Operand::Variable {
                        name: result_name,
                        ty: pointer_ty.clone(),
                    };
                }
            }
            _ => {}
        }
    }

    let (value_name, value_instructions, value_ty) =
        emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
    instructions.extend(value_instructions);
    emit_pointer_factory(
        "cell",
        vec![
            oomir::Operand::Variable {
                name: value_name,
                ty: value_ty,
            },
            rust_layout_size_operand(place.ty(&mir.local_decls, tcx).ty, tcx, instance),
            super::super::types::pointer_memory_codec_operand(
                place.ty(&mir.local_decls, tcx).ty,
                tcx,
                data_types,
                instance,
            ),
        ],
        pointer_ty,
        &format!("{temp_prefix}_cell_ptr"),
        instructions,
    )
}

fn reuse_pointer_to_place<'tcx>(
    place: &Place<'tcx>,
    pointer_ty: &oomir::Type,
    temp_prefix: &str,
    pointer_origins: &super::MutableBorrowMap<'tcx>,
    available_pointer_locals: &HashSet<rustc_middle::mir::Local>,
    mir: &Body<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    // A dereference is tied to the current value of its base pointer, not only
    // to the syntactic MIR place.  Loop-carried raw pointers can be reassigned
    // while an older borrow of `*pointer` is still available; reusing that
    // borrow would retain the previous address.  Reborrowing through
    // emit_pointer_to_place is allocation-free and preserves the current
    // pointer's provenance, so always materialize dereferenced places afresh.
    if place
        .projection
        .iter()
        .any(|projection| matches!(projection, ProjectionElem::Deref))
    {
        return None;
    }

    let oomir::Type::Pointer(target_inner) = pointer_ty else {
        return None;
    };
    let (_, origin) = pointer_origins.iter().find(|(local, origin)| {
        available_pointer_locals.contains(local)
            && !super::super::place::local_uses_stable_cell(**local, mir)
            && origin.original_place == *place
            && origin.carrier_name != temp_prefix
            && origin.pointee_type == *target_inner.as_ref()
    })?;
    let dest = format!("{temp_prefix}_existing_ptr");
    instructions.push(oomir::Instruction::Move {
        dest: dest.clone(),
        src: oomir::Operand::Variable {
            name: origin.carrier_name.clone(),
            ty: pointer_ty.clone(),
        },
    });
    Some(oomir::Operand::Variable {
        name: dest,
        ty: pointer_ty.clone(),
    })
}

/// Returns whether an operand already has the value supplied by a freshly
/// allocated JVM array.  Rust repeat expressions occur in some very large
/// core-library buffers, so emitting one store per element can exceed the JVM
/// method-size limit even when every store only writes the array's default.
fn is_jvm_array_default_value(value: &oomir::Operand, element_ty: &oomir::Type) -> bool {
    if !element_ty.has_jvm_value() {
        return true;
    }

    match (element_ty, value) {
        (oomir::Type::I8, oomir::Operand::Constant(oomir::Constant::I8(0)))
        | (oomir::Type::U8, oomir::Operand::Constant(oomir::Constant::U8(0)))
        | (oomir::Type::I16, oomir::Operand::Constant(oomir::Constant::I16(0)))
        | (oomir::Type::U16, oomir::Operand::Constant(oomir::Constant::U16(0)))
        | (oomir::Type::F16, oomir::Operand::Constant(oomir::Constant::F16(0)))
        | (oomir::Type::I32, oomir::Operand::Constant(oomir::Constant::I32(0)))
        | (oomir::Type::U32, oomir::Operand::Constant(oomir::Constant::U32(0)))
        | (oomir::Type::I64, oomir::Operand::Constant(oomir::Constant::I64(0)))
        | (oomir::Type::U64, oomir::Operand::Constant(oomir::Constant::U64(0))) => true,
        (oomir::Type::F32, oomir::Operand::Constant(oomir::Constant::F32(value))) => {
            value.to_bits() == 0
        }
        (oomir::Type::F64, oomir::Operand::Constant(oomir::Constant::F64(value))) => {
            value.to_bits() == 0
        }
        (oomir::Type::Boolean, oomir::Operand::Constant(oomir::Constant::Boolean(false)))
        | (oomir::Type::Char, oomir::Operand::Constant(oomir::Constant::Char('\0'))) => true,
        (ty, oomir::Operand::Constant(oomir::Constant::Null(_))) => ty.is_jvm_reference_type(),
        _ => false,
    }
}

fn jvm_default_value(ty: &oomir::Type) -> oomir::Operand {
    let constant = match ty {
        oomir::Type::Boolean => oomir::Constant::Boolean(false),
        oomir::Type::Char => oomir::Constant::Char('\0'),
        oomir::Type::I8 => oomir::Constant::I8(0),
        oomir::Type::U8 => oomir::Constant::U8(0),
        oomir::Type::I16 => oomir::Constant::I16(0),
        oomir::Type::U16 => oomir::Constant::U16(0),
        oomir::Type::F16 => oomir::Constant::F16(0),
        oomir::Type::I32 => oomir::Constant::I32(0),
        oomir::Type::U32 => oomir::Constant::U32(0),
        oomir::Type::I64 => oomir::Constant::I64(0),
        oomir::Type::U64 => oomir::Constant::U64(0),
        oomir::Type::F32 => oomir::Constant::F32(0.0),
        oomir::Type::F64 => oomir::Constant::F64(0.0),
        oomir::Type::Unit | oomir::Type::Void => oomir::Constant::Unit,
        ty if ty.is_jvm_reference_type() => oomir::Constant::Null(ty.clone()),
        other => panic!("no JVM default value for {other:?}"),
    };
    oomir::Operand::Constant(constant)
}

fn adapt_value_for_field<'tcx>(
    value_operand: oomir::Operand,
    field_rust_ty: rustc_middle::ty::Ty<'tcx>,
    field_ty: &oomir::Type,
    temp_base_name: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let value_operand = super::super::value_repr::adapt_operand_to_rust_type(
        value_operand,
        field_rust_ty,
        temp_base_name,
        tcx,
        instance,
        data_types,
        instructions,
    );
    let Some(value_ty) = value_operand.get_type() else {
        return value_operand;
    };

    if let oomir::Type::Class(class_name) = field_ty
        && !value_ty.is_jvm_reference_type()
    {
        let temp_name = generate_temp_var_name(temp_base_name);
        let enum_value = adapt_simple_enum_operand(
            value_operand.clone(),
            field_ty,
            &temp_name,
            data_types,
            instructions,
        );
        if enum_value.get_type().as_ref() == Some(field_ty) {
            return enum_value;
        }
        if class_name == "java/lang/Object" {
            instructions.push(oomir::Instruction::Cast {
                op: value_operand,
                ty: field_ty.clone(),
                dest: temp_name.clone(),
            });
            return oomir::Operand::Variable {
                name: temp_name,
                ty: field_ty.clone(),
            };
        }

        let is_marker_class = matches!(
            data_types.get(class_name),
            Some(oomir::DataType::Class {
                fields,
                is_abstract: false,
                ..
            }) if fields.is_empty()
        );

        if is_marker_class {
            instructions.push(oomir::Instruction::ConstructObject {
                dest: temp_name.clone(),
                class_name: class_name.clone(),
                args: Vec::new(),
            });
            return oomir::Operand::Variable {
                name: temp_name,
                ty: field_ty.clone(),
            };
        }
    }

    value_operand
}

#[derive(Debug, Clone)]
pub(crate) enum FnPointerTarget {
    Static(super::super::naming::FnNameData),
    Virtual {
        class_name: String,
        method_name: String,
    },
    Interface {
        class_name: String,
        method_name: String,
    },
}

impl FnPointerTarget {
    fn display_name(&self) -> String {
        match self {
            Self::Static(target) => target.class_to_call_on.as_deref().map_or_else(
                || target.method_name.clone(),
                |class| format!("{class}::{}", target.method_name),
            ),
            Self::Virtual {
                class_name,
                method_name,
            }
            | Self::Interface {
                class_name,
                method_name,
            } => format!("{class_name}::{method_name}"),
        }
    }
}

/// Finds a callable JVM target for a Rust function pointer. Dependency
/// functions normally have a static entry point in their own jar. An external
/// trait method whose receiver is a managed object is instead dispatched
/// through that object's generated JVM method, because its Rust function
/// pointer ABI carries `&Self` as a Pointer.
pub(crate) fn fn_pointer_target<'tcx>(
    tcx: TyCtxt<'tcx>,
    target_instance: Instance<'tcx>,
    signature: &oomir::Signature,
) -> Option<FnPointerTarget> {
    let static_name = super::super::naming::mono_fn_name_from_instance(tcx, target_instance);
    if target_instance.def_id().is_local() {
        return Some(FnPointerTarget::Static(static_name));
    }

    let Some(associated_item) = tcx.opt_associated_item(target_instance.def_id()) else {
        return Some(FnPointerTarget::Static(static_name));
    };
    if !associated_item.is_method() {
        return Some(FnPointerTarget::Static(static_name));
    }
    let (_, receiver_ty) = signature.params.first()?;
    let receiver_ty = match receiver_ty {
        oomir::Type::Pointer(inner) | oomir::Type::Reference(inner) => inner.as_ref(),
        other => other,
    };
    let mut method_signature = signature.clone();
    method_signature.is_static = false;
    let method_name = super::super::naming::associated_method_name_from_instance(
        tcx,
        target_instance,
        &method_signature,
    );
    match receiver_ty {
        oomir::Type::Class(class_name) => Some(FnPointerTarget::Virtual {
            class_name: class_name.clone(),
            method_name,
        }),
        oomir::Type::Interface(class_name) => Some(FnPointerTarget::Interface {
            class_name: class_name.clone(),
            method_name,
        }),
        // Primitive and slice-like receivers have no JVM object on which to
        // dispatch. Their compiled-core implementation is emitted as a static
        // method with the Rust function-pointer ABI.
        _ => Some(FnPointerTarget::Static(static_name)),
    }
}

pub(crate) fn ensure_fn_pointer_adapter_class<'tcx>(
    data_types: &mut HashMap<String, oomir::DataType>,
    target_function: Option<&FnPointerTarget>,
    signature: &oomir::Signature,
    interface_name: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> String {
    let descriptor = signature.to_jvm_descriptor_with_explicit_params();
    let target_name = target_function
        .map(FnPointerTarget::display_name)
        .unwrap_or_else(|| "unsupported".to_string());
    let identity = format!("{target_name}:{descriptor}");
    let base_name = jvm_names::path_segment(&target_name);
    let interface_token = interface_name.rsplit('/').next().unwrap_or(interface_name);
    let local_name = crate::stable_hash::readable_or_hashed_name(
        "FnPtrImpl",
        &format!("{base_name}_{interface_token}"),
        &identity,
        180,
    );
    let class_name = jvm_names::synthetic_class_for_instance(tcx, instance, local_name);

    let mut method_params = Vec::with_capacity(signature.params.len() + 1);
    method_params.push(("self".to_string(), oomir::Type::Class(class_name.clone())));
    method_params.extend(signature.params.iter().cloned());

    let instructions = if let Some(target_function) = target_function {
        let call_dest = if !signature.ret.has_jvm_value() {
            None
        } else {
            Some("_ret".to_string())
        };
        let mut call_args: Vec<_> = signature
            .params
            .iter()
            .enumerate()
            .map(|(i, (_, ty))| oomir::Operand::Variable {
                name: format!("_{}", i + 2),
                ty: ty.clone(),
            })
            .collect();

        let call = match target_function {
            FnPointerTarget::Static(target) => oomir::Instruction::InvokeStatic {
                dest: call_dest.clone(),
                class_name: target
                    .class_to_call_on
                    .clone()
                    .expect("function pointer targets have JVM owners"),
                method_name: target.method_name.clone(),
                method_ty: signature.clone(),
                args: call_args,
            },
            FnPointerTarget::Virtual {
                class_name,
                method_name,
            } => {
                let receiver = call_args.remove(0);
                let mut method_ty = signature.clone();
                method_ty.is_static = false;
                oomir::Instruction::InvokeVirtual {
                    dest: call_dest.clone(),
                    class_name: class_name.clone(),
                    method_name: method_name.clone(),
                    method_ty,
                    args: call_args,
                    operand: receiver,
                }
            }
            FnPointerTarget::Interface {
                class_name,
                method_name,
            } => {
                let receiver = call_args.remove(0);
                let mut method_ty = signature.clone();
                method_ty.is_static = false;
                oomir::Instruction::InvokeInterface {
                    dest: call_dest.clone(),
                    class_name: class_name.clone(),
                    method_name: method_name.clone(),
                    method_ty,
                    args: call_args,
                    operand: receiver,
                }
            }
        };
        let mut instructions = vec![call];

        instructions.push(oomir::Instruction::Return {
            operand: call_dest.map(|name| oomir::Operand::Variable {
                name,
                ty: signature.ret.as_ref().clone(),
            }),
        });
        instructions
    } else {
        vec![oomir::Instruction::ThrowNewWithMessage {
            exception_class: "java/lang/UnsupportedOperationException".to_string(),
            message: format!("Unsupported non-local function pointer: {target_name}"),
        }]
    };

    let call_method = oomir::DataTypeMethod::Function(oomir::Function {
        name: "call".to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: method_params,
            ret: signature.ret.clone(),
            is_static: false,
        },
        body: oomir::CodeBlock {
            entry: "bb0".to_string(),
            basic_blocks: HashMap::from([(
                "bb0".to_string(),
                oomir::BasicBlock {
                    label: "bb0".to_string(),
                    instructions,
                },
            )]),
        },
    });

    match data_types.get_mut(&class_name) {
        Some(oomir::DataType::Class {
            methods,
            interfaces,
            ..
        }) => {
            methods.entry("call".to_string()).or_insert(call_method);
            if !interfaces
                .iter()
                .any(|interface| interface == interface_name)
            {
                interfaces.push(interface_name.to_string());
            }
        }
        Some(oomir::DataType::Interface { .. }) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "mir-lowering",
                format!(
                    "Function pointer adapter name '{}' already exists as an interface",
                    class_name
                )
            );
        }
        None => {
            data_types.insert(
                class_name.clone(),
                oomir::DataType::Class {
                    fields: vec![],
                    is_abstract: false,
                    methods: HashMap::from([("call".to_string(), call_method)]),
                    super_class: Some("java/lang/Object".to_string()),
                    interfaces: vec![interface_name.to_string()],
                },
            );
        }
    }

    class_name
}

pub(crate) fn ensure_closure_fn_pointer_adapter_class<'tcx>(
    data_types: &mut HashMap<String, oomir::DataType>,
    closure_instance: Instance<'tcx>,
    signature: &oomir::Signature,
    interface_name: &str,
    tcx: TyCtxt<'tcx>,
    instance_context: Instance<'tcx>,
) -> String {
    let typing_env = TypingEnv::fully_monomorphized();
    let closure_ty = closure_instance.ty(tcx, typing_env);
    let TyKind::Closure(_, closure_args) = closure_ty.kind() else {
        panic!(
            "closure function-pointer adapter received non-closure instance {:?}",
            closure_instance
        );
    };
    assert!(
        closure_args.as_closure().upvar_tys().is_empty(),
        "only non-capturing closures can be coerced to function pointers"
    );

    let closure_sig = closure_args.as_closure().sig();
    let closure_inputs = closure_sig.inputs().skip_binder();
    let tuple_ty = *closure_inputs
        .first()
        .expect("closure call ABI always has a tuple argument");
    let tuple_oomir_ty = ty_to_oomir_type(tuple_ty, tcx, data_types, instance_context);
    let target_owner = super::super::naming::mono_owner_class(tcx, closure_instance);
    let target_method = super::super::generate_closure_function_name(tcx, closure_instance);
    let descriptor = signature.to_jvm_descriptor_with_explicit_params();
    let target_name = format!("{target_owner}::{target_method}");
    let identity = format!("{target_name}:{descriptor}");
    let base_name = jvm_names::path_segment(&target_method);
    let interface_token = interface_name.rsplit('/').next().unwrap_or(interface_name);
    let local_name = crate::stable_hash::readable_or_hashed_name(
        "ClosureFnPtrImpl",
        &format!("{base_name}_{interface_token}"),
        &identity,
        180,
    );
    let class_name = jvm_names::synthetic_class_for_instance(tcx, instance_context, local_name);

    let mut method_params = Vec::with_capacity(signature.params.len() + 1);
    method_params.push(("self".to_string(), oomir::Type::Class(class_name.clone())));
    method_params.extend(signature.params.iter().cloned());

    let tuple_dest = "_closure_args".to_string();
    let tuple_args = signature
        .params
        .iter()
        .enumerate()
        .filter(|(_, (_, ty))| ty.has_jvm_value())
        .map(|(index, (_, ty))| {
            (
                oomir::Operand::Variable {
                    name: format!("_{}", index + 2),
                    ty: ty.clone(),
                },
                ty.clone(),
            )
        })
        .collect::<Vec<_>>();
    let mut instructions = Vec::new();
    let closure_call_args = if tuple_oomir_ty.has_jvm_value() {
        let tuple_class = tuple_oomir_ty
            .get_class_name()
            .expect("non-unit closure argument tuple is represented by a JVM class")
            .to_string();
        instructions.push(oomir::Instruction::ConstructObject {
            dest: tuple_dest.clone(),
            class_name: tuple_class,
            args: tuple_args,
        });
        vec![oomir::Operand::Variable {
            name: tuple_dest,
            ty: tuple_oomir_ty.clone(),
        }]
    } else {
        Vec::new()
    };

    let closure_method_params = tuple_oomir_ty
        .has_jvm_value()
        .then(|| ("args".to_string(), tuple_oomir_ty))
        .into_iter()
        .collect();
    let call_dest = signature.ret.has_jvm_value().then(|| "_ret".to_string());
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: call_dest.clone(),
        class_name: target_owner,
        method_name: target_method,
        method_ty: oomir::Signature {
            params: closure_method_params,
            ret: signature.ret.clone(),
            is_static: true,
        },
        args: closure_call_args,
    });
    instructions.push(oomir::Instruction::Return {
        operand: call_dest.map(|name| oomir::Operand::Variable {
            name,
            ty: signature.ret.as_ref().clone(),
        }),
    });

    let call_method = oomir::DataTypeMethod::Function(oomir::Function {
        name: "call".to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: method_params,
            ret: signature.ret.clone(),
            is_static: false,
        },
        body: oomir::CodeBlock {
            entry: "bb0".to_string(),
            basic_blocks: HashMap::from([(
                "bb0".to_string(),
                oomir::BasicBlock {
                    label: "bb0".to_string(),
                    instructions,
                },
            )]),
        },
    });

    match data_types.get_mut(&class_name) {
        Some(oomir::DataType::Class {
            methods,
            interfaces,
            ..
        }) => {
            methods.entry("call".to_string()).or_insert(call_method);
            if !interfaces.iter().any(|name| name == interface_name) {
                interfaces.push(interface_name.to_string());
            }
        }
        Some(oomir::DataType::Interface { .. }) => {
            panic!("closure function-pointer adapter name collided with an interface")
        }
        None => {
            data_types.insert(
                class_name.clone(),
                oomir::DataType::Class {
                    fields: Vec::new(),
                    is_abstract: false,
                    methods: HashMap::from([("call".to_string(), call_method)]),
                    super_class: Some("java/lang/Object".to_string()),
                    interfaces: vec![interface_name.to_string()],
                },
            );
        }
    }

    class_name
}

fn ensure_non_capturing_closure_fn_pointer_bridge<'tcx>(
    data_types: &mut HashMap<String, oomir::DataType>,
    closure_instance: Instance<'tcx>,
    signature: &oomir::Signature,
    tcx: TyCtxt<'tcx>,
    instance_context: Instance<'tcx>,
) -> (String, String) {
    let closure_ty = closure_instance.ty(tcx, TypingEnv::fully_monomorphized());
    let TyKind::Closure(_, closure_args) = closure_ty.kind() else {
        panic!("function-pointer bridge received non-closure {closure_instance:?}");
    };
    assert!(
        closure_args.as_closure().upvar_tys().is_empty(),
        "only non-capturing closures can become function pointers"
    );
    let oomir::Type::Class(closure_class) =
        ty_to_oomir_type(closure_ty, tcx, data_types, instance_context)
    else {
        panic!("closure type did not lower to a JVM class");
    };
    let method_name = "_fn_ptr_call".to_string();
    let already_defined = matches!(
        data_types.get(&closure_class),
        Some(oomir::DataType::Class { methods, .. }) if methods.contains_key(&method_name)
    );
    if already_defined {
        return (closure_class, method_name);
    }

    // Optimised MIR can remove the standalone closure mono item when its only
    // use is this coercion. Keep the implementation alongside the bridge in
    // the closure's existing class so the LambdaMetafactory target never
    // depends on that separate reachability decision.
    let implementation_method_name = "_fn_ptr_impl".to_string();
    let implementation_already_defined = matches!(
        data_types.get(&closure_class),
        Some(oomir::DataType::Class { methods, .. })
            if methods.contains_key(&implementation_method_name)
    );
    if !implementation_already_defined {
        let mut closure_mir = tcx.instance_mir(closure_instance.def).clone();
        let implementation = super::super::mir_to_oomir(
            tcx,
            closure_instance,
            &mut closure_mir,
            Some(super::super::naming::FnNameData {
                class_to_call_on: Some(closure_class.clone()),
                method_name: implementation_method_name.clone(),
            }),
            true,
            data_types,
        );
        let Some(oomir::DataType::Class { methods, .. }) = data_types.get_mut(&closure_class)
        else {
            panic!("closure class disappeared while adding its function-pointer implementation");
        };
        methods.insert(
            implementation_method_name.clone(),
            DataTypeMethod::Function(implementation),
        );
    }

    let closure_sig = closure_args.as_closure().sig();
    let tuple_ty = *closure_sig
        .inputs()
        .skip_binder()
        .first()
        .expect("closure call ABI always has a tuple argument");
    let tuple_oomir_ty = ty_to_oomir_type(tuple_ty, tcx, data_types, instance_context);
    let mut instructions = Vec::new();
    let closure_args = if tuple_oomir_ty.has_jvm_value() {
        let tuple_dest = "_closure_args".to_string();
        instructions.push(oomir::Instruction::ConstructObject {
            dest: tuple_dest.clone(),
            class_name: tuple_oomir_ty
                .get_class_name()
                .expect("non-unit closure tuple is represented by a JVM class")
                .to_string(),
            args: signature
                .params
                .iter()
                .enumerate()
                .map(|(index, (_, ty))| {
                    (
                        oomir::Operand::Variable {
                            name: format!("_{}", index + 1),
                            ty: ty.clone(),
                        },
                        ty.clone(),
                    )
                })
                .collect(),
        });
        vec![oomir::Operand::Variable {
            name: tuple_dest,
            ty: tuple_oomir_ty.clone(),
        }]
    } else {
        Vec::new()
    };
    let call_dest = signature.ret.has_jvm_value().then(|| "_ret".to_string());
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: call_dest.clone(),
        class_name: closure_class.clone(),
        method_name: implementation_method_name,
        method_ty: oomir::Signature {
            params: tuple_oomir_ty
                .has_jvm_value()
                .then(|| ("args".to_string(), tuple_oomir_ty))
                .into_iter()
                .collect(),
            ret: signature.ret.clone(),
            is_static: true,
        },
        args: closure_args,
    });
    instructions.push(oomir::Instruction::Return {
        operand: call_dest.map(|name| oomir::Operand::Variable {
            name,
            ty: signature.ret.as_ref().clone(),
        }),
    });
    let bridge = DataTypeMethod::Function(oomir::Function {
        name: method_name.clone(),
        owner_class: None,
        signature: signature.clone(),
        debug_variables: Vec::new(),
        body: oomir::CodeBlock {
            entry: "bb0".to_string(),
            basic_blocks: HashMap::from([(
                "bb0".to_string(),
                oomir::BasicBlock {
                    label: "bb0".to_string(),
                    instructions,
                },
            )]),
        },
    });
    let Some(oomir::DataType::Class { methods, .. }) = data_types.get_mut(&closure_class) else {
        panic!("closure class disappeared while adding its function-pointer bridge");
    };
    methods.insert(method_name.clone(), bridge);
    (closure_class, method_name)
}

/// Makes a concrete Rust closure directly implement the primitive-specialized
/// JVM SAM used by `dyn Fn*`.  The bridge rebuilds Rust's tuple call argument
/// and, for a capturing closure, supplies the address-like environment carrier
/// expected by the lowered closure body.
fn ensure_closure_callable_bridge<'tcx>(
    closure_ty: rustc_middle::ty::Ty<'tcx>,
    callable_abi: &super::super::types::CallableTraitObjectAbi<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    tcx: TyCtxt<'tcx>,
    instance_context: Instance<'tcx>,
) -> bool {
    let instantiated = EarlyBinder::bind(tcx, closure_ty)
        .instantiate(tcx, instance_context.args)
        .skip_norm_wip();
    let closure_ty = tcx
        .try_normalize_erasing_regions(
            TypingEnv::fully_monomorphized(),
            rustc_middle::ty::Unnormalized::new_wip(instantiated),
        )
        .unwrap_or(instantiated);
    let closure_ty = match closure_ty.kind() {
        TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
        _ => closure_ty,
    };
    let TyKind::Closure(def_id, closure_args) = closure_ty.kind() else {
        return false;
    };
    let closure_instance = Instance::new_raw(*def_id, closure_args);
    let oomir::Type::Class(closure_class) =
        ty_to_oomir_type(closure_ty, tcx, data_types, instance_context)
    else {
        return false;
    };

    let tuple_oomir_ty = ty_to_oomir_type(callable_abi.tuple_ty, tcx, data_types, instance_context);
    let mut instructions = Vec::new();
    let tuple_dest = "_closure_args".to_string();
    let tuple_call_arg = if tuple_oomir_ty.has_jvm_value() {
        let tuple_class = tuple_oomir_ty
            .get_class_name()
            .expect("non-unit closure argument tuple is represented by a JVM class")
            .to_string();
        let tuple_args = callable_abi
            .signature
            .params
            .iter()
            .enumerate()
            .map(|(index, (_, ty))| {
                (
                    oomir::Operand::Variable {
                        name: format!("_{}", index + 2),
                        ty: ty.clone(),
                    },
                    ty.clone(),
                )
            })
            .collect();
        instructions.push(oomir::Instruction::ConstructObject {
            dest: tuple_dest.clone(),
            class_name: tuple_class,
            args: tuple_args,
        });
        Some(oomir::Operand::Variable {
            name: tuple_dest,
            ty: tuple_oomir_ty.clone(),
        })
    } else {
        None
    };

    let has_captures = closure_args
        .as_closure()
        .upvar_tys()
        .iter()
        .next()
        .is_some();
    let mut closure_params = Vec::new();
    let mut closure_call_args = Vec::new();
    if has_captures {
        let environment_ty =
            oomir::Type::Pointer(Box::new(oomir::Type::Class(closure_class.clone())));
        let environment_dest = "_closure_environment".to_string();
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(environment_dest.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "cell".to_string(),
            method_ty: oomir::Signature {
                params: vec![(
                    "value".to_string(),
                    oomir::Type::Class("java/lang/Object".to_string()),
                )],
                ret: Box::new(environment_ty.clone()),
                is_static: true,
            },
            args: vec![oomir::Operand::Variable {
                name: "_1".to_string(),
                ty: oomir::Type::Class(closure_class.clone()),
            }],
        });
        closure_params.push(("closure_env".to_string(), environment_ty.clone()));
        closure_call_args.push(oomir::Operand::Variable {
            name: environment_dest,
            ty: environment_ty,
        });
    }
    if let Some(tuple_call_arg) = tuple_call_arg {
        closure_params.push(("args".to_string(), tuple_oomir_ty));
        closure_call_args.push(tuple_call_arg);
    }

    let call_dest = callable_abi
        .signature
        .ret
        .has_jvm_value()
        .then(|| "_ret".to_string());
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: call_dest.clone(),
        class_name: super::super::naming::mono_owner_class(tcx, closure_instance),
        method_name: super::super::generate_closure_function_name(tcx, closure_instance),
        method_ty: oomir::Signature {
            params: closure_params,
            ret: callable_abi.signature.ret.clone(),
            is_static: true,
        },
        args: closure_call_args,
    });
    instructions.push(oomir::Instruction::Return {
        operand: call_dest.map(|name| oomir::Operand::Variable {
            name,
            ty: callable_abi.signature.ret.as_ref().clone(),
        }),
    });

    let mut method_params = vec![(
        "self".to_string(),
        oomir::Type::Class(closure_class.clone()),
    )];
    method_params.extend(callable_abi.signature.params.iter().cloned());
    let call_method = DataTypeMethod::Function(oomir::Function {
        name: "call".to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: method_params,
            ret: callable_abi.signature.ret.clone(),
            is_static: false,
        },
        body: oomir::CodeBlock {
            entry: "bb0".to_string(),
            basic_blocks: HashMap::from([(
                "bb0".to_string(),
                oomir::BasicBlock {
                    label: "bb0".to_string(),
                    instructions,
                },
            )]),
        },
    });

    let Some(oomir::DataType::Class {
        methods,
        interfaces,
        ..
    }) = data_types.get_mut(&closure_class)
    else {
        return false;
    };
    methods.entry("call".to_string()).or_insert(call_method);
    if !interfaces
        .iter()
        .any(|name| name == &callable_abi.interface_name)
    {
        interfaces.push(callable_abi.interface_name.clone());
    }
    true
}

fn ensure_erased_receiver_fn_pointer_bridge<'tcx>(
    data_types: &mut HashMap<String, oomir::DataType>,
    source_signature: &oomir::Signature,
    source_interface: &str,
    target_signature: &oomir::Signature,
    target_interface: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> Result<String, String> {
    if source_signature.params.len() != target_signature.params.len() {
        return Err("source and target arities differ".to_string());
    }
    if source_signature.ret.to_jvm_return_descriptor()
        != target_signature.ret.to_jvm_return_descriptor()
    {
        return Err("source and target return ABIs differ".to_string());
    }

    let Some((_, source_receiver_ty)) = source_signature.params.first() else {
        return Err("function has no receiver parameter to erase".to_string());
    };
    let Some((_, target_receiver_ty)) = target_signature.params.first() else {
        return Err("function has no receiver carrier parameter".to_string());
    };
    let oomir::Type::Class(carrier_class) = target_receiver_ty else {
        return Err("target receiver is not a NonNull carrier".to_string());
    };
    if !oomir::is_non_null_class_name(carrier_class) {
        return Err("target receiver is not a NonNull carrier".to_string());
    }
    if !source_receiver_ty.has_jvm_value() {
        return Err("zero-sized erased receivers need no bridge".to_string());
    }

    for ((_, source_ty), (_, target_ty)) in source_signature
        .params
        .iter()
        .skip(1)
        .zip(target_signature.params.iter().skip(1))
    {
        if source_ty.to_jvm_descriptor() != target_ty.to_jvm_descriptor() {
            return Err("a non-receiver parameter changes JVM ABI".to_string());
        }
    }

    let Some(oomir::DataType::Class { fields, .. }) = data_types.get(carrier_class) else {
        return Err(format!(
            "receiver carrier class {carrier_class} is undefined"
        ));
    };
    let Some((carrier_field_name, carrier_field_ty)) = fields.first().cloned() else {
        return Err(format!(
            "receiver carrier class {carrier_class} has no payload"
        ));
    };
    if carrier_field_ty.to_jvm_descriptor() != "Ljava/lang/Object;"
        && carrier_field_ty.to_jvm_descriptor() != source_receiver_ty.to_jvm_descriptor()
    {
        return Err(format!(
            "receiver carrier payload has unexpected JVM type {}",
            carrier_field_ty.to_jvm_descriptor()
        ));
    }
    let erased_payload_ty = carrier_field_ty;

    let source_descriptor = source_signature.to_jvm_descriptor_with_explicit_params();
    let target_descriptor = target_signature.to_jvm_descriptor_with_explicit_params();
    let identity = format!("{source_descriptor}->{target_descriptor}");
    let local_name = crate::stable_hash::readable_or_hashed_name(
        "FnPtrErasedReceiverBridge",
        &format!(
            "{}_to_{}",
            source_signature.fn_ptr_interface_name(),
            target_signature.fn_ptr_interface_name()
        ),
        &identity,
        180,
    );
    let class_name = jvm_names::synthetic_class_for_instance(tcx, instance, local_name);
    if data_types.contains_key(&class_name) {
        return Ok(class_name);
    }

    let self_operand = oomir::Operand::Variable {
        name: "_1".to_string(),
        ty: oomir::Type::Class(class_name.clone()),
    };
    let erased_receiver = oomir::Operand::Variable {
        name: "_2".to_string(),
        ty: target_receiver_ty.clone(),
    };
    let source_pointer_name = "_source_function".to_string();
    let erased_payload_name = "_erased_receiver_payload".to_string();
    let typed_receiver_name = "_typed_receiver".to_string();

    let mut instructions = vec![
        oomir::Instruction::GetField {
            dest: source_pointer_name.clone(),
            object: self_operand,
            field_name: "function".to_string(),
            field_ty: oomir::Type::Interface(source_interface.to_string()),
            owner_class: class_name.clone(),
        },
        oomir::Instruction::GetField {
            dest: erased_payload_name.clone(),
            object: erased_receiver,
            field_name: carrier_field_name,
            field_ty: erased_payload_ty.clone(),
            owner_class: carrier_class.clone(),
        },
        oomir::Instruction::Cast {
            op: oomir::Operand::Variable {
                name: erased_payload_name,
                ty: erased_payload_ty,
            },
            ty: source_receiver_ty.clone(),
            dest: typed_receiver_name.clone(),
        },
    ];

    let mut call_args = vec![oomir::Operand::Variable {
        name: typed_receiver_name,
        ty: source_receiver_ty.clone(),
    }];
    call_args.extend(
        target_signature
            .params
            .iter()
            .enumerate()
            .skip(1)
            .map(|(index, (_, ty))| oomir::Operand::Variable {
                name: format!("_{}", index + 2),
                ty: ty.clone(),
            }),
    );

    let call_dest = source_signature
        .ret
        .has_jvm_value()
        .then(|| "_ret".to_string());
    instructions.push(oomir::Instruction::CallIndirect {
        dest: call_dest.clone(),
        function_ptr: Box::new(oomir::Operand::Variable {
            name: source_pointer_name,
            ty: oomir::Type::Interface(source_interface.to_string()),
        }),
        args: call_args,
        signature: source_signature.clone(),
    });
    instructions.push(oomir::Instruction::Return {
        operand: call_dest.map(|name| oomir::Operand::Variable {
            name,
            ty: source_signature.ret.as_ref().clone(),
        }),
    });

    let mut method_params = Vec::with_capacity(target_signature.params.len() + 1);
    method_params.push(("self".to_string(), oomir::Type::Class(class_name.clone())));
    method_params.extend(target_signature.params.iter().cloned());
    let call_method = DataTypeMethod::Function(oomir::Function {
        name: "call".to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: method_params,
            ret: target_signature.ret.clone(),
            is_static: false,
        },
        body: oomir::CodeBlock {
            entry: "bb0".to_string(),
            basic_blocks: HashMap::from([(
                "bb0".to_string(),
                oomir::BasicBlock {
                    label: "bb0".to_string(),
                    instructions,
                },
            )]),
        },
    });

    data_types.insert(
        class_name.clone(),
        oomir::DataType::Class {
            fields: vec![(
                "function".to_string(),
                oomir::Type::Interface(source_interface.to_string()),
            )],
            is_abstract: false,
            methods: HashMap::from([("call".to_string(), call_method)]),
            super_class: Some("java/lang/Object".to_string()),
            interfaces: vec![target_interface.to_string()],
        },
    );
    Ok(class_name)
}

/// Evaluates an Rvalue and returns the resulting OOMIR Operand and any
/// intermediate instructions needed to calculate it.
///
/// The `original_dest_place` is used *only* for naming temporary variables
/// to make debugging easier, not for the final assignment.
pub(super) fn convert_rvalue_to_operand<'a>(
    rvalue: &Rvalue<'a>,
    original_dest_place: &Place<'a>, // Used for naming temps
    mir: &Body<'a>,
    tcx: TyCtxt<'a>,
    instance: Instance<'a>,
    data_types: &mut HashMap<String, oomir::DataType>,
    pointer_origins: &super::MutableBorrowMap<'a>,
    available_pointer_locals: &HashSet<rustc_middle::mir::Local>,
) -> (Vec<oomir::Instruction>, oomir::Operand) {
    let mut instructions = Vec::new();
    let result_operand: oomir::Operand;
    let base_temp_name = place_to_string(original_dest_place, tcx); // For temp naming

    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "mir-lowering",
        format!(
            "convert_rvalue_to_operand: rvalue={:?}, original_dest_place={:?}, dest_ty={:?}",
            rvalue,
            original_dest_place,
            original_dest_place.ty(&mir.local_decls, tcx).ty
        )
    );

    match rvalue {
        Rvalue::Use(mir_operand, _) => {
            match mir_operand {
                MirOperand::Copy(src_place) | MirOperand::Move(src_place) => {
                    // Need to get the value from the source place first
                    let (temp_var_name, get_instructions, temp_var_type) =
                        emit_instructions_to_get_on_own(src_place, tcx, instance, mir, data_types);
                    instructions.extend(get_instructions);
                    result_operand = oomir::Operand::Variable {
                        name: temp_var_name,
                        ty: temp_var_type,
                    };
                }
                MirOperand::Constant(_) | MirOperand::RuntimeChecks(_) => {
                    // Constant is already an operand, no extra instructions
                    result_operand = convert_operand(
                        mir_operand,
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    );
                }
            }
        }

        Rvalue::Repeat(element_op, len_const) => {
            // Create a temporary variable to hold the new array
            let temp_array_var = generate_temp_var_name(&base_temp_name);
            let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty; // Use original dest for type info

            if let rustc_middle::ty::TyKind::Array(elem_ty, _) = place_ty.kind() {
                let oomir_elem_type = ty_to_oomir_type(elem_ty.clone(), tcx, data_types, instance);
                let oomir_elem_op = convert_operand(
                    element_op,
                    tcx,
                    instance,
                    mir,
                    data_types,
                    &mut instructions,
                );
                let oomir_elem_op = adapt_value_for_field(
                    oomir_elem_op,
                    *elem_ty,
                    &oomir_elem_type,
                    &temp_array_var,
                    tcx,
                    instance,
                    data_types,
                    &mut instructions,
                );
                let array_size = EarlyBinder::bind(tcx, *len_const)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip()
                    .try_to_target_usize(tcx)
                    .unwrap_or_else(|| {
                        panic!("Could not resolve array repeat length {:?}", len_const)
                    });
                let size_operand =
                    oomir::Operand::Constant(oomir::Constant::I32(array_size as i32));

                instructions.push(oomir::Instruction::NewArray {
                    dest: temp_array_var.clone(), // Store in temp var
                    element_type: oomir_elem_type.clone(),
                    size: size_operand,
                });

                if !is_jvm_array_default_value(&oomir_elem_op, &oomir_elem_type) {
                    instructions.push(oomir::Instruction::ArrayFill {
                        array: temp_array_var.clone(),
                        value: oomir_elem_op,
                        copy_value: true,
                    });
                }
                result_operand = oomir::Operand::Variable {
                    name: temp_array_var,
                    ty: oomir::Type::Array(Box::new(oomir_elem_type)), // Correct array type
                };
            } else {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "mir-lowering",
                    format!(
                        "Warning: Rvalue::Repeat applied on non-array type: {:?}",
                        place_ty
                    )
                );
                result_operand =
                    get_placeholder_operand(original_dest_place, mir, tcx, instance, data_types);
            }
        }
        Rvalue::Ref(_region, borrow_kind, source_place) => {
            // Check if the result type (destination place type) is a trait object reference
            let dest_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "mir-lowering",
                format!(
                    "Rvalue::Ref start: original_dest_place={:?}, dest_ty={:?}, borrow_kind={:?}, source_place={:?}, source_ty={:?}",
                    original_dest_place,
                    dest_ty,
                    borrow_kind,
                    source_place,
                    source_place.ty(&mir.local_decls, tcx).ty
                )
            );
            let reference_oomir_ty = ty_to_oomir_type(dest_ty, tcx, data_types, instance);
            let is_trait_object = matches!(dest_ty.kind(), TyKind::Ref(..))
                && matches!(reference_oomir_ty, oomir::Type::Interface(_));
            if matches!(reference_oomir_ty, oomir::Type::Pointer(_)) {
                result_operand = reuse_pointer_to_place(
                    source_place,
                    &reference_oomir_ty,
                    &base_temp_name,
                    pointer_origins,
                    available_pointer_locals,
                    mir,
                    &mut instructions,
                )
                .unwrap_or_else(|| {
                    emit_pointer_to_place(
                        source_place,
                        &reference_oomir_ty,
                        &base_temp_name,
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    )
                });
                return (instructions, result_operand);
            }

            match borrow_kind {
                MirBorrowKind::Mut { .. } if !is_trait_object => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Rvalue::Ref(Mut) for place '{}' -> Temp Array Var",
                            place_to_string(source_place, tcx)
                        )
                    );

                    // 1. Get the value of the place being referenced (the 'pointee').
                    let (pointee_value_var_name, pointee_get_instructions, pointee_oomir_type) =
                        emit_instructions_to_get_on_own(
                            source_place,
                            tcx,
                            instance,
                            mir,
                            data_types,
                        );
                    instructions.extend(pointee_get_instructions); // Add instructions to get the value

                    if matches!(pointee_oomir_type, oomir::Type::Array(_)) {
                        let slice_name = generate_temp_var_name(&base_temp_name);
                        let slice_type = emit_slice_view(
                            oomir::Operand::Variable {
                                name: pointee_value_var_name,
                                ty: pointee_oomir_type.clone(),
                            },
                            &pointee_oomir_type,
                            0,
                            0,
                            true,
                            &slice_name,
                            &mut instructions,
                        );
                        result_operand = oomir::Operand::Variable {
                            name: slice_name,
                            ty: slice_type,
                        };
                        return (instructions, result_operand);
                    }

                    if matches!(pointee_oomir_type, oomir::Type::Slice(_) | oomir::Type::Str) {
                        result_operand = oomir::Operand::Variable {
                            name: pointee_value_var_name,
                            ty: pointee_oomir_type,
                        };
                        return (instructions, result_operand);
                    }

                    // 2. Determine the OOMIR type for the array reference itself.
                    let array_ref_oomir_type =
                        oomir::Type::MutableReference(Box::new(pointee_oomir_type.clone()));

                    // 3. Create a temporary variable name for the new array.
                    let array_ref_var_name = generate_temp_var_name(&base_temp_name);

                    // 4. Emit instruction to allocate the single-element array (new T[1]).
                    instructions.push(oomir::Instruction::NewArray {
                        dest: array_ref_var_name.clone(),
                        element_type: pointee_oomir_type.clone(),
                        size: oomir::Operand::Constant(oomir::Constant::I32(1)),
                    });

                    // 5. Emit instruction to store the pointee's value into the array's first element.
                    let pointee_value_operand = oomir::Operand::Variable {
                        name: pointee_value_var_name,
                        ty: pointee_oomir_type,
                    };
                    instructions.push(oomir::Instruction::ArrayStore {
                        array: array_ref_var_name.clone(),
                        index: oomir::Operand::Constant(oomir::Constant::I32(0)),
                        value: pointee_value_operand,
                        copy_value: false,
                    });

                    // 6. The result is the reference to the newly created array.
                    result_operand = oomir::Operand::Variable {
                        name: array_ref_var_name,
                        ty: array_ref_oomir_type,
                    };
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: -> Temp Array Var '{}' ({:?})",
                            result_operand.get_name().unwrap_or("<unknown>"),
                            result_operand.get_type()
                        )
                    );
                }
                MirBorrowKind::Mut { .. } | MirBorrowKind::Shared | MirBorrowKind::Fake { .. } => {
                    // Treat Fake like Shared (used for closures etc.)
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Rvalue::Ref({:?}) for place '{}' -> Direct Value",
                            borrow_kind,
                            place_to_string(source_place, tcx)
                        )
                    );

                    let source_mir_ty = source_place.ty(&mir.local_decls, tcx).ty;
                    if let TyKind::Closure(_, closure_args) = source_mir_ty.kind() {
                        let closure_oomir_type =
                            ty_to_oomir_type(source_mir_ty, tcx, data_types, instance);
                        if let oomir::Type::Class(class_name) = closure_oomir_type.clone() {
                            let has_captures = closure_args
                                .as_closure()
                                .upvar_tys()
                                .iter()
                                .next()
                                .is_some();
                            if has_captures {
                                let (temp_var_name, get_instructions, temp_var_type) =
                                    emit_instructions_to_get_on_own(
                                        source_place,
                                        tcx,
                                        instance,
                                        mir,
                                        data_types,
                                    );
                                instructions.extend(get_instructions);
                                result_operand = oomir::Operand::Variable {
                                    name: temp_var_name,
                                    ty: temp_var_type,
                                };
                            } else {
                                let closure_var_name = generate_temp_var_name(&base_temp_name);
                                instructions.push(oomir::Instruction::ConstructObject {
                                    dest: closure_var_name.clone(),
                                    class_name,
                                    args: Vec::new(),
                                });
                                result_operand = oomir::Operand::Variable {
                                    name: closure_var_name,
                                    ty: closure_oomir_type,
                                };
                            }
                        } else {
                            result_operand = get_placeholder_operand(
                                original_dest_place,
                                mir,
                                tcx,
                                instance,
                                data_types,
                            );
                        }
                    } else {
                        // 1. Get the value/reference of the place being borrowed directly.
                        //    `emit_instructions_to_get_on_own` handles loading/accessing the value.
                        let (pointee_value_var_name, pointee_get_instructions, pointee_oomir_type) =
                            emit_instructions_to_get_on_own(
                                source_place,
                                tcx,
                                instance,
                                mir,
                                data_types,
                            );

                        // 2. Add the instructions needed to get this value.
                        instructions.extend(pointee_get_instructions);

                        if matches!(pointee_oomir_type, oomir::Type::Array(_)) {
                            let slice_name = generate_temp_var_name(&base_temp_name);
                            let slice_type = emit_slice_view(
                                oomir::Operand::Variable {
                                    name: pointee_value_var_name,
                                    ty: pointee_oomir_type.clone(),
                                },
                                &pointee_oomir_type,
                                0,
                                0,
                                true,
                                &slice_name,
                                &mut instructions,
                            );
                            result_operand = oomir::Operand::Variable {
                                name: slice_name,
                                ty: slice_type,
                            };
                            return (instructions, result_operand);
                        }

                        // 3. The result *is* the operand representing the borrowed value itself.
                        //    No array wrapping is done.
                        result_operand = oomir::Operand::Variable {
                            name: pointee_value_var_name,
                            ty: pointee_oomir_type,
                        };
                    }
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: -> Direct Value Operand '{}' ({:?})",
                            result_operand.get_name().unwrap_or("<unknown>"),
                            result_operand.get_type()
                        )
                    );
                }
            }
        }

        Rvalue::Cast(cast_kind, operand, target_mir_ty) => {
            let temp_cast_var = generate_temp_var_name(&base_temp_name);
            let oomir_target_type = ty_to_oomir_type(*target_mir_ty, tcx, data_types, instance);
            let source_mir_ty = EarlyBinder::bind(tcx, operand.ty(&mir.local_decls, tcx))
                .instantiate(tcx, instance.args)
                .skip_norm_wip();

            if matches!(
                cast_kind,
                CastKind::PointerCoercion(PointerCoercion::ClosureFnPointer(_), _)
            ) {
                if let TyKind::Closure(def_id, closure_args) = source_mir_ty.kind() {
                    let closure_instance = Instance::new_raw(*def_id, closure_args);
                    let signature =
                        fn_ptr_signature_from_ty(*target_mir_ty, tcx, data_types, instance);
                    let interface_name =
                        ensure_fn_ptr_interface(&signature, data_types, tcx, instance);
                    let (target_class_name, target_method_name) =
                        ensure_non_capturing_closure_fn_pointer_bridge(
                            data_types,
                            closure_instance,
                            &signature,
                            tcx,
                            instance,
                        );
                    instructions.push(oomir::Instruction::CreateFunctionPointer {
                        dest: temp_cast_var.clone(),
                        interface_name: interface_name.clone(),
                        signature,
                        target_class_name,
                        target_method_name,
                    });
                    result_operand = oomir::Operand::Variable {
                        name: temp_cast_var,
                        ty: oomir::Type::Interface(interface_name),
                    };
                } else {
                    panic!(
                        "ClosureFnPointer cast has non-closure source type {:?}",
                        source_mir_ty
                    );
                }
            } else if matches!(
                cast_kind,
                CastKind::PointerCoercion(PointerCoercion::ReifyFnPointer(_), _)
            ) {
                if let TyKind::FnDef(def_id, substs) = source_mir_ty.kind() {
                    let func_instance = Instance::resolve_for_fn_ptr(
                        tcx,
                        TypingEnv::post_analysis(tcx, mir.source.def_id()),
                        *def_id,
                        substs.no_bound_vars().unwrap(),
                    )
                    .unwrap();
                    let fn_name =
                        super::super::naming::mono_fn_name_from_instance(tcx, func_instance);
                    let signature =
                        fn_ptr_signature_from_ty(*target_mir_ty, tcx, data_types, instance);
                    let interface_name =
                        ensure_fn_ptr_interface(&signature, data_types, tcx, instance);
                    let callable_target = fn_pointer_target(tcx, func_instance, &signature);
                    if callable_target.is_none() {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "mir-lowering",
                            format!(
                                "Warning: Reified non-local function pointer '{}' will use an UnsupportedOperationException stub if invoked.",
                                fn_name.method_name
                            )
                        );
                    }
                    if let Some(FnPointerTarget::Static(target)) = &callable_target
                        && let Some(target_class_name) = &target.class_to_call_on
                    {
                        instructions.push(oomir::Instruction::CreateFunctionPointer {
                            dest: temp_cast_var.clone(),
                            interface_name: interface_name.clone(),
                            signature,
                            target_class_name: target_class_name.clone(),
                            target_method_name: target.method_name.clone(),
                        });
                    } else {
                        let adapter_class = ensure_fn_pointer_adapter_class(
                            data_types,
                            callable_target.as_ref(),
                            &signature,
                            &interface_name,
                            tcx,
                            instance,
                        );
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Reifying FnDef to FnPtr: '{}' -> '{}' as '{}'",
                                source_mir_ty, fn_name.method_name, adapter_class
                            )
                        );
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_cast_var.clone(),
                            class_name: adapter_class,
                            args: Vec::new(),
                        });
                    }
                    result_operand = oomir::Operand::Variable {
                        name: temp_cast_var,
                        ty: oomir::Type::Interface(interface_name),
                    };
                } else {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mir-lowering",
                        format!(
                            "Warning: ReifyFnPointer cast with non-FnDef source type: {:?}",
                            source_mir_ty
                        )
                    );
                    result_operand =
                        oomir::Operand::Constant(oomir::Constant::Null(oomir_target_type));
                }
            } else {
                if matches!(source_mir_ty.kind(), TyKind::FnPtr(..))
                    && matches!(target_mir_ty.kind(), TyKind::FnPtr(..))
                {
                    let source_signature =
                        fn_ptr_signature_from_ty(source_mir_ty, tcx, data_types, instance);
                    let target_signature =
                        fn_ptr_signature_from_ty(*target_mir_ty, tcx, data_types, instance);
                    let source_interface =
                        ensure_fn_ptr_interface(&source_signature, data_types, tcx, instance);
                    let target_interface =
                        ensure_fn_ptr_interface(&target_signature, data_types, tcx, instance);
                    let oomir_operand =
                        convert_operand(operand, tcx, instance, mir, data_types, &mut instructions);

                    if source_signature.to_jvm_descriptor_with_explicit_params()
                        == target_signature.to_jvm_descriptor_with_explicit_params()
                    {
                        instructions.push(oomir::Instruction::Move {
                            dest: temp_cast_var.clone(),
                            src: oomir_operand,
                        });
                    } else if let Ok(bridge_class) = ensure_erased_receiver_fn_pointer_bridge(
                        data_types,
                        &source_signature,
                        &source_interface,
                        &target_signature,
                        &target_interface,
                        tcx,
                        instance,
                    ) {
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_cast_var.clone(),
                            class_name: bridge_class,
                            args: vec![(oomir_operand, oomir::Type::Interface(source_interface))],
                        });
                    } else {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "mir-lowering",
                            format!(
                                "Warning: Function pointer cast from '{}' to '{}' changes the JVM descriptor; generating an UnsupportedOperationException stub.",
                                source_signature.to_jvm_descriptor_with_explicit_params(),
                                target_signature.to_jvm_descriptor_with_explicit_params()
                            )
                        );
                        let adapter_class = ensure_fn_pointer_adapter_class(
                            data_types,
                            None,
                            &target_signature,
                            &target_interface,
                            tcx,
                            instance,
                        );
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_cast_var.clone(),
                            class_name: adapter_class,
                            args: Vec::new(),
                        });
                    }

                    result_operand = oomir::Operand::Variable {
                        name: temp_cast_var,
                        ty: oomir::Type::Interface(target_interface),
                    };
                } else {
                    let oomir_source_type =
                        ty_to_oomir_type(source_mir_ty, tcx, data_types, instance);
                    let raw_oomir_operand =
                        convert_operand(operand, tcx, instance, mir, data_types, &mut instructions);
                    // Promoted reference constants are exposed by rustc as the
                    // pointee scalar/object. Re-materialize their canonical
                    // Pointer carrier before interpreting any pointer cast.
                    let oomir_operand = super::super::value_repr::adapt_operand_to_rust_type(
                        raw_oomir_operand,
                        source_mir_ty,
                        &format!("{}_cast_source", base_temp_name),
                        tcx,
                        instance,
                        data_types,
                        &mut instructions,
                    );

                    if matches!(source_mir_ty.kind(), TyKind::FnPtr(..))
                        && matches!(target_mir_ty.kind(), TyKind::RawPtr(..))
                    {
                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: Some(temp_cast_var.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "cell".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    (
                                        "value".to_string(),
                                        oomir::Type::Class("java/lang/Object".to_string()),
                                    ),
                                    ("size".to_string(), oomir::Type::I32),
                                    ("codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(oomir_target_type.clone()),
                                is_static: true,
                            },
                            args: vec![
                                oomir_operand,
                                pointer_view_size_operand(*target_mir_ty, tcx, instance),
                                super::super::types::pointer_view_codec_operand(
                                    pointer_pointee_ty(*target_mir_ty),
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                        });
                        return (
                            instructions,
                            oomir::Operand::Variable {
                                name: temp_cast_var,
                                ty: oomir_target_type,
                            },
                        );
                    }

                    if matches!(source_mir_ty.kind(), TyKind::RawPtr(..))
                        && matches!(target_mir_ty.kind(), TyKind::FnPtr(..))
                        && matches!(cast_kind, CastKind::Transmute)
                    {
                        let callable = emit_pointer_read(
                            oomir_operand,
                            &oomir_target_type,
                            &temp_cast_var,
                            &mut instructions,
                        );
                        return (instructions, callable);
                    }

                    if matches!(oomir_source_type, oomir::Type::Pointer(_))
                        && oomir_target_type == oomir::Type::U64
                        && matches!(
                            cast_kind,
                            CastKind::Transmute | CastKind::PointerExposeProvenance
                        )
                    {
                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: Some(temp_cast_var.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "address".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![("pointer".to_string(), oomir_source_type)],
                                ret: Box::new(oomir::Type::U64),
                                is_static: true,
                            },
                            args: vec![oomir_operand],
                        });
                        return (
                            instructions,
                            oomir::Operand::Variable {
                                name: temp_cast_var,
                                ty: oomir::Type::U64,
                            },
                        );
                    }

                    if oomir_source_type == oomir::Type::U64
                        && matches!(oomir_target_type, oomir::Type::Pointer(_))
                        && matches!(
                            cast_kind,
                            CastKind::PointerWithExposedProvenance | CastKind::IntToInt
                        )
                    {
                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: Some(temp_cast_var.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "fromAddress".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("address".to_string(), oomir::Type::U64),
                                    ("view_size".to_string(), oomir::Type::U64),
                                    ("view_codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(oomir_target_type.clone()),
                                is_static: true,
                            },
                            args: vec![
                                oomir_operand,
                                pointer_view_size_operand(*target_mir_ty, tcx, instance),
                                super::super::types::pointer_view_codec_operand(
                                    pointer_pointee_ty(*target_mir_ty),
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                        });
                        return (
                            instructions,
                            oomir::Operand::Variable {
                                name: temp_cast_var,
                                ty: oomir_target_type,
                            },
                        );
                    }

                    let callable_abi = matches!(
                        cast_kind,
                        CastKind::PointerCoercion(PointerCoercion::Unsize, _)
                    )
                    .then(|| {
                        super::super::types::callable_trait_object_abi(
                            *target_mir_ty,
                            tcx,
                            data_types,
                            instance,
                        )
                    })
                    .flatten();
                    let callable_closure_bridge =
                        callable_abi.as_ref().is_some_and(|callable_abi| {
                            ensure_closure_callable_bridge(
                                source_mir_ty,
                                callable_abi,
                                data_types,
                                tcx,
                                instance,
                            )
                        });
                    let callable_fn_def_adapter = callable_abi.as_ref().and_then(|callable_abi| {
                        let callable_ty = match source_mir_ty.kind() {
                            TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
                            _ => source_mir_ty,
                        };
                        let TyKind::FnDef(def_id, args) = callable_ty.kind() else {
                            return None;
                        };
                        let function_instance = Instance::resolve_for_fn_ptr(
                            tcx,
                            TypingEnv::post_analysis(tcx, mir.source.def_id()),
                            *def_id,
                            args.no_bound_vars()?,
                        )?;
                        let target =
                            fn_pointer_target(tcx, function_instance, &callable_abi.signature);
                        Some(ensure_fn_pointer_adapter_class(
                            data_types,
                            target.as_ref(),
                            &callable_abi.signature,
                            &callable_abi.interface_name,
                            tcx,
                            instance,
                        ))
                    });

                    let trait_object_adapter = if !callable_closure_bridge
                        && callable_fn_def_adapter.is_none()
                        && matches!(
                            cast_kind,
                            CastKind::PointerCoercion(PointerCoercion::Unsize, _)
                        )
                        && let oomir::Type::Interface(interface_name) = &oomir_target_type
                        && (carrier_needs_trait_object_adapter(&oomir_source_type, data_types)
                            || interface_name
                                .rsplit('/')
                                .next()
                                .is_some_and(|name| name.contains("_Dyn_")))
                    {
                        match ensure_trait_object_adapter_class(
                            source_mir_ty,
                            *target_mir_ty,
                            &oomir_source_type,
                            interface_name,
                            data_types,
                            tcx,
                            instance,
                        ) {
                            Ok(class_name) => Some(class_name),
                            Err(error) => {
                                breadcrumbs::log!(
                                    breadcrumbs::LogLevel::Warn,
                                    "mir-lowering",
                                    format!(
                                        "Could not build trait-object carrier adapter for {source_mir_ty:?} -> {target_mir_ty:?}: {error}"
                                    )
                                );
                                None
                            }
                        }
                    } else {
                        None
                    };

                    let exact_transmute_helper = matches!(cast_kind, CastKind::Transmute)
                        .then(|| {
                            ensure_exact_transmute_helper(
                                source_mir_ty,
                                *target_mir_ty,
                                tcx,
                                data_types,
                                instance,
                            )
                        })
                        .transpose()
                        .unwrap_or_else(|error| {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "mir-lowering",
                                format!(
                                    "Exact-layout transmute codec is unavailable for {source_mir_ty:?} -> {target_mir_ty:?}: {error}"
                                )
                            );
                            None
                        });

                    if callable_closure_bridge {
                        instructions.push(oomir::Instruction::Cast {
                            op: oomir_operand,
                            ty: oomir_target_type.clone(),
                            dest: temp_cast_var.clone(),
                        });
                    } else if let Some(adapter_class) = callable_fn_def_adapter {
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_cast_var.clone(),
                            class_name: adapter_class,
                            args: Vec::new(),
                        });
                    } else if let Some(adapter_class) = trait_object_adapter {
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_cast_var.clone(),
                            class_name: adapter_class,
                            args: vec![(oomir_operand, oomir_source_type.clone())],
                        });
                    } else if let Some(helper) = exact_transmute_helper {
                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: oomir_target_type
                                .has_jvm_value()
                                .then(|| temp_cast_var.clone()),
                            class_name: helper.class_name,
                            method_name: helper.method_name,
                            method_ty: helper.signature,
                            args: oomir_source_type
                                .has_jvm_value()
                                .then_some(oomir_operand)
                                .into_iter()
                                .collect(),
                        });
                    } else if matches!(cast_kind, CastKind::Transmute)
                        && oomir_source_type
                            == oomir::Type::Class(crate::lower2::F128_CLASS.to_string())
                        && oomir_target_type
                            == oomir::Type::Class(crate::lower2::U128_CLASS.to_string())
                    {
                        instructions.push(oomir::Instruction::InvokeVirtual {
                            dest: Some(temp_cast_var.clone()),
                            class_name: crate::lower2::F128_CLASS.to_string(),
                            method_name: "toU128".to_string(),
                            method_ty: oomir::Signature {
                                params: Vec::new(),
                                ret: Box::new(oomir_target_type.clone()),
                                is_static: false,
                            },
                            args: Vec::new(),
                            operand: oomir_operand,
                        });
                    } else if matches!(cast_kind, CastKind::Transmute)
                        && oomir_source_type
                            == oomir::Type::Class(crate::lower2::U128_CLASS.to_string())
                        && oomir_target_type
                            == oomir::Type::Class(crate::lower2::F128_CLASS.to_string())
                    {
                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: Some(temp_cast_var.clone()),
                            class_name: crate::lower2::F128_CLASS.to_string(),
                            method_name: "fromU128".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![("bits".to_string(), oomir_source_type.clone())],
                                ret: Box::new(oomir_target_type.clone()),
                                is_static: true,
                            },
                            args: vec![oomir_operand],
                        });
                    } else if matches!(&oomir_source_type, oomir::Type::Slice(_))
                        && matches!(&oomir_target_type, oomir::Type::Slice(_))
                        && pointer_pointee_ty(source_mir_ty).is_slice()
                        && pointer_pointee_ty(*target_mir_ty).is_slice()
                        && pointer_pointee_ty(source_mir_ty).sequence_element_type(tcx)
                            != pointer_pointee_ty(*target_mir_ty).sequence_element_type(tcx)
                    {
                        let source_element = EarlyBinder::bind(
                            tcx,
                            pointer_pointee_ty(source_mir_ty).sequence_element_type(tcx),
                        )
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                        let target_element = EarlyBinder::bind(
                            tcx,
                            pointer_pointee_ty(*target_mir_ty).sequence_element_type(tcx),
                        )
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                        let source_element_oomir =
                            ty_to_oomir_type(source_element, tcx, data_types, instance);
                        let target_element_oomir =
                            ty_to_oomir_type(target_element, tcx, data_types, instance);
                        let source_pointer_ty =
                            oomir::Type::Pointer(Box::new(source_element_oomir));
                        let target_pointer_ty =
                            oomir::Type::Pointer(Box::new(target_element_oomir));
                        let data_name = format!("{temp_cast_var}_slice_data");
                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: Some(data_name.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "fromSlice".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    (
                                        "slice".to_string(),
                                        oomir::Type::Class("java/lang/Object".to_string()),
                                    ),
                                    ("element_size".to_string(), oomir::Type::U64),
                                    ("codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(source_pointer_ty.clone()),
                                is_static: true,
                            },
                            args: vec![
                                oomir_operand.clone(),
                                rust_layout_size_operand(source_element, tcx, instance),
                                super::super::types::pointer_view_codec_operand(
                                    source_element,
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                        });
                        let retyped_name = format!("{temp_cast_var}_slice_retyped");
                        instructions.push(oomir::Instruction::InvokeVirtual {
                            dest: Some(retyped_name.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "retype".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("self".to_string(), source_pointer_ty.clone()),
                                    ("view_size".to_string(), oomir::Type::U64),
                                    ("view_codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(target_pointer_ty.clone()),
                                is_static: false,
                            },
                            args: vec![
                                rust_layout_size_operand(target_element, tcx, instance),
                                super::super::types::pointer_view_codec_operand(
                                    target_element,
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                            operand: oomir::Operand::Variable {
                                name: data_name,
                                ty: source_pointer_ty,
                            },
                        });
                        let length_name = format!("{temp_cast_var}_slice_length");
                        instructions.push(oomir::Instruction::GetField {
                            dest: length_name.clone(),
                            object: oomir_operand,
                            field_name: "length".to_string(),
                            field_ty: oomir::Type::I32,
                            owner_class: oomir::SLICE_VIEW_CLASS.to_string(),
                        });
                        let slice_object_name = format!("{temp_cast_var}_slice_object");
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: slice_object_name.clone(),
                            class_name: oomir::SLICE_VIEW_CLASS.to_string(),
                            args: vec![
                                (
                                    oomir::Operand::Variable {
                                        name: retyped_name,
                                        ty: target_pointer_ty,
                                    },
                                    oomir::Type::Class("java/lang/Object".to_string()),
                                ),
                                (
                                    oomir::Operand::Constant(oomir::Constant::I32(0)),
                                    oomir::Type::I32,
                                ),
                                (
                                    oomir::Operand::Variable {
                                        name: length_name,
                                        ty: oomir::Type::I32,
                                    },
                                    oomir::Type::I32,
                                ),
                            ],
                        });
                        instructions.push(oomir::Instruction::Cast {
                            op: oomir::Operand::Variable {
                                name: slice_object_name,
                                ty: oomir::Type::Class(oomir::SLICE_VIEW_CLASS.to_string()),
                            },
                            ty: oomir_target_type.clone(),
                            dest: temp_cast_var.clone(),
                        });
                    } else if matches!(oomir_source_type, oomir::Type::Slice(_) | oomir::Type::Str)
                        && matches!(oomir_target_type, oomir::Type::Pointer(_))
                    {
                        let source_pointee = match source_mir_ty.kind() {
                            TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                                if pointee.is_slice() {
                                    pointee.sequence_element_type(tcx)
                                } else if pointee.is_str() {
                                    tcx.types.u8
                                } else {
                                    pointer_pointee_ty(*target_mir_ty)
                                }
                            }
                            _ => pointer_pointee_ty(*target_mir_ty),
                        };
                        let source_element_size =
                            super::super::types::layout_size_bytes(tcx, source_pointee)
                                .expect("fat pointer element has a concrete layout");
                        let data_pointer = format!("{temp_cast_var}_fat_data");
                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: Some(data_pointer.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "fromSlice".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    (
                                        "slice".to_string(),
                                        oomir::Type::Class("java/lang/Object".to_string()),
                                    ),
                                    ("element_size".to_string(), oomir::Type::U64),
                                    ("codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(oomir_target_type.clone()),
                                is_static: true,
                            },
                            args: vec![
                                oomir_operand,
                                oomir::Operand::Constant(oomir::Constant::U64(
                                    u64::try_from(source_element_size)
                                        .expect("Rust slice element layout exceeds u64"),
                                )),
                                super::super::types::pointer_view_codec_operand(
                                    source_pointee,
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                        });
                        instructions.push(oomir::Instruction::InvokeVirtual {
                            dest: Some(temp_cast_var.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "retype".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("self".to_string(), oomir_target_type.clone()),
                                    ("view_size".to_string(), oomir::Type::U64),
                                    ("view_codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(oomir_target_type.clone()),
                                is_static: false,
                            },
                            args: vec![
                                pointer_view_size_operand(*target_mir_ty, tcx, instance),
                                super::super::types::pointer_view_codec_operand(
                                    pointer_pointee_ty(*target_mir_ty),
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                            operand: oomir::Operand::Variable {
                                name: data_pointer,
                                ty: oomir_target_type.clone(),
                            },
                        });
                    } else if matches!(
                        cast_kind,
                        CastKind::PointerCoercion(PointerCoercion::Unsize, _)
                    ) && matches!(source_mir_ty.kind(), TyKind::Adt(..))
                        && emit_unsize_value(
                            source_mir_ty,
                            *target_mir_ty,
                            oomir_operand.clone(),
                            &temp_cast_var,
                            tcx,
                            instance,
                            data_types,
                            &mut instructions,
                        )
                        .is_some()
                    {
                        // The recursive helper emitted the complete target wrapper.
                    } else if matches!(
                        cast_kind,
                        CastKind::PointerCoercion(PointerCoercion::Unsize, _)
                    ) && let Some(target_class) = struct_tail_unsize_target_class(
                        source_mir_ty,
                        *target_mir_ty,
                        &oomir_source_type,
                        &oomir_target_type,
                        tcx,
                        instance,
                    ) {
                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: Some(temp_cast_var.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "unsizeStruct".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("pointer".to_string(), oomir_source_type.clone()),
                                    ("view_size".to_string(), oomir::Type::U64),
                                    ("target_class".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(oomir_target_type.clone()),
                                is_static: true,
                            },
                            args: vec![
                                oomir_operand,
                                pointer_view_size_operand(*target_mir_ty, tcx, instance),
                                oomir::Operand::Constant(oomir::Constant::String(target_class)),
                            ],
                        });
                    } else if matches!(oomir_source_type, oomir::Type::Pointer(_))
                        && matches!(oomir_target_type, oomir::Type::Pointer(_))
                    {
                        if let Some(target_class) = struct_tail_pointer_target_class(
                            *target_mir_ty,
                            &oomir_target_type,
                            tcx,
                            instance,
                        ) {
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(temp_cast_var.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "unsizeStruct".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("pointer".to_string(), oomir_source_type),
                                        ("view_size".to_string(), oomir::Type::U64),
                                        ("target_class".to_string(), oomir::Type::java_string()),
                                    ],
                                    ret: Box::new(oomir_target_type.clone()),
                                    is_static: true,
                                },
                                args: vec![
                                    oomir_operand,
                                    pointer_view_size_operand(*target_mir_ty, tcx, instance),
                                    oomir::Operand::Constant(oomir::Constant::String(target_class)),
                                ],
                            });
                            return (
                                instructions,
                                oomir::Operand::Variable {
                                    name: temp_cast_var,
                                    ty: oomir_target_type,
                                },
                            );
                        }
                        if matches!(
                            pointer_pointee_ty(*target_mir_ty).kind(),
                            TyKind::Dynamic(_, _)
                        ) {
                            instructions.push(oomir::Instruction::InvokeVirtual {
                                dest: Some(temp_cast_var.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "retype".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("self".to_string(), oomir_source_type),
                                        ("view_size".to_string(), oomir::Type::U64),
                                    ],
                                    ret: Box::new(oomir_target_type.clone()),
                                    is_static: false,
                                },
                                args: vec![oomir::Operand::Constant(oomir::Constant::U64(0))],
                                operand: oomir_operand,
                            });
                            return (
                                instructions,
                                oomir::Operand::Variable {
                                    name: temp_cast_var,
                                    ty: oomir_target_type,
                                },
                            );
                        }
                        instructions.push(oomir::Instruction::InvokeVirtual {
                            dest: Some(temp_cast_var.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "retype".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("self".to_string(), oomir_source_type.clone()),
                                    ("view_size".to_string(), oomir::Type::U64),
                                    ("view_codec".to_string(), oomir::Type::java_string()),
                                ],
                                ret: Box::new(oomir_target_type.clone()),
                                is_static: false,
                            },
                            args: vec![
                                pointer_view_size_operand(*target_mir_ty, tcx, instance),
                                super::super::types::pointer_view_codec_operand(
                                    pointer_pointee_ty(*target_mir_ty),
                                    tcx,
                                    data_types,
                                    instance,
                                ),
                            ],
                            operand: oomir_operand,
                        });
                    } else if matches!(
                        (&oomir_source_type, &oomir_target_type),
                        (oomir::Type::Str, oomir::Type::Slice(element_type))
                            if matches!(element_type.as_ref(), oomir::Type::I16)
                    ) {
                        instructions.push(oomir::Instruction::InvokeStatic {
                            class_name: oomir::UTF8_VIEW_CLASS.to_string(),
                            method_name: "asSlice".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![("value".to_string(), oomir::Type::Str)],
                                ret: Box::new(oomir_target_type.clone()),
                                is_static: true,
                            },
                            args: vec![oomir_operand],
                            dest: Some(temp_cast_var.clone()),
                        });
                    } else if matches!(
                        (&oomir_source_type, &oomir_target_type),
                        (oomir::Type::Slice(element_type), oomir::Type::Str)
                            if matches!(element_type.as_ref(), oomir::Type::I16)
                    ) {
                        instructions.push(oomir::Instruction::InvokeStatic {
                            class_name: oomir::UTF8_VIEW_CLASS.to_string(),
                            method_name: "fromSlice".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![("value".to_string(), oomir_source_type.clone())],
                                ret: Box::new(oomir::Type::Str),
                                is_static: true,
                            },
                            args: vec![oomir_operand],
                            dest: Some(temp_cast_var.clone()),
                        });
                    } else if matches!(oomir_target_type, oomir::Type::Slice(_))
                        && (matches!(
                            oomir_source_type,
                            oomir::Type::Array(_) | oomir::Type::Slice(_)
                        ) || matches!(
                            &oomir_source_type,
                            oomir::Type::MutableReference(inner)
                                if matches!(inner.as_ref(), oomir::Type::Array(_))
                        ) || matches!(
                            &oomir_source_type,
                            oomir::Type::Pointer(inner)
                                if matches!(inner.as_ref(), oomir::Type::Array(_))
                        ))
                    {
                        let slice_source = match &oomir_source_type {
                            oomir::Type::Pointer(inner)
                                if matches!(inner.as_ref(), oomir::Type::Array(_)) =>
                            {
                                let source_array_ty = pointer_pointee_ty(source_mir_ty);
                                let TyKind::Array(element_rust_ty, length) = source_array_ty.kind()
                                else {
                                    unreachable!(
                                        "array pointer OOMIR carrier has non-array Rust pointee"
                                    )
                                };
                                let element_oomir_ty = match &oomir_target_type {
                                    oomir::Type::Slice(element) => element.as_ref().clone(),
                                    _ => unreachable!(),
                                };
                                let element_pointer_ty =
                                    oomir::Type::Pointer(Box::new(element_oomir_ty));
                                let element_pointer_name =
                                    format!("{temp_cast_var}_element_pointer");
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    dest: Some(element_pointer_name.clone()),
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "retype".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            ("pointer".to_string(), oomir_source_type.clone()),
                                            ("view_size".to_string(), oomir::Type::U64),
                                            ("view_codec".to_string(), oomir::Type::java_string()),
                                        ],
                                        ret: Box::new(element_pointer_ty.clone()),
                                        is_static: true,
                                    },
                                    args: vec![
                                        oomir_operand,
                                        rust_layout_size_operand(*element_rust_ty, tcx, instance),
                                        super::super::types::pointer_view_codec_operand(
                                            *element_rust_ty,
                                            tcx,
                                            data_types,
                                            instance,
                                        ),
                                    ],
                                });
                                let length = EarlyBinder::bind(tcx, *length)
                                    .instantiate(tcx, instance.args)
                                    .skip_norm_wip()
                                    .try_to_target_usize(tcx)
                                    .expect("array-to-slice coercion length must be concrete");
                                let slice_object_name = format!("{temp_cast_var}_slice_object");
                                instructions.push(oomir::Instruction::ConstructObject {
                                    dest: slice_object_name.clone(),
                                    class_name: oomir::SLICE_VIEW_CLASS.to_string(),
                                    args: vec![
                                        (
                                            oomir::Operand::Variable {
                                                name: element_pointer_name,
                                                ty: element_pointer_ty,
                                            },
                                            oomir::Type::Class("java/lang/Object".to_string()),
                                        ),
                                        (
                                            oomir::Operand::Constant(oomir::Constant::I32(0)),
                                            oomir::Type::I32,
                                        ),
                                        (
                                            oomir::Operand::Constant(oomir::Constant::I32(
                                                i32::try_from(length).expect(
                                                    "array-to-slice length exceeds JVM address space",
                                                ),
                                            )),
                                            oomir::Type::I32,
                                        ),
                                    ],
                                });
                                instructions.push(oomir::Instruction::Cast {
                                    op: oomir::Operand::Variable {
                                        name: slice_object_name,
                                        ty: oomir::Type::Class(oomir::SLICE_VIEW_CLASS.to_string()),
                                    },
                                    ty: oomir_target_type.clone(),
                                    dest: temp_cast_var.clone(),
                                });
                                None
                            }
                            oomir::Type::MutableReference(inner)
                                if matches!(inner.as_ref(), oomir::Type::Array(_)) =>
                            {
                                let unwrapped_name = format!("{}_array", temp_cast_var);
                                instructions.push(oomir::Instruction::ArrayGet {
                                    dest: unwrapped_name.clone(),
                                    array: oomir_operand,
                                    index: oomir::Operand::Constant(oomir::Constant::I32(0)),
                                });
                                Some((
                                    oomir::Operand::Variable {
                                        name: unwrapped_name,
                                        ty: inner.as_ref().clone(),
                                    },
                                    inner.as_ref().clone(),
                                ))
                            }
                            _ => Some((oomir_operand, oomir_source_type.clone())),
                        };
                        if let Some((slice_source, slice_source_type)) = slice_source {
                            emit_slice_view(
                                slice_source,
                                &slice_source_type,
                                0,
                                0,
                                true,
                                &temp_cast_var,
                                &mut instructions,
                            );
                        }
                    } else if let oomir::Type::Class(class_name) = &oomir_target_type
                        && oomir::is_non_null_class_name(class_name)
                    {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            "Info: Handling Rvalue::Cast to NonNull wrapper."
                        );
                        let mut constructor_args = Vec::new();
                        if let Some(oomir::DataType::Class { fields, .. }) =
                            data_types.get(class_name)
                        {
                            if let Some((_field_name, field_ty)) = fields.first().cloned() {
                                let value_ty =
                                    oomir_operand.get_type().unwrap_or(oomir_source_type);
                                let needs_cast = field_ty != value_ty
                                    && field_ty.to_jvm_descriptor() != value_ty.to_jvm_descriptor();
                                let erased_object_field =
                                    field_ty.to_jvm_descriptor() == "Ljava/lang/Object;";
                                let constructor_arg_ty = if erased_object_field {
                                    oomir::Type::Class("java/lang/Object".to_string())
                                } else {
                                    field_ty.clone()
                                };

                                let value_operand =
                                    if matches!(source_mir_ty.kind(), TyKind::Ref(..))
                                        && erased_object_field
                                        && value_ty.has_jvm_value()
                                    {
                                        // Sized references already are stable
                                        // Pointer objects and can be carried
                                        // through NonNull's erased Object field
                                        // without another wrapper allocation.
                                        oomir_operand
                                    } else if erased_object_field
                                        && value_ty.is_jvm_reference_type()
                                    {
                                        oomir_operand
                                    } else if needs_cast
                                        && value_ty.is_jvm_primitive_like()
                                        && field_ty.is_jvm_reference_type()
                                    {
                                        oomir::Operand::Constant(oomir::Constant::Null(
                                            field_ty.clone(),
                                        ))
                                    } else if needs_cast {
                                        let cast_value_name = format!("{}_value", temp_cast_var);
                                        instructions.push(oomir::Instruction::Cast {
                                            op: oomir_operand,
                                            ty: field_ty.clone(),
                                            dest: cast_value_name.clone(),
                                        });
                                        oomir::Operand::Variable {
                                            name: cast_value_name,
                                            ty: field_ty.clone(),
                                        }
                                    } else {
                                        oomir_operand
                                    };

                                constructor_args.push((value_operand, constructor_arg_ty));
                            }
                        }
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_cast_var.clone(),
                            class_name: class_name.clone(),
                            args: constructor_args,
                        });
                    } else if oomir_target_type == oomir_source_type {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            "Info: Handling Rvalue::Cast (Same OOMIR Types) -> Temp Move."
                        );
                        instructions.push(oomir::Instruction::Move {
                            dest: temp_cast_var.clone(),
                            src: oomir_operand,
                        });
                    } else {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            "Info: Handling Rvalue::Cast (Different OOMIR Types) -> Temp Cast."
                        );
                        instructions.push(oomir::Instruction::Cast {
                            op: oomir_operand,
                            ty: oomir_target_type.clone(),
                            dest: temp_cast_var.clone(),
                        });
                    }
                    result_operand = if oomir_target_type.has_jvm_value() {
                        oomir::Operand::Variable {
                            name: temp_cast_var,
                            ty: oomir_target_type,
                        }
                    } else {
                        oomir::Operand::Constant(oomir::Constant::Unit)
                    };
                }
            }
        }

        Rvalue::BinaryOp(bin_op, box (op1, op2)) => {
            let temp_binop_var = generate_temp_var_name(&base_temp_name);
            let oomir_op1 = convert_operand(op1, tcx, instance, mir, data_types, &mut instructions);
            let oomir_op2 = convert_operand(op2, tcx, instance, mir, data_types, &mut instructions);
            // Determine result type based on operands or destination hint
            let oomir_result_type =
                get_place_type(original_dest_place, mir, tcx, instance, data_types);

            match bin_op {
                BinOp::Offset if matches!(oomir_op1.get_type(), Some(oomir::Type::Pointer(_))) => {
                    instructions.push(oomir::Instruction::InvokeVirtual {
                        dest: Some(temp_binop_var.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "offset".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                ("self".to_string(), oomir_op1.get_type().unwrap()),
                                ("elements".to_string(), oomir::Type::I64),
                            ],
                            ret: Box::new(oomir_result_type.clone()),
                            is_static: false,
                        },
                        args: vec![oomir_op2],
                        operand: oomir_op1,
                    });
                }
                BinOp::Eq | BinOp::Ne
                    if matches!(
                        oomir_op1.get_type(),
                        Some(oomir::Type::Slice(_) | oomir::Type::Str)
                    ) =>
                {
                    instructions.push(oomir::Instruction::InvokeStatic {
                        dest: Some(temp_binop_var.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: "fatPointerEquals".to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                (
                                    "left".to_string(),
                                    oomir::Type::Class("java/lang/Object".to_string()),
                                ),
                                (
                                    "right".to_string(),
                                    oomir::Type::Class("java/lang/Object".to_string()),
                                ),
                            ],
                            ret: Box::new(oomir::Type::Boolean),
                            is_static: true,
                        },
                        args: vec![oomir_op1, oomir_op2],
                    });
                    if matches!(bin_op, BinOp::Ne) {
                        let equality_name = temp_binop_var.clone();
                        let not_name = format!("{temp_binop_var}_not");
                        instructions.push(oomir::Instruction::Not {
                            dest: not_name.clone(),
                            src: oomir::Operand::Variable {
                                name: equality_name,
                                ty: oomir::Type::Boolean,
                            },
                        });
                        result_operand = oomir::Operand::Variable {
                            name: not_name,
                            ty: oomir::Type::Boolean,
                        };
                        return (instructions, result_operand);
                    }
                }
                BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
                    if matches!(oomir_op1.get_type(), Some(oomir::Type::Pointer(_))) =>
                {
                    let method_name = match bin_op {
                        BinOp::Eq | BinOp::Ne => "sameAddress",
                        BinOp::Lt => "lessThan",
                        BinOp::Le => "lessOrEqual",
                        BinOp::Gt => "greaterThan",
                        BinOp::Ge => "greaterOrEqual",
                        _ => unreachable!(),
                    };
                    instructions.push(oomir::Instruction::InvokeVirtual {
                        dest: Some(temp_binop_var.clone()),
                        class_name: oomir::POINTER_CLASS.to_string(),
                        method_name: method_name.to_string(),
                        method_ty: oomir::Signature {
                            params: vec![
                                ("self".to_string(), oomir_op1.get_type().unwrap()),
                                ("other".to_string(), oomir_op2.get_type().unwrap()),
                            ],
                            ret: Box::new(oomir::Type::Boolean),
                            is_static: false,
                        },
                        args: vec![oomir_op2],
                        operand: oomir_op1,
                    });
                    if matches!(bin_op, BinOp::Ne) {
                        let equality_name = temp_binop_var.clone();
                        let not_name = format!("{temp_binop_var}_not");
                        instructions.push(oomir::Instruction::Not {
                            dest: not_name.clone(),
                            src: oomir::Operand::Variable {
                                name: equality_name,
                                ty: oomir::Type::Boolean,
                            },
                        });
                        result_operand = oomir::Operand::Variable {
                            name: not_name,
                            ty: oomir::Type::Boolean,
                        };
                        return (instructions, result_operand);
                    }
                }
                BinOp::Add | BinOp::AddUnchecked => instructions.push(oomir::Instruction::Add {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Sub | BinOp::SubUnchecked => instructions.push(oomir::Instruction::Sub {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Mul | BinOp::MulUnchecked => instructions.push(oomir::Instruction::Mul {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Div => instructions.push(oomir::Instruction::Div {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Rem => instructions.push(oomir::Instruction::Rem {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::BitAnd => instructions.push(oomir::Instruction::BitAnd {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::BitOr => instructions.push(oomir::Instruction::BitOr {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::BitXor => instructions.push(oomir::Instruction::BitXor {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Shl | BinOp::ShlUnchecked => instructions.push(oomir::Instruction::Shl {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Shr | BinOp::ShrUnchecked => instructions.push(oomir::Instruction::Shr {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Eq => instructions.push(oomir::Instruction::Eq {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Lt => instructions.push(oomir::Instruction::Lt {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Le => instructions.push(oomir::Instruction::Le {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Ne => instructions.push(oomir::Instruction::Ne {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Ge => instructions.push(oomir::Instruction::Ge {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Gt => instructions.push(oomir::Instruction::Gt {
                    dest: temp_binop_var.clone(),
                    op1: oomir_op1,
                    op2: oomir_op2,
                }),
                BinOp::Cmp => {
                    let less = format!("{}_less", temp_binop_var);
                    let greater = format!("{}_greater", temp_binop_var);
                    instructions.push(oomir::Instruction::Lt {
                        dest: less.clone(),
                        op1: oomir_op1.clone(),
                        op2: oomir_op2.clone(),
                    });
                    instructions.push(oomir::Instruction::Gt {
                        dest: greater.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    });
                    let less_int = format!("{}_less_int", temp_binop_var);
                    let greater_int = format!("{}_greater_int", temp_binop_var);
                    instructions.push(oomir::Instruction::Cast {
                        op: oomir::Operand::Variable {
                            name: less,
                            ty: oomir::Type::Boolean,
                        },
                        ty: oomir::Type::I32,
                        dest: less_int.clone(),
                    });
                    instructions.push(oomir::Instruction::Cast {
                        op: oomir::Operand::Variable {
                            name: greater,
                            ty: oomir::Type::Boolean,
                        },
                        ty: oomir::Type::I32,
                        dest: greater_int.clone(),
                    });
                    instructions.push(oomir::Instruction::Sub {
                        dest: temp_binop_var.clone(),
                        op1: oomir::Operand::Variable {
                            name: greater_int,
                            ty: oomir::Type::I32,
                        },
                        op2: oomir::Operand::Variable {
                            name: less_int,
                            ty: oomir::Type::I32,
                        },
                    });
                    result_operand = adapt_simple_enum_operand(
                        oomir::Operand::Variable {
                            name: temp_binop_var,
                            ty: oomir::Type::I32,
                        },
                        &oomir_result_type,
                        &base_temp_name,
                        data_types,
                        &mut instructions,
                    );
                    return (instructions, result_operand);
                }
                // Checked ops need special handling as they produce a tuple
                BinOp::AddWithOverflow | BinOp::SubWithOverflow | BinOp::MulWithOverflow => {
                    // This case needs to return the *tuple* variable, and the instructions
                    // generated inside it are already correct for creating that tuple.

                    // Reuse the logic from the original handle_rvalue for checked ops,
                    // but target the temp_tuple_var instead of the final dest.
                    let (result_mir_ty, _) = {
                        let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
                        if let TyKind::Tuple(elements) = place_ty.kind() {
                            (elements[0], elements[1])
                        } else {
                            panic!("Checked op dest type mismatch")
                        }
                    };
                    let op_oomir_ty = ty_to_oomir_type(result_mir_ty, tcx, data_types, instance);

                    let operation_string = match bin_op {
                        /* ... */ BinOp::AddWithOverflow => "add",
                        BinOp::SubWithOverflow => "sub",
                        BinOp::MulWithOverflow => "mul",
                        _ => unreachable!(),
                    };
                    let tuple_local_name = checked_arithmetic_tuple_local_name(&op_oomir_ty)
                        .unwrap_or_else(|| {
                            panic!("Unsupported type for checked arithmetic: {:?}", op_oomir_ty)
                        });
                    let tuple_type_name = format!("org/rustlang/core/{tuple_local_name}");
                    let (checked_instructions, tmp_pair_var, _tmp_result_var, _tmp_overflow_var) =
                        emit_checked_arithmetic_oomir_instructions(
                            &base_temp_name, // Use base temp name for context
                            &oomir_op1,
                            &oomir_op2,
                            &op_oomir_ty,
                            operation_string,
                            instructions.len(),
                            &tuple_type_name,
                        );
                    instructions.extend(checked_instructions);
                    // Return the object as the operand
                    result_operand = oomir::Operand::Variable {
                        name: tmp_pair_var,
                        ty: oomir::Type::Class(tuple_type_name),
                    };
                    return (instructions, result_operand);
                }
                _ => {
                    /* Handle Offset, etc. or panic */
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mir-lowering",
                        format!("Warning: Unhandled binary op {:?}", bin_op)
                    );
                    result_operand = get_placeholder_operand(
                        original_dest_place,
                        mir,
                        tcx,
                        instance,
                        data_types,
                    );
                    // No instruction needed for placeholder
                    return (instructions, result_operand);
                }
            }
            result_operand = oomir::Operand::Variable {
                name: temp_binop_var,
                ty: oomir_result_type, // Use determined result type
            };
        }

        Rvalue::UnaryOp(operation, operand) => {
            let temp_unop_var = generate_temp_var_name(&base_temp_name);
            let oomir_src_operand =
                convert_operand(operand, tcx, instance, mir, data_types, &mut instructions);
            let oomir_result_type =
                get_place_type(original_dest_place, mir, tcx, instance, data_types);
            let mut produced_value = false;

            match operation {
                UnOp::Not => {
                    instructions.push(oomir::Instruction::Not {
                        dest: temp_unop_var.clone(),
                        src: oomir_src_operand,
                    });
                    produced_value = true;
                }
                UnOp::Neg => {
                    instructions.push(oomir::Instruction::Neg {
                        dest: temp_unop_var.clone(),
                        src: oomir_src_operand,
                    });
                    produced_value = true;
                }
                UnOp::PtrMetadata => {
                    let operand_ty = operand.ty(&mir.local_decls, tcx);
                    let pointee = match operand_ty.kind() {
                        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
                        _ => operand_ty,
                    };
                    if pointee.is_slice() || pointee.is_str() {
                        let length_i32 = format!("{temp_unop_var}_i32");
                        instructions.push(oomir::Instruction::Length {
                            dest: length_i32.clone(),
                            array: oomir_src_operand,
                        });
                        instructions.push(oomir::Instruction::Cast {
                            dest: temp_unop_var.clone(),
                            op: oomir::Operand::Variable {
                                name: length_i32,
                                ty: oomir::Type::I32,
                            },
                            ty: oomir::Type::U64,
                        });
                        produced_value = true;
                    } else if matches!(pointee.kind(), TyKind::Dynamic(..))
                        && matches!(oomir_src_operand.get_type(), Some(oomir::Type::Pointer(_)))
                    {
                        super::emit_trait_object_metadata(
                            oomir_src_operand,
                            &oomir_result_type,
                            temp_unop_var.clone(),
                            &format!("{base_temp_name}_trait_metadata"),
                            data_types,
                            &mut instructions,
                        );
                        produced_value = true;
                    } else {
                        let tail =
                            tcx.struct_tail_for_codegen(pointee, TypingEnv::fully_monomorphized());
                        if (tail.is_slice() || tail.is_str())
                            && matches!(oomir_src_operand.get_type(), Some(oomir::Type::Pointer(_)))
                        {
                            instructions.push(oomir::Instruction::InvokeVirtual {
                                dest: Some(temp_unop_var.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "metadata".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![(
                                        "self".to_string(),
                                        oomir_src_operand
                                            .get_type()
                                            .expect("DST metadata pointer must be typed"),
                                    )],
                                    ret: Box::new(oomir::Type::U64),
                                    is_static: false,
                                },
                                args: Vec::new(),
                                operand: oomir_src_operand,
                            });
                            produced_value = true;
                        }
                    }
                }
            }

            if produced_value {
                result_operand = oomir::Operand::Variable {
                    name: temp_unop_var,
                    ty: oomir_result_type,
                };
            } else if !oomir_result_type.has_jvm_value() {
                result_operand = oomir::Operand::Constant(oomir::Constant::Unit);
            } else {
                result_operand =
                    get_placeholder_operand(original_dest_place, mir, tcx, instance, data_types);
            }
        }

        Rvalue::Aggregate(box kind, operands) => {
            // Create a temporary variable to hold the aggregate
            let temp_aggregate_var = generate_temp_var_name(&base_temp_name);
            // Get the type from the original destination place
            let aggregate_oomir_type =
                get_place_type(original_dest_place, mir, tcx, instance, data_types);
            let aggregate_has_jvm_value = aggregate_oomir_type.has_jvm_value();

            match kind {
                rustc_middle::mir::AggregateKind::Tuple if !aggregate_has_jvm_value => {
                    // Coroutine MIR represents unit yields and returns as empty
                    // tuple aggregates, which have no JVM value or constructor.
                    debug_assert!(operands.is_empty());
                }
                rustc_middle::mir::AggregateKind::Tuple => {
                    let tuple_class_name = match &aggregate_oomir_type {
                        oomir::Type::Class(name) => name.clone(),
                        _ => panic!("Tuple aggregate type error"),
                    };
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Tuple Aggregate -> Temp Var '{}'",
                            temp_aggregate_var
                        )
                    );
                    let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
                    let mut constructor_args = Vec::new();
                    for (i, mir_op) in operands.iter().enumerate() {
                        let element_mir_ty = if let TyKind::Tuple(elements) = place_ty.kind() {
                            elements[i]
                        } else {
                            panic!("...")
                        };
                        let element_oomir_type =
                            ty_to_oomir_type(element_mir_ty.clone(), tcx, data_types, instance);
                        let value_operand = convert_operand(
                            mir_op,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                        let value_operand = adapt_value_for_field(
                            value_operand,
                            element_mir_ty,
                            &element_oomir_type,
                            &format!("{temp_aggregate_var}_field_{i}"),
                            tcx,
                            instance,
                            data_types,
                            &mut instructions,
                        );
                        constructor_args.push((value_operand, element_oomir_type));
                    }
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: temp_aggregate_var.clone(),
                        class_name: tuple_class_name.clone(),
                        args: constructor_args,
                    });
                }
                rustc_middle::mir::AggregateKind::Array(mir_element_ty) => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Array Aggregate -> Temp Var '{}'",
                            temp_aggregate_var
                        )
                    );
                    let oomir_element_type =
                        ty_to_oomir_type(*mir_element_ty, tcx, data_types, instance);
                    let array_size = operands.len();
                    let size_operand =
                        oomir::Operand::Constant(oomir::Constant::I32(array_size as i32));
                    instructions.push(oomir::Instruction::NewArray {
                        dest: temp_aggregate_var.clone(),
                        element_type: oomir_element_type.clone(),
                        size: size_operand,
                    });
                    // Store elements into the temporary array
                    for (i, mir_operand) in operands.iter().enumerate() {
                        let value_operand = convert_operand(
                            mir_operand,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                        let value_operand = adapt_value_for_field(
                            value_operand,
                            *mir_element_ty,
                            &oomir_element_type,
                            &format!("{}_{}", temp_aggregate_var, i),
                            tcx,
                            instance,
                            data_types,
                            &mut instructions,
                        );
                        let index_operand =
                            oomir::Operand::Constant(oomir::Constant::I32(i as i32));
                        instructions.push(oomir::Instruction::ArrayStore {
                            array: temp_aggregate_var.clone(),
                            index: index_operand,
                            value: value_operand,
                            copy_value: false,
                        });
                    }
                }
                rustc_middle::mir::AggregateKind::Closure(_, _) => {
                    let closure_class_name = match &aggregate_oomir_type {
                        oomir::Type::Class(name) => name.clone(),
                        _ => panic!("Closure aggregate type error"),
                    };
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Closure Aggregate -> Temp Var '{}' (Class: {})",
                            temp_aggregate_var, closure_class_name
                        )
                    );
                    let closure_fields = match data_types.get(&closure_class_name) {
                        Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
                        _ => Vec::new(),
                    };

                    let mut constructor_args = Vec::new();
                    for (i, mir_operand) in operands.iter().enumerate() {
                        let (_field_name, field_ty) =
                            closure_fields.get(i).cloned().unwrap_or_else(|| {
                                let operand_mir_ty = mir_operand.ty(&mir.local_decls, tcx);
                                (
                                    format!("arg{}", i),
                                    ty_to_oomir_type(operand_mir_ty, tcx, data_types, instance),
                                )
                            });
                        let value_operand = convert_operand(
                            mir_operand,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                        let value_operand = adapt_value_for_field(
                            value_operand,
                            mir_operand.ty(&mir.local_decls, tcx),
                            &field_ty,
                            &format!("{temp_aggregate_var}_field_{i}"),
                            tcx,
                            instance,
                            data_types,
                            &mut instructions,
                        );
                        constructor_args.push((value_operand, field_ty));
                    }
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: temp_aggregate_var.clone(),
                        class_name: closure_class_name.clone(),
                        args: constructor_args,
                    });
                }
                rustc_middle::mir::AggregateKind::Coroutine(_, _) => {
                    let coroutine_class_name = match &aggregate_oomir_type {
                        oomir::Type::Class(name) => name.clone(),
                        _ => panic!("Coroutine aggregate type error"),
                    };
                    let coroutine_fields = match data_types.get(&coroutine_class_name) {
                        Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
                        _ => Vec::new(),
                    };
                    let mut constructor_args = Vec::with_capacity(coroutine_fields.len());
                    for (field_name, field_ty) in &coroutine_fields {
                        let capture_index = field_name
                            .strip_prefix("arg")
                            .and_then(|index| index.parse::<usize>().ok());
                        if let Some((capture_index, mir_operand)) =
                            capture_index.and_then(|index| {
                                operands
                                    .get(FieldIdx::from_usize(index))
                                    .map(|op| (index, op))
                            })
                        {
                            let value = convert_operand(
                                mir_operand,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            let value = adapt_value_for_field(
                                value,
                                mir_operand.ty(&mir.local_decls, tcx),
                                field_ty,
                                &format!("{temp_aggregate_var}_field_{capture_index}"),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                            constructor_args.push((value, field_ty.clone()));
                        } else {
                            constructor_args.push((jvm_default_value(field_ty), field_ty.clone()));
                        }
                    }
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: temp_aggregate_var.clone(),
                        class_name: coroutine_class_name,
                        args: constructor_args,
                    });
                }
                rustc_middle::mir::AggregateKind::Adt(
                    def_id,
                    variant_idx,
                    substs,
                    _,
                    active_field_idx,
                ) => {
                    let adt_def = tcx.adt_def(*def_id);
                    let should_define_data_type = should_define_named_data_type(tcx, *def_id);
                    if !should_define_data_type {
                        super::super::types::force_define_named_adt(
                            Ty::new_adt(tcx, adt_def, substs),
                            tcx,
                            data_types,
                            instance,
                        );
                    }
                    if adt_def.is_struct() {
                        let variant = adt_def.variant(*variant_idx);
                        let jvm_class_name = generate_adt_jvm_class_name(
                            &adt_def, substs, tcx, data_types, instance,
                        );
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Handling Struct Aggregate -> Temp Var '{}' (Class: {})",
                                temp_aggregate_var, jvm_class_name
                            )
                        );
                        let oomir_fields: Vec<(String, oomir::Type)> = variant
                            .fields
                            .iter()
                            .filter_map(|f| {
                                let field_ty = ty_to_oomir_type(
                                    f.ty(tcx, substs).skip_norm_wip(),
                                    tcx,
                                    data_types,
                                    instance,
                                );
                                field_ty
                                    .has_jvm_value()
                                    .then(|| (f.ident(tcx).to_string(), field_ty))
                            })
                            .collect();
                        if should_define_data_type && !data_types.contains_key(&jvm_class_name) {
                            let mut methods = HashMap::new();
                            methods.insert(
                                "eq".to_string(),
                                DataTypeMethod::AdtHelperMethod {
                                    kind: oomir::AdtHelperKind::PartialEqClass {
                                        fields: oomir_fields.clone(),
                                    },
                                },
                            );
                            data_types.insert(
                                jvm_class_name.clone(),
                                oomir::DataType::Class {
                                    fields: oomir_fields.clone(),
                                    is_abstract: false,
                                    methods,
                                    super_class: None,
                                    interfaces: vec![],
                                },
                            );
                        } else if should_define_data_type
                            && let Some(oomir::DataType::Class {
                                fields, methods, ..
                            }) = data_types.get_mut(&jvm_class_name)
                        {
                            methods.entry("eq".to_string()).or_insert_with(|| {
                                DataTypeMethod::AdtHelperMethod {
                                    kind: oomir::AdtHelperKind::PartialEqClass {
                                        fields: fields.clone(),
                                    },
                                }
                            });
                        }

                        let mut constructor_args = Vec::new();
                        for (field_index, (field_def, mir_operand)) in
                            variant.fields.iter().zip(operands.iter()).enumerate()
                        {
                            let field_mir_ty = field_def.ty(tcx, substs).skip_norm_wip();
                            let field_oomir_type =
                                ty_to_oomir_type(field_mir_ty, tcx, data_types, instance);
                            if !field_oomir_type.has_jvm_value() {
                                continue;
                            }
                            let value_operand = convert_operand(
                                mir_operand,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            let value_operand = adapt_value_for_field(
                                value_operand,
                                field_mir_ty,
                                &field_oomir_type,
                                &format!("{temp_aggregate_var}_field_{field_index}"),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                            constructor_args.push((value_operand, field_oomir_type));
                        }
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: jvm_class_name.clone(),
                            args: constructor_args,
                        });
                    } else if adt_def.is_enum() {
                        let variant_def = adt_def.variant(*variant_idx);
                        let base_enum_name = generate_adt_jvm_class_name(
                            &adt_def, substs, tcx, data_types, instance,
                        );
                        let variant_class_name = format!(
                            "{}${}",
                            base_enum_name,
                            jvm_names::member_name(&variant_def.name.to_string())
                        );

                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Handling Enum Aggregate (Variant: {}) -> Temp Var '{}' (Class: {})",
                                variant_def.name, temp_aggregate_var, variant_class_name
                            )
                        );

                        if should_define_data_type {
                            let variants_info: Vec<_> = adt_def
                                .variants()
                                .iter()
                                .map(|v| {
                                    let v_name = jvm_names::member_name(&v.name.to_string());
                                    let v_fields: Vec<oomir::Type> = v
                                        .fields
                                        .iter()
                                        .filter_map(|f| {
                                            let field_ty = ty_to_oomir_type(
                                                f.ty(tcx, substs).skip_norm_wip(),
                                                tcx,
                                                data_types,
                                                instance,
                                            );
                                            field_ty.has_jvm_value().then_some(field_ty)
                                        })
                                        .collect();
                                    (v_name, v_fields)
                                })
                                .collect();

                            if !data_types.contains_key(&base_enum_name) {
                                let mut methods = HashMap::new();
                                methods.insert(
                                    "getVariantIdx".to_string(),
                                    DataTypeMethod::SimpleConstantReturn(oomir::Type::I32, None),
                                );

                                // Add AdtHelperMethod for PartialEq
                                methods.insert(
                                    "eq".to_string(),
                                    DataTypeMethod::AdtHelperMethod {
                                        kind: oomir::AdtHelperKind::PartialEqEnum {
                                            variants: variants_info.clone(),
                                        },
                                    },
                                );

                                if tcx.is_lang_item(adt_def.did(), rustc_hir::LangItem::Option) {
                                    methods.insert(
                                        "is_none".to_string(),
                                        DataTypeMethod::AdtHelperMethod {
                                            kind: oomir::AdtHelperKind::IsVariant {
                                                variant_idx: 0,
                                            },
                                        },
                                    );
                                    methods.insert(
                                        "is_some".to_string(),
                                        DataTypeMethod::AdtHelperMethod {
                                            kind: oomir::AdtHelperKind::IsVariant {
                                                variant_idx: 1,
                                            },
                                        },
                                    );
                                }

                                data_types.insert(
                                    base_enum_name.clone(),
                                    oomir::DataType::Class {
                                        fields: vec![],
                                        is_abstract: true,
                                        methods,
                                        super_class: None,
                                        interfaces: vec![],
                                    },
                                );
                            } else {
                                // Merge helper methods into existing enum class
                                if let oomir::DataType::Class { methods, .. } =
                                    data_types.get_mut(&base_enum_name).unwrap()
                                {
                                    if !methods.contains_key("eq") {
                                        methods.insert(
                                            "eq".to_string(),
                                            DataTypeMethod::AdtHelperMethod {
                                                kind: oomir::AdtHelperKind::PartialEqEnum {
                                                    variants: variants_info.clone(),
                                                },
                                            },
                                        );
                                    }
                                    if tcx.is_lang_item(adt_def.did(), rustc_hir::LangItem::Option)
                                    {
                                        if !methods.contains_key("is_none") {
                                            methods.insert(
                                                "is_none".to_string(),
                                                DataTypeMethod::AdtHelperMethod {
                                                    kind: oomir::AdtHelperKind::IsVariant {
                                                        variant_idx: 0,
                                                    },
                                                },
                                            );
                                        }
                                        if !methods.contains_key("is_some") {
                                            methods.insert(
                                                "is_some".to_string(),
                                                DataTypeMethod::AdtHelperMethod {
                                                    kind: oomir::AdtHelperKind::IsVariant {
                                                        variant_idx: 1,
                                                    },
                                                },
                                            );
                                        }
                                    }
                                }
                            }
                        }

                        // this variant
                        if should_define_data_type && !data_types.contains_key(&variant_class_name)
                        {
                            let mut fields = vec![];
                            for field in variant_def.fields.iter() {
                                let field_type = ty_to_oomir_type(
                                    field.ty(tcx, substs).skip_norm_wip(),
                                    tcx,
                                    data_types,
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
                                    Some(oomir::Constant::I32(variant_idx.as_u32() as i32)),
                                ),
                            );

                            data_types.insert(
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

                        let mut constructor_args = Vec::new();
                        for (i, field) in variant_def.fields.iter().enumerate() {
                            let field_mir_ty = field.ty(tcx, substs).skip_norm_wip();
                            let field_oomir_type =
                                ty_to_oomir_type(field_mir_ty, tcx, data_types, instance);
                            if !field_oomir_type.has_jvm_value() {
                                continue;
                            }
                            let value_operand = convert_operand(
                                &operands[FieldIdx::from_usize(i)],
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            let value_operand = adapt_value_for_field(
                                value_operand,
                                field_mir_ty,
                                &field_oomir_type,
                                &format!("{temp_aggregate_var}_field_{i}"),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                            constructor_args.push((value_operand, field_oomir_type));
                        }
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: variant_class_name.clone(),
                            args: constructor_args,
                        });
                    } else {
                        let union_class_name = if should_define_data_type {
                            ensure_union_data_type(&adt_def, substs, tcx, data_types, instance)
                        } else {
                            generate_adt_jvm_class_name(&adt_def, substs, tcx, data_types, instance)
                        };
                        let active_field_idx = active_field_idx.unwrap_or(FieldIdx::from_usize(0));
                        let variant = adt_def.variant(*variant_idx);
                        let field_def = &variant.fields[active_field_idx];
                        let field_name = field_def.ident(tcx).to_string();
                        let field_mir_ty = field_def.ty(tcx, substs).skip_norm_wip();
                        let field_oomir_ty =
                            ty_to_oomir_type(field_mir_ty, tcx, data_types, instance);
                        let is_unit = !field_oomir_ty.has_jvm_value();
                        let value_operand = (!is_unit).then(|| {
                            let value = convert_operand(
                                &operands[FieldIdx::from_usize(0)],
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                            );
                            adapt_value_for_field(
                                value,
                                field_mir_ty,
                                &field_oomir_ty,
                                &temp_aggregate_var,
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            )
                        });

                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mir-lowering",
                            format!(
                                "Info: Handling Union Aggregate field '{}' -> Temp Var '{}' (Class: {})",
                                field_name, temp_aggregate_var, union_class_name
                            )
                        );

                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: Some(temp_aggregate_var.clone()),
                            class_name: union_class_name.clone(),
                            method_name: union_from_method_name(&field_name),
                            method_ty: oomir::Signature {
                                params: if is_unit {
                                    vec![]
                                } else {
                                    vec![("value".to_string(), field_oomir_ty)]
                                },
                                ret: Box::new(oomir::Type::Class(union_class_name)),
                                is_static: true,
                            },
                            args: value_operand.into_iter().collect(),
                        });
                    }
                }
                rustc_middle::mir::AggregateKind::RawPtr(pointee_ty, _) => {
                    let pointee_ty = EarlyBinder::bind(tcx, *pointee_ty)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip();
                    let data = convert_operand(
                        &operands[FieldIdx::from_usize(0)],
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    );
                    if pointee_ty.is_slice() || pointee_ty.is_str() {
                        let metadata = convert_operand(
                            &operands[FieldIdx::from_usize(1)],
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                        let is_str = pointee_ty.is_str();
                        let view_class = if is_str {
                            oomir::UTF8_VIEW_CLASS
                        } else {
                            oomir::SLICE_VIEW_CLASS
                        };
                        let (backing, offset) = if is_str {
                            emit_pointer_slice_parts(data, &temp_aggregate_var, &mut instructions)
                        } else {
                            (data, oomir::Operand::Constant(oomir::Constant::I32(0)))
                        };
                        instructions.push(oomir::Instruction::ConstructObject {
                            dest: temp_aggregate_var.clone(),
                            class_name: view_class.to_string(),
                            args: vec![
                                (backing, oomir::Type::Class("java/lang/Object".to_string())),
                                (offset, oomir::Type::I32),
                                (metadata, oomir::Type::I32),
                            ],
                        });
                    } else if {
                        let tail = tcx
                            .struct_tail_for_codegen(pointee_ty, TypingEnv::fully_monomorphized());
                        tail.is_slice() || tail.is_str()
                    } {
                        let data_ty = data
                            .get_type()
                            .expect("slice-tailed raw pointer data pointer is typed");
                        let metadata = convert_operand(
                            &operands[FieldIdx::from_usize(1)],
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        );
                        let pointee_size = super::super::types::layout_size_bytes(tcx, pointee_ty)
                            .expect("slice-tailed raw pointer has a static prefix layout");
                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: Some(temp_aggregate_var.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "retypeWithMetadata".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("pointer".to_string(), data_ty),
                                    ("view_size".to_string(), oomir::Type::U64),
                                    ("view_codec".to_string(), oomir::Type::java_string()),
                                    ("metadata".to_string(), oomir::Type::U64),
                                ],
                                ret: Box::new(aggregate_oomir_type.clone()),
                                is_static: true,
                            },
                            args: vec![
                                data,
                                oomir::Operand::Constant(oomir::Constant::U64(
                                    u64::try_from(pointee_size)
                                        .expect("Rust raw DST prefix layout exceeds u64"),
                                )),
                                super::super::types::pointer_view_codec_operand(
                                    pointee_ty, tcx, data_types, instance,
                                ),
                                metadata,
                            ],
                        });
                    } else if matches!(pointee_ty.kind(), TyKind::Dynamic(_, _)) {
                        let data_ty = data
                            .get_type()
                            .expect("raw trait-object aggregate data pointer is typed");
                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: Some(temp_aggregate_var.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "restoreErasedView".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![("pointer".to_string(), data_ty)],
                                ret: Box::new(aggregate_oomir_type.clone()),
                                is_static: true,
                            },
                            args: vec![data],
                        });
                    } else {
                        let data_ty = data
                            .get_type()
                            .expect("thin raw pointer aggregate data pointer is typed");
                        if let Ok(pointee_size) =
                            super::super::types::layout_size_bytes(tcx, pointee_ty)
                        {
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(temp_aggregate_var.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "retype".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("pointer".to_string(), data_ty),
                                        ("view_size".to_string(), oomir::Type::U64),
                                        ("view_codec".to_string(), oomir::Type::java_string()),
                                    ],
                                    ret: Box::new(aggregate_oomir_type.clone()),
                                    is_static: true,
                                },
                                args: vec![
                                    data,
                                    oomir::Operand::Constant(oomir::Constant::U64(
                                        u64::try_from(pointee_size)
                                            .expect("Rust raw pointer layout exceeds u64"),
                                    )),
                                    super::super::types::pointer_view_codec_operand(
                                        pointee_ty, tcx, data_types, instance,
                                    ),
                                ],
                            });
                        } else {
                            // A generic thin pointer has no metadata operand carrying T's
                            // layout. Its data pointer came through an erased (`*const ()`)
                            // view, which records the prior concrete view in Pointer.
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(temp_aggregate_var.clone()),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "restoreErasedView".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![("pointer".to_string(), data_ty)],
                                    ret: Box::new(aggregate_oomir_type.clone()),
                                    is_static: true,
                                },
                                args: vec![data],
                            });
                        }
                    }
                }
                _ => {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mir-lowering",
                        format!(
                            "Warning: Unhandled non-pointer Aggregate Kind {:?} -> Temp Placeholder",
                            kind
                        )
                    );
                }
            }

            result_operand = if aggregate_has_jvm_value {
                oomir::Operand::Variable {
                    name: temp_aggregate_var,
                    ty: aggregate_oomir_type,
                }
            } else {
                oomir::Operand::Constant(oomir::Constant::Unit)
            };
        }
        Rvalue::RawPtr(kind, place) => {
            match kind {
                rustc_middle::mir::RawPtrKind::FakeForPtrMetadata => {
                    let (place_temp_var_name, place_get_instructions, place_temp_var_type) =
                        emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
                    instructions.extend(place_get_instructions);
                    // This pointer is *only* created to get metadata (like length)
                    // from the underlying place. The actual pointer value is irrelevant
                    // in the target code. The subsequent PtrMetadata operation will
                    // operate on the operand representing the place itself.
                    // So, we just pass the place's operand through.
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!(
                            "Info: Handling Rvalue::RawPtr(FakeForPtrMetadata) for place '{:?}'. Passing through place operand '{}' ({:?}).",
                            place_to_string(place, tcx),
                            place_temp_var_name,
                            place_temp_var_type
                        )
                    );
                    result_operand = oomir::Operand::Variable {
                        name: place_temp_var_name, // Use the operand computed for the place
                        ty: place_temp_var_type,
                    };
                }
                rustc_middle::mir::RawPtrKind::Const | rustc_middle::mir::RawPtrKind::Mut => {
                    let pointer_oomir_type =
                        get_place_type(original_dest_place, mir, tcx, instance, data_types);
                    if !matches!(pointer_oomir_type, oomir::Type::Pointer(_)) {
                        // Unsized raw pointers retain their existing fat
                        // carrier (SliceView/Utf8View/trait interface). In
                        // optimized MIR this is the first half of operations
                        // such as slice::as_ptr; allocating a scalar Pointer
                        // cell here would lose the metadata and invent an
                        // impossible runtime return type.
                        let (name, get_instructions, ty) =
                            emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
                        instructions.extend(get_instructions);
                        result_operand = oomir::Operand::Variable { name, ty };
                        return (instructions, result_operand);
                    }
                    result_operand = reuse_pointer_to_place(
                        place,
                        &pointer_oomir_type,
                        &base_temp_name,
                        pointer_origins,
                        available_pointer_locals,
                        mir,
                        &mut instructions,
                    )
                    .unwrap_or_else(|| {
                        emit_pointer_to_place(
                            place,
                            &pointer_oomir_type,
                            &base_temp_name,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                        )
                    });
                }
            }
        }
        Rvalue::Discriminant(place) => {
            // 1. Generate instructions to get the actual value from the place
            let (actual_value_var_name, get_instructions, actual_value_oomir_type) =
                emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);

            // Add the instructions needed to get the value (e.g., ArrayGet)
            instructions.extend(get_instructions);

            // 2. Now operate on the variable holding the actual value
            let temp_discriminant_var = generate_temp_var_name(&base_temp_name);

            let place_class_name = match actual_value_oomir_type.clone() {
                oomir::Type::Class(name) => name.clone(),
                // Handle potential references if get_on_own returns Ref(Class)
                oomir::Type::Reference(inner) => {
                    if let oomir::Type::Class(name) = inner.as_ref() {
                        name.clone()
                    } else {
                        panic!("Discriminant on Ref to non-class type: {:?}", inner)
                    }
                }
                oomir::Type::MutableReference(inner) => {
                    if let oomir::Type::Class(name) = inner.as_ref() {
                        name.clone()
                    } else {
                        panic!("Discriminant on MutableRef to non-class type: {:?}", inner)
                    }
                }
                _ => panic!(
                    "Discriminant on non-class type: {:?}",
                    actual_value_oomir_type
                ),
            };

            let place_mir_ty =
                normalize_unsize_ty(place.ty(&mir.local_decls, tcx).ty, tcx, instance);
            if matches!(place_mir_ty.kind(), TyKind::Coroutine(..)) {
                instructions.push(oomir::Instruction::GetField {
                    dest: temp_discriminant_var.clone(),
                    object: oomir::Operand::Variable {
                        name: actual_value_var_name,
                        ty: actual_value_oomir_type,
                    },
                    field_name: "__state".to_string(),
                    field_ty: oomir::Type::I32,
                    owner_class: place_class_name,
                });
                let result_ty = get_place_type(original_dest_place, mir, tcx, instance, data_types);
                if result_ty == oomir::Type::I32 {
                    result_operand = oomir::Operand::Variable {
                        name: temp_discriminant_var,
                        ty: oomir::Type::I32,
                    };
                } else {
                    let cast_dest = generate_temp_var_name(&base_temp_name);
                    instructions.push(oomir::Instruction::Cast {
                        op: oomir::Operand::Variable {
                            name: temp_discriminant_var,
                            ty: oomir::Type::I32,
                        },
                        ty: result_ty.clone(),
                        dest: cast_dest.clone(),
                    });
                    result_operand = oomir::Operand::Variable {
                        name: cast_dest,
                        ty: result_ty,
                    };
                }
                return (instructions, result_operand);
            }
            let use_numeric_discriminant = match place_mir_ty.kind() {
                TyKind::Adt(adt_def, _) if adt_def.is_enum() => {
                    enum_union_discriminant_supported(adt_def, tcx)
                }
                _ => false,
            };
            let method_name = if use_numeric_discriminant {
                ENUM_UNION_DISCRIMINANT_METHOD.to_string()
            } else {
                "getVariantIdx".to_string()
            };
            let method_return_type = if use_numeric_discriminant {
                oomir::Type::I64
            } else {
                oomir::Type::I32
            };

            let method_ty = oomir::Signature {
                params: vec![],
                ret: Box::new(method_return_type.clone()),
                is_static: false,
            };

            // 3. Call InvokeVirtual on the CORRECT variable
            instructions.push(oomir::Instruction::InvokeVirtual {
                class_name: place_class_name.clone(),
                method_name,
                args: vec![],
                dest: Some(temp_discriminant_var.clone()),
                method_ty,
                operand: oomir::Operand::Variable {
                    name: actual_value_var_name,
                    ty: actual_value_oomir_type,
                },
            });

            // 4. Convert a numeric enum discriminant to the MIR destination's
            // integer type. Other enum shapes retain the variant-index path.
            if use_numeric_discriminant {
                let result_ty = get_place_type(original_dest_place, mir, tcx, instance, data_types);
                if result_ty == method_return_type {
                    result_operand = oomir::Operand::Variable {
                        name: temp_discriminant_var,
                        ty: method_return_type,
                    };
                } else {
                    let cast_dest = generate_temp_var_name(&base_temp_name);
                    instructions.push(oomir::Instruction::Cast {
                        op: oomir::Operand::Variable {
                            name: temp_discriminant_var,
                            ty: method_return_type,
                        },
                        ty: result_ty.clone(),
                        dest: cast_dest.clone(),
                    });
                    result_operand = oomir::Operand::Variable {
                        name: cast_dest,
                        ty: result_ty,
                    };
                }
            } else {
                result_operand = oomir::Operand::Variable {
                    name: temp_discriminant_var,
                    ty: method_return_type,
                };
            }
        }
        Rvalue::CopyForDeref(place) => {
            // Need to get the value from the source place first
            let (temp_var_name, get_instructions, temp_var_type) =
                emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
            instructions.extend(get_instructions);
            result_operand = oomir::Operand::Variable {
                name: temp_var_name,
                ty: temp_var_type,
            };
        }
        // Handle other Rvalue variants by generating a placeholder
        _ => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "mir-lowering",
                format!(
                    "Warning: Unhandled Rvalue: {:?} for temp based on {:?}. Emitting placeholder.",
                    rvalue, original_dest_place
                )
            );
            result_operand =
                get_placeholder_operand(original_dest_place, mir, tcx, instance, data_types);
            // No instructions needed to "calculate" a placeholder
        }
    }

    (instructions, result_operand)
}
