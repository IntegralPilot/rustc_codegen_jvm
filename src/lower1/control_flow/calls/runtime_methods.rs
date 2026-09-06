//! Runtime methods.
use super::*;

pub(super) fn concrete<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    func_instance: Instance<'tcx>,
    fn_output: Ty<'tcx>,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    method_signature: oomir::Signature,
    item: rustc_middle::ty::AssocItem,
    receiver_mir_ty: Ty<'tcx>,
    receiver_operand: oomir::Operand,
    method_args: Vec<oomir::Operand>,
    method_name: String,
    declared_method_name: String,
    resolved_receiver_mir_ty: Ty<'tcx>,
    is_pointer_null_method: bool,
    is_pointer_cast_method: bool,
    receiver_self_requires_static_dispatch: bool,
    uses_concrete_trait_default: bool,
    comparison_rhs_ty: Option<oomir::Type>,
    pointer_api_receiver: bool,
) {
    // The receiver is a concrete class - use InvokeVirtual
    let class_type =
        crate::lower1::types::ty_to_oomir_type(receiver_mir_ty, tcx, data_types, instance);
    let runtime_static_target = match &class_type {
        oomir::Type::Str if declared_method_name == "as_bytes" => {
            Some((oomir::UTF8_VIEW_CLASS.to_string(), "asSlice".to_string()))
        }
        oomir::Type::Str
            if declared_method_name == "starts_with"
                && oomir_operands.get(1).is_some_and(|operand| {
                    matches!(operand.get_type(), Some(oomir::Type::I32))
                }) =>
        {
            Some((
                oomir::UTF8_VIEW_CLASS.to_string(),
                "startsWithChar".to_string(),
            ))
        }
        oomir::Type::Str if declared_method_name == "starts_with" => {
            Some((oomir::UTF8_VIEW_CLASS.to_string(), "startsWith".to_string()))
        }
        oomir::Type::Str
            if declared_method_name == "eq"
                && comparison_rhs_ty.as_ref() == Some(&oomir::Type::Str) =>
        {
            Some((oomir::UTF8_VIEW_CLASS.to_string(), "equals".to_string()))
        }
        oomir::Type::Str if declared_method_name == "len" => {
            Some((oomir::UTF8_VIEW_CLASS.to_string(), "len".to_string()))
        }
        oomir::Type::Slice(element)
            if declared_method_name == "starts_with"
                && matches!(element.as_ref(), oomir::Type::I8 | oomir::Type::U8) =>
        {
            Some((
                oomir::SLICE_VIEW_CLASS.to_string(),
                "startsWithI8".to_string(),
            ))
        }
        oomir::Type::Slice(element)
            if declared_method_name == "starts_with"
                && matches!(
                    element.as_ref(),
                    oomir::Type::I32 | oomir::Type::U32 | oomir::Type::Char
                ) =>
        {
            Some((
                oomir::SLICE_VIEW_CLASS.to_string(),
                "startsWithI32".to_string(),
            ))
        }
        oomir::Type::Slice(_) if declared_method_name == "starts_with" => Some((
            oomir::SLICE_VIEW_CLASS.to_string(),
            "startsWith".to_string(),
        )),
        oomir::Type::Slice(_)
            if matches!(declared_method_name.as_str(), "as_ptr" | "as_mut_ptr") =>
        {
            Some((oomir::POINTER_CLASS.to_string(), "fromSlice".to_string()))
        }
        oomir::Type::Pointer(_)
            if pointer_api_receiver
                && (is_pointer_cast_method
                    || is_pointer_null_method
                    || matches!(
                        declared_method_name.as_str(),
                        "add"
                            | "sub"
                            | "offset"
                            | "offset_from"
                            | "offsetFrom"
                            | "offset_from_unsigned"
                            | "byte_offset_from"
                            | "byte_offset_from_unsigned"
                            | "wrapping_add"
                            | "wrapping_sub"
                            | "wrapping_offset"
                            | "byte_add"
                            | "byte_sub"
                            | "byte_offset"
                            | "wrapping_byte_add"
                            | "wrapping_byte_sub"
                            | "wrapping_byte_offset"
                            | "align_offset"
                            | "is_aligned_to"
                            | "addr"
                            | "expose_provenance"
                            | "with_addr"
                            | "map_addr"
                            | "with_metadata_of"
                    )) =>
        {
            let source_is_trait_object = match resolved_receiver_mir_ty.kind() {
                TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                    matches!(pointee.kind(), TyKind::Dynamic(..))
                }
                _ => false,
            };
            let target_has_trait_object_tail = match fn_output.kind() {
                TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                    matches!(
                        pointee.kind(),
                        TyKind::Adt(adt_def, _)
                            if adt_def.is_struct()
                    ) && matches!(
                        tcx.struct_tail_for_codegen(*pointee, TypingEnv::fully_monomorphized(),)
                            .kind(),
                        TyKind::Dynamic(..)
                    )
                }
                _ => false,
            };
            let target_pointee_is_sized = match fn_output.kind() {
                TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                    crate::lower1::types::is_codegen_sized(*pointee, tcx)
                }
                _ => false,
            };
            Some((
                oomir::POINTER_CLASS.to_string(),
                if is_pointer_cast_method && source_is_trait_object && target_has_trait_object_tail
                {
                    "retypeStructTailFromTraitPointer".to_string()
                } else if is_pointer_cast_method
                    && source_is_trait_object
                    && target_pointee_is_sized
                {
                    "traitObjectDataPointer".to_string()
                } else if declared_method_name == "with_metadata_of" && target_has_trait_object_tail
                {
                    "retypeStructTailWithMetadataOf".to_string()
                } else if declared_method_name == "with_metadata_of" {
                    "retypeWithMetadataOf".to_string()
                } else if is_pointer_cast_method {
                    "retype".to_string()
                } else {
                    declared_method_name.clone()
                },
            ))
        }
        oomir::Type::Class(class_name)
            if class_name == crate::lower2::F128_CLASS && item.name().as_str() == "to_bits" =>
        {
            Some((crate::lower2::F128_CLASS.to_string(), "to_bits".to_string()))
        }
        _ => None,
    };
    let static_target = runtime_static_target
        .map(|(class_name, method_name)| (class_name, method_name, false))
        .or_else(|| {
            (uses_concrete_trait_default
                || receiver_self_requires_static_dispatch
                || requires_compiled_static_dispatch(&class_type))
            .then(|| {
                let target = crate::lower1::naming::mono_fn_name_from_instance(tcx, func_instance);
                (
                    target
                        .class_to_call_on
                        .expect("monomorphized functions have JVM owners"),
                    target.method_name,
                    true,
                )
            })
        });

    if let Some((class_name, static_method_name, generated_rust_target)) = static_target {
        let mut static_signature = method_signature;
        static_signature.is_static = true;
        let mut static_args = oomir_operands.clone();
        if class_name == oomir::POINTER_CLASS && static_method_name == "fromSlice" {
            static_signature.params = vec![
                (
                    "slice".to_string(),
                    oomir::Type::Class("java/lang/Object".to_string()),
                ),
                ("element_size".to_string(), oomir::Type::U64),
                ("codec".to_string(), oomir::Type::java_string()),
            ];
            let receiver_value_ty = match receiver_mir_ty.kind() {
                TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
                _ => receiver_mir_ty,
            };
            let element_ty = receiver_value_ty.sequence_element_type(tcx);
            let element_ty = EarlyBinder::bind(tcx, element_ty)
                .instantiate(tcx, instance.args)
                .skip_norm_wip();
            let element_size = crate::lower1::types::layout_size_bytes(tcx, element_ty)
                .unwrap_or_else(|error| {
                    panic!("could not determine slice pointer element size: {error}")
                });
            static_args.push(oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(element_size).expect("Rust slice element layout exceeds u64"),
            )));
            static_args.push(crate::lower1::types::pointer_view_codec_operand(
                element_ty, tcx, data_types, instance,
            ));
        } else if class_name == oomir::POINTER_CLASS
            && matches!(
                static_method_name.as_str(),
                "retype"
                    | "retypeWithMetadataOf"
                    | "retypeStructTailWithMetadataOf"
                    | "retypeStructTailFromTraitPointer"
                    | "traitObjectDataPointer"
            )
        {
            static_signature
                .params
                .push(("view_size".to_string(), oomir::Type::U64));
            static_signature
                .params
                .push(("view_codec".to_string(), oomir::Type::java_string()));
            let target_pointee = match fn_output.kind() {
                TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
                other => panic!("pointer cast returned non-pointer type {other:?}"),
            };
            let target_pointee = EarlyBinder::bind(tcx, target_pointee)
                .instantiate(tcx, instance.args)
                .skip_norm_wip();
            let target_size = crate::lower1::types::layout_size_bytes(tcx, target_pointee)
                .unwrap_or_else(|error| {
                    panic!("could not determine pointer cast target size: {error}")
                });
            static_args.push(oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(target_size).expect("Rust pointer target layout exceeds u64"),
            )));
            static_args.push(crate::lower1::types::pointer_view_codec_operand(
                target_pointee,
                tcx,
                data_types,
                instance,
            ));
        } else if class_name == oomir::POINTER_CLASS
            && static_method_name == "map_addr"
            && static_signature.params.len() >= 2
        {
            static_signature.params[1].1 = oomir::Type::Class("java/lang/Object".to_string());
        }
        if class_name.starts_with("org/rustlang/runtime/")
            && static_signature
                .params
                .last()
                .is_some_and(|(name, _)| name == oomir::CALLER_LOCATION_PARAM_NAME)
        {
            static_signature.params.pop();
            static_args.pop();
        }
        instructions.push(if generated_rust_target {
            oomir::Instruction::InvokeRustStatic {
                class_name,
                method_name: static_method_name,
                method_ty: static_signature,
                args: static_args,
                dest: effective_dest,
            }
        } else {
            oomir::Instruction::InvokeStatic {
                class_name,
                method_name: static_method_name,
                method_ty: static_signature,
                args: static_args,
                dest: effective_dest,
            }
        });
    } else {
        let class_name = class_type
        .get_class_name()
        .unwrap_or_else(|| {
            panic!(
                "no JVM class is available for virtual method `{declared_method_name}` on {class_type:?}"
            )
        })
        .to_string();

        instructions.push(oomir::Instruction::InvokeVirtual {
            class_name,
            method_name,
            method_ty: method_signature,
            args: method_args,
            dest: effective_dest,
            operand: receiver_operand,
        });
    }
}
