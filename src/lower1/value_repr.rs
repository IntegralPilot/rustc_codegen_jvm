use crate::lower1::context::Definitions;
use rustc_abi::{BackendRepr, TagEncoding, VariantIdx, Variants};
use rustc_middle::ty::{EarlyBinder, Instance, Ty, TyCtxt, TyKind, TypingEnv};

use crate::oomir;

use super::{
    jvm_names,
    types::{
        adapt_simple_enum_operand, ensure_exact_transmute_helper, ensure_union_data_type,
        generate_adt_jvm_class_name, generate_tuple_jvm_class_name, ty_to_oomir_type,
    },
};

enum RustValueRepresentation {
    NoJvmValue,
    Direct,
    EnumObject {
        base_class: String,
        layout: EnumRepresentation,
    },
}

enum EnumRepresentation {
    Uninhabited,
    Single { variant: VariantIdx },
    Tagged,
}

fn resolved_ty<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) -> Ty<'tcx> {
    let instantiated = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    tcx.try_normalize_erasing_regions(
        TypingEnv::fully_monomorphized(),
        rustc_middle::ty::Unnormalized::new_wip(instantiated),
    )
    .unwrap_or(instantiated)
}

fn representation<'tcx>(
    rust_ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
) -> RustValueRepresentation {
    let rust_ty = resolved_ty(rust_ty, tcx, instance);
    let jvm_ty = ty_to_oomir_type(rust_ty, tcx, data_types, instance);
    if !jvm_ty.has_jvm_value() {
        return RustValueRepresentation::NoJvmValue;
    }

    let TyKind::Adt(adt_def, substs) = rust_ty.kind() else {
        return RustValueRepresentation::Direct;
    };
    if !adt_def.is_enum() {
        return RustValueRepresentation::Direct;
    }

    let base_class = generate_adt_jvm_class_name(adt_def, substs, tcx, data_types, instance);
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(rust_ty))
        .unwrap_or_else(|err| panic!("could not determine enum layout for {rust_ty:?}: {err:?}"));
    let layout = match layout.variants {
        Variants::Empty => EnumRepresentation::Uninhabited,
        Variants::Single { index } => EnumRepresentation::Single { variant: index },
        Variants::Multiple { .. } => EnumRepresentation::Tagged,
    };
    RustValueRepresentation::EnumObject { base_class, layout }
}

fn operand_var(name: String, ty: oomir::Type) -> oomir::Operand {
    oomir::Operand::Variable { name, ty }
}

pub(super) fn emit_trait_object_reference_pointer(
    source: oomir::Operand,
    target_jvm_ty: &oomir::Type,
    temp_prefix: &str,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let dest = format!("{temp_prefix}_trait_object_pointer");
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(dest.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "fromTraitObjectReference".to_string(),
        method_ty: oomir::Signature {
            params: vec![(
                "reference".to_string(),
                oomir::Type::Class("java/lang/Object".to_string()),
            )],
            ret: Box::new(target_jvm_ty.clone()),
            is_static: true,
        },
        args: vec![source],
    });
    operand_var(dest, target_jvm_ty.clone())
}

fn integer_constant_bits(constant: &oomir::Constant) -> Option<u128> {
    match constant {
        oomir::Constant::I8(value) => Some(*value as u8 as u128),
        oomir::Constant::U8(value) => Some((*value).into()),
        oomir::Constant::I16(value) => Some(*value as u16 as u128),
        oomir::Constant::U16(value) => Some((*value).into()),
        oomir::Constant::I32(value) => Some(*value as u32 as u128),
        oomir::Constant::U32(value) => Some((*value).into()),
        oomir::Constant::I64(value) => Some(*value as u64 as u128),
        oomir::Constant::U64(value) => Some((*value).into()),
        oomir::Constant::Boolean(value) => Some(u128::from(*value)),
        oomir::Constant::Char(value) => Some(u128::from(*value as u32)),
        _ => None,
    }
}

fn materialize_direct_fieldless_enum_constant<'tcx>(
    source: &oomir::Operand,
    target_rust_ty: Ty<'tcx>,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let oomir::Operand::Constant(constant) = source else {
        return None;
    };
    let bits = integer_constant_bits(constant)?;
    let TyKind::Adt(adt_def, _) = target_rust_ty.kind() else {
        return None;
    };
    if !adt_def.is_enum()
        || adt_def
            .variants()
            .iter()
            .any(|variant| !variant.fields.is_empty())
    {
        return None;
    }
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(target_rust_ty))
        .ok()?;
    let Variants::Multiple {
        tag,
        tag_encoding: TagEncoding::Direct,
        ..
    } = layout.variants
    else {
        return None;
    };
    let bit_width = tag.size(&tcx.data_layout).bits();
    let mask = if bit_width == 128 {
        u128::MAX
    } else {
        (1_u128 << bit_width) - 1
    };
    let variant = adt_def
        .discriminants(tcx)
        .find_map(|(variant, discriminant)| {
            ((discriminant.val & mask) == (bits & mask)).then_some(variant)
        })?;
    construct_fieldless_enum_variant(
        target_rust_ty,
        variant,
        temp_prefix,
        tcx,
        instance,
        data_types,
        instructions,
    )
}

pub(super) fn construct_fieldless_enum_variant<'tcx>(
    enum_ty: Ty<'tcx>,
    variant: VariantIdx,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let enum_ty = resolved_ty(enum_ty, tcx, instance);
    let TyKind::Adt(adt_def, substs) = enum_ty.kind() else {
        return None;
    };
    if !adt_def.is_enum() {
        return None;
    }
    let variant_def = adt_def.variant(variant);
    if variant_def.fields.iter().any(|field| {
        let field_ty = resolved_ty(field.ty(tcx, substs).skip_norm_wip(), tcx, instance);
        ty_to_oomir_type(field_ty, tcx, data_types, instance).has_jvm_value()
    }) {
        return None;
    }

    let base_class = generate_adt_jvm_class_name(adt_def, substs, tcx, data_types, instance);
    let variant_class = format!(
        "{}${}",
        base_class,
        jvm_names::member_name(&variant_def.name.to_string())
    );
    let dest = format!("{temp_prefix}_enum_value");
    instructions.push(oomir::Instruction::ConstructObject {
        dest: dest.clone(),
        class_name: variant_class.clone(),
        args: Vec::new(),
    });
    Some(operand_var(dest, oomir::Type::Class(variant_class)))
}

pub(super) fn materialize_implicit_zst<'tcx>(
    rust_ty: Ty<'tcx>,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let rust_ty = resolved_ty(rust_ty, tcx, instance);
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(rust_ty))
        .ok()?;
    if layout.size.bytes() != 0 {
        return None;
    }

    match rust_ty.kind() {
        TyKind::Adt(adt_def, substs) if adt_def.is_enum() => {
            let Variants::Single { index } = layout.variants else {
                return None;
            };
            let variant = adt_def.variant(index);
            let base_class =
                generate_adt_jvm_class_name(adt_def, substs, tcx, data_types, instance);
            let variant_class = format!(
                "{}${}",
                base_class,
                jvm_names::member_name(&variant.name.to_string())
            );
            let mut args = Vec::new();
            for (field_index, field) in variant.fields.iter().enumerate() {
                let field_ty = resolved_ty(field.ty(tcx, substs).skip_norm_wip(), tcx, instance);
                let field_jvm_ty = ty_to_oomir_type(field_ty, tcx, data_types, instance);
                if !field_jvm_ty.has_jvm_value() {
                    continue;
                }
                let field_value = materialize_implicit_zst(
                    field_ty,
                    &format!("{temp_prefix}_field_{field_index}"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                )?;
                args.push((field_value, field_jvm_ty));
            }
            let dest = format!("{temp_prefix}_enum_value");
            instructions.push(oomir::Instruction::ConstructObject {
                dest: dest.clone(),
                class_name: variant_class.clone(),
                args,
            });
            Some(operand_var(dest, oomir::Type::Class(variant_class)))
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_struct() => {
            let class_name =
                generate_adt_jvm_class_name(adt_def, substs, tcx, data_types, instance);
            let mut args = Vec::new();
            for (field_index, field) in adt_def.variant(0usize.into()).fields.iter().enumerate() {
                let field_ty = resolved_ty(field.ty(tcx, substs).skip_norm_wip(), tcx, instance);
                let field_jvm_ty = ty_to_oomir_type(field_ty, tcx, data_types, instance);
                if !field_jvm_ty.has_jvm_value() {
                    continue;
                }
                let field_value = materialize_implicit_zst(
                    field_ty,
                    &format!("{temp_prefix}_field_{field_index}"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                )?;
                args.push((field_value, field_jvm_ty));
            }
            let dest = format!("{temp_prefix}_struct_value");
            instructions.push(oomir::Instruction::ConstructObject {
                dest: dest.clone(),
                class_name: class_name.clone(),
                args,
            });
            Some(operand_var(dest, oomir::Type::Class(class_name)))
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_union() => {
            let class_name = ensure_union_data_type(adt_def, substs, tcx, data_types, instance);
            let bytes = format!("{temp_prefix}_union_bytes");
            let objects = format!("{temp_prefix}_union_objects");
            instructions.push(oomir::Instruction::NewArray {
                dest: bytes.clone(),
                element_type: oomir::Type::I8,
                size: oomir::Operand::Constant(oomir::Constant::I32(0)),
            });
            instructions.push(oomir::Instruction::NewArray {
                dest: objects.clone(),
                element_type: oomir::Type::Class("java/lang/Object".to_string()),
                // Keep one identity slot for the runtime's uniform union
                // representation even though a ZST has no addressable bytes.
                size: oomir::Operand::Constant(oomir::Constant::I32(1)),
            });
            let dest = format!("{temp_prefix}_union_value");
            instructions.push(oomir::Instruction::ConstructObject {
                dest: dest.clone(),
                class_name: class_name.clone(),
                args: vec![
                    (
                        operand_var(bytes, oomir::Type::Array(Box::new(oomir::Type::I8))),
                        oomir::Type::Array(Box::new(oomir::Type::I8)),
                    ),
                    (
                        operand_var(
                            objects,
                            oomir::Type::Array(Box::new(oomir::Type::Class(
                                "java/lang/Object".to_string(),
                            ))),
                        ),
                        oomir::Type::Array(Box::new(oomir::Type::Class(
                            "java/lang/Object".to_string(),
                        ))),
                    ),
                ],
            });
            Some(operand_var(dest, oomir::Type::Class(class_name)))
        }
        TyKind::Tuple(elements) if !elements.is_empty() => {
            let element_tys = elements.iter().collect::<Vec<_>>();
            let class_name = generate_tuple_jvm_class_name(&element_tys, tcx, data_types, instance);
            let mut args = Vec::new();
            for (index, element_ty) in element_tys.into_iter().enumerate() {
                let element_jvm_ty = ty_to_oomir_type(element_ty, tcx, data_types, instance);
                if !element_jvm_ty.has_jvm_value() {
                    continue;
                }
                let element_value = materialize_implicit_zst(
                    element_ty,
                    &format!("{temp_prefix}_field_{index}"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                )?;
                args.push((element_value, element_jvm_ty));
            }
            let dest = format!("{temp_prefix}_tuple_value");
            instructions.push(oomir::Instruction::ConstructObject {
                dest: dest.clone(),
                class_name: class_name.clone(),
                args,
            });
            Some(operand_var(dest, oomir::Type::Class(class_name)))
        }
        TyKind::Array(element_ty, length) => {
            let length = length.try_to_target_usize(tcx)?;
            let jvm_length = i32::try_from(length).ok()?;
            let element_jvm_ty = ty_to_oomir_type(*element_ty, tcx, data_types, instance);
            let dest = format!("{temp_prefix}_array_value");
            instructions.push(oomir::Instruction::NewArray {
                dest: dest.clone(),
                element_type: element_jvm_ty.clone(),
                size: oomir::Operand::Constant(oomir::Constant::I32(jvm_length)),
            });
            if length == 0 || !element_jvm_ty.has_jvm_value() {
                return Some(operand_var(
                    dest,
                    oomir::Type::Array(Box::new(element_jvm_ty)),
                ));
            }
            let element_value = materialize_implicit_zst(
                *element_ty,
                &format!("{temp_prefix}_element"),
                tcx,
                instance,
                data_types,
                instructions,
            )?;
            // Keep generated code independent of the array length. Pointer.fillArray
            // performs the repeated value copies at runtime, just like MIR array-repeat
            // lowering, instead of emitting one OOMIR instruction per ZST element.
            instructions.push(oomir::Instruction::ArrayFill {
                array: oomir::Operand::Variable {
                    name: dest.clone(),
                    ty: oomir::Type::Array(Box::new(element_jvm_ty.clone())),
                },
                value: element_value,
                copy_value: true,
            });
            Some(operand_var(
                dest,
                oomir::Type::Array(Box::new(element_jvm_ty)),
            ))
        }
        TyKind::Closure(_, closure_args) => {
            let oomir::Type::Class(class_name) =
                ty_to_oomir_type(rust_ty, tcx, data_types, instance)
            else {
                return None;
            };
            let mut args = Vec::new();
            for (index, capture_ty) in closure_args.as_closure().upvar_tys().iter().enumerate() {
                let capture_jvm_ty = ty_to_oomir_type(capture_ty, tcx, data_types, instance);
                if !capture_jvm_ty.has_jvm_value() {
                    continue;
                }
                let capture_value = materialize_implicit_zst(
                    capture_ty,
                    &format!("{temp_prefix}_capture_{index}"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                )?;
                args.push((capture_value, capture_jvm_ty));
            }
            let dest = format!("{temp_prefix}_closure_value");
            instructions.push(oomir::Instruction::ConstructObject {
                dest: dest.clone(),
                class_name: class_name.clone(),
                args,
            });
            Some(operand_var(dest, oomir::Type::Class(class_name)))
        }
        _ => None,
    }
}

fn cast_direct_operand(
    source: oomir::Operand,
    target: &oomir::Type,
    temp_prefix: &str,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    if source.get_type().as_ref() == Some(target) {
        return source;
    }
    let dest = format!("{temp_prefix}_value");
    instructions.push(oomir::Instruction::Cast {
        op: source,
        ty: target.clone(),
        dest: dest.clone(),
    });
    operand_var(dest, target.clone())
}

fn physical_scalar_rust_ty<'tcx>(
    source_ty: &oomir::Type,
    rust_size: usize,
    tcx: TyCtxt<'tcx>,
) -> Option<Ty<'tcx>> {
    match (source_ty, rust_size) {
        (oomir::Type::Boolean, 1) => Some(tcx.types.bool),
        (oomir::Type::I8, 1) => Some(tcx.types.i8),
        (oomir::Type::U8, 1) => Some(tcx.types.u8),
        (oomir::Type::I16, 2) => Some(tcx.types.i16),
        (oomir::Type::U16, 2) => Some(tcx.types.u16),
        (oomir::Type::I32, 4) => Some(tcx.types.i32),
        (oomir::Type::U32, 4) => Some(tcx.types.u32),
        (oomir::Type::I64, 8) => Some(tcx.types.i64),
        (oomir::Type::U64, 8) => Some(tcx.types.u64),
        (oomir::Type::F16, 2) => Some(tcx.types.f16),
        (oomir::Type::F32, 4) => Some(tcx.types.f32),
        (oomir::Type::F64, 8) => Some(tcx.types.f64),
        (oomir::Type::Class(name), 16) if name == crate::lower2::I128_CLASS => Some(tcx.types.i128),
        (oomir::Type::Class(name), 16) if name == crate::lower2::U128_CLASS => Some(tcx.types.u128),
        (oomir::Type::Class(name), 16) if name == crate::lower2::F128_CLASS => Some(tcx.types.f128),
        (oomir::Type::Pointer(_), size) if size == tcx.data_layout.pointer_size().bytes_usize() => {
            Some(Ty::new_ptr(
                tcx,
                tcx.types.unit,
                rustc_middle::ty::Mutability::Not,
            ))
        }
        _ => None,
    }
}

fn adapt_physical_scalar<'tcx>(
    source: oomir::Operand,
    target_rust_ty: Ty<'tcx>,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let source_jvm_ty = source.get_type()?;
    let target_jvm_ty = ty_to_oomir_type(target_rust_ty, tcx, data_types, instance);
    if !target_jvm_ty.is_jvm_reference_type() {
        return None;
    }
    let target_layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(target_rust_ty))
        .ok()?;
    let source_rust_ty =
        physical_scalar_rust_ty(&source_jvm_ty, target_layout.size.bytes_usize(), tcx)?;
    let helper =
        ensure_exact_transmute_helper(source_rust_ty, target_rust_ty, tcx, data_types, instance)
            .ok()?;
    let dest = format!("{temp_prefix}_layout_value");
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(dest.clone()),
        class_name: helper.class_name,
        method_name: helper.method_name,
        method_ty: helper.signature,
        args: vec![source],
    });
    Some(operand_var(dest, target_jvm_ty))
}

fn materialize_scalar_struct<'tcx>(
    source: oomir::Operand,
    target_rust_ty: Ty<'tcx>,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> Option<oomir::Operand> {
    let TyKind::Adt(adt_def, substs) = target_rust_ty.kind() else {
        return None;
    };
    if !adt_def.is_struct() {
        return None;
    }
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(target_rust_ty))
        .ok()?;
    if !matches!(layout.backend_repr, BackendRepr::Scalar(_)) {
        return None;
    }

    let mut used_source = false;
    let mut args = Vec::new();
    for (field_index, field) in adt_def.variant(0usize.into()).fields.iter().enumerate() {
        let field_rust_ty = resolved_ty(field.ty(tcx, substs).skip_norm_wip(), tcx, instance);
        let field_jvm_ty = ty_to_oomir_type(field_rust_ty, tcx, data_types, instance);
        if !field_jvm_ty.has_jvm_value() {
            continue;
        }
        let field_layout = tcx
            .layout_of(TypingEnv::fully_monomorphized().as_query_input(field_rust_ty))
            .ok()?;
        let field_value = if field_layout.is_zst() {
            materialize_implicit_zst(
                field_rust_ty,
                &format!("{temp_prefix}_field_{field_index}"),
                tcx,
                instance,
                data_types,
                instructions,
            )?
        } else {
            if used_source {
                return None;
            }
            used_source = true;
            adapt_operand_to_rust_type(
                source.clone(),
                field_rust_ty,
                &format!("{temp_prefix}_field_{field_index}"),
                tcx,
                instance,
                data_types,
                instructions,
            )
        };
        let field_value = cast_direct_operand(
            field_value,
            &field_jvm_ty,
            &format!("{temp_prefix}_field_{field_index}"),
            instructions,
        );
        args.push((field_value, field_jvm_ty));
    }
    if !used_source {
        return None;
    }

    let class_name = generate_adt_jvm_class_name(adt_def, substs, tcx, data_types, instance);
    let dest = format!("{temp_prefix}_struct_value");
    instructions.push(oomir::Instruction::ConstructObject {
        dest: dest.clone(),
        class_name: class_name.clone(),
        args,
    });
    Some(operand_var(dest, oomir::Type::Class(class_name)))
}

/// Converts rustc's physical carrier into the canonical JVM representation for
/// a Rust type. This materializes storage-elided ZSTs and decodes scalar ABI
/// carriers through the shared exact-layout codec when a nominal object is needed.
pub(super) fn adapt_operand_to_rust_type<'tcx>(
    source: oomir::Operand,
    target_rust_ty: Ty<'tcx>,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let target_rust_ty = resolved_ty(target_rust_ty, tcx, instance);
    let target_jvm_ty = ty_to_oomir_type(target_rust_ty, tcx, data_types, instance);
    let source = adapt_mutable_reference_carrier(
        source,
        target_rust_ty,
        &target_jvm_ty,
        temp_prefix,
        tcx,
        instance,
        data_types,
        instructions,
    );
    if source.get_type().as_ref() == Some(&target_jvm_ty) {
        return source;
    }
    if let Some(value) = materialize_direct_fieldless_enum_constant(
        &source,
        target_rust_ty,
        temp_prefix,
        tcx,
        instance,
        data_types,
        instructions,
    ) {
        return value;
    }
    if !source
        .get_type()
        .is_some_and(|source_ty| source_ty.is_jvm_reference_type())
        && let Some(value) = materialize_implicit_zst(
            target_rust_ty,
            temp_prefix,
            tcx,
            instance,
            data_types,
            instructions,
        )
    {
        return value;
    }
    if let Some(value) = adapt_physical_scalar(
        source.clone(),
        target_rust_ty,
        temp_prefix,
        tcx,
        instance,
        data_types,
        instructions,
    ) {
        return value;
    }
    if let Some(value) = materialize_scalar_struct(
        source.clone(),
        target_rust_ty,
        temp_prefix,
        tcx,
        instance,
        data_types,
        instructions,
    ) {
        return value;
    }
    match representation(target_rust_ty, tcx, instance, data_types) {
        RustValueRepresentation::NoJvmValue => source,
        RustValueRepresentation::Direct => source,
        RustValueRepresentation::EnumObject { base_class, layout } => {
            let target = oomir::Type::Class(base_class.clone());
            if source
                .get_type()
                .is_some_and(|source_ty| source_ty.is_jvm_reference_type())
            {
                return source;
            }

            match layout {
                EnumRepresentation::Tagged => adapt_simple_enum_operand(
                    source,
                    &target,
                    temp_prefix,
                    data_types,
                    instructions,
                ),
                EnumRepresentation::Uninhabited => source,
                EnumRepresentation::Single { variant } => {
                    let TyKind::Adt(adt_def, substs) = target_rust_ty.kind() else {
                        unreachable!()
                    };
                    let variant_def = adt_def.variant(variant);
                    let mut value_fields = variant_def
                        .fields
                        .iter()
                        .filter_map(|field| {
                            let field_ty =
                                resolved_ty(field.ty(tcx, substs).skip_norm_wip(), tcx, instance);
                            let field_jvm_ty =
                                ty_to_oomir_type(field_ty, tcx, data_types, instance);
                            field_jvm_ty
                                .has_jvm_value()
                                .then_some((field_ty, field_jvm_ty))
                        })
                        .collect::<Vec<_>>();

                    let args = match value_fields.len() {
                        0 => {
                            return construct_fieldless_enum_variant(
                                target_rust_ty,
                                variant,
                                temp_prefix,
                                tcx,
                                instance,
                                data_types,
                                instructions,
                            )
                            .unwrap();
                        }
                        1 => {
                            let (field_rust_ty, field_jvm_ty) = value_fields.pop().unwrap();
                            let field_value = adapt_operand_to_rust_type(
                                source,
                                field_rust_ty,
                                &format!("{temp_prefix}_field"),
                                tcx,
                                instance,
                                data_types,
                                instructions,
                            );
                            let field_value = cast_direct_operand(
                                field_value,
                                &field_jvm_ty,
                                &format!("{temp_prefix}_field"),
                                instructions,
                            );
                            vec![(field_value, field_jvm_ty)]
                        }
                        _ => return source,
                    };

                    let variant_class = format!(
                        "{}${}",
                        base_class,
                        jvm_names::member_name(&variant_def.name.to_string())
                    );
                    let dest = format!("{temp_prefix}_enum_value");
                    instructions.push(oomir::Instruction::ConstructObject {
                        dest: dest.clone(),
                        class_name: variant_class.clone(),
                        args,
                    });
                    operand_var(dest, oomir::Type::Class(variant_class))
                }
            }
        }
    }
}

mod pointers;
use pointers::*;
