//! Native JVM enums emission.
use super::*;

pub(super) fn patch_branch_target(
    instructions: &mut [Instruction],
    branch_index: usize,
    target: u16,
) {
    match &mut instructions[branch_index] {
        Instruction::Ifeq(offset)
        | Instruction::Ifne(offset)
        | Instruction::If_icmpne(offset)
        | Instruction::If_acmpne(offset) => *offset = target,
        other => panic!("Cannot patch non-branch instruction: {:?}", other),
    }
}

fn has_generated_eq(module: &oomir::Module, class_name: &str) -> bool {
    match module.data_type(class_name) {
        Some(oomir::DataType::Class { methods, .. }) => methods.contains_key("eq"),
        Some(oomir::DataType::Interface { methods, .. }) => methods.contains_key("eq"),
        None => false,
    }
}

fn append_boolean_false_check(instructions: &mut Vec<Instruction>, false_fixups: &mut Vec<usize>) {
    false_fixups.push(instructions.len());
    instructions.push(Instruction::Ifeq(0));
}

pub(super) fn append_field_equality_check(
    module: &oomir::Module,
    cp: &mut InternedConstantPool,
    instructions: &mut Vec<Instruction>,
    false_fixups: &mut Vec<usize>,
    variant_class_idx: u16,
    field_name: &str,
    field_ty: &Type,
) -> jvm::Result<()> {
    let field_ref =
        cp.add_field_ref(variant_class_idx, field_name, &field_ty.to_jvm_descriptor())?;

    instructions.push(Instruction::Aload_0);
    instructions.push(Instruction::Checkcast(variant_class_idx));
    instructions.push(Instruction::Getfield(field_ref));
    instructions.push(Instruction::Aload_1);
    instructions.push(Instruction::Checkcast(variant_class_idx));
    instructions.push(Instruction::Getfield(field_ref));

    match field_ty {
        Type::I64 | Type::U64 => {
            instructions.push(Instruction::Lcmp);
            false_fixups.push(instructions.len());
            instructions.push(Instruction::Ifne(0));
        }
        Type::F32 => {
            instructions.push(Instruction::Fcmpl);
            false_fixups.push(instructions.len());
            instructions.push(Instruction::Ifne(0));
        }
        Type::F64 => {
            instructions.push(Instruction::Dcmpl);
            false_fixups.push(instructions.len());
            instructions.push(Instruction::Ifne(0));
        }
        Type::I8
        | Type::U8
        | Type::I16
        | Type::U16
        | Type::F16
        | Type::I32
        | Type::U32
        | Type::Boolean
        | Type::Char => {
            false_fixups.push(instructions.len());
            instructions.push(Instruction::If_icmpne(0));
        }
        Type::Str => {
            let view_class = cp.add_class(oomir::UTF8_VIEW_CLASS)?;
            let descriptor = format!(
                "(L{};L{};)Z",
                oomir::UTF8_VIEW_CLASS,
                oomir::UTF8_VIEW_CLASS
            );
            let equals_ref = cp.add_method_ref(view_class, "equals", descriptor)?;
            instructions.push(Instruction::Invokestatic(equals_ref));
            append_boolean_false_check(instructions, false_fixups);
        }
        Type::Pointer(_) => {
            let pointer_idx = cp.add_class(oomir::POINTER_CLASS)?;
            let descriptor = format!("(L{};)Z", oomir::POINTER_CLASS,);
            let equals_ref = cp.add_method_ref(pointer_idx, "sameAddress", descriptor)?;
            instructions.push(Instruction::Invokevirtual(equals_ref));
            append_boolean_false_check(instructions, false_fixups);
        }
        Type::Class(class_name) if has_generated_eq(module, class_name) => {
            let class_idx = cp.add_class(class_name)?;
            if matches!(
                module.data_type(class_name),
                Some(oomir::DataType::Interface { is_enum: true, .. })
            ) {
                let eq_desc = format!("(L{class_name};L{class_name};)Z");
                let eq_ref = cp.add_interface_method_ref(class_idx, "eq", &eq_desc)?;
                instructions.push(Instruction::Invokestatic(eq_ref));
            } else if matches!(
                module.data_type(class_name),
                Some(oomir::DataType::Interface { .. })
            ) {
                let eq_desc = format!("(L{class_name};)Z");
                let eq_ref = cp.add_interface_method_ref(class_idx, "eq", &eq_desc)?;
                instructions.push(Instruction::Invokeinterface(eq_ref, 2));
            } else {
                let eq_desc = format!("(L{class_name};)Z");
                let eq_ref = cp.add_method_ref(class_idx, "eq", &eq_desc)?;
                instructions.push(Instruction::Invokevirtual(eq_ref));
            }
            append_boolean_false_check(instructions, false_fixups);
        }
        Type::Interface(interface_name) if has_generated_eq(module, interface_name) => {
            let interface_idx = cp.add_class(interface_name)?;
            if matches!(
                module.data_type(interface_name),
                Some(oomir::DataType::Interface { is_enum: true, .. })
            ) {
                let eq_desc = format!("(L{interface_name};L{interface_name};)Z");
                let eq_ref = cp.add_interface_method_ref(interface_idx, "eq", &eq_desc)?;
                instructions.push(Instruction::Invokestatic(eq_ref));
            } else {
                let eq_desc = format!("(L{interface_name};)Z");
                let eq_ref = cp.add_interface_method_ref(interface_idx, "eq", &eq_desc)?;
                instructions.push(Instruction::Invokeinterface(eq_ref, 2));
            }
            append_boolean_false_check(instructions, false_fixups);
        }
        Type::Array(inner) if matches!(inner.as_ref(), Type::Pointer(_)) => {
            let pointer_idx = cp.add_class(oomir::POINTER_CLASS)?;
            let equals_ref = cp.add_method_ref(
                pointer_idx,
                "arraySameAddresses",
                "(Ljava/lang/Object;Ljava/lang/Object;)Z",
            )?;
            instructions.push(Instruction::Invokestatic(equals_ref));
            append_boolean_false_check(instructions, false_fixups);
        }
        Type::Class(_) | Type::Interface(_) => {
            let object_class_idx = cp.add_class("java/lang/Object")?;
            let equals_ref =
                cp.add_method_ref(object_class_idx, "equals", "(Ljava/lang/Object;)Z")?;
            instructions.push(Instruction::Invokevirtual(equals_ref));
            append_boolean_false_check(instructions, false_fixups);
        }
        _ => {
            false_fixups.push(instructions.len());
            instructions.push(Instruction::If_acmpne(0));
        }
    }

    Ok(())
}

pub(super) fn create_enum_adt_helper_method(
    cp: &mut InternedConstantPool,
    module: &oomir::Module,
    owner_name: &str,
    method_name: &str,
    kind: &AdtHelperKind,
) -> jvm::Result<jvm::Method> {
    let (descriptor, max_locals, instructions) = match kind {
        AdtHelperKind::EnumVariantIndex {
            enum_class,
            variants,
        } => {
            let mut instructions = Vec::new();
            for (variant_idx, variant) in variants.iter().enumerate() {
                instructions.push(Instruction::Aload_0);
                let runtime_type = cp.add_class(&variant.runtime_type)?;
                instructions.push(Instruction::Instanceof(runtime_type));
                let next_variant_fixup = instructions.len();
                instructions.push(Instruction::Ifeq(0));
                instructions.push(get_int_const_instr(cp, variant_idx as i32));
                instructions.push(Instruction::Ireturn);
                let next_variant = instructions.len() as u16;
                patch_branch_target(&mut instructions, next_variant_fixup, next_variant);
            }
            instructions.push(Instruction::Iconst_m1);
            instructions.push(Instruction::Ireturn);
            (format!("(L{enum_class};)I"), 1, instructions)
        }
        AdtHelperKind::EnumDiscriminant {
            enum_class,
            variants,
            values,
        } => {
            let mut instructions = Vec::new();
            for (variant, value) in variants.iter().zip(values) {
                instructions.push(Instruction::Aload_0);
                let runtime_type = cp.add_class(&variant.runtime_type)?;
                instructions.push(Instruction::Instanceof(runtime_type));
                let next_variant_fixup = instructions.len();
                instructions.push(Instruction::Ifeq(0));
                instructions.push(get_long_const_instr(cp, *value));
                instructions.push(Instruction::Lreturn);
                let next_variant = instructions.len() as u16;
                patch_branch_target(&mut instructions, next_variant_fixup, next_variant);
            }
            instructions.push(Instruction::Lconst_0);
            instructions.push(Instruction::Lreturn);
            (format!("(L{enum_class};)J"), 1, instructions)
        }
        AdtHelperKind::EnumIsVariant {
            enum_class,
            runtime_type,
        } => {
            let runtime_type = cp.add_class(runtime_type)?;
            (
                format!("(L{enum_class};)Z"),
                1,
                vec![
                    Instruction::Aload_0,
                    Instruction::Instanceof(runtime_type),
                    Instruction::Ireturn,
                ],
            )
        }
        AdtHelperKind::StaticPartialEqEnum {
            enum_class,
            variants,
        } => {
            let mut instructions = vec![Instruction::Aload_0, Instruction::Aload_1];
            let same_reference_fixup = instructions.len();
            instructions.push(Instruction::If_acmpne(0));
            instructions.push(Instruction::Iconst_1);
            instructions.push(Instruction::Ireturn);
            let first_variant = instructions.len() as u16;
            patch_branch_target(&mut instructions, same_reference_fixup, first_variant);

            let mut false_fixups = Vec::new();
            for variant in variants {
                let runtime_type_idx = cp.add_class(&variant.runtime_type)?;
                instructions.push(Instruction::Aload_0);
                instructions.push(Instruction::Instanceof(runtime_type_idx));
                let next_variant_fixup = instructions.len();
                instructions.push(Instruction::Ifeq(0));

                instructions.push(Instruction::Aload_1);
                instructions.push(Instruction::Instanceof(runtime_type_idx));
                false_fixups.push(instructions.len());
                instructions.push(Instruction::Ifeq(0));

                if variant.transparent {
                    instructions.push(Instruction::Aload_0);
                    instructions.push(Instruction::Checkcast(runtime_type_idx));
                    instructions.push(Instruction::Aload_1);
                    instructions.push(Instruction::Checkcast(runtime_type_idx));
                    let inner_eq_descriptor =
                        format!("(L{};L{};)Z", variant.runtime_type, variant.runtime_type);
                    let inner_eq =
                        cp.add_interface_method_ref(runtime_type_idx, "eq", &inner_eq_descriptor)?;
                    instructions.push(Instruction::Invokestatic(inner_eq));
                    instructions.push(Instruction::Ireturn);
                } else {
                    for (field_name, field_ty) in &variant.fields {
                        if field_ty.has_jvm_value() {
                            append_field_equality_check(
                                module,
                                cp,
                                &mut instructions,
                                &mut false_fixups,
                                runtime_type_idx,
                                field_name,
                                field_ty,
                            )?;
                        }
                    }
                    instructions.push(Instruction::Iconst_1);
                    instructions.push(Instruction::Ireturn);
                }

                let next_variant = instructions.len() as u16;
                patch_branch_target(&mut instructions, next_variant_fixup, next_variant);
            }
            let false_target = instructions.len() as u16;
            instructions.push(Instruction::Iconst_0);
            instructions.push(Instruction::Ireturn);
            for fixup in false_fixups {
                patch_branch_target(&mut instructions, fixup, false_target);
            }
            (format!("(L{enum_class};L{enum_class};)Z"), 2, instructions)
        }
        AdtHelperKind::PartialEqClass { .. } | AdtHelperKind::Component { .. } => {
            return Err(jvm::Error::VerificationError {
                context: format!("Enum helper {owner_name}::{method_name}"),
                message: "class-only helper requested from enum helper generator".to_string(),
            });
        }
    };

    let code = code_attribute_for_descriptor(
        cp,
        max_locals,
        instructions,
        &descriptor,
        true,
        Some(owner_name),
        method_name,
    )?;
    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8(method_name)?,
        descriptor_index: cp.add_utf8(&descriptor)?,
        attributes: vec![code],
    })
}
