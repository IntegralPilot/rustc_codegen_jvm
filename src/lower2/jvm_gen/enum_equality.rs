//! Keep generated enum equality within JVM method and branch size limits.
use super::*;

pub(super) fn create_enum_equality_methods(
    cp: &mut InternedConstantPool,
    module: &oomir::Module,
    owner_name: &str,
    method_name: &str,
    enum_class: &str,
    variants: &[oomir::EnumVariantShape],
) -> jvm::Result<Vec<jvm::Method>> {
    // Bound field comparisons as well as variant tests. Each comparison emits
    // only a small instruction sequence; this leaves room for short branches.
    let mut chunks = Vec::new();
    let mut start = 0;
    let mut work = 0;
    for (index, variant) in variants.iter().enumerate() {
        let cost = 1 + variant.fields.len();
        if work + cost > 128 && index > start {
            chunks.push(&variants[start..index]);
            start = index;
            work = 0;
        }
        work += cost;
    }
    chunks.push(&variants[start..]);
    let name = |index| {
        if index == 0 {
            method_name.to_owned()
        } else {
            format!("{method_name}$part{index}")
        }
    };
    let descriptor = format!("(L{enum_class};L{enum_class};)Z");
    let mut methods = Vec::with_capacity(chunks.len());
    for (index, chunk) in chunks.iter().enumerate() {
        let next = (index + 1 < chunks.len()).then(|| name(index + 1));
        let name = name(index);
        let instructions =
            equality_code(cp, module, owner_name, enum_class, chunk, next.as_deref())?;
        let code = code_attribute_for_descriptor(
            cp,
            2,
            instructions,
            &descriptor,
            true,
            Some(owner_name),
            &name,
        )?;
        methods.push(jvm::Method {
            access_flags: MethodAccessFlags::PUBLIC
                | MethodAccessFlags::STATIC
                | if index == 0 {
                    MethodAccessFlags::empty()
                } else {
                    MethodAccessFlags::SYNTHETIC
                },
            name_index: cp.add_utf8(&name)?,
            descriptor_index: cp.add_utf8(&descriptor)?,
            attributes: vec![code],
        });
    }
    Ok(methods)
}

fn equality_code(
    cp: &mut InternedConstantPool,
    module: &oomir::Module,
    owner_name: &str,
    enum_class: &str,
    variants: &[oomir::EnumVariantShape],
    next_method: Option<&str>,
) -> jvm::Result<Vec<Instruction>> {
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
    if let Some(next_method) = next_method {
        instructions.push(Instruction::Aload_0);
        instructions.push(Instruction::Aload_1);
        let owner = cp.add_class(owner_name)?;
        let descriptor = format!("(L{enum_class};L{enum_class};)Z");
        let next = if matches!(
            module.data_type(owner_name),
            Some(oomir::DataType::Interface { .. })
        ) {
            cp.add_interface_method_ref(owner, next_method, &descriptor)?
        } else {
            cp.add_method_ref(owner, next_method, &descriptor)?
        };
        instructions.push(Instruction::Invokestatic(next));
        instructions.push(Instruction::Ireturn);
    }
    let false_target = instructions.len() as u16;
    instructions.push(Instruction::Iconst_0);
    instructions.push(Instruction::Ireturn);
    for fixup in false_fixups {
        patch_branch_target(&mut instructions, fixup, false_target);
    }
    Ok(instructions)
}

#[cfg(test)]
mod tests;
