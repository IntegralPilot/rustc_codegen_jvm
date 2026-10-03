//! Keep the object ABI available for indirect calls and existing entry points.
use super::*;

pub(super) fn bridge(
    cp: &mut InternedConstantPool,
    context: &oomir::construct::Context,
    owner: &str,
    name: &str,
    original: &Signature,
    scalars: &Signature,
) -> jvm::Result<jvm::Method> {
    let source = original.component_signature();
    let target = scalars.component_signature();
    let source_split = &source != original;
    let mut code = Vec::new();
    let mut local: u16 = 0;
    for (_, ty) in &original.params {
        if let Some(fields) = context.scalar_fields(ty) {
            let Type::Class(class) = ty else {
                unreachable!()
            };
            let class = cp.add_class(class)?;
            for (name, ty) in fields {
                code.push(get_load_instruction(
                    &Type::Class("java/lang/Object".into()),
                    local,
                )?);
                code.push(Instruction::Getfield(cp.add_field_ref(
                    class,
                    name,
                    ty.to_jvm_descriptor(),
                )?));
            }
            local += 1;
        } else if let Some(parts) = ty.components().filter(|_| source_split) {
            for part in parts {
                code.push(get_load_instruction(&part, local)?);
                local += get_type_size(&part);
            }
        } else if ty.has_jvm_value() {
            code.push(get_load_instruction(ty, local)?);
            local += get_type_size(ty);
        }
    }
    if source.ret != original.ret {
        code.push(get_load_instruction(
            &Type::Class("java/lang/Object".into()),
            local,
        )?);
        local += 1;
    }
    let class = cp.add_class(owner)?;
    let method = cp.add_method_ref(
        class,
        format!("{name}{}", oomir::construct::RECORD_ENTRY),
        target.to_string(),
    )?;
    code.push(Instruction::Invokestatic(method));
    code.push(return_instruction_for_type(&source.ret));
    let descriptor = source.to_string();
    let attributes = vec![code_attribute_for_descriptor(
        cp,
        local,
        code,
        &descriptor,
        true,
        Some(owner),
        name,
    )?];
    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8(name)?,
        descriptor_index: cp.add_utf8(descriptor)?,
        attributes,
    })
}
