//! Logical field access at reflection and Java helper boundaries.
use super::*;

pub(super) fn load(
    cp: &mut InternedConstantPool,
    code: &mut Vec<Instruction>,
    owner: u16,
    name: &str,
    ty: &Type,
    split: bool,
) -> jvm::Result<()> {
    if split && matches!(ty, Type::TaggedI64) {
        let names = jvm_compiler_core::jvm::abi::tagged_field_names(name);
        let value = cp.add_field_ref(owner, &names[0], "J")?;
        let tag = cp.add_field_ref(owner, &names[1], "J")?;
        let carrier = cp.add_class(oomir::TAGGED_LONG_CLASS)?;
        code.extend([
            Instruction::Dup,
            Instruction::Getfield(value),
            Instruction::Dup2_x1,
            Instruction::Pop2,
            Instruction::Getfield(tag),
            Instruction::Invokestatic(cp.add_method_ref(
                carrier,
                "of",
                "(JJ)Lorg/rustlang/runtime/TaggedLong;",
            )?),
        ]);
    } else if split && matches!(ty, Type::Pointer(_)) {
        let root = cp.add_field_ref(owner, name, "Ljava/lang/Object;")?;
        let offset = cp.add_field_ref(owner, &oomir::fields::displacement_name(name, ty), "J")?;
        code.extend([
            Instruction::Dup,
            Instruction::Getfield(root),
            Instruction::Swap,
            Instruction::Getfield(offset),
        ]);
        ty.materialize_address(cp, code)?;
    } else if split && matches!(ty, Type::Slice(_) | Type::Str) {
        let names = jvm_compiler_core::jvm::abi::view_field_names(name, matches!(ty, Type::Str));
        let root = cp.add_field_ref(owner, &names[0], "Ljava/lang/Object;")?;
        let start = cp.add_field_ref(owner, &names[1], "I")?;
        let length = cp.add_field_ref(owner, &names[2], "J")?;
        let class = cp.add_class(if matches!(ty, Type::Str) {
            oomir::UTF8_VIEW_CLASS
        } else {
            oomir::SLICE_VIEW_CLASS
        })?;
        let init = cp.add_method_ref(class, "<init>", "(Ljava/lang/Object;IJ)V")?;
        code.extend([
            Instruction::New(class),
            Instruction::Dup_x1,
            Instruction::Swap,
            Instruction::Dup,
            Instruction::Getfield(root),
            Instruction::Swap,
            Instruction::Dup,
            Instruction::Getfield(start),
            Instruction::Swap,
            Instruction::Getfield(length),
            Instruction::Invokespecial(init),
        ]);
    } else {
        code.push(Instruction::Getfield(cp.add_field_ref(
            owner,
            name,
            &ty.to_jvm_descriptor(),
        )?));
    }
    Ok(())
}

pub(super) fn constructor_bridge(
    cp: &mut InternedConstantPool,
    owner: u16,
    fields: &[(String, Type)],
) -> jvm::Result<jvm::Method> {
    let physical = oomir::fields::physical(fields);
    let descriptor = format!(
        "({})V",
        fields
            .iter()
            .map(|(_, ty)| ty.to_jvm_descriptor())
            .collect::<String>()
    );
    let target = format!(
        "({})V",
        physical
            .iter()
            .map(|(_, ty)| ty.to_jvm_descriptor())
            .collect::<String>()
    );
    let target = cp.add_method_ref(owner, "<init>", &target)?;
    let mut code = vec![Instruction::Aload_0];
    let mut slot = 1;
    let mut parameters = Vec::new();
    for (name, ty) in fields {
        if matches!(ty, Type::TaggedI64) {
            let carrier = cp.add_class(oomir::TAGGED_LONG_CLASS)?;
            for part in ["value", "tag"] {
                code.extend([
                    get_load_instruction(ty, slot)?,
                    Instruction::Invokestatic(cp.add_method_ref(
                        carrier,
                        part,
                        "(Lorg/rustlang/runtime/TaggedLong;)J",
                    )?),
                ]);
            }
        } else if matches!(ty, Type::Slice(_) | Type::Str) {
            let runtime = cp.add_class("org/rustlang/runtime/RustField")?;
            for (method, result) in [
                ("viewRoot", "Ljava/lang/Object;"),
                ("viewStart", "I"),
                ("viewLength", "J"),
            ] {
                let method =
                    cp.add_method_ref(runtime, method, format!("(Ljava/lang/Object;){result}"))?;
                code.extend([
                    get_load_instruction(ty, slot)?,
                    Instruction::Invokestatic(method),
                ]);
            }
        } else {
            code.push(get_load_instruction(ty, slot)?);
            if matches!(ty, Type::Pointer(_)) {
                code.push(Instruction::Lconst_0);
            }
        }
        slot += get_type_size(ty);
        parameters.push(jvm::attributes::MethodParameter {
            name_index: cp.add_utf8(name)?,
            access_flags: MethodAccessFlags::empty(),
        });
    }
    code.extend([Instruction::Invokespecial(target), Instruction::Return]);
    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC,
        name_index: cp.add_utf8("<init>")?,
        descriptor_index: cp.add_utf8(&descriptor)?,
        attributes: vec![
            code_attribute_for_descriptor(cp, slot, code, &descriptor, false, None, "<init>")?,
            Attribute::MethodParameters {
                name_index: cp.add_utf8("MethodParameters")?,
                parameters,
            },
        ],
    })
}

/// Each field has an exact-layout codec constant. Reflection uses it to reconstruct general address
/// carriers.
pub(super) fn layout_metadata(
    cp: &mut InternedConstantPool,
    fields: &[(String, Type)],
) -> jvm::Result<Vec<jvm::Field>> {
    let mut metadata = Vec::new();
    for (name, ty) in fields {
        let Type::Pointer(pointee) = ty else { continue };
        let Some(layout) = &pointee.layout else {
            continue;
        };
        let Some(codec) = &layout.codec else { continue };
        let name = jvm_compiler_core::jvm::abi::address_codec_field_name(name);
        metadata.push(jvm::Field {
            access_flags: FieldAccessFlags::PUBLIC
                | FieldAccessFlags::STATIC
                | FieldAccessFlags::FINAL
                | FieldAccessFlags::SYNTHETIC,
            name_index: cp.add_utf8(name)?,
            descriptor_index: cp.add_utf8("Ljava/lang/String;")?,
            field_type: oomir_type_to_ristretto_field_type(&Type::java_string()),
            attributes: vec![Attribute::ConstantValue {
                name_index: cp.add_utf8("ConstantValue")?,
                constant_value_index: cp.add_name_string(codec)?,
            }],
        });
    }
    Ok(metadata)
}
