//! Native JVM constructors emission.
use super::*;

/// Creates a default constructor `<init>()V` that just calls `super()`.
pub(in crate::lower2) fn create_default_constructor(
    cp: &mut InternedConstantPool,
    super_class_index: u16,
) -> jvm::Result<jvm::Method> {
    let init_name_index = cp.add_utf8("<init>")?;
    let init_desc_index = cp.add_utf8("()V")?;

    // Add reference to super.<init>()V
    let super_init_ref_index = cp.add_method_ref(super_class_index, "<init>", "()V")?;

    let instructions = vec![
        Instruction::Aload_0,
        Instruction::Invokespecial(super_init_ref_index),
        Instruction::Return,
    ];

    let max_locals = 1;

    let code_attribute =
        code_attribute_for_descriptor(cp, max_locals, instructions, "()V", false, None, "<init>")?;

    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC,
        name_index: init_name_index,
        descriptor_index: init_desc_index,
        attributes: vec![code_attribute],
    })
}

pub(super) fn create_field_constructor(
    cp: &mut InternedConstantPool,
    this_class_index: u16,
    super_class_index: u16,
    fields: &[(String, Type)],
) -> jvm::Result<jvm::Method> {
    let init_name_index = cp.add_utf8("<init>")?;
    let descriptor = format!(
        "({})V",
        fields
            .iter()
            .filter(|(_, ty)| ty.has_jvm_value())
            .map(|(_, ty)| ty.to_jvm_descriptor())
            .collect::<String>()
    );
    let init_desc_index = cp.add_utf8(&descriptor)?;
    let super_init_ref_index = cp.add_method_ref(super_class_index, "<init>", "()V")?;

    let mut instructions = vec![
        Instruction::Aload_0,
        Instruction::Invokespecial(super_init_ref_index),
    ];
    let mut next_local = 1;

    for (field_name, field_ty) in fields.iter().filter(|(_, ty)| ty.has_jvm_value()) {
        let field_ref =
            cp.add_field_ref(this_class_index, field_name, &field_ty.to_jvm_descriptor())?;
        instructions.push(Instruction::Aload_0);
        instructions.push(get_load_instruction(field_ty, next_local)?);
        instructions.push(Instruction::Putfield(field_ref));

        let field_size = get_type_size(field_ty);

        next_local += field_size;
    }

    instructions.push(Instruction::Return);

    let mut parameters = Vec::new();
    for (field_name, _) in fields.iter().filter(|(_, ty)| ty.has_jvm_value()) {
        let name_index = cp.add_utf8(field_name)?;
        parameters.push(jvm::attributes::MethodParameter {
            name_index,
            access_flags: MethodAccessFlags::empty(),
        });
    }
    let method_parameters_attribute_name_index = cp.add_utf8("MethodParameters")?;

    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC,
        name_index: init_name_index,
        descriptor_index: init_desc_index,
        attributes: vec![
            code_attribute_for_descriptor(
                cp,
                next_local,
                instructions,
                &descriptor,
                false,
                None,
                "<init>",
            )?,
            Attribute::MethodParameters {
                name_index: method_parameters_attribute_name_index,
                parameters,
            },
        ],
    })
}

pub(super) fn create_relative_pointer_field_constructor(
    cp: &mut InternedConstantPool,
    this_class_index: u16,
    super_class_index: u16,
    fields: &[(String, Type)],
) -> jvm::Result<jvm::Method> {
    let descriptor = format!(
        "({})V",
        fields
            .iter()
            .map(|(_, ty)| {
                if matches!(ty, Type::Pointer(_)) {
                    format!("{}JJ", ty.to_jvm_descriptor())
                } else {
                    ty.to_jvm_descriptor()
                }
            })
            .collect::<String>()
    );
    let super_init = cp.add_method_ref(super_class_index, "<init>", "()V")?;
    let mut instructions = vec![Instruction::Aload_0, Instruction::Invokespecial(super_init)];
    let mut next_local = 1u16;
    let mut parameters = Vec::new();

    for (field_name, field_ty) in fields {
        let field =
            cp.add_field_ref(this_class_index, field_name, &field_ty.to_jvm_descriptor())?;
        instructions.push(Instruction::Aload_0);
        instructions.push(get_load_instruction(field_ty, next_local)?);
        instructions.push(Instruction::Putfield(field));
        parameters.push(jvm::attributes::MethodParameter {
            name_index: cp.add_utf8(field_name)?,
            access_flags: MethodAccessFlags::empty(),
        });
        next_local += get_type_size(field_ty);

        if matches!(field_ty, Type::Pointer(_)) {
            for offset_name in [
                oomir::relative_pointer_element_offset_field(field_name),
                oomir::relative_pointer_byte_offset_field(field_name),
            ] {
                let offset = cp.add_field_ref(this_class_index, &offset_name, "J")?;
                instructions.push(Instruction::Aload_0);
                instructions.push(get_load_instruction(&Type::I64, next_local)?);
                instructions.push(Instruction::Putfield(offset));
                parameters.push(jvm::attributes::MethodParameter {
                    name_index: cp.add_utf8(offset_name)?,
                    access_flags: MethodAccessFlags::SYNTHETIC,
                });
                next_local += 2;
            }
        }
    }
    instructions.push(Instruction::Return);

    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::SYNTHETIC,
        name_index: cp.add_utf8("<init>")?,
        descriptor_index: cp.add_utf8(&descriptor)?,
        attributes: vec![
            code_attribute_for_descriptor(
                cp,
                next_local,
                instructions,
                &descriptor,
                false,
                None,
                "<init>",
            )?,
            Attribute::MethodParameters {
                name_index: cp.add_utf8("MethodParameters")?,
                parameters,
            },
        ],
    })
}

pub(super) fn create_managed_copy_method(
    cp: &mut InternedConstantPool,
    this_class_index: u16,
    class_name: &str,
    fields: &[(String, Type)],
) -> jvm::Result<jvm::Method> {
    let descriptor = "()Ljava/lang/Object;";
    let constructor_descriptor = format!(
        "({})V",
        fields
            .iter()
            .map(|(_, ty)| ty.to_jvm_descriptor())
            .collect::<String>()
    );
    let constructor = cp.add_method_ref(this_class_index, "<init>", &constructor_descriptor)?;
    let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
    let copy_managed_value = cp.add_method_ref(
        pointer_class,
        "copyManagedValue",
        "(Ljava/lang/Object;)Ljava/lang/Object;",
    )?;
    let object_type = Type::Class("java/lang/Object".to_string());
    let mut instructions = vec![Instruction::New(this_class_index), Instruction::Dup];

    for (field_name, field_ty) in fields {
        let field =
            cp.add_field_ref(this_class_index, field_name, &field_ty.to_jvm_descriptor())?;
        instructions.push(Instruction::Aload_0);
        instructions.push(Instruction::Getfield(field));
        if matches!(field_ty, Type::Pointer(_)) {
            for offset_name in [
                oomir::relative_pointer_element_offset_field(field_name),
                oomir::relative_pointer_byte_offset_field(field_name),
            ] {
                let offset = cp.add_field_ref(this_class_index, offset_name, "J")?;
                instructions.push(Instruction::Aload_0);
                instructions.push(Instruction::Getfield(offset));
            }
            let materialize = cp.add_method_ref(
                pointer_class,
                "materializeRelative",
                &format!("(L{};JJ)L{};", oomir::POINTER_CLASS, oomir::POINTER_CLASS),
            )?;
            instructions.push(Instruction::Invokestatic(materialize));
        } else if field_ty.is_jvm_reference_type() {
            instructions.push(Instruction::Invokestatic(copy_managed_value));
            instructions.extend(get_cast_instructions(
                "rustCopy",
                &object_type,
                field_ty,
                cp,
            )?);
        }
    }
    instructions.push(Instruction::Invokespecial(constructor));
    instructions.push(Instruction::Areturn);

    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::FINAL,
        name_index: cp.add_utf8("rustCopy")?,
        descriptor_index: cp.add_utf8(descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            cp,
            1,
            instructions,
            descriptor,
            false,
            Some(class_name),
            "rustCopy",
        )?],
    })
}
