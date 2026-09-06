//! Static allocation initialization and runtime storage identity.
use super::*;

pub(in crate::lower2) fn create_static_initializer_method(
    cp: &mut InternedConstantPool,
    this_class_index: u16,
    owner_class: &str,
    statics: &[&oomir::Static],
    methods: &mut Vec<jvm::Method>,
    next_factory: &mut usize,
) -> jvm::Result<jvm::Method> {
    let mut instructions = Vec::new();
    for static_value in statics {
        if static_value.is_thread_local {
            return Err(jvm::Error::VerificationError {
                context: format!(
                    "Static {}::{}",
                    static_value.owner_class, static_value.field_name
                ),
                message: "thread-local statics are not yet representable".to_string(),
            });
        }

        let initializer = create_constant_factory(
            cp,
            owner_class,
            &static_value.initializer,
            methods,
            next_factory,
        )?;
        let initializer_type = oomir::Type::from_constant(&initializer);
        load_constant(&mut instructions, cp, &initializer)?;

        if matches!(static_value.storage_type, oomir::Type::Pointer(_)) {
            if initializer_type.has_jvm_value() {
                instructions.extend(get_cast_instructions(
                    "<clinit>",
                    &initializer_type,
                    &oomir::Type::Class("java/lang/Object".to_string()),
                    cp,
                )?);
            } else {
                instructions.push(Instruction::Aconst_null);
            }
            load_constant(
                &mut instructions,
                cp,
                &oomir::Constant::I32(i32::try_from(static_value.allocation_size).map_err(
                    |_| jvm::Error::VerificationError {
                        context: format!(
                            "Static {}::{}",
                            static_value.owner_class, static_value.field_name
                        ),
                        message:
                            "allocation size exceeds the JVM runtime address space".to_string(),
                    },
                )?),
            )?;
            load_constant(
                &mut instructions,
                cp,
                &match &static_value.allocation_codec_class_name {
                    Some(class_name) => oomir::Constant::String(class_name.clone()),
                    None => oomir::Constant::Null(oomir::Type::java_string()),
                },
            )?;
            load_constant(
                &mut instructions,
                cp,
                &oomir::Constant::I32(
                    i32::try_from(static_value.allocation_alignment).map_err(|_| {
                        jvm::Error::VerificationError {
                            context: format!(
                                "Static {}::{}",
                                static_value.owner_class, static_value.field_name
                            ),
                            message: "allocation alignment exceeds the JVM runtime address space"
                                .to_string(),
                        }
                    })?,
                ),
            )?;
            let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
            let cell = cp.add_method_ref(
                pointer_class,
                "cellAligned",
                &format!(
                    "(Ljava/lang/Object;ILjava/lang/String;I)L{};",
                    oomir::POINTER_CLASS
                ),
            )?;
            instructions.push(Instruction::Invokestatic(cell));
        }

        let field_ref = cp.add_field_ref(
            this_class_index,
            &static_value.field_name,
            &static_value.storage_type.to_jvm_descriptor(),
        )?;
        instructions.push(Instruction::Putstatic(field_ref));
    }
    instructions.push(Instruction::Return);

    let max_stack = instructions.max_stack(cp)?.saturating_mul(2).max(4);
    let code = Attribute::Code {
        name_index: cp.add_utf8("Code")?,
        max_stack,
        max_locals: 0,
        code: instructions,
        exception_table: Vec::new(),
        attributes: Vec::new(),
    };
    Ok(jvm::Method {
        access_flags: MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("<clinit>")?,
        descriptor_index: cp.add_utf8("()V")?,
        attributes: vec![code],
    })
}
