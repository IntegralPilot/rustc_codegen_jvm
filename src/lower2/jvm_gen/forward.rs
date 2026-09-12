//! Emit ABI forwarders directly. There is no computational OOMIR body and no
//! optimizer pass: the recipe already describes straight-line stack code.
use super::*;
use std::borrow::Cow;

pub(super) fn emit(
    cp: &mut InternedConstantPool,
    owner: &str,
    name: &str,
    recipe: &oomir::MethodForwarder,
    module: &oomir::Module,
    relative_methods: &HashSet<oomir::FunctionKey>,
    interface: bool,
) -> jvm::Result<Vec<jvm::Method>> {
    let signature = &recipe.signature;
    let source_relative = signature.is_static && signature.supports_relative_pointer_abi();
    let emitted_signature = if source_relative {
        Cow::Owned(signature.relative_pointer_abi_signature())
    } else {
        Cow::Borrowed(signature)
    };
    let emitted_name = if source_relative {
        Cow::Owned(format!("{name}{}", oomir::RELATIVE_POINTER_METHOD_SUFFIX))
    } else {
        Cow::Borrowed(name)
    };
    let (instructions, max_locals) =
        body(cp, owner, recipe, module, relative_methods, source_relative)?;
    let descriptor = emitted_signature.to_string();
    let code = code_attribute_for_descriptor(
        cp,
        max_locals,
        instructions,
        &descriptor,
        signature.is_static,
        Some(owner),
        name,
    )?;
    let mut parameters = Vec::new();
    for (name, ty) in emitted_signature.explicit_jvm_params() {
        if ty.has_jvm_value() {
            parameters.push(jvm::attributes::MethodParameter {
                name_index: cp.add_utf8(name)?,
                access_flags: MethodAccessFlags::empty(),
            });
        }
    }
    let static_body = (interface && !signature.is_static).then(|| code.clone());
    let mut methods = vec![jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC
            | if signature.is_static {
                MethodAccessFlags::STATIC
            } else {
                MethodAccessFlags::empty()
            },
        name_index: cp.add_utf8(emitted_name.as_ref())?,
        descriptor_index: cp.add_utf8(&descriptor)?,
        attributes: vec![
            code,
            Attribute::MethodParameters {
                name_index: cp.add_utf8("MethodParameters")?,
                parameters,
            },
        ],
    }];
    if source_relative {
        methods.push(create_relative_pointer_bridge(
            cp,
            owner,
            name,
            signature,
            MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            interface,
        )?);
    } else if let Some(code) = static_body {
        // Enum-interface static dispatch must select this canonical body even
        // when the receiver has a same-named method on a nested enum.
        let mut signature = signature.clone();
        signature.is_static = true;
        signature.params[0].1 = Type::Class(owner.to_owned());
        methods.push(jvm::Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index: cp.add_utf8(name)?,
            descriptor_index: cp.add_utf8(signature.to_string())?,
            attributes: vec![code],
        });
    } else if !signature.is_static {
        methods.push(create_static_instance_bridge(
            cp, owner, name, signature, false,
        )?);
    }
    Ok(methods)
}

fn body(
    cp: &mut InternedConstantPool,
    owner: &str,
    recipe: &oomir::MethodForwarder,
    module: &oomir::Module,
    relative_methods: &HashSet<oomir::FunctionKey>,
    source_relative: bool,
) -> jvm::Result<(Vec<Instruction>, u16)> {
    let target_signature = &recipe.target_signature;
    if recipe.signature.params.len() != target_signature.params.len() || !target_signature.is_static
    {
        return Err(jvm::Error::VerificationError {
            context: "method forwarder".into(),
            message: "incompatible canonical signature".into(),
        });
    }
    let target_relative = target_signature.supports_relative_pointer_abi()
        && (!crate::lower1::naming::is_global_link_symbol_class(&recipe.target_owner)
            || relative_methods.contains(&oomir::FunctionKey::new(
                &recipe.target_owner,
                &recipe.target_name,
                target_signature,
            )));
    let mut code = Vec::new();
    let mut local = 0u16;
    for (index, ((_, source), (_, target))) in recipe
        .signature
        .params
        .iter()
        .zip(&target_signature.params)
        .enumerate()
    {
        let receiver = !recipe.signature.is_static && index == 0;
        let actual_source = if receiver {
            &Type::Class(owner.to_owned())
        } else {
            source
        };
        if !actual_source.has_jvm_value() {
            continue;
        }
        code.push(get_load_instruction(actual_source, local)?);
        let pointer_offsets = source_relative && matches!(source, Type::Pointer(_));
        local += get_type_size(actual_source);
        if receiver && let Some(receiver) = &recipe.receiver {
            code.push(get_int_const_instr(cp, receiver.size));
            load_constant(&mut code, cp, &receiver.codec)?;
            code.push(get_int_const_instr(cp, receiver.alignment));
            let class = cp.add_class(oomir::POINTER_CLASS)?;
            let method = cp.add_method_ref(
                class,
                "receiverCellAligned",
                "(Ljava/lang/Object;ILjava/lang/String;I)Lorg/rustlang/runtime/Pointer;",
            )?;
            code.push(Instruction::Invokestatic(method));
        } else if !actual_source.same_jvm_type(target) {
            code.extend(get_cast_instructions(
                &recipe.target_name,
                actual_source,
                target,
                cp,
            )?);
        }
        if target_relative && matches!(target, Type::Pointer(_)) {
            if pointer_offsets {
                code.push(get_load_instruction(&Type::I64, local)?);
                code.push(get_load_instruction(&Type::I64, local + 2)?);
            } else {
                code.extend([Instruction::Lconst_0, Instruction::Lconst_0]);
            }
        } else if pointer_offsets {
            code.push(get_load_instruction(&Type::I64, local)?);
            code.push(get_load_instruction(&Type::I64, local + 2)?);
            let class = cp.add_class(oomir::POINTER_CLASS)?;
            let method = cp.add_method_ref(
                class,
                "materializeRelative",
                "(Lorg/rustlang/runtime/Pointer;JJ)Lorg/rustlang/runtime/Pointer;",
            )?;
            code.push(Instruction::Invokestatic(method));
        }
        if pointer_offsets {
            local += 4;
        }
    }
    let signature = if target_relative {
        Cow::Owned(target_signature.relative_pointer_abi_signature())
    } else {
        Cow::Borrowed(target_signature)
    };
    let name = if target_relative {
        Cow::Owned(format!(
            "{}{}",
            recipe.target_name,
            oomir::RELATIVE_POINTER_METHOD_SUFFIX
        ))
    } else {
        Cow::Borrowed(recipe.target_name.as_str())
    };
    let class = cp.add_class(&recipe.target_owner)?;
    let method = if matches!(
        module.data_type(&recipe.target_owner),
        Some(oomir::DataType::Interface { .. })
    ) || module.external_interfaces.contains(&recipe.target_owner)
    {
        cp.add_interface_method_ref(class, name.as_ref(), &signature.to_string())?
    } else {
        cp.add_method_ref(class, name.as_ref(), &signature.to_string())?
    };
    code.push(Instruction::Invokestatic(method));
    if !target_signature.ret.same_jvm_type(&recipe.signature.ret) {
        code.extend(get_cast_instructions(
            &recipe.target_name,
            &target_signature.ret,
            &recipe.signature.ret,
            cp,
        )?);
    }
    code.push(return_instruction_for_type(&recipe.signature.ret));
    Ok((code, local))
}
