//! Emit ABI forwarders directly. There is no computational OOMIR body and no
//! optimizer pass: the recipe already describes straight-line stack code.
use super::*;
use jvm_compiler_core::jvm::locals::LocalKind as Kind;

pub(super) fn emit(
    cp: &mut InternedConstantPool,
    owner: &str,
    name: &str,
    recipe: &oomir::MethodForwarder,
    module: &oomir::Module,
    interface: bool,
) -> jvm::Result<Vec<jvm::Method>> {
    let emitted = if module.component_method(owner, name) {
        recipe.signature.component_signature()
    } else {
        recipe.signature.clone()
    };
    let signature = &emitted;
    let (instructions, max_locals) = body(cp, owner, recipe, module, signature)?;
    let descriptor = signature.to_string();
    let code = code_attribute_for_descriptor(
        cp,
        max_locals,
        instructions,
        &descriptor,
        signature.is_static,
        Some(owner),
        name,
    )?;
    let static_body = (interface && !signature.is_static).then(|| code.clone());
    let mut methods = vec![jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC
            | if signature.is_static {
                MethodAccessFlags::STATIC
            } else {
                MethodAccessFlags::empty()
            },
        name_index: cp.add_utf8(name)?,
        descriptor_index: cp.add_utf8(&descriptor)?,
        attributes: vec![code],
    }];
    if let Some(code) = static_body {
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
    source_signature: &Signature,
) -> jvm::Result<(Vec<Instruction>, u16)> {
    let target_signature = &recipe.target_signature;
    if recipe.signature.params.len() != target_signature.params.len() || !target_signature.is_static
    {
        return Err(jvm::Error::VerificationError {
            context: "method forwarder".into(),
            message: "incompatible canonical signature".into(),
        });
    }
    let emitted_target = if module.component_method(&recipe.target_owner, &recipe.target_name) {
        target_signature.component_signature()
    } else {
        target_signature.clone()
    };
    let source_components = source_signature.params.len() != recipe.signature.params.len();
    let target_components = emitted_target.params.len() != target_signature.params.len();
    let parameter_slots = source_signature
        .params
        .iter()
        .map(|(_, ty)| get_type_size(ty))
        .sum::<u16>();
    let mut max_locals = parameter_slots;
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
        let source_parts = source_components && !receiver && source.component_shape().is_some();
        let target_parts = target_components && target.component_shape().is_some();
        if source_parts && target_parts && source.component_shape() == target.component_shape() {
            for ty in source.components().unwrap() {
                code.push(get_load_instruction(&ty, local)?);
                local += get_type_size(&ty);
            }
            continue;
        }
        let actual_source = if receiver {
            &Type::Class(owner.to_owned())
        } else {
            source
        };
        if !actual_source.has_jvm_value() {
            continue;
        }
        if source_parts {
            if matches!(source, Type::TaggedI64) {
                let owner = cp.add_class(oomir::TAGGED_LONG_CLASS)?;
                code.extend([
                    Instruction::New(owner),
                    Instruction::Dup,
                    Kind::Long.load(local),
                    Kind::Long.load(local + 2),
                    Instruction::Invokespecial(cp.add_method_ref(owner, "<init>", "(JJ)V")?),
                ]);
                local += 4;
            } else if matches!(source, Type::Pointer(_)) {
                let size = source.address_plan();
                code.extend([Kind::Reference.load(local), Kind::Long.load(local + 1)]);
                source.materialize_address(cp, &mut code)?;
                local += 3;
            } else {
                let view_class = cp.add_class(if matches!(source, Type::Str) {
                    oomir::UTF8_VIEW_CLASS
                } else {
                    oomir::SLICE_VIEW_CLASS
                })?;
                let init = cp.add_method_ref(view_class, "<init>", "(Ljava/lang/Object;IJ)V")?;
                code.extend([
                    Instruction::New(view_class),
                    Instruction::Dup,
                    Kind::Reference.load(local),
                    Kind::Int.load(local + 1),
                    Kind::Long.load(local + 2),
                    Instruction::Invokespecial(init),
                ]);
                local += 4;
            }
        } else {
            code.push(get_load_instruction(actual_source, local)?);
            local += get_type_size(actual_source);
        }
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
        if target_parts && matches!(target, Type::Pointer(_)) {
            code.push(Instruction::Lconst_0);
        } else if target_parts && matches!(target, Type::TaggedI64) {
            max_locals = parameter_slots + 1;
            code.push(Kind::Reference.store(parameter_slots));
            let owner = cp.add_class(oomir::TAGGED_LONG_CLASS)?;
            for part in ["value", "tag"] {
                code.extend([
                    Kind::Reference.load(parameter_slots),
                    Instruction::Invokestatic(cp.add_method_ref(
                        owner,
                        part,
                        "(Lorg/rustlang/runtime/TaggedLong;)J",
                    )?),
                ]);
            }
        } else if target_parts {
            max_locals = parameter_slots + 1;
            code.push(Kind::Reference.store(parameter_slots));
            for index in 0..3 {
                code.push(Kind::Reference.load(parameter_slots));
                code.push(jvm_compiler_core::jvm::abi::view_part_access(cp, index)?);
            }
        }
    }
    let source_return = source_signature.ret != recipe.signature.ret;
    let target_return = emitted_target.ret != target_signature.ret;
    let metadata = super::forward_returns::prepare(
        cp,
        &mut code,
        source_return,
        target_return,
        parameter_slots,
        &mut max_locals,
    )?;
    let class = cp.add_class(&recipe.target_owner)?;
    let method = if matches!(
        module.data_type(&recipe.target_owner),
        Some(oomir::DataType::Interface { .. })
    ) || module.external_interfaces.contains(&recipe.target_owner)
    {
        cp.add_interface_method_ref(class, &recipe.target_name, &emitted_target.to_string())?
    } else {
        cp.add_method_ref(class, &recipe.target_name, &emitted_target.to_string())?
    };
    code.push(Instruction::Invokestatic(method));
    super::forward_returns::finish(
        cp,
        &mut code,
        &recipe.signature.ret,
        &target_signature.ret,
        source_return,
        target_return,
        metadata,
        &mut max_locals,
    )?;
    Ok((code, max_locals))
}
