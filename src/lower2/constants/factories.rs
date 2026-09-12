//! Bounded factory methods for constants too large to inline.
use super::*;
use std::borrow::Cow;

fn add_constant_helper_method(
    cp: &mut InternedConstantPool,
    methods: &mut Vec<jvm::Method>,
    method_name: &str,
    descriptor: &str,
    max_locals: u16,
    instructions: Vec<Instruction>,
) -> jvm::Result<()> {
    let max_stack = instructions.max_stack(cp)?.saturating_mul(2).max(4);
    let code = Attribute::Code {
        name_index: cp.add_utf8("Code")?,
        max_stack,
        max_locals,
        code: instructions,
        exception_table: Vec::new(),
        attributes: Vec::new(),
    };
    methods.push(jvm::Method {
        access_flags: MethodAccessFlags::PRIVATE
            | MethodAccessFlags::STATIC
            | MethodAccessFlags::SYNTHETIC,
        name_index: cp.add_utf8(method_name)?,
        descriptor_index: cp.add_utf8(descriptor)?,
        attributes: vec![code],
    });
    Ok(())
}

fn create_chunked_array_factory(
    cp: &mut InternedConstantPool,
    owner_class: &str,
    element_type: &oomir::Type,
    elements: &[oomir::Constant],
    methods: &mut Vec<jvm::Method>,
    next_factory: &mut usize,
) -> jvm::Result<oomir::Constant> {
    let mut prepared: Cow<'_, [oomir::Constant]> = Cow::Borrowed(elements);
    for (index, element) in elements.iter().enumerate() {
        if constant_instruction_cost(element) > MAX_INLINE_CONSTANT_INSTRUCTIONS {
            let replacement =
                create_constant_factory(cp, owner_class, element, methods, next_factory)?;
            prepared.to_mut()[index] = replacement;
        }
    }

    let array_type = oomir::Type::Array(Box::new(element_type.clone()));
    let array_descriptor = array_type.to_jvm_descriptor();
    let fill_descriptor = format!("({array_descriptor})V");
    let store_instruction = element_type.get_jvm_array_store_instruction();
    let mut fill_methods = Vec::new();

    if let Some(store_instruction) = store_instruction {
        let mut start = 0;
        while start < prepared.len() {
            let mut end = start;
            let mut chunk_cost = 0usize;
            while end < prepared.len() {
                let element_cost = 3usize.saturating_add(constant_instruction_cost(&prepared[end]));
                if end > start
                    && chunk_cost.saturating_add(element_cost) > MAX_INLINE_CONSTANT_INSTRUCTIONS
                {
                    break;
                }
                chunk_cost = chunk_cost.saturating_add(element_cost);
                end += 1;
            }

            let method_name = format!("_constant_fill_{}", *next_factory);
            *next_factory += 1;
            let mut instructions = Vec::new();
            for (index, element) in prepared[start..end].iter().enumerate() {
                let absolute_index = start + index;
                let constant_type = oomir::Type::from_constant(element);
                if constant_type != *element_type
                    && !crate::lower2::helpers::are_types_jvm_compatible(
                        &constant_type,
                        element_type,
                    )
                {
                    return Err(jvm::Error::VerificationError {
                        context: format!("constant array element {absolute_index}"),
                        message: format!("Expected {element_type:?}, found {constant_type:?}"),
                    });
                }
                instructions.push(Instruction::Aload_0);
                append_unpooled_int_const(
                    &mut instructions,
                    i32::try_from(absolute_index).map_err(|_| jvm::Error::VerificationError {
                        context: "constant array fill".to_string(),
                        message: "Constant array index exceeds the JVM address space".to_string(),
                    })?,
                );
                load_constant(&mut instructions, cp, element)?;
                instructions.push(store_instruction.clone());
            }
            instructions.push(Instruction::Return);
            add_constant_helper_method(
                cp,
                methods,
                &method_name,
                &fill_descriptor,
                1,
                instructions,
            )?;
            fill_methods.push(method_name);
            start = end;
        }
    }

    let method_name = format!("_constant_factory_{}", *next_factory);
    *next_factory += 1;
    let descriptor = format!("(){array_descriptor}");
    let mut instructions = Vec::new();
    append_empty_array(&mut instructions, cp, element_type, prepared.len())?;
    instructions.push(Instruction::Astore_0);
    let owner = cp.add_class(owner_class)?;
    for fill_method in fill_methods {
        instructions.push(Instruction::Aload_0);
        let method = cp.add_method_ref(owner, &fill_method, &fill_descriptor)?;
        instructions.push(Instruction::Invokestatic(method));
    }
    instructions.push(Instruction::Aload_0);
    instructions.push(Instruction::Areturn);
    add_constant_helper_method(cp, methods, &method_name, &descriptor, 1, instructions)?;

    Ok(oomir::Constant::FactoryCall {
        owner_class: owner_class.to_string(),
        method_name,
        ty: array_type,
    })
}

pub(super) fn create_shared_array_factory(
    cp: &mut InternedConstantPool,
    owner_class: &str,
    element_type: &oomir::Type,
    elements: &[oomir::Constant],
    methods: &mut Vec<jvm::Method>,
    next_factory: &mut usize,
) -> jvm::Result<oomir::Constant> {
    let builder = create_chunked_array_factory(
        cp,
        owner_class,
        element_type,
        elements,
        methods,
        next_factory,
    )?;
    let oomir::Constant::FactoryCall {
        method_name: builder_method,
        ty,
        ..
    } = builder
    else {
        unreachable!("chunked array construction always returns a factory call");
    };

    let method_name = format!("_constant_factory_{}", *next_factory);
    *next_factory += 1;
    let descriptor = format!("(){}", ty.to_jvm_descriptor());
    let identity = format!("{owner_class}#{builder_method}");
    let mut instructions = Vec::new();
    for value in [&identity, owner_class, &builder_method] {
        load_constant(
            &mut instructions,
            cp,
            &oomir::Constant::String(value.to_string()),
        )?;
    }
    let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
    let shared_constant = cp.add_method_ref(
        pointer_class,
        "sharedConstant",
        "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/Object;",
    )?;
    instructions.push(Instruction::Invokestatic(shared_constant));
    instructions.extend(get_cast_instructions(
        &method_name,
        &oomir::Type::Class("java/lang/Object".to_string()),
        &ty,
        cp,
    )?);
    instructions.push(Instruction::Areturn);
    add_constant_helper_method(cp, methods, &method_name, &descriptor, 0, instructions)?;

    Ok(oomir::Constant::FactoryCall {
        owner_class: owner_class.to_string(),
        method_name,
        ty,
    })
}

pub(super) fn create_constant_factory(
    cp: &mut InternedConstantPool,
    owner_class: &str,
    constant: &oomir::Constant,
    methods: &mut Vec<jvm::Method>,
    next_factory: &mut usize,
) -> jvm::Result<oomir::Constant> {
    if let oomir::Constant::Array(element_type, elements) = constant
        && constant_instruction_cost(constant) > MAX_INLINE_CONSTANT_INSTRUCTIONS
    {
        return create_chunked_array_factory(
            cp,
            owner_class,
            element_type,
            elements,
            methods,
            next_factory,
        );
    }
    if let oomir::Constant::Slice(element_type, elements) = constant
        && constant_instruction_cost(constant) > MAX_INLINE_CONSTANT_INSTRUCTIONS
    {
        let backing = create_chunked_array_factory(
            cp,
            owner_class,
            element_type,
            elements,
            methods,
            next_factory,
        )?;
        return create_constant_factory(
            cp,
            owner_class,
            &oomir::Constant::SliceRef {
                backing: Box::new(backing),
                element_type: element_type.clone(),
                offset: 0,
                length: elements.len() as u64,
            },
            methods,
            next_factory,
        );
    }

    let prepared = match constant {
        oomir::Constant::Array(element_type, elements) => oomir::Constant::Array(
            element_type.clone(),
            elements
                .iter()
                .map(|element| {
                    if constant_instruction_cost(element) > MAX_INLINE_CONSTANT_INSTRUCTIONS {
                        create_constant_factory(cp, owner_class, element, methods, next_factory)
                    } else {
                        Ok(element.clone())
                    }
                })
                .collect::<jvm::Result<Vec<_>>>()?,
        ),
        oomir::Constant::Slice(element_type, elements) => oomir::Constant::Slice(
            element_type.clone(),
            elements
                .iter()
                .map(|element| {
                    if constant_instruction_cost(element) > MAX_INLINE_CONSTANT_INSTRUCTIONS {
                        create_constant_factory(cp, owner_class, element, methods, next_factory)
                    } else {
                        Ok(element.clone())
                    }
                })
                .collect::<jvm::Result<Vec<_>>>()?,
        ),
        oomir::Constant::SliceRef {
            backing,
            element_type,
            offset,
            length,
        } => oomir::Constant::SliceRef {
            backing: Box::new(create_constant_factory(
                cp,
                owner_class,
                backing,
                methods,
                next_factory,
            )?),
            element_type: element_type.clone(),
            offset: *offset,
            length: *length,
        },
        oomir::Constant::InternedPointer {
            identity,
            value,
            array_backed,
            allocation_size,
            offset,
            view_size,
            alignment,
            view_codec,
            pointee,
        } => oomir::Constant::InternedPointer {
            identity: identity.clone(),
            value: Box::new(create_constant_factory(
                cp,
                owner_class,
                value,
                methods,
                next_factory,
            )?),
            array_backed: *array_backed,
            allocation_size: *allocation_size,
            offset: *offset,
            view_size: *view_size,
            alignment: *alignment,
            view_codec: view_codec.clone(),
            pointee: pointee.clone(),
        },
        oomir::Constant::Instance {
            class_name,
            fields,
            params,
            param_types,
        } => oomir::Constant::Instance {
            class_name: class_name.clone(),
            fields: fields.clone(),
            param_types: param_types.clone(),
            params: params
                .iter()
                .map(|param| {
                    if constant_instruction_cost(param) > MAX_INLINE_CONSTANT_INSTRUCTIONS {
                        create_constant_factory(cp, owner_class, param, methods, next_factory)
                    } else {
                        Ok(param.clone())
                    }
                })
                .collect::<jvm::Result<Vec<_>>>()?,
        },
        oomir::Constant::StaticCall {
            owner_class: call_owner,
            method_name,
            args,
            param_types,
            ty,
        } => oomir::Constant::StaticCall {
            owner_class: call_owner.clone(),
            method_name: method_name.clone(),
            args: args
                .iter()
                .map(|arg| create_constant_factory(cp, owner_class, arg, methods, next_factory))
                .collect::<jvm::Result<Vec<_>>>()?,
            param_types: param_types.clone(),
            ty: ty.clone(),
        },
        _ => return Ok(constant.clone()),
    };

    let return_type = oomir::Type::from_constant(&prepared);
    let method_name = format!("_constant_factory_{}", *next_factory);
    *next_factory += 1;
    let descriptor = format!("(){}", return_type.to_jvm_descriptor());
    let mut instructions = Vec::new();
    load_constant(&mut instructions, cp, &prepared)?;
    instructions.push(return_instruction_for_type(&return_type));
    add_constant_helper_method(cp, methods, &method_name, &descriptor, 0, instructions)?;

    Ok(oomir::Constant::FactoryCall {
        owner_class: owner_class.to_string(),
        method_name,
        ty: return_type,
    })
}
