use super::*;

pub(super) fn transfer_instruction(
    instruction_index: usize,
    instruction: &Instruction,
    state: &mut FrameState,
    local_hints: &[FrameValue],
    constant_pool: &ConstantPool,
    context: &str,
    signatures: &mut SignatureCache,
) -> jvm::Result<()> {
    use Instruction as I;

    match instruction {
        I::Nop => {}
        I::Aconst_null => state.push(FrameValue::Null),
        I::Iconst_m1
        | I::Iconst_0
        | I::Iconst_1
        | I::Iconst_2
        | I::Iconst_3
        | I::Iconst_4
        | I::Iconst_5
        | I::Bipush(_)
        | I::Sipush(_) => state.push(FrameValue::Integer),
        I::Lconst_0 | I::Lconst_1 => state.push(FrameValue::Long),
        I::Fconst_0 | I::Fconst_1 | I::Fconst_2 => state.push(FrameValue::Float),
        I::Dconst_0 | I::Dconst_1 => state.push(FrameValue::Double),
        I::Ldc(index) => state.push(frame_value_from_ldc(constant_pool, u16::from(*index))?),
        I::Ldc_w(index) => state.push(frame_value_from_ldc(constant_pool, *index)?),
        I::Ldc2_w(index) => state.push(frame_value_from_ldc2(constant_pool, *index)?),

        I::Iload(index) => state.load_local(
            u16::from(*index),
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Lload(index) => state.load_local(
            u16::from(*index),
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Fload(index) => state.load_local(
            u16::from(*index),
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Dload(index) => state.load_local(
            u16::from(*index),
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Aload(index) => state.load_local(
            u16::from(*index),
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Iload_0 | I::Lload_0 | I::Fload_0 | I::Dload_0 | I::Aload_0 => state.load_local(
            0,
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Iload_1 | I::Lload_1 | I::Fload_1 | I::Dload_1 | I::Aload_1 => state.load_local(
            1,
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Iload_2 | I::Lload_2 | I::Fload_2 | I::Dload_2 | I::Aload_2 => state.load_local(
            2,
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Iload_3 | I::Lload_3 | I::Fload_3 | I::Dload_3 | I::Aload_3 => state.load_local(
            3,
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,
        I::Iload_w(index)
        | I::Lload_w(index)
        | I::Fload_w(index)
        | I::Dload_w(index)
        | I::Aload_w(index) => state.load_local(
            *index,
            local_hints,
            load_hint_for_instruction(instruction),
            context,
            instruction_index,
        )?,

        I::Istore(index) => {
            state.pop(context, instruction_index)?;
            state.store_local(u16::from(*index), FrameValue::Integer);
        }
        I::Lstore(index) => {
            state.pop(context, instruction_index)?;
            state.store_local(u16::from(*index), FrameValue::Long);
        }
        I::Fstore(index) => {
            state.pop(context, instruction_index)?;
            state.store_local(u16::from(*index), FrameValue::Float);
        }
        I::Dstore(index) => {
            state.pop(context, instruction_index)?;
            state.store_local(u16::from(*index), FrameValue::Double);
        }
        I::Astore(index) => {
            let value = state.pop_reference(context, instruction_index)?;
            state.store_local(u16::from(*index), value);
        }
        I::Istore_0 => store_fixed(state, 0, FrameValue::Integer, context, instruction_index)?,
        I::Istore_1 => store_fixed(state, 1, FrameValue::Integer, context, instruction_index)?,
        I::Istore_2 => store_fixed(state, 2, FrameValue::Integer, context, instruction_index)?,
        I::Istore_3 => store_fixed(state, 3, FrameValue::Integer, context, instruction_index)?,
        I::Lstore_0 => store_fixed(state, 0, FrameValue::Long, context, instruction_index)?,
        I::Lstore_1 => store_fixed(state, 1, FrameValue::Long, context, instruction_index)?,
        I::Lstore_2 => store_fixed(state, 2, FrameValue::Long, context, instruction_index)?,
        I::Lstore_3 => store_fixed(state, 3, FrameValue::Long, context, instruction_index)?,
        I::Fstore_0 => store_fixed(state, 0, FrameValue::Float, context, instruction_index)?,
        I::Fstore_1 => store_fixed(state, 1, FrameValue::Float, context, instruction_index)?,
        I::Fstore_2 => store_fixed(state, 2, FrameValue::Float, context, instruction_index)?,
        I::Fstore_3 => store_fixed(state, 3, FrameValue::Float, context, instruction_index)?,
        I::Dstore_0 => store_fixed(state, 0, FrameValue::Double, context, instruction_index)?,
        I::Dstore_1 => store_fixed(state, 1, FrameValue::Double, context, instruction_index)?,
        I::Dstore_2 => store_fixed(state, 2, FrameValue::Double, context, instruction_index)?,
        I::Dstore_3 => store_fixed(state, 3, FrameValue::Double, context, instruction_index)?,
        I::Astore_0 => store_reference_fixed(state, 0, context, instruction_index)?,
        I::Astore_1 => store_reference_fixed(state, 1, context, instruction_index)?,
        I::Astore_2 => store_reference_fixed(state, 2, context, instruction_index)?,
        I::Astore_3 => store_reference_fixed(state, 3, context, instruction_index)?,
        I::Istore_w(local) => store_fixed(
            state,
            *local,
            FrameValue::Integer,
            context,
            instruction_index,
        )?,
        I::Lstore_w(local) => {
            store_fixed(state, *local, FrameValue::Long, context, instruction_index)?
        }
        I::Fstore_w(local) => {
            store_fixed(state, *local, FrameValue::Float, context, instruction_index)?
        }
        I::Dstore_w(local) => store_fixed(
            state,
            *local,
            FrameValue::Double,
            context,
            instruction_index,
        )?,
        I::Astore_w(local) => store_reference_fixed(state, *local, context, instruction_index)?,

        I::Iaload | I::Baload | I::Caload | I::Saload => {
            state.pop(context, instruction_index)?;
            state.pop_reference(context, instruction_index)?;
            state.push(FrameValue::Integer);
        }
        I::Laload => array_load(state, FrameValue::Long, context, instruction_index)?,
        I::Faload => array_load(state, FrameValue::Float, context, instruction_index)?,
        I::Daload => array_load(state, FrameValue::Double, context, instruction_index)?,
        I::Aaload => {
            state.pop(context, instruction_index)?;
            let array = state.pop_reference(context, instruction_index)?;
            state.push(array_component_value(&array));
        }
        I::Iastore
        | I::Lastore
        | I::Fastore
        | I::Dastore
        | I::Aastore
        | I::Bastore
        | I::Castore
        | I::Sastore => {
            state.pop(context, instruction_index)?;
            state.pop(context, instruction_index)?;
            state.pop_reference(context, instruction_index)?;
        }

        I::Pop => {
            state.pop(context, instruction_index)?;
        }
        I::Pop2 => {
            let value = state.pop(context, instruction_index)?;
            if !value.is_category2() {
                state.pop(context, instruction_index)?;
            }
        }
        I::Dup => {
            let value = state.pop_category1(context, instruction_index)?;
            state.push(value.clone());
            state.push(value);
        }
        I::Swap => {
            let first = state.pop_category1(context, instruction_index)?;
            let second = state.pop_category1(context, instruction_index)?;
            state.push(first);
            state.push(second);
        }
        I::Dup_x1 => {
            let value1 = state.pop_category1(context, instruction_index)?;
            let value2 = state.pop_category1(context, instruction_index)?;
            state.push(value1.clone());
            state.push(value2);
            state.push(value1);
        }
        I::Dup2 => {
            let value1 = state.pop(context, instruction_index)?;
            if value1.is_category2() {
                state.push(value1.clone());
                state.push(value1);
            } else {
                let value2 = state.pop_category1(context, instruction_index)?;
                state.push(value2.clone());
                state.push(value1.clone());
                state.push(value2);
                state.push(value1);
            }
        }
        I::Dup_x2 | I::Dup2_x1 | I::Dup2_x2 => {
            return Err(jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Stack-map builder does not yet support {:?} at instruction {instruction_index}",
                    instruction
                ),
            });
        }

        I::Iadd | I::Isub | I::Imul | I::Idiv | I::Irem | I::Iand | I::Ior | I::Ixor => {
            binary(state, FrameValue::Integer, context, instruction_index)?
        }
        I::Ladd | I::Lsub | I::Lmul | I::Ldiv | I::Lrem | I::Land | I::Lor | I::Lxor => {
            binary(state, FrameValue::Long, context, instruction_index)?
        }
        I::Fadd | I::Fsub | I::Fmul | I::Fdiv | I::Frem => {
            binary(state, FrameValue::Float, context, instruction_index)?
        }
        I::Dadd | I::Dsub | I::Dmul | I::Ddiv | I::Drem => {
            binary(state, FrameValue::Double, context, instruction_index)?
        }
        I::Ineg => unary(state, FrameValue::Integer, context, instruction_index)?,
        I::Lneg => unary(state, FrameValue::Long, context, instruction_index)?,
        I::Fneg => unary(state, FrameValue::Float, context, instruction_index)?,
        I::Dneg => unary(state, FrameValue::Double, context, instruction_index)?,
        I::Ishl | I::Ishr | I::Iushr => {
            shift(state, FrameValue::Integer, context, instruction_index)?
        }
        I::Lshl | I::Lshr | I::Lushr => shift(state, FrameValue::Long, context, instruction_index)?,
        I::Iinc(local, _) => state.store_local(u16::from(*local), FrameValue::Integer),
        I::Iinc_w(local, _) => state.store_local(*local, FrameValue::Integer),

        I::I2l => convert(state, FrameValue::Long, context, instruction_index)?,
        I::I2f => convert(state, FrameValue::Float, context, instruction_index)?,
        I::I2d => convert(state, FrameValue::Double, context, instruction_index)?,
        I::L2i | I::F2i | I::D2i | I::I2b | I::I2c | I::I2s => {
            convert(state, FrameValue::Integer, context, instruction_index)?
        }
        I::L2f | I::D2f => convert(state, FrameValue::Float, context, instruction_index)?,
        I::L2d | I::F2d => convert(state, FrameValue::Double, context, instruction_index)?,
        I::F2l | I::D2l => convert(state, FrameValue::Long, context, instruction_index)?,
        I::Lcmp | I::Fcmpl | I::Fcmpg | I::Dcmpl | I::Dcmpg => {
            state.pop(context, instruction_index)?;
            state.pop(context, instruction_index)?;
            state.push(FrameValue::Integer);
        }

        I::Ifeq(_) | I::Ifne(_) | I::Iflt(_) | I::Ifge(_) | I::Ifgt(_) | I::Ifle(_) => {
            state.pop(context, instruction_index)?;
        }
        I::If_icmpeq(_)
        | I::If_icmpne(_)
        | I::If_icmplt(_)
        | I::If_icmpge(_)
        | I::If_icmpgt(_)
        | I::If_icmple(_)
        | I::If_acmpeq(_)
        | I::If_acmpne(_) => {
            state.pop(context, instruction_index)?;
            state.pop(context, instruction_index)?;
        }
        I::Ifnull(_) | I::Ifnonnull(_) => {
            state.pop_reference(context, instruction_index)?;
        }
        I::Goto(_) | I::Goto_w(_) => {}
        I::Tableswitch(_) | I::Lookupswitch(_) => {
            state.pop(context, instruction_index)?;
        }
        I::Jsr(_) | I::Ret(_) | I::Jsr_w(_) | I::Ret_w(_) => {
            return Err(jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Legacy subroutine instruction unsupported at {instruction_index}"
                ),
            });
        }

        I::Ireturn | I::Lreturn | I::Freturn | I::Dreturn | I::Areturn => {
            state.pop(context, instruction_index)?;
            return Ok(());
        }
        I::Return => return Ok(()),

        I::Getstatic(field_ref) => {
            state.push(signatures.field(constant_pool, *field_ref)?);
        }
        I::Putstatic(_) => {
            state.pop(context, instruction_index)?;
        }
        I::Getfield(field_ref) => {
            state.pop_reference(context, instruction_index)?;
            state.push(signatures.field(constant_pool, *field_ref)?);
        }
        I::Putfield(_) => {
            state.pop(context, instruction_index)?;
            state.pop_reference(context, instruction_index)?;
        }
        I::Invokevirtual(method_ref) | I::Invokespecial(method_ref) => {
            let method = signatures.method(constant_pool, *method_ref)?;
            apply_invoke(
                state,
                method,
                false,
                matches!(instruction, I::Invokespecial(_)),
                context,
                instruction_index,
            )?;
        }
        I::Invokestatic(method_ref) => {
            let method = signatures.method(constant_pool, *method_ref)?;
            apply_invoke(state, method, true, false, context, instruction_index)?;
        }
        I::Invokedynamic(invoke_dynamic_ref) => {
            let method = signatures.method(constant_pool, *invoke_dynamic_ref)?;
            apply_invoke(state, method, true, false, context, instruction_index)?;
        }
        I::Invokeinterface(method_ref, _) => {
            let method = signatures.method(constant_pool, *method_ref)?;
            apply_invoke(state, method, false, false, context, instruction_index)?;
        }

        I::New(class_index) => {
            let _ = constant_pool.try_get_class(*class_index)?;
            state.push(FrameValue::Uninitialized(instruction_index as u16));
        }
        I::Newarray(array_type) => {
            state.pop(context, instruction_index)?;
            state.push(FrameValue::Object(
                array_descriptor_from_type(array_type).into(),
            ));
        }
        I::Anewarray(class_index) => {
            state.pop(context, instruction_index)?;
            let class_name = constant_pool.try_get_class(*class_index)?.to_string();
            let array_name = if class_name.starts_with('[') {
                format!("[{class_name}")
            } else {
                format!("[L{};", normalize_class_name(&class_name))
            };
            state.push(FrameValue::Object(array_name.into()));
        }
        I::Arraylength => {
            state.pop_reference(context, instruction_index)?;
            state.push(FrameValue::Integer);
        }
        I::Athrow => {
            state.pop_reference(context, instruction_index)?;
            return Ok(());
        }
        I::Checkcast(class_index) => {
            state.pop_reference(context, instruction_index)?;
            let class_name = constant_pool.try_get_class(*class_index)?.to_string();
            state.push(FrameValue::Object(normalize_class_name(&class_name).into()));
        }
        I::Instanceof(_) => {
            state.pop_reference(context, instruction_index)?;
            state.push(FrameValue::Integer);
        }
        I::Monitorenter | I::Monitorexit => {
            state.pop_reference(context, instruction_index)?;
        }
        I::Multianewarray(class_index, dimensions) => {
            for _ in 0..*dimensions {
                state.pop(context, instruction_index)?;
            }
            let class_name = constant_pool.try_get_class(*class_index)?.to_string();
            state.push(FrameValue::Object(normalize_class_name(&class_name).into()));
        }
        I::Wide | I::Breakpoint | I::Impdep1 | I::Impdep2 => {}
    }

    Ok(())
}

pub(super) fn apply_invoke(
    state: &mut FrameState,
    method: &MethodTransfer,
    is_static: bool,
    is_special: bool,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    for _ in 0..method.params {
        state.pop(context, instruction_index)?;
    }

    let receiver = if is_static {
        None
    } else {
        Some(state.pop_reference(context, instruction_index)?)
    };

    if is_special && let Some(class_name) = &method.constructor {
        if let Some(receiver) = receiver {
            match receiver {
                FrameValue::Uninitialized(_) | FrameValue::UninitializedThis => {
                    state.initialize_object(&receiver, class_name);
                }
                _ => {}
            }
        }
    }

    if let Some(return_type) = &method.result {
        state.push(return_type.clone());
    }
    Ok(())
}

pub(super) fn store_fixed(
    state: &mut FrameState,
    local: u16,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.store_local(local, value);
    Ok(())
}

pub(super) fn store_reference_fixed(
    state: &mut FrameState,
    local: u16,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    let value = state.pop_reference(context, instruction_index)?;
    state.store_local(local, value);
    Ok(())
}

pub(super) fn array_load(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.pop_reference(context, instruction_index)?;
    state.push(value);
    Ok(())
}

pub(super) fn unary(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.push(value);
    Ok(())
}

pub(super) fn binary(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.pop(context, instruction_index)?;
    state.push(value);
    Ok(())
}

pub(super) fn shift(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.pop(context, instruction_index)?;
    state.push(value);
    Ok(())
}

pub(super) fn convert(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.push(value);
    Ok(())
}

pub(super) fn load_hint_for_instruction(instruction: &Instruction) -> FrameValue {
    match instruction {
        Instruction::Iload(_)
        | Instruction::Iload_0
        | Instruction::Iload_1
        | Instruction::Iload_2
        | Instruction::Iload_3
        | Instruction::Iload_w(_) => FrameValue::Integer,
        Instruction::Lload(_)
        | Instruction::Lload_0
        | Instruction::Lload_1
        | Instruction::Lload_2
        | Instruction::Lload_3
        | Instruction::Lload_w(_) => FrameValue::Long,
        Instruction::Fload(_)
        | Instruction::Fload_0
        | Instruction::Fload_1
        | Instruction::Fload_2
        | Instruction::Fload_3
        | Instruction::Fload_w(_) => FrameValue::Float,
        Instruction::Dload(_)
        | Instruction::Dload_0
        | Instruction::Dload_1
        | Instruction::Dload_2
        | Instruction::Dload_3
        | Instruction::Dload_w(_) => FrameValue::Double,
        Instruction::Aload(_)
        | Instruction::Aload_0
        | Instruction::Aload_1
        | Instruction::Aload_2
        | Instruction::Aload_3
        | Instruction::Aload_w(_) => FrameValue::Object("java/lang/Object".into()),
        _ => FrameValue::Top,
    }
}

pub(super) fn frame_value_from_ldc(
    constant_pool: &ConstantPool,
    index: u16,
) -> jvm::Result<FrameValue> {
    let value = match constant_pool.try_get(index)? {
        Constant::Integer(_) => FrameValue::Integer,
        Constant::Float(_) => FrameValue::Float,
        Constant::String(_) => FrameValue::Object("java/lang/String".into()),
        Constant::Class(_) => FrameValue::Object("java/lang/Class".into()),
        Constant::MethodType(_) => FrameValue::Object("java/lang/invoke/MethodType".into()),
        Constant::MethodHandle { .. } => FrameValue::Object("java/lang/invoke/MethodHandle".into()),
        Constant::Dynamic { .. } => FrameValue::Object("java/lang/Object".into()),
        _ => FrameValue::Top,
    };
    Ok(value)
}

pub(super) fn frame_value_from_ldc2(
    constant_pool: &ConstantPool,
    index: u16,
) -> jvm::Result<FrameValue> {
    let value = match constant_pool.try_get(index)? {
        Constant::Long(_) => FrameValue::Long,
        Constant::Double(_) => FrameValue::Double,
        _ => FrameValue::Top,
    };
    Ok(value)
}

pub(super) fn array_component_value(array: &FrameValue) -> FrameValue {
    let FrameValue::Object(class_name) = array else {
        return FrameValue::Object("java/lang/Object".into());
    };
    let Some(component_descriptor) = class_name.strip_prefix('[') else {
        return FrameValue::Object("java/lang/Object".into());
    };
    if component_descriptor.starts_with('[') {
        return FrameValue::Object(component_descriptor.into());
    }
    if component_descriptor.starts_with('L') && component_descriptor.ends_with(';') {
        return FrameValue::Object(component_descriptor[1..component_descriptor.len() - 1].into());
    }
    match component_descriptor.chars().next() {
        Some('J') => FrameValue::Long,
        Some('F') => FrameValue::Float,
        Some('D') => FrameValue::Double,
        Some('Z' | 'B' | 'C' | 'S' | 'I') => FrameValue::Integer,
        _ => FrameValue::Object("java/lang/Object".into()),
    }
}

pub(super) fn array_descriptor_from_type(array_type: &ArrayType) -> String {
    let descriptor = match array_type {
        ArrayType::Boolean => "Z",
        ArrayType::Char => "C",
        ArrayType::Float => "F",
        ArrayType::Double => "D",
        ArrayType::Byte => "B",
        ArrayType::Short => "S",
        ArrayType::Int => "I",
        ArrayType::Long => "J",
    };
    format!("[{descriptor}")
}
