use super::jvm::{
    self,
    attributes::{ArrayType, Instruction},
};
use super::{
    I128_CLASS, U128_CLASS,
    constant_pool::InternedConstantPool,
    helpers::{are_types_jvm_compatible, get_cast_instructions},
};
use crate::oomir::{self, Type};

// Helper to get the appropriate integer constant loading instruction
pub fn get_int_const_instr(cp: &mut InternedConstantPool, val: i32) -> Instruction {
    match val {
        // Direct iconst mapping
        -1 => Instruction::Iconst_m1,
        0 => Instruction::Iconst_0,
        1 => Instruction::Iconst_1,
        2 => Instruction::Iconst_2,
        3 => Instruction::Iconst_3,
        4 => Instruction::Iconst_4,
        5 => Instruction::Iconst_5,

        // Bipush range (-128 to 127), excluding the iconst values already handled
        v @ -128..=-2 | v @ 6..=127 => Instruction::Bipush(v as i8),

        // Sipush range (-32768 to 32767), excluding the bipush range
        v @ -32768..=-129 | v @ 128..=32767 => Instruction::Sipush(v as i16),

        // Use LDC for values outside the -32768 to 32767 range
        v => {
            let index = cp
                .add_integer(v)
                .expect("Failed to add integer to constant pool");
            if let Ok(idx8) = u8::try_from(index) {
                Instruction::Ldc(idx8)
            } else {
                Instruction::Ldc_w(index)
            }
        }
    }
}

// Helper to get the appropriate long constant loading instruction
pub fn get_long_const_instr(cp: &mut InternedConstantPool, val: i64) -> Instruction {
    match val {
        0 => Instruction::Lconst_0,
        1 => Instruction::Lconst_1,
        _ => {
            // Add the long value to the constant pool.
            let index = cp
                .add_long(val)
                .expect("Failed to add long to constant pool");
            // Ldc2_w is used for long/double constants and always takes a u16 index.
            Instruction::Ldc2_w(index)
        }
    }
}

// Helper to get the appropriate float constant loading instruction
pub fn get_float_const_instr(cp: &mut InternedConstantPool, val: f32) -> Instruction {
    if val == 0.0 {
        Instruction::Fconst_0
    } else if val == 1.0 {
        Instruction::Fconst_1
    } else if val == 2.0 {
        Instruction::Fconst_2
    } else {
        // Add the float value to the constant pool.
        let index = cp
            .add_float(val)
            .expect("Failed to add float to constant pool");
        // Ldc2_w is used for long/double constants and always takes a u16 index.
        Instruction::Ldc_w(index)
    }
}

// Helper to get the appropriate double constant loading instruction
pub fn get_double_const_instr(cp: &mut InternedConstantPool, val: f64) -> Instruction {
    // Using bit representation for exact zero comparison is more robust
    if val.to_bits() == 0.0f64.to_bits() {
        // Handles +0.0 and -0.0
        Instruction::Dconst_0
    } else if val == 1.0 {
        Instruction::Dconst_1
    } else {
        // Add the double value to the constant pool.
        let index = cp
            .add_double(val)
            .expect("Failed to add double to constant pool");
        // Ldc2_w is used for long/double constants and always takes a u16 index.
        Instruction::Ldc2_w(index)
    }
}

/// Appends JVM instructions for loading a constant onto the stack.
pub fn load_constant(
    instructions: &mut Vec<Instruction>,
    cp: &mut InternedConstantPool,
    constant: &oomir::Constant,
) -> Result<(), jvm::Error> {
    use jvm::attributes::Instruction as JI;
    use oomir::Constant as OC;

    let mut instructions_to_add = Vec::new();

    match constant {
        OC::Unit => {}
        OC::StaticRef {
            owner_class,
            field_name,
            ty,
        } => {
            let owner = cp.add_class(owner_class)?;
            let field = cp.add_field_ref(owner, field_name, &ty.to_jvm_descriptor())?;
            instructions_to_add.push(JI::Getstatic(field));
        }
        OC::FunctionPointer { adapter_class, .. } => {
            let class_index = cp.add_class(adapter_class)?;
            let constructor = cp.add_method_ref(class_index, "<init>", "()V")?;
            instructions_to_add.push(JI::New(class_index));
            instructions_to_add.push(JI::Dup);
            instructions_to_add.push(JI::Invokespecial(constructor));
        }
        OC::FactoryCall {
            owner_class,
            method_name,
            ty,
        } => {
            let owner = cp.add_class(owner_class)?;
            let method =
                cp.add_method_ref(owner, method_name, &format!("(){}", ty.to_jvm_descriptor()))?;
            instructions_to_add.push(JI::Invokestatic(method));
        }
        OC::PointerAddress {
            address, view_size, ..
        } => {
            let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
            let from_address = cp.add_method_ref(
                pointer_class,
                "fromAddress",
                &format!("(JI)L{};", oomir::POINTER_CLASS),
            )?;
            instructions_to_add.push(get_long_const_instr(cp, *address as i64));
            instructions_to_add.push(get_int_const_instr(cp, *view_size));
            instructions_to_add.push(JI::Invokestatic(from_address));
        }
        OC::I8(v) => instructions_to_add.push(get_int_const_instr(cp, *v as i32)),
        OC::U8(v) => instructions_to_add.push(get_int_const_instr(cp, i32::from(*v as i8))),
        OC::I16(v) => instructions_to_add.push(get_int_const_instr(cp, *v as i32)),
        OC::U16(v) => instructions_to_add.push(get_int_const_instr(cp, i32::from(*v))),
        OC::I32(v) => instructions_to_add.push(get_int_const_instr(cp, *v)),
        OC::U32(v) => instructions_to_add.push(get_int_const_instr(cp, *v as i32)),
        OC::I64(v) => instructions_to_add.push(get_long_const_instr(cp, *v)),
        OC::U64(v) => instructions_to_add.push(get_long_const_instr(cp, *v as i64)),
        OC::F16(bits) => instructions_to_add.push(get_int_const_instr(cp, i32::from(*bits as i16))),
        OC::F32(v) => instructions_to_add.push(get_float_const_instr(cp, *v)),
        OC::F64(v) => instructions_to_add.push(get_double_const_instr(cp, *v)),
        OC::Boolean(v) => instructions_to_add.push(if *v { JI::Iconst_1 } else { JI::Iconst_0 }),
        OC::Char(v) => instructions_to_add.push(get_int_const_instr(cp, *v as i32)),
        OC::Str(s) => {
            let index = cp.add_string(s)?;
            instructions_to_add.push(if let Ok(idx8) = u8::try_from(index) {
                JI::Ldc(idx8)
            } else {
                JI::Ldc_w(index)
            });
            let view_class = cp.add_class(oomir::UTF8_VIEW_CLASS)?;
            let descriptor = format!("(Ljava/lang/String;)L{};", oomir::UTF8_VIEW_CLASS);
            let from_java = cp.add_method_ref(view_class, "fromJavaString", descriptor)?;
            instructions_to_add.push(JI::Invokestatic(from_java));
        }
        OC::String(s) => {
            let index = cp.add_string(s)?;
            instructions_to_add.push(if let Ok(idx8) = u8::try_from(index) {
                JI::Ldc(idx8)
            } else {
                JI::Ldc_w(index)
            });
        }
        OC::Null(_) => {
            instructions_to_add.push(JI::Aconst_null);
        }
        OC::Slice(element_type, elements) => {
            let class_index = cp.add_class(oomir::SLICE_VIEW_CLASS)?;
            let constructor =
                cp.add_method_ref(class_index, "<init>", "(Ljava/lang/Object;II)V")?;
            instructions_to_add.push(JI::New(class_index));
            instructions_to_add.push(JI::Dup);
            load_constant(
                &mut instructions_to_add,
                cp,
                &OC::Array(element_type.clone(), elements.clone()),
            )?;
            instructions_to_add.push(JI::Iconst_0);
            instructions_to_add.push(get_int_const_instr(cp, elements.len() as i32));
            instructions_to_add.push(JI::Invokespecial(constructor));
        }
        OC::SliceRef {
            backing,
            offset,
            length,
            ..
        } => {
            let class_index = cp.add_class(oomir::SLICE_VIEW_CLASS)?;
            let constructor =
                cp.add_method_ref(class_index, "<init>", "(Ljava/lang/Object;II)V")?;
            instructions_to_add.push(JI::New(class_index));
            instructions_to_add.push(JI::Dup);
            load_constant(&mut instructions_to_add, cp, backing)?;
            instructions_to_add.push(get_int_const_instr(
                cp,
                i32::try_from(*offset).map_err(|_| jvm::Error::VerificationError {
                    context: format!("Attempting to load constant {constant:?}"),
                    message: "Constant slice offset exceeds the JVM address space".to_string(),
                })?,
            ));
            instructions_to_add.push(get_int_const_instr(
                cp,
                i32::try_from(*length).map_err(|_| jvm::Error::VerificationError {
                    context: format!("Attempting to load constant {constant:?}"),
                    message: "Constant slice length exceeds the JVM address space".to_string(),
                })?,
            ));
            instructions_to_add.push(JI::Invokespecial(constructor));
        }
        OC::Array(elem_ty, elements) => {
            let array_len = elements.len();

            // 1. Push array size onto stack
            instructions_to_add.push(get_int_const_instr(cp, array_len as i32));

            // 2. Create the new array (primitive or reference)
            if !elem_ty.has_jvm_value() {
                let class_index = cp.add_class("java/lang/Object")?;
                instructions_to_add.push(JI::Anewarray(class_index));
            } else if let Some(atype_code) = elem_ty.to_jvm_primitive_array_type_code() {
                let array_type = ArrayType::from_bytes(&mut jvm::ByteReader::new(&[atype_code]))
                    .map_err(|e| jvm::Error::VerificationError {
                        context: format!("Attempting to load constant {:?}", constant), // Use Display formatting for the error type if available
                        message: format!(
                            "Invalid primitive array type code {}: {:?}",
                            atype_code, e
                        ),
                    })?;
                instructions_to_add.push(JI::Newarray(array_type)); // Stack: [arrayref]
            } else if let Some(internal_name) = elem_ty.to_jvm_internal_name() {
                let class_index = cp.add_class(&internal_name)?;
                instructions_to_add.push(JI::Anewarray(class_index)); // Stack: [arrayref]
            } else {
                return Err(jvm::Error::VerificationError {
                    context: format!("Attempting to load constant {:?}", constant),
                    message: format!("Cannot create JVM array for element type: {:?}", elem_ty),
                });
            }

            if !elem_ty.has_jvm_value() {
                instructions.extend(instructions_to_add);
                return Ok(());
            }

            let store_instruction = elem_ty.get_jvm_array_store_instruction().ok_or_else(|| {
                jvm::Error::VerificationError {
                    context: format!("Attempting to load constant {:?}", constant),
                    message: format!(
                        "Cannot determine array store instruction for type: {:?}",
                        elem_ty
                    ),
                }
            })?;

            // 3. Populate the array
            for (i, element_const) in elements.iter().enumerate() {
                let constant_type = Type::from_constant(element_const);
                if &constant_type != elem_ty.as_ref()
                    && !are_types_jvm_compatible(&constant_type, elem_ty)
                {
                    return Err(jvm::Error::VerificationError {
                        context: format!("Attempting to load constant {:?}", constant),
                        message: format!(
                            "Type mismatch in Constant::Array: expected {:?}, found {:?} for element {}",
                            elem_ty, constant_type, i
                        ),
                    });
                }

                instructions_to_add.push(JI::Dup); // Stack: [arrayref, arrayref]
                instructions_to_add.push(get_int_const_instr(cp, i as i32)); // Stack: [arrayref, arrayref, index]

                // 1. Record the length of the main instruction vector *before* the recursive call.
                let original_jvm_len = instructions.len();

                // 2. Make the recursive call. This *will* append instructions to instructions.
                load_constant(instructions, cp, element_const)?;

                // 3. Determine the range of instructions added by the recursive call.
                let new_jvm_len = instructions.len();

                // 4. If instructions were added, copy them from instructions to instructions_to_add.
                if new_jvm_len > original_jvm_len {
                    // Create a slice referencing the newly added instructions
                    let added_instructions_slice = &instructions[original_jvm_len..new_jvm_len];
                    // Extend the temporary vector with a clone of these instructions
                    instructions_to_add.extend_from_slice(added_instructions_slice);
                }

                // 5. Remove the instructions just added by the recursive call from instructions.
                //    We truncate back to the length it had *before* the recursive call.
                instructions.truncate(original_jvm_len);
                // Now, instructions is back to its state before loading the element,
                // and instructions_to_add contains the necessary Dup, index, element load instructions.

                // Add the array store instruction to the temporary vector
                instructions_to_add.push(store_instruction.clone()); // Stack: [arrayref]
            }
            // Final stack state after loop: [arrayref] (the populated array)
        }
        OC::Instance {
            class_name,
            fields,
            params,
        } => {
            // 1. Add Class reference to constant pool
            let class_index = cp.add_class(class_name)?;

            // i128/u128 constants are represented in OOMIR as decimal strings so
            // that the interpreter can manipulate them without losing width. At
            // bytecode generation time, materialise their two primitive limbs
            // directly. This avoids string allocation every time a wide
            // constant is loaded at runtime.
            if (class_name == I128_CLASS || class_name == U128_CLASS)
                && let [OC::String(value)] = params.as_slice()
            {
                let (high, low) =
                    if class_name == I128_CLASS {
                        let value = value.parse::<i128>().map_err(|error| {
                            jvm::Error::VerificationError {
                                context: format!("Attempting to load constant {constant:?}"),
                                message: format!("Invalid i128 constant '{value}': {error}"),
                            }
                        })?;
                        ((value >> 64) as i64, value as i64)
                    } else {
                        let value = value.parse::<u128>().map_err(|error| {
                            jvm::Error::VerificationError {
                                context: format!("Attempting to load constant {constant:?}"),
                                message: format!("Invalid u128 constant '{value}': {error}"),
                            }
                        })?;
                        ((value >> 64) as i64, value as i64)
                    };
                let constructor = cp.add_method_ref(class_index, "<init>", "(JJ)V")?;
                instructions_to_add.push(JI::New(class_index));
                instructions_to_add.push(JI::Dup);
                instructions_to_add.push(get_long_const_instr(cp, high));
                instructions_to_add.push(get_long_const_instr(cp, low));
                instructions_to_add.push(JI::Invokespecial(constructor));
                instructions.extend(instructions_to_add);
                return Ok(());
            }

            if class_name == oomir::POINTER_CLASS
                && let [value, OC::I32(size), codec] = params.as_slice()
            {
                let constructor = cp.add_method_ref(
                    class_index,
                    "<init>",
                    "(Ljava/lang/Object;ILjava/lang/String;)V",
                )?;
                instructions_to_add.push(JI::New(class_index));
                instructions_to_add.push(JI::Dup);
                instructions.extend(instructions_to_add);
                load_constant(instructions, cp, value)?;
                let value_ty = Type::from_constant(value);
                if !value_ty.has_jvm_value() {
                    instructions.push(JI::Aconst_null);
                } else if value_ty != Type::Class("java/lang/Object".to_string()) {
                    instructions.extend(get_cast_instructions(
                        "constant Pointer cell",
                        &value_ty,
                        &Type::Class("java/lang/Object".to_string()),
                        cp,
                    )?);
                }
                instructions.push(get_int_const_instr(cp, *size));
                load_constant(instructions, cp, codec)?;
                instructions.push(JI::Invokespecial(constructor));
                return Ok(());
            }

            if params.is_empty() && !fields.is_empty() {
                return Err(jvm::Error::VerificationError {
                    context: format!("Attempting to load constant {:?}", constant),
                    message: format!(
                        "Constant::Instance for fielded class '{}' has no constructor parameters",
                        class_name
                    ),
                });
            }
            let constructor_params = params
                .iter()
                .filter_map(|param| {
                    let ty = Type::from_constant(param);
                    if !ty.has_jvm_value() {
                        return None;
                    }
                    let declared_ty = match ty {
                        Type::Class(class_name) if class_name.contains('$') => Type::Class(
                            class_name
                                .split_once('$')
                                .map(|(base, _)| base)
                                .unwrap_or(&class_name)
                                .to_string(),
                        ),
                        other => other,
                    };
                    Some((param, declared_ty))
                })
                .collect::<Vec<_>>();

            // 2. Determine constructor signature descriptor.
            let constructor_descriptor = format!(
                "({})V",
                constructor_params
                    .iter()
                    .map(|(_, ty)| ty.to_jvm_descriptor())
                    .collect::<String>()
            );

            // 3. Add Method reference for the constructor "<init>" with the determined signature
            let constructor_ref_index = cp.add_method_ref(
                class_index,
                "<init>",                // Standard name for constructors
                &constructor_descriptor, // Use the calculated descriptor
            )?;
            // 4. Generate instructions to create the object and set its fields

            // a. Emit 'new' instruction: Create uninitialized object
            instructions_to_add.push(JI::New(class_index)); // Stack: [uninitialized_ref]

            // b. Emit 'dup' instruction: Duplicate ref (one for invokespecial, one for result/fields)
            instructions_to_add.push(JI::Dup); // Stack: [uninitialized_ref, uninitialized_ref]

            // c. Load constructor parameters onto the stack IN ORDER
            for (param_const, _) in constructor_params {
                // Recursively load the constant value for the parameter.
                // Append the instructions directly to our temporary list.
                load_constant(&mut instructions_to_add, cp, param_const)?;
                // Stack: [uninitialized_ref, uninitialized_ref, param1, ..., param_i]
            }

            // d. Emit 'invokespecial' to call the constructor
            // Consumes the top ref and all params, initializes the object pointed to by the second ref.
            instructions_to_add.push(JI::Invokespecial(constructor_ref_index)); // Stack: [initialized_ref]

            // The generated constructor initializes all fields from `params`.
            // `fields` carries named OOMIR metadata and must not be written a
            // second time here (doing so duplicates referenced object graphs).
            let _ = fields;
        }
    };

    // Append the generated instructions for this constant (now including array logic)
    instructions.extend(instructions_to_add);

    Ok(())
}
