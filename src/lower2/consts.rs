use super::helpers::are_types_jvm_compatible;
use crate::oomir::{self, Type};
use ristretto_classfile::{
    self as jvm, ConstantPool,
    attributes::{ArrayType, Instruction},
};

// Helper to get the appropriate integer constant loading instruction
pub fn get_int_const_instr(cp: &mut ConstantPool, val: i32) -> Instruction {
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
pub fn get_long_const_instr(cp: &mut ConstantPool, val: i64) -> Instruction {
    // <-- Add `cp: &mut ConstantPool`
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
pub fn get_float_const_instr(cp: &mut ConstantPool, val: f32) -> Instruction {
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
pub fn get_double_const_instr(cp: &mut ConstantPool, val: f64) -> Instruction {
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
    cp: &mut ConstantPool,
    constant: &oomir::Constant,
) -> Result<(), jvm::Error> {
    use jvm::attributes::Instruction as JI;
    use oomir::Constant as OC;

    let mut instructions_to_add = Vec::new();

    match constant {
        OC::I8(v) => instructions_to_add.push(get_int_const_instr(cp, *v as i32)),
        OC::I16(v) => instructions_to_add.push(get_int_const_instr(cp, *v as i32)),
        OC::I32(v) => instructions_to_add.push(get_int_const_instr(cp, *v)),
        OC::I64(v) => instructions_to_add.push(get_long_const_instr(cp, *v)),
        OC::F32(v) => instructions_to_add.push(get_float_const_instr(cp, *v)),
        OC::F64(v) => instructions_to_add.push(get_double_const_instr(cp, *v)),
        OC::Boolean(v) => instructions_to_add.push(if *v { JI::Iconst_1 } else { JI::Iconst_0 }),
        OC::Char(v) => instructions_to_add.push(get_int_const_instr(cp, *v as i32)),
        OC::String(s) => {
            let index = cp.add_string(s)?;
            instructions_to_add.push(if let Ok(idx8) = u8::try_from(index) {
                JI::Ldc(idx8)
            } else {
                JI::Ldc_w(index)
            });
        }
        OC::Class(c) => {
            let index = cp.add_class(c)?;
            instructions_to_add.push(if let Ok(idx8) = u8::try_from(index) {
                JI::Ldc(idx8)
            } else {
                JI::Ldc_w(index)
            });
        }
        OC::Array(elem_ty, elements) => {
            let array_len = elements.len();

            // 1. Push array size onto stack
            instructions_to_add.push(get_int_const_instr(cp, array_len as i32));

            // 2. Create the new array (primitive or reference)
            if let Some(atype_code) = elem_ty.to_jvm_primitive_array_type_code() {
                let array_type = ArrayType::from_bytes(&mut std::io::Cursor::new(vec![atype_code])) // Wrap atype_code in Cursor<Vec<u8>>
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

                // --- Corrected Element Loading ---
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
        OC::Instance { class_name, fields } => {
            // 1. Add Class reference to constant pool
            let class_index = cp.add_class(class_name)?;

            // 2. Add Method reference for the default constructor "<init>()V"
            let constructor_ref_index = cp.add_method_ref(
                class_index,
                "<init>", // Standard name for constructors
                "()V",    // Standard descriptor for default constructor
            )?;

            // 3. Emit 'new' instruction
            instructions_to_add.push(JI::New(class_index)); // Stack: [uninitialized_ref]

            // 4. Emit 'dup' instruction (One ref for init, one for field setting/result)
            instructions_to_add.push(JI::Dup); // Stack: [uninitialized_ref, uninitialized_ref]

            // 5. Emit 'invokespecial' to call the constructor
            instructions_to_add.push(JI::Invokespecial(constructor_ref_index)); // Stack: [initialized_ref]

            // 6. Iterate through fields to set them
            for (field_name, field_value) in fields {
                // a. Duplicate the object reference (needed for putfield)
                instructions_to_add.push(JI::Dup); // Stack: [initialized_ref, initialized_ref]

                // b. Load the field's value onto the stack (using recursion into a temp vec)
                let mut field_value_instructions = Vec::new();
                load_constant(&mut field_value_instructions, cp, field_value)?;
                instructions_to_add.extend(field_value_instructions);
                // Stack: [initialized_ref, initialized_ref, field_value] (size 1 or 2)

                // c. Add Field reference
                let field_type = Type::from_constant(field_value);
                let field_descriptor = field_type.to_jvm_descriptor();
                let field_ref_index =
                    cp.add_field_ref(class_index, field_name, &field_descriptor)?;

                // d. Emit 'putfield'
                instructions_to_add.push(JI::Putfield(field_ref_index));
                // Stack: [initialized_ref] (putfield consumes the top ref and the value)
            }
            // After the loop, the final initialized_ref is left on the stack.
        }
    };

    // Append the generated instructions for this constant (now including array logic)
    instructions.extend(instructions_to_add);

    Ok(())
}
