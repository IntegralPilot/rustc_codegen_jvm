use super::{
    helpers::{
        are_types_jvm_compatible, get_cast_instructions, get_load_instruction, get_operand_type,
        get_store_instruction, get_type_size,
    },
    shim::get_shim_metadata,
};
use crate::oomir::{self, Type};

use ristretto_classfile::attributes::{ArrayType, Instruction};
use ristretto_classfile::{self as jvm, ConstantPool};
use std::collections::{HashMap, VecDeque};
use std::convert::TryInto;

/// Represents the state during the translation of a single function's body.
pub struct FunctionTranslator<'a, 'cp> {
    module: &'a oomir::Module,
    oomir_func: &'a oomir::Function,
    constant_pool: &'cp mut ConstantPool,
    this_class_name: &'a str, // Class name for self-references if needed

    local_var_map: HashMap<String, u16>, // OOMIR var name -> JVM local index
    local_var_types: HashMap<String, oomir::Type>, // OOMIR var name -> OOMIR Type
    next_local_index: u16,
    jvm_instructions: Vec<jvm::attributes::Instruction>,
    label_to_instr_index: HashMap<String, u16>, // OOMIR label -> JVM instruction index
    // Store (instruction_index_to_patch, target_label) for fixups
    branch_fixups: Vec<(usize, String)>,
    current_oomir_block_label: String, // For error reporting maybe

    // For max_locals calculation - track highest index used + size
    max_locals_used: u16,
}

impl<'a, 'cp> FunctionTranslator<'a, 'cp> {
    pub fn new(
        oomir_func: &'a oomir::Function,
        constant_pool: &'cp mut ConstantPool,
        this_class_name: &'a str,
        module: &'a oomir::Module,
    ) -> Self {
        let mut translator = FunctionTranslator {
            oomir_func,
            module,
            constant_pool,
            this_class_name,
            local_var_map: HashMap::new(),
            local_var_types: HashMap::new(),
            next_local_index: 0, // Start at 0 for static methods
            jvm_instructions: Vec::new(),
            label_to_instr_index: HashMap::new(),
            branch_fixups: Vec::new(),
            current_oomir_block_label: String::new(),
            max_locals_used: 0,
        };

        // Assign JVM local slots 0, 1, 2... to MIR argument names _1, _2, _3...
        // Assumes static methods where args start at slot 0.
        // Assumes OOMIR uses MIR's _1, _2 naming convention for args passed from lower1.rs.
        let num_params = oomir_func.signature.params.len();
        for i in 0..num_params {
            // Internal name for translator logic (optional, but helps clarity if complex logic added later)
            let param_translator_name = format!("param_{}", i);
            // The name used in the OOMIR body, corresponding to MIR convention (_1, _2, ...)
            let param_oomir_name = format!("_{}", i + 1);
            let param_ty = &oomir_func.signature.params[i];

            // Use assign_local to allocate the slot using the *translator* name first.
            // This ensures the slot is reserved and next_local_index advances correctly.
            let assigned_index = translator.assign_local(param_translator_name.as_str(), param_ty);

            // Now, explicitly map the OOMIR name (_1, _2, ...) to the *same* slot index
            // and store its type information using the OOMIR name as the key.
            // This allows instructions in the body using "_1" to find the correct slot.
            if translator
                .local_var_map
                .insert(param_oomir_name.clone(), assigned_index)
                .is_some()
            {
                // This shouldn't happen if MIR locals truly start after parameters
                println!(
                    "Warning: OOMIR parameter name '{}' potentially clashed with an existing mapping during parameter assignment.",
                    param_oomir_name
                );
            }
            if translator
                .local_var_types
                .insert(param_oomir_name.clone(), param_ty.clone())
                .is_some()
            {
                println!(
                    "Warning: OOMIR parameter name '{}' potentially clashed with an existing type mapping during parameter assignment.",
                    param_oomir_name
                );
            }
        }

        translator
    }

    /// Assigns a local variable to a JVM slot, returning the index.
    // Ensure assign_local ONLY inserts if vacant, it shouldn't be called directly
    // when we intend to overwrite like in the type mismatch case above.
    // The logic above directly modifies the map and next_local_index when overwriting.
    fn assign_local(&mut self, var_name: &str, ty: &oomir::Type) -> u16 {
        if let std::collections::hash_map::Entry::Vacant(e) =
            self.local_var_map.entry(var_name.to_string())
        {
            let index = self.next_local_index;
            e.insert(index);
            // Only insert type if it's also vacant (should always be if index is vacant)
            if self
                .local_var_types
                .insert(var_name.to_string(), ty.clone())
                .is_some()
            {
                println!(
                    "Internal Warning: Type map already had entry for supposedly new local '{}'",
                    var_name
                );
            }

            let size = get_type_size(ty);
            self.next_local_index += size;
            self.max_locals_used = self.max_locals_used.max(index + size);
            index
        } else {
            // This case should ideally not be hit if get_or_assign_local handles re-assignments.
            // If it IS hit, it means assign_local was called when the variable already exists.
            // This might happen with parameters if called carelessly after initial setup.
            let existing_index = self.local_var_map[var_name];
            println!(
                "Warning: assign_local called for existing variable '{}' (index {}). Reusing index.",
                var_name, existing_index
            );
            // Should we verify type consistency here too? Probably.
            if let Some(existing_ty) = self.local_var_types.get(var_name) {
                if existing_ty != ty {
                    println!(
                        "   -> CRITICAL WARNING: assign_local type mismatch for '{}'! Existing: {:?}, New: {:?}. Keeping existing index but this indicates a flaw!",
                        var_name, existing_ty, ty
                    );
                }
            }
            existing_index // Return existing index, but flag this as problematic
        }
    }

    /// Gets the slot index for a variable, assigning if new.
    fn get_or_assign_local(&mut self, var_name: &str, ty_hint: &oomir::Type) -> u16 {
        if let Some(existing_index) = self.local_var_map.get(var_name) {
            if let Some(existing_ty) = self.local_var_types.get(var_name) {
                // Check for exact type match OR JVM compatibility (e.g., storing I8 into an I32 slot might be okay sometimes, but struct vs array is not)
                // Let's be strict for now: require exact match to reuse.
                if existing_ty == ty_hint {
                    // Types match, reuse the slot
                    println!(
                        "Debug: Reusing local '{}' (index {}) with matching type {:?}.",
                        var_name, existing_index, ty_hint
                    );
                    *existing_index
                } else {
                    // Types differ. Assign a NEW slot and UPDATE the mapping for var_name.
                    println!(
                        "Warning: Type change detected for MIR local '{}'. Existing: {:?} (index {}), New: {:?}. Assigning NEW slot.",
                        var_name, existing_ty, existing_index, ty_hint
                    );

                    // Use assign_local's core logic but ensure we update the map for this specific var_name
                    let index = self.next_local_index;
                    // --- Overwrite the mapping for var_name ---
                    self.local_var_map.insert(var_name.to_string(), index);
                    self.local_var_types
                        .insert(var_name.to_string(), ty_hint.clone());
                    // --- Advance next_local_index ---
                    let size = get_type_size(ty_hint);
                    self.next_local_index += size;
                    self.max_locals_used = self.max_locals_used.max(index + size);
                    println!(
                        "   -> Assigned NEW index {} for '{}', size {}, next_local_index is now {}",
                        index, var_name, size, self.next_local_index
                    );
                    index // Return the newly assigned index
                }
            } else {
                // Inconsistency: index exists, but type doesn't. Treat as new assignment.
                println!(
                    "Error: Local '{}' index {} found but type missing! Assigning NEW slot.",
                    var_name, existing_index
                );
                // This duplicates the logic from the block above - could be refactored
                let index = self.next_local_index;
                self.local_var_map.insert(var_name.to_string(), index);
                self.local_var_types
                    .insert(var_name.to_string(), ty_hint.clone());
                let size = get_type_size(ty_hint);
                self.next_local_index += size;
                self.max_locals_used = self.max_locals_used.max(index + size);
                println!(
                    "   -> Assigned NEW index {} for '{}', size {}, next_local_index is now {}",
                    index, var_name, size, self.next_local_index
                );
                index
            }
        } else {
            // Variable is genuinely new (or first time seen in this type context). Use assign_local.
            self.assign_local(var_name, ty_hint) // assign_local handles map insertion etc.
        }
    }

    fn get_local_index(&self, var_name: &str) -> Result<u16, jvm::Error> {
        self.local_var_map
            .get(var_name)
            .copied()
            .ok_or_else(|| jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!("Undefined local variable used: {}", var_name),
            })
    }

    fn get_local_type(&self, var_name: &str) -> Result<&oomir::Type, jvm::Error> {
        self.local_var_types
            .get(var_name)
            .ok_or_else(|| jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!("Undefined local variable type requested for: {}", var_name),
            })
    }

    /// Translates the entire function body.
    pub fn translate(mut self) -> Result<(Vec<jvm::attributes::Instruction>, u16), jvm::Error> {
        // Use a worklist algorithm for potentially better handling of arbitrary CFGs
        let mut worklist: VecDeque<String> = VecDeque::new();
        let mut visited: HashMap<String, bool> = HashMap::new();

        worklist.push_back(self.oomir_func.body.entry.clone());
        visited.insert(self.oomir_func.body.entry.clone(), true);

        while let Some(block_label) = worklist.pop_front() {
            let block = self
                .oomir_func
                .body
                .basic_blocks
                .get(&block_label)
                .ok_or_else(|| jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!("Basic block label not found: {}", block_label),
                })?;

            self.current_oomir_block_label = block_label.clone();

            // Record the start instruction index for this block label
            let start_instr_index = self.jvm_instructions.len().try_into().unwrap();
            self.label_to_instr_index
                .insert(block_label.clone(), start_instr_index);

            // Translate instructions in the block
            for instr in &block.instructions {
                self.translate_instruction(self.module, instr)?;
            }

            // Add successor blocks to worklist if not visited
            if let Some(last_instr) = block.instructions.last() {
                match last_instr {
                    oomir::Instruction::Jump { target } => {
                        if visited.insert(target.clone(), true).is_none() {
                            worklist.push_back(target.clone());
                        }
                    }
                    oomir::Instruction::Branch {
                        true_block,
                        false_block,
                        ..
                    } => {
                        if visited.insert(true_block.clone(), true).is_none() {
                            worklist.push_back(true_block.clone());
                        }
                        if visited.insert(false_block.clone(), true).is_none() {
                            worklist.push_back(false_block.clone());
                        }
                    }
                    oomir::Instruction::Switch {
                        targets, otherwise, ..
                    } => {
                        // Add all unique target labels from the switch cases
                        for (_, target_label) in targets {
                            if visited.insert(target_label.clone(), true).is_none() {
                                worklist.push_back(target_label.clone());
                            }
                        }
                        // Add the otherwise label
                        if visited.insert(otherwise.clone(), true).is_none() {
                            worklist.push_back(otherwise.clone());
                        }
                    }
                    oomir::Instruction::Return { .. } => {
                        // Terminal blocks have no successors to add
                    }
                    _ => {
                        // Implicit fallthrough - This requires OOMIR blocks to be ordered or have explicit jumps.
                        // Assuming explicit jumps for now. If fallthrough is possible, need block ordering info.
                        // Find the next block label based on some ordering if necessary.
                        // For simplicity here, we *require* terminal instructions (Jump, Branch, Return, Throw).
                        // return Err(jvm::Error::VerificationError {
                        //     context: format!("Function {}", self.oomir_func.name),
                        //     message: format!("Basic block '{}' does not end with a terminal instruction", block_label),
                        // });
                    }
                }
            } else if self.oomir_func.body.basic_blocks.len() > 1 {
                // Empty block needs explicit jump?
                return Err(jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!("Non-terminal empty basic block '{}' found", block_label),
                });
            }
        }

        // --- Branch Fixup Pass ---
        for (instr_index, target_label) in self.branch_fixups {
            let target_instr_index =
                *self
                    .label_to_instr_index
                    .get(&target_label)
                    .ok_or_else(|| jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!("Branch target label not found: {}", target_label),
                    })?;

            // Update the placeholder instruction
            match &mut self.jvm_instructions[instr_index] {
                Instruction::Goto(offset)
                | Instruction::Ifnull(offset)
                | Instruction::Ifnonnull(offset)
                | Instruction::Ifeq(offset)
                | Instruction::Ifne(offset)
                | Instruction::Iflt(offset)
                | Instruction::Ifge(offset)
                | Instruction::Ifgt(offset)
                | Instruction::Ifle(offset)
                | Instruction::If_icmpeq(offset)
                | Instruction::If_icmpne(offset)
                | Instruction::If_icmplt(offset)
                | Instruction::If_icmpge(offset)
                | Instruction::If_icmpgt(offset)
                | Instruction::If_icmple(offset)
                | Instruction::If_acmpeq(offset)
                | Instruction::If_acmpne(offset) => {
                    *offset = target_instr_index;
                }
                _ => {
                    return Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!(
                            "Branch fixup expected a branch instruction at index {}",
                            instr_index
                        ),
                    });
                }
            }
        }

        Ok((self.jvm_instructions, self.max_locals_used))
    }

    /// Appends JVM instructions for loading an operand onto the stack.
    fn load_operand(&mut self, operand: &oomir::Operand) -> Result<(), jvm::Error> {
        match operand {
            oomir::Operand::Constant(c) => self.load_constant(c)?,
            oomir::Operand::Variable { name: var_name, ty } => {
                let index = self.get_local_index(var_name)?;
                let load_instr = get_load_instruction(ty, index)?;
                self.jvm_instructions.push(load_instr);
            }
        }
        Ok(())
    }

    /// Appends JVM instructions for loading a constant onto the stack.
    fn load_constant(&mut self, constant: &oomir::Constant) -> Result<(), jvm::Error> {
        use jvm::attributes::Instruction as JI;
        use oomir::Constant as OC;

        let mut instructions_to_add = Vec::new();

        match constant {
            OC::I8(v) => instructions_to_add.push(self.get_int_const_instr(*v as i32)),
            OC::I16(v) => instructions_to_add.push(self.get_int_const_instr(*v as i32)),
            OC::I32(v) => instructions_to_add.push(self.get_int_const_instr(*v)),
            OC::I64(v) => instructions_to_add.push(self.get_long_const_instr(*v)),
            OC::F32(v) => instructions_to_add.push(self.get_float_const_instr(*v)),
            OC::F64(v) => instructions_to_add.push(self.get_double_const_instr(*v)),
            OC::Boolean(v) => {
                instructions_to_add.push(if *v { JI::Iconst_1 } else { JI::Iconst_0 })
            }
            OC::Char(v) => instructions_to_add.push(self.get_int_const_instr(*v as i32)),
            OC::String(s) => {
                let index = self.constant_pool.add_string(s)?;
                instructions_to_add.push(if let Ok(idx8) = u8::try_from(index) {
                    JI::Ldc(idx8)
                } else {
                    JI::Ldc_w(index)
                });
            }
            OC::Class(c) => {
                let index = self.constant_pool.add_class(c)?;
                instructions_to_add.push(if let Ok(idx8) = u8::try_from(index) {
                    JI::Ldc(idx8)
                } else {
                    JI::Ldc_w(index)
                });
            }
            OC::Array(elem_ty, elements) => {
                let array_len = elements.len();

                // 1. Push array size onto stack
                instructions_to_add.push(self.get_int_const_instr(array_len as i32));

                // 2. Create the new array (primitive or reference)
                if let Some(atype_code) = elem_ty.to_jvm_primitive_array_type_code() {
                    let array_type =
                        ArrayType::from_bytes(&mut std::io::Cursor::new(vec![atype_code])) // Wrap atype_code in Cursor<Vec<u8>>
                            .map_err(|e| jvm::Error::VerificationError {
                                context: format!("Function {}", self.oomir_func.name),
                                // Use Display formatting for the error type if available
                                message: format!(
                                    "Invalid primitive array type code {}: {:?}",
                                    atype_code, e
                                ),
                            })?;
                    instructions_to_add.push(JI::Newarray(array_type)); // Stack: [arrayref]
                } else if let Some(internal_name) = elem_ty.to_jvm_internal_name() {
                    let class_index = self.constant_pool.add_class(&internal_name)?;
                    instructions_to_add.push(JI::Anewarray(class_index)); // Stack: [arrayref]
                } else {
                    return Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!("Cannot create JVM array for element type: {:?}", elem_ty),
                    });
                }

                let store_instruction =
                    elem_ty.get_jvm_array_store_instruction().ok_or_else(|| {
                        jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
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
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Type mismatch in Constant::Array: expected {:?}, found {:?} for element {}",
                                elem_ty, constant_type, i
                            ),
                        });
                    }

                    instructions_to_add.push(JI::Dup); // Stack: [arrayref, arrayref]
                    instructions_to_add.push(self.get_int_const_instr(i as i32)); // Stack: [arrayref, arrayref, index]

                    // --- Corrected Element Loading ---
                    // 1. Record the length of the main instruction vector *before* the recursive call.
                    let original_jvm_len = self.jvm_instructions.len();

                    // 2. Make the recursive call. This *will* append instructions to self.jvm_instructions.
                    self.load_constant(element_const)?;

                    // 3. Determine the range of instructions added by the recursive call.
                    let new_jvm_len = self.jvm_instructions.len();

                    // 4. If instructions were added, copy them from self.jvm_instructions to instructions_to_add.
                    if new_jvm_len > original_jvm_len {
                        // Create a slice referencing the newly added instructions
                        let added_instructions_slice =
                            &self.jvm_instructions[original_jvm_len..new_jvm_len];
                        // Extend the temporary vector with a clone of these instructions
                        instructions_to_add.extend_from_slice(added_instructions_slice);
                    }

                    // 5. Remove the instructions just added by the recursive call from self.jvm_instructions.
                    //    We truncate back to the length it had *before* the recursive call.
                    self.jvm_instructions.truncate(original_jvm_len);
                    // Now, self.jvm_instructions is back to its state before loading the element,
                    // and instructions_to_add contains the necessary Dup, index, element load instructions.

                    // Add the array store instruction to the temporary vector
                    instructions_to_add.push(store_instruction.clone()); // Stack: [arrayref]
                }
                // Final stack state after loop: [arrayref] (the populated array)
            }
            OC::Null => {
                // Push null reference onto the stack
                instructions_to_add.push(JI::Aconst_null);
            }
        };

        // Append the generated instructions for this constant (now including array logic)
        self.jvm_instructions.extend(instructions_to_add);

        Ok(())
    }

    /// Appends JVM instructions for storing the value currently on top of the stack
    /// into a local variable.
    fn store_result(&mut self, dest_var: &str, ty: &oomir::Type) -> Result<(), jvm::Error> {
        let index: u16 = self.get_or_assign_local(dest_var, ty);
        let store_instr = get_store_instruction(ty, index)?;
        self.jvm_instructions.push(store_instr);
        Ok(())
    }

    // Helper to get the appropriate integer constant loading instruction
    fn get_int_const_instr(&mut self, val: i32) -> Instruction {
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
                let index = self
                    .constant_pool
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
    fn get_long_const_instr(&mut self, val: i64) -> Instruction {
        // <-- Add `&mut self`
        match val {
            0 => Instruction::Lconst_0,
            1 => Instruction::Lconst_1,
            _ => {
                // Add the long value to the constant pool.
                let index = self
                    .constant_pool
                    .add_long(val)
                    .expect("Failed to add long to constant pool");
                // Ldc2_w is used for long/double constants and always takes a u16 index.
                Instruction::Ldc2_w(index)
            }
        }
    }

    // Helper to get the appropriate float constant loading instruction
    fn get_float_const_instr(&mut self, val: f32) -> Instruction {
        if val == 0.0 {
            Instruction::Fconst_0
        } else if val == 1.0 {
            Instruction::Fconst_1
        } else if val == 2.0 {
            Instruction::Fconst_2
        } else {
            // Add the float value to the constant pool.
            let index = self
                .constant_pool
                .add_float(val)
                .expect("Failed to add float to constant pool");
            // Ldc2_w is used for long/double constants and always takes a u16 index.
            Instruction::Ldc_w(index)
        }
    }

    // Helper to get the appropriate double constant loading instruction
    fn get_double_const_instr(&mut self, val: f64) -> Instruction {
        // Using bit representation for exact zero comparison is more robust
        if val.to_bits() == 0.0f64.to_bits() {
            // Handles +0.0 and -0.0
            Instruction::Dconst_0
        } else if val == 1.0 {
            Instruction::Dconst_1
        } else {
            // Add the double value to the constant pool.
            let index = self
                .constant_pool
                .add_double(val)
                .expect("Failed to add double to constant pool");
            // Ldc2_w is used for long/double constants and always takes a u16 index.
            Instruction::Ldc2_w(index)
        }
    }

    // --- Instruction Translation Helpers ---

    fn translate_binary_op(
        &mut self,
        dest: &str,
        op1: &oomir::Operand,
        op2: &oomir::Operand,
        jvm_op: Instruction,
    ) -> Result<(), jvm::Error> {
        self.load_operand(op1)?;
        self.load_operand(op2)?;
        self.jvm_instructions.push(jvm_op);
        let op1_type = match op1 {
            oomir::Operand::Variable { ty, .. } => ty.clone(),
            oomir::Operand::Constant(c) => Type::from_constant(c),
        };
        self.store_result(dest, &op1_type)?;
        Ok(())
    }

    /// Translates a comparison operation.
    ///
    /// The function emits code to compare `op1` and `op2` using the specified operation
    /// (e.g. "eq", "ne", "lt", "le", "gt", "ge") and leaves a boolean (0 or 1) result.
    /// For integer (or char) types it uses the if_icmp? instructions directly, while for I64,
    /// F32, and F64 it first emits an explicit comparison instruction (Lcmp/Fcmpl/Dcmpl)
    /// and then an integer branch instruction (Ifeq, Ifne, Iflt, Ifle, Ifgt, or Ifge).
    fn translate_comparison_op(
        &mut self,
        dest: &str,
        op1: &oomir::Operand,
        op2: &oomir::Operand,
        comp_op: &str, // expected values: "eq", "ne", "lt", "le", "gt", "ge"
    ) -> Result<(), jvm::Error> {
        // Determine operand types.
        let op1_type = get_operand_type(op1);
        let op2_type = get_operand_type(op2);
        if op1_type != op2_type {
            return Err(jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!(
                    "Type mismatch in comparison: {:?} vs {:?}",
                    op1_type, op2_type
                ),
            });
        }

        // Load both operands onto the stack.
        self.load_operand(op1)?;
        self.load_operand(op2)?;

        // These will be set to the branch constructor function and an optional comparison
        // instruction to be emitted first.
        let branch_constructor: Box<dyn Fn(u16) -> Instruction>;
        // For types where a separate compare instruction is required (I64, F32, F64),
        // we emit that now.
        match op1_type {
            // For I32 and Char, we can do a direct if_icmp? branch.
            oomir::Type::I32 | oomir::Type::Char | oomir::Type::Boolean => {
                branch_constructor = Box::new(|offset| match comp_op {
                    "eq" => Instruction::If_icmpeq(offset),
                    "ne" => Instruction::If_icmpne(offset),
                    "lt" => Instruction::If_icmplt(offset),
                    "le" => Instruction::If_icmple(offset),
                    "gt" => Instruction::If_icmpgt(offset),
                    "ge" => Instruction::If_icmpge(offset),
                    _ => {
                        // This branch should never be reached if comp_op is validated.
                        Instruction::Nop
                    }
                });
            }
            // For I64, we need to perform a comparison (Lcmp) then use an integer branch.
            oomir::Type::I64 => {
                self.jvm_instructions.push(Instruction::Lcmp); // Compares two longs; result is int.
                branch_constructor = Box::new(|offset| match comp_op {
                    "eq" => Instruction::Ifeq(offset),
                    "ne" => Instruction::Ifne(offset),
                    "lt" => Instruction::Iflt(offset),
                    "le" => Instruction::Ifle(offset),
                    "gt" => Instruction::Ifgt(offset),
                    "ge" => Instruction::Ifge(offset),
                    _ => Instruction::Nop,
                });
            }
            // For F32, perform a float comparison.
            oomir::Type::F32 => {
                self.jvm_instructions.push(Instruction::Fcmpl); // Compares floats; result is int.
                branch_constructor = Box::new(|offset| match comp_op {
                    "eq" => Instruction::Ifeq(offset),
                    "ne" => Instruction::Ifne(offset),
                    "lt" => Instruction::Iflt(offset),
                    "le" => Instruction::Ifle(offset),
                    "gt" => Instruction::Ifgt(offset),
                    "ge" => Instruction::Ifge(offset),
                    _ => Instruction::Nop,
                });
            }
            // For F64, perform a double comparison.
            oomir::Type::F64 => {
                self.jvm_instructions.push(Instruction::Dcmpl); // Compares doubles; result is int.
                branch_constructor = Box::new(|offset| match comp_op {
                    "eq" => Instruction::Ifeq(offset),
                    "ne" => Instruction::Ifne(offset),
                    "lt" => Instruction::Iflt(offset),
                    "le" => Instruction::Ifle(offset),
                    "gt" => Instruction::Ifgt(offset),
                    "ge" => Instruction::Ifge(offset),
                    _ => Instruction::Nop,
                });
            }
            _ => {
                return Err(jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!("Unsupported type for comparison: {:?}", op1_type),
                });
            }
        }

        // Validate that the comp_op string was one of the allowed values.
        if !["eq", "ne", "lt", "le", "gt", "ge"].contains(&comp_op) {
            return Err(jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!("Invalid comparison operator: {}", comp_op),
            });
        }

        // Record instruction indices and labels for branch fixups.
        let instr_idx_if = self.jvm_instructions.len();
        let label_true = format!("_comparison_true_{}", instr_idx_if);
        let label_after = format!("_comparison_after_{}", instr_idx_if);

        // Emit branch instruction using the branch_constructor.
        // The actual branch offset is a placeholder (0) to be fixed later.
        self.jvm_instructions.push(branch_constructor(0));
        self.branch_fixups.push((instr_idx_if, label_true.clone()));

        // False case: push 0 (false)
        self.jvm_instructions.push(Instruction::Iconst_0);
        let instr_idx_goto_after = self.jvm_instructions.len();
        self.jvm_instructions.push(Instruction::Goto(0)); // Placeholder offset.
        self.branch_fixups
            .push((instr_idx_goto_after, label_after.clone()));

        // True case: record label, then push 1 (true)
        let true_instr_index: u16 = self.jvm_instructions.len().try_into().unwrap();
        self.label_to_instr_index
            .insert(label_true, true_instr_index);
        self.jvm_instructions.push(Instruction::Iconst_1);

        // After branch: record label.
        let after_instr_index: u16 = self.jvm_instructions.len().try_into().unwrap();
        self.label_to_instr_index
            .insert(label_after, after_instr_index);

        // Store the result (comparison yields a boolean value).
        self.store_result(dest, &oomir::Type::Boolean)?;

        Ok(())
    }

    /// Translates a single OOMIR instruction and appends the corresponding JVM instructions.
    #[allow(clippy::too_many_lines)]
    fn translate_instruction(
        &mut self,
        module: &oomir::Module,
        instr: &oomir::Instruction,
    ) -> Result<(), jvm::Error> {
        use jvm::attributes::Instruction as JI;
        use oomir::Instruction as OI;
        use oomir::Operand as OO;

        match instr {
            OI::Add { dest, op1, op2 } => {
                let op_type = match op1 {
                    oomir::Operand::Variable { ty, .. } => ty.clone(),
                    oomir::Operand::Constant(c) => Type::from_constant(c),
                };
                let jvm_op = match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => JI::Iadd,
                    Type::I64 => JI::Ladd,
                    Type::F32 => JI::Fadd,
                    Type::F64 => JI::Dadd,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Add operation: {:?}", op_type),
                        });
                    }
                };
                self.translate_binary_op(dest, op1, op2, jvm_op)?
            }
            OI::Sub { dest, op1, op2 } => {
                let op_type = match op1 {
                    oomir::Operand::Variable { ty, .. } => ty.clone(),
                    oomir::Operand::Constant(c) => Type::from_constant(c),
                };
                let jvm_op = match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => JI::Isub,
                    Type::I64 => JI::Lsub,
                    Type::F32 => JI::Fsub,
                    Type::F64 => JI::Dsub,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Sub operation: {:?}", op_type),
                        });
                    }
                };
                self.translate_binary_op(dest, op1, op2, jvm_op)?
            }
            OI::Mul { dest, op1, op2 } => {
                let op_type = match op1 {
                    oomir::Operand::Variable { ty, .. } => ty.clone(),
                    oomir::Operand::Constant(c) => Type::from_constant(c),
                };
                let jvm_op = match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => JI::Imul,
                    Type::I64 => JI::Lmul,
                    Type::F32 => JI::Fmul,
                    Type::F64 => JI::Dmul,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Mul operation: {:?}", op_type),
                        });
                    }
                };
                self.translate_binary_op(dest, op1, op2, jvm_op)?
            }
            OI::Div { dest, op1, op2 } => {
                let op_type = match op1 {
                    oomir::Operand::Variable { ty, .. } => ty.clone(),
                    oomir::Operand::Constant(c) => Type::from_constant(c),
                };
                let jvm_op = match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => JI::Idiv,
                    Type::I64 => JI::Ldiv,
                    Type::F32 => JI::Fdiv,
                    Type::F64 => JI::Ddiv,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Div operation: {:?}", op_type),
                        });
                    }
                };
                self.translate_binary_op(dest, op1, op2, jvm_op)?
            }
            OI::Rem { dest, op1, op2 } => {
                let op_type = match op1 {
                    oomir::Operand::Variable { ty, .. } => ty.clone(),
                    oomir::Operand::Constant(c) => Type::from_constant(c),
                };
                let jvm_op = match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => JI::Irem,
                    Type::I64 => JI::Lrem,
                    Type::F32 => JI::Frem,
                    Type::F64 => JI::Drem,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Rem operation: {:?}", op_type),
                        });
                    }
                };
                self.translate_binary_op(dest, op1, op2, jvm_op)?
            }
            // --- Comparisons ---
            OI::Eq { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "eq")?,
            OI::Ne { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "ne")?,
            OI::Lt { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "lt")?,
            OI::Le { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "le")?,
            OI::Gt { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "gt")?,
            OI::Ge { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "ge")?,

            // --- Bitwise Operations ---
            OI::BitAnd { dest, op1, op2 } => {
                let op_type = match op1 {
                    oomir::Operand::Variable { ty, .. } => ty.clone(),
                    oomir::Operand::Constant(c) => Type::from_constant(c),
                };
                let jvm_op = match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => JI::Iand,
                    Type::I64 => JI::Land,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Unsupported type for BitAnd operation: {:?}",
                                op_type
                            ),
                        });
                    }
                };
                self.translate_binary_op(dest, op1, op2, jvm_op)?
            }
            OI::BitOr { dest, op1, op2 } => {
                let op_type = match op1 {
                    oomir::Operand::Variable { ty, .. } => ty.clone(),
                    oomir::Operand::Constant(c) => Type::from_constant(c),
                };
                let jvm_op = match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => JI::Ior,
                    Type::I64 => JI::Lor,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for BitOr operation: {:?}", op_type),
                        });
                    }
                };
                self.translate_binary_op(dest, op1, op2, jvm_op)?
            }
            OI::BitXor { dest, op1, op2 } => {
                let op_type = match op1 {
                    oomir::Operand::Variable { ty, .. } => ty.clone(),
                    oomir::Operand::Constant(c) => Type::from_constant(c),
                };
                let jvm_op = match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => JI::Ixor,
                    Type::I64 => JI::Lxor,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Unsupported type for BitXor operation: {:?}",
                                op_type
                            ),
                        });
                    }
                };
                self.translate_binary_op(dest, op1, op2, jvm_op)?
            }
            OI::Shl { dest, op1, op2 } => {
                let op_type = match op1 {
                    oomir::Operand::Variable { ty, .. } => ty.clone(),
                    oomir::Operand::Constant(c) => Type::from_constant(c),
                };
                let jvm_op = match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => JI::Ishl,
                    Type::I64 => JI::Lshl,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Shl operation: {:?}", op_type),
                        });
                    }
                };
                self.translate_binary_op(dest, op1, op2, jvm_op)?
            }
            OI::Shr { dest, op1, op2 } => {
                let op_type = match op1 {
                    oomir::Operand::Variable { ty, .. } => ty.clone(),
                    oomir::Operand::Constant(c) => Type::from_constant(c),
                };
                let jvm_op = match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => JI::Ishr,
                    Type::I64 => JI::Lshr,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Shr operation: {:?}", op_type),
                        });
                    }
                };
                self.translate_binary_op(dest, op1, op2, jvm_op)?
            }

            // --- Logical Operations (Need Short-circuiting) ---
            // These are more complex as they involve control flow, not direct JVM ops.
            // Example for And:
            //   load op1
            //   ifeq label_false  // If op1 is false, result is false
            //   load op2
            //   ifeq label_false  // If op2 is false, result is false
            //   iconst_1          // Both true, result is true
            //   goto label_end
            // label_false:
            //   iconst_0          // Result is false
            // label_end:
            //   istore dest
            OI::And {
                dest: _,
                op1: _,
                op2: _,
            } => {
                unimplemented!("Logical And needs control flow translation")
            }
            OI::Or {
                dest: _,
                op1: _,
                op2: _,
            } => {
                unimplemented!("Logical Or needs control flow translation")
            }
            OI::Not { dest, src } => {
                // 1. Load source operand
                self.load_operand(src)?; // Stack: [value]
                // 2. Determine type and apply appropriate NOT logic
                let src_type = get_operand_type(src); // You need this helper
                match src_type {
                    oomir::Type::Boolean => {
                        // Boolean NOT (!bool_val equivalent to bool_val ^ 1)
                        self.jvm_instructions.push(JI::Iconst_1); // Stack: [value(0/1), 1]
                        self.jvm_instructions.push(JI::Ixor); // Stack: [!value]
                    }
                    // Integer types use bitwise NOT (~int_val equivalent to int_val ^ -1)
                    oomir::Type::I8 | oomir::Type::I16 | oomir::Type::I32 | oomir::Type::Char => {
                        self.jvm_instructions.push(JI::Iconst_m1); // Stack: [value, -1]
                        self.jvm_instructions.push(JI::Ixor); // Stack: [~value]
                    }
                    oomir::Type::I64 => {
                        // Long Bitwise NOT: value ^ -1L
                        // Ldc2_w is used for loading long/double constants. -1L needs to be in the pool.
                        let neg_one_long_index = self.constant_pool.add_long(-1_i64)?; // Add -1L if not present
                        self.jvm_instructions.push(JI::Ldc2_w(neg_one_long_index)); // Stack: [value(long), -1L(long)]
                        self.jvm_instructions.push(JI::Lxor); // Stack: [~value(long)]
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Not operation: {:?}", src_type),
                        });
                    }
                }
                // 3. Store the result (type remains the same as source)
                self.store_result(dest, &src_type)?; // Stack: []
            }

            OI::Neg { dest, src } => {
                // 1. Load source operand
                self.load_operand(src)?; // Stack: [value]
                // 2. Determine type and apply appropriate Negation instruction
                let src_type = get_operand_type(src); // You need this helper
                let neg_instr = match src_type {
                    // Integer types (including bool/char treated as int) use Ineg
                    oomir::Type::I8
                    | oomir::Type::I16
                    | oomir::Type::I32
                    | oomir::Type::Boolean
                    | oomir::Type::Char => JI::Ineg,
                    oomir::Type::I64 => JI::Lneg,
                    oomir::Type::F32 => JI::Fneg,
                    oomir::Type::F64 => JI::Dneg,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Neg operation: {:?}", src_type),
                        });
                    }
                };
                self.jvm_instructions.push(neg_instr.clone()); // Stack: [-value]
                // 3. Store the result (type remains the same as source)
                self.store_result(dest, &src_type)?; // Stack: []
            }

            // --- Control Flow ---
            OI::Jump { target } => {
                let instr_index = self.jvm_instructions.len();
                self.jvm_instructions.push(JI::Goto(0)); // Placeholder
                self.branch_fixups.push((instr_index, target.clone()));
            }
            OI::Branch {
                condition,
                true_block,
                false_block,
            } => {
                // 1. Load the condition (must evaluate to int 0 or 1)
                self.load_operand(condition)?;

                // 2. Add conditional jump (if condition != 0, jump to true_block)
                let instr_idx_ifne = self.jvm_instructions.len();
                self.jvm_instructions.push(JI::Ifne(0)); // Placeholder (If Not Equal to zero)
                self.branch_fixups
                    .push((instr_idx_ifne, true_block.clone()));

                // 3. Add unconditional jump to false_block (this is the fallthrough if condition == 0)
                let instr_idx_goto_false = self.jvm_instructions.len();
                self.jvm_instructions.push(JI::Goto(0)); // Placeholder
                self.branch_fixups
                    .push((instr_idx_goto_false, false_block.clone()));
            }
            OI::Switch {
                discr,
                targets,
                otherwise,
            } => {
                // We will translate this into a chain of if/goto instructions.

                // 0. calculate the type of the discriminant
                let discr_type = match discr {
                    OO::Variable { ty, .. } => ty.clone(),
                    OO::Constant(c) => Type::from_constant(c),
                };

                // --- Input Validation ---
                // Check if the discriminant type is suitable for comparison
                match discr_type {
                    Type::I8
                    | Type::I16
                    | Type::I32
                    | Type::Boolean
                    | Type::Char
                    | Type::I64
                    | Type::F32
                    | Type::F64 => {
                        // These types are okay for switch comparison
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Unsupported discriminant type {:?} for OOMIR Switch instruction",
                                discr_type
                            ),
                        });
                    }
                }

                // 1. Load the discriminant value onto the stack
                self.load_operand(discr)?; // Stack: [discr_value] (size 1 or 2 depending on type)

                // 2. Store the discriminant in a temporary local variable.
                let temp_discr_var_name =
                    format!("_switch_discr_temp_{}", self.current_oomir_block_label);
                let temp_discr_index = self.get_or_assign_local(&temp_discr_var_name, &discr_type);
                let store_instr = get_store_instruction(&discr_type, temp_discr_index)?;
                self.jvm_instructions.push(store_instr); // Stack is now empty

                // 3. Iterate through the specific targets and generate comparison checks
                for (constant_key, target_label) in targets {
                    // a. Reload the discriminant value from the temporary local
                    let load_instr = get_load_instruction(&discr_type, temp_discr_index)?;
                    self.jvm_instructions.push(load_instr); // Stack: [discr_value] (size 1 or 2)

                    // --- b. Load constant key and perform comparison based on discriminant type ---
                    let comparison_succeeded; // Flag to track if this key was handled

                    match &discr_type {
                        // --- Integer-like types (use if_icmpeq) ---
                        Type::I8 | Type::I16 | Type::I32 | Type::Boolean | Type::Char => {
                            let key_value_i32 = match constant_key {
                                oomir::Constant::I8(v) => i32::from(*v),
                                oomir::Constant::I16(v) => i32::from(*v),
                                oomir::Constant::I32(v) => *v,
                                oomir::Constant::Boolean(b) => {
                                    if *b {
                                        1
                                    } else {
                                        0
                                    }
                                }
                                oomir::Constant::Char(c) => *c as i32,
                                _ => {
                                    return Err(jvm::Error::VerificationError {
                                        context: format!("Function {}", self.oomir_func.name),
                                        message: format!(
                                            "Type mismatch in OOMIR Switch: Discriminant type is {:?}, but case key is {:?}",
                                            discr_type, constant_key
                                        ),
                                    });
                                }
                            };

                            // Load the i32 constant key
                            let const_instr = self.get_int_const_instr(key_value_i32);
                            self.jvm_instructions.push(const_instr); // Stack: [discr_value(i32), key_value(i32)]

                            // Add the comparison instruction (if_icmpeq jumps if equal)
                            let if_instr_index = self.jvm_instructions.len();
                            self.jvm_instructions.push(JI::If_icmpeq(0)); // Placeholder offset
                            self.branch_fixups
                                .push((if_instr_index, target_label.clone()));
                            comparison_succeeded = true;
                        }

                        // --- I64 (long) type (use lcmp, ifeq) ---
                        Type::I64 => {
                            self.load_constant(constant_key)?;

                            // Compare longs: pushes -1, 0, or 1 (int) onto the stack
                            self.jvm_instructions.push(JI::Lcmp); // Stack: [comparison_result(i32)]

                            // Jump if the comparison result is 0 (equal)
                            let if_instr_index = self.jvm_instructions.len();
                            self.jvm_instructions.push(JI::Ifeq(0)); // Placeholder offset
                            self.branch_fixups
                                .push((if_instr_index, target_label.clone()));
                            comparison_succeeded = true;
                        }

                        // --- F32 (float) type (use fcmpl/fcmpg, ifeq) ---
                        Type::F32 => {
                            self.load_constant(constant_key)?; // Stack: [discr_value(f32), key_value(f32)]

                            // Compare floats: pushes -1, 0, or 1 (int) onto the stack.
                            // fcmpl treats NaN comparison as -1. fcmpg treats it as +1.
                            // For equality checks (== 0), they behave the same unless one operand is NaN.
                            // Standard equality `discr == key` is false if either is NaN, so `ifeq` works after fcmpl/fcmpg.
                            self.jvm_instructions.push(JI::Fcmpl); // Stack: [comparison_result(i32)]

                            // Jump if the comparison result is 0 (equal)
                            let if_instr_index = self.jvm_instructions.len();
                            self.jvm_instructions.push(JI::Ifeq(0)); // Placeholder offset
                            self.branch_fixups
                                .push((if_instr_index, target_label.clone()));
                            comparison_succeeded = true;
                        }

                        // --- F64 (double) type (use dcmpl/dcmpg, ifeq) ---
                        Type::F64 => {
                            self.load_constant(constant_key)?; // Stack: [discr_value(f64), key_value(f64)]

                            // Compare doubles (similar logic to floats regarding NaN and dcmpl/dcmpg)
                            self.jvm_instructions.push(JI::Dcmpl); // Stack: [comparison_result(i32)]

                            // Jump if the comparison result is 0 (equal)
                            let if_instr_index = self.jvm_instructions.len();
                            self.jvm_instructions.push(JI::Ifeq(0)); // Placeholder offset
                            self.branch_fixups
                                .push((if_instr_index, target_label.clone()));
                            comparison_succeeded = true;
                        }

                        // Should have been caught by the validation check before the loop
                        _ => unreachable!("Invalid discriminant type survived initial check"),
                    }

                    // If the constant_key type didn't match the discr_type in the inner match
                    if !comparison_succeeded {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Type mismatch in OOMIR Switch: Discriminant type is {:?}, but case key is {:?}",
                                discr_type, constant_key
                            ),
                        });
                    }

                    // If the comparison is false, execution falls through to the next check (or the final goto)
                    // The stack should be empty after the conditional jump instruction consumes its operand(s).
                }

                // 4. After all specific checks, add an unconditional jump to the 'otherwise' block.
                //    This is executed if none of the comparisons were true.
                let goto_instr_index = self.jvm_instructions.len();
                self.jvm_instructions.push(JI::Goto(0)); // Placeholder offset
                self.branch_fixups
                    .push((goto_instr_index, otherwise.clone()));
            }
            OI::Return { operand } => {
                match operand {
                    Some(op) => {
                        // Determine type based on function signature's return type
                        let ret_ty = &self.oomir_func.signature.ret;
                        self.load_operand(op)?;
                        let return_instr = match **ret_ty {
                            oomir::Type::I8
                            | oomir::Type::I16
                            | oomir::Type::I32
                            | oomir::Type::Boolean
                            | oomir::Type::Char => JI::Ireturn,
                            oomir::Type::I64 => JI::Lreturn,
                            oomir::Type::F32 => JI::Freturn,
                            oomir::Type::F64 => JI::Dreturn,
                            oomir::Type::Reference(_)
                            | oomir::Type::Array(_)
                            | oomir::Type::String
                            | oomir::Type::Class(_) => JI::Areturn,
                            oomir::Type::Void => JI::Return, // Should not happen with Some(op)
                        };
                        self.jvm_instructions.push(return_instr);
                    }
                    None => {
                        self.jvm_instructions.push(JI::Return);
                    }
                }
            }
            OI::Label { name } => {
                // This instruction marks a potential jump target within the bytecode stream.
                // Record the current JVM instruction index (offset) for this label name.
                // This index points to the *next* instruction that will be generated.
                let current_jvm_instr_index =
                    self.jvm_instructions.len().try_into().map_err(|_| {
                        jvm::Error::VerificationError {
                            context: "Function too large".to_string(),
                            message: "Instruction index exceeds u16::MAX".to_string(),
                        }
                    })?;

                // Insert the mapping from the OOMIR label name to the JVM instruction index.
                if let Some(old_idx) = self
                    .label_to_instr_index
                    .insert(name.clone(), current_jvm_instr_index)
                {
                    // This *could* happen if a label name conflicts with a basic block name,
                    // or if the label generation logic somehow creates duplicates.
                    // Should be investigated if it occurs. Might indicate an issue in lower1's label generation.
                    println!(
                        "Warning: Overwriting existing entry in label_to_instr_index for label '{}'. Old index: {}, New index: {}",
                        name, old_idx, current_jvm_instr_index
                    );
                    // Depending on requirements, you might want to error here instead of warning.
                }
                // No JVM instructions are generated for an OOMIR Label itself.
                // It only affects the mapping used by branch fixups.
            }
            OI::Call {
                dest,
                function: function_name,
                args,
            } => {
                let mut handled_as_shim = false;
                let mut is_diverging_call = false;

                // --- Shim Lookup using JSON metadata ---
                match get_shim_metadata() {
                    Ok(shim_map) => {
                        // Use the function_name (make_jvm_safe output) as the key
                        if let Some(shim_info) = shim_map.get(function_name) {
                            handled_as_shim = true;

                            // 1. Load arguments
                            for arg in args {
                                self.load_call_argument(arg)?; // Use helper to handle references properly
                            }

                            // 2. Add MethodRef
                            // Convention: Class is always org/rustlang/core/Core
                            let kotlin_shim_class = "org/rustlang/core/Core";
                            let class_index = self.constant_pool.add_class(kotlin_shim_class)?;
                            let method_ref_index = self.constant_pool.add_method_ref(
                                class_index,
                                function_name,         // The key IS the method name
                                &shim_info.descriptor, // Use descriptor from JSON
                            )?;

                            // 3. Add invoke instruction
                            if shim_info.is_static {
                                self.jvm_instructions
                                    .push(JI::Invokestatic(method_ref_index));
                            } else {
                                return Err(jvm::Error::VerificationError {
                                    context: format!("Function {}", self.oomir_func.name),
                                    message: format!(
                                        "Non-static shim calls not yet supported ('{}')",
                                        function_name
                                    ),
                                });
                            }

                            // Check for diverging
                            // Currently hardcoded
                            if function_name == "panic_fmt"
                                || function_name == "core_panicking_panic"
                            {
                                is_diverging_call = true;
                            }

                            // 4. Store result (logic remains similar, needs descriptor parsing)
                            if !is_diverging_call && !shim_info.descriptor.ends_with(")V") {
                                if let Some(dest_var) = dest {
                                    // Parse the return type from the descriptor
                                    let return_type = oomir::Type::from_jvm_descriptor_return_type(
                                        &shim_info.descriptor,
                                    );
                                    self.store_result(dest_var, &return_type)?;
                                } else {
                                    // Pop ignored result (needs size from descriptor)
                                    let return_type = oomir::Type::from_jvm_descriptor_return_type(
                                        &shim_info.descriptor,
                                    );
                                    match get_type_size(&return_type) {
                                        1 => self.jvm_instructions.push(JI::Pop),
                                        2 => self.jvm_instructions.push(JI::Pop2),
                                        _ => {
                                            return Err(jvm::Error::VerificationError {
                                                context: format!(
                                                    "Function {}",
                                                    self.oomir_func.name
                                                ),
                                                message: format!(
                                                    "Unexpected return type size for shim '{}' from descriptor '{}'",
                                                    function_name, shim_info.descriptor
                                                ),
                                            });
                                        }
                                    }
                                }
                            } else if dest.is_some()
                                && !is_diverging_call
                                && shim_info.descriptor.ends_with(")V")
                            {
                                return Err(jvm::Error::VerificationError {
                                    context: format!("Function {}", self.oomir_func.name),
                                    message: format!(
                                        "Attempting to store void result from shim '{}'",
                                        function_name
                                    ),
                                });
                            }
                        } // End if shim_info found by name
                    } // End Ok(shim_map)
                    Err(e) => {
                        // Metadata loading failed, print warning and fall through
                        println!(
                            "Warning: Failed to get shim metadata: {}. Falling back to intra-module call attempt for '{}'.",
                            e, function_name
                        );
                    }
                } // End Shim Lookup

                // --- Intra-Module Call (Fallback) ---
                if !handled_as_shim {
                    // This logic remains the same, using function_name for lookup
                    let target_func = module.functions.get(function_name).ok_or_else(|| {
                        jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Cannot find function '{}' within OOMIR module or as a known shim.",
                                function_name
                            ),
                        }
                    })?;
                    let target_sig = &target_func.signature;

                    // 1. Load arguments
                    if args.len() != target_sig.params.len() {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Argument count mismatch for function '{}': expected {}, found {}",
                                function_name,
                                target_sig.params.len(),
                                args.len()
                            ),
                        });
                    }
                    for (arg, _) in args.iter().zip(target_sig.params.iter()) {
                        self.load_call_argument(arg)?; // Use helper to handle references properly
                    }

                    // 2. Add MethodRef
                    let class_index = self.constant_pool.add_class(self.this_class_name)?;
                    let method_ref_index = self.constant_pool.add_method_ref(
                        class_index,
                        function_name.clone(),
                        target_sig.to_string(),
                    )?;

                    // 3. Add invokestatic
                    self.jvm_instructions
                        .push(JI::Invokestatic(method_ref_index));

                    // 4. Store result or Pop
                    if let Some(dest_var) = dest {
                        if *target_sig.ret != oomir::Type::Void {
                            self.store_result(dest_var, &target_sig.ret)?;
                        } else { /* error storing void */
                        }
                    } else if *target_sig.ret != oomir::Type::Void {
                        match get_type_size(&target_sig.ret) {
                            1 => self.jvm_instructions.push(JI::Pop),
                            2 => self.jvm_instructions.push(JI::Pop2),
                            _ => {}
                        }
                    }
                }

                if is_diverging_call {
                    // After calling a function like panic_fmt that returns void but always throws,
                    // add an unreachable throw sequence to satisfy the bytecode verifier.
                    let error_class_name = "java/lang/Error"; // Or AssertionError, doesn't matter
                    let error_class_index = self.constant_pool.add_class(error_class_name)?;
                    let error_init_ref = self.constant_pool.add_method_ref(
                        error_class_index,
                        "<init>",
                        "()V", // Default constructor descriptor
                    )?;
                    self.jvm_instructions.push(JI::New(error_class_index));
                    self.jvm_instructions.push(JI::Dup);
                    self.jvm_instructions
                        .push(JI::Invokespecial(error_init_ref));
                    self.jvm_instructions.push(JI::Athrow);
                }
            }
            OI::Move { dest, src } => {
                // Determine the type of the VALUE being moved (from the source operand)
                let value_type = match src {
                    OO::Constant(c) => Type::from_constant(c),
                    OO::Variable { ty, .. } => ty.clone(),
                };

                // 1. Load the source operand's value onto the stack.
                self.load_operand(src)?; // e.g., pushes I32(11)

                // 2. Store the value from the stack into the destination variable.
                //    Crucially, use the 'value_type' determined above.
                //    'store_result' will call 'get_or_assign_local' internally.
                //    'get_or_assign_local' will handle finding the index (and warning
                //    if reusing a slot like '_1' with an incompatible type hint).
                //    'store_result' will then use the correct 'value_type' (e.g., I32)
                //    to select the appropriate JVM store instruction (e.g., istore_0).
                self.store_result(dest, &value_type)?;
            }
            OI::NewArray {
                dest,
                element_type,
                size,
            } => {
                // 1. Load size onto the stack
                self.load_operand(size)?; // Stack: [size_int]

                // 2. Determine and add the array creation instruction
                let array_type_for_dest = oomir::Type::Array(Box::new(element_type.clone()));
                if let Some(atype_code) = element_type.to_jvm_primitive_array_type_code() {
                    // Primitive array
                    let array_type_enum =
                        ArrayType::from_bytes(&mut std::io::Cursor::new(vec![atype_code]))
                            .map_err(|e| jvm::Error::VerificationError {
                                context: format!("Function {}", self.oomir_func.name),
                                message: format!(
                                    "Invalid primitive array type code {} for NewArray: {:?}",
                                    atype_code, e
                                ),
                            })?;
                    self.jvm_instructions.push(JI::Newarray(array_type_enum)); // Stack: [arrayref]
                } else if let Some(internal_name) = element_type.to_jvm_internal_name() {
                    // Reference type array
                    let class_index = self.constant_pool.add_class(&internal_name)?;
                    self.jvm_instructions.push(JI::Anewarray(class_index)); // Stack: [arrayref]
                } else {
                    // Unsupported element type
                    return Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!(
                            "Cannot create JVM array for element type: {:?} in NewArray",
                            element_type
                        ),
                    });
                }

                // 3. Store the resulting array reference into the destination variable
                // This also ensures the type Type::Array(...) is stored for 'dest'
                self.store_result(dest, &array_type_for_dest)?; // Stack: []
            }

            OI::ArrayStore {
                array_var,
                index,
                value,
            } => {
                // 1. Get the type of the array variable to find the element type
                let array_type = self.get_local_type(array_var)?.clone(); // Clone to avoid borrow issues
                let element_type = match array_type.clone() {
                    oomir::Type::Array(et) => et,
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Variable '{}' used in ArrayStore is not an array type, found {:?}",
                                array_var,
                                array_type.clone()
                            ),
                        });
                    }
                };

                // 2. Load array reference
                // Use the full array type when loading the variable
                let array_operand = oomir::Operand::Variable {
                    name: array_var.clone(),
                    ty: array_type,
                };
                self.load_operand(&array_operand)?; // Stack: [arrayref]

                // 3. Load index
                self.load_operand(index)?; // Stack: [arrayref, index_int]

                // 4. Load value
                self.load_operand(value)?; // Stack: [arrayref, index_int, value]

                // 5. Get and add the appropriate array store instruction
                let store_instr =
                    element_type
                        .get_jvm_array_store_instruction()
                        .ok_or_else(|| jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Cannot determine array store instruction for element type: {:?}",
                                element_type
                            ),
                        })?;
                self.jvm_instructions.push(store_instr); // Stack: []
            }
            OI::ArrayGet {
                dest,
                array_var,
                index,
            } => {
                // 1. Get the type of the array variable to find the element type
                let array_type = self.get_local_type(array_var)?.clone(); // Clone to avoid borrow issues
                let element_type = match &array_type {
                    // Use reference to avoid move
                    oomir::Type::Array(et) => et.as_ref(), // Get a reference to the contained element type
                    _ => {
                        // Error: The variable is not an array type
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Variable '{}' used in ArrayGet is not an array type, found {:?}",
                                array_var, array_type
                            ),
                        });
                    }
                };

                // 2. Load array reference onto the stack
                // Construct the operand with the correct *array* type for loading the variable
                let array_operand = oomir::Operand::Variable {
                    name: array_var.clone(),
                    ty: array_type.clone(), // Use the full array type here
                };
                self.load_operand(&array_operand)?; // Stack: [arrayref]

                // 3. Load index onto the stack
                // The index operand should always be some integer type, load_operand handles it.
                self.load_operand(index)?; // Stack: [arrayref, index_int]

                // 4. Get and add the appropriate array load instruction based on element type
                let load_instr = element_type
                    .get_jvm_array_load_instruction() // Use the helper we defined/assumed
                    .ok_or_else(|| jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!(
                            "Cannot determine array load instruction for element type: {:?}",
                            element_type
                        ),
                    })?;
                self.jvm_instructions.push(load_instr); // Consumes arrayref and index, pushes value
                // Stack: [value] (size 1 or 2 depending on element_type)

                // 5. Store the resulting element into the destination variable
                // The type of the value on the stack is the element_type
                self.store_result(dest, element_type)?; // Consumes value, Stack: []
            }
            OI::Length { dest, array_var } => {
                // 1. Get the type of the array variable itself
                let array_actual_type = self.get_local_type(array_var)?.clone();
                match &array_actual_type {
                    oomir::Type::Array(_) => { /* Okay */ }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Variable '{}' used in Length instruction is not an array type, found {:?}",
                                array_var, array_actual_type
                            ),
                        });
                    }
                };

                // 2. Load the array reference onto the stack
                let array_var_index = self.get_local_index(array_var)?;
                // Use the actual type retrieved above for loading
                let load_array_instr = get_load_instruction(&array_actual_type, array_var_index)?;
                self.jvm_instructions.push(load_array_instr.clone()); // Stack: [arrayref]

                // 3. Emit 'arraylength' instruction
                //    This consumes the arrayref and pushes the length (int)
                self.jvm_instructions.push(JI::Arraylength); // Stack: [length_int]
                let dest_type = oomir::Type::I32;

                self.store_result(dest, &dest_type)?; // Stack: []
            }
            OI::Throw { exception } => {
                // Assuming the operand is already an exception *instance* reference
                // TODO: Determine the correct type hint here! Assuming Object for now.
                self.load_operand(exception)?;
                self.jvm_instructions.push(JI::Athrow);
            }
            OI::ThrowNewWithMessage {
                exception_class,
                message,
            } => {
                // 1. Add necessary constants to the pool
                let class_index = self.constant_pool.add_class(exception_class)?;
                let string_index = self.constant_pool.add_string(message)?;
                // Assumes a constructor like new RuntimeException(String msg)
                let constructor_ref_index = self.constant_pool.add_method_ref(
                    class_index,
                    "<init>",
                    "(Ljava/lang/String;)V", // Descriptor for constructor taking a String
                )?;

                // 2. Emit the bytecode sequence: new, dup, ldc(message), invokespecial, athrow
                self.jvm_instructions.push(JI::New(class_index));
                self.jvm_instructions.push(JI::Dup);

                // Load the message string constant
                if let Ok(idx8) = u8::try_from(string_index) {
                    self.jvm_instructions.push(JI::Ldc(idx8));
                } else {
                    self.jvm_instructions.push(JI::Ldc_w(string_index));
                }

                self.jvm_instructions
                    .push(JI::Invokespecial(constructor_ref_index));
                self.jvm_instructions.push(JI::Athrow);
            }
            OI::ConstructObject { dest, class_name } => {
                // 1. Add Class reference to constant pool
                let class_index = self.constant_pool.add_class(class_name)?;

                // 2. Add Method reference for the default constructor "<init>()V"
                let constructor_ref_index = self.constant_pool.add_method_ref(
                    class_index,
                    "<init>", // Standard name for constructors
                    "()V",    // Standard descriptor for default constructor
                )?;

                // 3. Emit 'new' instruction
                self.jvm_instructions.push(JI::New(class_index)); // Stack: [uninitialized_ref]

                // 4. Emit 'dup' instruction
                self.jvm_instructions.push(JI::Dup); // Stack: [uninitialized_ref, uninitialized_ref]

                // 5. Emit 'invokespecial' to call the constructor
                self.jvm_instructions
                    .push(JI::Invokespecial(constructor_ref_index)); // Stack: [initialized_ref]

                // 6. Store the initialized object reference into the destination variable
                //    The type of the destination variable is Type::Class(class_name)
                let dest_type = oomir::Type::Class(class_name.clone());
                self.store_result(dest, &dest_type)?; // Stack: []
            }

            OI::SetField {
                object_var,
                field_name,
                value,
                field_ty,
                owner_class, // Class where the field is *defined*
            } => {
                // 1. Get the type of the object variable itself (should be a Class type)
                let object_actual_type = self.get_local_type(object_var)?.clone();

                // We don't strictly *need* object_actual_type for the load instruction itself
                // if get_load_instruction correctly handles all reference types with Aload,
                // but it's good practice to verify it's a reference type.
                if !object_actual_type.is_jvm_reference_type() {
                    return Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!(
                            "Variable '{}' used in SetField is not a reference type, found {:?}",
                            object_var, object_actual_type
                        ),
                    });
                }

                // 2. Add Field reference to constant pool
                let owner_class_index = self.constant_pool.add_class(owner_class)?;
                let field_descriptor = field_ty.to_jvm_descriptor();
                let field_ref_index = self.constant_pool.add_field_ref(
                    owner_class_index,
                    field_name,
                    &field_descriptor,
                )?;

                // 3. Load the object reference onto the stack
                // Use object_actual_type (which must be a reference type) to get aload
                let object_var_index = self.get_local_index(object_var)?;
                let load_object_instr =
                    get_load_instruction(&object_actual_type, object_var_index)?;
                self.jvm_instructions.push(load_object_instr.clone()); // Stack: [object_ref]

                // 4. Load the value to be stored onto the stack
                self.load_operand(value)?; // Stack: [object_ref, value] (value size 1 or 2)

                // 5. Emit 'putfield' instruction
                self.jvm_instructions.push(JI::Putfield(field_ref_index)); // Stack: []
            }

            OI::GetField {
                dest,
                object_var,
                field_name,
                field_ty,    // Type of the field *value* being retrieved
                owner_class, // Class where the field is *defined*
            } => {
                // 1. Get the type of the object variable itself
                let object_actual_type = self.get_local_type(object_var)?.clone();

                if !object_actual_type.is_jvm_reference_type() {
                    return Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!(
                            "Variable '{}' used in GetField is not a reference type, found {:?}",
                            object_var, object_actual_type
                        ),
                    });
                }

                // 2. Add Field reference to constant pool (same as SetField)
                let owner_class_index = self.constant_pool.add_class(owner_class)?;
                let field_descriptor = field_ty.to_jvm_descriptor();
                let field_ref_index = self.constant_pool.add_field_ref(
                    owner_class_index,
                    field_name,
                    &field_descriptor,
                )?;

                // 3. Load the object reference onto the stack
                let object_var_index = self.get_local_index(object_var)?;
                let load_object_instr =
                    get_load_instruction(&object_actual_type, object_var_index)?;
                self.jvm_instructions.push(load_object_instr.clone()); // Stack: [object_ref]

                // 4. Emit 'getfield' instruction
                self.jvm_instructions.push(JI::Getfield(field_ref_index)); // Stack: [field_value] (size 1 or 2)

                // 5. Store the retrieved field value into the destination variable
                //    The type for storage is the field's type (field_ty)
                self.store_result(dest, field_ty)?; // Stack: []
            }
            OI::Cast { op, ty, dest } => {
                // 1. Load the operand on the stack
                self.load_operand(op)?; // Stack: [value]

                let old_ty = get_operand_type(op);

                // 2. Get cast instructions
                let instructions = get_cast_instructions(&old_ty, ty)?;

                // 3. Emit the cast instructions
                for instr in instructions {
                    self.jvm_instructions.push(instr);
                }

                // 4. Store the casted value into the destination variable
                //    The type for storage is the new type (ty)
                self.store_result(dest, ty)?; // Stack: []
            }
        }
        Ok(())
    }

    /// Helper to load an operand specifically for a function call argument.
    /// Handles the case where OOMIR indicates a Ref<Primitive> but we need to load the primitive.
    fn load_call_argument(&mut self, operand: &oomir::Operand) -> Result<(), jvm::Error> {
        match operand {
            oomir::Operand::Variable { name: var_name, ty } => {
                let index = self.get_local_index(var_name)?;
                // Decide which type to use for loading based on whether it's a Ref to Primitive
                let load_type = match ty {
                    // If the argument is declared as Ref<Primitive>, load the primitive directly
                    oomir::Type::Reference(inner_ty) if inner_ty.is_jvm_primitive() => {
                        inner_ty.as_ref() // Use the inner type for loading
                    }
                    // Otherwise, use the declared type
                    _ => ty,
                };
                let load_instr = get_load_instruction(load_type, index)?;
                self.jvm_instructions.push(load_instr);
            }
            oomir::Operand::Constant(c) => {
                // Constants are loaded directly, no special handling needed here for refs
                self.load_constant(c)?;
            }
        }
        Ok(())
    }
}
