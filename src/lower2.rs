// src/lower2.rs
//! This module converts OOMIR into JVM bytecode.

use crate::oomir;
use rustc_middle::ty::TyCtxt; // Assuming this is needed elsewhere, but not directly used in the core logic below.

use ristretto_classfile::attributes::{
    Attribute::{self, Code}, ExceptionTableEntry, Instruction, LineNumber, LocalVariableTable, MaxLocals,
    MaxStack, StackFrame, VerificationType,
};
use ristretto_classfile::{
    self as jvm, ClassAccessFlags, ClassFile, Constant, ConstantPool, FieldType, Method,
    MethodAccessFlags, Version,
};
use std::collections::{HashMap, VecDeque};
use std::convert::TryInto;

// --- Helper Structs ---

/// Represents the state during the translation of a single function's body.
struct FunctionTranslator<'a, 'cp> {
    module: &'a oomir::Module,
    oomir_func: &'a oomir::Function,
    constant_pool: &'cp mut ConstantPool,
    this_class_name: &'a str, // Class name for self-references if needed

    local_var_map: HashMap<String, u16>, // OOMIR var name -> JVM local index
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
    fn new(
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
            // Construct the MIR-style parameter name "_1", "_2", etc.
            let param_mir_name = format!("_{}", i + 1);
            let param_ty = &oomir_func.signature.params[i];

            // Use assign_local to allocate the correct slot(s) and update state
            // This correctly handles types taking 1 or 2 slots (like long/double)
            // and advances next_local_index.
            let assigned_index = translator.assign_local(param_mir_name.as_str(), param_ty);

            // Optional: Debug print to confirm mapping
             println!(
                 "Debug: Mapped parameter {} ({:?}) to JVM local starting at index {}",
                 param_mir_name, param_ty, assigned_index
             );
        }
        // `next_local_index` and `max_locals_used` are now correctly updated
        // by the assign_local calls within the loop.

         // Recalculate next_local_index based on parameters
        translator.next_local_index = translator.max_locals_used;


        translator
    }

    /// Assigns a JVM local variable slot to an OOMIR variable name.
    /// Handles types that take two slots (long, double).
    fn assign_local(&mut self, var_name: &str, ty: &oomir::Type) -> u16 {
        if let std::collections::hash_map::Entry::Vacant(e) = self.local_var_map.entry(var_name.to_string()) {
            let index = self.next_local_index;
            e.insert(index);
            let size = Self::get_type_size(ty);
            self.next_local_index += size;
            self.max_locals_used = self.max_locals_used.max(index + size);
            index
        } else {
            // Should ideally not happen if OOMIR uses SSA-like properties or unique names per scope
            self.local_var_map[var_name]
        }
    }

    /// Gets the slot index for a variable, assigning if new.
    /// *Important*: This assumes the type is known when assigning. It might be better
    /// to pre-scan or have type info available alongside variable names in OOMIR.
    /// For now, we'll try to infer type from context or default to size 1.
    fn get_or_assign_local(&mut self, var_name: &str, ty_hint: &oomir::Type) -> u16 {
         *self.local_var_map.entry(var_name.to_string()).or_insert_with(|| {
            let index = self.next_local_index;
            let size = Self::get_type_size(ty_hint);
            self.next_local_index += size;
            self.max_locals_used = self.max_locals_used.max(index + size);
            index
        })
    }

    fn get_local(&self, var_name: &str) -> Result<u16, jvm::Error> {
        self.local_var_map.get(var_name).copied().ok_or_else(|| {
            jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!("Undefined local variable used: {}", var_name),
            }
        })
    }

    /// Returns the number of JVM local variable slots a type occupies (1 or 2).
    fn get_type_size(ty: &oomir::Type) -> u16 {
        match ty {
            oomir::Type::I64 | oomir::Type::U64 | oomir::Type::F64 => 2,
            _ => 1,
        }
    }

    /// Translates the entire function body.
    fn translate(mut self) -> Result<(Vec<jvm::attributes::Instruction>, u16), jvm::Error> {
        // Use a worklist algorithm for potentially better handling of arbitrary CFGs
        let mut worklist: VecDeque<String> = VecDeque::new();
        let mut visited: HashMap<String, bool> = HashMap::new();

        worklist.push_back(self.oomir_func.body.entry.clone());
        visited.insert(self.oomir_func.body.entry.clone(), true);

        while let Some(block_label) = worklist.pop_front() {
            let block = self.oomir_func.body.basic_blocks.get(&block_label).ok_or_else(|| {
                jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!("Basic block label not found: {}", block_label),
                }
            })?;

            self.current_oomir_block_label = block_label.clone();

            // Record the start instruction index for this block label
            let start_instr_index = self.jvm_instructions.len().try_into().unwrap();
            self.label_to_instr_index.insert(block_label.clone(), start_instr_index);

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
                     oomir::Instruction::Branch { true_block, false_block, .. } => {
                         if visited.insert(true_block.clone(), true).is_none() {
                             worklist.push_back(true_block.clone());
                         }
                         if visited.insert(false_block.clone(), true).is_none() {
                             worklist.push_back(false_block.clone());
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
            let target_instr_index = *self.label_to_instr_index.get(&target_label)
                .ok_or_else(|| jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!("Branch target label not found: {}", target_label),
                })?;

             // Update the placeholder instruction
             match &mut self.jvm_instructions[instr_index] {
                Instruction::Goto(offset) | Instruction::Ifnull(offset) | Instruction::Ifnonnull(offset) |
                Instruction::Ifeq(offset) | Instruction::Ifne(offset) | Instruction::Iflt(offset) |
                Instruction::Ifge(offset) | Instruction::Ifgt(offset) | Instruction::Ifle(offset) |
                Instruction::If_icmpeq(offset) | Instruction::If_icmpne(offset) | Instruction::If_icmplt(offset) |
                Instruction::If_icmpge(offset) | Instruction::If_icmpgt(offset) | Instruction::If_icmple(offset) |
                Instruction::If_acmpeq(offset) | Instruction::If_acmpne(offset) => {
                    *offset = target_instr_index;
                }
                _ => {
                     return Err(jvm::Error::VerificationError {
                         context: format!("Function {}", self.oomir_func.name),
                         message: format!("Branch fixup expected a branch instruction at index {}", instr_index),
                     });
                 }
             }
        }

        Ok((self.jvm_instructions, self.max_locals_used))
    }

    /// Appends JVM instructions for loading an operand onto the stack.
    fn load_operand(&mut self, operand: &oomir::Operand, ty_hint: &oomir::Type) -> Result<(), jvm::Error> {
        match operand {
            oomir::Operand::Constant(c) => self.load_constant(c)?,
            oomir::Operand::Variable(var_name) => {
                let index = self.get_local(var_name)?;
                let load_instr = Self::get_load_instruction(ty_hint, index)?;
                self.jvm_instructions.push(load_instr);
            }
        }
        Ok(())
    }

     /// Appends JVM instructions for loading a constant onto the stack.
    fn load_constant(&mut self, constant: &oomir::Constant) -> Result<(), jvm::Error> {
        use oomir::Constant as OC;
        use jvm::attributes::Instruction as JI;

        let instr = match constant {
            OC::I8(v) => Self::get_int_const_instr(*v as i32),
            OC::I16(v) => Self::get_int_const_instr(*v as i32),
            OC::I32(v) => Self::get_int_const_instr(*v),
            OC::I64(v) => Self::get_long_const_instr(*v),
            OC::U8(v) => Self::get_int_const_instr(*v as i32), // Treat as signed int
            OC::U16(v) => Self::get_int_const_instr(*v as i32), // Treat as signed int
            OC::U32(v) => Self::get_int_const_instr(*v as i32), // Treat as signed int
            OC::U64(v) => Self::get_long_const_instr(*v as i64), // Treat as signed long
            OC::F32(v) => Self::get_float_const_instr(*v),
            OC::F64(v) => Self::get_double_const_instr(*v),
            OC::Boolean(v) => if *v { JI::Iconst_1 } else { JI::Iconst_0 },
            OC::Char(v) => Self::get_int_const_instr(*v as i32), // Char treated as int
            OC::String(s) => {
                let index = self.constant_pool.add_string(s)?;
                if let Ok(idx8) = u8::try_from(index) {
                    JI::Ldc(idx8)
                } else {
                    JI::Ldc_w(index)
                }
            }
         };
         self.jvm_instructions.push(instr.clone());
         // Handle Ldc2_w for long/double constants loaded via LDC
         if matches!(instr, JI::Ldc2_w(_)) {
             // Ldc2_w pushes a 64-bit value (2 stack words)
         }
         Ok(())
    }

    /// Appends JVM instructions for storing the value currently on top of the stack
    /// into a local variable.
    fn store_result(&mut self, dest_var: &str, ty: &oomir::Type) -> Result<(), jvm::Error> {
        let index = self.get_or_assign_local(dest_var, ty);
        let store_instr = Self::get_store_instruction(ty, index)?;
        self.jvm_instructions.push(store_instr);
        Ok(())
    }

    // Helper to get the appropriate integer constant loading instruction
    fn get_int_const_instr(val: i32) -> Instruction {
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

            // --- Fallback with Clamping ---
            // Values outside the -32768 to 32767 range
            mut v => { // Make v mutable for potential clamping
                let original_val = v; // Store original value for warning message

                if v > i16::MAX as i32 {
                    println!(
                        "Warning: Integer constant {} is outside sipush range (> {}). Clamping to {}. LDC support not implemented.",
                        original_val, i16::MAX, i16::MAX
                    );
                    v = i16::MAX as i32; // Clamp down to max i16
                } else { // v must be < i16::MIN
                    println!(
                        "Warning: Integer constant {} is outside sipush range (< {}). Clamping to {}. LDC support not implemented.",
                        original_val, i16::MIN, i16::MIN
                    );
                    v = i16::MIN as i32; // Clamp up to min i16
                }
                // Use sipush with the clamped value
                Instruction::Sipush(v as i16)
            }
        }
    }
    
    // Helper to get the appropriate long constant loading instruction
    fn get_long_const_instr(val: i64) -> Instruction {
        match val {
            0 => Instruction::Lconst_0,
            1 => Instruction::Lconst_1,
             _ => unimplemented!("Ldc2_w for i64 not implemented via ConstantPool yet"), // Need constant_pool.add_long
        }
    }

     // Helper to get the appropriate float constant loading instruction
    fn get_float_const_instr(val: f32) -> Instruction {
         if val == 0.0 { Instruction::Fconst_0 }
         else if val == 1.0 { Instruction::Fconst_1 }
         else if val == 2.0 { Instruction::Fconst_2 }
         else { unimplemented!("Ldc for f32 not implemented via ConstantPool yet") } // Need constant_pool.add_float
    }

    // Helper to get the appropriate double constant loading instruction
    fn get_double_const_instr(val: f64) -> Instruction {
        if val == 0.0 { Instruction::Dconst_0 }
        else if val == 1.0 { Instruction::Dconst_1 }
        else { unimplemented!("Ldc2_w for f64 not implemented via ConstantPool yet") } // Need constant_pool.add_double
    }


    /// Gets the appropriate type-specific load instruction.
    fn get_load_instruction(ty: &oomir::Type, index: u16) -> Result<Instruction, jvm::Error> {
        let index_u8: u8 = index.try_into().unwrap();
        Ok(match ty {
            oomir::Type::I8 | oomir::Type::I16 | oomir::Type::I32 |
            oomir::Type::U8 | oomir::Type::U16 | oomir::Type::U32 |
            oomir::Type::Boolean | oomir::Type::Char => match index {
                0 => Instruction::Iload_0,
                1 => Instruction::Iload_1,
                2 => Instruction::Iload_2,
                3 => Instruction::Iload_3,
                _ => Instruction::Iload(index_u8),
            },
            oomir::Type::I64 | oomir::Type::U64 => match index {
                0 => Instruction::Lload_0,
                1 => Instruction::Lload_1,
                2 => Instruction::Lload_2,
                3 => Instruction::Lload_3,
                _ => Instruction::Lload(index_u8),
            },
            oomir::Type::F32 => match index {
                0 => Instruction::Fload_0,
                1 => Instruction::Fload_1,
                2 => Instruction::Fload_2,
                3 => Instruction::Fload_3,
                _ => Instruction::Fload(index_u8),
            },
            oomir::Type::F64 => match index {
                0 => Instruction::Dload_0,
                1 => Instruction::Dload_1,
                2 => Instruction::Dload_2,
                3 => Instruction::Dload_3,
                _ => Instruction::Dload(index_u8),
            },
            oomir::Type::Reference(_) | oomir::Type::Array(_) |
            oomir::Type::String | oomir::Type::Class(_) => match index {
                0 => Instruction::Aload_0,
                1 => Instruction::Aload_1,
                2 => Instruction::Aload_2,
                3 => Instruction::Aload_3,
                _ => Instruction::Aload(index_u8),
            },
            oomir::Type::Void => return Err(jvm::Error::VerificationError{ context: "get_load_instruction".to_string(), message:"Cannot load void type".to_string()}),
        })
    }

    /// Gets the appropriate type-specific store instruction.
    fn get_store_instruction(ty: &oomir::Type, index: u16) -> Result<Instruction, jvm::Error> {
         let index_u8: u8 = index.try_into().unwrap(); // TODO: Handle wide instructions if index > 255
        Ok(match ty {
            oomir::Type::I8 | oomir::Type::I16 | oomir::Type::I32 |
            oomir::Type::U8 | oomir::Type::U16 | oomir::Type::U32 |
            oomir::Type::Boolean | oomir::Type::Char => match index {
                0 => Instruction::Istore_0,
                1 => Instruction::Istore_1,
                2 => Instruction::Istore_2,
                3 => Instruction::Istore_3,
                _ => Instruction::Istore(index_u8),
            },
            oomir::Type::I64 | oomir::Type::U64 => match index {
                0 => Instruction::Lstore_0,
                1 => Instruction::Lstore_1,
                2 => Instruction::Lstore_2,
                3 => Instruction::Lstore_3,
                _ => Instruction::Lstore(index_u8),
            },
            oomir::Type::F32 => match index {
                0 => Instruction::Fstore_0,
                1 => Instruction::Fstore_1,
                2 => Instruction::Fstore_2,
                3 => Instruction::Fstore_3,
                _ => Instruction::Fstore(index_u8),
            },
            oomir::Type::F64 => match index {
                0 => Instruction::Dstore_0,
                1 => Instruction::Dstore_1,
                2 => Instruction::Dstore_2,
                3 => Instruction::Dstore_3,
                _ => Instruction::Dstore(index_u8),
            },
            oomir::Type::Reference(_) | oomir::Type::Array(_) |
            oomir::Type::String | oomir::Type::Class(_) => match index {
                0 => Instruction::Astore_0,
                1 => Instruction::Astore_1,
                2 => Instruction::Astore_2,
                3 => Instruction::Astore_3,
                _ => Instruction::Astore(index_u8),
            },
             oomir::Type::Void => return Err(jvm::Error::VerificationError{ context: "get_store_instruction".to_string(), message:"Cannot store void type".to_string()}),
        })
    }


    // --- Instruction Translation Helpers ---

    fn translate_binary_op(&mut self, dest: &str, op1: &oomir::Operand, op2: &oomir::Operand, op_type: &oomir::Type, jvm_op: Instruction) -> Result<(), jvm::Error> {
        self.load_operand(op1, op_type)?;
        self.load_operand(op2, op_type)?;
        self.jvm_instructions.push(jvm_op);
        self.store_result(dest, op_type)?;
        Ok(())
    }

    // Translate comparison operations (Eq, Ne, Lt, etc.)
    // Pushes 1 if true, 0 if false onto the stack, then stores it in `dest`.
    // Uses conditional branches.
    fn translate_comparison_op(
        &mut self,
        dest: &str,
        op1: &oomir::Operand,
        op2: &oomir::Operand,
        op_type: &oomir::Type, // Type of operands being compared
        jump_op_if_true: fn(u16) -> Instruction, // e.g., Instruction::If_icmpeq for Eq
    ) -> Result<(), jvm::Error> {
        self.load_operand(op1, op_type)?;
        self.load_operand(op2, op_type)?;

        let instr_idx_if = self.jvm_instructions.len();
        let label_true = format!("_comparison_true_{}", instr_idx_if);
        let label_after = format!("_comparison_after_{}", instr_idx_if);

        // Add placeholder for the conditional jump to the 'true' case
        self.jvm_instructions.push(jump_op_if_true(0)); // Placeholder offset
        self.branch_fixups.push((instr_idx_if, label_true.clone()));

        // False case: push 0
        self.jvm_instructions.push(Instruction::Iconst_0);
        let instr_idx_goto_after = self.jvm_instructions.len();
        self.jvm_instructions.push(Instruction::Goto(0)); // Placeholder offset
        self.branch_fixups.push((instr_idx_goto_after, label_after.clone()));

        // True case: push 1 (record label first)
        let true_instr_index = self.jvm_instructions.len().try_into().unwrap();
        self.label_to_instr_index.insert(label_true, true_instr_index);
        self.jvm_instructions.push(Instruction::Iconst_1);

        // After case: store the result (0 or 1)
        let after_instr_index = self.jvm_instructions.len().try_into().unwrap();
        self.label_to_instr_index.insert(label_after, after_instr_index);
        self.store_result(dest, &oomir::Type::Boolean)?; // Comparison result is always boolean

        Ok(())
    }


    /// Translates a single OOMIR instruction and appends the corresponding JVM instructions.
    #[allow(clippy::too_many_lines)]
    fn translate_instruction(&mut self, module: &oomir::Module, instr: &oomir::Instruction) -> Result<(), jvm::Error> {
        use oomir::Instruction as OI;
        use jvm::attributes::Instruction as JI;

        // TODO: Infer types properly. This is a major simplification.
        // We need type information for variables to select the correct JVM opcodes.
        // Assuming I32 for many operations for now based on the example.
        let default_type = oomir::Type::I32;

        match instr {
            OI::Const { dest, value } => {
                let ty = match value {
                     oomir::Constant::I8(_) | oomir::Constant::U8(_) => oomir::Type::I8,
                     oomir::Constant::I16(_) | oomir::Constant::U16(_) => oomir::Type::I16,
                     oomir::Constant::I32(_) | oomir::Constant::U32(_) => oomir::Type::I32,
                     oomir::Constant::I64(_) | oomir::Constant::U64(_) => oomir::Type::I64,
                     oomir::Constant::F32(_) => oomir::Type::F32,
                     oomir::Constant::F64(_) => oomir::Type::F64,
                     oomir::Constant::Boolean(_) => oomir::Type::Boolean,
                     oomir::Constant::Char(_) => oomir::Type::Char,
                     oomir::Constant::String(_) => oomir::Type::String,
                };
                self.load_constant(value)?;
                self.store_result(dest, &ty)?;
            }
            OI::Add { dest, op1, op2 } => self.translate_binary_op(dest, op1, op2, &default_type, JI::Iadd)?,
            OI::Sub { dest, op1, op2 } => self.translate_binary_op(dest, op1, op2, &default_type, JI::Isub)?,
            OI::Mul { dest, op1, op2 } => self.translate_binary_op(dest, op1, op2, &default_type, JI::Imul)?,
            OI::Div { dest, op1, op2 } => self.translate_binary_op(dest, op1, op2, &default_type, JI::Idiv)?, // Handle division by zero?
            OI::Rem { dest, op1, op2 } => self.translate_binary_op(dest, op1, op2, &default_type, JI::Irem)?,

            // --- Comparisons ---
            // Need to handle operand types (int, long, float, double, ref)
            OI::Eq { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, &default_type, JI::If_icmpeq)?,
            OI::Ne { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, &default_type, JI::If_icmpne)?,
            OI::Lt { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, &default_type, JI::If_icmplt)?,
            OI::Le { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, &default_type, JI::If_icmple)?,
            OI::Gt { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, &default_type, JI::If_icmpgt)?,
            OI::Ge { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, &default_type, JI::If_icmpge)?,

            // --- Bitwise Operations ---
            OI::BitAnd { dest, op1, op2 } => self.translate_binary_op(dest, op1, op2, &default_type, JI::Iand)?,
            OI::BitOr { dest, op1, op2 } => self.translate_binary_op(dest, op1, op2, &default_type, JI::Ior)?,
            OI::BitXor { dest, op1, op2 } => self.translate_binary_op(dest, op1, op2, &default_type, JI::Ixor)?,
            OI::Shl { dest, op1, op2 } => self.translate_binary_op(dest, op1, op2, &default_type, JI::Ishl)?,
            OI::Shr { dest, op1, op2 } => self.translate_binary_op(dest, op1, op2, &default_type, JI::Ishr)?, // Assuming signed shr, needs checking

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
            OI::And { dest, op1, op2 } => unimplemented!("Logical And needs control flow translation"),
            OI::Or { dest, op1, op2 } => unimplemented!("Logical Or needs control flow translation"),


            // --- Control Flow ---
            OI::Jump { target } => {
                let instr_index = self.jvm_instructions.len();
                self.jvm_instructions.push(JI::Goto(0)); // Placeholder
                self.branch_fixups.push((instr_index, target.clone()));
            }
            OI::Branch { condition, true_block, false_block } => {
                // 1. Load the condition (must evaluate to int 0 or 1)
                self.load_operand(condition, &oomir::Type::Boolean)?; // Assuming boolean type

                // 2. Add conditional jump (if condition != 0, jump to true_block)
                let instr_idx_ifne = self.jvm_instructions.len();
                self.jvm_instructions.push(JI::Ifne(0)); // Placeholder (If Not Equal to zero)
                self.branch_fixups.push((instr_idx_ifne, true_block.clone()));

                // 3. Add unconditional jump to false_block (this is the fallthrough if condition == 0)
                let instr_idx_goto_false = self.jvm_instructions.len();
                self.jvm_instructions.push(JI::Goto(0)); // Placeholder
                self.branch_fixups.push((instr_idx_goto_false, false_block.clone()));

            }
            OI::Return { operand } => {
                 match operand {
                    Some(op) => {
                        // Determine type based on function signature's return type
                        let ret_ty = &self.oomir_func.signature.ret;
                        self.load_operand(op, ret_ty)?;
                        let return_instr = match **ret_ty {
                            oomir::Type::I8 | oomir::Type::I16 | oomir::Type::I32 |
                            oomir::Type::U8 | oomir::Type::U16 | oomir::Type::U32 |
                            oomir::Type::Boolean | oomir::Type::Char => JI::Ireturn,
                            oomir::Type::I64 | oomir::Type::U64 => JI::Lreturn,
                            oomir::Type::F32 => JI::Freturn,
                            oomir::Type::F64 => JI::Dreturn,
                             oomir::Type::Reference(_) | oomir::Type::Array(_) |
                             oomir::Type::String | oomir::Type::Class(_) => JI::Areturn,
                             oomir::Type::Void => JI::Return, // Should not happen with Some(op)
                        };
                        self.jvm_instructions.push(return_instr);
                    }
                    None => {
                        self.jvm_instructions.push(JI::Return);
                    }
                }
            }
             OI::Call { dest, function, args } => {
                 // Find the function signature (assuming it's in the same module for now)
                 // In a real compiler, you'd look up the function globally.
                let target_func = self.oomir_func.body.basic_blocks.values() // Hacky: Need global function lookup
                                    .flat_map(|b| &b.instructions)
                                    .find_map(|i| if let OI::Call { function: f, .. } = i { Some(f) } else { None })
                                    .and_then(|_| Some(self.oomir_func)); // Reuse current func sig if name matches - VERY BAD assumption


                let target_sig = if function == &self.oomir_func.name { // Self-call?
                     &self.oomir_func.signature
                 } else {
                     // HACK: Need proper function lookup. Find *any* function with that name in the module for now.
                     self.oomir_func.body.basic_blocks.values()
                         .find_map(|_| { // Find the function signature from the module functions map
                             module.functions.get(function).map(|f| &f.signature)
                         })
                         .ok_or_else(|| jvm::Error::VerificationError {
                             context: format!("Function {}", self.oomir_func.name),
                             message: format!("Cannot find signature for called function '{}'", function),
                         })?
                 };


                 // 1. Load arguments onto the stack
                if args.len() != target_sig.params.len() {
                     return Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!("Argument count mismatch calling function '{}'", function),
                     });
                }
                for (arg, param_ty) in args.iter().zip(target_sig.params.iter()) {
                    self.load_operand(arg, param_ty)?;
                 }

                 // 2. Add MethodRef to constant pool
                 // Assuming static calls within the same class for now
                let class_index = self.constant_pool.add_class(self.this_class_name)?; // Assume call within same class
                let method_ref_index = self.constant_pool.add_method_ref(
                                     class_index,
                                     function.clone(),
                                     target_sig.to_string(),
                                 )?;

                 // 3. Add invokestatic instruction
                 self.jvm_instructions.push(JI::Invokestatic(method_ref_index));

                 // 4. Store result if 'dest' is Some
                 if let Some(dest_var) = dest {
                     if *target_sig.ret != oomir::Type::Void {
                         self.store_result(dest_var, &target_sig.ret)?;
                     } else if dest.is_some() {
                          return Err(jvm::Error::VerificationError {
                              context: format!("Function {}", self.oomir_func.name),
                              message: format!("Attempting to store void result from function '{}'", function),
                          });
                     }
                 }
             }
             OI::Move { dest, src } => {
                 // Assuming types are compatible or known. Need type info.
                self.load_operand(src, &default_type)?;
                self.store_result(dest, &default_type)?;
             }
            /*  OI::Throw { message } => {
                 // 1. Create a new RuntimeException object
                 let runtime_exception_class_index = self.constant_pool.add_class("java/lang/RuntimeException")?;
                 let new_instr = JI::New(runtime_exception_class_index);
                 self.jvm_instructions.push(new_instr);
                 self.jvm_instructions.push(JI::Dup); // Duplicate the reference for the constructor call

                 // 2. Load the message string onto the stack
                 let string_const_index = self.constant_pool.add_string(message)?;
                  if let Ok(idx8) = u8::try_from(string_const_index) {
                    self.jvm_instructions.push(JI::Ldc(idx8));
                  } else {
                    self.jvm_instructions.push(JI::Ldc_w(string_const_index));
                  }


                 // 3. Call the RuntimeException constructor <init>(String)
                 let constructor_ref_index = self.constant_pool.add_method_ref(
                     runtime_exception_class_index,
                     "<init>",
                     "(Ljava/lang/String;)V",
                 )?;
                 self.jvm_instructions.push(JI::Invokespecial(constructor_ref_index));

                 // 4. Throw the exception
                 self.jvm_instructions.push(JI::Athrow);
             } */

        }
        Ok(())
    }
}


// --- Main Conversion Function ---

/// Converts an OOMIR module into a JVM class file (as a byte vector).
pub fn oomir_to_jvm_bytecode(
    module: &oomir::Module,
    _tcx: TyCtxt, // Keep tcx in signature if needed later, but unused now
) -> jvm::Result<Vec<u8>> {
    let mut constant_pool = ConstantPool::default();
    let super_class_index = constant_pool.add_class("java/lang/Object")?;
    let this_class_index = constant_pool.add_class(&module.name)?;
    let code_attribute_name_index = constant_pool.add_utf8("Code")?;

    let mut methods: Vec<jvm::Method> = Vec::new();
    let mut has_constructor = false;

    for function in module.functions.values() {
        if function.name == "<init>" {
            has_constructor = true;
            // TODO: Translate the provided constructor if necessary.
            // For now, we'll rely on the default one if none is explicit.
            // If an explicit one exists, it MUST call a super constructor.
        }

        let name_index = constant_pool.add_utf8(&function.name)?;
        let descriptor_index = constant_pool.add_utf8(&function.signature.to_string())?;

        // Translate the function body to JVM instructions
        let translator = FunctionTranslator::new(function, &mut constant_pool, &module.name, module);
        let (jvm_code, max_locals_val) = translator.translate()?;


        // Calculate max_stack
        let max_stack_val = jvm_code.max_stack(&constant_pool)?;
        // max_locals is calculated during translation

        // Create the Code attribute
        let code_attribute = Attribute::Code {
            name_index: code_attribute_name_index,
            max_stack: max_stack_val,
            max_locals: max_locals_val,
            code: jvm_code,
            exception_table: Vec::new(), // TODO: Populate if OOMIR supports exceptions
            attributes: Vec::new(),     // TODO: Add LineNumberTable, etc. if needed
        };

        let mut method = jvm::Method::default();
        method.access_flags = MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC; // Assuming all are public static for now
        method.name_index = name_index;
        method.descriptor_index = descriptor_index;
        method.attributes.push(code_attribute);

        methods.push(method);
    }

    // Add a default constructor if none was found
    if !has_constructor {
        methods.push(create_default_constructor(&mut constant_pool, super_class_index)?);
    }

    let mut class_file = ClassFile {
        // Java 6 so we don't need stack map tables! :)
        version: Version::Java6 { minor: 0 },
        constant_pool, // Will be moved
        access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
        this_class: this_class_index,
        super_class: super_class_index,
        interfaces: Vec::new(),
        fields: Vec::new(), // Assuming no fields for now
        methods,
        attributes: Vec::new(), // Add SourceFile attribute?
    };

    // Add SourceFile attribute (optional but good practice)
    let source_file_name = format!("{}.rs", module.name); // Or derive from actual source
    let source_file_utf8_index = class_file.constant_pool.add_utf8(&source_file_name)?;
    let source_file_attr_name_index = class_file.constant_pool.add_utf8("SourceFile")?;
    class_file.attributes.push(Attribute::SourceFile {
        name_index: source_file_attr_name_index,
        source_file_index: source_file_utf8_index,
    });


    // Serialize the class file to bytes
    let mut byte_vector = Vec::new();
    class_file.to_bytes(&mut byte_vector)?;
    Ok(byte_vector)
}

/// Creates a default constructor `<init>()V` that just calls `super()`.
fn create_default_constructor(
    cp: &mut ConstantPool,
    super_class_index: u16,
) -> jvm::Result<jvm::Method> {
    let code_attr_name_index = cp.add_utf8("Code")?;
    let init_name_index = cp.add_utf8("<init>")?;
    let init_desc_index = cp.add_utf8("()V")?;

    // Add reference to super.<init>()V
    let super_init_ref_index = cp.add_method_ref(super_class_index, "<init>", "()V")?;

    let instructions = vec![
        Instruction::Aload_0, // Load 'this'
        Instruction::Invokespecial(super_init_ref_index), // Call super()
        Instruction::Return,
    ];

    let max_stack = instructions.max_stack(cp)?; // Should be 1
    let max_locals = 1; // Just 'this'

    let code_attribute = Attribute::Code {
        name_index: code_attr_name_index,
        max_stack,
        max_locals,
        code: instructions,
        exception_table: Vec::new(),
        attributes: Vec::new(), // No inner attributes like LineNumberTable needed for default
    };

    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC, // Default constructor is public
        name_index: init_name_index,
        descriptor_index: init_desc_index,
        attributes: vec![code_attribute],
    })
}
