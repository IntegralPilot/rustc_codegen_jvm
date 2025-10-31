use super::{
    consts::{get_int_const_instr, load_constant},
    helpers::{
        get_cast_instructions, get_load_instruction, get_operand_type, get_store_instruction,
        get_type_size, parse_jvm_descriptor_params,
    },
    shim::get_shim_metadata,
};
use crate::oomir::{self, Type};

use ristretto_classfile::attributes::{ArrayType, Instruction};
use ristretto_classfile::{self as jvm, ConstantPool};
use std::collections::{HashMap, VecDeque};
use std::convert::TryInto;

use super::{BIG_DECIMAL_CLASS, BIG_INTEGER_CLASS};

/// Represents the state during the translation of a single function's body.
pub struct FunctionTranslator<'a, 'cp> {
    module: &'a oomir::Module,
    oomir_func: &'a oomir::Function,
    constant_pool: &'cp mut ConstantPool,

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
        module: &'a oomir::Module,
        is_static: bool,
    ) -> Self {
        let mut translator = FunctionTranslator {
            oomir_func,
            module,
            constant_pool,
            local_var_map: HashMap::new(),
            local_var_types: HashMap::new(),
            next_local_index: if is_static { 0 } else { 1 },
            jvm_instructions: Vec::new(),
            label_to_instr_index: HashMap::new(),
            branch_fixups: Vec::new(),
            current_oomir_block_label: String::new(),
            max_locals_used: 0,
        };

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!("static: {}, function_name: {}", is_static, oomir_func.name)
        );

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
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "bytecode-gen",
                    format!(
                        "Warning: OOMIR parameter name '{}' potentially clashed with an existing mapping during parameter assignment.",
                        param_oomir_name
                    )
                );
            }
            if translator
                .local_var_types
                .insert(param_oomir_name.clone(), param_ty.clone())
                .is_some()
            {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "bytecode-gen",
                    format!(
                        "Warning: OOMIR parameter name '{}' potentially clashed with an existing type mapping during parameter assignment.",
                        param_oomir_name
                    )
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
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "bytecode-gen",
                    format!(
                        "Internal Warning: Type map already had entry for supposedly new local '{}'",
                        var_name
                    )
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
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "bytecode-gen",
                format!(
                    "Warning: assign_local called for existing variable '{}' (index {}). Reusing index.",
                    var_name, existing_index
                )
            );
            // Should we verify type consistency here too? Probably.
            if let Some(existing_ty) = self.local_var_types.get(var_name) {
                if existing_ty != ty {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "bytecode-gen",
                        format!(
                            "   -> CRITICAL WARNING: assign_local type mismatch for '{}'! Existing: {:?}, New: {:?}. Keeping existing index but this indicates a flaw!",
                            var_name, existing_ty, ty
                        )
                    );
                }
            }
            existing_index // Return existing index, but flag this as problematic
        }
    }

    /// Gets the slot index for a variable, assigning if new.
    fn get_or_assign_local(&mut self, var_name: &str, ty_hint: &oomir::Type) -> u16 {
        if let Some(existing_index) = self.local_var_map.get(var_name).copied() {
            // Check if the existing type matches the hint. If not, update it.
            if let Some(existing_ty) = self.local_var_types.get(var_name) {
                if existing_ty != ty_hint {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "bytecode-gen",
                        format!(
                            "Warning: Reusing local '{}' (index {}) with new type {:?} (previous: {:?}). Updating type.",
                            var_name, existing_index, ty_hint, existing_ty
                        )
                    );
                    // Update the type in the map
                    self.local_var_types
                        .insert(var_name.to_string(), ty_hint.clone());
                    // Re-check if max_locals_used needs update due to size change
                    let size = get_type_size(ty_hint);
                    self.max_locals_used = self.max_locals_used.max(existing_index + size);
                } else {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "bytecode-gen",
                        format!(
                            "Debug: Reusing local '{}' (index {}) with same type hint {:?}.",
                            var_name, existing_index, ty_hint
                        )
                    );
                }
            } else {
                // This case is unlikely if index exists, but handle defensively
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "bytecode-gen",
                    format!(
                        "Warning: Local '{}' has index {} but no type in map. Assigning type {:?}.",
                        var_name, existing_index, ty_hint
                    )
                );
                self.local_var_types
                    .insert(var_name.to_string(), ty_hint.clone());
                let size = get_type_size(ty_hint);
                self.max_locals_used = self.max_locals_used.max(existing_index + size);
            }
            existing_index
        } else {
            // Variable is genuinely new. Use assign_local to get a fresh slot.
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "bytecode-gen",
                format!(
                    "Debug: Assigning new local '{}' (type {:?})",
                    var_name, ty_hint
                )
            );
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

    /// Parses a variable name like "_1" into its numeric index, if applicable.
    #[allow(dead_code)]
    fn parse_local_index(var_name: &str) -> Option<u16> {
        if let Some(rest) = var_name.strip_prefix('_') {
            if let Ok(n) = rest.parse::<u16>() {
                return Some(n);
            }
        }
        None
    }

    /// Convenience wrapper to parse argument-like local indices (e.g., "_1").
    #[allow(dead_code)]
    fn parse_arg_index(var_name: &str) -> Option<u16> {
        Self::parse_local_index(var_name)
    }

    /// Appends JVM instructions for loading an operand onto the stack.
    fn load_operand(&mut self, operand: &oomir::Operand) -> Result<(), jvm::Error> {
        match operand {
            oomir::Operand::Constant(c) => {
                load_constant(&mut self.jvm_instructions, &mut self.constant_pool, c)?
            }
            oomir::Operand::Variable { name: var_name, ty } => {
                // Use the provided type directly - OOMIR variables should have correct types
                let index = self.get_or_assign_local(var_name, ty);
                let load_instr = get_load_instruction(ty, index)?;
                self.jvm_instructions.push(load_instr);
            }
        }
        Ok(())
    }

    /// Appends JVM instructions for storing the value currently on top of the stack
    /// into a local variable.
    fn store_result(&mut self, dest_var: &str, ty: &oomir::Type) -> Result<(), jvm::Error> {
        // Use the provided type directly - OOMIR variables should have correct types
        let index: u16 = self.get_or_assign_local(dest_var, ty);
        let store_instr = get_store_instruction(ty, index)?;
        self.jvm_instructions.push(store_instr);
        Ok(())
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

    /// Determines the common comparison type based on numeric promotion rules,
    /// including BigInteger and BigDecimal. Also returns necessary cast targets.
    fn determine_comparison_type(
        &self, // Keep self if error reporting needs function context
        op1_type: &oomir::Type,
        op2_type: &oomir::Type,
    ) -> Result<(oomir::Type, Option<oomir::Type>, Option<oomir::Type>), jvm::Error> {
        // Helper to check if a type is BigInteger or BigDecimal
        let is_big_type = |ty: &Type| {
            matches!(ty,
            Type::Class(c) if c == BIG_INTEGER_CLASS || c == BIG_DECIMAL_CLASS)
        };

        // Helper to check if a type is numeric primitive or boolean/char
        let is_promotable_primitive = |ty: &Type| {
            matches!(
                ty,
                Type::I8
                    | Type::I16
                    | Type::I32
                    | Type::I64
                    | Type::F32
                    | Type::F64
                    | Type::Boolean
                    | Type::Char
            )
        };

        match (op1_type, op2_type) {
            // --- Both are the same type ---
            (t1, t2) if t1 == t2 => {
                // Check if the type itself is comparable
                match t1 {
                    Type::I8
                    | Type::I16
                    | Type::I32
                    | Type::I64
                    | Type::F32
                    | Type::F64
                    | Type::Boolean
                    | Type::Char
                    | Type::Class(_)
                    | Type::Interface(_)
                    | Type::String
                    | Type::Reference(_)
                    | Type::MutableReference(_)
                    | Type::Array(_) => Ok((t1.clone(), None, None)), // Assume comparable for now, specific logic in main function handles details
                    Type::Void => Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!("Cannot compare void types"),
                    }),
                }
            }

            // --- BigInteger / BigDecimal Handling ---
            (Type::Class(c1), t2) if c1 == BIG_INTEGER_CLASS && is_promotable_primitive(t2) => {
                Ok((op1_type.clone(), None, Some(op1_type.clone())))
            } // Promote primitive to BigInt
            (t1, Type::Class(c2)) if c2 == BIG_INTEGER_CLASS && is_promotable_primitive(t1) => {
                Ok((op2_type.clone(), Some(op2_type.clone()), None))
            } // Promote primitive to BigInt

            (Type::Class(c1), t2) if c1 == BIG_DECIMAL_CLASS && is_promotable_primitive(t2) => {
                Ok((op1_type.clone(), None, Some(op1_type.clone())))
            } // Promote primitive to BigDec
            (t1, Type::Class(c2)) if c2 == BIG_DECIMAL_CLASS && is_promotable_primitive(t1) => {
                Ok((op2_type.clone(), Some(op2_type.clone()), None))
            } // Promote primitive to BigDec

            // Prevent comparing BigInt with BigDec directly (require explicit cast in source)
            (Type::Class(c1), Type::Class(c2))
                if (c1 == BIG_INTEGER_CLASS && c2 == BIG_DECIMAL_CLASS)
                    || (c1 == BIG_DECIMAL_CLASS && c2 == BIG_INTEGER_CLASS) =>
            {
                Err(jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!(
                        "Cannot directly compare BigInteger and BigDecimal. Cast one operand explicitly."
                    ),
                })
            }

            // Prevent comparing Big types with other non-primitive reference types for now
            (t1, t2)
                if (is_big_type(t1) && !is_promotable_primitive(t2) && !is_big_type(t2))
                    || (is_big_type(t2) && !is_promotable_primitive(t1) && !is_big_type(t1)) =>
            {
                Err(jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!(
                        "Cannot compare BigInteger/BigDecimal with non-primitive type: {:?} vs {:?}",
                        op1_type, op2_type
                    ),
                })
            }

            // --- Numeric Primitive Promotion Rules ---
            (t1, t2) if is_promotable_primitive(t1) && is_promotable_primitive(t2) => {
                // Determine target type based on promotion rules
                let target_type = if t1 == &Type::F64 || t2 == &Type::F64 {
                    Type::F64
                } else if t1 == &Type::F32 || t2 == &Type::F32 {
                    Type::F32
                } else if t1 == &Type::I64 || t2 == &Type::I64 {
                    Type::I64
                } else {
                    Type::I32
                }; // Promote smaller ints/bool/char to I32

                let cast1 = if t1 != &target_type {
                    Some(target_type.clone())
                } else {
                    None
                };
                let cast2 = if t2 != &target_type {
                    Some(target_type.clone())
                } else {
                    None
                };
                Ok((target_type, cast1, cast2))
            }

            // --- Reference Type Equality (only eq/ne) ---
            // Handled by the t1 == t2 case for simplicity, but could be explicit:
            (t1, t2) if t1.is_jvm_reference_type() && t2.is_jvm_reference_type() => {
                // Allow comparison if types are compatible (e.g. String vs String, MyClass vs MyClass)
                // For now, require exact match for simplicity. Could potentially allow subclass checks later.
                if t1 == t2 {
                    Ok((t1.clone(), None, None)) // Compare as references
                } else {
                    Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!(
                            "Cannot compare incompatible reference types: {:?} vs {:?}",
                            op1_type, op2_type
                        ),
                    })
                }
            }

            // --- Fallback for other unsupported combinations ---
            _ => Err(jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!(
                    "Unsupported comparison between types: {:?} and {:?}",
                    op1_type, op2_type
                ),
            }),
        }
    }

    fn get_local_type(&self, var_name: &str) -> Result<&oomir::Type, jvm::Error> {
        self.local_var_types
            .get(var_name)
            .ok_or_else(|| jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!("Undefined local variable type requested for: {}", var_name),
            })
    }

    fn translate_comparison_op(
        &mut self,
        dest: &str,
        op1: &oomir::Operand,
        op2: &oomir::Operand,
        comp_op: &str, // "eq", "ne", "lt", "le", "gt", "ge"
    ) -> Result<(), jvm::Error> {
        let op1_type = get_operand_type(op1);
        let op2_type = get_operand_type(op2);

        // Determine the type to compare operands as, and if casting is needed
        let (comparison_type, cast1_target, cast2_target) =
            self.determine_comparison_type(&op1_type, &op2_type)?;

        // --- Load and Cast Operands ---
        self.load_operand(op1)?;
        if let Some(target_type) = cast1_target {
            // Use the enhanced casting helper which needs the constant pool
            let cast_instrs =
                get_cast_instructions(&op1_type, &target_type, &mut self.constant_pool)?;
            self.jvm_instructions.extend(cast_instrs);
        }

        self.load_operand(op2)?;
        if let Some(target_type) = cast2_target {
            let cast_instrs =
                get_cast_instructions(&op2_type, &target_type, &mut self.constant_pool)?;
            self.jvm_instructions.extend(cast_instrs);
        }
        // Stack now holds: [value1_promoted, value2_promoted] (both of comparison_type)

        // --- Perform Comparison based on comparison_type ---
        let branch_constructor: Box<dyn Fn(u16) -> Instruction>;
        //let is_reference_comparison = comparison_type.is_jvm_reference_type();

        match comparison_type {
            // Integer types (I32 includes promoted I8, I16, Char, Boolean)
            Type::I8 | Type::I16 | Type::I32 | Type::Char | Type::Boolean => {
                if !["eq", "ne", "lt", "le", "gt", "ge"].contains(&comp_op) { /* error */ }
                branch_constructor = Box::new(move |offset| match comp_op {
                    // move comp_op
                    "eq" => Instruction::If_icmpeq(offset),
                    "ne" => Instruction::If_icmpne(offset),
                    "lt" => Instruction::If_icmplt(offset),
                    "le" => Instruction::If_icmple(offset),
                    "gt" => Instruction::If_icmpgt(offset),
                    "ge" => Instruction::If_icmpge(offset),
                    _ => unreachable!(), // Already checked
                });
            }
            Type::I64 => {
                if !["eq", "ne", "lt", "le", "gt", "ge"].contains(&comp_op) { /* error */ }
                self.jvm_instructions.push(Instruction::Lcmp); // Stack: [int_result]
                branch_constructor = Box::new(move |offset| match comp_op {
                    // move comp_op
                    "eq" => Instruction::Ifeq(offset), // compares int_result with 0
                    "ne" => Instruction::Ifne(offset),
                    "lt" => Instruction::Iflt(offset),
                    "le" => Instruction::Ifle(offset),
                    "gt" => Instruction::Ifgt(offset),
                    "ge" => Instruction::Ifge(offset),
                    _ => unreachable!(),
                });
            }
            Type::F32 => {
                if !["eq", "ne", "lt", "le", "gt", "ge"].contains(&comp_op) { /* error */ }
                // Use Fcmpl (NaN -> -1) or Fcmpg (NaN -> +1). Doesn't matter for == 0.
                self.jvm_instructions.push(Instruction::Fcmpl); // Stack: [int_result]
                branch_constructor = Box::new(move |offset| match comp_op {
                    // move comp_op
                    "eq" => Instruction::Ifeq(offset),
                    "ne" => Instruction::Ifne(offset),
                    "lt" => Instruction::Iflt(offset),
                    "le" => Instruction::Ifle(offset),
                    "gt" => Instruction::Ifgt(offset),
                    "ge" => Instruction::Ifge(offset),
                    _ => unreachable!(),
                });
            }
            Type::F64 => {
                if !["eq", "ne", "lt", "le", "gt", "ge"].contains(&comp_op) { /* error */ }
                self.jvm_instructions.push(Instruction::Dcmpl); // Stack: [int_result]
                branch_constructor = Box::new(move |offset| match comp_op {
                    // move comp_op
                    "eq" => Instruction::Ifeq(offset),
                    "ne" => Instruction::Ifne(offset),
                    "lt" => Instruction::Iflt(offset),
                    "le" => Instruction::Ifle(offset),
                    "gt" => Instruction::Ifgt(offset),
                    "ge" => Instruction::Ifge(offset),
                    _ => unreachable!(),
                });
            }
            Type::Class(ref class_name) if class_name == BIG_INTEGER_CLASS => {
                if !["eq", "ne", "lt", "le", "gt", "ge"].contains(&comp_op) { /* error */ }
                // Call BigInteger.compareTo(BigInteger) -> int
                let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                let method_ref = self.constant_pool.add_method_ref(
                    class_idx,
                    "compareTo",
                    "(Ljava/math/BigInteger;)I",
                )?;
                self.jvm_instructions
                    .push(Instruction::Invokevirtual(method_ref)); // Stack: [int_result]
                // Branch based on the int result compared to 0
                branch_constructor = Box::new(move |offset| match comp_op {
                    // move comp_op
                    "eq" => Instruction::Ifeq(offset),
                    "ne" => Instruction::Ifne(offset),
                    "lt" => Instruction::Iflt(offset),
                    "le" => Instruction::Ifle(offset),
                    "gt" => Instruction::Ifgt(offset),
                    "ge" => Instruction::Ifge(offset),
                    _ => unreachable!(),
                });
            }
            Type::Class(ref class_name) if class_name == BIG_DECIMAL_CLASS => {
                if !["eq", "ne", "lt", "le", "gt", "ge"].contains(&comp_op) { /* error */ }
                // Call BigDecimal.compareTo(BigDecimal) -> int
                let class_idx = self.constant_pool.add_class(BIG_DECIMAL_CLASS)?;
                let method_ref = self.constant_pool.add_method_ref(
                    class_idx,
                    "compareTo",
                    "(Ljava/math/BigDecimal;)I",
                )?;
                self.jvm_instructions
                    .push(Instruction::Invokevirtual(method_ref)); // Stack: [int_result]
                // Branch based on the int result compared to 0
                branch_constructor = Box::new(move |offset| match comp_op {
                    // move comp_op
                    "eq" => Instruction::Ifeq(offset),
                    "ne" => Instruction::Ifne(offset),
                    "lt" => Instruction::Iflt(offset),
                    "le" => Instruction::Ifle(offset),
                    "gt" => Instruction::Ifgt(offset),
                    "ge" => Instruction::Ifge(offset),
                    _ => unreachable!(),
                });
            }
            // General Reference types (including String, Array, other Classes)
            ref ty if ty.is_jvm_reference_type() => {
                // Only support equality/inequality for general references
                match comp_op {
                    "eq" => branch_constructor = Box::new(|offset| Instruction::If_acmpeq(offset)),
                    "ne" => branch_constructor = Box::new(|offset| Instruction::If_acmpne(offset)),
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Unsupported comparison operator '{}' for reference type {:?}",
                                comp_op, comparison_type
                            ),
                        });
                    }
                }
            }

            // Should be caught by determine_comparison_type, but as a safeguard:
            _ => {
                return Err(jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!("Unsupported type for comparison: {:?}", comparison_type),
                });
            }
        }

        // --- Generate Branching Logic (unchanged from original) ---
        let instr_idx_if = self.jvm_instructions.len();
        let label_true = format!("_comparison_true_{}", instr_idx_if);
        let label_after = format!("_comparison_after_{}", instr_idx_if);

        // Emit branch instruction (using the constructor decided above)
        self.jvm_instructions.push(branch_constructor(0)); // Placeholder offset
        self.branch_fixups.push((instr_idx_if, label_true.clone()));

        // False case: push 0
        self.jvm_instructions.push(Instruction::Iconst_0);
        let instr_idx_goto_after = self.jvm_instructions.len();
        self.jvm_instructions.push(Instruction::Goto(0)); // Placeholder offset
        self.branch_fixups
            .push((instr_idx_goto_after, label_after.clone()));

        // True case: record label, push 1
        let true_instr_index: u16 = self.jvm_instructions.len().try_into().unwrap();
        self.label_to_instr_index
            .insert(label_true, true_instr_index);
        self.jvm_instructions.push(Instruction::Iconst_1);

        // After branch: record label
        let after_instr_index: u16 = self.jvm_instructions.len().try_into().unwrap();
        self.label_to_instr_index
            .insert(label_after, after_instr_index);

        // Store the boolean result (unchanged)
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
                let op1_type = get_operand_type(op1);
                let op2_type = get_operand_type(op2); // Get type of op2 as well

                // Promote based on types (Simplified: assumes BigInt/BigDec promote others)
                // A more robust system would use determine_comparison_type logic
                let op_type = if op1_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
                    || op2_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
                {
                    Type::Class(BIG_DECIMAL_CLASS.to_string())
                } else if op1_type == Type::Class(BIG_INTEGER_CLASS.to_string())
                    || op2_type == Type::Class(BIG_INTEGER_CLASS.to_string())
                {
                    Type::Class(BIG_INTEGER_CLASS.to_string())
                } else {
                    // Fallback to op1's type for primitive promotion (translate_binary_op handles this)
                    op1_type.clone()
                };

                match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                        // TODO: Implement numeric promotion (e.g., i8+i32 -> i32) if not handled by translate_binary_op
                        self.translate_binary_op(dest, op1, op2, JI::Iadd)?
                    }
                    Type::I64 => self.translate_binary_op(dest, op1, op2, JI::Ladd)?,
                    Type::F32 => self.translate_binary_op(dest, op1, op2, JI::Fadd)?,
                    Type::F64 => self.translate_binary_op(dest, op1, op2, JI::Dadd)?,
                    Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger ADD operation: add(BigInteger)
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "add",
                            "(Ljava/math/BigInteger;)Ljava/math/BigInteger;",
                        )?;

                        // 1. Load op1, casting to BigInt if needed
                        self.load_operand(op1)?;
                        if op1_type != op_type {
                            let cast_instrs =
                                get_cast_instructions(&op1_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        // 2. Load op2, casting to BigInt if needed
                        self.load_operand(op2)?;
                        if op2_type != op_type {
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        // 3. Call the method
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Stack: [result]
                        // 4. Store the result
                        self.store_result(dest, &op_type)?; // Stack: []
                    }
                    Type::Class(ref c) if c == BIG_DECIMAL_CLASS => {
                        // BigDecimal ADD operation: add(BigDecimal)
                        let class_idx = self.constant_pool.add_class(BIG_DECIMAL_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "add",
                            "(Ljava/math/BigDecimal;)Ljava/math/BigDecimal;",
                        )?;
                        // 1. Load op1, casting to BigDec if needed
                        self.load_operand(op1)?;
                        if op1_type != op_type {
                            let cast_instrs =
                                get_cast_instructions(&op1_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        // 2. Load op2, casting to BigDec if needed
                        self.load_operand(op2)?;
                        if op2_type != op_type {
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        // 3. Call the method
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Stack: [result]
                        // 4. Store the result
                        self.store_result(dest, &op_type)?; // Stack: []
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Add operation: {:?}", op_type),
                        });
                    }
                }
            }
            OI::Sub { dest, op1, op2 } => {
                let op1_type = get_operand_type(op1);
                let op2_type = get_operand_type(op2);

                // Determine result type (similar promotion logic as Add)
                let op_type = if op1_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
                    || op2_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
                {
                    Type::Class(BIG_DECIMAL_CLASS.to_string())
                } else if op1_type == Type::Class(BIG_INTEGER_CLASS.to_string())
                    || op2_type == Type::Class(BIG_INTEGER_CLASS.to_string())
                {
                    Type::Class(BIG_INTEGER_CLASS.to_string())
                } else {
                    op1_type.clone()
                };

                match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                        self.translate_binary_op(dest, op1, op2, JI::Isub)?
                    }
                    Type::I64 => self.translate_binary_op(dest, op1, op2, JI::Lsub)?,
                    Type::F32 => self.translate_binary_op(dest, op1, op2, JI::Fsub)?,
                    Type::F64 => self.translate_binary_op(dest, op1, op2, JI::Dsub)?,
                    Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger SUBTRACT operation: subtract(BigInteger)
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "subtract", // Correct method name
                            "(Ljava/math/BigInteger;)Ljava/math/BigInteger;",
                        )?;

                        // 1. Load op1, casting if needed
                        self.load_operand(op1)?;
                        if op1_type != op_type {
                            let cast_instrs =
                                get_cast_instructions(&op1_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        // 2. Load op2, casting if needed
                        self.load_operand(op2)?;
                        if op2_type != op_type {
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        // 3. Call the method
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref));
                        // 4. Store the result
                        self.store_result(dest, &op_type)?;
                    }
                    Type::Class(ref c) if c == BIG_DECIMAL_CLASS => {
                        // BigDecimal SUBTRACT operation: subtract(BigDecimal)
                        let class_idx = self.constant_pool.add_class(BIG_DECIMAL_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "subtract", // Correct method name
                            "(Ljava/math/BigDecimal;)Ljava/math/BigDecimal;",
                        )?;
                        // 1. Load op1, casting if needed
                        self.load_operand(op1)?;
                        if op1_type != op_type {
                            let cast_instrs =
                                get_cast_instructions(&op1_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        // 2. Load op2, casting if needed
                        self.load_operand(op2)?;
                        if op2_type != op_type {
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        // 3. Call the method
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref));
                        // 4. Store the result
                        self.store_result(dest, &op_type)?;
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Sub operation: {:?}", op_type),
                        });
                    }
                }
            }
            OI::Mul { dest, op1, op2 } => {
                let op1_type = get_operand_type(op1);
                let op2_type = get_operand_type(op2);
                let op_type = if op1_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
                    || op2_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
                {
                    Type::Class(BIG_DECIMAL_CLASS.to_string())
                } else if op1_type == Type::Class(BIG_INTEGER_CLASS.to_string())
                    || op2_type == Type::Class(BIG_INTEGER_CLASS.to_string())
                {
                    Type::Class(BIG_INTEGER_CLASS.to_string())
                } else {
                    op1_type.clone()
                };

                match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                        self.translate_binary_op(dest, op1, op2, JI::Imul)?
                    }
                    Type::I64 => self.translate_binary_op(dest, op1, op2, JI::Lmul)?,
                    Type::F32 => self.translate_binary_op(dest, op1, op2, JI::Fmul)?,
                    Type::F64 => self.translate_binary_op(dest, op1, op2, JI::Dmul)?,
                    Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger MULTIPLY operation: multiply(BigInteger)
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "multiply", // Correct method name
                            "(Ljava/math/BigInteger;)Ljava/math/BigInteger;",
                        )?;

                        self.load_operand(op1)?; // Load op1
                        if op1_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op1_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.load_operand(op2)?; // Load op2
                        if op2_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Call
                        self.store_result(dest, &op_type)?; // Store
                    }
                    Type::Class(ref c) if c == BIG_DECIMAL_CLASS => {
                        // BigDecimal MULTIPLY operation: multiply(BigDecimal)
                        let class_idx = self.constant_pool.add_class(BIG_DECIMAL_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "multiply", // Correct method name
                            "(Ljava/math/BigDecimal;)Ljava/math/BigDecimal;",
                        )?;

                        self.load_operand(op1)?; // Load op1
                        if op1_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op1_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.load_operand(op2)?; // Load op2
                        if op2_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Call
                        self.store_result(dest, &op_type)?; // Store
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Mul operation: {:?}", op_type),
                        });
                    }
                }
            }
            OI::Div { dest, op1, op2 } => {
                let op1_type = get_operand_type(op1);
                let op2_type = get_operand_type(op2);
                let op_type = if op1_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
                    || op2_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
                {
                    Type::Class(BIG_DECIMAL_CLASS.to_string())
                } else if op1_type == Type::Class(BIG_INTEGER_CLASS.to_string())
                    || op2_type == Type::Class(BIG_INTEGER_CLASS.to_string())
                {
                    Type::Class(BIG_INTEGER_CLASS.to_string())
                } else {
                    op1_type.clone()
                };

                match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                        // Potential DivisionByZeroError for primitives handled by JVM
                        self.translate_binary_op(dest, op1, op2, JI::Idiv)?
                    }
                    Type::I64 => self.translate_binary_op(dest, op1, op2, JI::Ldiv)?,
                    Type::F32 => self.translate_binary_op(dest, op1, op2, JI::Fdiv)?, // Handles +/- Infinity, NaN
                    Type::F64 => self.translate_binary_op(dest, op1, op2, JI::Ddiv)?, // Handles +/- Infinity, NaN
                    Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger DIVIDE operation: divide(BigInteger)
                        // Throws ArithmeticException if divisor is zero.
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "divide", // Correct method name
                            "(Ljava/math/BigInteger;)Ljava/math/BigInteger;",
                        )?;

                        self.load_operand(op1)?; // Load op1
                        if op1_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op1_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.load_operand(op2)?; // Load op2
                        if op2_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Call
                        self.store_result(dest, &op_type)?; // Store
                    }
                    Type::Class(ref c) if c == BIG_DECIMAL_CLASS => {
                        // BigDecimal DIVIDE operation: divide(BigDecimal, RoundingMode)
                        // Using RoundingMode.HALF_UP.
                        // Throws ArithmeticException if divisor is zero or exact result requires infinite digits.
                        let rounding_mode_enum = "java/math/RoundingMode";
                        let rounding_mode_field = "HALF_UP"; // Example default

                        let big_dec_class_idx = self.constant_pool.add_class(BIG_DECIMAL_CLASS)?;
                        let rounding_mode_class_idx =
                            self.constant_pool.add_class(rounding_mode_enum)?;

                        // Method ref for divide(BigDecimal, RoundingMode)
                        let method_ref = self.constant_pool.add_method_ref(
                            big_dec_class_idx,
                            "divide",
                            &format!(
                                "(Ljava/math/BigDecimal;L{};)Ljava/math/BigDecimal;",
                                rounding_mode_enum
                            ),
                        )?;
                        // Field ref for the RoundingMode constant
                        let field_ref = self.constant_pool.add_field_ref(
                            rounding_mode_class_idx,
                            rounding_mode_field,
                            &format!("L{};", rounding_mode_enum), // Descriptor for the enum field
                        )?;

                        self.load_operand(op1)?; // Load op1 (dividend)
                        if op1_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op1_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.load_operand(op2)?; // Load op2 (divisor)
                        if op2_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        // Load the RoundingMode constant
                        self.jvm_instructions.push(JI::Getstatic(field_ref)); // Stack: [op1, op2, rounding_mode]

                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Call
                        self.store_result(dest, &op_type)?; // Store
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Div operation: {:?}", op_type),
                        });
                    }
                }
            }
            OI::Rem { dest, op1, op2 } => {
                let op1_type = get_operand_type(op1);
                let op2_type = get_operand_type(op2);
                let op_type = if op1_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
                    || op2_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
                {
                    Type::Class(BIG_DECIMAL_CLASS.to_string())
                } else if op1_type == Type::Class(BIG_INTEGER_CLASS.to_string())
                    || op2_type == Type::Class(BIG_INTEGER_CLASS.to_string())
                {
                    Type::Class(BIG_INTEGER_CLASS.to_string())
                } else {
                    op1_type.clone()
                };

                match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                        // Potential DivisionByZeroError handled by JVM
                        self.translate_binary_op(dest, op1, op2, JI::Irem)?
                    }
                    Type::I64 => self.translate_binary_op(dest, op1, op2, JI::Lrem)?,
                    Type::F32 => self.translate_binary_op(dest, op1, op2, JI::Frem)?, // Handles NaN
                    Type::F64 => self.translate_binary_op(dest, op1, op2, JI::Drem)?, // Handles NaN
                    Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger REMAINDER operation: remainder(BigInteger)
                        // Throws ArithmeticException if divisor is zero.
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "remainder", // Correct method name
                            "(Ljava/math/BigInteger;)Ljava/math/BigInteger;",
                        )?;

                        self.load_operand(op1)?; // Load op1
                        if op1_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op1_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.load_operand(op2)?; // Load op2
                        if op2_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Call
                        self.store_result(dest, &op_type)?; // Store
                    }
                    Type::Class(ref c) if c == BIG_DECIMAL_CLASS => {
                        // BigDecimal REMAINDER operation: remainder(BigDecimal)
                        // Throws ArithmeticException if divisor is zero.
                        let class_idx = self.constant_pool.add_class(BIG_DECIMAL_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "remainder", // Correct method name
                            "(Ljava/math/BigDecimal;)Ljava/math/BigDecimal;",
                        )?;

                        self.load_operand(op1)?; // Load op1
                        if op1_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op1_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.load_operand(op2)?; // Load op2
                        if op2_type != op_type {
                            // Cast if needed
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &op_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Call
                        self.store_result(dest, &op_type)?; // Store
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Rem operation: {:?}", op_type),
                        });
                    }
                }
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
                let op_type = get_operand_type(op1); // Use helper to get type robustly

                match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                        self.translate_binary_op(dest, op1, op2, JI::Iand)?
                    }
                    Type::I64 => self.translate_binary_op(dest, op1, op2, JI::Land)?,
                    Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger AND operation
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "and",
                            "(Ljava/math/BigInteger;)Ljava/math/BigInteger;", // Correct descriptor
                        )?;

                        // 1. Load the instance (`op1`)
                        self.load_operand(op1)?; // Stack: [instance]
                        // 2. Load the argument (`op2`)
                        self.load_operand(op2)?; // Stack: [instance, argument]
                        // 3. Call the method
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Stack: [result]
                        // 4. Store the result into 'dest'
                        //    The result type is known from the method signature's return type
                        let result_type = Type::Class(BIG_INTEGER_CLASS.to_string());
                        self.store_result(dest, &result_type)?; // Stack: []
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Unsupported type for BitAnd operation: {:?}",
                                op_type
                            ),
                        });
                    }
                }
            }
            OI::BitOr { dest, op1, op2 } => {
                // Use helper to get type robustly
                let op_type = get_operand_type(op1);

                match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                        // Primitive case handled by translate_binary_op below
                        self.translate_binary_op(dest, op1, op2, JI::Ior)?
                    }
                    Type::I64 => {
                        // Primitive case handled by translate_binary_op below
                        self.translate_binary_op(dest, op1, op2, JI::Lor)?
                    }
                    Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger OR operation
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "or",
                            "(Ljava/math/BigInteger;)Ljava/math/BigInteger;", // Correct descriptor
                        )?;

                        // 1. Load the instance (`op1`)
                        self.load_operand(op1)?; // Stack: [instance]
                        // 2. Load the argument (`op2`)
                        self.load_operand(op2)?; // Stack: [instance, argument]
                        // 3. Call the method
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Stack: [result]
                        // 4. Store the result into 'dest'
                        let result_type = Type::Class(BIG_INTEGER_CLASS.to_string());
                        self.store_result(dest, &result_type)?; // Stack: []

                        // No premature return! Ok(()) implicitly returned at end of block
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for BitOr operation: {:?}", op_type),
                        });
                    }
                }
                // Primitive cases fall through here if translate_binary_op was called
            }
            OI::BitXor { dest, op1, op2 } => {
                // Use helper to get type robustly
                let op_type = get_operand_type(op1);

                match op_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                        // Primitive case handled by translate_binary_op below
                        self.translate_binary_op(dest, op1, op2, JI::Ixor)?
                    }
                    Type::I64 => {
                        // Primitive case handled by translate_binary_op below
                        self.translate_binary_op(dest, op1, op2, JI::Lxor)?
                    }
                    Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger XOR operation
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "xor",
                            "(Ljava/math/BigInteger;)Ljava/math/BigInteger;", // Correct descriptor
                        )?;

                        // 1. Load the instance (`op1`)
                        self.load_operand(op1)?; // Stack: [instance]
                        // 2. Load the argument (`op2`)
                        self.load_operand(op2)?; // Stack: [instance, argument]
                        // 3. Call the method
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Stack: [result]
                        // 4. Store the result into 'dest'
                        let result_type = Type::Class(BIG_INTEGER_CLASS.to_string());
                        self.store_result(dest, &result_type)?; // Stack: []

                        // No premature return! Ok(()) implicitly returned at end of block
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Unsupported type for BitXor operation: {:?}",
                                op_type
                            ),
                        });
                    }
                }
                // Primitive cases fall through here if translate_binary_op was called
            }
            OI::Shl { dest, op1, op2 } => {
                // Type of the object being shifted
                let op1_type = get_operand_type(op1);
                // Type of the shift amount (must be int for BigInteger.shiftLeft/Right)
                let op2_type = get_operand_type(op2);

                match op1_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                        // JVM Ishl/Lshl takes int shift amount. op2 might need casting if not I32.
                        // translate_binary_op implicitly handles this by taking only I32 shift value.
                        self.load_operand(op1)?; // Load value to shift
                        self.load_operand(op2)?; // Load shift amount (should be I32)
                        // Perform potential cast of op1 to I32 if needed (e.g., I8 -> I32)
                        match get_operand_type(op1) {
                            // Re-check op1's specific type for casting
                            Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                                let cast_instrs = get_cast_instructions(
                                    &get_operand_type(op1),
                                    &Type::I32,
                                    self.constant_pool,
                                )?;
                                self.jvm_instructions.extend(cast_instrs);
                            }
                            _ => {} // Already I32
                        }
                        self.jvm_instructions.push(JI::Ishl);
                        self.store_result(dest, &Type::I32)?; // Result is I32
                    }
                    Type::I64 => {
                        self.load_operand(op1)?; // Load long value
                        self.load_operand(op2)?; // Load int shift amount
                        self.jvm_instructions.push(JI::Lshl);
                        self.store_result(dest, &Type::I64)?; // Result is I64
                    }
                    Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger SHL operation: shiftLeft(int)
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "shiftLeft",
                            "(I)Ljava/math/BigInteger;", // Takes int!
                        )?;

                        // 1. Load the instance (`op1`)
                        self.load_operand(op1)?; // Stack: [instance]
                        // 2. Load the shift amount (`op2`) and ensure it's int
                        self.load_operand(op2)?; // Stack: [instance, shift_amount_raw]
                        if op2_type != Type::I32 {
                            // Cast op2 to int if necessary (e.g., from I64, I16, I8)
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &Type::I32, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs); // Stack: [instance, shift_amount_int]
                        }
                        // 3. Call the method
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Stack: [result]
                        // 4. Store the result into 'dest'
                        let result_type = Type::Class(BIG_INTEGER_CLASS.to_string());
                        self.store_result(dest, &result_type)?; // Stack: []
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Shl operation: {:?}", op1_type),
                        });
                    }
                }
            }
            OI::Shr { dest, op1, op2 } => {
                // Type of the object being shifted
                let op1_type = get_operand_type(op1);
                // Type of the shift amount (must be int for BigInteger.shiftLeft/Right)
                let op2_type = get_operand_type(op2);

                match op1_type {
                    Type::I32 | Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                        // JVM Ishr/Lshr takes int shift amount. op2 might need casting if not I32.
                        self.load_operand(op1)?; // Load value to shift
                        self.load_operand(op2)?; // Load shift amount (should be I32)
                        // Perform potential cast of op1 to I32 if needed (e.g., I8 -> I32)
                        match get_operand_type(op1) {
                            // Re-check op1's specific type for casting
                            Type::I8 | Type::I16 | Type::Boolean | Type::Char => {
                                let cast_instrs = get_cast_instructions(
                                    &get_operand_type(op1),
                                    &Type::I32,
                                    self.constant_pool,
                                )?;
                                self.jvm_instructions.extend(cast_instrs);
                            }
                            _ => {} // Already I32
                        }
                        self.jvm_instructions.push(JI::Ishr); // Use Ishr for signed right shift
                        self.store_result(dest, &Type::I32)?; // Result is I32
                    }
                    Type::I64 => {
                        self.load_operand(op1)?; // Load long value
                        self.load_operand(op2)?; // Load int shift amount
                        self.jvm_instructions.push(JI::Lshr); // Use Lshr for signed right shift
                        self.store_result(dest, &Type::I64)?; // Result is I64
                    }
                    Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger SHR operation: shiftRight(int)
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "shiftRight",
                            "(I)Ljava/math/BigInteger;", // Takes int!
                        )?;

                        // 1. Load the instance (`op1`)
                        self.load_operand(op1)?; // Stack: [instance]
                        // 2. Load the shift amount (`op2`) and ensure it's int
                        self.load_operand(op2)?; // Stack: [instance, shift_amount_raw]
                        if op2_type != Type::I32 {
                            // Cast op2 to int if necessary (e.g., from I64, I16, I8)
                            let cast_instrs =
                                get_cast_instructions(&op2_type, &Type::I32, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs); // Stack: [instance, shift_amount_int]
                        }
                        // 3. Call the method
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Stack: [result]
                        // 4. Store the result into 'dest'
                        let result_type = Type::Class(BIG_INTEGER_CLASS.to_string());
                        self.store_result(dest, &result_type)?; // Stack: []
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Shr operation: {:?}", op1_type),
                        });
                    }
                }
            }

            OI::Not { dest, src } => {
                let src_type = get_operand_type(src);
                match src_type {
                    oomir::Type::Boolean => {
                        self.load_operand(src)?;
                        self.jvm_instructions.push(JI::Iconst_1);
                        self.jvm_instructions.push(JI::Ixor);
                        self.store_result(dest, &src_type)?; // Store boolean result
                    }
                    oomir::Type::I8 | oomir::Type::I16 | oomir::Type::I32 | oomir::Type::Char => {
                        self.load_operand(src)?;
                        // Cast to I32 if necessary before XOR
                        if src_type != Type::I32 {
                            let cast_instrs =
                                get_cast_instructions(&src_type, &Type::I32, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.jvm_instructions.push(JI::Iconst_m1);
                        self.jvm_instructions.push(JI::Ixor);
                        // Store result as original type (e.g., if src was I8, dest should be I8)
                        // Result of IXOR is I32, so cast back if needed
                        if src_type != Type::I32 {
                            let cast_instrs =
                                get_cast_instructions(&Type::I32, &src_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.store_result(dest, &src_type)?;
                    }
                    oomir::Type::I64 => {
                        self.load_operand(src)?;
                        let neg_one_long_index = self.constant_pool.add_long(-1_i64)?;
                        self.jvm_instructions.push(JI::Ldc2_w(neg_one_long_index));
                        self.jvm_instructions.push(JI::Lxor);
                        self.store_result(dest, &src_type)?; // Store long result
                    }
                    oomir::Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger NOT operation: not()
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "not",
                            "()Ljava/math/BigInteger;", // Takes no args!
                        )?;

                        // 1. Load the instance (`src`)
                        self.load_operand(src)?; // Stack: [instance]
                        // 2. Call the method
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Stack: [result]
                        // 3. Store the result into 'dest'
                        let result_type = Type::Class(BIG_INTEGER_CLASS.to_string());
                        self.store_result(dest, &result_type)?; // Stack: []
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Not operation: {:?}", src_type),
                        });
                    }
                }
                // No single store_result needed here, handled within each match arm
            }

            OI::Neg { dest, src } => {
                let src_type = get_operand_type(src);
                match src_type {
                    oomir::Type::I8
                    | oomir::Type::I16
                    | oomir::Type::I32
                    | oomir::Type::Boolean
                    | oomir::Type::Char => {
                        self.load_operand(src)?;
                        // Cast to I32 if needed before INEG
                        if src_type != Type::I32 {
                            let cast_instrs =
                                get_cast_instructions(&src_type, &Type::I32, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.jvm_instructions.push(JI::Ineg);
                        // Cast back if needed
                        if src_type != Type::I32 {
                            let cast_instrs =
                                get_cast_instructions(&Type::I32, &src_type, self.constant_pool)?;
                            self.jvm_instructions.extend(cast_instrs);
                        }
                        self.store_result(dest, &src_type)?;
                    }
                    oomir::Type::I64 => {
                        self.load_operand(src)?;
                        self.jvm_instructions.push(JI::Lneg);
                        self.store_result(dest, &src_type)?;
                    }
                    oomir::Type::F32 => {
                        self.load_operand(src)?;
                        self.jvm_instructions.push(JI::Fneg);
                        self.store_result(dest, &src_type)?;
                    }
                    oomir::Type::F64 => {
                        self.load_operand(src)?;
                        self.jvm_instructions.push(JI::Dneg);
                        self.store_result(dest, &src_type)?;
                    }
                    oomir::Type::Class(ref c) if c == BIG_INTEGER_CLASS => {
                        // BigInteger Negation operation: negate()
                        let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                        let method_ref = self.constant_pool.add_method_ref(
                            class_idx,
                            "negate",
                            "()Ljava/math/BigInteger;", // Takes no args!
                        )?;

                        // 1. Load the instance (`src`)
                        self.load_operand(src)?; // Stack: [instance]
                        // 2. Call the method
                        self.jvm_instructions.push(JI::Invokevirtual(method_ref)); // Stack: [result]
                        // 3. Store the result into 'dest'
                        let result_type = Type::Class(BIG_INTEGER_CLASS.to_string());
                        self.store_result(dest, &result_type)?; // Stack: []
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Unsupported type for Neg operation: {:?}", src_type),
                        });
                    }
                }
                // No single store_result needed here, handled within each match arm
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
                // 0. Calculate the type of the discriminant
                let discr_type = get_operand_type(discr); // Use helper consistently

                // --- Input Validation ---
                // Check if the discriminant type is suitable for switch comparison
                let is_valid_switch_type = match &discr_type {
                    Type::I8 | Type::I16 | Type::I32 | Type::Boolean | Type::Char => true, // Promoted to int
                    Type::I64 => true,
                    Type::F32 => true,
                    Type::F64 => true,
                    Type::String => true, // Use .equals()
                    Type::Class(c) if c == BIG_INTEGER_CLASS || c == BIG_DECIMAL_CLASS => true, // Use .compareTo()
                    _ => false,
                };

                if !is_valid_switch_type {
                    return Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!(
                            "Unsupported discriminant type {:?} for OOMIR Switch instruction",
                            discr_type
                        ),
                    });
                }

                // 1. Load the discriminant value onto the stack
                self.load_operand(discr)?; // Stack: [discr_value] (size 1 or 2 depending on type)

                // 2. Store the discriminant in a temporary local variable.
                //    This is necessary because we need to reload it for each comparison.
                let temp_discr_var_name = format!(
                    "_switch_discr_temp_{}_{}",
                    self.oomir_func.name, self.current_oomir_block_label
                );
                let temp_discr_index = self.get_or_assign_local(&temp_discr_var_name, &discr_type);
                let store_instr = get_store_instruction(&discr_type, temp_discr_index)?;
                self.jvm_instructions.push(store_instr); // Stack is now empty

                // 3. Iterate through the specific targets and generate comparison checks
                for (constant_key, target_label) in targets {
                    // a. Reload the discriminant value from the temporary local
                    let load_instr = get_load_instruction(&discr_type, temp_discr_index)?;
                    self.jvm_instructions.push(load_instr); // Stack: [discr_value] (size 1 or 2)

                    // --- b. Load constant key and perform comparison based on discriminant type ---

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
                            let const_instr =
                                get_int_const_instr(&mut self.constant_pool, key_value_i32);
                            self.jvm_instructions.push(const_instr); // Stack: [discr(i32), key(i32)]
                            let if_instr_index = self.jvm_instructions.len();
                            self.jvm_instructions.push(JI::If_icmpeq(0)); // Jump if equal
                            self.branch_fixups
                                .push((if_instr_index, target_label.clone()));
                        }

                        // --- I64 (long) type (use lcmp, ifeq) ---
                        Type::I64 => {
                            match constant_key {
                                oomir::Constant::I64(_) => {} // Expected type
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
                            load_constant(
                                &mut self.jvm_instructions,
                                &mut self.constant_pool,
                                constant_key,
                            )?; // Stack: [discr(long), key(long)]
                            self.jvm_instructions.push(JI::Lcmp); // Stack: [cmp_result(int)]
                            let if_instr_index = self.jvm_instructions.len();
                            self.jvm_instructions.push(JI::Ifeq(0)); // Jump if equal (cmp_result == 0)
                            self.branch_fixups
                                .push((if_instr_index, target_label.clone()));
                        }

                        // --- F32 (float) type (use fcmpl, ifeq) ---
                        Type::F32 => {
                            match constant_key {
                                oomir::Constant::F32(_) => {} // Expected type
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
                            load_constant(
                                &mut self.jvm_instructions,
                                &mut self.constant_pool,
                                constant_key,
                            )?; // Stack: [discr(f32), key(f32)]
                            self.jvm_instructions.push(JI::Fcmpl); // Stack: [cmp_result(int)]
                            let if_instr_index = self.jvm_instructions.len();
                            self.jvm_instructions.push(JI::Ifeq(0)); // Jump if equal
                            self.branch_fixups
                                .push((if_instr_index, target_label.clone()));
                        }

                        // --- F64 (double) type (use dcmpl, ifeq) ---
                        Type::F64 => {
                            match constant_key {
                                oomir::Constant::F64(_) => {} // Expected type
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
                            load_constant(
                                &mut self.jvm_instructions,
                                &mut self.constant_pool,
                                constant_key,
                            )?; // Stack: [discr(f64), key(f64)]
                            self.jvm_instructions.push(JI::Dcmpl); // Stack: [cmp_result(int)]
                            let if_instr_index = self.jvm_instructions.len();
                            self.jvm_instructions.push(JI::Ifeq(0)); // Jump if equal
                            self.branch_fixups
                                .push((if_instr_index, target_label.clone()));
                        }

                        // --- String type (use .equals(), ifne) ---
                        Type::String => {
                            match constant_key {
                                oomir::Constant::String(_) => {} // Expected type
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
                            load_constant(
                                &mut self.jvm_instructions,
                                &mut self.constant_pool,
                                constant_key,
                            )?; // Stack: [discr(str_ref), key(str_ref)]

                            let string_class_idx =
                                self.constant_pool.add_class("java/lang/String")?;
                            let equals_method_ref = self.constant_pool.add_method_ref(
                                string_class_idx,
                                "equals",
                                "(Ljava/lang/Object;)Z", // String.equals takes Object, returns boolean (Z -> int)
                            )?;
                            self.jvm_instructions
                                .push(JI::Invokevirtual(equals_method_ref)); // Stack: [equals_result(int 0 or 1)]

                            let if_instr_index = self.jvm_instructions.len();
                            // Jump if the result is non-zero (i.e., true)
                            self.jvm_instructions.push(JI::Ifne(0));
                            self.branch_fixups
                                .push((if_instr_index, target_label.clone()));
                        }

                        // --- BigInteger type (use .compareTo(), ifeq) ---
                        Type::Class(c) if c == BIG_INTEGER_CLASS => {
                            load_constant(
                                &mut self.jvm_instructions,
                                &mut self.constant_pool,
                                constant_key,
                            )?; // Stack: [discr(BI_ref), key(BI_ref)]

                            let class_idx = self.constant_pool.add_class(BIG_INTEGER_CLASS)?;
                            let compare_to_ref = self.constant_pool.add_method_ref(
                                class_idx,
                                "compareTo",
                                "(Ljava/math/BigInteger;)I",
                            )?;
                            self.jvm_instructions
                                .push(JI::Invokevirtual(compare_to_ref)); // Stack: [cmp_result(int)]
                            let if_instr_index = self.jvm_instructions.len();
                            self.jvm_instructions.push(JI::Ifeq(0)); // Jump if equal (cmp_result == 0)
                            self.branch_fixups
                                .push((if_instr_index, target_label.clone()));
                        }

                        // --- BigDecimal type (use .compareTo(), ifeq) ---
                        Type::Class(c) if c == BIG_DECIMAL_CLASS => {
                            load_constant(
                                &mut self.jvm_instructions,
                                &mut self.constant_pool,
                                constant_key,
                            )?; // Stack: [discr(BD_ref), key(BD_ref)]

                            let class_idx = self.constant_pool.add_class(BIG_DECIMAL_CLASS)?;
                            let compare_to_ref = self.constant_pool.add_method_ref(
                                class_idx,
                                "compareTo",
                                "(Ljava/math/BigDecimal;)I",
                            )?;
                            self.jvm_instructions
                                .push(JI::Invokevirtual(compare_to_ref)); // Stack: [cmp_result(int)]
                            let if_instr_index = self.jvm_instructions.len();
                            self.jvm_instructions.push(JI::Ifeq(0)); // Jump if equal (cmp_result == 0)
                            self.branch_fixups
                                .push((if_instr_index, target_label.clone()));
                        }

                        // Should be caught by the validation check before the loop
                        _ => unreachable!(
                            "Invalid discriminant type {:?} survived initial check",
                            discr_type
                        ),
                    }

                    // If the comparison is false, execution falls through to the next check.
                    // The stack should be empty after the conditional jump or method call + conditional jump consumes its operands.
                }

                // 4. After all specific checks, add an unconditional jump to the 'otherwise' block.
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
                            | oomir::Type::MutableReference(_)
                            | oomir::Type::String
                            | oomir::Type::Class(_)
                            | oomir::Type::Interface(_) => JI::Areturn,
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
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "bytecode-gen",
                        format!(
                            "Warning: Overwriting existing entry in label_to_instr_index for label '{}'. Old index: {}, New index: {}",
                            name, old_idx, current_jvm_instr_index
                        )
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

                            let shim_descriptor = Some(shim_info.descriptor.clone()); // Store descriptor

                            // --- Argument Loading with Boxing ---
                            let expected_jvm_param_types = parse_jvm_descriptor_params(
                                &shim_info.descriptor,
                            )
                            .map_err(|e| jvm::Error::VerificationError {
                                context: format!("Function {}", self.oomir_func.name),
                                message: format!("Error parsing shim descriptor: {}", e),
                            })?;

                            if args.len() != expected_jvm_param_types.len() {
                                return Err(jvm::Error::VerificationError {
                                    context: format!("Function {}", self.oomir_func.name),
                                    message: format!(
                                        "Shim argument count mismatch for '{}': descriptor '{}' expects {}, found {}",
                                        function_name,
                                        shim_info.descriptor,
                                        expected_jvm_param_types.len(),
                                        args.len()
                                    ),
                                });
                            }

                            for (arg_operand, expected_jvm_type) in
                                args.iter().zip(expected_jvm_param_types.iter())
                            {
                                // 1. Get the OOMIR type of the argument being passed
                                let provided_oomir_type = get_operand_type(arg_operand);

                                // 2. Load the raw value onto the stack first
                                self.load_call_argument(arg_operand)?; // Assumes this loads the primitive value correctly

                                // 3. Check if boxing is needed: Expects Object, Got Primitive
                                //    (Only boxing to java.lang.Object for now)
                                if expected_jvm_type == "Ljava/lang/Object;"
                                    && provided_oomir_type.is_jvm_primitive()
                                {
                                    // 4. Get boxing information
                                    if let Some((wrapper_class, box_method, box_desc)) =
                                        provided_oomir_type.get_boxing_info()
                                    {
                                        // 5. Add MethodRef for the boxing method (e.g., Integer.valueOf)
                                        let class_index =
                                            self.constant_pool.add_class(wrapper_class)?;
                                        let method_ref_index = self.constant_pool.add_method_ref(
                                            class_index,
                                            box_method,
                                            box_desc,
                                        )?;
                                        // 6. Add the invokestatic instruction for boxing
                                        self.jvm_instructions
                                            .push(JI::Invokestatic(method_ref_index));
                                    } else {
                                        // This indicates an internal error - we identified a primitive
                                        // but don't know how to box it.
                                        return Err(jvm::Error::VerificationError {
                                            context: format!("Function {}", self.oomir_func.name),
                                            message: format!(
                                                "No boxing information found for type {:?}",
                                                provided_oomir_type
                                            ),
                                        });
                                    }
                                } else if expected_jvm_type.starts_with('L')
                                    && !expected_jvm_type.ends_with(';')
                                {
                                    // Basic sanity check for descriptor parsing
                                    return Err(jvm::Error::VerificationError {
                                        context: format!("Function {}", self.oomir_func.name),
                                        message: format!(
                                            "Invalid JVM descriptor for expected type: {}",
                                            expected_jvm_type
                                        ),
                                    });
                                }
                                // Else: No boxing needed. The type is either already an object,
                                // or the expected type is not Object (e.g., primitive expected, primitive provided).
                            } // End loop over args

                            // --- Continue with Shim Call Invocation ---

                            // Add MethodRef for the actual shim function
                            let kotlin_shim_class = "org/rustlang/core/Core"; // Or get from shim_info if varies
                            let class_index = self.constant_pool.add_class(kotlin_shim_class)?;
                            let method_ref_index = self.constant_pool.add_method_ref(
                                class_index,
                                function_name, // Use the original function name key
                                &shim_info.descriptor, // Use the stored descriptor
                            )?;

                            // Add invoke instruction
                            if shim_info.is_static {
                                self.jvm_instructions
                                    .push(JI::Invokestatic(method_ref_index));
                            } else {
                                // Handle non-static if needed, or keep error
                                return Err(jvm::Error::VerificationError {
                                    context: format!("Function {}", self.oomir_func.name),
                                    message: format!(
                                        "Non-static shim function '{}' not supported",
                                        function_name
                                    ),
                                });
                            }

                            // Check for diverging (can use shim_info if available)
                            if function_name == "panic_fmt"
                                || function_name == "core_panicking_panic"
                                || function_name == "core_assert_failed"
                            // Or check shim_info.diverges
                            {
                                is_diverging_call = true;
                            }

                            // Store result or pop (using the stored descriptor)
                            let shim_descriptor = shim_descriptor
                                .as_ref()
                                .expect("no descriptor found for shim?");
                            if !is_diverging_call && !shim_descriptor.ends_with(")V") {
                                // Parse the OOMIR type corresponding to the shim's return value
                                let shim_return_oomir_type =
                                    oomir::Type::from_jvm_descriptor_return_type(shim_descriptor);

                                if let Some(dest_var) = dest {
                                    // Now, store the result (which is correctly typed on the stack)
                                    // Use the OOMIR type derived from the descriptor for storage logic
                                    self.store_result(dest_var, &shim_return_oomir_type)?;
                                } else {
                                    // No destination variable, just pop the result based on its size
                                    // Use the OOMIR type derived from the descriptor
                                    match get_type_size(&shim_return_oomir_type) {
                                        1 => self.jvm_instructions.push(JI::Pop),
                                        2 => self.jvm_instructions.push(JI::Pop2),
                                        _ => { /* Void return already handled, ignore size 0 */ }
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
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "bytecode-gen",
                            format!(
                                "Warning: Failed to get shim metadata: {}. Falling back to intra-module call attempt for '{}'.",
                                e, function_name
                            )
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
                    let class_index = self.constant_pool.add_class(module.name.clone())?;
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
                array,
                index,
                value,
            } => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "bytecode-gen",
                    format!(
                        "ArrayStore: array {:?}, index {:?}, value {:?}",
                        array, index, value
                    )
                );
                let mut is_string = false;
                // 1. Get the type of the array variable to find the element type
                let array_type = self.get_local_type(array)?.clone(); // Clone to avoid borrow issues
                let element_type = match array_type.clone() {
                    oomir::Type::Array(et) | oomir::Type::MutableReference(et) => et,
                    oomir::Type::String => {
                        // convert
                        let instrs = get_cast_instructions(
                            &oomir::Type::String,
                            &oomir::Type::Array(Box::new(oomir::Type::I16)),
                            &mut self.constant_pool,
                        )
                        .unwrap();
                        self.jvm_instructions.extend(instrs);
                        is_string = true;
                        Box::new(oomir::Type::I16)
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Variable '{}' used in ArrayStore is not an array type, found {:?}",
                                array,
                                array_type.clone()
                            ),
                        });
                    }
                };

                // check if any instructions are needed to cast the value to the element type
                let value_type = get_operand_type(value);
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "bytecode-gen",
                    format!(
                        "Value type: {:?}, element type {:?}",
                        value_type, element_type
                    )
                );
                if value_type != *element_type {
                    let instrs =
                        get_cast_instructions(&value_type, &element_type, &mut self.constant_pool)
                            .unwrap();
                    self.jvm_instructions.extend(instrs);
                }

                // 2. Load array reference
                // Use the full array type when loading the variable
                let array_operand = oomir::Operand::Variable {
                    name: array.clone(),
                    ty: array_type,
                };
                self.load_operand(&array_operand)?; // Stack: [arrayref]

                // 3. Load value onto the stack
                self.load_operand(index)?; // Stack: [arrayref, index_int]

                // 4. Load value onto the stack
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

                // 6. if it's a string, we need to convert it back
                if is_string {
                    let instrs = get_cast_instructions(
                        &oomir::Type::Array(Box::new(oomir::Type::I16)),
                        &oomir::Type::String,
                        &mut self.constant_pool,
                    )
                    .unwrap();
                    self.jvm_instructions.extend(instrs);
                }
            }
            OI::ArrayGet { dest, array, index } => {
                // 1. Load array reference
                self.load_operand(&array)?; // Stack: [arrayref]

                // 2. Determine thw element type by inspecting the array operand's type
                let array_operand_type = match &array {
                    OO::Variable { ty, .. } => ty,
                    OO::Constant(c) => match c.clone() {
                        // Need the type representation for a constant array, e.g.,
                        oomir::Constant::Array(inner_ty, _) => {
                            &oomir::Type::Array(inner_ty.clone())
                        }
                        oomir::Constant::String(_) => {
                            let instrs = get_cast_instructions(
                                &oomir::Type::String,
                                &oomir::Type::Array(Box::new(oomir::Type::I16)),
                                &mut self.constant_pool,
                            )
                            .unwrap();
                            self.jvm_instructions.extend(instrs);
                            &oomir::Type::Array(Box::new(oomir::Type::I16))
                        }
                        _ => {
                            return Err(jvm::Error::VerificationError {
                                context: format!("Function {}", self.oomir_func.name),
                                message: format!(
                                    "Operand {:?} used in ArrayGet is not an array type, found {:?}",
                                    array, c
                                ),
                            });
                        }
                    },
                };

                // Now extract the element type *from* the array type
                let element_type = match array_operand_type {
                    oomir::Type::Array(inner_type) | oomir::Type::MutableReference(inner_type) => {
                        // inner_type is likely Box<oomir::Type>, so deref it
                        (**inner_type).clone()
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Operand {:?} used in ArrayGet is not an array type, found {:?}",
                                array, array_operand_type
                            ),
                        });
                    }
                };

                // 3. Load index
                self.load_operand(index)?; // Stack: [arrayref, index_int]

                // 4. Get and add the appropriate array load instruction
                let load_instr = element_type // Now correctly holds I64, I32, Class(...), etc.
                    .get_jvm_array_load_instruction() // Should now return laload, iaload, aaload correctly
                    .ok_or_else(|| jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!(
                            "Cannot determine array load instruction for element type: {:?}",
                            element_type // Use the correct element type in error message
                        ),
                    })?;
                self.jvm_instructions.push(load_instr); // Pushes the correct instruction (e.g., laload)
                // Stack: [value] (long value in this case)

                // 5. Store the resulting element (which has the correct element_type)
                // store_result now receives I64 and should generate lstore correctly.
                self.store_result(dest, &element_type)?; // Stack: []
            }
            OI::Length { dest, array } => {
                // 1. Load the array reference onto the stack
                self.load_operand(array)?; // Stack: [arrayref]

                // 2. Verify that the operand is an array type
                let array_actual_type = get_operand_type(array);
                match &array_actual_type {
                    oomir::Type::Array(_) | oomir::Type::MutableReference(_) => { /* Okay */ }
                    oomir::Type::String => {
                        // covert
                        let instrs = get_cast_instructions(
                            &oomir::Type::String,
                            &oomir::Type::Array(Box::new(oomir::Type::I16)),
                            &mut self.constant_pool,
                        )
                        .unwrap();
                        self.jvm_instructions.extend(instrs);
                    }
                    _ => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Operand used in Length instruction is not an array type, found {:?}",
                                array_actual_type
                            ),
                        });
                    }
                };

                // 3. Emit 'arraylength' instruction
                //    This consumes the arrayref and pushes the length (int)
                self.jvm_instructions.push(JI::Arraylength); // Stack: [length_int]
                let dest_type = oomir::Type::I32;

                self.store_result(dest, &dest_type)?; // Stack: []
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
                object,
                field_name,
                value,
                field_ty,
                owner_class, // Class where the field is *defined*
            } => {
                // 1. Get the type of the object variable itself (should be a Class type)
                let object_actual_type = self.get_local_type(object)?.clone();

                // We don't strictly *need* object_actual_type for the load instruction itself
                // if get_load_instruction correctly handles all reference types with Aload,
                // but it's good practice to verify it's a reference type.
                if !object_actual_type.is_jvm_reference_type() {
                    return Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!(
                            "Variable '{}' used in SetField is not a reference type, found {:?}",
                            object, object_actual_type
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
                let object_var_index = self.get_local_index(object)?;
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
                object,
                field_name,
                field_ty,    // Type of the field *value* being retrieved
                owner_class, // Class where the field is *defined*
            } => {
                // 1. Get the type of the object operand itself
                let object_actual_type = get_operand_type(object);

                if !object_actual_type.is_jvm_reference_type() {
                    return Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!(
                            "Operand used in GetField is not a reference type, found {:?}",
                            object_actual_type
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
                self.load_operand(object)?; // Stack: [object_ref]

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
                let instructions = get_cast_instructions(&old_ty, ty, self.constant_pool)?;

                // 3. Emit the cast instructions
                for instr in instructions {
                    self.jvm_instructions.push(instr);
                }

                // 4. Store the casted value into the destination variable
                //    The type for storage is the new type (ty)
                self.store_result(dest, ty)?; // Stack: []
            }

            OI::InvokeInterface {
                class_name,
                method_name,
                method_ty,
                args,
                dest,
                operand,
            } => {
                // 1. Add Method reference to constant pool
                let class_index = self.constant_pool.add_class(class_name)?;
                let method_ref_index = self.constant_pool.add_interface_method_ref(
                    class_index,
                    method_name,
                    &method_ty.to_string(),
                )?;

                // 2.1 load the operand we're calling this method on
                self.load_operand(operand)?; // Stack: [object_ref]

                // 2.2 Load arguments onto the stack
                for arg in args {
                    self.load_call_argument(arg)?; // Use helper to handle references properly
                    // stack: [object_ref, args...]
                }

                // 3. Emit 'invokeinterface' instruction
                self.jvm_instructions
                    .push(JI::Invokeinterface(method_ref_index, args.len() as u8 + 1)); // Stack: [result]

                // 4. Handle the return value
                if let Some(dest_var) = dest {
                    // Store the result in the destination variable
                    self.store_result(dest_var, &method_ty.ret)?;
                } else if *method_ty.ret.as_ref() != oomir::Type::Void {
                    // Pop the result if it's not void and no destination is provided
                    match get_type_size(&method_ty.ret) {
                        1 => self.jvm_instructions.push(JI::Pop),
                        2 => self.jvm_instructions.push(JI::Pop2),
                        _ => {}
                    }
                }
            }

            OI::InvokeVirtual {
                dest,
                class_name,
                method_name,
                method_ty,
                args,
                operand,
            } => {
                // 1. Add Method reference to constant pool
                let class_index = self.constant_pool.add_class(class_name)?;
                let method_ref_index = self.constant_pool.add_method_ref(
                    class_index,
                    method_name,
                    &method_ty.to_string(),
                )?;

                // 2. Load the object reference (self) onto the stack
                // The object reference is the first argument for instance methods
                self.load_operand(operand)?; // Stack: [object_ref]

                // 3. Load arguments onto the stack
                for arg in args {
                    self.load_call_argument(arg)?; // Use helper to handle references properly
                }

                // 4. Emit 'invokevirtual' instruction
                self.jvm_instructions
                    .push(JI::Invokevirtual(method_ref_index)); // Stack: [result]
                // Note: The result type is determined by the method signature

                // 5. Handle the return value
                if let Some(dest_var) = dest {
                    // Store the result in the destination variable
                    self.store_result(dest_var, &method_ty.ret)?;
                } else if *method_ty.ret.as_ref() != oomir::Type::Void {
                    // Pop the result if it's not void and no destination is provided
                    match get_type_size(&method_ty.ret) {
                        1 => self.jvm_instructions.push(JI::Pop),
                        2 => self.jvm_instructions.push(JI::Pop2),
                        _ => {}
                    }
                }
            }
        }
        Ok(())
    }

    /// Helper to load an operand specifically for a function call argument.
    /// Handles Reference/MutableReference
    fn load_call_argument(&mut self, operand: &oomir::Operand) -> Result<(), jvm::Error> {
        match operand {
            oomir::Operand::Variable { name: var_name, ty } => {
                let index = self.get_local_index(var_name)?;
                let expected_type = self.get_local_type(var_name)?.clone();
                // Decide which type to use for loading based on whether it's a Ref to Primitive
                let load_type = match ty {
                    // If the argument is declared as Ref<Primitive>, load the primitive directly
                    oomir::Type::Reference(box inner_ty) if inner_ty.is_jvm_primitive() => {
                        inner_ty // Use the inner type for loading
                    }
                    oomir::Type::MutableReference(box inner_ty) => {
                        // check the expected type
                        match expected_type {
                            oomir::Type::MutableReference(_) => ty,
                            _ => {
                                // it's not a MutableReference, so we need to move the value out
                                self.jvm_instructions.push(get_load_instruction(ty, index)?);
                                self.jvm_instructions.push(Instruction::Iconst_0);
                                self.jvm_instructions.push(Instruction::Aaload);
                                inner_ty
                            }
                        }
                    }
                    // Otherwise, use the declared type
                    _ => ty,
                };

                let load_instr = get_load_instruction(load_type, index)?;
                self.jvm_instructions.push(load_instr);
            }
            oomir::Operand::Constant(c) => {
                // Constants are loaded directly, no special handling needed here for refs
                load_constant(&mut self.jvm_instructions, &mut self.constant_pool, c)?;
            }
        }
        Ok(())
    }
}
