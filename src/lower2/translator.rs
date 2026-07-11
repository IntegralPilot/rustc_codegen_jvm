use super::{
    constant_pool::InternedConstantPool,
    consts::{get_int_const_instr, load_constant},
    helpers::{
        get_cast_instructions, get_load_instruction, get_operand_type, get_store_instruction,
        get_type_size, parse_jvm_descriptor_params,
    },
    optimise2,
    shim::{ShimInfo, get_shim_metadata},
    stackmaps,
};
use crate::oomir::{self, Type};

use ristretto_classfile::attributes::{ArrayType, Instruction};
use ristretto_classfile::{self as jvm};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::convert::TryInto;
use std::io::Cursor;

use super::{BIG_DECIMAL_CLASS, BIG_INTEGER_CLASS, F128_CLASS};

/// Represents the state during the translation of a single function's body.
pub struct FunctionTranslator<'a, 'cp> {
    module: &'a oomir::Module,
    oomir_func: &'a oomir::Function,
    constant_pool: &'cp mut InternedConstantPool,

    local_var_map: HashMap<String, u16>, // OOMIR var name -> JVM local index
    local_var_types: HashMap<String, oomir::Type>, // OOMIR var name -> OOMIR Type
    typed_local_var_map: HashMap<(String, oomir::Type), u16>,
    next_local_index: u16,
    jvm_instructions: Vec<jvm::attributes::Instruction>,
    label_to_instr_index: HashMap<String, u16>, // OOMIR label -> JVM instruction index
    // Store (instruction_index_to_patch, target_label) for fixups
    branch_fixups: Vec<(usize, String)>,
    switch_fixups: Vec<SwitchFixup>,
    current_oomir_block_label: String, // For error reporting maybe
    current_fallthrough_block_label: Option<String>,
    initial_locals: Vec<stackmaps::FrameValue>,

    // For max_locals calculation - track highest index used + size
    max_locals_used: u16,
}

struct SwitchFixup {
    instruction_index: usize,
    default_label: String,
    kind: SwitchFixupKind,
}

enum SwitchFixupKind {
    Table { target_labels: Vec<String> },
    Lookup { target_labels: Vec<(i32, String)> },
}

impl<'a, 'cp> FunctionTranslator<'a, 'cp> {
    pub fn new(
        oomir_func: &'a oomir::Function,
        constant_pool: &'cp mut InternedConstantPool,
        module: &'a oomir::Module,
        is_static: bool,
        owner_class_name: Option<&str>,
    ) -> Self {
        let mut translator = FunctionTranslator {
            oomir_func,
            module,
            constant_pool,
            local_var_map: HashMap::new(),
            local_var_types: HashMap::new(),
            typed_local_var_map: HashMap::new(),
            next_local_index: if is_static { 0 } else { 1 },
            jvm_instructions: Vec::new(),
            label_to_instr_index: HashMap::new(),
            branch_fixups: Vec::new(),
            switch_fixups: Vec::new(),
            current_oomir_block_label: String::new(),
            current_fallthrough_block_label: None,
            initial_locals: stackmaps::initial_locals_for_oomir_function(
                oomir_func,
                is_static,
                owner_class_name,
            ),
            max_locals_used: 0,
        };

        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!("static: {}, function_name: {}", is_static, oomir_func.name)
        );

        // For instance methods, map _1 (self) to JVM Slot 0
        if !is_static {
            if let Some(class_name) = owner_class_name {
                // _1 is the receiver (this), maps to JVM Slot 0
                translator.local_var_map.insert("_1".to_string(), 0);
                translator
                    .local_var_types
                    .insert("_1".to_string(), Type::Class(class_name.to_string()));
                translator
                    .typed_local_var_map
                    .insert(("_1".to_string(), Type::Class(class_name.to_string())), 0);
                translator.max_locals_used = translator.max_locals_used.max(1);

                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "bytecode-gen",
                    format!(
                        "Mapped _1 (self) to JVM Slot 0 with type Class({})",
                        class_name
                    )
                );
            }
        }

        // Assign JVM local slots to MIR argument names
        let num_params = oomir_func.signature.params.len();
        let first_explicit_param = if is_static { 0 } else { 1 };
        for i in first_explicit_param..num_params {
            // Internal name for translator logic
            let param_translator_name: String = format!("param_{}", i);
            // Signature params are aligned with MIR locals: param[0] is _1.
            // For instance methods, _1 is the implicit JVM receiver in slot 0.
            let param_oomir_name = format!("_{}", i + 1);
            let (_param_name, param_ty) = &oomir_func.signature.params[i];
            let is_synthetic_jvm_main_arg = is_static
                && oomir_func.name == "main"
                && i == 0
                && matches!(
                    param_ty,
                    Type::Array(inner)
                        if matches!(inner.as_ref(), Type::Class(name) if name == "java/lang/String")
                );

            // Use assign_local to allocate the slot
            let assigned_index = translator.assign_local(param_translator_name.as_str(), param_ty);

            if is_synthetic_jvm_main_arg {
                continue;
            }

            // Map the OOMIR name to the same slot index
            if translator
                .local_var_map
                .insert(param_oomir_name.clone(), assigned_index)
                .is_some()
            {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "bytecode-gen",
                    format!(
                        "Warning: OOMIR parameter name '{}' clashed with an existing mapping during parameter assignment.",
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
                        "Warning: OOMIR parameter name '{}' clashed with an existing type mapping during parameter assignment.",
                        param_oomir_name
                    )
                );
            }
            translator
                .typed_local_var_map
                .insert((param_oomir_name, param_ty.clone()), assigned_index);
        }

        translator
    }

    fn emit_integer_switch(
        &mut self,
        discr: &oomir::Operand,
        discr_type: &Type,
        targets: &[(oomir::Constant, String)],
        otherwise: &str,
    ) -> Result<bool, jvm::Error> {
        if !is_jvm_switch_type(discr_type) || targets.len() < 3 {
            return Ok(false);
        }

        let context = format!("Function {}", self.oomir_func.name);
        let mut cases = BTreeMap::new();
        for (constant_key, target_label) in targets {
            let key = jvm_switch_key(discr_type, constant_key, &context)?;
            if let Some(existing_target) = cases.insert(key, target_label.clone())
                && existing_target != *target_label
            {
                return Err(jvm::Error::VerificationError {
                    context,
                    message: format!(
                        "Switch has duplicate key {key} with targets {existing_target} and {target_label}"
                    ),
                });
            }
        }

        if cases.len() < 3 {
            return Ok(false);
        }

        let low = *cases.keys().next().expect("switch cases are non-empty");
        let high = *cases
            .keys()
            .next_back()
            .expect("switch cases are non-empty");
        let span = i64::from(high) - i64::from(low) + 1;
        let Ok(span) = usize::try_from(span) else {
            return Ok(false);
        };
        let table_payload_bytes = 12usize.saturating_add(span.saturating_mul(4));
        let lookup_payload_bytes = 8usize.saturating_add(cases.len().saturating_mul(8));
        let use_table = table_payload_bytes <= lookup_payload_bytes;

        self.load_operand(discr)?;
        let instruction_index = self.jvm_instructions.len();
        if use_table {
            let default_label = otherwise.to_string();
            let target_labels = (low..=high)
                .map(|key| {
                    cases
                        .get(&key)
                        .cloned()
                        .unwrap_or_else(|| default_label.clone())
                })
                .collect::<Vec<_>>();
            self.jvm_instructions.push(Instruction::Tableswitch {
                default: 0,
                low,
                high,
                offsets: vec![0; span],
            });
            self.switch_fixups.push(SwitchFixup {
                instruction_index,
                default_label,
                kind: SwitchFixupKind::Table { target_labels },
            });
        } else {
            let target_labels = cases.into_iter().collect::<Vec<_>>();
            self.jvm_instructions.push(Instruction::Lookupswitch {
                default: 0,
                pairs: target_labels.iter().map(|(key, _)| (*key, 0)).collect(),
            });
            self.switch_fixups.push(SwitchFixup {
                instruction_index,
                default_label: otherwise.to_string(),
                kind: SwitchFixupKind::Lookup { target_labels },
            });
        }

        Ok(true)
    }

    fn emit_iinc_add(
        &mut self,
        dest: &str,
        op1: &oomir::Operand,
        op2: &oomir::Operand,
    ) -> Result<bool, jvm::Error> {
        if self.emit_iinc_update(dest, op1, op2, 1)? {
            return Ok(true);
        }
        self.emit_iinc_update(dest, op2, op1, 1)
    }

    fn emit_iinc_sub(
        &mut self,
        dest: &str,
        op1: &oomir::Operand,
        op2: &oomir::Operand,
    ) -> Result<bool, jvm::Error> {
        self.emit_iinc_update(dest, op1, op2, -1)
    }

    fn emit_iinc_update(
        &mut self,
        dest: &str,
        local_operand: &oomir::Operand,
        amount_operand: &oomir::Operand,
        amount_sign: i32,
    ) -> Result<bool, jvm::Error> {
        let oomir::Operand::Variable { name, ty } = local_operand else {
            return Ok(false);
        };
        if name != dest || *ty != Type::I32 {
            return Ok(false);
        }

        let Some(amount) = iinc_amount(amount_operand, amount_sign) else {
            return Ok(false);
        };
        let Some(local_index) = self
            .typed_local_var_map
            .get(&(dest.to_string(), Type::I32))
            .copied()
        else {
            return Ok(false);
        };

        if amount != 0 {
            self.jvm_instructions
                .push(make_iinc_instruction(local_index, amount));
        }
        self.max_locals_used = self.max_locals_used.max(local_index + 1);
        self.local_var_types.insert(dest.to_string(), Type::I32);
        Ok(true)
    }

    fn apply_switch_fixup(&mut self, fixup: SwitchFixup) -> Result<(), jvm::Error> {
        let default_target =
            self.label_instruction_delta_i32(fixup.instruction_index, &fixup.default_label)?;
        match fixup.kind {
            SwitchFixupKind::Table { target_labels } => {
                let patched_offsets = target_labels
                    .iter()
                    .map(|target_label| {
                        self.label_instruction_delta_i32(fixup.instruction_index, target_label)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                match self.jvm_instructions.get_mut(fixup.instruction_index) {
                    Some(Instruction::Tableswitch {
                        default, offsets, ..
                    }) if offsets.len() == patched_offsets.len() => {
                        *default = default_target;
                        *offsets = patched_offsets;
                    }
                    Some(_) => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Switch fixup expected a tableswitch instruction at index {}",
                                fixup.instruction_index
                            ),
                        });
                    }
                    None => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Switch fixup index {} is out of bounds",
                                fixup.instruction_index
                            ),
                        });
                    }
                }
            }
            SwitchFixupKind::Lookup { target_labels } => {
                let patched_pairs = target_labels
                    .into_iter()
                    .map(|(key, target_label)| {
                        self.label_instruction_delta_i32(fixup.instruction_index, &target_label)
                            .map(|target| (key, target))
                    })
                    .collect::<Result<_, _>>()?;
                match self.jvm_instructions.get_mut(fixup.instruction_index) {
                    Some(Instruction::Lookupswitch { default, pairs }) => {
                        *default = default_target;
                        *pairs = patched_pairs;
                    }
                    Some(_) => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Switch fixup expected a lookupswitch instruction at index {}",
                                fixup.instruction_index
                            ),
                        });
                    }
                    None => {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Switch fixup index {} is out of bounds",
                                fixup.instruction_index
                            ),
                        });
                    }
                }
            }
        }
        Ok(())
    }

    fn label_instruction_delta_i32(
        &self,
        source_index: usize,
        target_label: &str,
    ) -> Result<i32, jvm::Error> {
        let target_instr_index = *self.label_to_instr_index.get(target_label).ok_or_else(|| {
            jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!("Switch target label not found: {target_label}"),
            }
        })?;
        i32::try_from(i64::from(target_instr_index) - source_index as i64).map_err(|_| {
            jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!(
                    "Switch target delta from instruction {source_index} to label {target_label} overflowed"
                ),
            }
        })
    }

    fn layout_block_order(&self) -> Result<Vec<String>, jvm::Error> {
        let mut order = Vec::new();
        let mut visited = HashSet::new();
        let mut stack = vec![self.oomir_func.body.entry.clone()];

        while let Some(block_label) = stack.pop() {
            if !visited.insert(block_label.clone()) {
                continue;
            }

            let block = self
                .oomir_func
                .body
                .basic_blocks
                .get(&block_label)
                .ok_or_else(|| jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!("Basic block label not found: {block_label}"),
                })?;

            order.push(block_label);

            for successor in layout_successors(block).into_iter().rev() {
                if !visited.contains(&successor) {
                    stack.push(successor);
                }
            }
        }

        Ok(order)
    }

    fn assign_local(&mut self, var_name: &str, ty: &oomir::Type) -> u16 {
        let key = (var_name.to_string(), ty.clone());
        if let Some(index) = self.typed_local_var_map.get(&key).copied() {
            self.local_var_map.insert(var_name.to_string(), index);
            self.local_var_types
                .insert(var_name.to_string(), ty.clone());
            return index;
        }

        let index = self.next_local_index;
        let size = get_type_size(ty);
        self.next_local_index += size;
        self.max_locals_used = self.max_locals_used.max(index + size);
        self.typed_local_var_map.insert(key, index);
        self.local_var_map.insert(var_name.to_string(), index);
        self.local_var_types
            .insert(var_name.to_string(), ty.clone());
        index
    }

    /// Gets the slot index for a variable, assigning if new.
    fn get_or_assign_local(&mut self, var_name: &str, ty_hint: &oomir::Type) -> u16 {
        if let Some(index) = self
            .typed_local_var_map
            .get(&(var_name.to_string(), ty_hint.clone()))
            .copied()
        {
            self.local_var_map.insert(var_name.to_string(), index);
            self.local_var_types
                .insert(var_name.to_string(), ty_hint.clone());
            index
        } else if let (Some(index), Some(current_ty)) = (
            self.local_var_map.get(var_name).copied(),
            self.local_var_types.get(var_name).cloned(),
        ) && Self::can_share_jvm_local(&current_ty, ty_hint)
        {
            self.typed_local_var_map
                .insert((var_name.to_string(), ty_hint.clone()), index);
            self.local_var_types
                .insert(var_name.to_string(), ty_hint.clone());
            index
        } else {
            self.assign_local(var_name, ty_hint)
        }
    }

    fn can_share_jvm_local(existing: &oomir::Type, new: &oomir::Type) -> bool {
        existing == new
            || (existing.is_jvm_reference_type() && new.is_jvm_reference_type())
            || (matches!(
                existing,
                Type::I8 | Type::I16 | Type::I32 | Type::Boolean | Type::Char
            ) && matches!(
                new,
                Type::I8 | Type::I16 | Type::I32 | Type::Boolean | Type::Char
            ))
    }

    fn get_typed_local_index(&self, var_name: &str, ty: &oomir::Type) -> Option<u16> {
        self.typed_local_var_map
            .get(&(var_name.to_string(), ty.clone()))
            .copied()
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
    pub fn translate(
        mut self,
    ) -> Result<
        (
            Vec<jvm::attributes::Instruction>,
            u16,
            Vec<jvm::attributes::Attribute>,
        ),
        jvm::Error,
    > {
        let block_order = self.layout_block_order()?;

        for (block_order_index, block_label) in block_order.iter().enumerate() {
            let block = self
                .oomir_func
                .body
                .basic_blocks
                .get(block_label)
                .ok_or_else(|| jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!("Basic block label not found: {}", block_label),
                })?;

            self.current_oomir_block_label = block_label.clone();
            self.current_fallthrough_block_label = block_order
                .get(block_order_index + 1)
                .map(|label| label.to_string());

            // Record the start instruction index for this block label
            let start_instr_index = self.jvm_instructions.len().try_into().unwrap();
            self.label_to_instr_index
                .insert(block_label.clone(), start_instr_index);

            // Translate instructions in the block
            for instr in &block.instructions {
                self.translate_instruction(self.module, instr)?;
            }

            if block.instructions.is_empty() && self.oomir_func.body.basic_blocks.len() > 1 {
                // Empty block needs explicit jump?
                return Err(jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!("Non-terminal empty basic block '{}' found", block_label),
                });
            }
        }
        self.current_fallthrough_block_label = None;

        let branch_fixups = std::mem::take(&mut self.branch_fixups);
        for (instr_index, target_label) in branch_fixups {
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

        let switch_fixups = std::mem::take(&mut self.switch_fixups);
        for fixup in switch_fixups {
            self.apply_switch_fixup(fixup)?;
        }

        for (instr_index, instruction) in self.jvm_instructions.iter_mut().enumerate() {
            if matches!(instruction, Instruction::Goto(target) if usize::from(*target) == instr_index + 1)
            {
                *instruction = Instruction::Nop;
            }
        }

        let local_hints = stackmaps::local_hints_for_oomir_locals(
            &self.typed_local_var_map,
            self.max_locals_used,
        );
        let fixed_prefix_slots = self.initial_locals.len() as u16;
        let optimised = optimise2::optimise(
            std::mem::take(&mut self.jvm_instructions),
            self.max_locals_used,
            fixed_prefix_slots,
        )
        .map_err(|error| jvm::Error::VerificationError {
            context: format!("Function {}", self.oomir_func.name),
            message: format!("Failed to run optimise2: {error:?}"),
        })?;
        self.jvm_instructions = optimised.instructions;
        self.max_locals_used = optimised.max_locals;

        self.widen_branches()
            .map_err(|error| jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!("Failed to widen branches: {error:?}"),
            })?;

        let local_hints = optimise2::remap_frame_values(
            &local_hints,
            &optimised.local_slot_map,
            self.max_locals_used,
        );
        let stack_map_attributes = stackmaps::build_stack_map_attributes(
            &self.jvm_instructions,
            &self.initial_locals,
            &local_hints,
            self.max_locals_used,
            self.constant_pool,
            &format!("Function {}", self.oomir_func.name),
        )
        .map_err(|error| jvm::Error::VerificationError {
            context: format!("Function {}", self.oomir_func.name),
            message: format!("Failed to build StackMapTable: {error:?}"),
        })?;

        Ok((
            self.jvm_instructions,
            self.max_locals_used,
            stack_map_attributes,
        ))
    }

    fn widen_branches(&mut self) -> Result<(), jvm::Error> {
        loop {
            let byte_offsets = instruction_byte_offsets(&self.jvm_instructions)?;
            let mut changed = false;

            for index in 0..self.jvm_instructions.len() {
                if let Some(original_target) =
                    conditional_branch_target(&self.jvm_instructions[index])
                {
                    if branch_offset_fits_i16(&byte_offsets, index, usize::from(original_target)) {
                        continue;
                    }

                    let insert_at = index + 1;
                    self.retarget_after_insert(insert_at)?;
                    let adjusted_target = if usize::from(original_target) >= insert_at {
                        original_target.checked_add(1).ok_or_else(|| {
                            jvm::Error::VerificationError {
                                context: format!("Function {}", self.oomir_func.name),
                                message: "Conditional branch target overflow during widening"
                                    .to_string(),
                            }
                        })?
                    } else {
                        original_target
                    };
                    let skip_wide_goto =
                        u16::try_from(index + 2).map_err(|_| jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: "Conditional branch skip target overflow during widening"
                                .to_string(),
                        })?;

                    self.jvm_instructions[index] =
                        invert_conditional_branch(&self.jvm_instructions[index], skip_wide_goto)
                            .ok_or_else(|| jvm::Error::VerificationError {
                                context: format!("Function {}", self.oomir_func.name),
                                message: "Expected conditional branch during widening".to_string(),
                            })?;
                    self.jvm_instructions
                        .insert(insert_at, Instruction::Goto_w(i32::from(adjusted_target)));
                    changed = true;
                    break;
                }

                if let Instruction::Goto(target) = self.jvm_instructions[index]
                    && !branch_offset_fits_i16(&byte_offsets, index, usize::from(target))
                {
                    self.jvm_instructions[index] = Instruction::Goto_w(i32::from(target));
                    changed = true;
                    break;
                }
            }

            if !changed {
                break;
            }
        }
        Ok(())
    }

    fn retarget_after_insert(&mut self, insert_at: usize) -> Result<(), jvm::Error> {
        let context = format!("Function {}", self.oomir_func.name);
        for (instruction_index, instruction) in self.jvm_instructions.iter_mut().enumerate() {
            match instruction {
                Instruction::Ifeq(target)
                | Instruction::Ifne(target)
                | Instruction::Iflt(target)
                | Instruction::Ifge(target)
                | Instruction::Ifgt(target)
                | Instruction::Ifle(target)
                | Instruction::If_icmpeq(target)
                | Instruction::If_icmpne(target)
                | Instruction::If_icmplt(target)
                | Instruction::If_icmpge(target)
                | Instruction::If_icmpgt(target)
                | Instruction::If_icmple(target)
                | Instruction::If_acmpeq(target)
                | Instruction::If_acmpne(target)
                | Instruction::Goto(target)
                | Instruction::Ifnull(target)
                | Instruction::Ifnonnull(target) => {
                    bump_u16_branch_target(target, insert_at, &context)?;
                }
                Instruction::Goto_w(target) => {
                    if *target >= insert_at as i32 {
                        *target += 1;
                    }
                }
                Instruction::Tableswitch {
                    default, offsets, ..
                } => {
                    bump_i32_relative_switch_target(
                        default,
                        instruction_index,
                        insert_at,
                        &context,
                    )?;
                    for target in offsets {
                        bump_i32_relative_switch_target(
                            target,
                            instruction_index,
                            insert_at,
                            &context,
                        )?;
                    }
                }
                Instruction::Lookupswitch { default, pairs } => {
                    bump_i32_relative_switch_target(
                        default,
                        instruction_index,
                        insert_at,
                        &context,
                    )?;
                    for target in pairs.values_mut() {
                        bump_i32_relative_switch_target(
                            target,
                            instruction_index,
                            insert_at,
                            &context,
                        )?;
                    }
                }
                _ => {}
            }
        }
        Ok(())
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
    fn zero_sized_class_name(&self, ty: &oomir::Type) -> Option<String> {
        let oomir::Type::Class(class_name) = ty else {
            return None;
        };
        match self.module.data_types.get(class_name) {
            Some(oomir::DataType::Class {
                fields,
                is_abstract: false,
                ..
            }) if fields.iter().all(|(_, field_ty)| !field_ty.has_jvm_value()) => {
                Some(class_name.clone())
            }
            _ => None,
        }
    }

    fn construct_zero_sized_class_value(&mut self, class_name: &str) -> Result<(), jvm::Error> {
        let class_index = self.constant_pool.add_class(class_name)?;
        let constructor = self
            .constant_pool
            .add_method_ref(class_index, "<init>", "()V")?;
        self.jvm_instructions.push(Instruction::New(class_index));
        self.jvm_instructions.push(Instruction::Dup);
        self.jvm_instructions
            .push(Instruction::Invokespecial(constructor));
        Ok(())
    }

    fn materialize_zero_sized_local(
        &mut self,
        var_name: &str,
        ty: &oomir::Type,
    ) -> Result<bool, jvm::Error> {
        if self.get_typed_local_index(var_name, ty).is_some() {
            return Ok(false);
        }
        let Some(class_name) = self.zero_sized_class_name(ty) else {
            return Ok(false);
        };
        self.construct_zero_sized_class_value(&class_name)?;
        self.store_result(var_name, ty)?;
        Ok(true)
    }

    fn load_operand(&mut self, operand: &oomir::Operand) -> Result<(), jvm::Error> {
        if operand
            .get_type()
            .is_some_and(|operand_ty| !operand_ty.has_jvm_value())
        {
            return Ok(());
        }
        match operand {
            oomir::Operand::Constant(c) => {
                load_constant(&mut self.jvm_instructions, &mut self.constant_pool, c)?
            }
            oomir::Operand::Variable { name: var_name, ty } => {
                self.materialize_zero_sized_local(var_name, ty)?;
                let (index, actual_ty) =
                    if let Some(index) = self.get_typed_local_index(var_name, ty) {
                        (index, ty.clone())
                    } else {
                        let mut actual_ty = self
                            .local_var_types
                            .get(var_name)
                            .cloned()
                            .unwrap_or_else(|| ty.clone());
                        if !actual_ty.has_jvm_value() && ty.has_jvm_value() {
                            actual_ty = ty.clone();
                        }
                        let index = self.get_or_assign_local(var_name, &actual_ty);
                        (index, actual_ty)
                    };
                let load_instr = get_load_instruction(&actual_ty, index)?;
                self.jvm_instructions.push(load_instr);
            }
        }
        Ok(())
    }

    fn load_operand_as(
        &mut self,
        operand: &oomir::Operand,
        expected_ty: &oomir::Type,
    ) -> Result<(), jvm::Error> {
        let actual_ty = get_operand_type(operand);
        if !expected_ty.has_jvm_value() {
            return Ok(());
        }
        if actual_ty == oomir::Type::Str && *expected_ty == oomir::Type::String {
            self.load_operand(operand)?;
            return self.adapt_loaded_str_to_java_string();
        }
        if actual_ty == oomir::Type::String && *expected_ty == oomir::Type::Str {
            self.load_operand(operand)?;
            return self.adapt_loaded_java_string_to_str();
        }
        if matches!(actual_ty, oomir::Type::Slice(_))
            && expected_ty.to_jvm_descriptor().starts_with('[')
        {
            self.load_operand(operand)?;
            return self.adapt_loaded_slice_to_array(&expected_ty.to_jvm_descriptor());
        }
        if matches!(expected_ty, oomir::Type::MutableReference(inner) if !inner.has_jvm_value())
            && actual_ty != *expected_ty
        {
            if actual_ty.has_jvm_value() {
                self.load_operand(operand)?;
                self.jvm_instructions.extend(get_cast_instructions(
                    &actual_ty,
                    &oomir::Type::Class("java/lang/Object".to_string()),
                    self.constant_pool,
                )?);
            } else {
                self.construct_zero_sized_class_value("java/lang/Object")?;
            }
            return Ok(());
        }
        if actual_ty != *expected_ty
            && let oomir::Type::Class(class_name) = expected_ty
            && oomir::is_non_null_class_name(class_name)
            && !matches!(operand, oomir::Operand::Constant(oomir::Constant::Null(_)))
        {
            return self.construct_non_null_wrapper_from_operand(operand, &actual_ty, class_name);
        }
        self.load_operand(operand)?;
        if actual_ty != *expected_ty
            && actual_ty.to_jvm_descriptor() != expected_ty.to_jvm_descriptor()
        {
            let cast_instructions =
                get_cast_instructions(&actual_ty, expected_ty, self.constant_pool)?;
            self.jvm_instructions.extend(cast_instructions);
        }
        Ok(())
    }

    fn construct_non_null_wrapper_from_operand(
        &mut self,
        operand: &oomir::Operand,
        operand_ty: &oomir::Type,
        class_name: &str,
    ) -> Result<(), jvm::Error> {
        let class_index = self.constant_pool.add_class(class_name)?;
        let constructor_descriptor = format!("({})V", operand_ty.to_jvm_descriptor());
        let constructor_ref_index =
            self.constant_pool
                .add_method_ref(class_index, "<init>", &constructor_descriptor)?;

        self.jvm_instructions.push(Instruction::New(class_index));
        self.jvm_instructions.push(Instruction::Dup);
        self.load_operand(operand)?;
        self.jvm_instructions
            .push(Instruction::Invokespecial(constructor_ref_index));
        Ok(())
    }

    /// Appends JVM instructions for storing the value currently on top of the stack
    /// into a local variable.
    fn store_result(&mut self, dest_var: &str, ty: &oomir::Type) -> Result<(), jvm::Error> {
        if !ty.has_jvm_value() {
            self.local_var_types
                .insert(dest_var.to_string(), ty.clone());
            self.typed_local_var_map
                .insert((dest_var.to_string(), ty.clone()), self.next_local_index);
            return Ok(());
        }
        // Assign or update the local variable slot with the provided type
        let index: u16 = self.get_or_assign_local(dest_var, ty);
        let store_instr = get_store_instruction(ty, index)?;
        self.jvm_instructions.push(store_instr);
        Ok(())
    }

    fn store_result_in_distinct_slot(
        &mut self,
        dest_var: &str,
        ty: &oomir::Type,
    ) -> Result<(), jvm::Error> {
        if !ty.has_jvm_value() {
            return self.store_result(dest_var, ty);
        }
        let index = self.assign_local(dest_var, ty);
        self.jvm_instructions
            .push(get_store_instruction(ty, index)?);
        Ok(())
    }

    fn emit_shim_call(
        &mut self,
        shim_key: &str,
        shim_info: &ShimInfo,
        args: &[oomir::Operand],
        dest: &Option<String>,
        expected_return_type: Option<&oomir::Type>,
        is_diverging_call: bool,
    ) -> Result<(), jvm::Error> {
        let expected_jvm_param_types =
            parse_jvm_descriptor_params(&shim_info.descriptor).map_err(|e| {
                jvm::Error::VerificationError {
                    context: format!("Function {}", self.oomir_func.name),
                    message: format!("Error parsing shim descriptor: {}", e),
                }
            })?;

        if args.len() != expected_jvm_param_types.len() {
            return Err(jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!(
                    "Shim argument count mismatch for '{}': descriptor '{}' expects {}, found {}",
                    shim_key,
                    shim_info.descriptor,
                    expected_jvm_param_types.len(),
                    args.len()
                ),
            });
        }

        for (arg_operand, expected_jvm_type) in args.iter().zip(expected_jvm_param_types.iter()) {
            let provided_oomir_type = get_operand_type(arg_operand);
            self.load_call_argument(arg_operand)?;
            self.adapt_loaded_shim_argument(&provided_oomir_type, expected_jvm_type)?;
        }

        let class_index = self
            .constant_pool
            .add_class(shim_info.java_class(shim_key))?;
        let method_ref_index = self.constant_pool.add_method_ref(
            class_index,
            shim_info.java_method(shim_key),
            &shim_info.descriptor,
        )?;

        if shim_info.is_static {
            self.jvm_instructions
                .push(Instruction::Invokestatic(method_ref_index));
        } else {
            return Err(jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!("Non-static shim function '{}' not supported", shim_key),
            });
        }

        if is_diverging_call {
            return Ok(());
        }

        let shim_return_type = oomir::Type::from_jvm_descriptor_return_type(&shim_info.descriptor);
        if shim_return_type == oomir::Type::Void {
            if dest.is_some() {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "bytecode-gen",
                    format!(
                        "Info: Ignoring store for void return from shim '{}'",
                        shim_key
                    )
                );
            }
            return Ok(());
        }

        let store_type = expected_return_type.unwrap_or(&shim_return_type);
        self.cast_loaded_shim_return_if_needed(&shim_return_type, store_type)?;

        if let Some(dest_var) = dest {
            self.store_result(dest_var, store_type)?;
        } else {
            match get_type_size(store_type) {
                1 => self.jvm_instructions.push(Instruction::Pop),
                2 => self.jvm_instructions.push(Instruction::Pop2),
                _ => {}
            }
        }

        Ok(())
    }

    fn emit_runtime_intrinsic_call(
        &mut self,
        function_name: &str,
        signature: &oomir::Signature,
        args: &[oomir::Operand],
        dest: &Option<String>,
    ) -> Result<bool, jvm::Error> {
        let is_from_utf8_unchecked = function_name == "from_utf8_unchecked"
            && args.len() == 1
            && signature.params.len() == 1
            && matches!(
                &signature.params[0].1,
                oomir::Type::Slice(element_type)
                    if matches!(element_type.as_ref(), oomir::Type::I16)
            )
            && matches!(signature.ret.as_ref(), oomir::Type::Str);
        if is_from_utf8_unchecked {
            self.load_call_argument(&args[0])?;
            let view_class = self.constant_pool.add_class(oomir::UTF8_VIEW_CLASS)?;
            let descriptor = format!(
                "(L{};)L{};",
                oomir::SLICE_VIEW_CLASS,
                oomir::UTF8_VIEW_CLASS
            );
            let method = self
                .constant_pool
                .add_method_ref(view_class, "fromSlice", descriptor)?;
            self.jvm_instructions
                .push(Instruction::Invokestatic(method));
            if let Some(dest) = dest {
                self.store_result(dest, &oomir::Type::Str)?;
            } else {
                self.jvm_instructions.push(Instruction::Pop);
            }
            return Ok(true);
        }

        let is_encode_utf8 = function_name == "encode_utf8_raw"
            && args.len() == 2
            && signature.params.len() == 2
            && signature.params[0].1 == oomir::Type::I64
            && matches!(
                &signature.params[1].1,
                oomir::Type::Slice(element_type)
                    if matches!(element_type.as_ref(), oomir::Type::I16)
            )
            && matches!(
                signature.ret.as_ref(),
                oomir::Type::Slice(element_type)
                    if matches!(element_type.as_ref(), oomir::Type::I16)
            );
        if !is_encode_utf8 {
            return Ok(false);
        }

        self.load_call_argument(&args[0])?;
        self.load_call_argument(&args[1])?;
        let runtime_class = self.constant_pool.add_class(oomir::SLICE_VIEW_CLASS)?;
        let descriptor = format!(
            "(JL{};)L{};",
            oomir::SLICE_VIEW_CLASS,
            oomir::SLICE_VIEW_CLASS
        );
        let method = self
            .constant_pool
            .add_method_ref(runtime_class, "encodeUtf8", &descriptor)?;
        self.jvm_instructions
            .push(Instruction::Invokestatic(method));

        if let Some(dest) = dest {
            self.store_result(dest, &signature.ret)?;
        } else {
            self.jvm_instructions.push(Instruction::Pop);
        }
        Ok(true)
    }

    fn adapt_loaded_shim_argument(
        &mut self,
        provided_oomir_type: &oomir::Type,
        expected_jvm_type: &str,
    ) -> Result<(), jvm::Error> {
        if *provided_oomir_type == oomir::Type::Str && expected_jvm_type == "Ljava/lang/String;" {
            return self.adapt_loaded_str_to_java_string();
        }
        if *provided_oomir_type == oomir::Type::Str && expected_jvm_type.starts_with('[') {
            let view_class = self.constant_pool.add_class(oomir::UTF8_VIEW_CLASS)?;
            let descriptor = format!(
                "(L{};)L{};",
                oomir::UTF8_VIEW_CLASS,
                oomir::SLICE_VIEW_CLASS
            );
            let as_slice = self
                .constant_pool
                .add_method_ref(view_class, "asSlice", descriptor)?;
            self.jvm_instructions
                .push(Instruction::Invokestatic(as_slice));
            return self.adapt_loaded_slice_to_array(expected_jvm_type);
        }
        if matches!(provided_oomir_type, oomir::Type::Slice(_))
            && expected_jvm_type.starts_with('[')
        {
            return self.adapt_loaded_slice_to_array(expected_jvm_type);
        }

        if expected_jvm_type == "Ljava/lang/Object;" && provided_oomir_type.is_jvm_primitive() {
            if let Some((wrapper_class, box_method, box_desc)) =
                provided_oomir_type.get_boxing_info()
            {
                let class_index = self.constant_pool.add_class(wrapper_class)?;
                let method_ref_index =
                    self.constant_pool
                        .add_method_ref(class_index, box_method, box_desc)?;
                self.jvm_instructions
                    .push(Instruction::Invokestatic(method_ref_index));
                return Ok(());
            }

            return Err(jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!(
                    "No boxing information found for type {:?}",
                    provided_oomir_type
                ),
            });
        }

        if expected_jvm_type.starts_with('L') && !expected_jvm_type.ends_with(';') {
            return Err(jvm::Error::VerificationError {
                context: format!("Function {}", self.oomir_func.name),
                message: format!(
                    "Invalid JVM descriptor for expected type: {}",
                    expected_jvm_type
                ),
            });
        }

        Ok(())
    }

    fn adapt_loaded_slice_to_array(&mut self, expected_jvm_type: &str) -> Result<(), jvm::Error> {
        let slice_class = self.constant_pool.add_class(oomir::SLICE_VIEW_CLASS)?;
        let to_array =
            self.constant_pool
                .add_method_ref(slice_class, "toArray", "()Ljava/lang/Object;")?;
        self.jvm_instructions
            .push(Instruction::Invokevirtual(to_array));

        let expected_array_class = self.constant_pool.add_class(expected_jvm_type)?;
        self.jvm_instructions
            .push(Instruction::Checkcast(expected_array_class));
        Ok(())
    }

    fn adapt_loaded_str_to_java_string(&mut self) -> Result<(), jvm::Error> {
        let view_class = self.constant_pool.add_class(oomir::UTF8_VIEW_CLASS)?;
        let descriptor = format!("(L{};)Ljava/lang/String;", oomir::UTF8_VIEW_CLASS);
        let method = self
            .constant_pool
            .add_method_ref(view_class, "toJavaString", descriptor)?;
        self.jvm_instructions
            .push(Instruction::Invokestatic(method));
        Ok(())
    }

    fn adapt_loaded_java_string_to_str(&mut self) -> Result<(), jvm::Error> {
        let view_class = self.constant_pool.add_class(oomir::UTF8_VIEW_CLASS)?;
        let descriptor = format!("(Ljava/lang/String;)L{};", oomir::UTF8_VIEW_CLASS);
        let method = self
            .constant_pool
            .add_method_ref(view_class, "fromJavaString", descriptor)?;
        self.jvm_instructions
            .push(Instruction::Invokestatic(method));
        Ok(())
    }

    fn cast_loaded_shim_return_if_needed(
        &mut self,
        actual_type: &oomir::Type,
        expected_type: &oomir::Type,
    ) -> Result<(), jvm::Error> {
        if actual_type == expected_type
            || actual_type.to_jvm_descriptor() == expected_type.to_jvm_descriptor()
            || *expected_type == oomir::Type::Void
        {
            return Ok(());
        }
        if *actual_type == oomir::Type::String && *expected_type == oomir::Type::Str {
            return self.adapt_loaded_java_string_to_str();
        }
        if *actual_type == oomir::Type::Str && *expected_type == oomir::Type::String {
            return self.adapt_loaded_str_to_java_string();
        }

        let Some(class_name) = expected_type.get_class_name() else {
            return Ok(());
        };
        let class_index = self.constant_pool.add_class(class_name)?;
        self.jvm_instructions
            .push(Instruction::Checkcast(class_index));
        Ok(())
    }

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

    fn translate_f128_binary_op(
        &mut self,
        dest: &str,
        op1: &oomir::Operand,
        op2: &oomir::Operand,
        method_name: &str,
    ) -> Result<(), jvm::Error> {
        self.load_operand(op1)?;
        self.load_operand(op2)?;
        let class = self.constant_pool.add_class(F128_CLASS)?;
        let descriptor = format!("(L{F128_CLASS};)L{F128_CLASS};");
        let method = self
            .constant_pool
            .add_method_ref(class, method_name, descriptor)?;
        self.jvm_instructions
            .push(Instruction::Invokevirtual(method));
        self.store_result(dest, &Type::Class(F128_CLASS.to_string()))
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
                    | Type::Str
                    | Type::String
                    | Type::Reference(_)
                    | Type::MutableReference(_)
                    | Type::Array(_)
                    | Type::Slice(_) => Ok((t1.clone(), None, None)), // Assume comparable for now, specific logic in main function handles details
                    Type::Unit | Type::Void => Err(jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!("Cannot compare void types"),
                    }),
                }
            }

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

        if op1_type == Type::Unit && op2_type == Type::Unit {
            let value = match comp_op {
                "eq" | "le" | "ge" => true,
                "ne" | "lt" | "gt" => false,
                _ => unreachable!(),
            };
            self.jvm_instructions.push(if value {
                Instruction::Iconst_1
            } else {
                Instruction::Iconst_0
            });
            self.store_result(dest, &Type::Boolean)?;
            return Ok(());
        }

        // Determine the type to compare operands as, and if casting is needed
        let (comparison_type, cast1_target, cast2_target) =
            self.determine_comparison_type(&op1_type, &op2_type)?;

        if comparison_type.is_jvm_reference_type()
            && matches!(comp_op, "eq" | "ne")
            && (is_null_operand(op1) ^ is_null_operand(op2))
        {
            let value_operand = if is_null_operand(op1) { op2 } else { op1 };
            self.load_operand(value_operand)?;
            let branch_constructor: Box<dyn Fn(u16) -> Instruction> = match comp_op {
                "eq" => Box::new(Instruction::Ifnull),
                "ne" => Box::new(Instruction::Ifnonnull),
                _ => unreachable!(),
            };
            self.materialize_boolean_from_branch(dest, branch_constructor)?;
            return Ok(());
        }

        if comparison_type == Type::Str && matches!(comp_op, "eq" | "ne") {
            self.load_operand(op1)?;
            self.load_operand(op2)?;
            let view_class = self.constant_pool.add_class(oomir::UTF8_VIEW_CLASS)?;
            let descriptor = format!(
                "(L{};L{};)Z",
                oomir::UTF8_VIEW_CLASS,
                oomir::UTF8_VIEW_CLASS
            );
            let equals = self
                .constant_pool
                .add_method_ref(view_class, "equals", descriptor)?;
            self.jvm_instructions
                .push(Instruction::Invokestatic(equals));
            if comp_op == "ne" {
                self.jvm_instructions.push(Instruction::Iconst_1);
                self.jvm_instructions.push(Instruction::Ixor);
            }
            self.store_result(dest, &Type::Boolean)?;
            return Ok(());
        }

        if comparison_type == Type::Class(F128_CLASS.to_string()) {
            self.load_operand(op1)?;
            self.load_operand(op2)?;
            let class = self.constant_pool.add_class(F128_CLASS)?;
            let descriptor = format!("(L{F128_CLASS};)Z");
            let method = self
                .constant_pool
                .add_method_ref(class, comp_op, descriptor)?;
            self.jvm_instructions
                .push(Instruction::Invokevirtual(method));
            self.store_result(dest, &Type::Boolean)?;
            return Ok(());
        }

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

        self.materialize_boolean_from_branch(dest, branch_constructor)?;

        Ok(())
    }

    fn materialize_boolean_from_branch<'branch>(
        &mut self,
        dest: &str,
        branch_constructor: Box<dyn Fn(u16) -> Instruction + 'branch>,
    ) -> Result<(), jvm::Error> {
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
                if self.emit_iinc_add(dest, op1, op2)? {
                    return Ok(());
                }

                let op1_type = get_operand_type(op1);
                let op2_type = get_operand_type(op2); // Get type of op2 as well

                // Promote based on types (Simplified: assumes BigInt/BigDec promote others)
                // A more robust system would use determine_comparison_type logic
                let op_type = if op1_type == Type::Class(F128_CLASS.to_string())
                    || op2_type == Type::Class(F128_CLASS.to_string())
                {
                    Type::Class(F128_CLASS.to_string())
                } else if op1_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
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
                    Type::Class(ref c) if c == F128_CLASS => {
                        self.translate_f128_binary_op(dest, op1, op2, "add")?
                    }
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
                if self.emit_iinc_sub(dest, op1, op2)? {
                    return Ok(());
                }

                let op1_type = get_operand_type(op1);
                let op2_type = get_operand_type(op2);

                // Determine result type (similar promotion logic as Add)
                let op_type = if op1_type == Type::Class(F128_CLASS.to_string())
                    || op2_type == Type::Class(F128_CLASS.to_string())
                {
                    Type::Class(F128_CLASS.to_string())
                } else if op1_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
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
                    Type::Class(ref c) if c == F128_CLASS => {
                        self.translate_f128_binary_op(dest, op1, op2, "subtract")?
                    }
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
                let op_type = if op1_type == Type::Class(F128_CLASS.to_string())
                    || op2_type == Type::Class(F128_CLASS.to_string())
                {
                    Type::Class(F128_CLASS.to_string())
                } else if op1_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
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
                    Type::Class(ref c) if c == F128_CLASS => {
                        self.translate_f128_binary_op(dest, op1, op2, "multiply")?
                    }
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
                let op_type = if op1_type == Type::Class(F128_CLASS.to_string())
                    || op2_type == Type::Class(F128_CLASS.to_string())
                {
                    Type::Class(F128_CLASS.to_string())
                } else if op1_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
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
                    Type::Class(ref c) if c == F128_CLASS => {
                        self.translate_f128_binary_op(dest, op1, op2, "divide")?
                    }
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
                let op_type = if op1_type == Type::Class(F128_CLASS.to_string())
                    || op2_type == Type::Class(F128_CLASS.to_string())
                {
                    Type::Class(F128_CLASS.to_string())
                } else if op1_type == Type::Class(BIG_DECIMAL_CLASS.to_string())
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
                    Type::Class(ref c) if c == F128_CLASS => {
                        self.translate_f128_binary_op(dest, op1, op2, "remainder")?
                    }
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
            OI::Eq { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "eq")?,
            OI::Ne { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "ne")?,
            OI::Lt { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "lt")?,
            OI::Le { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "le")?,
            OI::Gt { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "gt")?,
            OI::Ge { dest, op1, op2 } => self.translate_comparison_op(dest, op1, op2, "ge")?,

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
                    oomir::Type::Class(ref c) if c == F128_CLASS => {
                        let class = self.constant_pool.add_class(F128_CLASS)?;
                        let descriptor = format!("()L{F128_CLASS};");
                        let method = self
                            .constant_pool
                            .add_method_ref(class, "negate", descriptor)?;
                        self.load_operand(src)?;
                        self.jvm_instructions.push(JI::Invokevirtual(method));
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

            OI::Jump { target } => {
                if self.current_fallthrough_block_label.as_deref() == Some(target) {
                    return Ok(());
                }

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

                match self.current_fallthrough_block_label.as_deref() {
                    Some(fallthrough) if fallthrough == false_block => {
                        let instr_idx_ifne = self.jvm_instructions.len();
                        self.jvm_instructions.push(JI::Ifne(0));
                        self.branch_fixups
                            .push((instr_idx_ifne, true_block.clone()));
                    }
                    Some(fallthrough) if fallthrough == true_block => {
                        let instr_idx_ifeq = self.jvm_instructions.len();
                        self.jvm_instructions.push(JI::Ifeq(0));
                        self.branch_fixups
                            .push((instr_idx_ifeq, false_block.clone()));
                    }
                    _ => {
                        // 2. Add conditional jump (if condition != 0, jump to true_block)
                        let instr_idx_ifne = self.jvm_instructions.len();
                        self.jvm_instructions.push(JI::Ifne(0));
                        self.branch_fixups
                            .push((instr_idx_ifne, true_block.clone()));

                        // 3. Add unconditional jump to false_block.
                        let instr_idx_goto_false = self.jvm_instructions.len();
                        self.jvm_instructions.push(JI::Goto(0));
                        self.branch_fixups
                            .push((instr_idx_goto_false, false_block.clone()));
                    }
                }
            }
            OI::Switch {
                discr,
                targets,
                otherwise,
            } => {
                // 0. Calculate the type of the discriminant
                let discr_type = get_operand_type(discr); // Use helper consistently

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

                if self.emit_integer_switch(discr, &discr_type, targets, otherwise)? {
                    return Ok(());
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

                    match &discr_type {
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
                            | oomir::Type::Slice(_)
                            | oomir::Type::MutableReference(_)
                            | oomir::Type::Str
                            | oomir::Type::String
                            | oomir::Type::Class(_)
                            | oomir::Type::Interface(_) => JI::Areturn,
                            oomir::Type::Unit | oomir::Type::Void => JI::Return,
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
                class_name,
                function: function_name,
                signature,
                args,
            } => {
                if self.emit_runtime_intrinsic_call(function_name, signature, args, dest)? {
                    return Ok(());
                }
                let mut handled_as_shim = false;
                let mut is_diverging_call = false;

                match get_shim_metadata() {
                    Ok(shim_map) => {
                        // Use the lowered function name as the key.
                        if let Some(shim_info) = shim_map.get(function_name) {
                            handled_as_shim = true;
                            is_diverging_call = function_name == "panic"
                                || function_name == "panic_fmt"
                                || function_name == "core_panicking_panic"
                                || function_name == "core_assert_failed";
                            self.emit_shim_call(
                                function_name,
                                shim_info,
                                args,
                                dest,
                                None,
                                is_diverging_call,
                            )?;
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

                if !handled_as_shim {
                    let target_func = module.get_function(class_name.as_deref(), function_name);
                    let owner_class = target_func
                        .map(|target_func| module.owner_class_for_function(target_func).to_string())
                        .or_else(|| class_name.clone())
                        .ok_or_else(|| jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!(
                                "Cannot find local function '{}' and no owner class was supplied for a linked external call.",
                                function_name
                            ),
                        })?;
                    let target_sig = target_func
                        .map(|target_func| &target_func.signature)
                        .unwrap_or(signature);

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
                    for (arg, (_, expected_ty)) in args.iter().zip(target_sig.params.iter()) {
                        self.load_call_argument_as(arg, expected_ty)?;
                    }

                    // 2. Add MethodRef
                    let class_index = self.constant_pool.add_class(owner_class)?;
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
                        if target_sig.ret.has_jvm_value() {
                            self.store_result(dest_var, &target_sig.ret)?;
                        } else { /* error storing void */
                        }
                    } else if target_sig.ret.has_jvm_value() {
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
            OI::CallIndirect {
                dest,
                function_ptr,
                args,
                signature,
            } => {
                let interface_name = match function_ptr.get_type() {
                    Some(oomir::Type::Interface(name)) => name,
                    _ => signature.fn_ptr_interface_name(),
                };
                let class_index = self.constant_pool.add_class(&interface_name)?;
                let descriptor = signature.to_jvm_descriptor_with_explicit_params();
                let method_ref = self.constant_pool.add_interface_method_ref(
                    class_index,
                    "call",
                    &descriptor,
                )?;

                self.load_operand(function_ptr)?;
                for arg in args.iter() {
                    self.load_call_argument(arg)?;
                }

                let count = self.invokeinterface_count(args)?;
                self.jvm_instructions
                    .push(JI::Invokeinterface(method_ref, count));

                if let Some(dest_var) = dest {
                    if signature.ret.has_jvm_value() {
                        self.store_result(dest_var, &signature.ret)?;
                    }
                } else if signature.ret.has_jvm_value() {
                    match get_type_size(&signature.ret) {
                        1 => self.jvm_instructions.push(JI::Pop),
                        2 => self.jvm_instructions.push(JI::Pop2),
                        _ => {}
                    }
                }
            }
            OI::Move { dest, src } => {
                let value_type = match src {
                    OO::Constant(c) => Type::from_constant(c),
                    OO::Variable { ty, .. } => ty.clone(),
                };

                self.load_operand(src)?;
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
                if let oomir::Type::Slice(element_type) = &array_type {
                    let view_class = self.constant_pool.add_class(oomir::SLICE_VIEW_CLASS)?;
                    let backing_field = self.constant_pool.add_field_ref(
                        view_class,
                        "array",
                        "Ljava/lang/Object;",
                    )?;
                    let offset_field = self
                        .constant_pool
                        .add_field_ref(view_class, "offset", "I")?;
                    let slice_operand = oomir::Operand::Variable {
                        name: array.clone(),
                        ty: array_type.clone(),
                    };
                    self.load_operand(&slice_operand)?;
                    self.jvm_instructions.push(JI::Getfield(backing_field));
                    let backing_class = self
                        .constant_pool
                        .add_class(&oomir::Type::Array(element_type.clone()).to_jvm_descriptor())?;
                    self.jvm_instructions.push(JI::Checkcast(backing_class));
                    self.load_operand(&slice_operand)?;
                    self.jvm_instructions.push(JI::Getfield(offset_field));
                    self.load_operand(index)?;
                    self.jvm_instructions.push(JI::Iadd);
                    self.load_operand_as(value, element_type)?;
                    let store =
                        element_type
                            .get_jvm_array_store_instruction()
                            .ok_or_else(|| jvm::Error::VerificationError {
                                context: format!("Function {}", self.oomir_func.name),
                                message: format!(
                                    "Cannot store slice element of type {:?}",
                                    element_type
                                ),
                            })?;
                    self.jvm_instructions.push(store);
                    return Ok(());
                }
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

                let value_type = get_operand_type(value);
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "bytecode-gen",
                    format!(
                        "Value type: {:?}, element type {:?}",
                        value_type, element_type
                    )
                );
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
                self.load_operand_as(value, &element_type)?; // Stack: [arrayref, index_int, value]

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
                if let oomir::Type::Slice(element_type) = get_operand_type(array) {
                    let view_class = self.constant_pool.add_class(oomir::SLICE_VIEW_CLASS)?;
                    let backing_field = self.constant_pool.add_field_ref(
                        view_class,
                        "array",
                        "Ljava/lang/Object;",
                    )?;
                    let offset_field = self
                        .constant_pool
                        .add_field_ref(view_class, "offset", "I")?;
                    self.load_operand(array)?;
                    self.jvm_instructions.push(JI::Getfield(backing_field));
                    let backing_class = self
                        .constant_pool
                        .add_class(&oomir::Type::Array(element_type.clone()).to_jvm_descriptor())?;
                    self.jvm_instructions.push(JI::Checkcast(backing_class));
                    self.load_operand(array)?;
                    self.jvm_instructions.push(JI::Getfield(offset_field));
                    self.load_operand(index)?;
                    self.jvm_instructions.push(JI::Iadd);
                    let load = element_type
                        .get_jvm_array_load_instruction()
                        .ok_or_else(|| jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Cannot load slice element of type {element_type:?}"),
                        })?;
                    self.jvm_instructions.push(load);
                    self.store_result(dest, &element_type)?;
                    return Ok(());
                }
                // Special case: In instance methods, _1 is 'this' (raw object at slot 0), not an array
                // If OOMIR tries to unbox it with ArrayGet, we should treat it as a simple move
                let is_this_unbox = match (&array, index) {
                    (OO::Variable { name, ty }, OO::Constant(oomir::Constant::I32(0))) => {
                        name == "_1"
                            && !self.oomir_func.signature.is_static
                            && matches!(ty, oomir::Type::MutableReference(_))
                    }
                    _ => false,
                };

                if is_this_unbox {
                    // This is dereferencing _1 (this) in an instance method
                    // _1 is already the raw object at slot 0, so just load and store it
                    self.load_operand(&array)?; // Loads 'this' from slot 0

                    // Get the inner type (the actual class type)
                    let element_type = if let OO::Variable {
                        ty: oomir::Type::MutableReference(inner),
                        ..
                    } = &array
                    {
                        (**inner).clone()
                    } else {
                        return Err(jvm::Error::VerificationError {
                            context: format!("Function {}", self.oomir_func.name),
                            message: format!("Expected MutableReference for _1, found {:?}", array),
                        });
                    };

                    self.store_result(dest, &element_type)?;
                } else {
                    // Normal array access
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
                        oomir::Type::Array(inner_type)
                        | oomir::Type::MutableReference(inner_type) => {
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
            }
            OI::Length { dest, array } => {
                if matches!(
                    get_operand_type(array),
                    oomir::Type::Slice(_) | oomir::Type::Str
                ) {
                    let view_class = self.constant_pool.add_class(oomir::SLICE_VIEW_CLASS)?;
                    let length_field = self
                        .constant_pool
                        .add_field_ref(view_class, "length", "I")?;
                    self.load_operand(array)?;
                    self.jvm_instructions.push(JI::Getfield(length_field));
                    self.store_result(dest, &oomir::Type::I32)?;
                    return Ok(());
                }
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
            OI::ConstructObject {
                dest,
                class_name,
                args,
            } => {
                // 1. Add Class reference to constant pool
                let class_index = self.constant_pool.add_class(class_name)?;

                let constructor_descriptor = format!(
                    "({})V",
                    args.iter()
                        .filter(|(_, ty)| ty.has_jvm_value())
                        .map(|(_, ty)| ty.to_jvm_descriptor())
                        .collect::<String>()
                );
                let constructor_ref_index = self.constant_pool.add_method_ref(
                    class_index,
                    "<init>",
                    &constructor_descriptor,
                )?;

                // 3. Emit 'new' instruction
                self.jvm_instructions.push(JI::New(class_index)); // Stack: [uninitialized_ref]

                // 4. Emit 'dup' instruction
                self.jvm_instructions.push(JI::Dup); // Stack: [uninitialized_ref, uninitialized_ref]

                for (arg, arg_ty) in args {
                    if arg_ty.has_jvm_value() {
                        self.load_operand_as(arg, arg_ty)?;
                    }
                }

                // 5. Emit 'invokespecial' to call the constructor
                self.jvm_instructions
                    .push(JI::Invokespecial(constructor_ref_index)); // Stack: [initialized_ref]

                // 6. Store the initialized object reference into the destination variable
                //    The type of the destination variable is Type::Class(class_name)
                let dest_type = oomir::Type::Class(class_name.clone());
                self.store_result(dest, &dest_type)?; // Stack: []
            }

            OI::SetField { field_ty, .. } if !field_ty.has_jvm_value() => {}
            OI::SetField {
                object,
                field_name,
                value,
                field_ty,
                owner_class, // Class where the field is *defined*
            } => {
                // 1. Get the type of the object variable itself (should be a Class type)
                let owner_ty = oomir::Type::Class(owner_class.clone());
                let (object_var_index, object_actual_type) = self
                    .get_typed_local_index(object, &owner_ty)
                    .map(|index| (index, owner_ty))
                    .unwrap_or((
                        self.get_local_index(object)?,
                        self.get_local_type(object)?.clone(),
                    ));

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
                let load_object_instr =
                    get_load_instruction(&object_actual_type, object_var_index)?;
                self.jvm_instructions.push(load_object_instr.clone()); // Stack: [object_ref]

                // 4. Load the value to be stored onto the stack
                self.load_operand_as(value, field_ty)?; // Stack: [object_ref, value] (value size 1 or 2)

                // 5. Emit 'putfield' instruction
                self.jvm_instructions.push(JI::Putfield(field_ref_index)); // Stack: []
            }

            OI::GetField { dest, field_ty, .. } if !field_ty.has_jvm_value() => {
                self.local_var_types.insert(dest.clone(), field_ty.clone());
                self.typed_local_var_map
                    .insert((dest.clone(), field_ty.clone()), self.next_local_index);
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
                if object.get_name() == Some(dest.as_str()) {
                    self.store_result_in_distinct_slot(dest, field_ty)?;
                } else {
                    self.store_result(dest, field_ty)?;
                }
            }
            OI::Cast { op, ty, dest } => {
                self.load_operand_as(op, ty)?;

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
                let count = self.invokeinterface_count(args)?;
                self.jvm_instructions
                    .push(JI::Invokeinterface(method_ref_index, count)); // Stack: [result]

                // 4. Handle the return value
                if let Some(dest_var) = dest {
                    // Store the result in the destination variable
                    self.store_result(dest_var, &method_ty.ret)?;
                } else if method_ty.ret.has_jvm_value() {
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
                method_name,
                operand,
                ..
            } if operand.get_type() == Some(oomir::Type::Unit)
                && matches!(method_name.as_str(), "eq" | "ne") =>
            {
                if let Some(dest_var) = dest {
                    self.jvm_instructions.push(if method_name == "eq" {
                        JI::Iconst_1
                    } else {
                        JI::Iconst_0
                    });
                    self.store_result(dest_var, &oomir::Type::Boolean)?;
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
                // If operand is a MutableReference, it's represented as a single-element array,
                // so we need to load the array and then use aaload to get element 0.
                let receiver_type = operand.get_type();
                let is_mutable_ref =
                    matches!(receiver_type, Some(oomir::Type::MutableReference(_)));
                let is_this_receiver = matches!(
                    operand,
                    OO::Variable { name, .. } if name == "_1" && !self.oomir_func.signature.is_static
                );

                if is_mutable_ref && !is_this_receiver {
                    // Load the array reference
                    self.load_operand(operand)?; // Stack: [arrayref]
                    // Load index 0
                    self.jvm_instructions.push(JI::Iconst_0); // Stack: [arrayref, 0]
                    // Get element at index 0
                    self.jvm_instructions.push(JI::Aaload); // Stack: [objectref]
                } else {
                    self.load_operand(operand)?; // Stack: [object_ref]
                }

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
                } else if method_ty.ret.has_jvm_value() {
                    // Pop the result if it's not void and no destination is provided
                    match get_type_size(&method_ty.ret) {
                        1 => self.jvm_instructions.push(JI::Pop),
                        2 => self.jvm_instructions.push(JI::Pop2),
                        _ => {}
                    }
                }
            }
            OI::InvokeStatic {
                dest,
                class_name,
                method_name,
                method_ty,
                args,
            } => {
                if let Ok(shim_map) = get_shim_metadata() {
                    let shim_key = format!("{class_name}::{method_name}");
                    let shim = shim_map
                        .get(&shim_key)
                        .map(|info| (shim_key.as_str(), info))
                        .or_else(|| {
                            shim_map.get(method_name).and_then(|info| {
                                (info.java_class(method_name) == class_name)
                                    .then_some((method_name.as_str(), info))
                            })
                        });
                    if let Some((shim_key, shim_info)) = shim {
                        self.emit_shim_call(
                            shim_key,
                            shim_info,
                            args,
                            dest,
                            Some(method_ty.ret.as_ref()),
                            false,
                        )?;
                        return Ok(());
                    }
                }

                // 1. Add Method reference to constant pool
                let class_index = self.constant_pool.add_class(class_name)?;
                let method_ref_index = self.constant_pool.add_method_ref(
                    class_index,
                    method_name,
                    &method_ty.to_string(),
                )?;

                // 2. Load arguments onto the stack
                for arg in args {
                    self.load_call_argument(arg)?; // Use helper to handle references properly
                }

                // 3. Emit 'invokestatic' instruction
                self.jvm_instructions
                    .push(JI::Invokestatic(method_ref_index)); // Stack: [result]
                // Note: The result type is determined by the method signature

                // 4. Handle the return value
                if let Some(dest_var) = dest {
                    // Store the result in the destination variable
                    self.store_result(dest_var, &method_ty.ret)?;
                } else if method_ty.ret.has_jvm_value() {
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
        if operand
            .get_type()
            .is_some_and(|operand_ty| !operand_ty.has_jvm_value())
        {
            return Ok(());
        }
        match operand {
            oomir::Operand::Variable { name: var_name, ty } => {
                self.materialize_zero_sized_local(var_name, ty)?;
                let index = self
                    .get_typed_local_index(var_name, ty)
                    .or_else(|| self.local_var_map.get(var_name).copied())
                    .ok_or_else(|| jvm::Error::VerificationError {
                        context: format!("Function {}", self.oomir_func.name),
                        message: format!(
                            "Undefined call argument local variable: {var_name} ({ty:?})"
                        ),
                    })?;
                let load_type = match ty {
                    // If the argument is declared as Ref<Primitive>, load the primitive directly
                    oomir::Type::Reference(box inner_ty) if inner_ty.is_jvm_primitive() => {
                        inner_ty // Use the inner type for loading
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

    fn load_call_argument_as(
        &mut self,
        operand: &oomir::Operand,
        expected_ty: &oomir::Type,
    ) -> Result<(), jvm::Error> {
        if !expected_ty.has_jvm_value() {
            return Ok(());
        }

        let actual_ty = get_operand_type(operand);
        if actual_ty == oomir::Type::Str && *expected_ty == oomir::Type::String {
            self.load_call_argument(operand)?;
            return self.adapt_loaded_str_to_java_string();
        }
        if actual_ty == oomir::Type::String && *expected_ty == oomir::Type::Str {
            self.load_call_argument(operand)?;
            return self.adapt_loaded_java_string_to_str();
        }
        if matches!(actual_ty, oomir::Type::Slice(_))
            && expected_ty.to_jvm_descriptor().starts_with('[')
        {
            self.load_call_argument(operand)?;
            return self.adapt_loaded_slice_to_array(&expected_ty.to_jvm_descriptor());
        }
        if let Some(class_name) = self.zero_sized_class_name(expected_ty)
            && actual_ty != *expected_ty
        {
            return self.construct_zero_sized_class_value(&class_name);
        }

        self.load_call_argument(operand)?;
        if actual_ty != *expected_ty
            && actual_ty.to_jvm_descriptor() != expected_ty.to_jvm_descriptor()
        {
            self.jvm_instructions.extend(get_cast_instructions(
                &actual_ty,
                expected_ty,
                self.constant_pool,
            )?);
        }
        Ok(())
    }

    fn call_argument_slot_size(operand: &oomir::Operand) -> u16 {
        let ty = get_operand_type(operand);
        match ty {
            oomir::Type::Reference(inner) if inner.is_jvm_primitive() => get_type_size(&inner),
            _ => get_type_size(&ty),
        }
    }

    fn invokeinterface_count(&self, args: &[oomir::Operand]) -> Result<u8, jvm::Error> {
        let slots = 1 + args.iter().map(Self::call_argument_slot_size).sum::<u16>();
        u8::try_from(slots).map_err(|_| jvm::Error::VerificationError {
            context: format!("Function {}", self.oomir_func.name),
            message: format!("invokeinterface argument slot count {slots} exceeds u8 range"),
        })
    }
}

fn conditional_branch_target(instruction: &Instruction) -> Option<u16> {
    match instruction {
        Instruction::Ifeq(target)
        | Instruction::Ifne(target)
        | Instruction::Iflt(target)
        | Instruction::Ifge(target)
        | Instruction::Ifgt(target)
        | Instruction::Ifle(target)
        | Instruction::If_icmpeq(target)
        | Instruction::If_icmpne(target)
        | Instruction::If_icmplt(target)
        | Instruction::If_icmpge(target)
        | Instruction::If_icmpgt(target)
        | Instruction::If_icmple(target)
        | Instruction::If_acmpeq(target)
        | Instruction::If_acmpne(target)
        | Instruction::Ifnull(target)
        | Instruction::Ifnonnull(target) => Some(*target),
        _ => None,
    }
}

fn layout_successors(block: &oomir::BasicBlock) -> Vec<String> {
    let mut successors = Vec::new();
    let mut seen = HashSet::new();
    let mut push_unique = |target: &String| {
        if seen.insert(target.clone()) {
            successors.push(target.clone());
        }
    };

    match block.instructions.last() {
        Some(oomir::Instruction::Jump { target }) => push_unique(target),
        Some(oomir::Instruction::Branch {
            true_block,
            false_block,
            ..
        }) => {
            push_unique(false_block);
            push_unique(true_block);
        }
        Some(oomir::Instruction::Switch {
            targets, otherwise, ..
        }) => {
            push_unique(otherwise);
            for (_, target) in targets {
                push_unique(target);
            }
        }
        _ => {}
    }

    successors
}

fn is_null_operand(operand: &oomir::Operand) -> bool {
    matches!(operand, oomir::Operand::Constant(oomir::Constant::Null(_)))
}

fn iinc_amount(operand: &oomir::Operand, sign: i32) -> Option<i16> {
    let oomir::Operand::Constant(constant) = operand else {
        return None;
    };
    let amount = match constant {
        oomir::Constant::I8(value) => i32::from(*value),
        oomir::Constant::I16(value) => i32::from(*value),
        oomir::Constant::I32(value) => *value,
        _ => return None,
    };
    amount
        .checked_mul(sign)
        .and_then(|amount| i16::try_from(amount).ok())
}

fn make_iinc_instruction(index: u16, amount: i16) -> Instruction {
    if index <= u16::from(u8::MAX) && amount >= i16::from(i8::MIN) && amount <= i16::from(i8::MAX) {
        Instruction::Iinc(index as u8, amount as i8)
    } else {
        Instruction::Iinc_w(index, amount)
    }
}

fn is_jvm_switch_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::I8 | Type::I16 | Type::I32 | Type::Boolean | Type::Char
    )
}

fn jvm_switch_key(
    discr_type: &Type,
    constant_key: &oomir::Constant,
    context: &str,
) -> Result<i32, jvm::Error> {
    match (discr_type, constant_key) {
        (Type::I8, oomir::Constant::I8(value)) => Ok(i32::from(*value)),
        (Type::I16, oomir::Constant::I16(value)) => Ok(i32::from(*value)),
        (Type::I32, oomir::Constant::I32(value)) => Ok(*value),
        (Type::Boolean, oomir::Constant::Boolean(value)) => Ok(i32::from(*value)),
        (Type::Char, oomir::Constant::Char(value)) => Ok(*value as i32),
        _ => Err(jvm::Error::VerificationError {
            context: context.to_string(),
            message: format!(
                "Type mismatch in OOMIR Switch: Discriminant type is {discr_type:?}, but case key is {constant_key:?}"
            ),
        }),
    }
}

fn instruction_byte_offsets(instructions: &[Instruction]) -> Result<Vec<usize>, jvm::Error> {
    let mut offsets = Vec::with_capacity(instructions.len() + 1);
    let mut byte_offset = 0usize;
    for instruction in instructions {
        offsets.push(byte_offset);
        byte_offset += instruction_size_at(instruction, byte_offset)?;
    }
    offsets.push(byte_offset);
    Ok(offsets)
}

fn instruction_size_at(instruction: &Instruction, byte_offset: usize) -> Result<usize, jvm::Error> {
    match instruction {
        Instruction::Ifeq(_)
        | Instruction::Ifne(_)
        | Instruction::Iflt(_)
        | Instruction::Ifge(_)
        | Instruction::Ifgt(_)
        | Instruction::Ifle(_)
        | Instruction::If_icmpeq(_)
        | Instruction::If_icmpne(_)
        | Instruction::If_icmplt(_)
        | Instruction::If_icmpge(_)
        | Instruction::If_icmpgt(_)
        | Instruction::If_icmple(_)
        | Instruction::If_acmpeq(_)
        | Instruction::If_acmpne(_)
        | Instruction::Goto(_)
        | Instruction::Jsr(_)
        | Instruction::Ifnull(_)
        | Instruction::Ifnonnull(_) => Ok(3),
        Instruction::Goto_w(_) | Instruction::Jsr_w(_) => Ok(5),
        Instruction::Tableswitch { offsets, .. } => {
            let position_after_opcode = byte_offset + 1;
            let padding = (4 - (position_after_opcode % 4)) % 4;
            Ok(1 + padding + 12 + offsets.len() * 4)
        }
        Instruction::Lookupswitch { pairs, .. } => {
            let position_after_opcode = byte_offset + 1;
            let padding = (4 - (position_after_opcode % 4)) % 4;
            Ok(1 + padding + 8 + pairs.len() * 8)
        }
        _ => {
            let mut bytes = Cursor::new(Vec::new());
            instruction.to_bytes(&mut bytes)?;
            Ok(bytes.get_ref().len())
        }
    }
}

fn branch_offset_fits_i16(byte_offsets: &[usize], index: usize, target: usize) -> bool {
    let Some(origin) = byte_offsets.get(index) else {
        return false;
    };
    let Some(destination) = byte_offsets.get(target) else {
        return false;
    };
    let offset = *destination as isize - *origin as isize;
    i16::try_from(offset).is_ok()
}

fn invert_conditional_branch(instruction: &Instruction, target: u16) -> Option<Instruction> {
    Some(match instruction {
        Instruction::Ifeq(_) => Instruction::Ifne(target),
        Instruction::Ifne(_) => Instruction::Ifeq(target),
        Instruction::Iflt(_) => Instruction::Ifge(target),
        Instruction::Ifge(_) => Instruction::Iflt(target),
        Instruction::Ifgt(_) => Instruction::Ifle(target),
        Instruction::Ifle(_) => Instruction::Ifgt(target),
        Instruction::If_icmpeq(_) => Instruction::If_icmpne(target),
        Instruction::If_icmpne(_) => Instruction::If_icmpeq(target),
        Instruction::If_icmplt(_) => Instruction::If_icmpge(target),
        Instruction::If_icmpge(_) => Instruction::If_icmplt(target),
        Instruction::If_icmpgt(_) => Instruction::If_icmple(target),
        Instruction::If_icmple(_) => Instruction::If_icmpgt(target),
        Instruction::If_acmpeq(_) => Instruction::If_acmpne(target),
        Instruction::If_acmpne(_) => Instruction::If_acmpeq(target),
        Instruction::Ifnull(_) => Instruction::Ifnonnull(target),
        Instruction::Ifnonnull(_) => Instruction::Ifnull(target),
        _ => return None,
    })
}

fn bump_u16_branch_target(
    target: &mut u16,
    insert_at: usize,
    context: &str,
) -> Result<(), jvm::Error> {
    if usize::from(*target) >= insert_at {
        *target = target
            .checked_add(1)
            .ok_or_else(|| jvm::Error::VerificationError {
                context: context.to_string(),
                message: "Branch target overflow while widening branches".to_string(),
            })?;
    }
    Ok(())
}

fn bump_i32_relative_switch_target(
    target: &mut i32,
    source_index: usize,
    insert_at: usize,
    context: &str,
) -> Result<(), jvm::Error> {
    let absolute_target = source_index as i64 + i64::from(*target);
    if absolute_target < 0 {
        return Err(jvm::Error::VerificationError {
            context: context.to_string(),
            message: format!(
                "Invalid relative switch target {} from instruction {}",
                *target, source_index
            ),
        });
    }

    let adjusted_source = source_index as i64 + if source_index >= insert_at { 1 } else { 0 };
    let adjusted_target = absolute_target
        + if absolute_target >= insert_at as i64 {
            1
        } else {
            0
        };
    *target = i32::try_from(adjusted_target - adjusted_source).map_err(|_| {
        jvm::Error::VerificationError {
            context: context.to_string(),
            message: "Switch target overflow while widening branches".to_string(),
        }
    })?;
    Ok(())
}
