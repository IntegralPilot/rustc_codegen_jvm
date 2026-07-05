use super::constant_pool::InternedConstantPool;
use crate::oomir::{self, Type};
use ristretto_classfile::{
    self as jvm, BaseType, Constant, ConstantPool, FieldType,
    attributes::{ArrayType, Attribute, Instruction, StackFrame, VerificationType},
};
use std::{
    collections::{BTreeSet, HashMap, VecDeque},
    io::Cursor,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum FrameValue {
    Top,
    Integer,
    Float,
    Long,
    Double,
    Null,
    Object(String),
    UninitializedThis,
    Uninitialized(u16),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct FrameState {
    locals: Vec<FrameValue>,
    stack: Vec<FrameValue>,
}

impl FrameValue {
    fn is_category2(&self) -> bool {
        matches!(self, FrameValue::Long | FrameValue::Double)
    }

    fn is_reference_like(&self) -> bool {
        matches!(
            self,
            FrameValue::Null
                | FrameValue::Object(_)
                | FrameValue::UninitializedThis
                | FrameValue::Uninitialized(_)
        )
    }
}

impl FrameState {
    fn new(initial_locals: Vec<FrameValue>, max_locals: usize) -> Self {
        let mut locals = initial_locals;
        locals.resize(max_locals, FrameValue::Top);
        Self {
            locals,
            stack: Vec::new(),
        }
    }

    fn pop(&mut self, context: &str, instruction_index: usize) -> jvm::Result<FrameValue> {
        self.stack
            .pop()
            .ok_or_else(|| jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!("Stack underflow at instruction {instruction_index}"),
            })
    }

    fn pop_category1(
        &mut self,
        context: &str,
        instruction_index: usize,
    ) -> jvm::Result<FrameValue> {
        let value = self.pop(context, instruction_index)?;
        if value.is_category2() {
            return Err(jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Expected category-1 value at instruction {instruction_index}, found {value:?}"
                ),
            });
        }
        Ok(value)
    }

    fn pop_reference(
        &mut self,
        context: &str,
        instruction_index: usize,
    ) -> jvm::Result<FrameValue> {
        let value = self.pop_category1(context, instruction_index)?;
        if !value.is_reference_like() && value != FrameValue::Top {
            return Err(jvm::Error::VerificationError {
                context: context.to_string(),
                message: format!(
                    "Expected reference value at instruction {instruction_index}, found {value:?}"
                ),
            });
        }
        Ok(value)
    }

    fn load_local(
        &mut self,
        index: u16,
        local_hints: &[FrameValue],
        load_hint: FrameValue,
        context: &str,
        instruction_index: usize,
    ) -> jvm::Result<()> {
        let mut value = self
            .locals
            .get(index as usize)
            .cloned()
            .unwrap_or(FrameValue::Top);
        if value == FrameValue::Top {
            value = local_hints
                .get(index as usize)
                .cloned()
                .unwrap_or(FrameValue::Top);
            if value == FrameValue::Top {
                value = load_hint;
                if value == FrameValue::Top {
                    return Err(jvm::Error::VerificationError {
                        context: context.to_string(),
                        message: format!(
                            "Loaded uninitialized local {index} at instruction {instruction_index}"
                        ),
                    });
                }
            }
            self.store_local(index, value.clone());
        }
        self.stack.push(value);
        Ok(())
    }

    fn store_local(&mut self, index: u16, value: FrameValue) {
        let index = index as usize;
        let width = if value.is_category2() { 2 } else { 1 };
        if self.locals.len() < index + width {
            self.locals.resize(index + width, FrameValue::Top);
        }

        if index > 0 && self.locals[index - 1].is_category2() {
            self.locals[index - 1] = FrameValue::Top;
        }
        self.locals[index] = value;
        if width == 2 {
            self.locals[index + 1] = FrameValue::Top;
        }
    }

    fn initialize_object(&mut self, uninitialized: &FrameValue, class_name: &str) {
        let initialized = FrameValue::Object(normalize_class_name(class_name));
        for local in &mut self.locals {
            if local == uninitialized {
                *local = initialized.clone();
            }
        }
        for stack_value in &mut self.stack {
            if stack_value == uninitialized {
                *stack_value = initialized.clone();
            }
        }
    }
}

pub(super) fn initial_locals_for_oomir_function(
    function: &oomir::Function,
    is_static: bool,
    owner_class_name: Option<&str>,
) -> Vec<FrameValue> {
    let mut locals = Vec::new();
    if !is_static {
        let this_value = if function.name == "<init>" {
            FrameValue::UninitializedThis
        } else {
            FrameValue::Object(normalize_class_name(
                owner_class_name
                    .or(function.owner_class.as_deref())
                    .unwrap_or("java/lang/Object"),
            ))
        };
        push_local_value(&mut locals, this_value);
    }

    let first_explicit_param = if is_static { 0 } else { 1 };
    for (_, param_ty) in function.signature.params.iter().skip(first_explicit_param) {
        push_local_value(&mut locals, frame_value_from_oomir_type(param_ty));
    }
    locals
}

pub(super) fn initial_locals_for_descriptor(
    descriptor: &str,
    is_static: bool,
    this_class_name: Option<&str>,
    is_constructor: bool,
) -> jvm::Result<Vec<FrameValue>> {
    let mut locals = Vec::new();
    if !is_static {
        let this_value = if is_constructor {
            FrameValue::UninitializedThis
        } else {
            FrameValue::Object(normalize_class_name(
                this_class_name.unwrap_or("java/lang/Object"),
            ))
        };
        push_local_value(&mut locals, this_value);
    }

    let (params, _) = FieldType::parse_method_descriptor(descriptor)?;
    for param in &params {
        push_local_value(&mut locals, frame_value_from_field_type(param));
    }
    Ok(locals)
}

pub(super) fn local_hints_for_oomir_locals(
    local_var_map: &HashMap<String, u16>,
    local_var_types: &HashMap<String, oomir::Type>,
    max_locals: u16,
) -> Vec<FrameValue> {
    let mut hints = vec![FrameValue::Top; max_locals as usize];
    for (name, local_index) in local_var_map {
        let Some(ty) = local_var_types.get(name) else {
            continue;
        };
        set_slot_value(&mut hints, *local_index, frame_value_from_oomir_type(ty));
    }
    hints
}

pub(super) fn build_stack_map_attributes(
    instructions: &[Instruction],
    initial_locals: &[FrameValue],
    local_hints: &[FrameValue],
    max_locals: u16,
    constant_pool: &mut InternedConstantPool,
    context: &str,
) -> jvm::Result<Vec<Attribute>> {
    let target_offsets = branch_targets(instructions);
    if instructions.is_empty() || target_offsets.is_empty() {
        return Ok(Vec::new());
    }

    let states = solve_frame_states(
        instructions,
        initial_locals,
        local_hints,
        max_locals as usize,
        constant_pool,
        context,
    )?;

    let instruction_to_byte_map = instruction_to_byte_offsets(instructions)?;
    let name_index = constant_pool.add_utf8("StackMapTable")?;
    let mut previous_instruction_offset: Option<u16> = None;
    let mut previous_byte_offset: Option<u16> = None;
    let mut previous_locals =
        locals_for_stack_map(initial_locals, constant_pool, &mut HashMap::new())?;
    let mut frames = Vec::new();
    let mut verification_class_cache = HashMap::new();
    for target in target_offsets {
        if target == 0 {
            continue;
        }
        let Some(state) = states.get(target as usize).and_then(Option::as_ref) else {
            continue;
        };
        let byte_offset =
            *instruction_to_byte_map
                .get(&target)
                .ok_or_else(|| jvm::Error::VerificationError {
                    context: context.to_string(),
                    message: format!("No byte offset for stack-map target instruction {target}"),
                })?;
        let instruction_delta = match previous_instruction_offset {
            Some(previous) => target.saturating_sub(previous).saturating_sub(1),
            None => target,
        };
        let byte_delta = match previous_byte_offset {
            Some(previous) => byte_offset.saturating_sub(previous).saturating_sub(1),
            None => byte_offset,
        };
        let locals =
            locals_for_stack_map(&state.locals, constant_pool, &mut verification_class_cache)?;
        let stack =
            stack_for_stack_map(&state.stack, constant_pool, &mut verification_class_cache)?;
        frames.push(compact_stack_frame(
            &previous_locals,
            &locals,
            stack,
            instruction_delta,
            byte_delta,
        )?);
        previous_instruction_offset = Some(target);
        previous_byte_offset = Some(byte_offset);
        previous_locals = locals;
    }

    if frames.is_empty() {
        Ok(Vec::new())
    } else {
        Ok(vec![Attribute::StackMapTable { name_index, frames }])
    }
}

fn instruction_to_byte_offsets(instructions: &[Instruction]) -> jvm::Result<HashMap<u16, u16>> {
    let mut bytes = Cursor::new(Vec::new());
    let mut offsets = HashMap::new();
    for (index, instruction) in instructions.iter().enumerate() {
        offsets.insert(u16::try_from(index)?, u16::try_from(bytes.position())?);
        instruction.to_bytes(&mut bytes)?;
    }
    Ok(offsets)
}

fn compact_stack_frame(
    previous_locals: &[VerificationType],
    locals: &[VerificationType],
    stack: Vec<VerificationType>,
    instruction_delta: u16,
    byte_delta: u16,
) -> jvm::Result<StackFrame> {
    if locals == previous_locals {
        return if stack.is_empty() {
            Ok(if byte_delta <= 63 {
                StackFrame::SameFrame {
                    frame_type: u8::try_from(instruction_delta)?,
                }
            } else {
                StackFrame::SameFrameExtended {
                    frame_type: 251,
                    offset_delta: instruction_delta,
                }
            })
        } else if stack.len() == 1 {
            Ok(if byte_delta <= 63 {
                StackFrame::SameLocals1StackItemFrame {
                    frame_type: u8::try_from(instruction_delta.saturating_add(64))?,
                    stack,
                }
            } else {
                StackFrame::SameLocals1StackItemFrameExtended {
                    frame_type: 247,
                    offset_delta: instruction_delta,
                    stack,
                }
            })
        } else {
            Ok(StackFrame::FullFrame {
                frame_type: 255,
                offset_delta: instruction_delta,
                locals: locals.to_vec(),
                stack,
            })
        };
    }

    if stack.is_empty() {
        if previous_locals.starts_with(locals) {
            let removed = previous_locals.len().saturating_sub(locals.len());
            if (1..=3).contains(&removed) {
                return Ok(StackFrame::ChopFrame {
                    frame_type: u8::try_from(251usize.saturating_sub(removed))?,
                    offset_delta: instruction_delta,
                });
            }
        }
        if locals.starts_with(previous_locals) {
            let added = locals.len().saturating_sub(previous_locals.len());
            if (1..=3).contains(&added) {
                return Ok(StackFrame::AppendFrame {
                    frame_type: u8::try_from(251 + added)?,
                    offset_delta: instruction_delta,
                    locals: locals[previous_locals.len()..].to_vec(),
                });
            }
        }
    }

    Ok(StackFrame::FullFrame {
        frame_type: 255,
        offset_delta: instruction_delta,
        locals: locals.to_vec(),
        stack,
    })
}

fn solve_frame_states(
    instructions: &[Instruction],
    initial_locals: &[FrameValue],
    local_hints: &[FrameValue],
    max_locals: usize,
    constant_pool: &ConstantPool,
    context: &str,
) -> jvm::Result<Vec<Option<FrameState>>> {
    let mut states = vec![None; instructions.len()];
    states[0] = Some(FrameState::new(initial_locals.to_vec(), max_locals));
    let live_locals = compute_live_locals(instructions, max_locals);

    let mut worklist = VecDeque::from([0usize]);
    while let Some(index) = worklist.pop_front() {
        let Some(input_state) = states[index].clone() else {
            continue;
        };
        let successors = transfer_instruction(
            index,
            &instructions[index],
            input_state,
            local_hints,
            constant_pool,
            context,
        )?;

        for (target, successor_state) in successors {
            if target >= instructions.len() {
                continue;
            }
            let changed = match &mut states[target] {
                Some(existing) => merge_state(
                    existing,
                    &successor_state,
                    live_locals.get(target).map_or(&[][..], Vec::as_slice),
                ),
                slot @ None => {
                    *slot = Some(successor_state);
                    true
                }
            };
            if changed && !worklist.contains(&target) {
                worklist.push_back(target);
            }
        }
    }

    Ok(states)
}

fn transfer_instruction(
    instruction_index: usize,
    instruction: &Instruction,
    mut state: FrameState,
    local_hints: &[FrameValue],
    constant_pool: &ConstantPool,
    context: &str,
) -> jvm::Result<Vec<(usize, FrameState)>> {
    use Instruction as I;

    match instruction {
        I::Nop => {}
        I::Aconst_null => state.stack.push(FrameValue::Null),
        I::Iconst_m1
        | I::Iconst_0
        | I::Iconst_1
        | I::Iconst_2
        | I::Iconst_3
        | I::Iconst_4
        | I::Iconst_5
        | I::Bipush(_)
        | I::Sipush(_) => state.stack.push(FrameValue::Integer),
        I::Lconst_0 | I::Lconst_1 => state.stack.push(FrameValue::Long),
        I::Fconst_0 | I::Fconst_1 | I::Fconst_2 => state.stack.push(FrameValue::Float),
        I::Dconst_0 | I::Dconst_1 => state.stack.push(FrameValue::Double),
        I::Ldc(index) => state
            .stack
            .push(frame_value_from_ldc(constant_pool, u16::from(*index))?),
        I::Ldc_w(index) => state
            .stack
            .push(frame_value_from_ldc(constant_pool, *index)?),
        I::Ldc2_w(index) => state
            .stack
            .push(frame_value_from_ldc2(constant_pool, *index)?),

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
        I::Istore_0 => store_fixed(
            &mut state,
            0,
            FrameValue::Integer,
            context,
            instruction_index,
        )?,
        I::Istore_1 => store_fixed(
            &mut state,
            1,
            FrameValue::Integer,
            context,
            instruction_index,
        )?,
        I::Istore_2 => store_fixed(
            &mut state,
            2,
            FrameValue::Integer,
            context,
            instruction_index,
        )?,
        I::Istore_3 => store_fixed(
            &mut state,
            3,
            FrameValue::Integer,
            context,
            instruction_index,
        )?,
        I::Lstore_0 => store_fixed(&mut state, 0, FrameValue::Long, context, instruction_index)?,
        I::Lstore_1 => store_fixed(&mut state, 1, FrameValue::Long, context, instruction_index)?,
        I::Lstore_2 => store_fixed(&mut state, 2, FrameValue::Long, context, instruction_index)?,
        I::Lstore_3 => store_fixed(&mut state, 3, FrameValue::Long, context, instruction_index)?,
        I::Fstore_0 => store_fixed(&mut state, 0, FrameValue::Float, context, instruction_index)?,
        I::Fstore_1 => store_fixed(&mut state, 1, FrameValue::Float, context, instruction_index)?,
        I::Fstore_2 => store_fixed(&mut state, 2, FrameValue::Float, context, instruction_index)?,
        I::Fstore_3 => store_fixed(&mut state, 3, FrameValue::Float, context, instruction_index)?,
        I::Dstore_0 => store_fixed(
            &mut state,
            0,
            FrameValue::Double,
            context,
            instruction_index,
        )?,
        I::Dstore_1 => store_fixed(
            &mut state,
            1,
            FrameValue::Double,
            context,
            instruction_index,
        )?,
        I::Dstore_2 => store_fixed(
            &mut state,
            2,
            FrameValue::Double,
            context,
            instruction_index,
        )?,
        I::Dstore_3 => store_fixed(
            &mut state,
            3,
            FrameValue::Double,
            context,
            instruction_index,
        )?,
        I::Astore_0 => store_reference_fixed(&mut state, 0, context, instruction_index)?,
        I::Astore_1 => store_reference_fixed(&mut state, 1, context, instruction_index)?,
        I::Astore_2 => store_reference_fixed(&mut state, 2, context, instruction_index)?,
        I::Astore_3 => store_reference_fixed(&mut state, 3, context, instruction_index)?,
        I::Istore_w(local) => store_fixed(
            &mut state,
            *local,
            FrameValue::Integer,
            context,
            instruction_index,
        )?,
        I::Lstore_w(local) => store_fixed(
            &mut state,
            *local,
            FrameValue::Long,
            context,
            instruction_index,
        )?,
        I::Fstore_w(local) => store_fixed(
            &mut state,
            *local,
            FrameValue::Float,
            context,
            instruction_index,
        )?,
        I::Dstore_w(local) => store_fixed(
            &mut state,
            *local,
            FrameValue::Double,
            context,
            instruction_index,
        )?,
        I::Astore_w(local) => {
            store_reference_fixed(&mut state, *local, context, instruction_index)?
        }

        I::Iaload | I::Baload | I::Caload | I::Saload => {
            state.pop(context, instruction_index)?;
            state.pop_reference(context, instruction_index)?;
            state.stack.push(FrameValue::Integer);
        }
        I::Laload => array_load(&mut state, FrameValue::Long, context, instruction_index)?,
        I::Faload => array_load(&mut state, FrameValue::Float, context, instruction_index)?,
        I::Daload => array_load(&mut state, FrameValue::Double, context, instruction_index)?,
        I::Aaload => {
            state.pop(context, instruction_index)?;
            let array = state.pop_reference(context, instruction_index)?;
            state.stack.push(array_component_value(&array));
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
            state.stack.push(value.clone());
            state.stack.push(value);
        }
        I::Swap => {
            let first = state.pop_category1(context, instruction_index)?;
            let second = state.pop_category1(context, instruction_index)?;
            state.stack.push(first);
            state.stack.push(second);
        }
        I::Dup_x1 => {
            let value1 = state.pop_category1(context, instruction_index)?;
            let value2 = state.pop_category1(context, instruction_index)?;
            state.stack.push(value1.clone());
            state.stack.push(value2);
            state.stack.push(value1);
        }
        I::Dup2 => {
            let value1 = state.pop(context, instruction_index)?;
            if value1.is_category2() {
                state.stack.push(value1.clone());
                state.stack.push(value1);
            } else {
                let value2 = state.pop_category1(context, instruction_index)?;
                state.stack.push(value2.clone());
                state.stack.push(value1.clone());
                state.stack.push(value2);
                state.stack.push(value1);
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
            binary(&mut state, FrameValue::Integer, context, instruction_index)?
        }
        I::Ladd | I::Lsub | I::Lmul | I::Ldiv | I::Lrem | I::Land | I::Lor | I::Lxor => {
            binary(&mut state, FrameValue::Long, context, instruction_index)?
        }
        I::Fadd | I::Fsub | I::Fmul | I::Fdiv | I::Frem => {
            binary(&mut state, FrameValue::Float, context, instruction_index)?
        }
        I::Dadd | I::Dsub | I::Dmul | I::Ddiv | I::Drem => {
            binary(&mut state, FrameValue::Double, context, instruction_index)?
        }
        I::Ineg => unary(&mut state, FrameValue::Integer, context, instruction_index)?,
        I::Lneg => unary(&mut state, FrameValue::Long, context, instruction_index)?,
        I::Fneg => unary(&mut state, FrameValue::Float, context, instruction_index)?,
        I::Dneg => unary(&mut state, FrameValue::Double, context, instruction_index)?,
        I::Ishl | I::Ishr | I::Iushr => {
            shift(&mut state, FrameValue::Integer, context, instruction_index)?
        }
        I::Lshl | I::Lshr | I::Lushr => {
            shift(&mut state, FrameValue::Long, context, instruction_index)?
        }
        I::Iinc(local, _) => state.store_local(u16::from(*local), FrameValue::Integer),
        I::Iinc_w(local, _) => state.store_local(*local, FrameValue::Integer),

        I::I2l => convert(&mut state, FrameValue::Long, context, instruction_index)?,
        I::I2f => convert(&mut state, FrameValue::Float, context, instruction_index)?,
        I::I2d => convert(&mut state, FrameValue::Double, context, instruction_index)?,
        I::L2i | I::F2i | I::D2i | I::I2b | I::I2c | I::I2s => {
            convert(&mut state, FrameValue::Integer, context, instruction_index)?
        }
        I::L2f | I::D2f => convert(&mut state, FrameValue::Float, context, instruction_index)?,
        I::L2d | I::F2d => convert(&mut state, FrameValue::Double, context, instruction_index)?,
        I::F2l | I::D2l => convert(&mut state, FrameValue::Long, context, instruction_index)?,
        I::Lcmp | I::Fcmpl | I::Fcmpg | I::Dcmpl | I::Dcmpg => {
            state.pop(context, instruction_index)?;
            state.pop(context, instruction_index)?;
            state.stack.push(FrameValue::Integer);
        }

        I::Ifeq(target)
        | I::Ifne(target)
        | I::Iflt(target)
        | I::Ifge(target)
        | I::Ifgt(target)
        | I::Ifle(target) => {
            state.pop(context, instruction_index)?;
            return Ok(branch_successors(instruction_index, *target, state));
        }
        I::If_icmpeq(target)
        | I::If_icmpne(target)
        | I::If_icmplt(target)
        | I::If_icmpge(target)
        | I::If_icmpgt(target)
        | I::If_icmple(target)
        | I::If_acmpeq(target)
        | I::If_acmpne(target) => {
            state.pop(context, instruction_index)?;
            state.pop(context, instruction_index)?;
            return Ok(branch_successors(instruction_index, *target, state));
        }
        I::Ifnull(target) | I::Ifnonnull(target) => {
            state.pop_reference(context, instruction_index)?;
            return Ok(branch_successors(instruction_index, *target, state));
        }
        I::Goto(target) => return Ok(vec![(*target as usize, state)]),
        I::Goto_w(target) => return Ok(vec![(*target as usize, state)]),
        I::Tableswitch {
            default, offsets, ..
        } => {
            state.pop(context, instruction_index)?;
            let mut successors = Vec::with_capacity(offsets.len() + 1);
            successors.push((
                relative_switch_target(instruction_index, *default, context)?,
                state.clone(),
            ));
            for target in offsets {
                successors.push((
                    relative_switch_target(instruction_index, *target, context)?,
                    state.clone(),
                ));
            }
            return Ok(successors);
        }
        I::Lookupswitch { default, pairs } => {
            state.pop(context, instruction_index)?;
            let mut successors = Vec::with_capacity(pairs.len() + 1);
            successors.push((
                relative_switch_target(instruction_index, *default, context)?,
                state.clone(),
            ));
            for target in pairs.values() {
                successors.push((
                    relative_switch_target(instruction_index, *target, context)?,
                    state.clone(),
                ));
            }
            return Ok(successors);
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
            return Ok(Vec::new());
        }
        I::Return => return Ok(Vec::new()),

        I::Getstatic(field_ref) => {
            let field_type = field_type_for_ref(constant_pool, *field_ref)?;
            state.stack.push(frame_value_from_field_type(&field_type));
        }
        I::Putstatic(_) => {
            state.pop(context, instruction_index)?;
        }
        I::Getfield(field_ref) => {
            state.pop_reference(context, instruction_index)?;
            let field_type = field_type_for_ref(constant_pool, *field_ref)?;
            state.stack.push(frame_value_from_field_type(&field_type));
        }
        I::Putfield(_) => {
            state.pop(context, instruction_index)?;
            state.pop_reference(context, instruction_index)?;
        }
        I::Invokevirtual(method_ref)
        | I::Invokestatic(method_ref)
        | I::Invokespecial(method_ref)
        | I::Invokedynamic(method_ref) => {
            let method = method_ref_info(constant_pool, *method_ref, false)?;
            apply_invoke(
                &mut state,
                &method,
                matches!(instruction, I::Invokestatic(_) | I::Invokedynamic(_)),
                matches!(instruction, I::Invokespecial(_)),
                context,
                instruction_index,
            )?;
        }
        I::Invokeinterface(method_ref, _) => {
            let method = method_ref_info(constant_pool, *method_ref, true)?;
            apply_invoke(
                &mut state,
                &method,
                false,
                false,
                context,
                instruction_index,
            )?;
        }

        I::New(class_index) => {
            let _ = constant_pool.try_get_class(*class_index)?;
            state
                .stack
                .push(FrameValue::Uninitialized(instruction_index as u16));
        }
        I::Newarray(array_type) => {
            state.pop(context, instruction_index)?;
            state
                .stack
                .push(FrameValue::Object(array_descriptor_from_type(array_type)));
        }
        I::Anewarray(class_index) => {
            state.pop(context, instruction_index)?;
            let class_name = constant_pool.try_get_class(*class_index)?;
            let array_name = if class_name.starts_with('[') {
                format!("[{class_name}")
            } else {
                format!("[L{};", normalize_class_name(class_name))
            };
            state.stack.push(FrameValue::Object(array_name));
        }
        I::Arraylength => {
            state.pop_reference(context, instruction_index)?;
            state.stack.push(FrameValue::Integer);
        }
        I::Athrow => {
            state.pop_reference(context, instruction_index)?;
            return Ok(Vec::new());
        }
        I::Checkcast(class_index) => {
            state.pop_reference(context, instruction_index)?;
            let class_name = constant_pool.try_get_class(*class_index)?;
            state
                .stack
                .push(FrameValue::Object(normalize_class_name(class_name)));
        }
        I::Instanceof(_) => {
            state.pop_reference(context, instruction_index)?;
            state.stack.push(FrameValue::Integer);
        }
        I::Monitorenter | I::Monitorexit => {
            state.pop_reference(context, instruction_index)?;
        }
        I::Multianewarray(class_index, dimensions) => {
            for _ in 0..*dimensions {
                state.pop(context, instruction_index)?;
            }
            let class_name = constant_pool.try_get_class(*class_index)?;
            state
                .stack
                .push(FrameValue::Object(normalize_class_name(class_name)));
        }
        I::Wide | I::Breakpoint | I::Impdep1 | I::Impdep2 => {}
    }

    Ok(next_successor(instruction_index, state))
}

struct MethodRefInfo {
    class_name: String,
    method_name: String,
    descriptor: String,
}

fn apply_invoke(
    state: &mut FrameState,
    method: &MethodRefInfo,
    is_static: bool,
    is_special: bool,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    let (params, return_type) = FieldType::parse_method_descriptor(&method.descriptor)?;
    for _ in params.iter().rev() {
        state.pop(context, instruction_index)?;
    }

    let receiver = if is_static {
        None
    } else {
        Some(state.pop_reference(context, instruction_index)?)
    };

    if is_special && method.method_name == "<init>" {
        if let Some(receiver) = receiver {
            match receiver {
                FrameValue::Uninitialized(_) | FrameValue::UninitializedThis => {
                    state.initialize_object(&receiver, &method.class_name);
                }
                _ => {}
            }
        }
    }

    if let Some(return_type) = return_type {
        state.stack.push(frame_value_from_field_type(&return_type));
    }
    Ok(())
}

fn branch_successors(index: usize, target: u16, state: FrameState) -> Vec<(usize, FrameState)> {
    let mut successors = vec![(target as usize, state.clone())];
    successors.extend(next_successor(index, state));
    successors
}

fn next_successor(index: usize, state: FrameState) -> Vec<(usize, FrameState)> {
    vec![(index + 1, state)]
}

fn store_fixed(
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

fn store_reference_fixed(
    state: &mut FrameState,
    local: u16,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    let value = state.pop_reference(context, instruction_index)?;
    state.store_local(local, value);
    Ok(())
}

fn array_load(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.pop_reference(context, instruction_index)?;
    state.stack.push(value);
    Ok(())
}

fn unary(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.stack.push(value);
    Ok(())
}

fn binary(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.pop(context, instruction_index)?;
    state.stack.push(value);
    Ok(())
}

fn shift(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.pop(context, instruction_index)?;
    state.stack.push(value);
    Ok(())
}

fn convert(
    state: &mut FrameState,
    value: FrameValue,
    context: &str,
    instruction_index: usize,
) -> jvm::Result<()> {
    state.pop(context, instruction_index)?;
    state.stack.push(value);
    Ok(())
}

fn merge_state(existing: &mut FrameState, incoming: &FrameState, live_locals: &[bool]) -> bool {
    let mut changed = false;

    let local_len = existing.locals.len().max(incoming.locals.len());
    existing.locals.resize(local_len, FrameValue::Top);
    for index in 0..local_len {
        let incoming_value = incoming.locals.get(index).unwrap_or(&FrameValue::Top);
        let merged = merge_local_value(
            &existing.locals[index],
            incoming_value,
            live_locals.get(index).copied().unwrap_or(false),
        );
        if existing.locals[index] != merged {
            existing.locals[index] = merged;
            changed = true;
        }
    }

    if existing.stack.len() != incoming.stack.len() {
        let merged_len = existing.stack.len().min(incoming.stack.len());
        existing.stack.truncate(merged_len);
        changed = true;
    }
    for (existing_value, incoming_value) in existing.stack.iter_mut().zip(&incoming.stack) {
        let merged = merge_value(existing_value, incoming_value);
        if *existing_value != merged {
            *existing_value = merged;
            changed = true;
        }
    }

    changed
}

fn merge_local_value(a: &FrameValue, b: &FrameValue, is_live: bool) -> FrameValue {
    if a == b {
        return a.clone();
    }
    match (a, b) {
        (FrameValue::Top, value) | (value, FrameValue::Top) if is_live => value.clone(),
        _ => merge_value(a, b),
    }
}

fn merge_value(a: &FrameValue, b: &FrameValue) -> FrameValue {
    if a == b {
        return a.clone();
    }
    match (a, b) {
        (FrameValue::Top, _) | (_, FrameValue::Top) => FrameValue::Top,
        (FrameValue::Null, FrameValue::Object(class_name))
        | (FrameValue::Object(class_name), FrameValue::Null) => {
            FrameValue::Object(class_name.clone())
        }
        (FrameValue::Null, FrameValue::Null) => FrameValue::Null,
        (FrameValue::Object(a_class), FrameValue::Object(b_class)) => {
            FrameValue::Object(common_object_class(a_class, b_class))
        }
        _ => FrameValue::Top,
    }
}

fn common_object_class(a: &str, b: &str) -> String {
    if a == b {
        return a.to_string();
    }
    if a == "java/lang/Object" || b == "java/lang/Object" {
        return "java/lang/Object".to_string();
    }
    if let Some(parent) = nested_parent_class(a) {
        if parent == b {
            return b.to_string();
        }
    }
    if let Some(parent) = nested_parent_class(b) {
        if parent == a {
            return a.to_string();
        }
    }
    if let (Some(a_parent), Some(b_parent)) = (nested_parent_class(a), nested_parent_class(b)) {
        if a_parent == b_parent {
            return a_parent.to_string();
        }
    }
    "java/lang/Object".to_string()
}

fn nested_parent_class(class_name: &str) -> Option<&str> {
    if class_name.starts_with('[') {
        return None;
    }
    let (parent, _) = class_name.rsplit_once('$')?;
    if parent.is_empty() {
        None
    } else {
        Some(parent)
    }
}

fn compute_live_locals(instructions: &[Instruction], max_locals: usize) -> Vec<Vec<bool>> {
    let mut live = vec![vec![false; max_locals]; instructions.len()];
    let mut changed = true;

    while changed {
        changed = false;

        for index in (0..instructions.len()).rev() {
            let mut next_live = vec![false; max_locals];
            for successor in instruction_successors(index, &instructions[index], instructions.len())
            {
                if let Some(successor_live) = live.get(successor) {
                    for (slot, is_live) in successor_live.iter().enumerate() {
                        next_live[slot] |= *is_live;
                    }
                }
            }

            for (slot, width) in local_defs(&instructions[index]) {
                for killed in usize::from(slot)..usize::from(slot).saturating_add(width) {
                    if let Some(is_live) = next_live.get_mut(killed) {
                        *is_live = false;
                    }
                }
            }
            for slot in local_uses(&instructions[index]) {
                if let Some(is_live) = next_live.get_mut(usize::from(slot)) {
                    *is_live = true;
                }
            }

            if live[index] != next_live {
                live[index] = next_live;
                changed = true;
            }
        }
    }

    live
}

fn instruction_successors(
    index: usize,
    instruction: &Instruction,
    instruction_count: usize,
) -> Vec<usize> {
    use Instruction as I;

    let next = || {
        if index + 1 < instruction_count {
            vec![index + 1]
        } else {
            Vec::new()
        }
    };

    match instruction {
        I::Ifeq(target)
        | I::Ifne(target)
        | I::Iflt(target)
        | I::Ifge(target)
        | I::Ifgt(target)
        | I::Ifle(target)
        | I::If_icmpeq(target)
        | I::If_icmpne(target)
        | I::If_icmplt(target)
        | I::If_icmpge(target)
        | I::If_icmpgt(target)
        | I::If_icmple(target)
        | I::If_acmpeq(target)
        | I::If_acmpne(target)
        | I::Ifnull(target)
        | I::Ifnonnull(target) => {
            let mut successors = vec![usize::from(*target)];
            successors.extend(next());
            successors
        }
        I::Goto(target) => vec![usize::from(*target)],
        I::Goto_w(target) if *target >= 0 => vec![*target as usize],
        I::Tableswitch {
            default, offsets, ..
        } => {
            let mut successors = Vec::with_capacity(offsets.len() + 1);
            if let Some(default) = relative_switch_target_opt(index, *default) {
                successors.push(default);
            }
            for target in offsets {
                if let Some(target) = relative_switch_target_opt(index, *target) {
                    successors.push(target);
                }
            }
            successors
        }
        I::Lookupswitch { default, pairs } => {
            let mut successors = Vec::with_capacity(pairs.len() + 1);
            if let Some(default) = relative_switch_target_opt(index, *default) {
                successors.push(default);
            }
            for target in pairs.values() {
                if let Some(target) = relative_switch_target_opt(index, *target) {
                    successors.push(target);
                }
            }
            successors
        }
        I::Ireturn | I::Lreturn | I::Freturn | I::Dreturn | I::Areturn | I::Return | I::Athrow => {
            Vec::new()
        }
        _ => next(),
    }
}

fn local_uses(instruction: &Instruction) -> Vec<u16> {
    match instruction {
        Instruction::Iload(index)
        | Instruction::Lload(index)
        | Instruction::Fload(index)
        | Instruction::Dload(index)
        | Instruction::Aload(index)
        | Instruction::Iinc(index, _) => vec![u16::from(*index)],
        Instruction::Iload_0
        | Instruction::Lload_0
        | Instruction::Fload_0
        | Instruction::Dload_0
        | Instruction::Aload_0 => vec![0],
        Instruction::Iload_1
        | Instruction::Lload_1
        | Instruction::Fload_1
        | Instruction::Dload_1
        | Instruction::Aload_1 => vec![1],
        Instruction::Iload_2
        | Instruction::Lload_2
        | Instruction::Fload_2
        | Instruction::Dload_2
        | Instruction::Aload_2 => vec![2],
        Instruction::Iload_3
        | Instruction::Lload_3
        | Instruction::Fload_3
        | Instruction::Dload_3
        | Instruction::Aload_3 => vec![3],
        Instruction::Iload_w(index)
        | Instruction::Lload_w(index)
        | Instruction::Fload_w(index)
        | Instruction::Dload_w(index)
        | Instruction::Aload_w(index)
        | Instruction::Iinc_w(index, _)
        | Instruction::Ret_w(index) => vec![*index],
        Instruction::Ret(index) => vec![u16::from(*index)],
        _ => Vec::new(),
    }
}

fn local_defs(instruction: &Instruction) -> Vec<(u16, usize)> {
    match instruction {
        Instruction::Istore(index)
        | Instruction::Fstore(index)
        | Instruction::Astore(index)
        | Instruction::Iinc(index, _) => vec![(u16::from(*index), 1)],
        Instruction::Lstore(index) | Instruction::Dstore(index) => vec![(u16::from(*index), 2)],
        Instruction::Istore_0 | Instruction::Fstore_0 | Instruction::Astore_0 => vec![(0, 1)],
        Instruction::Lstore_0 | Instruction::Dstore_0 => vec![(0, 2)],
        Instruction::Istore_1 | Instruction::Fstore_1 | Instruction::Astore_1 => vec![(1, 1)],
        Instruction::Lstore_1 | Instruction::Dstore_1 => vec![(1, 2)],
        Instruction::Istore_2 | Instruction::Fstore_2 | Instruction::Astore_2 => vec![(2, 1)],
        Instruction::Lstore_2 | Instruction::Dstore_2 => vec![(2, 2)],
        Instruction::Istore_3 | Instruction::Fstore_3 | Instruction::Astore_3 => vec![(3, 1)],
        Instruction::Lstore_3 | Instruction::Dstore_3 => vec![(3, 2)],
        Instruction::Istore_w(index)
        | Instruction::Fstore_w(index)
        | Instruction::Astore_w(index)
        | Instruction::Iinc_w(index, _) => vec![(*index, 1)],
        Instruction::Lstore_w(index) | Instruction::Dstore_w(index) => vec![(*index, 2)],
        _ => Vec::new(),
    }
}

fn relative_switch_target(
    instruction_index: usize,
    offset: i32,
    context: &str,
) -> jvm::Result<usize> {
    relative_switch_target_opt(instruction_index, offset).ok_or_else(|| {
        jvm::Error::VerificationError {
            context: context.to_string(),
            message: format!(
                "Invalid switch target offset {offset} at instruction {instruction_index}"
            ),
        }
    })
}

fn relative_switch_target_opt(instruction_index: usize, offset: i32) -> Option<usize> {
    let target = instruction_index as i64 + i64::from(offset);
    (target >= 0).then_some(target as usize)
}

fn branch_targets(instructions: &[Instruction]) -> BTreeSet<u16> {
    let mut targets = BTreeSet::new();
    for (instruction_index, instruction) in instructions.iter().enumerate() {
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
                targets.insert(*target);
            }
            Instruction::Goto_w(target) => {
                if *target >= 0 {
                    targets.insert(*target as u16);
                }
            }
            Instruction::Tableswitch {
                default, offsets, ..
            } => {
                if let Some(default) = relative_switch_target_opt(instruction_index, *default) {
                    targets.insert(default as u16);
                }
                for offset in offsets {
                    if let Some(target) = relative_switch_target_opt(instruction_index, *offset) {
                        targets.insert(target as u16);
                    }
                }
            }
            Instruction::Lookupswitch { default, pairs } => {
                if let Some(default) = relative_switch_target_opt(instruction_index, *default) {
                    targets.insert(default as u16);
                }
                for offset in pairs.values() {
                    if let Some(target) = relative_switch_target_opt(instruction_index, *offset) {
                        targets.insert(target as u16);
                    }
                }
            }
            _ => {}
        }
    }
    targets
}

fn load_hint_for_instruction(instruction: &Instruction) -> FrameValue {
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
        | Instruction::Aload_w(_) => FrameValue::Object("java/lang/Object".to_string()),
        _ => FrameValue::Top,
    }
}

fn frame_value_from_oomir_type(ty: &Type) -> FrameValue {
    match ty {
        Type::Void => FrameValue::Top,
        Type::Boolean | Type::Char | Type::I8 | Type::I16 | Type::I32 => FrameValue::Integer,
        Type::I64 => FrameValue::Long,
        Type::F32 => FrameValue::Float,
        Type::F64 => FrameValue::Double,
        Type::String => FrameValue::Object("java/lang/String".to_string()),
        Type::Class(name) | Type::Interface(name) => FrameValue::Object(normalize_class_name(name)),
        Type::Array(_) | Type::MutableReference(_) => FrameValue::Object(ty.to_jvm_descriptor()),
        Type::Reference(inner) => {
            if inner.is_jvm_reference_type() {
                frame_value_from_oomir_type(inner)
            } else {
                FrameValue::Object("java/lang/Object".to_string())
            }
        }
    }
}

fn frame_value_from_field_type(field_type: &FieldType) -> FrameValue {
    match field_type {
        FieldType::Base(BaseType::Long) => FrameValue::Long,
        FieldType::Base(BaseType::Float) => FrameValue::Float,
        FieldType::Base(BaseType::Double) => FrameValue::Double,
        FieldType::Base(_) => FrameValue::Integer,
        FieldType::Object(class_name) => FrameValue::Object(normalize_class_name(class_name)),
        FieldType::Array(_) => FrameValue::Object(field_type.class_name()),
    }
}

fn frame_value_from_ldc(constant_pool: &ConstantPool, index: u16) -> jvm::Result<FrameValue> {
    let value = match constant_pool.try_get(index)? {
        Constant::Integer(_) => FrameValue::Integer,
        Constant::Float(_) => FrameValue::Float,
        Constant::String(_) => FrameValue::Object("java/lang/String".to_string()),
        Constant::Class(_) => FrameValue::Object("java/lang/Class".to_string()),
        Constant::MethodType(_) => FrameValue::Object("java/lang/invoke/MethodType".to_string()),
        Constant::MethodHandle { .. } => {
            FrameValue::Object("java/lang/invoke/MethodHandle".to_string())
        }
        Constant::Dynamic { .. } => FrameValue::Object("java/lang/Object".to_string()),
        _ => FrameValue::Top,
    };
    Ok(value)
}

fn frame_value_from_ldc2(constant_pool: &ConstantPool, index: u16) -> jvm::Result<FrameValue> {
    let value = match constant_pool.try_get(index)? {
        Constant::Long(_) => FrameValue::Long,
        Constant::Double(_) => FrameValue::Double,
        _ => FrameValue::Top,
    };
    Ok(value)
}

fn field_type_for_ref(constant_pool: &ConstantPool, field_ref: u16) -> jvm::Result<FieldType> {
    let (_, name_and_type_index) = constant_pool.try_get_field_ref(field_ref)?;
    let (_, descriptor_index) = constant_pool.try_get_name_and_type(*name_and_type_index)?;
    let descriptor = constant_pool.try_get_utf8(*descriptor_index)?;
    FieldType::parse(descriptor)
}

fn method_ref_info(
    constant_pool: &ConstantPool,
    method_ref: u16,
    is_interface: bool,
) -> jvm::Result<MethodRefInfo> {
    let (class_index, name_and_type_index) = if is_interface {
        constant_pool.try_get_interface_method_ref(method_ref)?
    } else {
        constant_pool.try_get_method_ref(method_ref)?
    };
    let class_name = constant_pool.try_get_class(*class_index)?.clone();
    let (name_index, descriptor_index) =
        constant_pool.try_get_name_and_type(*name_and_type_index)?;
    let method_name = constant_pool.try_get_utf8(*name_index)?.clone();
    let descriptor = constant_pool.try_get_utf8(*descriptor_index)?.clone();
    Ok(MethodRefInfo {
        class_name,
        method_name,
        descriptor,
    })
}

fn push_local_value(locals: &mut Vec<FrameValue>, value: FrameValue) {
    let is_category2 = value.is_category2();
    locals.push(value);
    if is_category2 {
        locals.push(FrameValue::Top);
    }
}

fn set_slot_value(locals: &mut Vec<FrameValue>, local_index: u16, value: FrameValue) {
    let local_index = local_index as usize;
    let width = if value.is_category2() { 2 } else { 1 };
    if locals.len() < local_index + width {
        locals.resize(local_index + width, FrameValue::Top);
    }
    locals[local_index] = value;
    if width == 2 {
        locals[local_index + 1] = FrameValue::Top;
    }
}

fn locals_for_stack_map(
    locals: &[FrameValue],
    constant_pool: &mut InternedConstantPool,
    verification_class_cache: &mut HashMap<String, u16>,
) -> jvm::Result<Vec<VerificationType>> {
    let mut end = locals.len();
    while end > 0 && locals[end - 1] == FrameValue::Top {
        end -= 1;
    }

    let mut result = Vec::new();
    let mut index = 0;
    while index < end {
        let value = &locals[index];
        result.push(to_verification_type(
            value,
            constant_pool,
            verification_class_cache,
        )?);
        index += if value.is_category2() { 2 } else { 1 };
    }
    Ok(result)
}

fn stack_for_stack_map(
    stack: &[FrameValue],
    constant_pool: &mut InternedConstantPool,
    verification_class_cache: &mut HashMap<String, u16>,
) -> jvm::Result<Vec<VerificationType>> {
    stack
        .iter()
        .map(|value| to_verification_type(value, constant_pool, verification_class_cache))
        .collect()
}

fn to_verification_type(
    value: &FrameValue,
    constant_pool: &mut InternedConstantPool,
    verification_class_cache: &mut HashMap<String, u16>,
) -> jvm::Result<VerificationType> {
    Ok(match value {
        FrameValue::Top => VerificationType::Top,
        FrameValue::Integer => VerificationType::Integer,
        FrameValue::Float => VerificationType::Float,
        FrameValue::Long => VerificationType::Long,
        FrameValue::Double => VerificationType::Double,
        FrameValue::Null => VerificationType::Null,
        FrameValue::Object(class_name) => {
            let cpool_index = match verification_class_cache.get(class_name) {
                Some(cpool_index) => *cpool_index,
                None => {
                    let cpool_index = constant_pool.add_class(class_name)?;
                    verification_class_cache.insert(class_name.clone(), cpool_index);
                    cpool_index
                }
            };
            VerificationType::Object { cpool_index }
        }
        FrameValue::UninitializedThis => VerificationType::UninitializedThis,
        FrameValue::Uninitialized(offset) => VerificationType::Uninitialized { offset: *offset },
    })
}

fn array_component_value(array: &FrameValue) -> FrameValue {
    let FrameValue::Object(class_name) = array else {
        return FrameValue::Object("java/lang/Object".to_string());
    };
    let Some(component_descriptor) = class_name.strip_prefix('[') else {
        return FrameValue::Object("java/lang/Object".to_string());
    };
    if component_descriptor.starts_with('[') {
        return FrameValue::Object(component_descriptor.to_string());
    }
    if component_descriptor.starts_with('L') && component_descriptor.ends_with(';') {
        return FrameValue::Object(component_descriptor[1..component_descriptor.len() - 1].into());
    }
    match component_descriptor.chars().next() {
        Some('J') => FrameValue::Long,
        Some('F') => FrameValue::Float,
        Some('D') => FrameValue::Double,
        Some('Z' | 'B' | 'C' | 'S' | 'I') => FrameValue::Integer,
        _ => FrameValue::Object("java/lang/Object".to_string()),
    }
}

fn array_descriptor_from_type(array_type: &ArrayType) -> String {
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

fn normalize_class_name(class_name: &str) -> String {
    class_name.replace('.', "/")
}
