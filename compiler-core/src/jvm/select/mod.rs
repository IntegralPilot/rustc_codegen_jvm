//! Direct selection from compact SSA with interval-based JVM local reuse.
mod allocate;
mod arrays;
mod assemble;
mod bits;
mod calls;
mod checked;
mod debug;
mod general;
mod memory;
mod objects;
mod representation;
mod scalar;
mod slots;
mod switches;
mod views;

use super::locals::LocalKind as Kind;
use super::{constants::*, frames};
use crate::classfile::{
    self as jvm,
    attributes::{ExceptionTableEntry, Instruction},
    constant_pool::InternedConstantPool,
};
use crate::ir::*;
use crate::scalar::{BinaryOp, Scalar, ScalarType};
use assemble::{Assembly, Label};
use slots::{kind, parallel_copies};

fn error(message: &str) -> jvm::Error {
    jvm::Error::VerificationError {
        context: "SSA JVM selection".into(),
        message: message.into(),
    }
}

/// Numeric literals and null need no JVM local or live interval. External
/// constants retain their original execution point and effects.
fn literal(body: &Body, value: ValueId) -> Option<Constant> {
    let ValueDef::Inst(id) = body.values[body.resolve(value).index()].def else {
        return None;
    };
    let Op::Constant(id) = body.instructions[id.index()].op else {
        return None;
    };
    match body.constants[id.index()] {
        value @ (Constant::Scalar(_) | Constant::Null(_) | Constant::Uninit(_)) => Some(value),
        _ => None,
    }
}

pub use super::MethodCode;
pub use representation::POINTER_CLASS;

/// Optional source lines are side tables, never executable IR instructions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct SourceLines {
    pub instructions: Vec<Option<u16>>,
    pub terminators: Vec<Option<u16>>,
}

pub trait Constants {
    /// Append code that pushes one value, without using JVM locals or adding
    /// control-flow targets. Frames and source lines belong to the selector.
    fn emit(
        &self,
        index: u32,
        code: &mut Vec<Instruction>,
        cp: &mut InternedConstantPool,
    ) -> jvm::Result<()>;

    fn adapt(
        &self,
        from: TypeId,
        to: TypeId,
        types: &Types,
        code: &mut Vec<Instruction>,
        cp: &mut InternedConstantPool,
    ) -> jvm::Result<()> {
        let _ = (from, to, types, code, cp);
        Err(error("missing ABI adaptation provider"))
    }
}

#[derive(Default)]
pub struct Options<'a> {
    pub lines: Option<&'a SourceLines>,
    pub relative_pointer_abi: bool,
    pub constants: Option<&'a dyn Constants>,
    pub debug: Option<&'a DebugInfo>,
    pub bootstrap: Option<&'a mut Vec<jvm::attributes::BootstrapMethod>>,
}

struct Selector<'a> {
    body: &'a Body,
    types: &'a Types,
    cp: &'a mut InternedConstantPool,
    assembly: Assembly,
    blocks: Vec<Label>,
    slots: Vec<Option<u16>>,
    next_slot: u16,
    scratch_used: bool,
    exception_slot: Option<u16>,
    exceptions: Vec<ExceptionTableEntry>,
    handlers: Vec<(Label, BlockId)>,
    copies: Vec<(u16, u16, Kind)>,
    live: crate::opt::Live,
    storage: Vec<u16>,
    constants: Option<&'a dyn Constants>,
    bootstrap: Option<&'a mut Vec<jvm::attributes::BootstrapMethod>>,
}

/// Select one typed body into JVM code, validating its SSA and storage invariants.
pub fn compile(
    body: &Body,
    types: &Types,
    cp: &mut InternedConstantPool,
) -> jvm::Result<MethodCode> {
    compile_with_options(body, types, cp, Options::default())
}

pub fn compile_with_options(
    body: &Body,
    types: &Types,
    cp: &mut InternedConstantPool,
    options: Options<'_>,
) -> jvm::Result<MethodCode> {
    let Options {
        lines,
        relative_pointer_abi,
        constants,
        debug,
        bootstrap,
    } = options;
    if let Some(lines) = lines {
        if lines.instructions.len() != body.instructions.len()
            || lines.terminators.len() != body.blocks.len()
        {
            return Err(error("source line tables do not match SSA body"));
        }
    }
    verify_with_debug(body, types, debug).map_err(|e| error(&e.0))?;
    let live =
        crate::opt::live_with_roots(body, types, debug.into_iter().flat_map(|d| d.roots(body)));
    let order = body.layout();
    let allocation = allocate::allocate(body, types, &live, relative_pointer_abi, debug, &order)?;
    let mut debug = debug.map(|d| debug::Debug::new(d, body));
    let mut s = Selector {
        body,
        types,
        cp,
        assembly: Assembly::default(),
        blocks: Vec::with_capacity(body.blocks.len()),
        slots: allocation.slots,
        next_slot: allocation.count,
        scratch_used: false,
        exception_slot: None,
        exceptions: Vec::new(),
        handlers: Vec::new(),
        copies: Vec::new(),
        live,
        storage: Vec::new(),
        constants,
        bootstrap,
    };
    for _ in &body.blocks {
        s.blocks.push(s.assembly.label());
    }
    let mut line_numbers = Vec::<jvm::attributes::LineNumber>::new();
    let mut mark_line = |start, end, line| -> jvm::Result<()> {
        if let Some(line_number) = line {
            if start != end
                && line_numbers
                    .last()
                    .is_none_or(|last| last.line_number != line_number)
            {
                line_numbers.push(jvm::attributes::LineNumber {
                    start_pc: u16::try_from(start)?,
                    line_number,
                });
            }
        }
        Ok(())
    };
    let mut initial = Vec::new();
    for &param in &body.blocks[body.entry.index()].params {
        let value = s.initial_value(param)?;
        frames::push_local_value(&mut initial, value);
        if relative_pointer_abi
            && matches!(types.get(body.value_type(param)), Some(Type::Pointer(_)))
        {
            frames::push_local_value(&mut initial, frames::FrameValue::Long);
            frames::push_local_value(&mut initial, frames::FrameValue::Long);
        }
    }
    if body.blocks.iter().any(|b| {
        matches!(
            b.terminator,
            Some(
                Terminator::Invoke { .. }
                    | Terminator::Rethrow
                    | Terminator::Throw {
                        unwind: Some(_),
                        ..
                    }
            )
        )
    }) {
        s.exception_slot = Some(s.next_slot);
        s.next_slot = s
            .next_slot
            .checked_add(1)
            .ok_or_else(|| error("JVM local limit"))?;
    }
    for block in order {
        s.assembly.bind(s.blocks[block.index()]);
        if block == body.entry {
            s.initialize_storage()?;
            if let Some(debug) = &mut debug {
                debug.allocate(&mut s)?;
            }
            if relative_pointer_abi {
                s.materialize_parameters()?;
            }
            // JVM byte/short parameters are sign-extended by Java callers.
            for &param in &body.blocks[block.index()].params {
                if matches!(
                    s.types.get(s.body.value_type(param)),
                    Some(Type::Scalar(ScalarType::U8 | ScalarType::U16))
                ) {
                    s.load(param)?;
                    s.normalize_value(param);
                    s.store(param)?;
                }
            }
        }
        let mut events = debug
            .as_mut()
            .map(|debug| {
                debug.start(block);
                debug.events(block)
            })
            .unwrap_or_default()
            .into_iter()
            .peekable();
        for position in 0..=body.blocks[block.index()].instructions.len() {
            while events
                .peek()
                .is_some_and(|event| event.position as usize == position)
            {
                let event = events.next().unwrap();
                let start = s.assembly.code.len();
                debug.as_mut().unwrap().apply(&mut s, event.change)?;
                if lines.is_some() {
                    mark_line(start, s.assembly.code.len(), event.line)?;
                }
            }
            let Some(&inst) = body.blocks[block.index()].instructions.get(position) else {
                break;
            };
            if s.live.instructions[inst.index()] {
                let start = s.assembly.code.len();
                s.instruction(inst)?;
                if let Some(debug) = &mut debug {
                    debug.mark(start, s.assembly.code.len());
                }
                mark_line(
                    start,
                    s.assembly.code.len(),
                    lines.and_then(|l| l.instructions[inst.index()]),
                )?;
            }
        }
        let start = s.assembly.code.len();
        s.terminator(body.blocks[block.index()].terminator.unwrap())?;
        if let Some(debug) = &mut debug {
            debug.mark(start, s.assembly.code.len());
        }
        mark_line(
            start,
            s.assembly.code.len(),
            lines.and_then(|l| l.terminators[block.index()]),
        )?;
    }
    for (i, (label, block)) in s.handlers.iter().copied().enumerate() {
        s.assembly.bind(label);
        s.exceptions[i].handler_pc = u16::try_from(s.assembly.code.len())?;
        s.assembly
            .code
            .push(Kind::Reference.store(s.exception_slot.unwrap()));
        s.assembly
            .branch(Instruction::Goto_w(0), s.blocks[block.index()]);
    }
    let max_locals = s
        .next_slot
        .checked_add(if s.scratch_used { 2 } else { 0 })
        .ok_or_else(|| error("JVM local limit"))?;
    let instructions = s.assembly.finish()?;
    let analysis = frames::analyze(
        &instructions,
        &initial,
        &[],
        usize::from(max_locals),
        s.cp,
        "SSA scalar body",
        &s.exceptions,
    )?;
    let mut attributes = frames::build_stack_map_attributes_from_analysis(
        &instructions,
        &initial,
        s.cp,
        &s.exceptions,
        &analysis,
    )?;
    if !line_numbers.is_empty() {
        attributes.push(jvm::attributes::Attribute::LineNumberTable {
            name_index: s.cp.add_utf8("LineNumberTable")?,
            line_numbers,
        });
    }
    if let Some(debug) = debug {
        if let Some(attribute) = debug.finish(types, s.cp, &instructions)? {
            attributes.push(attribute);
        }
    }
    let max_stack = analysis.max_stack;
    Ok(MethodCode {
        instructions,
        max_stack,
        max_locals,
        attributes,
        exceptions: s.exceptions,
    })
}

impl Selector<'_> {
    fn scalar_type(&self, value: ValueId) -> jvm::Result<ScalarType> {
        match self.types.get(self.body.value_type(value)) {
            Some(Type::Scalar(ty)) => {
                kind(ty)?;
                Ok(ty)
            }
            _ => Err(error("non-scalar value in scalar selection")),
        }
    }
    fn slot(&self, value: ValueId) -> u16 {
        self.slots[self.body.resolve(value).index()].expect("assigned SSA slot")
    }
    fn load(&mut self, value: ValueId) -> jvm::Result<()> {
        match literal(self.body, value) {
            Some(Constant::Scalar(value)) => self.constant(value)?,
            Some(Constant::Null(_)) => self.assembly.code.push(Instruction::Aconst_null),
            Some(Constant::Uninit(_)) => self.assembly.code.push(match self.value_kind(value)? {
                Kind::Int => Instruction::Iconst_0,
                Kind::Long => Instruction::Lconst_0,
                Kind::Float => Instruction::Fconst_0,
                Kind::Double => Instruction::Dconst_0,
                Kind::Reference => Instruction::Aconst_null,
            }),
            None => self
                .assembly
                .code
                .push(self.value_kind(value)?.load(self.slot(value))),
            _ => unreachable!(),
        }
        Ok(())
    }
    fn store(&mut self, value: ValueId) -> jvm::Result<()> {
        self.assembly
            .code
            .push(self.value_kind(value)?.store(self.slot(value)));
        Ok(())
    }
    fn copies(&mut self, edge: EdgeId) -> jvm::Result<()> {
        let edge = &self.body.edges[edge.index()];
        self.copies.clear();
        for (&to, &from) in self.body.blocks[edge.target.index()]
            .params
            .iter()
            .zip(&edge.args)
        {
            if !self.live.values[to.index()] {
                continue;
            }
            if literal(self.body, from).is_none() {
                self.copies
                    .push((self.slot(to), self.slot(from), self.value_kind(to)?));
            }
        }
        self.scratch_used |=
            parallel_copies(&mut self.copies, self.next_slot, &mut self.assembly.code);
        // Emit literal assignments after every register source has been read.
        // A literal destination may overlap a still-needed source in the cycle.
        for (&to, &from) in self.body.blocks[edge.target.index()]
            .params
            .iter()
            .zip(&edge.args)
        {
            if self.live.values[to.index()] && literal(self.body, from).is_some() {
                self.load(from)?;
                self.store(to)?;
            }
        }
        Ok(())
    }
    fn jump(&mut self, edge: EdgeId) -> jvm::Result<()> {
        self.copies(edge)?;
        self.assembly.branch(
            Instruction::Goto_w(0),
            self.blocks[self.body.edges[edge.index()].target.index()],
        );
        Ok(())
    }
    fn terminator(&mut self, term: Terminator) -> jvm::Result<()> {
        match term {
            Terminator::Jump(edge) => self.jump(edge)?,
            Terminator::Branch { condition, yes, no } => {
                let yes_label = self.assembly.label();
                self.load(condition)?;
                self.assembly.branch(Instruction::Ifne(0), yes_label);
                self.jump(no)?;
                self.assembly.bind(yes_label);
                self.jump(yes)?;
            }
            Terminator::Switch {
                value,
                cases,
                otherwise,
            } => self.switch(value, cases, otherwise)?,
            Terminator::Return(Some(value)) => {
                self.argument(value)?;
                self.assembly.code.push(self.value_kind(value)?.return_op());
            }
            Terminator::Return(None) => self.assembly.code.push(Instruction::Return),
            Terminator::Invoke {
                inst,
                normal,
                unwind,
            } => {
                // Handler block parameters must already be in their slots at
                // the throw point: JVM exceptional edges cannot run copies.
                self.copies(unwind)?;
                let start = u16::try_from(self.assembly.code.len())?;
                self.instruction(inst)?;
                let end = u16::try_from(self.assembly.code.len())?;
                self.protect(start, end, unwind);
                self.jump(normal)?;
            }
            Terminator::Rethrow => {
                self.assembly.code.extend([
                    Kind::Reference.load(self.exception_slot.unwrap()),
                    Instruction::Athrow,
                ]);
            }
            Terminator::Unreachable => self
                .assembly
                .code
                .extend([Instruction::Aconst_null, Instruction::Athrow]),
            Terminator::Throw { value, unwind } => {
                if let Some(edge) = unwind {
                    self.copies(edge)?;
                }
                let start = u16::try_from(self.assembly.code.len())?;
                self.load(value)?;
                self.assembly.code.push(Instruction::Athrow);
                if let Some(edge) = unwind {
                    self.protect(start, u16::try_from(self.assembly.code.len())?, edge);
                }
            }
        }
        Ok(())
    }
    fn protect(&mut self, start: u16, end: u16, unwind: EdgeId) {
        if start == end {
            return;
        }
        let handler = self.assembly.label();
        self.handlers
            .push((handler, self.body.edges[unwind.index()].target));
        self.exceptions.push(ExceptionTableEntry {
            range_pc: start..end,
            handler_pc: 0,
            catch_type: 0,
        });
    }
}

#[cfg(test)]
mod tests;

#[cfg(test)]
mod debug_tests;

#[cfg(test)]
mod object_tests;

#[cfg(test)]
mod view_tests;

#[cfg(test)]
mod unwind_tests;
