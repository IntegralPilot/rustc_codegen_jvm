//! Seal semantic emission into one compact, typed SSA body.
//!
//! Names and rich representation operands are construction inputs. Completed
//! computational bodies contain only IDs, pooled payloads and explicit edges.
use crate::oomir;
use jvm_compiler_core::{
    ir,
    scalar::{Scalar, ScalarType},
};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::sync::Arc;
mod arithmetic;
mod arrays;
mod context;
mod operations;
mod types;
mod wrappers;
pub(crate) use context::Context;
use types::Vocabulary;
pub(crate) use types::source_type;

type Result<T> = std::result::Result<T, String>;

pub(crate) fn seal(function: oomir::Function, context: &Context) -> Result<oomir::SsaFunction> {
    static DUMP: std::sync::OnceLock<Option<String>> = std::sync::OnceLock::new();
    if let Some(names) = DUMP.get_or_init(|| std::env::var("RCGJ_SSA_DUMP").ok()) {
        let qualified_name = format!(
            "{}::{}",
            function.owner_class.as_deref().unwrap_or(""),
            function.name
        );
        if names.split(',').any(|name| qualified_name.contains(name)) {
            let path = format!(
                concat!(
                    env!("CARGO_MANIFEST_DIR"),
                    "/.generated/oomir-redesign/ssa-source-{:016x}.txt"
                ),
                crate::stable_hash::hash_value(&qualified_name)
            );
            let _ = std::fs::write(path, format!("{function:#?}"));
        }
    }
    let mut vocabulary = Vocabulary::default();
    vocabulary.signature(&function.signature);
    for variable in &function.debug_variables {
        vocabulary.add(&variable.ty);
    }
    let body = &function.body;
    for block in body.basic_blocks.values() {
        for instruction in &block.instructions {
            vocabulary.instruction(instruction);
            if let oomir::Instruction::ConstructObject { class_name, .. } = instruction
                && let Some(fields) = context.constructors.get(class_name)
            {
                for ty in fields {
                    vocabulary.add(ty);
                }
            }
        }
    }
    vocabulary.add_wrappers(context);
    let mut builder = ir::Builder::new(&vocabulary.types, vocabulary.id(&function.signature.ret));
    let mut blocks = HashMap::default();
    let mut handlers = HashSet::default();
    blocks.insert(body.entry.clone(), builder.create_block());
    let mut names = body.basic_blocks.keys().cloned().collect::<Vec<_>>();
    names.sort_unstable();
    for name in &names {
        if !blocks.contains_key(name) {
            blocks.insert(name.clone(), builder.create_block());
        }
        for instruction in &body.basic_blocks[name].instructions {
            if let oomir::Instruction::UnwindStart { target } = instruction {
                handlers.insert(target.clone());
            }
            if let oomir::Instruction::Label { name } = instruction {
                if !blocks.contains_key(name) {
                    blocks.insert(name.clone(), builder.create_block());
                }
            }
        }
    }
    let mut handlers = handlers.into_iter().collect::<Vec<_>>();
    handlers.sort_unstable();
    let handlers = handlers
        .into_iter()
        .map(|name| (name, builder.create_block()))
        .collect();
    let mut emission = Emission {
        builder,
        vocabulary: &vocabulary,
        context,
        variables: Default::default(),
        named_types: HashMap::default(),
        blocks,
        constants: Vec::new(),
        unwind: None,
        line: None,
        source_file: None,
        lines: None,
        debug: ir::DebugInfo::default(),
        debug_names: HashMap::default(),
        handlers,
    };
    // Record declared source representations for zero-sized local initialization.
    for name in &names {
        for instruction in &body.basic_blocks[name].instructions {
            instruction.visit_operands(|operand| {
                if let oomir::Operand::Variable { name, ty } = operand {
                    let ty = vocabulary.id(ty);
                    if vocabulary.types.get(ty).unwrap().carrier() == 5 {
                        emission.named_types.entry(name.clone()).or_insert(ty);
                    }
                }
            });
            let hint = match instruction {
                oomir::Instruction::NewArray {
                    dest, element_type, ..
                } => Some((dest, oomir::Type::Array(Box::new(element_type.clone())))),
                oomir::Instruction::ConstructObject {
                    dest, class_name, ..
                } => Some((dest, oomir::Type::Class(class_name.clone()))),
                _ => None,
            };
            if let Some((name, ty)) = hint {
                emission
                    .named_types
                    .entry(name.clone())
                    .or_insert(vocabulary.id(&ty));
            }
        }
    }
    for (index, variable) in function.debug_variables.iter().enumerate() {
        emission
            .debug
            .locals
            .push(ir::DebugLocal::Value(vocabulary.id(&variable.ty)));
        emission.debug.variables.push(ir::DebugVariable {
            name: variable.name.clone(),
            local: index as u32,
        });
        emission
            .debug_names
            .entry(variable.oomir_name.clone())
            .or_insert_with(Vec::new)
            .push(index as u32);
    }
    // Outlined cleanup state may travel through a normal edge before any
    // exception exists. Each real handler supplies a fresh exception definition.
    let exception = emission.constant(oomir::Constant::Null(oomir::Type::Class(
        "java/lang/Throwable".into(),
    )))?;
    emission.write("__rust_unwind_exception", exception)?;
    for (index, (name, ty)) in function.signature.params.iter().enumerate() {
        if !ty.has_jvm_value() {
            continue;
        }
        let value = emission
            .builder
            .parameter(emission.builder.current(), vocabulary.id(ty));
        emission.write(&format!("param_{index}"), value)?;
        let synthetic_main = function.name == "main"
            && index == 0
            && matches!(ty, oomir::Type::Array(inner) if matches!(inner.as_ref(), oomir::Type::Class(name) if name == "java/lang/String"));
        if !synthetic_main {
            let local = if name == oomir::CALLER_LOCATION_PARAM_NAME {
                name.clone()
            } else {
                format!("_{}", index + 1)
            };
            emission.write(&local, value)?;
        }
    }
    let zero_locals = emission
        .named_types
        .iter()
        .filter_map(|(name, &ty)| {
            let source = source_type(&vocabulary.types, ty);
            (name
                .strip_prefix('_')
                .is_some_and(|n| !n.is_empty() && n.bytes().all(|c| c.is_ascii_digit()))
                && context.is_zero(&source)
                && !emission.variables[5].contains_key(name))
            .then(|| (name.clone(), source))
        })
        .collect::<Vec<_>>();
    for (name, ty) in zero_locals {
        let value = emission.zero_value(&ty)?;
        emission.write(&name, value)?;
    }
    // Arguments live in a distinct entry; source control-flow joins carry
    // mutable bindings through their own block parameters.
    emission
        .builder
        .jump(emission.blocks[&body.entry], Vec::new());
    let body = function.body;
    let entry = body.entry;
    let mut source = body.basic_blocks;
    names.sort_by_key(|name| (name != &entry, name.clone()));
    for name in names {
        emission.builder.switch_to(emission.blocks[&name]);
        emission.unwind = None;
        for instruction in source.remove(&name).unwrap().instructions {
            if emission.builder.body.blocks[emission.builder.current().index()]
                .terminator
                .is_some()
                && !matches!(instruction, oomir::Instruction::Label { .. })
            {
                continue;
            }
            emission
                .instruction(instruction)
                .map_err(|e| format!("{} in block {name}: {e}", function.name))?;
        }
        if emission.builder.body.blocks[emission.builder.current().index()]
            .terminator
            .is_none()
        {
            emission.builder.terminate(ir::Terminator::Unreachable);
        }
    }
    // Cleanup blocks can also be entered normally by outlined dispatchers.
    // Only exceptional edges enter these landing pads and define a new caught
    // value; ordinary edges preserve their incoming exception SSA binding.
    let mut handlers = std::mem::take(&mut emission.handlers)
        .into_iter()
        .collect::<Vec<_>>();
    handlers.sort_by(|a, b| a.0.cmp(&b.0));
    for (name, landing) in handlers {
        emission.builder.switch_to(landing);
        emission.unwind = None;
        emission.line = None;
        let ty = emission.ty(&oomir::Type::Class("java/lang/Throwable".into()));
        let value = emission.builder.emit(ir::Op::Exception, Some(ty)).unwrap();
        emission.write("__rust_unwind_exception", value)?;
        emission.builder.jump(emission.blocks[&name], Vec::new());
    }
    emission.sync_lines();
    let Emission {
        builder,
        lines,
        source_file,
        constants,
        debug,
        variables,
        ..
    } = emission;
    let ir = builder.finish().map_err(|e| {
        let mut variables = variables
            .into_iter()
            .enumerate()
            .flat_map(|(carrier, variables)| {
                variables
                    .into_iter()
                    .map(move |(name, id)| (id, (name, carrier)))
            })
            .collect::<Vec<_>>();
        variables.sort_by_key(|(id, _)| id.index());
        format!("{}: {e}; bindings: {variables:?}", function.name)
    })?;
    let lines = lines.map(|mut lines| {
        lines.instructions.resize(ir.instructions.len(), None);
        lines.terminators.resize(ir.blocks.len(), None);
        lines
    });
    Ok(oomir::Function {
        name: function.name,
        owner_class: function.owner_class,
        signature: function.signature,
        debug_variables: Vec::new(),
        body: Arc::new(oomir::SsaBody {
            ir,
            types: vocabulary.into_types(),
            lines,
            source_file,
            constants,
            debug: (!debug.locals.is_empty()).then_some(debug),
        }),
    })
}

struct Emission<'a> {
    builder: ir::Builder<'a>,
    vocabulary: &'a Vocabulary,
    context: &'a Context,
    variables: [HashMap<String, ir::VariableId>; 6],
    named_types: HashMap<String, ir::TypeId>,
    blocks: HashMap<String, ir::BlockId>,
    constants: Vec<oomir::Constant>,
    unwind: Option<ir::BlockId>,
    line: Option<u16>,
    source_file: Option<String>,
    lines: Option<jvm_compiler_core::jvm::select::SourceLines>,
    debug: ir::DebugInfo,
    debug_names: HashMap<String, Vec<u32>>,
    handlers: HashMap<String, ir::BlockId>,
}
impl Emission<'_> {
    fn ty(&self, ty: &oomir::Type) -> ir::TypeId {
        self.vocabulary.id(ty)
    }
    fn variable(&mut self, name: &str, ty: ir::TypeId) -> ir::VariableId {
        let carrier = self.vocabulary.types.get(ty).unwrap().carrier();
        let canonical = if carrier == 5 {
            self.ty(&types::object())
        } else {
            ty
        };
        let variables = &mut self.variables[carrier as usize];
        if let Some(&variable) = variables.get(name) {
            return variable;
        }
        let variable = if name
            .strip_prefix('_')
            .is_some_and(|tail| !tail.is_empty() && tail.bytes().all(|c| c.is_ascii_digit()))
        {
            self.builder.local(canonical)
        } else {
            self.builder.variable(canonical)
        };
        variables.insert(name.to_owned(), variable);
        variable
    }
    fn write(&mut self, name: &str, value: ir::ValueId) -> Result<()> {
        let ty = self.builder.body.value_type(value);
        if self.vocabulary.types.get(ty) == Some(ir::Type::Unit) {
            return Ok(());
        }
        let variable = self.variable(name, ty);
        self.builder.define_carrier(variable, value);
        if let Some(locals) = self.debug_names.get(name).cloned() {
            for local in locals {
                let ir::DebugLocal::Value(ty) = self.debug.locals[local as usize] else {
                    unreachable!()
                };
                // A source name can temporarily hold a different ABI carrier
                // (closure argument tuple, boxing temporary, etc.). Debugging
                // must never execute conversions or introduce exceptions.
                let actual = self.builder.body.value_type(value);
                let same_carrier = actual == ty || self.vocabulary.same_carrier(actual, ty);
                if !same_carrier {
                    self.debug
                        .push(&self.builder, ir::DebugChange::Clear(local))
                        .line = self.line;
                    continue;
                }
                let value = if actual == ty {
                    value
                } else {
                    self.builder
                        .emit(ir::Op::Reinterpret(value), Some(ty))
                        .unwrap()
                };
                self.debug
                    .push(&self.builder, ir::DebugChange::Set { local, value })
                    .line = self.line;
            }
        }
        Ok(())
    }
    fn operand(&mut self, operand: oomir::Operand) -> Result<ir::ValueId> {
        match operand {
            oomir::Operand::Constant(value) => self.constant(value),
            oomir::Operand::Variable { name, ty } => {
                let ty = self.ty(&ty);
                if self.vocabulary.types.get(ty) == Some(ir::Type::Unit) {
                    return self.constant(oomir::Constant::Unit);
                }
                let variable = self.variable(&name, ty);
                let value = self.builder.read(variable);
                self.adapt(value, ty)
            }
        }
    }
    fn constant(&mut self, value: oomir::Constant) -> Result<ir::ValueId> {
        let ty = self.ty(&oomir::Type::from_constant(&value));
        if let Some(scalar) = oomir::scalar::from_constant(&value)
            && let Some(ir::Type::Scalar(target)) = self.vocabulary.types.get(ty)
        {
            return Ok(self.builder.constant(
                ty,
                Scalar::from_bits(target, scalar.bits()).expect("scalar representation"),
            ));
        }
        let constant = match value {
            oomir::Constant::Unit => ir::Constant::Unit,
            oomir::Constant::Null(_) => ir::Constant::Null(ty),
            value => {
                let index = self.constants.len() as u32;
                self.constants.push(value);
                ir::Constant::External { index, ty }
            }
        };
        let id = ir::ConstId::new(self.builder.body.constants.len());
        self.builder.body.constants.push(constant);
        Ok(self.emit(ir::Op::Constant(id), Some(ty)).unwrap())
    }
    fn adapt(&mut self, value: ir::ValueId, ty: ir::TypeId) -> Result<ir::ValueId> {
        if self.builder.body.value_type(value) == ty {
            return Ok(value);
        }
        let from = self
            .vocabulary
            .types
            .get(self.builder.body.value_type(value))
            .unwrap();
        let to = self.vocabulary.types.get(ty).unwrap();
        if to == ir::Type::Unit {
            return self.constant(oomir::Constant::Unit);
        }
        if from == ir::Type::Unit && to.carrier() == 5 {
            return self.constant(oomir::Constant::Null(source_type(
                &self.vocabulary.types,
                ty,
            )));
        }
        if let ir::Type::Pointer(inner) = from
            && !matches!(to, ir::Type::Pointer(_))
            && ty != self.ty(&types::object())
            && !self
                .vocabulary
                .same_carrier(self.builder.body.value_type(value), ty)
        {
            let value = self.emit(ir::Op::Load(value), Some(inner)).unwrap();
            return self.adapt(value, ty);
        }
        if !self
            .vocabulary
            .same_carrier(self.builder.body.value_type(value), ty)
            && let Some(value) = self.wrapper_adaptation(value, ty)?
        {
            return Ok(value);
        }
        let op = if matches!((from, to), (ir::Type::Scalar(_), ir::Type::Scalar(_))) {
            ir::Op::Cast(value)
        } else if (to
            == self
                .vocabulary
                .types
                .get(self.ty(&types::object()))
                .unwrap()
            || self
                .vocabulary
                .same_carrier(self.builder.body.value_type(value), ty))
            && from.carrier() == 5
        {
            ir::Op::Reinterpret(value)
        } else {
            ir::Op::Adapt(value)
        };
        Ok(self.emit(op, Some(ty)).unwrap())
    }
    fn emit(&mut self, op: ir::Op, ty: Option<ir::TypeId>) -> Option<ir::ValueId> {
        let block = self.builder.current();
        let result = if let Some(unwind) = self.unwind {
            self.builder.invoke(op, ty, unwind)
        } else {
            self.builder.emit(op, ty)
        };
        self.sync_lines();
        if let Some(lines) = &mut self.lines {
            lines.terminators[block.index()] = self.line;
        }
        result
    }
    fn sync_lines(&mut self) {
        let Some(lines) = &mut self.lines else {
            return;
        };
        lines
            .instructions
            .resize(self.builder.body.instructions.len(), self.line);
        lines
            .terminators
            .resize(self.builder.body.blocks.len(), self.line);
        lines.terminators[self.builder.current().index()] = self.line;
    }
}

#[cfg(test)]
mod tests;
