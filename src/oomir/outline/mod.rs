//! Split oversized SSA bodies into bounded regions. A dispatcher loop transfers
//! control without growing the JVM stack, including edges through large loops.
use super::{SsaBody, SsaFunction as Function};
use jvm_compiler_core::{
    ir::*,
    jvm::select::SourceLines,
    scalar::{Scalar, ScalarType},
};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::sync::Arc;
mod extract;
mod frame;
use frame::Frame;
pub(crate) const METHOD_PREFIX: &str = "$outlined$";
const MAX_WORK: usize = 1800;

fn integer(b: &mut Builder<'_>, ty: TypeId, value: i32) -> ValueId {
    b.constant(ty, Scalar::integer(ScalarType::I32, value as u128).unwrap())
}
fn cost(op: Op, source: &SsaBody) -> usize {
    match op {
        Op::Constant(id) => match source.ir.constants[id.index()] {
            Constant::External { index, .. } => {
                super::constant_instruction_cost(&source.constants[index as usize]).min(1024)
            }
            _ => 1,
        },
        Op::Call { .. } | Op::Project { .. } | Op::FunctionPointer { .. } => 8,
        Op::GetField { .. } | Op::SetField { .. } | Op::Load(_) | Op::Store { .. } => 4,
        _ => 1,
    }
}
fn work(source: &SsaBody) -> usize {
    source
        .ir
        .instructions
        .iter()
        .map(|i| cost(i.op, source))
        .sum()
}

pub(crate) fn needed(function: &Function) -> bool {
    function.name != "<init>"
        && !function.name.starts_with(METHOD_PREFIX)
        && work(&function.body) > MAX_WORK
}

pub(crate) fn split(
    mut function: Function,
    owner: &str,
    interface: bool,
) -> Result<Vec<Function>, String> {
    let source = function.body;
    let mut source = Arc::unwrap_or_clone(source);
    split_blocks(&mut source);
    let order = source.ir.layout();
    let mut groups = vec![usize::MAX; source.ir.blocks.len()];
    let mut chunks: Vec<Vec<BlockId>> = Vec::new();
    let mut budget = MAX_WORK;
    for block in order {
        let data = &source.ir.blocks[block.index()];
        let mut amount: usize = data
            .instructions
            .iter()
            .map(|&i| cost(source.ir.instructions[i.index()].op, &source))
            .sum();
        if let Some(Terminator::Invoke { inst, .. }) = data.terminator {
            amount += cost(source.ir.instructions[inst.index()].op, &source);
        }
        if chunks.is_empty() || amount > budget {
            chunks.push(Vec::new());
            budget = MAX_WORK;
        }
        groups[block.index()] = chunks.len() - 1;
        chunks.last_mut().unwrap().push(block);
        budget = budget.saturating_sub(amount);
    }
    if chunks.len() < 2 {
        function.body = Arc::new(source);
        return Ok(vec![function]);
    }
    let mut types = (*source.types).clone();
    let frame = Frame::new(&source, &mut types, &groups);
    let types = Arc::new(types);
    let mut entries = vec![HashSet::default(); chunks.len()];
    entries[groups[source.ir.entry.index()]].insert(source.ir.entry);
    for (index, block) in source.ir.blocks.iter().enumerate() {
        if groups[index] == usize::MAX {
            continue;
        }
        block
            .terminator
            .unwrap()
            .visit_edges(&source.ir.cases, |edge| {
                let target = source.ir.edges[edge.index()].target;
                if groups[index] != groups[target.index()] {
                    entries[groups[target.index()]].insert(target);
                }
            });
    }
    let entries: Vec<Vec<_>> = entries
        .into_iter()
        .map(|entries| {
            let mut entries: Vec<_> = entries.into_iter().collect();
            entries.sort_unstable();
            entries
        })
        .collect();
    let identity = crate::stable_hash::short_hash(
        &format!("{owner}::{}{}", function.name, function.signature),
        12,
    );
    let names: Vec<_> = (0..chunks.len())
        .map(|n| format!("{METHOD_PREFIX}{}{identity}${n}", function.name))
        .collect();
    let mut result = Vec::with_capacity(chunks.len() + 1);
    for (group, blocks) in chunks.iter().enumerate() {
        let body = extract::region(&source, Arc::clone(&types), &frame, blocks, &entries[group])?;
        let params = std::iter::once(("entry".into(), super::Type::I32))
            .chain(frame.arrays.iter().enumerate().map(|(n, &ty)| {
                (
                    format!("frame{n}"),
                    super::construct::source_type(&types, ty),
                )
            }))
            .collect();
        result.push(Function {
            name: names[group].clone(),
            owner_class: Some(owner.into()),
            signature: super::Signature {
                params,
                ret: Box::new(super::Type::I32),
                is_static: true,
            },
            body: Arc::new(body),
            debug_variables: Vec::new(),
        });
    }
    let body = dispatcher(
        &source,
        Arc::clone(&types),
        &frame,
        &entries,
        &names,
        owner,
        interface,
    )?;
    function.body = Arc::new(body);
    result.push(function);
    Ok(result)
}

fn dispatcher(
    source: &SsaBody,
    types: Arc<Types>,
    frame: &Frame,
    entries: &[Vec<BlockId>],
    names: &[String],
    owner: &str,
    interface: bool,
) -> Result<SsaBody, String> {
    let mut b = Builder::new(&types, source.ir.return_type);
    let entry = b.current();
    let params: Vec<_> = source.ir.blocks[source.ir.entry.index()]
        .params
        .iter()
        .map(|&value| b.parameter(entry, source.ir.value_type(value)))
        .collect();
    let arrays: Vec<_> = frame
        .arrays
        .iter()
        .enumerate()
        .map(|(i, &ty)| {
            let count = integer(&mut b, frame.elements[0], frame.lengths[i] as i32);
            b.emit(Op::NewArray(count), Some(ty)).unwrap()
        })
        .collect();
    for (&old, value) in source.ir.blocks[source.ir.entry.index()]
        .params
        .iter()
        .zip(params)
    {
        frame.store(&mut b, &arrays, frame.values[old.index()].unwrap(), value);
    }
    b.body.slots = source.ir.slots.clone();
    for (i, &location) in frame.storage.iter().enumerate() {
        let value = b
            .emit(Op::AddressOfSlot(SlotId::new(i)), Some(location.ty))
            .unwrap();
        frame.store(&mut b, &arrays, location, value);
    }
    let dispatch = b.create_block();
    let finished = b.create_block();
    let selector = b.parameter(dispatch, frame.elements[0]);
    let initial = integer(&mut b, frame.elements[0], source.ir.entry.index() as i32);
    b.jump(dispatch, vec![initial]);
    b.switch_to(dispatch);
    let calls: Vec<_> = entries.iter().map(|_| b.create_block()).collect();
    let mut cases = Vec::new();
    for (group, entries) in entries.iter().enumerate() {
        for entry in entries {
            cases.push((
                Scalar::integer(ScalarType::I32, entry.index() as u128).unwrap(),
                calls[group],
            ));
        }
    }
    b.switch(selector, cases, finished);
    for (group, block) in calls.into_iter().enumerate() {
        b.switch_to(block);
        let method = b.method(MethodRef {
            owner: owner.into(),
            name: names[group].clone(),
            params: std::iter::once(frame.elements[0])
                .chain(frame.arrays.iter().copied())
                .collect(),
            returns: frame.elements[0],
            interface,
        });
        let args = b.args(std::iter::once(selector).chain(arrays.iter().copied()));
        let next = b
            .emit(
                Op::Call {
                    method,
                    kind: CallKind::JvmStatic,
                    args,
                },
                Some(frame.elements[0]),
            )
            .unwrap();
        b.jump(dispatch, vec![next]);
    }
    b.switch_to(finished);
    let value = frame
        .result
        .map(|location| frame.load(&mut b, &arrays, location));
    b.terminate(Terminator::Return(value));
    let ir = b.finish().map_err(|e| e.to_string())?;
    Ok(SsaBody {
        ir,
        types,
        lines: None,
        source_file: source.source_file.clone(),
        constants: Vec::new(),
        debug: None,
    })
}

/// Split instruction-heavy blocks before assigning regions. SSA definitions
/// retain their identities and dominance through the new sequential edges.
fn split_blocks(source: &mut SsaBody) {
    let original = source.ir.blocks.len();
    for index in 0..original {
        let old = BlockId::new(index);
        let instructions = std::mem::take(&mut source.ir.blocks[index].instructions);
        let term = source.ir.blocks[index].terminator.take();
        let mut current = old;
        let mut amount = 0;
        let mut starts = vec![(0usize, old)];
        for (position, inst) in instructions.into_iter().enumerate() {
            let work = cost(source.ir.instructions[inst.index()].op, source);
            if amount != 0 && amount + work > MAX_WORK {
                let next = BlockId::new(source.ir.blocks.len());
                source.ir.blocks.push(Block::default());
                let edge = EdgeId::new(source.ir.edges.len());
                source.ir.edges.push(Edge {
                    target: next,
                    args: Vec::new(),
                });
                source.ir.blocks[current.index()].terminator = Some(Terminator::Jump(edge));
                current = next;
                amount = 0;
                starts.push((position, next));
            }
            source.ir.blocks[current.index()].instructions.push(inst);
            amount += work;
        }
        source.ir.blocks[current.index()].terminator = term;
        if let Some(lines) = &mut source.lines {
            let line = lines.terminators.get(index).copied().flatten();
            lines.terminators.resize(source.ir.blocks.len(), None);
            for &(_, block) in &starts {
                lines.terminators[block.index()] = line;
            }
        }
        if let Some(debug) = &mut source.debug {
            for event in &mut debug.events {
                if event.block == old {
                    let &(start, block) = starts
                        .iter()
                        .rev()
                        .find(|&&(start, _)| start <= event.position as usize)
                        .unwrap();
                    event.block = block;
                    event.position -= start as u32;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests;
