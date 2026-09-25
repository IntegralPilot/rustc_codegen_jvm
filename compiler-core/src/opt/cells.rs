//! Escape analysis and SSA promotion for private, explicitly initialized cells.
use crate::ir::*;

const NONE: u32 = u32::MAX;

fn cell_index(index: u32) -> Option<usize> {
    (index != NONE).then_some(index as usize)
}

fn origins(body: &Body, roots: &[u32]) -> Vec<u32> {
    let mut origins = roots.to_vec();
    let mut known = roots.iter().map(|&index| index != NONE).collect::<Vec<_>>();
    let mut path = Vec::new();
    for index in 0..body.values.len() {
        if known[index] {
            continue;
        }
        let mut value = ValueId::new(index);
        let root = loop {
            path.push(value);
            value = body.resolve(value);
            if known[value.index()] {
                break origins[value.index()];
            }
            // Memoize negative results too. Marking before following a use
            // bounds the walk even for malformed reinterpretation cycles.
            known[value.index()] = true;
            path.push(value);
            let ValueDef::Inst(inst) = body.values[value.index()].def else {
                break NONE;
            };
            let Op::Reinterpret(input) = body.instructions[inst.index()].op else {
                break NONE;
            };
            value = input;
        };
        for value in path.drain(..) {
            known[value.index()] = true;
            origins[value.index()] = root;
        }
    }
    origins
}

/// Each candidate is a fresh allocation and its typed initial contents. The
/// producer guarantees that creating it has no effects beyond allocation.
/// Keep cells whose address escapes, is retyped, or participates in a join of
/// distinct addresses. Contents of private cells may cross arbitrary CFG joins.
pub fn promote_cells(
    body: Body,
    types: &Types,
    cells: &[(ValueId, ValueId)],
) -> Result<Body, VerifyError> {
    let mut roots = vec![NONE; body.values.len()];
    for (index, &(cell, initial)) in cells.iter().enumerate() {
        assert_eq!(
            types.get(body.value_type(cell)),
            Some(Type::Pointer(body.value_type(initial)))
        );
        roots[cell.index()] = u32::try_from(index).expect("too many private cells");
    }
    let origins = origins(&body, &roots);
    let mut escaped = vec![false; cells.len()];
    for inst in &body.instructions {
        inst.op.visit_uses(&body.args, |value| {
            let Some(index) = cell_index(origins[value.index()]) else {
                return;
            };
            let pointee = body.value_type(cells[index].1);
            let allowed = match inst.op {
                Op::Reinterpret(_) => true,
                Op::Load(pointer) => {
                    pointer == value && inst.result.is_some_and(|v| body.value_type(v) == pointee)
                }
                Op::Store {
                    pointer,
                    value: stored,
                } => pointer == value && stored != value && body.value_type(stored) == pointee,
                _ => false,
            };
            escaped[index] |= !allowed;
        });
    }
    // Only trivial address joins have been resolved. A nontrivial address phi
    // may select another allocation; retain its storage rather than guessing.
    let mut escape = |value: ValueId| {
        if let Some(index) = cell_index(origins[value.index()]) {
            escaped[index] = true;
        }
    };
    for edge in &body.edges {
        for &value in &edge.args {
            escape(value);
        }
    }
    for block in &body.blocks {
        block.terminator.unwrap().visit_uses(&mut escape);
    }
    if escaped.iter().all(|&e| e) {
        return Ok(body);
    }

    let mut builder = Builder::from_body(body, types);
    let variables = cells
        .iter()
        .enumerate()
        .map(|(index, &(_, initial))| {
            (!escaped[index]).then(|| builder.variable(builder.body.value_type(initial)))
        })
        .collect::<Vec<_>>();
    for block_index in 0..builder.body.blocks.len() {
        let block = BlockId::new(block_index);
        builder.switch_to(block);
        let instructions = builder.body.blocks[block_index].instructions.clone();
        let invoke = match builder.body.blocks[block_index].terminator.unwrap() {
            Terminator::Invoke { inst, .. } => Some(inst),
            _ => None,
        };
        for id in instructions.into_iter().chain(invoke) {
            let inst = builder.body.instructions[id.index()];
            if let Some(index) = inst
                .result
                .and_then(|v| roots.get(v.index()).copied().and_then(cell_index))
                && let Some(variable) = variables[index]
            {
                builder.define(variable, cells[index].1);
                // Alias-only remnants are dead after their loads and stores
                // disappear. A typed null preserves table and debug positions.
                let constant = ConstId::new(builder.body.constants.len());
                builder
                    .body
                    .constants
                    .push(Constant::Null(builder.body.value_type(cells[index].0)));
                builder.body.instructions[id.index()].op = Op::Constant(constant);
                continue;
            }
            let pointer = match inst.op {
                Op::Load(pointer) | Op::Store { pointer, .. } => pointer,
                _ => continue,
            };
            let Some(variable) = cell_index(origins[pointer.index()]).and_then(|i| variables[i])
            else {
                continue;
            };
            builder.body.instructions[id.index()].op = match inst.op {
                Op::Load(_) => Op::Reinterpret(builder.read(variable)),
                Op::Store { value, .. } => {
                    builder.define(variable, value);
                    Op::Nop
                }
                _ => unreachable!(),
            };
        }
        if let Some(Terminator::Invoke { inst, normal, .. }) =
            builder.body.blocks[block_index].terminator
            && !builder.body.instructions[inst.index()]
                .op
                .may_throw(&builder.body, types)
        {
            builder.body.blocks[block_index].instructions.push(inst);
            builder.body.blocks[block_index].terminator = Some(Terminator::Jump(normal));
        }
    }
    builder.finish()
}
