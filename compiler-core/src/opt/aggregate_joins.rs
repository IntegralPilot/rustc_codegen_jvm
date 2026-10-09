//! Promote read-only carriers at joins as values, without allocation identity.
//! Join aliases and CFG inputs with union-find. Unknown producers, writes
//! and escaping uses reject the connected group.
use super::aggregates::Candidate;
use crate::ir::*;

struct Groups {
    parents: Vec<usize>,
    ranks: Vec<u8>,
}
impl Groups {
    fn root(&mut self, mut value: usize) -> usize {
        while self.parents[value] != value {
            self.parents[value] = self.parents[self.parents[value]];
            value = self.parents[value];
        }
        value
    }
    fn join(&mut self, a: usize, b: usize) {
        let a = self.root(a);
        let b = self.root(b);
        if a == b {
            return;
        }
        let (a, b) = if self.ranks[a] < self.ranks[b] {
            (b, a)
        } else {
            (a, b)
        };
        self.parents[b] = a;
        if self.ranks[a] == self.ranks[b] {
            self.ranks[a] += 1;
        }
    }
}

pub(super) fn promote(
    body: &mut Body,
    roots: &mut Vec<u32>,
    candidates: &[Candidate],
) -> Vec<bool> {
    let mut removed = vec![false; candidates.len()];
    if !body
        .blocks
        .iter()
        .enumerate()
        .any(|(index, block)| BlockId::new(index) != body.entry && !block.params.is_empty())
    {
        return removed;
    }
    let count = body.values.len();
    let mut groups = Groups {
        parents: (0..count).collect(),
        ranks: vec![0; count],
    };
    let mut users = crate::analysis::ValueUsers::new(count);
    let mut known = vec![false; count];
    for (index, value) in body.values.iter().enumerate() {
        if roots[index] != u32::MAX {
            known[index] = true;
            continue;
        }
        let source = match value.def {
            ValueDef::Alias(source) => Some(source),
            ValueDef::Inst(id) => match body.instructions[id.index()].op {
                Op::Reinterpret(source) | Op::Refine(source) => Some(source),
                _ => None,
            },
            ValueDef::Param(block) if block != body.entry => {
                known[index] = true;
                None
            }
            _ => None,
        };
        if let Some(source) = source {
            groups.join(index, source.index());
            users.connect(source, index);
            known[index] = true;
        }
    }
    for edge in &body.edges {
        for (&source, &target) in edge
            .args
            .iter()
            .zip(&body.blocks[edge.target.index()].params)
        {
            groups.join(source.index(), target.index());
        }
    }
    let group = (0..count).map(|i| groups.root(i)).collect::<Vec<_>>();
    let mut layout = vec![None::<usize>; count];
    let mut valid = vec![true; count];
    for index in 0..count {
        let group = group[index];
        valid[group] &= known[index];
        let root = roots[index];
        if root == u32::MAX {
            continue;
        }
        let candidate = root as usize;
        if candidates[candidate].fields.len() > 8 {
            valid[group] = false;
        }
        if let Some(previous) = layout[group] {
            valid[group] &= candidates[previous].fields == candidates[candidate].fields;
        } else {
            layout[group] = Some(candidate);
        }
    }
    let position = |value: ValueId, field: MemberId| {
        layout[group[value.index()]].and_then(|index| {
            candidates[index]
                .fields
                .iter()
                .position(|f| f == &body.fields[field.index()])
        })
    };
    for inst in &body.instructions {
        inst.op.visit_uses(&body.args, |value| {
            let allowed = match inst.op {
                Op::Reinterpret(_) | Op::Refine(_) => true,
                Op::GetField { object, field } => {
                    object == value && position(value, field).is_some()
                }
                _ => false,
            };
            if !allowed {
                valid[group[value.index()]] = false;
            }
        });
    }
    for block in &body.blocks {
        block
            .terminator
            .unwrap()
            .visit_uses(|v| valid[group[v.index()]] = false);
    }
    let eligible = group
        .iter()
        .map(|&g| valid[g] && layout[g].is_some())
        .collect::<Vec<_>>();
    if !eligible.iter().any(|&yes| yes) {
        return removed;
    }
    let mut components = vec![None; count];
    let mut joins = Vec::new();
    for index in 0..count {
        if !eligible[index] {
            continue;
        }
        let root = roots[index];
        if root != u32::MAX {
            components[index] = Some(candidates[root as usize].initial);
            removed[root as usize] = true;
            continue;
        }
        if let ValueDef::Param(block) = body.values[index].def {
            let position = body.blocks[block.index()]
                .params
                .iter()
                .position(|p| p.index() == index)
                .unwrap();
            let fields = &candidates[layout[group[index]].unwrap()].fields;
            let mut parts = Vec::with_capacity(fields.len());
            for field in fields {
                let part = ValueId::new(body.values.len());
                body.values.push(Value {
                    ty: field.ty,
                    def: ValueDef::Param(block),
                });
                body.blocks[block.index()].params.push(part);
                parts.push(part);
            }
            components[index] = Some(List::append(&mut body.args, parts));
            joins.push((block, position));
        }
    }
    users.propagate(&eligible, &mut components);
    let predecessors = body.predecessors();
    for (block, position) in joins {
        for &(_, edge) in &predecessors[block.index()] {
            let source = body.edges[edge.index()].args[position];
            let parts = components[source.index()].unwrap();
            body.edges[edge.index()]
                .args
                .extend_from_slice(&body.args[parts.range()]);
        }
    }
    for inst in &mut body.instructions {
        if let Some(value) = inst.result {
            let root = roots[value.index()];
            if root != u32::MAX && removed[root as usize] {
                let id = ConstId::new(body.constants.len());
                body.constants
                    .push(Constant::Null(body.values[value.index()].ty));
                inst.op = Op::Constant(id);
                roots[value.index()] = u32::MAX;
                continue;
            }
        }
        if let Op::GetField { object, field } = inst.op
            && eligible[object.index()]
        {
            let fields = &candidates[layout[group[object.index()]].unwrap()].fields;
            let position = fields
                .iter()
                .position(|f| f == &body.fields[field.index()])
                .unwrap();
            let parts = components[object.index()].unwrap();
            inst.op = Op::Reinterpret(body.args[parts.range()][position]);
        }
    }
    for block in &mut body.blocks {
        if let Some(Terminator::Invoke { inst, normal, .. }) = block.terminator
            && matches!(
                body.instructions[inst.index()].op,
                Op::Constant(_) | Op::Reinterpret(_)
            )
        {
            block.instructions.push(inst);
            block.terminator = Some(Terminator::Jump(normal));
        }
    }
    roots.resize(body.values.len(), u32::MAX);
    removed
}
