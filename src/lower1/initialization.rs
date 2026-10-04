//! Definite initialization tracks only the locals used by lowering decisions.
use rustc_hash::FxHashSet as HashSet;
use rustc_middle::mir::{
    BasicBlock, Body, Local, Place, ProjectionElem, StatementKind, TerminatorKind,
};
use std::collections::VecDeque;

struct Domain {
    slots: Vec<u32>,
    locals: Vec<Local>,
}

impl Domain {
    fn new(local_count: usize, locals: impl IntoIterator<Item = Local>) -> Self {
        let mut result = Self {
            slots: Vec::new(),
            locals: Vec::new(),
        };
        for local in locals {
            if result.slots.is_empty() {
                result.slots.resize(local_count, u32::MAX);
            }
            if result.slots[local.index()] == u32::MAX {
                result.slots[local.index()] = result.locals.len() as u32;
                result.locals.push(local);
            }
        }
        result
    }

    fn slot(&self, local: Local) -> Option<usize> {
        self.slots
            .get(local.index())
            .copied()
            .filter(|&slot| slot != u32::MAX)
            .map(|slot| slot as usize)
    }

    fn empty(&self) -> Bits {
        Bits(vec![0; self.locals.len().div_ceil(64)])
    }
    fn all(&self) -> Bits {
        let mut bits = self.empty();
        bits.0.fill(u64::MAX);
        let tail = self.locals.len() % 64;
        if tail != 0
            && let Some(last) = bits.0.last_mut()
        {
            *last = (1 << tail) - 1;
        }
        bits
    }
}

#[derive(Clone, PartialEq, Eq)]
struct Bits(Vec<u64>);

impl Bits {
    fn insert(&mut self, slot: usize) {
        self.0[slot / 64] |= 1 << (slot % 64);
    }
    fn remove(&mut self, slot: usize) {
        self.0[slot / 64] &= !(1 << (slot % 64));
    }
    fn contains(&self, slot: usize) -> bool {
        self.0[slot / 64] & (1 << (slot % 64)) != 0
    }

    fn transfer(&mut self, input: &Self, killed: Option<&Self>, generated: &Self, intersect: bool) {
        for (index, dest) in self.0.iter_mut().enumerate() {
            let value = (input.0[index] & !killed.map_or(0, |k| k.0[index])) | generated.0[index];
            *dest = if intersect { *dest & value } else { value };
        }
    }

    fn locals<'a>(&'a self, domain: &'a Domain) -> impl Iterator<Item = Local> + 'a {
        self.0.iter().enumerate().flat_map(move |(index, &word)| {
            let mut word = word;
            std::iter::from_fn(move || {
                if word == 0 {
                    return None;
                }
                let bit = word.trailing_zeros() as usize;
                word &= word - 1;
                Some(domain.locals[index * 64 + bit])
            })
        })
    }
}

pub(super) struct MirControlFlow {
    predecessors: Vec<Vec<BasicBlock>>,
    reachable: Vec<bool>,
}

impl MirControlFlow {
    pub(super) fn new(mir: &Body<'_>) -> Self {
        let mut predecessors = vec![Vec::new(); mir.basic_blocks.len()];
        for (block, data) in mir.basic_blocks.iter_enumerated() {
            for successor in data.terminator().successors() {
                predecessors[successor.index()].push(block);
            }
        }
        let mut reachable = vec![false; mir.basic_blocks.len()];
        let mut queue = VecDeque::from([BasicBlock::from_usize(0)]);
        while let Some(block) = queue.pop_front() {
            if std::mem::replace(&mut reachable[block.index()], true) {
                continue;
            }
            queue.extend(mir.basic_blocks[block].terminator().successors());
        }
        Self {
            predecessors,
            reachable,
        }
    }

    fn initialized(
        &self,
        domain: &Domain,
        generated: &[Bits],
        killed: Option<&[Bits]>,
        entry: Bits,
    ) -> Vec<Bits> {
        let all = domain.all();
        let mut available = self
            .reachable
            .iter()
            .enumerate()
            .map(|(block, &reachable)| {
                if block == 0 {
                    entry.clone()
                } else if reachable {
                    all.clone()
                } else {
                    domain.empty()
                }
            })
            .collect::<Vec<_>>();
        let mut incoming = domain.empty();
        loop {
            let mut changed = false;
            for block in 1..self.reachable.len() {
                if !self.reachable[block] {
                    continue;
                }
                let mut first = true;
                for predecessor in &self.predecessors[block] {
                    let index = predecessor.index();
                    if !self.reachable[index] {
                        continue;
                    }
                    incoming.transfer(
                        &available[index],
                        killed.map(|k| &k[index]),
                        &generated[index],
                        !first,
                    );
                    first = false;
                }
                if first {
                    incoming.0.fill(0);
                }
                if available[block] != incoming {
                    available[block].clone_from(&incoming);
                    changed = true;
                }
            }
            if !changed {
                return available;
            }
        }
    }
}

fn field_assignment(place: Place<'_>) -> bool {
    matches!(place.projection.first(), Some(ProjectionElem::Field(..)))
}

#[derive(Clone, Copy)]
enum Write {
    Whole,
    Field,
    Reset,
}

/// Visit writes and storage-lifetime resets in the order they take effect.
fn writes(mir: &Body<'_>, block: BasicBlock, mut visit: impl FnMut(Local, Write)) {
    fn assignment(place: Place<'_>, visit: &mut impl FnMut(Local, Write)) {
        if field_assignment(place) {
            visit(place.local, Write::Field);
        } else if place.projection.is_empty() {
            visit(place.local, Write::Whole);
        }
    }
    let data = &mir.basic_blocks[block];
    for statement in &data.statements {
        match &statement.kind {
            StatementKind::Assign(value) => assignment(value.0, &mut visit),
            StatementKind::StorageLive(local) | StatementKind::StorageDead(local) => {
                visit(*local, Write::Reset)
            }
            _ => {}
        }
    }
    if let TerminatorKind::Call { destination, .. } = &data.terminator().kind {
        assignment(*destination, &mut visit);
    }
}

pub(super) fn class_locals_needing_initial_carriers(
    mir: &Body<'_>,
    flow: &MirControlFlow,
) -> Vec<Local> {
    let mut candidates = Vec::new();
    for (block, _) in mir.basic_blocks.iter_enumerated() {
        writes(mir, block, |local, write| {
            if matches!(write, Write::Field) {
                candidates.push(local);
            }
        });
    }
    let domain = Domain::new(mir.local_decls.len(), candidates);
    if domain.locals.is_empty() {
        return Vec::new();
    }
    let mut generated = vec![domain.empty(); mir.basic_blocks.len()];
    let mut killed = generated.clone();
    for (block, _) in mir.basic_blocks.iter_enumerated() {
        writes(mir, block, |local, write| {
            if let Some(slot) = domain.slot(local) {
                if matches!(write, Write::Reset) {
                    generated[block.index()].remove(slot);
                    killed[block.index()].insert(slot);
                } else {
                    generated[block.index()].insert(slot);
                    killed[block.index()].remove(slot);
                }
            }
        });
    }
    let mut entry = domain.empty();
    for (slot, local) in domain.locals.iter().enumerate() {
        if local.index() > 0 && local.index() <= mir.arg_count {
            entry.insert(slot);
        }
    }
    let available = flow.initialized(&domain, &generated, Some(&killed), entry);
    let mut needed = domain.empty();
    for (block, _) in mir.basic_blocks.iter_enumerated() {
        if !flow.reachable[block.index()] {
            continue;
        }
        let mut state = available[block.index()].clone();
        writes(mir, block, |local, write| {
            if let Some(slot) = domain.slot(local) {
                if matches!(write, Write::Reset) {
                    state.remove(slot);
                } else {
                    if matches!(write, Write::Field) && !state.contains(slot) {
                        needed.insert(slot);
                    }
                    state.insert(slot);
                }
            }
        });
    }
    let mut result = needed.locals(&domain).collect::<Vec<_>>();
    result.sort_unstable();
    result
}

#[cfg(test)]
mod tests;
