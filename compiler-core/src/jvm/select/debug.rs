//! Debug-only mirrors keep source bindings stable while computational SSA slots
//! are reused. Definite initialization and lexical scope control visible ranges.
use super::*;
use jvm::attributes::{Attribute, LocalVariableTable};

pub(super) struct Debug<'a> {
    info: &'a DebugInfo,
    events: Vec<Vec<DebugEvent>>,
    entries: Vec<Vec<bool>>,
    slots: Vec<Option<u16>>,
    initialized: Vec<bool>,
    scope: Option<u32>,
    ranges: Vec<Vec<(usize, usize)>>,
}

impl<'a> Debug<'a> {
    pub fn new(info: &'a DebugInfo, body: &Body) -> Self {
        let mut events = vec![Vec::new(); body.blocks.len()];
        for &event in &info.events {
            events[event.block.index()].push(event);
        }
        for events in &mut events {
            events.sort_by_key(|e| e.position);
        }
        let initial = info
            .locals
            .iter()
            .map(|l| matches!(l, DebugLocal::Storage(_)))
            .collect::<Vec<_>>();
        let reachable = body.reachable();
        let predecessors = body.predecessors();
        let mut entries = vec![vec![true; info.locals.len()]; body.blocks.len()];
        let mut exits = entries.clone();
        loop {
            let mut changed = false;
            for index in 0..body.blocks.len() {
                if !reachable[index] {
                    continue;
                }
                let mut state = if index == body.entry.index() {
                    initial.clone()
                } else {
                    let mut state = vec![true; info.locals.len()];
                    for &(source, _) in &predecessors[index] {
                        if reachable[source.index()] {
                            for (to, &from) in state.iter_mut().zip(&exits[source.index()]) {
                                *to &= from;
                            }
                        }
                    }
                    state
                };
                entries[index].clone_from(&state);
                for event in &events[index] {
                    update(&mut state, event.change);
                }
                if state != exits[index] {
                    exits[index] = state;
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
        Self {
            info,
            events,
            entries,
            slots: Vec::new(),
            initialized: initial,
            scope: None,
            ranges: vec![Vec::new(); info.variables.len()],
        }
    }

    pub fn allocate(&mut self, s: &mut Selector<'_>) -> jvm::Result<()> {
        for local in &self.info.locals {
            let slot = match *local {
                DebugLocal::Storage(slot) => Some(s.storage[slot.index()]),
                DebugLocal::Value(ty) if s.types.get(ty) == Some(Type::Unit) => None,
                DebugLocal::Value(ty) => {
                    let slot = s.next_slot;
                    s.next_slot = slot
                        .checked_add(representation::value_kind(s.types, ty)?.width())
                        .ok_or_else(|| error("JVM debug local limit"))?;
                    Some(slot)
                }
            };
            self.slots.push(slot);
        }
        Ok(())
    }

    pub fn start(&mut self, block: BlockId) {
        self.initialized.clone_from(&self.entries[block.index()]);
        self.scope = None;
    }

    pub fn events(&mut self, block: BlockId) -> Vec<DebugEvent> {
        std::mem::take(&mut self.events[block.index()])
    }

    pub fn apply(&mut self, s: &mut Selector<'_>, change: DebugChange) -> jvm::Result<()> {
        match change {
            DebugChange::Set { local, value } => {
                let Some(slot) = self.slots[local as usize] else {
                    return Ok(());
                };
                let start = s.assembly.code.len();
                s.argument(value)?;
                s.assembly.code.push(s.value_kind(value)?.store(slot));
                // The old binding remains visible until its store completes.
                self.mark(start, s.assembly.code.len());
            }
            DebugChange::Scope(scope) => self.scope = Some(scope),
            DebugChange::Clear(_) => {}
        }
        update(&mut self.initialized, change);
        Ok(())
    }

    pub fn mark(&mut self, start: usize, end: usize) {
        let Some(scope) = self.scope else {
            return;
        };
        if start == end {
            return;
        }
        for &variable in &self.info.scopes[scope as usize] {
            let variable = variable as usize;
            if !self.initialized[self.info.variables[variable].local as usize] {
                continue;
            }
            let ranges = &mut self.ranges[variable];
            if let Some((_, last)) = ranges.last_mut().filter(|(_, last)| *last == start) {
                *last = end;
            } else {
                ranges.push((start, end));
            }
        }
    }

    pub fn finish(
        self,
        types: &Types,
        cp: &mut InternedConstantPool,
        code: &[Instruction],
    ) -> jvm::Result<Option<Attribute>> {
        let offsets = super::super::encoding::instruction_byte_offsets(code)?;
        let mut variables = Vec::new();
        for (variable, ranges) in self.info.variables.iter().zip(self.ranges) {
            if ranges.is_empty() {
                continue;
            }
            let local = variable.local as usize;
            let Some(slot) = self.slots[local] else {
                continue;
            };
            let mut descriptor = String::new();
            match self.info.locals[local] {
                DebugLocal::Value(ty) => representation::descriptor(types, ty, &mut descriptor)?,
                DebugLocal::Storage(_) => descriptor = format!("L{POINTER_CLASS};"),
            }
            let name_index = cp.add_utf8(&variable.name)?;
            let descriptor_index = cp.add_utf8(descriptor)?;
            for (start, end) in ranges {
                variables.push(LocalVariableTable {
                    start_pc: u16::try_from(offsets[start])?,
                    length: u16::try_from(offsets[end] - offsets[start])?,
                    name_index,
                    descriptor_index,
                    index: slot,
                });
            }
        }
        variables.sort_by_key(|v| (v.index, v.start_pc));
        Ok(if variables.is_empty() {
            None
        } else {
            Some(Attribute::LocalVariableTable {
                name_index: cp.add_utf8("LocalVariableTable")?,
                variables,
            })
        })
    }
}

fn update(initialized: &mut [bool], change: DebugChange) {
    match change {
        DebugChange::Set { local, .. } => initialized[local as usize] = true,
        DebugChange::Clear(local) => initialized[local as usize] = false,
        DebugChange::Scope(_) => {}
    }
}
