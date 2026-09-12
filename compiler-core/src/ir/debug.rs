//! Optional source bindings. These are absent from release bodies and never
//! participate in computational identity or change SSA definitions.
use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DebugLocal {
    Value(TypeId),
    /// The debugger observes the stable runtime cell, including alias writes.
    Storage(SlotId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DebugVariable {
    pub name: String,
    pub local: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DebugChange {
    Set { local: u32, value: ValueId },
    Clear(u32),
    Scope(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DebugEvent {
    pub block: BlockId,
    /// Before this instruction in the block; len denotes its terminator.
    pub position: u32,
    pub change: DebugChange,
    pub line: Option<u16>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct DebugInfo {
    pub locals: Vec<DebugLocal>,
    pub variables: Vec<DebugVariable>,
    pub scopes: Vec<Vec<u32>>,
    pub events: Vec<DebugEvent>,
}

impl DebugInfo {
    pub fn push(&mut self, builder: &Builder<'_>, change: DebugChange) -> &mut DebugEvent {
        let block = builder.current();
        self.events.push(DebugEvent {
            block,
            position: u32::try_from(builder.body.blocks[block.index()].instructions.len())
                .expect("debug position limit"),
            change,
            line: None,
        });
        self.events.last_mut().unwrap()
    }

    pub fn roots<'a>(&'a self, body: &'a Body) -> impl Iterator<Item = ValueId> + 'a {
        let reachable = body.reachable();
        self.events
            .iter()
            .filter_map(move |event| match event.change {
                DebugChange::Set { value, .. } if reachable[event.block.index()] => Some(value),
                _ => None,
            })
    }
}
