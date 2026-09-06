//! The dispatcher owns one set of typed arrays for values crossing regions.
use super::*;

#[derive(Clone, Copy)]
pub(super) struct Location {
    pub array: usize,
    pub index: usize,
    pub ty: TypeId,
}
pub(super) struct Frame {
    pub elements: Vec<TypeId>,
    pub arrays: Vec<TypeId>,
    pub lengths: Vec<usize>,
    pub values: Vec<Option<Location>>,
    pub storage: Vec<Location>,
    pub exception: Location,
    pub result: Option<Location>,
}
impl Frame {
    pub fn new(source: &SsaBody, types: &mut Types, groups: &[usize]) -> Self {
        let int = types.scalar(ScalarType::I32);
        let long = types.scalar(ScalarType::I64);
        let float = types.scalar(ScalarType::F32);
        let double = types.scalar(ScalarType::F64);
        let object = types.symbol("java/lang/Object");
        let object = types.intern(Type::Class(object));
        let throwable = types.symbol("java/lang/Throwable");
        let throwable = types.intern(Type::Class(throwable));
        let elements = vec![int, long, float, double, object];
        let arrays = elements
            .iter()
            .map(|&t| types.intern(Type::Array(t)))
            .collect();
        let mut frame = Self {
            elements,
            arrays,
            lengths: vec![0; 5],
            values: vec![None; source.ir.values.len()],
            storage: Vec::new(),
            exception: Location {
                array: 4,
                index: 0,
                ty: throwable,
            },
            result: None,
        };
        frame.exception = frame.allocate(throwable, types);
        if source.ir.return_type != types.intern(Type::Unit) {
            frame.result = Some(frame.allocate(source.ir.return_type, types));
        }
        for slot in &source.ir.slots {
            let pointer = types.intern(Type::Pointer(slot.ty));
            let location = frame.allocate(pointer, types);
            frame.storage.push(location);
        }
        let ir = &source.ir;
        let mut owners = vec![None; ir.values.len()];
        for (b, block) in ir.blocks.iter().enumerate() {
            for &param in &block.params {
                owners[param.index()] = Some(BlockId::new(b));
            }
            for &inst in &block.instructions {
                if let Some(value) = ir.instructions[inst.index()].result {
                    owners[value.index()] = Some(BlockId::new(b));
                }
            }
            if let Some(Terminator::Invoke { inst, .. }) = block.terminator
                && let Some(value) = ir.instructions[inst.index()].result
            {
                owners[value.index()] = Some(BlockId::new(b));
            }
        }
        let mut crossed = vec![false; ir.values.len()];
        let predecessors = ir.predecessors();
        let mut visited = vec![None; ir.blocks.len()];
        let mut pending = Vec::new();
        let mut used_at = |value: ValueId, at: BlockId, crossed: &mut [bool]| {
            let value = ir.resolve(value);
            let Some(definition) = owners[value.index()] else {
                return;
            };
            if crossed[value.index()] {
                return;
            }
            pending.push(at);
            while let Some(block) = pending.pop() {
                if block == definition || visited[block.index()] == Some(value) {
                    continue;
                }
                visited[block.index()] = Some(value);
                if groups[block.index()] != groups[definition.index()] {
                    crossed[value.index()] = true;
                    pending.clear();
                    break;
                }
                pending.extend(
                    predecessors[block.index()]
                        .iter()
                        .map(|&(source, _)| source),
                );
            }
        };
        for &param in &ir.blocks[ir.entry.index()].params {
            crossed[param.index()] = true;
        }
        for (b, block) in ir.blocks.iter().enumerate() {
            if groups[b] == usize::MAX {
                continue;
            }
            let mut used = |value: ValueId| used_at(value, BlockId::new(b), &mut crossed);
            for &inst in &block.instructions {
                ir.instructions[inst.index()]
                    .op
                    .visit_uses(&ir.args, &mut used);
            }
            let term = block.terminator.unwrap();
            term.visit_uses(&mut used);
            if let Terminator::Invoke { inst, .. } = term {
                ir.instructions[inst.index()]
                    .op
                    .visit_uses(&ir.args, &mut used);
            }
            term.visit_edges(&ir.cases, |edge| {
                let edge = &ir.edges[edge.index()];
                for &arg in &edge.args {
                    used(arg);
                }
            });
            term.visit_edges(&ir.cases, |edge| {
                let edge = &ir.edges[edge.index()];
                if groups[b] != groups[edge.target.index()] {
                    for &param in &ir.blocks[edge.target.index()].params {
                        crossed[param.index()] = true;
                    }
                }
            });
        }
        if let Some(debug) = &source.debug {
            for event in &debug.events {
                if let DebugChange::Set { value, .. } = event.change {
                    used_at(value, event.block, &mut crossed);
                }
            }
        }
        for (index, crossed) in crossed.into_iter().enumerate() {
            if crossed {
                frame.values[index] = Some(frame.allocate(ir.values[index].ty, types));
            }
        }
        frame
    }
    fn allocate(&mut self, ty: TypeId, types: &Types) -> Location {
        let array = types.get(ty).unwrap().carrier() as usize - 1;
        let location = Location {
            array,
            index: self.lengths[array],
            ty,
        };
        self.lengths[array] += 1;
        location
    }
    pub fn load(&self, b: &mut Builder<'_>, arrays: &[ValueId], location: Location) -> ValueId {
        let index = integer(b, self.elements[0], location.index as i32);
        let value = b
            .emit(
                Op::ArrayGet {
                    array: arrays[location.array],
                    index,
                },
                Some(self.elements[location.array]),
            )
            .unwrap();
        if self.elements[location.array] == location.ty {
            value
        } else {
            b.emit(
                if location.array == 4 {
                    Op::Adapt(value)
                } else {
                    Op::Reinterpret(value)
                },
                Some(location.ty),
            )
            .unwrap()
        }
    }
    pub fn store(
        &self,
        b: &mut Builder<'_>,
        arrays: &[ValueId],
        location: Location,
        value: ValueId,
    ) {
        let element = self.elements[location.array];
        let value = if b.body.value_type(value) == element {
            value
        } else {
            b.emit(Op::Reinterpret(value), Some(element)).unwrap()
        };
        let index = integer(b, self.elements[0], location.index as i32);
        b.emit(
            Op::ArraySet {
                array: arrays[location.array],
                index,
                value,
            },
            None,
        );
    }
}
