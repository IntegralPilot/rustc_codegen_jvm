//! Relocate one region into a compact body and import only the tables it uses.
use super::*;

struct Extract<'a> {
    source: &'a SsaBody,
    b: Builder<'a>,
    frame: &'a Frame,
    arrays: Vec<ValueId>,
    variables: Vec<Option<VariableId>>,
    blocks: HashMap<BlockId, BlockId>,
    edges: Vec<(EdgeId, BlockId, bool)>,
    constants: Vec<super::super::Constant>,
    constant_ids: HashMap<ConstId, ConstId>,
    methods: HashMap<MethodId, MethodId>,
    fields: HashMap<MemberId, MemberId>,
    lines: SourceLines,
    debug: DebugInfo,
}
impl Extract<'_> {
    fn variable(&mut self, value: ValueId) -> VariableId {
        *self.variables[value.index()]
            .get_or_insert_with(|| self.b.variable(self.source.ir.value_type(value)))
    }
    fn define(&mut self, old: ValueId, value: ValueId) {
        let variable = self.variable(old);
        self.b.define(variable, value);
        if let Some(location) = self.frame.values[old.index()] {
            self.frame.store(&mut self.b, &self.arrays, location, value);
        }
    }
    fn route(&mut self, edge: EdgeId, exceptional: bool) -> BlockId {
        let target = self.b.create_block();
        self.edges.push((edge, target, exceptional));
        target
    }
    fn transfer(&mut self, edge: EdgeId) {
        let edge = &self.source.ir.edges[edge.index()];
        let values: Vec<_> = edge.args.iter().map(|&value| self.value(value)).collect();
        if let Some(&target) = self.blocks.get(&edge.target) {
            self.b.jump(target, values);
        } else {
            for (&param, value) in self.source.ir.blocks[edge.target.index()]
                .params
                .iter()
                .zip(values)
            {
                self.frame.store(
                    &mut self.b,
                    &self.arrays,
                    self.frame.values[param.index()].unwrap(),
                    value,
                );
            }
            let next = integer(
                &mut self.b,
                self.frame.elements[0],
                edge.target.index() as i32,
            );
            self.b.terminate(Terminator::Return(Some(next)));
        }
    }
    fn instruction(&mut self, old: InstId, unwind: Option<BlockId>) {
        let inst = self.source.ir.instructions[old.index()];
        let ty = inst.result.map(|value| self.source.ir.value_type(value));
        let op = match inst.op {
            Op::Exception => {
                let value = self
                    .frame
                    .load(&mut self.b, &self.arrays, self.frame.exception);
                if let Some(old) = inst.result {
                    self.define(old, value);
                }
                return;
            }
            Op::LoadSlot(slot) | Op::AddressOfSlot(slot) => {
                let pointer =
                    self.frame
                        .load(&mut self.b, &self.arrays, self.frame.storage[slot.index()]);
                if matches!(inst.op, Op::AddressOfSlot(_)) {
                    self.define(inst.result.unwrap(), pointer);
                    return;
                }
                Op::Load(pointer)
            }
            Op::StoreSlot { slot, value } => {
                let pointer =
                    self.frame
                        .load(&mut self.b, &self.arrays, self.frame.storage[slot.index()]);
                let value = self.value(value);
                if self.source.ir.slots[slot.index()].size == 0 {
                    let object = self.frame.elements[4];
                    let value = if self.b.body.value_type(value) == object {
                        value
                    } else {
                        self.b.emit(Op::Reinterpret(value), Some(object)).unwrap()
                    };
                    let method = self.b.method(MethodRef {
                        owner: jvm_compiler_core::jvm::select::POINTER_CLASS.into(),
                        name: "initializeZeroSizedLocal".into(),
                        params: vec![object],
                        returns: self.source.types.find(Type::Unit).unwrap(),
                        interface: false,
                    });
                    let args = self.b.args([pointer, value]);
                    Op::Call {
                        method,
                        kind: CallKind::Virtual,
                        args,
                    }
                } else {
                    Op::Store { pointer, value }
                }
            }
            op => op.remap(self),
        };
        let value = if let Some(unwind) = unwind {
            self.b.invoke(op, ty, unwind)
        } else {
            self.b.emit(op, ty)
        };
        if let (Some(old), Some(value)) = (inst.result, value) {
            self.define(old, value);
        }
    }
    fn line(&mut self, line: Option<u16>) {
        if self.source.lines.is_none() {
            return;
        }
        self.lines
            .instructions
            .resize(self.b.body.instructions.len(), line);
        self.lines
            .terminators
            .resize(self.b.body.blocks.len(), line);
        self.lines.terminators[self.b.current().index()] = line;
    }
    fn terminator(&mut self, term: Terminator) {
        match term {
            Terminator::Jump(edge) => self.transfer(edge),
            Terminator::Branch { condition, yes, no } => {
                let condition = self.value(condition);
                let yes = self.route(yes, false);
                let no = self.route(no, false);
                self.b.branch(condition, yes, no);
            }
            Terminator::Switch {
                value,
                cases,
                otherwise,
            } => {
                let value = self.value(value);
                let targets: Vec<_> = self.source.ir.cases[cases.range()]
                    .iter()
                    .map(|&(key, edge)| (key, self.route(edge, false)))
                    .collect();
                let otherwise = self.route(otherwise, false);
                self.b.switch(value, targets, otherwise);
            }
            Terminator::Return(value) => {
                if let (Some(value), Some(location)) = (value, self.frame.result) {
                    let value = self.value(value);
                    self.frame.store(&mut self.b, &self.arrays, location, value);
                }
                let finished = integer(&mut self.b, self.frame.elements[0], -1);
                self.b.terminate(Terminator::Return(Some(finished)));
            }
            Terminator::Throw { value, unwind } => {
                let value = self.value(value);
                let unwind = unwind.map(|edge| {
                    let block = self.route(edge, true);
                    self.b.edge(block, Vec::new())
                });
                self.b.terminate(Terminator::Throw { value, unwind });
            }
            Terminator::Rethrow => {
                let value = self
                    .frame
                    .load(&mut self.b, &self.arrays, self.frame.exception);
                self.b.terminate(Terminator::Throw {
                    value,
                    unwind: None,
                });
            }
            Terminator::Unreachable => self.b.terminate(Terminator::Unreachable),
            Terminator::Invoke {
                inst,
                normal,
                unwind,
            } => {
                let unwind = self.route(unwind, true);
                self.instruction(inst, Some(unwind));
                self.transfer(normal);
            }
        }
    }
}
impl Remap for Extract<'_> {
    fn value(&mut self, value: ValueId) -> ValueId {
        let value = self.source.ir.resolve(value);
        let variable = self.variable(value);
        self.b.read(variable)
    }
    fn args(&mut self, args: List) -> List {
        let values: Vec<_> = self.source.ir.args[args.range()]
            .iter()
            .map(|&value| self.value(value))
            .collect();
        self.b.args(values)
    }
    fn constant(&mut self, id: ConstId) -> ConstId {
        if let Some(&id) = self.constant_ids.get(&id) {
            return id;
        }
        let mut constant = self.source.ir.constants[id.index()];
        if let Constant::External { index, ty } = constant {
            let next = self.constants.len() as u32;
            self.constants
                .push(self.source.constants[index as usize].clone());
            constant = Constant::External { index: next, ty };
        }
        let next = ConstId::new(self.b.body.constants.len());
        self.b.body.constants.push(constant);
        self.constant_ids.insert(id, next);
        next
    }
    fn method(&mut self, id: MethodId) -> MethodId {
        *self
            .methods
            .entry(id)
            .or_insert_with(|| self.b.method(self.source.ir.methods[id.index()].clone()))
    }
    fn field(&mut self, id: MemberId) -> MemberId {
        *self
            .fields
            .entry(id)
            .or_insert_with(|| self.b.field(self.source.ir.fields[id.index()].clone()))
    }
    fn projection(&mut self, id: ProjectionId) -> ProjectionId {
        let mut projection = self.source.ir.projections[id.index()].clone();
        projection.field = self.field(projection.field);
        self.b.projection(projection)
    }
    fn slot(&mut self, _: SlotId) -> SlotId {
        unreachable!("region storage is imported through frame pointers")
    }
}

pub(super) fn region(
    source: &SsaBody,
    types: Arc<Types>,
    frame: &Frame,
    blocks: &[BlockId],
    entries: &[BlockId],
) -> Result<SsaBody, String> {
    let mut b = Builder::new(&types, frame.elements[0]);
    let entry = b.current();
    let selector = b.parameter(entry, frame.elements[0]);
    let arrays = frame
        .arrays
        .iter()
        .map(|&ty| b.parameter(entry, ty))
        .collect();
    let block_map = blocks
        .iter()
        .map(|&block| (block, b.create_block()))
        .collect();
    let mut x = Extract {
        source,
        b,
        frame,
        arrays,
        variables: vec![None; source.ir.values.len()],
        blocks: block_map,
        edges: Vec::new(),
        constants: Vec::new(),
        constant_ids: HashMap::default(),
        methods: HashMap::default(),
        fields: HashMap::default(),
        lines: SourceLines::default(),
        debug: source.debug.clone().unwrap_or_default(),
    };
    x.debug.events.clear();
    // Debug storage follows its stable pointer imported from the dispatcher's frame.
    for local in 0..x.debug.locals.len() {
        if let DebugLocal::Storage(slot) = x.debug.locals[local] {
            let location = frame.storage[slot.index()];
            x.debug.locals[local] = DebugLocal::Value(location.ty);
            let value = frame.load(&mut x.b, &x.arrays, location);
            x.debug.push(
                &x.b,
                DebugChange::Set {
                    local: local as u32,
                    value,
                },
            );
        }
    }
    let mut defined = vec![false; source.ir.values.len()];
    let mut used = vec![false; source.ir.values.len()];
    for &block in blocks {
        let data = &source.ir.blocks[block.index()];
        for &param in &data.params {
            defined[param.index()] = true;
        }
        let mut visit = |inst: InstId| {
            let inst = source.ir.instructions[inst.index()];
            if let Some(value) = inst.result {
                defined[value.index()] = true;
            }
            inst.op.visit_uses(&source.ir.args, |value| {
                used[source.ir.resolve(value).index()] = true
            });
        };
        for &inst in &data.instructions {
            visit(inst);
        }
        let term = data.terminator.unwrap();
        if let Terminator::Invoke { inst, .. } = term {
            visit(inst);
        }
        term.visit_uses(|value| used[source.ir.resolve(value).index()] = true);
        term.visit_edges(&source.ir.cases, |edge| {
            for &arg in &source.ir.edges[edge.index()].args {
                used[source.ir.resolve(arg).index()] = true;
            }
        });
    }
    if let Some(debug) = &source.debug {
        for event in &debug.events {
            if x.blocks.contains_key(&event.block)
                && let DebugChange::Set { value, .. } = event.change
            {
                used[source.ir.resolve(value).index()] = true;
            }
        }
    }
    for (index, used) in used.into_iter().enumerate() {
        if used && (frame.values[index].is_some() || !defined[index]) {
            let location = frame.values[index]
                .ok_or_else(|| format!("unframed external SSA value {index}"))?;
            let value = frame.load(&mut x.b, &x.arrays, location);
            let variable = x.variable(ValueId::new(index));
            x.b.define(variable, value);
        }
    }
    let invalid = x.b.create_block();
    let dispatch: Vec<_> = entries
        .iter()
        .map(|&old| (old, x.b.create_block()))
        .collect();
    x.b.switch(
        selector,
        dispatch.iter().map(|&(old, block)| {
            (
                Scalar::integer(ScalarType::I32, old.index() as u128).unwrap(),
                block,
            )
        }),
        invalid,
    );
    x.b.switch_to(invalid);
    x.b.terminate(Terminator::Unreachable);
    for (old, dispatch) in dispatch {
        x.b.switch_to(dispatch);
        let args = source.ir.blocks[old.index()]
            .params
            .iter()
            .map(|&param| frame.load(&mut x.b, &x.arrays, frame.values[param.index()].unwrap()))
            .collect();
        x.b.jump(x.blocks[&old], args);
    }
    let mut events: HashMap<BlockId, Vec<DebugEvent>> = HashMap::default();
    if let Some(debug) = &source.debug {
        for &event in &debug.events {
            events.entry(event.block).or_default().push(event);
        }
    }
    for &old in blocks {
        let block = x.blocks[&old];
        x.b.switch_to(block);
        for &param in &source.ir.blocks[old.index()].params {
            let value = x.b.parameter(block, source.ir.value_type(param));
            x.define(param, value);
        }
        let mut debug = events.remove(&old).unwrap_or_default();
        debug.sort_by_key(|event| event.position);
        let mut debug = debug.into_iter().peekable();
        let data = &source.ir.blocks[old.index()];
        for position in 0..=data.instructions.len() {
            while debug
                .peek()
                .is_some_and(|event| event.position as usize == position)
            {
                let mut event = debug.next().unwrap();
                if let DebugChange::Set { local, value } = event.change {
                    event.change = DebugChange::Set {
                        local,
                        value: x.value(value),
                    };
                }
                x.debug.push(&x.b, event.change).line = event.line;
            }
            if let Some(&inst) = data.instructions.get(position) {
                x.instruction(inst, None);
                x.line(
                    source
                        .lines
                        .as_ref()
                        .and_then(|lines| lines.instructions.get(inst.index()).copied().flatten()),
                );
            }
        }
        x.terminator(data.terminator.unwrap());
        x.line(
            source
                .lines
                .as_ref()
                .and_then(|lines| lines.terminators.get(old.index()).copied().flatten()),
        );
    }
    for (edge, block, exceptional) in std::mem::take(&mut x.edges) {
        x.b.switch_to(block);
        if exceptional {
            let value = x.b.emit(Op::Exception, Some(frame.exception.ty)).unwrap();
            frame.store(&mut x.b, &x.arrays, frame.exception, value);
        }
        x.transfer(edge);
    }
    let ir = x.b.finish().map_err(|e| format!("outlined region: {e}"))?;
    let lines = source.lines.as_ref().map(|_| {
        x.lines.instructions.resize(ir.instructions.len(), None);
        x.lines.terminators.resize(ir.blocks.len(), None);
        x.lines
    });
    Ok(SsaBody {
        ir,
        types: Arc::clone(&types),
        lines,
        source_file: source.source_file.clone(),
        constants: x.constants,
        debug: (!x.debug.locals.is_empty()).then_some(x.debug),
    })
}
