use super::*;
use crate::scalar::{Scalar, ScalarType};
use rustc_hash::FxHashMap;
use std::collections::VecDeque;

/// Mutable source bindings exist only during construction. Unresolved reads
/// create block parameters; finalization fills their incoming arguments using
/// an iterative queue, including loop backedges and synthetic continuations.
pub struct Builder<'a> {
    pub body: Body,
    pub(super) types: &'a Types,
    current: BlockId,
    variables: Vec<TypeId>,
    uninitialized: Vec<bool>,
    bindings: FxHashMap<(BlockId, VariableId), ValueId>,
    pending: VecDeque<(BlockId, VariableId, ValueId, usize)>,
    constants: FxHashMap<(BlockId, Scalar), ValueId>,
}

impl<'a> Builder<'a> {
    pub fn new(types: &'a Types, return_type: TypeId) -> Self {
        let body = Body::new(return_type);
        Self {
            current: body.entry,
            body,
            types,
            variables: Vec::new(),
            uninitialized: Vec::new(),
            bindings: FxHashMap::default(),
            pending: VecDeque::new(),
            constants: FxHashMap::default(),
        }
    }
    pub fn current(&self) -> BlockId {
        self.current
    }
    pub fn create_block(&mut self) -> BlockId {
        let block = BlockId::new(self.body.blocks.len());
        self.body.blocks.push(Block::default());
        block
    }
    pub fn switch_to(&mut self, block: BlockId) {
        assert!(block.index() < self.body.blocks.len());
        self.current = block;
    }
    fn value(&mut self, ty: TypeId, def: ValueDef) -> ValueId {
        let value = ValueId::new(self.body.values.len());
        self.body.values.push(Value { ty, def });
        value
    }
    pub fn parameter(&mut self, block: BlockId, ty: TypeId) -> ValueId {
        let value = self.value(ty, ValueDef::Param(block));
        self.body.blocks[block.index()].params.push(value);
        value
    }
    pub fn variable(&mut self, ty: TypeId) -> VariableId {
        let var = VariableId::new(self.variables.len());
        self.variables.push(ty);
        self.uninitialized.push(false);
        var
    }
    /// A declared source local may be uninitialized on paths excluded by a
    /// separate drop flag. Generated temporaries continue to require a definition.
    pub fn local(&mut self, ty: TypeId) -> VariableId {
        let var = self.variable(ty);
        self.uninitialized[var.index()] = true;
        var
    }
    pub fn define(&mut self, var: VariableId, value: ValueId) {
        assert_eq!(
            self.variables[var.index()],
            self.body.value_type(value),
            "source variable changes type"
        );
        self.bindings.insert((self.current, var), value);
    }
    /// Source emission may use several semantic types for the same physical
    /// binding. Keep the precise value until a join actually needs a common type.
    pub fn define_carrier(&mut self, var: VariableId, value: ValueId) {
        assert_eq!(
            self.types
                .get(self.variables[var.index()])
                .unwrap()
                .carrier(),
            self.types
                .get(self.body.value_type(value))
                .unwrap()
                .carrier()
        );
        self.bindings.insert((self.current, var), value);
    }
    pub fn read(&mut self, var: VariableId) -> ValueId {
        self.read_in(self.current, var)
    }
    fn read_in(&mut self, block: BlockId, var: VariableId) -> ValueId {
        if let Some(&value) = self.bindings.get(&(block, var)) {
            return value;
        }
        if block == self.body.entry && self.uninitialized[var.index()] {
            let ty = self.variables[var.index()];
            let constant = ConstId::new(self.body.constants.len());
            self.body.constants.push(Constant::Uninit(ty));
            let inst = InstId::new(self.body.instructions.len());
            let value = self.value(ty, ValueDef::Inst(inst));
            self.body.instructions.push(Inst {
                op: Op::Constant(constant),
                result: Some(value),
            });
            self.body.blocks[block.index()].instructions.push(inst);
            self.bindings.insert((block, var), value);
            return value;
        }
        let index = self.body.blocks[block.index()].params.len();
        let value = self.parameter(block, self.variables[var.index()]);
        self.bindings.insert((block, var), value);
        self.pending.push_back((block, var, value, index));
        value
    }
    fn instruction(&mut self, op: Op, ty: Option<TypeId>) -> InstId {
        assert!(
            self.body.blocks[self.current.index()].terminator.is_none(),
            "instruction after terminator"
        );
        let inst = InstId::new(self.body.instructions.len());
        let result = ty.map(|ty| self.value(ty, ValueDef::Inst(inst)));
        self.body.instructions.push(Inst { op, result });
        inst
    }
    pub fn emit(&mut self, op: Op, ty: Option<TypeId>) -> Option<ValueId> {
        assert!(
            self.body.blocks[self.current.index()].terminator.is_none(),
            "instruction after terminator"
        );
        if let Some(value) = ty.and_then(|ty| self.fold(op, ty)) {
            return Some(value);
        }
        let inst = self.instruction(op, ty);
        self.body.blocks[self.current.index()]
            .instructions
            .push(inst);
        self.body.instructions[inst.index()].result
    }
    pub fn constant(&mut self, ty: TypeId, scalar: Scalar) -> ValueId {
        assert_eq!(self.types.get(ty), Some(Type::Scalar(scalar.ty())));
        if let Some(&value) = self.constants.get(&(self.current, scalar)) {
            return value;
        }
        let id = ConstId::new(self.body.constants.len());
        self.body.constants.push(Constant::Scalar(scalar));
        let value = self.emit(Op::Constant(id), Some(ty)).unwrap();
        self.constants.insert((self.current, scalar), value);
        value
    }
    pub fn args(&mut self, values: impl IntoIterator<Item = ValueId>) -> List {
        List::append(&mut self.body.args, values)
    }
    pub fn method(&mut self, method: MethodRef) -> MethodId {
        if let Some(index) = self.body.methods.iter().position(|m| *m == method) {
            return MethodId::new(index);
        }
        let id = MethodId::new(self.body.methods.len());
        self.body.methods.push(method);
        id
    }
    pub fn field(&mut self, field: FieldRef) -> MemberId {
        if let Some(index) = self.body.fields.iter().position(|f| *f == field) {
            return MemberId::new(index);
        }
        let id = MemberId::new(self.body.fields.len());
        self.body.fields.push(field);
        id
    }
    pub fn projection(&mut self, projection: PointerProjection) -> ProjectionId {
        if let Some(index) = self.body.projections.iter().position(|p| *p == projection) {
            return ProjectionId::new(index);
        }
        let id = ProjectionId::new(self.body.projections.len());
        self.body.projections.push(projection);
        id
    }
    pub fn edge(&mut self, target: BlockId, args: Vec<ValueId>) -> EdgeId {
        let edge = EdgeId::new(self.body.edges.len());
        self.body.edges.push(Edge { target, args });
        edge
    }
    pub fn terminate(&mut self, terminator: Terminator) {
        assert!(
            self.body.blocks[self.current.index()]
                .terminator
                .replace(terminator)
                .is_none(),
            "two terminators in one block"
        );
    }
    pub fn jump(&mut self, target: BlockId, args: Vec<ValueId>) {
        let edge = self.edge(target, args);
        self.terminate(Terminator::Jump(edge));
    }
    pub fn branch(&mut self, condition: ValueId, yes: BlockId, no: BlockId) {
        assert!(matches!(
            self.types.get(self.body.value_type(condition)),
            Some(Type::Scalar(ScalarType::Bool))
        ));
        if let Some(constant) = self.scalar_value(condition) {
            self.jump(if constant.bits() != 0 { yes } else { no }, Vec::new());
            return;
        }
        let yes = self.edge(yes, Vec::new());
        let no = self.edge(no, Vec::new());
        self.terminate(Terminator::Branch { condition, yes, no });
    }
    pub fn switch(
        &mut self,
        value: ValueId,
        targets: impl IntoIterator<Item = (Scalar, BlockId)>,
        otherwise: BlockId,
    ) {
        if let Some(constant) = self.scalar_value(value) {
            let target = targets
                .into_iter()
                .find_map(|(key, target)| (key == constant).then_some(target))
                .unwrap_or(otherwise);
            self.jump(target, Vec::new());
            return;
        }
        let start = self.body.cases.len();
        for (key, target) in targets {
            let edge = self.edge(target, Vec::new());
            self.body.cases.push((key, edge));
        }
        let cases = List {
            start: u32::try_from(start).expect("switch pool capacity"),
            len: u32::try_from(self.body.cases.len() - start).expect("switch capacity"),
        };
        let otherwise = self.edge(otherwise, Vec::new());
        self.terminate(Terminator::Switch {
            value,
            cases,
            otherwise,
        });
    }
    /// End the protected block and enter a fresh normal continuation. Source
    /// assignment happens after this call, so unwind bindings retain old values.
    pub fn invoke(&mut self, op: Op, ty: Option<TypeId>, handler: BlockId) -> Option<ValueId> {
        if !op.may_throw(&self.body, self.types) {
            return self.emit(op, ty);
        }
        if let Some(value) = ty.and_then(|ty| self.fold(op, ty)) {
            return Some(value);
        }
        let inst = self.instruction(op, ty);
        let continuation = self.create_block();
        let normal = self.edge(continuation, Vec::new());
        let unwind = self.edge(handler, Vec::new());
        self.terminate(Terminator::Invoke {
            inst,
            normal,
            unwind,
        });
        self.switch_to(continuation);
        self.body.instructions[inst.index()].result
    }

    pub fn finish(mut self) -> Result<Body, VerifyError> {
        let predecessors = self.body.predecessors();
        let reachable = self.body.reachable();
        while let Some((block, var, value, index)) = self.pending.pop_front() {
            if predecessors[block.index()].is_empty() {
                if reachable[block.index()] {
                    return Err(VerifyError(format!(
                        "undefined variable {var:?} in {block:?}"
                    )));
                }
                self.body.values[value.index()].def = ValueDef::Unreachable;
                continue;
            }
            for &(source, edge) in &predecessors[block.index()] {
                let mut incoming = self.read_in(source, var);
                let ty = self.variables[var.index()];
                if self.body.value_type(incoming) != ty {
                    let inst = InstId::new(self.body.instructions.len());
                    let result = self.value(ty, ValueDef::Inst(inst));
                    self.body.instructions.push(Inst {
                        op: Op::Reinterpret(incoming),
                        result: Some(result),
                    });
                    self.body.blocks[source.index()].instructions.push(inst);
                    incoming = result;
                }
                let args = &mut self.body.edges[edge.index()].args;
                // Pending parameters are filled in declaration order. Explicit
                // parameters must already have arguments on the branch edge.
                if args.len() != index {
                    return Err(VerifyError(format!(
                        "edge {edge:?} has inconsistent explicit parameters"
                    )));
                }
                args.push(incoming);
            }
        }
        super::parameters::remove_trivial_parameters(&mut self.body, &predecessors);
        // Selection checks the final body after outlining and constant
        // preparation. Also check this intermediate form in development builds.
        #[cfg(debug_assertions)]
        verify(&self.body, self.types)?;
        Ok(self.body)
    }
}
