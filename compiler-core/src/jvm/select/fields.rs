//! Select promoted field pointers without reflective runtime field cells.
use super::*;

impl Selector<'_> {
    pub(super) fn field_memory(&mut self, inst: Inst) -> jvm::Result<bool> {
        let (base, projection, value) = match inst.op {
            Op::LoadField { base, projection } => (base, projection, None),
            Op::StoreField {
                base,
                projection,
                value,
            } => (base, projection, Some(value)),
            _ => return Ok(false),
        };
        let field = &self.body.fields[self.body.projections[projection.index()].field.index()];
        let Some(Type::Class(owner)) = self.types.get(field.owner) else {
            return Err(error("field memory requires a concrete owner"));
        };
        let owner = self.cp.add_class(self.types.symbol_name(owner).unwrap())?;
        let mut descriptor = String::new();
        representation::descriptor(self.types, field.ty, &mut descriptor)?;
        let member = self.cp.add_field_ref(owner, &field.name, &descriptor)?;
        let pointer = self.cp.add_class(POINTER_CLASS)?;
        let direct = self.cp.add_method_ref(
            pointer,
            "directAggregate",
            "(Ljava/lang/Class;)Ljava/lang/Object;",
        )?;
        let slow = self.assembly.label();
        let done = self.assembly.label();
        // Keep the pointer for committing the live carrier, or projecting just
        // this field when storage is raw or only partially initialized.
        self.load(base)?;
        self.assembly.code.extend([
            Instruction::Dup,
            Instruction::Ldc_w(owner),
            Instruction::Invokevirtual(direct),
            Instruction::Dup,
        ]);
        self.assembly.branch(Instruction::Ifnull(0), slow);
        if value.is_none() {
            self.assembly
                .code
                .extend([Instruction::Swap, Instruction::Pop]);
        }
        self.assembly.code.push(Instruction::Checkcast(owner));
        if let Some(value) = value {
            self.argument(value)?;
            self.assembly.code.push(Instruction::Putfield(member));
            let commit = self.cp.add_method_ref(pointer, "commitMemoryView", "()V")?;
            self.assembly.code.push(Instruction::Invokevirtual(commit));
        } else {
            self.assembly.code.push(Instruction::Getfield(member));
        }
        self.assembly.branch(Instruction::Goto_w(0), done);
        self.assembly.bind(slow);
        self.assembly.code.push(Instruction::Pop);
        self.project_field(projection)?;
        if let Some(value) = value {
            self.argument(value)?;
            self.write_memory(field.ty)?;
        } else {
            self.read_memory(field.ty)?;
        }
        self.assembly.bind(done);
        Ok(true)
    }
}
