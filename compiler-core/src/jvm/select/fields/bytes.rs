//! Scalar field fallback with byte-storage dispatch shared in the runtime.
use super::*;

impl Selector<'_> {
    /// Consume the root already on the stack, preserving stack forwarding.
    pub(super) fn scalar_field(
        &mut self,
        offset: Option<ValueId>,
        projection: ProjectionId,
        value: Option<ValueId>,
    ) -> jvm::Result<bool> {
        let layout = &self.body.projections[projection.index()];
        let field = &self.body.fields[layout.field.index()];
        let ty = field.ty;
        if layout.codec.is_some()
            || !StorageSlot::scalar(ty, self.types)
                .is_some_and(|s| (1..=8).contains(&s.size) && u64::from(s.size) == layout.size)
        {
            return Ok(false);
        }
        let Some(Type::Class(owner)) = self.types.get(field.owner) else {
            return Err(error("scalar field requires a concrete owner"));
        };
        if let Some(offset) = offset {
            self.load(offset)?;
        } else {
            self.assembly.code.push(Instruction::Lconst_0);
        }
        self.assembly.code.extend([
            Instruction::Ldc_w(
                self.cp
                    .add_name_string(self.types.symbol_name(owner).unwrap())?,
            ),
            Instruction::Ldc_w(self.cp.add_string(&field.name)?),
            get_long_const_instr(self.cp, layout.offset as i64),
        ]);
        if let Some(value) = value {
            self.argument(value)?;
            self.scalar_to_bits(ty)?;
        }
        self.assembly
            .code
            .push(get_int_const_instr(self.cp, layout.size as i32));
        let pointer = self.cp.add_class(POINTER_CLASS)?;
        let method = self.cp.add_method_ref(
            pointer,
            if value.is_some() {
                "storeScalarField"
            } else {
                "loadScalarField"
            },
            if value.is_some() {
                "(Ljava/lang/Object;JLjava/lang/String;Ljava/lang/String;JJI)V"
            } else {
                "(Ljava/lang/Object;JLjava/lang/String;Ljava/lang/String;JI)J"
            },
        )?;
        self.assembly.code.push(Instruction::Invokestatic(method));
        if value.is_none() {
            self.bits_to_scalar(ty)?;
            // Integer result normalization is shared with all other IR loads.
            match self.types.get(ty) {
                Some(Type::Scalar(ScalarType::F16)) => self.assembly.code.push(Instruction::I2s),
                Some(Type::Scalar(ScalarType::Char)) => self.assembly.code.push(Instruction::I2c),
                _ => {}
            }
        }
        Ok(true)
    }
}
