//! Scalar field fallback with byte-storage dispatch shared in the runtime.
use super::*;

impl Selector<'_> {
    pub(super) fn nested_byte_field(
        &mut self,
        offset: Option<ValueId>,
        projection: ProjectionId,
        part: Option<u8>,
        done: Label,
    ) -> jvm::Result<()> {
        let layout = &self.body.projections[projection.index()];
        let ty = self.body.fields[layout.field.index()].ty;
        let scalar = part.is_none()
            && layout.codec.is_none()
            && StorageSlot::scalar(ty, self.types)
                .is_some_and(|s| u64::from(s.size) == layout.size);
        let pointer = matches!(self.types.get(ty), Some(Type::Pointer(_)))
            && part.is_none_or(|part| part == 0)
            && (1..=i32::MAX as u64).contains(&layout.size);
        if !scalar && !pointer {
            return Ok(());
        }
        let mut displacement = 0_u64;
        let mut next = Some(projection);
        while let Some(id) = next {
            let field = &self.body.projections[id.index()];
            let Some(sum) = displacement.checked_add(field.offset) else {
                return Ok(());
            };
            displacement = sum;
            next = field.parent;
        }
        let Ok(displacement) = i64::try_from(displacement) else {
            return Ok(());
        };
        let runtime = self.cp.add_class(POINTER_CLASS)?;
        let bytes =
            self.cp
                .add_method_ref(runtime, "hasPlainByteStorage", "(Ljava/lang/Object;)Z")?;
        let fallback = self.assembly.label();
        self.assembly
            .code
            .extend([Instruction::Dup, Instruction::Invokestatic(bytes)]);
        self.assembly.branch(Instruction::Ifeq(0), fallback);
        self.assembly
            .code
            .push(get_long_const_instr(self.cp, displacement));
        if let Some(offset) = offset {
            self.load(offset)?;
            let math = self.cp.add_class("java/lang/Math")?;
            let add = self.cp.add_method_ref(math, "addExact", "(JJ)J")?;
            self.assembly.code.push(Instruction::Invokestatic(add));
        }
        if scalar {
            self.read_address(ty)?;
            if self.types.get(ty) == Some(Type::Scalar(ScalarType::F16)) {
                self.assembly.code.push(Instruction::I2s);
            }
        } else {
            self.assembly
                .code
                .push(get_int_const_instr(self.cp, layout.size as i32));
            self.assembly.code.push(match &layout.codec {
                Some(codec) => Instruction::Ldc_w(self.cp.add_name_string(codec)?),
                None => Instruction::Aconst_null,
            });
            self.assembly
                .code
                .push(Instruction::Ldc_w(self.cp.add_name_string(POINTER_CLASS)?));
            let load = self.cp.add_method_ref(
                runtime,
                "loadTypedStorage",
                "(Ljava/lang/Object;JILjava/lang/String;Ljava/lang/String;)Ljava/lang/Object;",
            )?;
            self.assembly.code.extend([
                Instruction::Invokestatic(load),
                Instruction::Checkcast(runtime),
            ]);
        }
        self.assembly.branch(Instruction::Goto_w(0), done);
        self.assembly.bind(fallback);
        Ok(())
    }

    pub(in crate::jvm::select) fn project_scalar_path(
        &mut self,
        address: List,
        projection: ProjectionId,
    ) -> jvm::Result<()> {
        let parts = &self.body.args[address.range()];
        let pointer = self.cp.add_class(POINTER_CLASS)?;
        let bytes =
            self.cp
                .add_method_ref(pointer, "hasPlainByteStorage", "(Ljava/lang/Object;)Z")?;
        let done = self.assembly.label();
        self.load(parts[0])?;
        self.assembly
            .code
            .extend([Instruction::Dup, Instruction::Invokestatic(bytes)]);
        self.assembly.branch(Instruction::Ifne(0), done);
        self.load(parts[1])?;
        let materialize = self.cp.add_method_ref(
            pointer,
            "fromStorageLocation",
            "(Ljava/lang/Object;J)Lorg/rustlang/runtime/Pointer;",
        )?;
        self.assembly
            .code
            .push(Instruction::Invokestatic(materialize));
        let mut path = vec![projection];
        while let Some(parent) = self.body.projections[path[path.len() - 1].index()].parent {
            path.push(parent);
        }
        for projection in path.into_iter().rev() {
            self.project_field(projection)?;
        }
        self.assembly.bind(done);
        Ok(())
    }

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
