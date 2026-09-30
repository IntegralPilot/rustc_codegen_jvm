//! Select projected fields through authoritative typed storage when available.
use super::*;

mod bytes;

impl Selector<'_> {
    pub(super) fn field_memory(&mut self, inst: Inst) -> jvm::Result<bool> {
        let address = match inst.op {
            Op::LoadStorageField { address, .. } | Op::LoadStorageFieldCopy { address, .. } => {
                Some(&self.body.args[address.range()])
            }
            Op::StoreStorageField { args, .. } => Some(&self.body.args[args.range()][..2]),
            _ => None,
        };
        let owned = matches!(
            inst.op,
            Op::LoadFieldCopy { .. } | Op::LoadStorageFieldCopy { .. }
        );
        let (base, projection, value, part, components) = match inst.op {
            Op::LoadStorageFieldCopy { projection, .. } => {
                (address.unwrap()[0], projection, None, None, None)
            }
            Op::LoadStorageField {
                projection, index, ..
            } => (address.unwrap()[0], projection, None, index, None),
            Op::StoreStorageField {
                args,
                projection,
                split,
            } => {
                let values = List {
                    start: args.start + 2,
                    len: args.len - 2,
                };
                (
                    address.unwrap()[0],
                    projection,
                    (!split).then(|| self.body.args[values.range().start]),
                    None,
                    split.then_some(values),
                )
            }
            Op::LoadField { base, projection } | Op::LoadFieldCopy { base, projection } => {
                (base, projection, None, None, None)
            }
            Op::StoreField {
                base,
                projection,
                value,
            } => (base, projection, Some(value), None, None),
            Op::LoadFieldPart {
                base,
                projection,
                index,
            } => (base, projection, None, Some(index), None),
            Op::StoreFieldParts {
                base,
                projection,
                parts,
            } => (base, projection, None, None, Some(parts)),
            _ => return Ok(false),
        };
        let field = &self.body.fields[self.body.projections[projection.index()].field.index()];
        let Some(Type::Class(owner)) = self.types.get(field.owner) else {
            return Err(error("field memory requires a concrete owner"));
        };
        let owner = self.cp.add_class(self.types.symbol_name(owner).unwrap())?;
        let pointer = self.cp.add_class(POINTER_CLASS)?;
        let direct = self.cp.add_method_ref(
            pointer,
            "directAggregate",
            "(Ljava/lang/Class;)Ljava/lang/Object;",
        )?;
        let split = part.is_some() || components.is_some();
        let physical = if split {
            let names = match self.types.get(field.ty) {
                Some(Type::TaggedI64) => super::super::abi::tagged_field_names(&field.name)
                    .into_iter()
                    .zip(["J", "J"])
                    .collect(),
                Some(Type::Pointer(_)) => {
                    vec![
                        (field.name.clone(), "Ljava/lang/Object;"),
                        (
                            super::super::abi::address_displacement_name(
                                self.types,
                                field.ty,
                                &field.name,
                            ),
                            "J",
                        ),
                    ]
                }
                Some(Type::Slice(_) | Type::Str) => super::super::abi::view_field_names(
                    &field.name,
                    matches!(self.types.get(field.ty), Some(Type::Str)),
                )
                .into_iter()
                .zip(["Ljava/lang/Object;", "I", "J"])
                .collect(),
                _ => return Err(error("invalid split field")),
            };
            Some(
                names
                    .into_iter()
                    .map(|(name, ty)| self.cp.add_field_ref(owner, &name, ty))
                    .collect::<jvm::Result<Vec<_>>>()?,
            )
        } else {
            None
        };
        let member = if let Some(physical) = &physical {
            physical[part.unwrap_or(0) as usize]
        } else {
            let mut descriptor = String::new();
            representation::descriptor(self.types, field.ty, &mut descriptor)?;
            self.cp.add_field_ref(owner, &field.name, &descriptor)?
        };
        let slow = self.assembly.label();
        let done = self.assembly.label();
        let store = value.is_some() || components.is_some();
        self.load(base)?;
        // Adjacent field components share one resolved aggregate object.
        // Mutations, opaque calls and control-flow boundaries invalidate it.
        let cached = part.is_some();
        let key = (
            self.body.resolve(base),
            address.map(|a| self.body.resolve(a[1])),
            field.owner,
        );
        if cached && self.aggregate_cache == Some(key) {
            self.assembly
                .code
                .push(Kind::Reference.load(self.aggregate_slot.unwrap()));
        } else {
            self.assembly.code.push(Instruction::Dup);
            if let Some(parts) = address {
                self.load(parts[1])?;
                self.assembly.code.push(Instruction::Ldc_w(owner));
                let resolve = self.cp.add_method_ref(
                    pointer,
                    "directStorageAggregate",
                    "(Ljava/lang/Object;JLjava/lang/Class;)Ljava/lang/Object;",
                )?;
                self.assembly.code.push(Instruction::Invokestatic(resolve));
            } else {
                self.assembly.code.extend([
                    Instruction::Ldc_w(owner),
                    Instruction::Invokevirtual(direct),
                ]);
            }
            if cached {
                let slot = if let Some(slot) = self.aggregate_slot {
                    slot
                } else {
                    let slot = self.next_slot;
                    self.next_slot = self
                        .next_slot
                        .checked_add(1)
                        .ok_or_else(|| error("JVM local limit"))?;
                    self.aggregate_slot = Some(slot);
                    slot
                };
                self.assembly
                    .code
                    .extend([Instruction::Dup, Kind::Reference.store(slot)]);
                self.aggregate_cache = Some(key);
            }
        }
        self.assembly.code.push(Instruction::Dup);
        self.assembly.branch(Instruction::Ifnull(0), slow);
        if !store {
            self.assembly
                .code
                .extend([Instruction::Swap, Instruction::Pop]);
        }
        self.assembly.code.push(Instruction::Checkcast(owner));
        if let Some(parts) = components {
            let values = &self.body.args[parts.range()];
            for (index, &value) in values.iter().enumerate() {
                if index + 1 < values.len() {
                    self.assembly.code.push(Instruction::Dup);
                }
                self.load(value)?;
                self.assembly
                    .code
                    .push(Instruction::Putfield(physical.as_ref().unwrap()[index]));
            }
        } else if let Some(value) = value {
            self.argument(value)?;
            self.assembly.code.push(Instruction::Putfield(member));
        } else {
            self.assembly.code.push(Instruction::Getfield(member));
            if owned {
                self.copy_value(field.ty)?;
            }
        }
        if store {
            if let Some(parts) = address {
                self.load(parts[1])?;
                let commit = self.cp.add_method_ref(
                    pointer,
                    "commitStorageLocation",
                    "(Ljava/lang/Object;J)V",
                )?;
                self.assembly.code.push(Instruction::Invokestatic(commit));
            } else {
                let commit = self.cp.add_method_ref(pointer, "commitMemoryView", "()V")?;
                self.assembly.code.push(Instruction::Invokevirtual(commit));
            }
        }
        self.assembly.branch(Instruction::Goto_w(0), done);
        self.assembly.bind(slow);
        self.assembly.code.push(Instruction::Pop);
        if owned {
            if let Some(parts) = address {
                self.load(parts[1])?;
            } else {
                self.assembly.code.push(Instruction::Lconst_0);
            }
            self.project_field_arguments(projection)?;
            let target = self.address_target(field.ty)?;
            let method = self.cp.add_method_ref(pointer, "loadStorageFieldCopy",
                "(Ljava/lang/Object;JLjava/lang/String;Ljava/lang/String;JJLjava/lang/String;Ljava/lang/String;)Ljava/lang/Object;")?;
            let target = self.cp.add_class(target)?;
            self.assembly.code.extend([
                Instruction::Invokestatic(method),
                Instruction::Checkcast(target),
            ]);
            self.assembly.bind(done);
            return Ok(true);
        }
        // Keep managed access direct. Share byte and fallback dispatch in the
        // runtime because larger generated methods can prevent JVM inlining.
        if let Some(value) = value
            && matches!(
                self.types.get(field.ty),
                Some(Type::Class(_) | Type::Array(_))
            )
        {
            if let Some(parts) = address {
                self.load(parts[1])?;
            } else {
                self.assembly.code.push(Instruction::Lconst_0);
            }
            self.project_field_arguments(projection)?;
            self.argument(value)?;
            let method = self.cp.add_method_ref(pointer, "storeStorageField",
                "(Ljava/lang/Object;JLjava/lang/String;Ljava/lang/String;JJLjava/lang/String;Ljava/lang/Object;)V")?;
            self.assembly.code.push(Instruction::Invokestatic(method));
            self.assembly.bind(done);
            return Ok(true);
        }
        if !split && self.scalar_field(address.map(|parts| parts[1]), projection, value)? {
            self.assembly.bind(done);
            return Ok(true);
        }
        if let Some(parts) = address {
            self.load(parts[1])?;
            let materialize = self.cp.add_method_ref(
                pointer,
                "fromStorageLocation",
                "(Ljava/lang/Object;J)Lorg/rustlang/runtime/Pointer;",
            )?;
            self.assembly
                .code
                .push(Instruction::Invokestatic(materialize));
        }
        self.project_field(projection)?;
        if let Some(parts) = components {
            if let Some(Type::Pointer(inner)) = self.types.get(field.ty) {
                for &value in &self.body.args[parts.range()] {
                    self.load(value)?;
                }
                self.materialize_address(inner)?;
            } else if matches!(self.types.get(field.ty), Some(Type::TaggedI64)) {
                self.materialize_tagged(parts)?;
            } else {
                self.materialize_view(field.ty, parts)?;
            }
            self.write_memory(field.ty)?;
        } else if let Some(value) = value {
            self.argument(value)?;
            self.write_memory(field.ty)?;
        } else {
            self.read_memory(field.ty)?;
            if let Some(index) = part {
                if matches!(self.types.get(field.ty), Some(Type::Pointer(_))) {
                    if index == 1 {
                        self.assembly
                            .code
                            .extend([Instruction::Pop, Instruction::Lconst_0]);
                    }
                } else if matches!(self.types.get(field.ty), Some(Type::TaggedI64)) {
                    let owner = self.cp.add_class(super::super::abi::TAGGED_LONG_CLASS)?;
                    self.assembly
                        .code
                        .push(Instruction::Invokestatic(self.cp.add_method_ref(
                            owner,
                            if index == 0 { "value" } else { "tag" },
                            "(Lorg/rustlang/runtime/TaggedLong;)J",
                        )?));
                } else {
                    let slice = self.cp.add_class(representation::SLICE_VIEW_CLASS)?;
                    let (name, ty) = [
                        ("array", "Ljava/lang/Object;"),
                        ("offset", "I"),
                        ("rustLength", "J"),
                    ][index as usize];
                    let member = self.cp.add_field_ref(slice, name, ty)?;
                    self.assembly.code.push(Instruction::Getfield(member));
                }
            }
        }
        self.assembly.bind(done);
        Ok(true)
    }
}
