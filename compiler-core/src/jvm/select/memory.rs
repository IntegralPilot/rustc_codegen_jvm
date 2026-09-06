use super::*;
use representation::descriptor;

fn scalar(types: &Types, ty: TypeId) -> jvm::Result<ScalarType> {
    match types.get(ty) {
        Some(Type::Scalar(ty)) => Ok(ty),
        _ => Err(error("memory value representation is not yet supported")),
    }
}

impl Selector<'_> {
    pub(super) fn materialize_parameters(&mut self) -> jvm::Result<()> {
        for &param in &self.body.blocks[self.body.entry.index()].params {
            if !matches!(
                self.types.get(self.body.value_type(param)),
                Some(Type::Pointer(_))
            ) {
                continue;
            }
            let slot = self.slot(param);
            let owner = self.cp.add_class(POINTER_CLASS)?;
            let method = self.cp.add_method_ref(
                owner,
                "materializeRelative",
                "(Lorg/rustlang/runtime/Pointer;JJ)Lorg/rustlang/runtime/Pointer;",
            )?;
            self.assembly.code.extend([
                Kind::Reference.load(slot),
                Kind::Long.load(slot + 1),
                Kind::Long.load(slot + 3),
                Instruction::Invokestatic(method),
                Kind::Reference.store(slot),
            ]);
        }
        Ok(())
    }
    pub(super) fn initialize_storage(&mut self) -> jvm::Result<()> {
        for storage in &self.body.slots {
            let owner = self.cp.add_class(POINTER_CLASS)?;
            let target = if let Some(Type::Scalar(scalar)) = self.types.get(storage.ty) {
                use ScalarType::*;
                let code = match scalar {
                    Bool => 4,
                    I8 | U8 => 8,
                    I16 => 9,
                    U16 => 5,
                    I32 | U32 => 10,
                    I64 | U64 => 11,
                    F32 => 6,
                    F64 => 7,
                    _ => return Err(error("unsupported primitive storage")),
                };
                let array =
                    jvm::attributes::ArrayType::from_bytes(&mut jvm::ByteReader::new(&[code]))?;
                self.assembly.code.extend([
                    Instruction::Iconst_1,
                    Instruction::Newarray(array),
                    Instruction::Iconst_0,
                    get_int_const_instr(self.cp, storage.size as i32),
                ]);
                self.cp.add_method_ref(
                    owner,
                    "array",
                    "(Ljava/lang/Object;II)Lorg/rustlang/runtime/Pointer;",
                )?
            } else {
                self.assembly.code.push(Instruction::Aconst_null);
                self.assembly
                    .code
                    .push(get_int_const_instr(self.cp, storage.size as i32));
                self.assembly.code.push(if let Some(codec) = storage.codec {
                    Instruction::Ldc_w(self.cp.add_string(self.types.symbol_name(codec).unwrap())?)
                } else {
                    Instruction::Aconst_null
                });
                self.assembly
                    .code
                    .push(get_int_const_instr(self.cp, storage.alignment as i32));
                self.cp.add_method_ref(
                    owner,
                    "cellAligned",
                    "(Ljava/lang/Object;ILjava/lang/String;I)Lorg/rustlang/runtime/Pointer;",
                )?
            };
            let slot = self.next_slot;
            self.next_slot = self
                .next_slot
                .checked_add(1)
                .ok_or_else(|| error("JVM storage slot limit"))?;
            self.storage.push(slot);
            self.assembly.code.extend([
                Instruction::Invokestatic(target),
                Kind::Reference.store(slot),
            ]);
        }
        Ok(())
    }

    pub(super) fn memory(&mut self, inst: Inst) -> jvm::Result<bool> {
        match inst.op {
            Op::Opaque(value) => self.load(value)?,
            Op::Project { base, projection } => {
                let projection = &self.body.projections[projection.index()];
                let field = &self.body.fields[projection.field.index()];
                let Some(Type::Class(symbol)) = self.types.get(field.owner) else {
                    return Err(error("projection requires class layout"));
                };
                self.load(base)?;
                self.assembly.code.extend([
                    Instruction::Ldc_w(
                        self.cp
                            .add_string(self.types.symbol_name(symbol).unwrap())?,
                    ),
                    Instruction::Ldc_w(self.cp.add_string(&field.name)?),
                    get_long_const_instr(self.cp, projection.offset as i64),
                    get_long_const_instr(self.cp, projection.size as i64),
                    match &projection.codec {
                        Some(codec) => Instruction::Ldc_w(self.cp.add_string(codec)?),
                        None => Instruction::Aconst_null,
                    },
                ]);
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(owner, "projectStructField", "(Ljava/lang/String;Ljava/lang/String;JJLjava/lang/String;)Lorg/rustlang/runtime/Pointer;")?;
                self.assembly.code.push(Instruction::Invokevirtual(method));
            }
            Op::Offset {
                pointer,
                offset,
                bytes,
                wrapping,
            } => {
                self.load(pointer)?;
                self.load(offset)?;
                self.assembly.code.extend(super::super::casts::primitive(
                    &self.scalar_type(offset)?,
                    &ScalarType::I64,
                    self.cp,
                )?);
                let name = match (bytes, wrapping) {
                    (false, false) => "offset",
                    (true, false) => "byte_offset",
                    (false, true) => "wrapping_offset",
                    (true, true) => "wrapping_byte_offset",
                };
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(
                    owner,
                    name,
                    "(Lorg/rustlang/runtime/Pointer;J)Lorg/rustlang/runtime/Pointer;",
                )?;
                self.assembly.code.push(Instruction::Invokestatic(method));
            }
            Op::AddressOfSlot(slot) => self
                .assembly
                .code
                .push(Kind::Reference.load(self.storage[slot.index()])),
            Op::LoadSlot(slot) => {
                self.assembly
                    .code
                    .push(Kind::Reference.load(self.storage[slot.index()]));
                self.read_memory(self.body.slots[slot.index()].ty)?;
            }
            Op::StoreSlot { slot, value } => {
                self.assembly
                    .code
                    .push(Kind::Reference.load(self.storage[slot.index()]));
                self.argument(value)?;
                let storage = &self.body.slots[slot.index()];
                if storage.size == 0 {
                    let owner = self.cp.add_class(POINTER_CLASS)?;
                    let init = self.cp.add_method_ref(
                        owner,
                        "initializeZeroSizedLocal",
                        "(Ljava/lang/Object;)V",
                    )?;
                    self.assembly.code.push(Instruction::Invokevirtual(init));
                } else {
                    self.write_memory(storage.ty)?;
                }
            }
            Op::Load(pointer) => {
                self.load(pointer)?;
                self.read_memory(self.body.value_type(inst.result.unwrap()))?;
            }
            Op::Store { pointer, value } => {
                self.load(pointer)?;
                self.argument(value)?;
                self.write_memory(self.body.value_type(value))?;
            }
            Op::Cast(value)
                if matches!(
                    self.types.get(self.body.value_type(value)),
                    Some(Type::Pointer(_))
                ) && matches!(
                    self.types.get(self.body.value_type(inst.result.unwrap())),
                    Some(Type::Pointer(_))
                ) =>
            {
                // Scalar pointer retyping keeps its allocation/provenance and
                // updates the runtime view size used by checked loads/stores.
                let result = inst.result.unwrap();
                let Some(Type::Pointer(pointee)) = self.types.get(self.body.value_type(result))
                else {
                    return Err(error("unsupported reference cast"));
                };
                let to = scalar(self.types, pointee)?;
                let bytes = match to {
                    ScalarType::Bool => 1,
                    ScalarType::F32 => 4,
                    ScalarType::F64 => 8,
                    _ => to.integer().ok_or_else(|| error("pointer view type"))?.0 / 8,
                };
                self.load(value)?;
                self.assembly.code.extend([
                    get_int_const_instr(self.cp, bytes as i32),
                    Instruction::Aconst_null,
                ]);
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let target = self.cp.add_method_ref(
                    owner,
                    "retype",
                    "(ILjava/lang/String;)Lorg/rustlang/runtime/Pointer;",
                )?;
                self.assembly.code.push(Instruction::Invokevirtual(target));
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    pub(super) fn read_memory(&mut self, ty: TypeId) -> jvm::Result<()> {
        let class = match self.types.get(ty) {
            Some(Type::Class(symbol) | Type::Interface(symbol)) => {
                Some(self.types.symbol_name(symbol).unwrap())
            }
            Some(Type::Pointer(_)) => Some(POINTER_CLASS),
            Some(Type::Slice(_)) => Some(representation::SLICE_VIEW_CLASS),
            Some(Type::Str) => Some(representation::UTF8_VIEW_CLASS),
            _ => None,
        };
        if let Some(class) = class {
            let owner = self.cp.add_class(POINTER_CLASS)?;
            let target = if matches!(
                self.types.get(ty),
                Some(Type::Class(_) | Type::Interface(_) | Type::Slice(_) | Type::Str)
            ) {
                self.assembly
                    .code
                    .push(Instruction::Ldc_w(self.cp.add_string(class)?));
                self.cp.add_method_ref(
                    owner,
                    "getObjectAs",
                    "(Ljava/lang/String;)Ljava/lang/Object;",
                )?
            } else {
                self.cp
                    .add_method_ref(owner, "getObject", "()Ljava/lang/Object;")?
            };
            let class = self.cp.add_class(class)?;
            self.assembly.code.extend([
                Instruction::Invokevirtual(target),
                Instruction::Checkcast(class),
            ]);
            return Ok(());
        }
        use ScalarType::*;
        let (name, result) = match scalar(self.types, ty)? {
            Bool => ("getBoolean", "Z"),
            I8 | U8 => ("getI8", "B"),
            I16 | U16 => ("getI16", "S"),
            I32 | U32 => ("getI32", "I"),
            I64 | U64 => ("getI64", "J"),
            F32 => ("getF32", "F"),
            F64 => ("getF64", "D"),
            _ => return Err(error("unsupported pointer load")),
        };
        let owner = self.cp.add_class(POINTER_CLASS)?;
        let target = self
            .cp
            .add_method_ref(owner, name, &format!("(){result}"))?;
        self.assembly.code.push(Instruction::Invokevirtual(target));
        Ok(())
    }
    pub(super) fn write_memory(&mut self, ty: TypeId) -> jvm::Result<()> {
        let mut signature = String::from("(");
        if matches!(
            self.types.get(ty),
            Some(
                Type::Class(_) | Type::Interface(_) | Type::Pointer(_) | Type::Slice(_) | Type::Str
            )
        ) {
            signature.push_str("Ljava/lang/Object;");
        } else {
            scalar(self.types, ty)?;
            descriptor(self.types, ty, &mut signature)?;
        }
        signature.push_str(")V");
        let owner = self.cp.add_class(POINTER_CLASS)?;
        let target = self.cp.add_method_ref(owner, "set", &signature)?;
        self.assembly.code.push(Instruction::Invokevirtual(target));
        Ok(())
    }
}
