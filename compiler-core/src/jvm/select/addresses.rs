//! Physical address components and scalar memory operations.
use super::*;

impl Selector<'_> {
    pub(super) fn address(&mut self, inst: Inst) -> jvm::Result<bool> {
        match inst.op {
            Op::CopyStorage {
                parts,
                layouts,
                nonoverlapping,
            } => {
                let parts = &self.body.args[parts.range()];
                for (i, layout) in layouts.into_iter().enumerate() {
                    let Some(Type::Layout(layout)) = self.types.get(layout) else {
                        return Err(error("copy needs an exact storage layout"));
                    };
                    let AddressLayout { size, codec, .. } = self.types.get_layout(layout);
                    self.load(parts[i * 2])?;
                    self.load(parts[i * 2 + 1])?;
                    self.address_layout(size, codec)?;
                }
                self.load(parts[4])?;
                if self
                    .types
                    .get(self.body.value_type(parts[4]))
                    .unwrap()
                    .carrier()
                    == 0
                {
                    self.assembly.code.push(Instruction::I2l);
                }
                self.assembly.code.push(if nonoverlapping {
                    Instruction::Iconst_1
                } else {
                    Instruction::Iconst_0
                });
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(owner, "copyStorage",
                    "(Ljava/lang/Object;JILjava/lang/String;Ljava/lang/Object;JILjava/lang/String;JZ)V")?;
                self.assembly.code.push(Instruction::Invokestatic(method));
            }
            Op::TypedAddressViewPart {
                parts,
                size,
                codec,
                index,
            } => {
                for &part in &self.body.args[parts.range()] {
                    self.load(part)?;
                }
                self.address_layout(size, codec)?;
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let (name, result) = if index == 0 {
                    ("typedLocationSliceBacking", "Ljava/lang/Object;")
                } else {
                    ("typedLocationSliceOffset", "I")
                };
                let method = self.cp.add_method_ref(
                    owner,
                    name,
                    format!("(Ljava/lang/Object;JILjava/lang/String;){result}"),
                )?;
                self.assembly.code.push(Instruction::Invokestatic(method));
            }
            Op::RetypeAddress {
                pointer,
                size,
                codec,
            } => {
                self.load(pointer)?;
                self.address_layout(size, codec)?;
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(
                    owner,
                    "retype",
                    "(ILjava/lang/String;)Lorg/rustlang/runtime/Pointer;",
                )?;
                self.assembly.code.push(Instruction::Invokevirtual(method));
            }
            Op::TypedAddressPack { parts, size, codec } => {
                for &part in &self.body.args[parts.range()] {
                    self.load(part)?;
                }
                self.address_layout(size, codec)?;
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(
                    owner,
                    "fromTypedStorageLocation",
                    "(Ljava/lang/Object;JILjava/lang/String;)Lorg/rustlang/runtime/Pointer;",
                )?;
                self.assembly.code.push(Instruction::Invokestatic(method));
            }
            Op::LoadTypedCopy { parts, size, codec } => {
                for &part in &self.body.args[parts.range()] {
                    self.load(part)?;
                }
                self.address_layout(size, codec)?;
                let ty = self.body.value_type(inst.result.unwrap());
                let name = self.address_target(ty)?;
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(
                    owner,
                    "loadTypedStorageCopy",
                    "(Ljava/lang/Object;JILjava/lang/String;Ljava/lang/String;)Ljava/lang/Object;",
                )?;
                let class = self.cp.add_class(&name)?;
                self.assembly.code.extend([
                    Instruction::Invokestatic(method),
                    Instruction::Checkcast(class),
                ]);
            }
            Op::LoadTyped { parts, size, codec } => {
                let parts = &self.body.args[parts.range()];
                self.load(parts[0])?;
                self.load(parts[1])?;
                self.address_layout(size, codec)?;
                if let Some(&target) = parts.get(2) {
                    self.load(target)?;
                } else if let Some(Type::Class(name) | Type::Interface(name)) =
                    self.types.get(self.body.value_type(inst.result.unwrap()))
                    && self.types.symbol_name(name) != Some("java/lang/Object")
                {
                    self.assembly.code.push(Instruction::Ldc_w(
                        self.cp
                            .add_name_string(self.types.symbol_name(name).unwrap())?,
                    ));
                } else {
                    self.assembly.code.push(Instruction::Aconst_null);
                }
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(
                    owner,
                    "loadTypedStorage",
                    "(Ljava/lang/Object;JILjava/lang/String;Ljava/lang/String;)Ljava/lang/Object;",
                )?;
                self.assembly.code.push(Instruction::Invokestatic(method));
                let mut descriptor = String::new();
                representation::descriptor(
                    self.types,
                    self.body.value_type(inst.result.unwrap()),
                    &mut descriptor,
                )?;
                if descriptor != "Ljava/lang/Object;" {
                    let target = descriptor
                        .strip_prefix('L')
                        .and_then(|s| s.strip_suffix(';'))
                        .unwrap_or(&descriptor);
                    self.assembly
                        .code
                        .push(Instruction::Checkcast(self.cp.add_class(target)?));
                }
            }
            Op::StoreTyped { parts, size, codec } => {
                let parts = &self.body.args[parts.range()];
                self.load(parts[0])?;
                self.load(parts[1])?;
                self.address_layout(size, codec)?;
                self.load(parts[2])?;
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(
                    owner,
                    "storeTypedStorage",
                    "(Ljava/lang/Object;JILjava/lang/String;Ljava/lang/Object;)V",
                )?;
                self.assembly.code.push(Instruction::Invokestatic(method));
            }
            Op::ProjectRoot {
                address,
                projection,
            } => {
                for &part in &self.body.args[address.range()] {
                    self.load(part)?;
                }
                self.project_field_arguments(projection)?;
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let field =
                    &self.body.fields[self.body.projections[projection.index()].field.index()];
                let name = if ComponentShape::of(self.types, field.ty)
                    .is_some_and(ComponentShape::is_borrowed)
                {
                    "storageBorrowedFieldRoot"
                } else {
                    "storageFieldRoot"
                };
                let method = self.cp.add_method_ref(owner, name,
                    "(Ljava/lang/Object;JLjava/lang/String;Ljava/lang/String;JJLjava/lang/String;)Ljava/lang/Object;")?;
                self.assembly.code.push(Instruction::Invokestatic(method));
            }
            Op::ProjectOffset { root, base, offset } => {
                self.load(root)?;
                self.load(base)?;
                self.load(offset)?;
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(
                    owner,
                    "storageFieldOffset",
                    "(Ljava/lang/Object;Ljava/lang/Object;J)J",
                )?;
                self.assembly.code.push(Instruction::Invokestatic(method));
            }
            Op::AddressTag(value) => {
                self.load(value)?;
                self.assembly.code.push(Instruction::Lconst_0);
                self.location_tag()?;
            }
            Op::LocationTag(parts) => {
                for &part in &self.body.args[parts.range()] {
                    self.load(part)?;
                }
                self.location_tag()?;
            }
            Op::AddressViewPart { address, index } => {
                self.load(address)?;
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let (name, descriptor) = if index == 0 {
                    ("sliceBackingArray", "()Ljava/lang/Object;")
                } else {
                    ("sliceElementOffset", "()I")
                };
                let method = self.cp.add_method_ref(owner, name, descriptor)?;
                self.assembly.code.push(Instruction::Invokevirtual(method));
            }
            Op::AddressEqual { left, right } | Op::AddressCompare { left, right } => {
                self.load(left)?;
                self.assembly.code.push(Instruction::Lconst_0);
                self.load(right)?;
                self.assembly.code.push(Instruction::Lconst_0);
                self.location_comparison(matches!(inst.op, Op::AddressCompare { .. }))?;
            }
            Op::LocationEqual(parts) | Op::LocationCompare(parts) => {
                for &part in &self.body.args[parts.range()] {
                    self.load(part)?;
                }
                self.location_comparison(matches!(inst.op, Op::LocationCompare(_)))?;
            }
            Op::AddressPart { address, index: 0 } => self.load(address)?,
            Op::AddressPart { index: 1, .. } => self.assembly.code.push(Instruction::Lconst_0),
            Op::SlotRoot(slot) => self
                .assembly
                .code
                .push(Kind::Reference.load(self.storage[slot.index()])),
            Op::AddressPack(parts) => {
                for &part in &self.body.args[parts.range()] {
                    self.load(part)?;
                }
                let Some(Type::Pointer(inner)) =
                    self.types.get(self.body.value_type(inst.result.unwrap()))
                else {
                    return Err(error("address pack needs pointer type"));
                };
                self.materialize_address(inner)?;
            }
            Op::LoadAddressCopy(parts) => {
                for &part in &self.body.args[parts.range()] {
                    self.load(part)?;
                }
                self.read_object_address(self.body.value_type(inst.result.unwrap()), true)?;
            }
            Op::LoadAddress(parts) => {
                for &part in &self.body.args[parts.range()] {
                    self.load(part)?;
                }
                self.read_address(self.body.value_type(inst.result.unwrap()))?;
            }
            Op::StoreAddress { parts, value } => {
                for &part in &self.body.args[parts.range()] {
                    self.load(part)?;
                }
                self.load(value)?;
                self.write_address(self.body.value_type(value))?;
            }
            _ => return Ok(false),
        }
        Ok(true)
    }
    pub(super) fn materialize_address(&mut self, ty: TypeId) -> jvm::Result<()> {
        if let Some(Type::Layout(id)) = self.types.get(ty) {
            let AddressLayout { size, codec, .. } = self.types.get_layout(id);
            return super::super::abi::materialize_typed_address(
                self.cp,
                &mut self.assembly.code,
                size,
                codec.map(|s| self.types.symbol_name(s).unwrap()),
            );
        }
        super::super::abi::materialize_address(
            self.cp,
            &mut self.assembly.code,
            super::super::abi::address_plan(self.types, ty),
        )
    }
    fn location_tag(&mut self) -> jvm::Result<()> {
        let owner = self.cp.add_class(POINTER_CLASS)?;
        let method =
            self.cp
                .add_method_ref(owner, "nullableLocationTag", "(Ljava/lang/Object;J)J")?;
        self.assembly.code.push(Instruction::Invokestatic(method));
        Ok(())
    }
    fn location_comparison(&mut self, ordering: bool) -> jvm::Result<()> {
        let owner = self.cp.add_class(POINTER_CLASS)?;
        let method = self.cp.add_method_ref(
            owner,
            if ordering {
                "compareLocations"
            } else {
                "sameLocation"
            },
            if ordering {
                "(Ljava/lang/Object;JLjava/lang/Object;J)I"
            } else {
                "(Ljava/lang/Object;JLjava/lang/Object;J)Z"
            },
        )?;
        self.assembly.code.push(Instruction::Invokestatic(method));
        Ok(())
    }
    pub(super) fn read_address(&mut self, ty: TypeId) -> jvm::Result<()> {
        if self.types.get(ty).is_some_and(|t| t.carrier() == 5) {
            self.read_object_address(ty, false)?;
            return Ok(());
        }
        let size = StorageSlot::scalar(ty, self.types)
            .ok_or_else(|| error("scalar load layout"))?
            .size;
        self.assembly
            .code
            .push(get_int_const_instr(self.cp, size as i32));
        let owner = self.cp.add_class(POINTER_CLASS)?;
        let method =
            self.cp
                .add_method_ref(owner, "loadLocationBits", "(Ljava/lang/Object;JI)J")?;
        self.assembly.code.push(Instruction::Invokestatic(method));
        self.bits_to_scalar(ty)
    }
    pub(super) fn bits_to_scalar(&mut self, ty: TypeId) -> jvm::Result<()> {
        match self.types.get(ty) {
            Some(Type::Scalar(ScalarType::F64)) => {
                self.address_float("java/lang/Double", "longBitsToDouble", "(J)D")?
            }
            Some(Type::Scalar(ScalarType::I64 | ScalarType::U64)) => {}
            Some(Type::Scalar(scalar)) => {
                self.assembly.code.push(Instruction::L2i);
                if scalar == ScalarType::F32 {
                    self.address_float("java/lang/Float", "intBitsToFloat", "(I)F")?;
                }
            }
            _ => return Err(error("non-scalar address load")),
        }
        Ok(())
    }
    pub(super) fn read_object_address(&mut self, ty: TypeId, owned: bool) -> jvm::Result<()> {
        let name = self.address_target(ty)?;
        let owner = self.cp.add_class(POINTER_CLASS)?;
        let method = self.cp.add_method_ref(
            owner,
            if owned {
                "loadStorageCopy"
            } else if matches!(self.types.get(ty), Some(Type::Array(_))) {
                "loadStorageArray"
            } else {
                "loadStorageLocation"
            },
            "(Ljava/lang/Object;JLjava/lang/String;)Ljava/lang/Object;",
        )?;
        let class = self.cp.add_class(&name)?;
        self.assembly.code.extend([
            Instruction::Invokestatic(method),
            Instruction::Checkcast(class),
        ]);
        Ok(())
    }
    pub(super) fn address_layout(&mut self, size: u32, codec: Option<SymbolId>) -> jvm::Result<()> {
        self.assembly
            .code
            .push(get_int_const_instr(self.cp, size as i32));
        self.assembly.code.push(match codec {
            Some(id) => Instruction::Ldc_w(
                self.cp
                    .add_name_string(self.types.symbol_name(id).unwrap())?,
            ),
            None => Instruction::Aconst_null,
        });
        Ok(())
    }
    pub(super) fn address_target(&mut self, ty: TypeId) -> jvm::Result<String> {
        let mut descriptor = String::new();
        representation::descriptor(self.types, ty, &mut descriptor)?;
        let name = descriptor
            .strip_prefix('L')
            .and_then(|s| s.strip_suffix(';'))
            .unwrap_or(&descriptor);
        self.assembly
            .code
            .push(if matches!(self.types.get(ty), Some(Type::Pointer(_))) {
                Instruction::Aconst_null
            } else {
                Instruction::Ldc_w(self.cp.add_name_string(name)?)
            });
        Ok(name.into())
    }
    pub(super) fn write_address(&mut self, ty: TypeId) -> jvm::Result<()> {
        if self.types.get(ty).is_some_and(|t| t.carrier() == 5) {
            let owner = self.cp.add_class(POINTER_CLASS)?;
            let method = self.cp.add_method_ref(
                owner,
                "storeStorageLocation",
                "(Ljava/lang/Object;JLjava/lang/Object;)V",
            )?;
            self.assembly.code.push(Instruction::Invokestatic(method));
            return Ok(());
        }
        let size = StorageSlot::scalar(ty, self.types)
            .ok_or_else(|| error("scalar store layout"))?
            .size;
        self.scalar_to_bits(ty)?;
        self.assembly
            .code
            .push(get_int_const_instr(self.cp, size as i32));
        let owner = self.cp.add_class(POINTER_CLASS)?;
        let method =
            self.cp
                .add_method_ref(owner, "storeLocationBits", "(Ljava/lang/Object;JJI)V")?;
        self.assembly.code.push(Instruction::Invokestatic(method));
        Ok(())
    }
    pub(super) fn scalar_to_bits(&mut self, ty: TypeId) -> jvm::Result<()> {
        match self.types.get(ty) {
            Some(Type::Scalar(ScalarType::F64)) => {
                self.address_float("java/lang/Double", "doubleToRawLongBits", "(D)J")?
            }
            Some(Type::Scalar(ScalarType::I64 | ScalarType::U64)) => {}
            Some(Type::Scalar(scalar)) => {
                if scalar == ScalarType::F32 {
                    self.address_float("java/lang/Float", "floatToRawIntBits", "(F)I")?;
                }
                self.assembly.code.push(Instruction::I2l);
            }
            _ => return Err(error("non-scalar address store")),
        }
        Ok(())
    }
    fn address_float(&mut self, class: &str, name: &str, descriptor: &str) -> jvm::Result<()> {
        let owner = self.cp.add_class(class)?;
        let method = self.cp.add_method_ref(owner, name, descriptor)?;
        self.assembly.code.push(Instruction::Invokestatic(method));
        Ok(())
    }
}
