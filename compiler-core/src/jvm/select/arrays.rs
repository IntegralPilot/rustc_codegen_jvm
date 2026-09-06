//! Array operations preserve lazy repeat copies and pointer-backed slice views.
use super::*;
impl Selector<'_> {
    pub(super) fn array(&mut self, inst: Inst) -> jvm::Result<bool> {
        if let Op::ArrayLength(array) = inst.op {
            self.load(array)?;
            let op = if matches!(
                self.types.get(self.body.value_type(array)),
                Some(Type::Array(_))
            ) {
                Instruction::Arraylength
            } else {
                let owner = self.cp.add_class(representation::SLICE_VIEW_CLASS)?;
                Instruction::Getfield(self.cp.add_field_ref(owner, "length", "I")?)
            };
            self.assembly.code.push(op);
            return Ok(true);
        }
        let (array, index, value) = match inst.op {
            Op::ArrayGet { array, index } => (array, index, None),
            Op::ArraySet {
                array,
                index,
                value,
            } => (array, index, Some(value)),
            _ => return Ok(false),
        };
        let representation = self.types.get(self.body.value_type(array));
        let element = match representation {
            Some(Type::Array(e) | Type::Slice(e) | Type::Pointer(e)) => e,
            Some(Type::Str) => self.types.find(Type::Scalar(ScalarType::U8)).unwrap(),
            _ => return Err(error("invalid array/view representation")),
        };
        self.load(array)?;
        if matches!(representation, Some(Type::Pointer(_))) {
            self.load(index)?;
            self.assembly.code.push(Instruction::I2l);
            let owner = self.cp.add_class(POINTER_CLASS)?;
            let offset = self.cp.add_method_ref(
                owner,
                "offset",
                "(Lorg/rustlang/runtime/Pointer;J)Lorg/rustlang/runtime/Pointer;",
            )?;
            self.assembly.code.push(Instruction::Invokestatic(offset));
            if let Some(value) = value {
                self.argument(value)?;
                self.write_memory(element)?;
            } else {
                self.read_memory(element)?;
            }
            return Ok(true);
        }
        let view = matches!(representation, Some(Type::Slice(_) | Type::Str));
        if view {
            let owner = self.cp.add_class(representation::SLICE_VIEW_CLASS)?;
            self.assembly
                .code
                .push(Instruction::Getfield(self.cp.add_field_ref(
                    owner,
                    "array",
                    "Ljava/lang/Object;",
                )?));
            self.load(array)?;
            self.assembly.code.push(Instruction::Getfield(
                self.cp.add_field_ref(owner, "offset", "I")?,
            ));
        }
        self.load(index)?;
        if view {
            self.assembly.code.push(Instruction::Iadd);
        }
        if let Some(value) = value {
            self.argument(value)?;
        }
        use ScalarType::*;
        let (suffix, descriptor, read, write) = match self.types.get(element) {
            Some(Type::Scalar(Bool)) => ("Boolean", "Z", Instruction::Baload, Instruction::Bastore),
            Some(Type::Scalar(I8 | U8)) => ("I8", "B", Instruction::Baload, Instruction::Bastore),
            Some(Type::Scalar(I16 | F16)) => {
                ("I16", "S", Instruction::Saload, Instruction::Sastore)
            }
            Some(Type::Scalar(U16 | Char)) => {
                ("U16", "C", Instruction::Caload, Instruction::Castore)
            }
            Some(Type::Scalar(I32 | U32)) => {
                ("I32", "I", Instruction::Iaload, Instruction::Iastore)
            }
            Some(Type::Scalar(I64 | U64)) => {
                ("I64", "J", Instruction::Laload, Instruction::Lastore)
            }
            Some(Type::Scalar(F32)) => ("F32", "F", Instruction::Faload, Instruction::Fastore),
            Some(Type::Scalar(F64)) => ("F64", "D", Instruction::Daload, Instruction::Dastore),
            _ => (
                "Object",
                "Ljava/lang/Object;",
                Instruction::Aaload,
                Instruction::Aastore,
            ),
        };
        if view || (suffix == "Object" && value.is_none()) {
            let owner = self.cp.add_class(POINTER_CLASS)?;
            let name = if view {
                format!(
                    "slice{}{suffix}",
                    if value.is_some() { "Set" } else { "Get" }
                )
            } else {
                "arrayGetObject".into()
            };
            let signature = if value.is_some() {
                format!("(Ljava/lang/Object;I{descriptor})V")
            } else {
                format!("(Ljava/lang/Object;I){descriptor}")
            };
            let method = self.cp.add_method_ref(owner, name, signature)?;
            self.assembly.code.push(Instruction::Invokestatic(method));
            if suffix == "Object" && value.is_none() {
                let mut name = String::new();
                representation::descriptor(self.types, element, &mut name)?;
                let name = name
                    .strip_prefix('L')
                    .and_then(|s| s.strip_suffix(';'))
                    .unwrap_or(&name);
                self.assembly
                    .code
                    .push(Instruction::Checkcast(self.cp.add_class(name)?));
            }
        } else {
            self.assembly
                .code
                .push(if value.is_some() { write } else { read });
        }
        Ok(true)
    }
}
