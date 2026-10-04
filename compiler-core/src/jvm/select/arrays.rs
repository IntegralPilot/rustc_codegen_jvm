//! Array operations preserve lazy repeat copies and pointer-backed slice views.
use super::*;
#[derive(Clone, Copy, PartialEq, Eq)]
enum Access {
    Native,
    Array,
    View,
    Owned,
}

impl Selector<'_> {
    pub(super) fn array(&mut self, id: InstId, inst: Inst) -> jvm::Result<bool> {
        if let Op::ArrayFill { array, value } = inst.op {
            let element = self.body.value_type(value);
            let native = self.native_array_accesses.get(id.index()) == Some(&Some(element));
            self.load(array)?;
            self.argument(value)?;
            let mut descriptor = String::from("(");
            representation::descriptor(self.types, self.body.value_type(array), &mut descriptor)?;
            representation::descriptor(self.types, element, &mut descriptor)?;
            descriptor.push_str(")V");
            let owner = self.cp.add_class(if native {
                "java/util/Arrays"
            } else {
                POINTER_CLASS
            })?;
            let target = self.cp.add_method_ref(
                owner,
                if native { "fill" } else { "fillArray" },
                descriptor,
            )?;
            self.assembly.code.push(Instruction::Invokestatic(target));
            return Ok(true);
        }
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
        if let Op::ViewGet(parts) | Op::ViewGetCopy(parts) | Op::ViewSet { parts, .. } = inst.op {
            let [backing, start, index] = self.body.args[parts.range()] else {
                return Err(error("slice access components"));
            };
            let value = match inst.op {
                Op::ViewSet { value, .. } => Some(value),
                _ => None,
            };
            let element = self.body.value_type(value.or(inst.result).unwrap());
            let native = self.native_array_accesses.get(id.index()) == Some(&Some(element));
            self.load(backing)?;
            if native {
                let mut descriptor = String::from("[");
                representation::descriptor(self.types, element, &mut descriptor)?;
                self.assembly
                    .code
                    .push(Instruction::Checkcast(self.cp.add_class(&descriptor)?));
            }
            self.load(start)?;
            self.load(index)?;
            self.assembly.code.push(Instruction::Iadd);
            if let Some(value) = value {
                self.argument(value)?;
            }
            self.array_access(
                element,
                value.is_some(),
                if matches!(inst.op, Op::ViewGetCopy(_)) {
                    Access::Owned
                } else if native {
                    Access::Native
                } else {
                    Access::View
                },
            )?;
            return Ok(true);
        }
        let (array, index, value, native) = match inst.op {
            Op::ArrayGet {
                array,
                index,
                native,
            } => (array, index, None, native),
            Op::ArrayGetCopy { array, index } => (array, index, None, false),
            Op::ArraySet {
                array,
                index,
                value,
                native,
            } => (array, index, Some(value), native),
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
            } else if matches!(inst.op, Op::ArrayGetCopy { .. }) {
                self.assembly.code.push(Instruction::Lconst_0);
                self.read_object_address(element, true)?;
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
        let native = native || self.native_array_accesses.get(id.index()) == Some(&Some(element));
        let primitive = matches!(self.types.get(element), Some(Type::Scalar(_)));
        // A raw pointer can expose this array to a decoded aggregate alias later.
        // Only a whole-lifetime proof can remove coherence checks.
        self.array_access(
            element,
            value.is_some(),
            if matches!(inst.op, Op::ArrayGetCopy { .. }) {
                Access::Owned
            } else if native {
                Access::Native
            } else if view || primitive {
                Access::View
            } else {
                Access::Array
            },
        )?;
        Ok(true)
    }

    fn array_access(&mut self, element: TypeId, store: bool, access: Access) -> jvm::Result<()> {
        use ScalarType::*;
        if access == Access::Owned {
            if let Some(bootstrap) = &mut self.bootstrap {
                let owner = self.cp.add_class("org/rustlang/runtime/OwnedReads")?;
                let method = self.cp.add_method_ref(
                    owner,
                    "bootstrap",
                    concat!(
                        "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;",
                        "Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
                    ),
                )?;
                let bootstrap_method_ref = self
                    .cp
                    .add_method_handle(jvm::ReferenceKind::InvokeStatic, method)?;
                let index = u16::try_from(bootstrap.len())?;
                bootstrap.push(jvm::attributes::BootstrapMethod {
                    bootstrap_method_ref,
                    arguments: vec![],
                });
                let mut descriptor = String::from("(Ljava/lang/Object;I)");
                representation::descriptor(self.types, element, &mut descriptor)?;
                let site = self.cp.add_invoke_dynamic(index, "read", descriptor)?;
                self.assembly.code.push(Instruction::Invokedynamic(site));
                return Ok(());
            }
            let name = self.address_target(element)?;
            let owner = self.cp.add_class(POINTER_CLASS)?;
            let method = self.cp.add_method_ref(
                owner,
                "sliceGetObjectCopy",
                "(Ljava/lang/Object;ILjava/lang/String;)Ljava/lang/Object;",
            )?;
            self.assembly.code.push(Instruction::Invokestatic(method));
            self.assembly
                .code
                .push(Instruction::Checkcast(self.cp.add_class(&name)?));
            return Ok(());
        }
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
        if access == Access::View || (access == Access::Array && suffix == "Object" && !store) {
            let owner = self.cp.add_class(POINTER_CLASS)?;
            let name = if access == Access::View {
                format!("slice{}{suffix}", if store { "Set" } else { "Get" })
            } else {
                "arrayGetObject".into()
            };
            let signature = if store {
                format!("(Ljava/lang/Object;I{descriptor})V")
            } else {
                format!("(Ljava/lang/Object;I){descriptor}")
            };
            let method = self.cp.add_method_ref(owner, name, signature)?;
            self.assembly.code.push(Instruction::Invokestatic(method));
            if suffix == "Object" && !store {
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
            self.assembly.code.push(if store { write } else { read });
        }
        Ok(())
    }
}
