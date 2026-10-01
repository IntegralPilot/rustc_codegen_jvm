//! Preserve field layout and primitive memory operations for SSA analysis.
use super::*;

fn getter(ty: &oomir::Type) -> Option<&'static str> {
    use oomir::Type::*;
    Some(match ty {
        Boolean => "getBoolean",
        I8 | U8 => "getI8",
        I16 | U16 | F16 => "getI16",
        I32 | U32 => "getI32",
        I64 | U64 => "getI64",
        F32 => "getF32",
        F64 => "getF64",
        _ => return None,
    })
}

impl Emission<'_> {
    pub(super) fn initial_cell(
        &mut self,
        owner: &str,
        name: &str,
        kind: ir::CallKind,
        returns: ir::TypeId,
        values: &[ir::ValueId],
    ) -> Option<ir::ValueId> {
        if owner != oomir::POINTER_CLASS
            || !matches!(name, "cell" | "cellAligned")
            || kind != ir::CallKind::JvmStatic
        {
            return None;
        }
        let ir::Type::Pointer(inner) = self.vocabulary.types.get(returns)? else {
            return None;
        };
        // Cell promotion requires every use to preserve the exact storage layout. Byte views and
        // escaped projections prevent promotion.
        let direct = match self.vocabulary.types.get(inner) {
            Some(ir::Type::Scalar(_)) => {
                ir::StorageSlot::scalar(inner, &self.vocabulary.types).is_some()
            }
            Some(
                ir::Type::Pointer(_) | ir::Type::Slice(_) | ir::Type::Str | ir::Type::Array(_),
            ) => true,
            Some(ir::Type::Class(name)) => self
                .context
                .fields
                .get(self.vocabulary.types.symbol_name(name)?)
                .is_some_and(|layout| layout.direct),
            _ => false,
        };
        if !direct {
            return None;
        }
        let integer = |value: ir::ValueId| {
            let value = self.builder.body.resolve(value);
            let ir::ValueDef::Inst(id) = self.builder.body.values[value.index()].def else {
                return None;
            };
            let ir::Op::Constant(id) = self.builder.body.instructions[id.index()].op else {
                return None;
            };
            let ir::Constant::Scalar(value) = self.builder.body.constants[id.index()] else {
                return None;
            };
            value.ty().integer()?;
            Some(value.bits())
        };
        // A positive, representable cell size guarantees fresh storage. Both
        // private cells need no byte layout once all loads/stores are promoted.
        let size = integer(*values.get(1)?)?;
        if size == 0 || size > i32::MAX as u128 {
            return None;
        }
        let scalar = ir::StorageSlot::scalar(inner, &self.vocabulary.types);
        if scalar.is_some_and(|slot| u128::from(slot.size) != size) {
            return None;
        }
        if name == "cellAligned" {
            let alignment = integer(*values.get(3)?)?;
            if alignment > i32::MAX as u128
                || !alignment.is_power_of_two()
                || (scalar.is_some() && alignment > 16)
            {
                return None;
            }
        }
        let initial = *values.first()?;
        if self.builder.body.value_type(initial) == inner {
            return Some(initial);
        }
        let value = self.builder.body.resolve(initial);
        let ir::ValueDef::Inst(id) = self.builder.body.values[value.index()].def else {
            return None;
        };
        let ir::Op::Constant(c) = self.builder.body.instructions[id.index()].op else {
            return None;
        };
        if !matches!(
            self.builder.body.constants[c.index()],
            ir::Constant::Null(_)
        ) {
            return None;
        }
        let constant = ir::ConstId::new(self.builder.body.constants.len());
        self.builder
            .body
            .constants
            .push(match self.vocabulary.types.get(inner) {
                Some(ir::Type::Scalar(ty)) => ir::Constant::Scalar(Scalar::from_bits(ty, 0)?),
                _ => ir::Constant::Null(inner),
            });
        self.emit(ir::Op::Constant(constant), Some(inner))
    }

    pub(super) fn pointer_operation(
        &mut self,
        name: &str,
        signature: &oomir::Signature,
        operand: &oomir::Operand,
        args: &[oomir::Operand],
        dest: &Option<String>,
    ) -> Result<bool> {
        if args.is_empty()
            && operand
                .get_type()
                .is_some_and(|t| matches!(t, oomir::Type::Pointer(_)))
            && matches!(name, "sliceBackingArray" | "sliceElementOffset")
        {
            let address = self.operand(operand.clone())?;
            let index = u8::from(name == "sliceElementOffset");
            let value = self
                .emit(
                    ir::Op::AddressViewPart { address, index },
                    Some(self.ty(&signature.ret)),
                )
                .unwrap();
            if let Some(dest) = dest {
                self.write(dest, value)?;
            }
            return Ok(true);
        }
        let thin = |operand: &oomir::Operand| {
            operand.get_type().is_some_and(|ty| {
                ty.scalar_address_size().is_some()
                    || matches!(ty, oomir::Type::Pointer(inner) if match inner.as_ref() {
                        oomir::Type::Class(name) => self.context.fields.get(name)
                            .is_some_and(|layout| layout.direct),
                        oomir::Type::Array(_) | oomir::Type::Pointer(_)
                            | oomir::Type::Slice(_) | oomir::Type::Str => true,
                        _ => false,
                    })
            })
        };
        if matches!(
            name,
            "samePointer"
                | "sameAddress"
                | "lessThan"
                | "lessOrEqual"
                | "greaterThan"
                | "greaterOrEqual"
        ) && thin(operand)
            && args.len() == 1
            && thin(&args[0])
        {
            let left = self.operand(operand.clone())?;
            let right = self.operand(args[0].clone())?;
            let boolean = self.ty(&oomir::Type::Boolean);
            let comparison = match name {
                "lessThan" => Some(jvm_compiler_core::scalar::BinaryOp::Lt),
                "lessOrEqual" => Some(jvm_compiler_core::scalar::BinaryOp::Le),
                "greaterThan" => Some(jvm_compiler_core::scalar::BinaryOp::Gt),
                "greaterOrEqual" => Some(jvm_compiler_core::scalar::BinaryOp::Ge),
                _ => None,
            };
            let value = if let Some(op) = comparison {
                let int = self.ty(&oomir::Type::I32);
                let order = self
                    .emit(ir::Op::AddressCompare { left, right }, Some(int))
                    .unwrap();
                let zero = self.constant(oomir::Constant::I32(0))?;
                self.emit(
                    ir::Op::Binary {
                        op,
                        left: order,
                        right: zero,
                    },
                    Some(boolean),
                )
                .unwrap()
            } else {
                self.emit(ir::Op::AddressEqual { left, right }, Some(boolean))
                    .unwrap()
            };
            if let Some(dest) = dest {
                self.write(dest, value)?;
            }
            return Ok(true);
        }
        let Some(oomir::Type::Pointer(pointee)) = operand.get_type() else {
            return Ok(false);
        };
        let direct_class = match pointee.as_ref() {
            oomir::Type::Class(name)
                if self
                    .context
                    .fields
                    .get(name)
                    .is_some_and(|layout| layout.direct) =>
            {
                Some(name.as_str())
            }
            _ => None,
        };
        let object_contents = direct_class.is_some()
            || matches!(
                pointee.as_ref(),
                oomir::Type::Pointer(_)
                    | oomir::Type::Slice(_)
                    | oomir::Type::Str
                    | oomir::Type::Array(_)
            );
        let view_class = match pointee.as_ref() {
            oomir::Type::Slice(_) => Some(oomir::SLICE_VIEW_CLASS),
            oomir::Type::Str => Some(oomir::UTF8_VIEW_CLASS),
            _ => direct_class,
        };
        let expected_getter = if object_contents {
            "getObject"
        } else if let Some(getter) = getter(&pointee) {
            getter
        } else {
            return Ok(false);
        };
        let owned = name == "getObjectCopyAs" && direct_class.is_some();
        let typed_view_getter = (name == "getObjectAs" || owned)
            && matches!(args,
            [oomir::Operand::Constant(oomir::Constant::String(class))]
                if view_class == Some(class.as_str()));
        if (direct_class.is_none() && name == expected_getter && args.is_empty())
            || typed_view_getter
        {
            let pointer = self.operand(operand.clone())?;
            let value = self
                .emit(
                    if owned {
                        ir::Op::LoadCopy(pointer)
                    } else {
                        ir::Op::Load(pointer)
                    },
                    Some(self.ty(&pointee)),
                )
                .unwrap();
            // Short getters and setters transfer f16 bits. They do not convert numeric values.
            let value = if pointee.as_ref() == &oomir::Type::F16 {
                self.emit(ir::Op::Reinterpret(value), Some(self.ty(&signature.ret)))
                    .unwrap()
            } else {
                self.adapt(value, self.ty(&signature.ret))?
            };
            if let Some(dest) = dest {
                self.write(dest, value)?;
            }
            return Ok(true);
        }
        if name == "set"
            && args.len() == 1
            && signature
                .explicit_jvm_params()
                .first()
                .is_some_and(|(_, ty)| {
                    if object_contents {
                        matches!(ty, oomir::Type::Class(name) if name == "java/lang/Object")
                    } else {
                        getter(ty) == Some(expected_getter)
                    }
                })
        {
            let pointer = self.operand(operand.clone())?;
            let value = self.operand(args[0].clone())?;
            let value = if pointee.as_ref() == &oomir::Type::F16 {
                self.emit(ir::Op::Reinterpret(value), Some(self.ty(&pointee)))
                    .unwrap()
            } else {
                self.adapt(value, self.ty(&pointee))?
            };
            self.emit(ir::Op::Store { pointer, value }, None);
            return Ok(true);
        }
        Ok(false)
    }
}
