//! Preserve field layout and primitive memory operations for SSA analysis.
use super::*;

fn getter(ty: &oomir::Type) -> Option<&'static str> {
    use oomir::Type::*;
    Some(match ty {
        Boolean => "getBoolean",
        I8 | U8 => "getI8",
        I16 | U16 => "getI16",
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
        // Pointer contents have value semantics; aggregate cells can also carry
        // mutable decoded views and need a separate escape analysis.
        if !matches!(self.vocabulary.types.get(inner), Some(ir::Type::Pointer(_))) {
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
        // thin and fat pointer values are immutable Pointer carriers; their
        // private cells need no byte layout once all loads/stores are promoted.
        let size = integer(*values.get(1)?)?;
        if size == 0 || size > i32::MAX as u128 {
            return None;
        }
        if name == "cellAligned" {
            let alignment = integer(*values.get(3)?)?;
            if alignment > i32::MAX as u128 || !alignment.is_power_of_two() {
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
        self.builder.body.constants.push(ir::Constant::Null(inner));
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
        let Some(oomir::Type::Pointer(pointee)) = operand.get_type() else {
            return Ok(false);
        };
        if name == "projectStructField" {
            let [
                oomir::Operand::Constant(oomir::Constant::String(owner)),
                oomir::Operand::Constant(oomir::Constant::String(field)),
                oomir::Operand::Constant(oomir::Constant::U64(offset)),
                oomir::Operand::Constant(oomir::Constant::U64(size)),
                codec,
            ] = args
            else {
                return Ok(false);
            };
            let Some((_, field_ty)) = self
                .context
                .fields
                .get(owner)
                .filter(|layout| layout.direct)
                .and_then(|layout| layout.members.iter().find(|(n, _)| n == field))
            else {
                return Ok(false);
            };
            if !matches!(pointee.as_ref(), oomir::Type::Class(name) if name == owner)
                || !matches!(signature.ret.as_ref(), oomir::Type::Pointer(ty) if ty.as_ref() == field_ty)
            {
                return Ok(false);
            }
            let codec = match codec {
                oomir::Operand::Constant(oomir::Constant::String(codec)) => Some(codec.clone()),
                oomir::Operand::Constant(oomir::Constant::Null(_)) => None,
                _ => return Ok(false),
            };
            let field = self.builder.field(ir::FieldRef {
                owner: self.ty(&pointee),
                name: field.clone(),
                ty: self.ty(field_ty),
                is_static: false,
                relative_pointer: matches!(field_ty, oomir::Type::Pointer(_)),
            });
            let projection = self.builder.projection(ir::PointerProjection {
                field,
                offset: *offset,
                size: *size,
                codec,
            });
            let base = self.operand(operand.clone())?;
            let value = self
                .emit(
                    ir::Op::Project { base, projection },
                    Some(self.ty(&signature.ret)),
                )
                .unwrap();
            if let Some(dest) = dest {
                self.write(dest, value)?;
            }
            return Ok(true);
        }
        let pointer_contents = matches!(pointee.as_ref(), oomir::Type::Pointer(_));
        let expected_getter = if pointer_contents {
            "getObject"
        } else if let Some(getter) = getter(&pointee) {
            getter
        } else {
            return Ok(false);
        };
        if name == expected_getter && args.is_empty() {
            let pointer = self.operand(operand.clone())?;
            let value = self
                .emit(ir::Op::Load(pointer), Some(self.ty(&pointee)))
                .unwrap();
            let value = self.adapt(value, self.ty(&signature.ret))?;
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
                    if pointer_contents {
                        matches!(ty, oomir::Type::Class(name) if name == "java/lang/Object")
                    } else {
                        getter(ty) == Some(expected_getter)
                    }
                })
        {
            let pointer = self.operand(operand.clone())?;
            let value = self.operand(args[0].clone())?;
            let value = self.adapt(value, self.ty(&pointee))?;
            self.emit(ir::Op::Store { pointer, value }, None);
            return Ok(true);
        }
        Ok(false)
    }
}
