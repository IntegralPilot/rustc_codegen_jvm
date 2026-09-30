use super::*;
use crate::scalar::{BinaryFold, Scalar, fold_binary};

pub(crate) enum Folded {
    Value(ValueId),
    Constant(Scalar),
}

impl Body {
    pub(crate) fn scalar_value(&self, value: ValueId) -> Option<Scalar> {
        let value = self.resolve(value);
        let ValueDef::Inst(inst) = self.values[value.index()].def else {
            return None;
        };
        let Op::Constant(constant) = self.instructions[inst.index()].op else {
            return None;
        };
        let Constant::Scalar(scalar) = self.constants[constant.index()] else {
            return None;
        };
        Some(scalar)
    }

    pub(crate) fn fold(&self, types: &Types, op: Op, result: TypeId) -> Option<Folded> {
        if let Op::Length(view) = op {
            let view = self.resolve(view);
            if let ValueDef::Inst(inst) = self.values[view.index()].def
                && let Op::View { length, .. } = self.instructions[inst.index()].op
                && self.value_type(length) == result
            {
                return Some(Folded::Value(length));
            }
        }
        let Some(Type::Scalar(result_ty)) = types.get(result) else {
            return None;
        };
        let constant = match op {
            Op::Binary { op, left, right } => {
                let Some(Type::Scalar(left_ty)) = types.get(self.value_type(left)) else {
                    return None;
                };
                let Some(Type::Scalar(right_ty)) = types.get(self.value_type(right)) else {
                    return None;
                };
                let result = fold_binary(
                    op,
                    left_ty,
                    right_ty,
                    self.scalar_value(left),
                    self.scalar_value(right),
                    || self.resolve(left) == self.resolve(right),
                )?;
                match result {
                    BinaryFold::Left if result_ty == left_ty => return Some(Folded::Value(left)),
                    BinaryFold::Right if result_ty == right_ty => {
                        return Some(Folded::Value(right));
                    }
                    BinaryFold::Constant(value) => value,
                    _ => return None,
                }
            }
            Op::Reinterpret(value) => {
                // Full JVM integer-width signedness is only an annotation.
                // Narrow int carriers can have different extension semantics.
                let value = self.scalar_value(value)?;
                let (width, _) = value.ty().integer()?;
                if !matches!(width, 32 | 64) || result_ty.integer()?.0 != width {
                    return None;
                }
                value.cast(result_ty)?
            }
            Op::Not(value) => self.scalar_value(value)?.not()?,
            Op::Bit { op, value } => self.scalar_value(value)?.bit(op)?,
            Op::Overflow { op, args } => {
                let args = &self.args[args.range()];
                let a = self.scalar_value(args[0])?;
                let b = self.scalar_value(args[1])?;
                Scalar::boolean(a.overflows(op, b)?)
            }
            Op::Neg(value) => self.scalar_value(value)?.neg()?,
            Op::Cast(value) => {
                if self.value_type(value) == result {
                    return Some(Folded::Value(value));
                }
                self.scalar_value(value)?.cast(result_ty)?
            }
            _ => return None,
        };
        (constant.ty() == result_ty).then_some(Folded::Constant(constant))
    }
}

impl Builder<'_> {
    pub(super) fn scalar_value(&self, value: ValueId) -> Option<Scalar> {
        self.body.scalar_value(value)
    }
    pub(super) fn fold(&mut self, op: Op, result: TypeId) -> Option<ValueId> {
        match self.body.fold(self.types, op, result)? {
            Folded::Value(value) => Some(value),
            Folded::Constant(value) => Some(self.constant(result, value)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scalar::{BinaryOp, ScalarType};

    #[test]
    fn folds_during_construction_without_erasing_traps_or_float_semantics() {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let float = types.scalar(ScalarType::F32);
        let mut b = Builder::new(&types, int);
        let x = b.parameter(b.current(), int);
        let f = b.parameter(b.current(), float);
        let zero = b.constant(int, Scalar::integer(ScalarType::I32, 0).unwrap());
        let one = b.constant(int, Scalar::integer(ScalarType::I32, 1).unwrap());
        let add = b
            .emit(
                Op::Binary {
                    op: BinaryOp::Add,
                    left: x,
                    right: zero,
                },
                Some(int),
            )
            .unwrap();
        assert_eq!(add, x);
        let cast = b.emit(Op::Cast(x), Some(int)).unwrap();
        assert_eq!(cast, x);
        let count = b.body.instructions.len();
        let folded = b
            .emit(
                Op::Binary {
                    op: BinaryOp::Add,
                    left: zero,
                    right: one,
                },
                Some(int),
            )
            .unwrap();
        assert_eq!(folded, one);
        assert_eq!(b.body.instructions.len(), count);
        let trapping = b
            .emit(
                Op::Binary {
                    op: BinaryOp::Div,
                    left: zero,
                    right: x,
                },
                Some(int),
            )
            .unwrap();
        assert_ne!(trapping, zero);
        let float_zero = b.constant(float, Scalar::f32(0.0));
        let float_add = b
            .emit(
                Op::Binary {
                    op: BinaryOp::Add,
                    left: f,
                    right: float_zero,
                },
                Some(float),
            )
            .unwrap();
        assert_ne!(float_add, f);
        b.terminate(Terminator::Return(Some(x)));
        b.finish().unwrap();
    }
}
