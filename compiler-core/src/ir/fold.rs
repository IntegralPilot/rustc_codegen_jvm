use super::*;
use crate::scalar::{BinaryFold, Scalar, fold_binary};

impl Builder<'_> {
    pub(super) fn scalar_value(&self, value: ValueId) -> Option<Scalar> {
        let value = self.body.resolve(value);
        let ValueDef::Inst(inst) = self.body.values[value.index()].def else {
            return None;
        };
        let Op::Constant(constant) = self.body.instructions[inst.index()].op else {
            return None;
        };
        let Constant::Scalar(scalar) = self.body.constants[constant.index()] else {
            return None;
        };
        Some(scalar)
    }

    pub(super) fn fold(&mut self, op: Op, result: TypeId) -> Option<ValueId> {
        if let Op::Length(view) = op {
            let view = self.body.resolve(view);
            if let ValueDef::Inst(inst) = self.body.values[view.index()].def
                && let Op::View { length, .. } = self.body.instructions[inst.index()].op
                && self.body.value_type(length) == result
            {
                return Some(length);
            }
        }
        let Some(Type::Scalar(result_ty)) = self.types.get(result) else {
            return None;
        };
        let constant = match op {
            Op::Binary { op, left, right } => {
                let Some(Type::Scalar(left_ty)) = self.types.get(self.body.value_type(left)) else {
                    return None;
                };
                let Some(Type::Scalar(right_ty)) = self.types.get(self.body.value_type(right))
                else {
                    return None;
                };
                let result = fold_binary(
                    op,
                    left_ty,
                    right_ty,
                    self.scalar_value(left),
                    self.scalar_value(right),
                    || self.body.resolve(left) == self.body.resolve(right),
                )?;
                match result {
                    BinaryFold::Left if result_ty == left_ty => return Some(left),
                    BinaryFold::Right if result_ty == right_ty => return Some(right),
                    BinaryFold::Constant(value) => value,
                    _ => return None,
                }
            }
            Op::Not(value) => self.scalar_value(value)?.not()?,
            Op::Bit { op, value } => self.scalar_value(value)?.bit(op)?,
            Op::Overflow { op, args } => {
                let args = &self.body.args[args.range()];
                let a = self.scalar_value(args[0])?;
                let b = self.scalar_value(args[1])?;
                Scalar::boolean(a.overflows(op, b)?)
            }
            Op::Neg(value) => self.scalar_value(value)?.neg()?,
            Op::Cast(value) => {
                if self.body.value_type(value) == result {
                    return Some(value);
                }
                self.scalar_value(value)?.cast(result_ty)?
            }
            _ => return None,
        };
        (constant.ty() == result_ty).then(|| self.constant(result, constant))
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
