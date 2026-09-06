//! Primitive operations remain native SSA; wide carriers use their runtime ABI.
use super::*;
use ir::{CallKind, Op};
use oomir::BinaryOp;
impl Emission<'_> {
    pub(super) fn binary(
        &mut self,
        dest: &str,
        op: BinaryOp,
        left: oomir::Operand,
        right: oomir::Operand,
    ) -> Result<()> {
        let left_ty = left.get_type().unwrap();
        let right_ty = right.get_type().unwrap();
        if !left_ty.has_jvm_value() && !right_ty.has_jvm_value() && op.is_comparison() {
            let value = self.constant(oomir::Constant::Boolean(matches!(
                op,
                BinaryOp::Eq | BinaryOp::Le | BinaryOp::Ge
            )))?;
            return self.write(dest, value);
        }
        let null = matches!(&left, oomir::Operand::Constant(oomir::Constant::Null(_)))
            || matches!(&right, oomir::Operand::Constant(oomir::Constant::Null(_)));
        let mut left = self.operand(left)?;
        let mut right = self.operand(right)?;
        let wide = |ty: &oomir::Type| matches!(ty,oomir::Type::Class(c) if c=="org/rustlang/runtime/I128" || c=="org/rustlang/runtime/U128" || c=="org/rustlang/runtime/F128");
        let shift = matches!(op, BinaryOp::Shl | BinaryOp::Shr);
        let ty = if shift || wide(&left_ty) {
            &left_ty
        } else if wide(&right_ty) {
            &right_ty
        } else {
            &left_ty
        };
        let result = if let oomir::Type::Class(owner) = ty
            && wide(ty)
        {
            left = self.adapt(left, self.ty(ty))?;
            let right_type = self.ty(if shift { &oomir::Type::I32 } else { ty });
            right = self.adapt(right, right_type)?;
            let name = match op {
                BinaryOp::Add => "add",
                BinaryOp::Sub => "subtract",
                BinaryOp::Mul => "multiply",
                BinaryOp::Div => "divide",
                BinaryOp::Rem => "remainder",
                BinaryOp::BitAnd => "and",
                BinaryOp::BitOr => "or",
                BinaryOp::BitXor => "xor",
                BinaryOp::Shl => "shiftLeft",
                BinaryOp::Shr => "shiftRight",
                BinaryOp::Eq => "eq",
                BinaryOp::Ne => "ne",
                BinaryOp::Lt => "lt",
                BinaryOp::Le => "le",
                BinaryOp::Gt => "gt",
                BinaryOp::Ge => "ge",
            };
            let returns = self.ty(if op.is_comparison() {
                &oomir::Type::Boolean
            } else {
                ty
            });
            if op.is_comparison() && owner != "org/rustlang/runtime/F128" {
                let order = self
                    .call(
                        owner.clone(),
                        "compareTo".into(),
                        vec![right_type],
                        self.ty(&oomir::Type::I32),
                        CallKind::Virtual,
                        vec![left, right],
                    )?
                    .unwrap();
                let zero = self.constant(oomir::Constant::I32(0))?;
                self.emit(
                    Op::Binary {
                        op,
                        left: order,
                        right: zero,
                    },
                    Some(returns),
                )
                .unwrap()
            } else {
                self.call(
                    owner.clone(),
                    name.into(),
                    vec![right_type],
                    returns,
                    CallKind::Virtual,
                    vec![left, right],
                )?
                .unwrap()
            }
        } else if *ty == oomir::Type::F16 {
            let name = match op {
                BinaryOp::Add => "f16Add",
                BinaryOp::Sub => "f16Sub",
                BinaryOp::Mul => "f16Mul",
                BinaryOp::Div => "f16Div",
                BinaryOp::Rem => "f16Rem",
                BinaryOp::Eq => "f16Eq",
                BinaryOp::Ne => "f16Ne",
                BinaryOp::Lt => "f16Lt",
                BinaryOp::Le => "f16Le",
                BinaryOp::Gt => "f16Gt",
                BinaryOp::Ge => "f16Ge",
                _ => return Err("invalid f16 operation".into()),
            };
            let returns = self.ty(if op.is_comparison() {
                &oomir::Type::Boolean
            } else {
                ty
            });
            self.call(
                "org/rustlang/runtime/Numbers".into(),
                name.into(),
                vec![self.ty(ty); 2],
                returns,
                CallKind::JvmStatic,
                vec![left, right],
            )?
            .unwrap()
        } else if self
            .vocabulary
            .types
            .get(self.builder.body.value_type(left))
            .unwrap()
            .carrier()
            == 5
        {
            if !matches!(op, BinaryOp::Eq | BinaryOp::Ne) {
                return Err(format!("non-equality operation {op:?} on {ty:?}"));
            }
            if *ty == oomir::Type::Str && !null {
                let equal = self
                    .call(
                        oomir::UTF8_VIEW_CLASS.into(),
                        "equals".into(),
                        vec![self.ty(ty); 2],
                        self.ty(&oomir::Type::Boolean),
                        CallKind::JvmStatic,
                        vec![left, right],
                    )?
                    .unwrap();
                if op == BinaryOp::Ne {
                    self.emit(Op::Not(equal), Some(self.ty(&oomir::Type::Boolean)))
                        .unwrap()
                } else {
                    equal
                }
            } else {
                self.emit(
                    Op::Binary { op, left, right },
                    Some(self.ty(&oomir::Type::Boolean)),
                )
                .unwrap()
            }
        } else {
            if shift && wide(&right_ty) {
                right = self.adapt(right, self.ty(&oomir::Type::I32))?;
            } else if !shift {
                right = self.adapt(right, self.builder.body.value_type(left))?;
            }
            let returns = if op.is_comparison() {
                self.ty(&oomir::Type::Boolean)
            } else {
                self.builder.body.value_type(left)
            };
            self.emit(Op::Binary { op, left, right }, Some(returns))
                .unwrap()
        };
        self.write(dest, result)
    }
    pub(super) fn unary(&mut self, dest: &str, src: oomir::Operand, not: bool) -> Result<()> {
        let ty = src.get_type().unwrap();
        let value = self.operand(src)?;
        let result = if let oomir::Type::Class(owner) = &ty {
            self.call(
                owner.clone(),
                if not { "not" } else { "negate" }.into(),
                vec![],
                self.ty(&ty),
                CallKind::Virtual,
                vec![value],
            )?
            .unwrap()
        } else if ty == oomir::Type::F16 && !not {
            self.call(
                "org/rustlang/runtime/Numbers".into(),
                "f16Neg".into(),
                vec![self.ty(&ty)],
                self.ty(&ty),
                CallKind::JvmStatic,
                vec![value],
            )?
            .unwrap()
        } else {
            self.emit(
                if not { Op::Not(value) } else { Op::Neg(value) },
                Some(self.ty(&ty)),
            )
            .unwrap()
        };
        self.write(dest, result)
    }
}
