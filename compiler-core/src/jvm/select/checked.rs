use super::*;

impl Selector<'_> {
    pub(super) fn overflow(&mut self, op: BinaryOp, args: List) -> jvm::Result<()> {
        let [a, b, wrapped] = self.body.args[args.range()] else {
            return Err(error("invalid checked operands"));
        };
        let ty = self.scalar_type(a)?;
        let (width, signed) = ty
            .integer()
            .ok_or_else(|| error("checked arithmetic requires integer"))?;
        if matches!(op, BinaryOp::Add | BinaryOp::Sub) {
            if !signed {
                self.load(if op == BinaryOp::Add { wrapped } else { a })?;
                self.load(if op == BinaryOp::Add { a } else { b })?;
                return self.comparison(BinaryOp::Lt, ty);
            }
            // Addition: ((a ^ sum) & (b ^ sum)) < 0.
            // Subtraction: ((a ^ b) & (a ^ difference)) < 0.
            self.load(a)?;
            self.load(if op == BinaryOp::Add { wrapped } else { b })?;
            self.assembly.code.push(if width == 64 {
                Instruction::Lxor
            } else {
                Instruction::Ixor
            });
            self.load(if op == BinaryOp::Add { b } else { a })?;
            self.load(wrapped)?;
            self.assembly.code.push(if width == 64 {
                Instruction::Lxor
            } else {
                Instruction::Ixor
            });
            self.assembly.code.push(if width == 64 {
                Instruction::Land
            } else {
                Instruction::Iand
            });
            self.constant(Scalar::integer(ty, 0).unwrap())?;
            return self.comparison(BinaryOp::Lt, ty);
        }
        if op != BinaryOp::Mul {
            return Err(error("unsupported checked operation"));
        }
        let no = self.assembly.label();
        let yes = self.assembly.label();
        let end = self.assembly.label();
        self.load(b)?;
        self.constant(Scalar::integer(ty, 0).unwrap())?;
        self.integer_equal_jump(ty, no, true)?;
        if signed {
            let ordinary = self.assembly.label();
            self.load(a)?;
            self.constant(Scalar::integer(ty, 1u128 << (width - 1)).unwrap())?;
            self.integer_equal_jump(ty, ordinary, false)?;
            self.load(b)?;
            self.constant(Scalar::integer(ty, u128::MAX).unwrap())?;
            self.integer_equal_jump(ty, yes, true)?;
            self.assembly.bind(ordinary);
        }
        // The divisor is nonzero. Keep this division local to selection so
        // the semantic overflow flag remains pure and can be discarded.
        self.load(wrapped)?;
        self.load(b)?;
        self.binary(BinaryOp::Div, ty, ty)?;
        self.load(a)?;
        self.integer_equal_jump(ty, yes, false)?;
        self.assembly.bind(no);
        self.assembly.code.push(Instruction::Iconst_0);
        self.assembly.branch(Instruction::Goto_w(0), end);
        self.assembly.bind(yes);
        self.assembly.code.push(Instruction::Iconst_1);
        self.assembly.bind(end);
        Ok(())
    }

    fn integer_equal_jump(
        &mut self,
        ty: ScalarType,
        target: Label,
        equal: bool,
    ) -> jvm::Result<()> {
        let branch = if kind(ty)? == Kind::Long {
            self.assembly.code.push(Instruction::Lcmp);
            if equal {
                Instruction::Ifeq(0)
            } else {
                Instruction::Ifne(0)
            }
        } else if equal {
            Instruction::If_icmpeq(0)
        } else {
            Instruction::If_icmpne(0)
        };
        self.assembly.branch(branch, target);
        Ok(())
    }
}
