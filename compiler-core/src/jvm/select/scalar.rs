use super::*;

impl Selector<'_> {
    pub(super) fn instruction(&mut self, id: InstId) -> jvm::Result<()> {
        let inst = self.body.instructions[id.index()];
        if inst.result.is_some_and(|v| literal(self.body, v).is_some()) {
            return Ok(());
        }
        if let Op::Constant(id) = inst.op {
            if let Constant::External { index, .. } = self.body.constants[id.index()] {
                self.constants
                    .ok_or_else(|| error("missing representation constant pool"))?
                    .emit(index, &mut self.assembly.code, self.cp)?;
                let result = inst.result.unwrap();
                self.normalize_value(result);
                return self.finish_result(result);
            }
        }
        if let Op::Call { method, kind, args } = inst.op {
            self.call(method, kind, args)?;
            if let Some(result) = inst.result {
                self.normalize_value(result);
                self.finish_result(result)?;
            }
            return Ok(());
        }
        if self.general(inst)?
            || self.array(inst)?
            || self.memory(inst)?
            || self.object(inst)?
            || self.view(inst)?
        {
            if let Some(result) = inst.result {
                self.normalize_value(result);
                self.finish_result(result)?;
            }
            return Ok(());
        }
        let result = inst
            .result
            .ok_or_else(|| error("operation requires a result"))?;
        let ty = self.scalar_type(result)?;
        match inst.op {
            Op::Constant(id) => match self.body.constants[id.index()] {
                Constant::Scalar(value) => self.constant(value)?,
                _ => return Err(error("non-scalar constant")),
            },
            Op::Binary { op, left, right } => {
                self.load(left)?;
                self.load(right)?;
                let left_ty = self.scalar_type(left)?;
                if op.is_comparison() {
                    self.comparison(op, left_ty)?;
                } else {
                    self.binary(op, left_ty, self.scalar_type(right)?)?;
                }
            }
            Op::Overflow { op, args } => self.overflow(op, args)?,
            Op::Bit { op, value } => self.bit(op, value)?,
            Op::Neg(value) => {
                self.load(value)?;
                self.assembly.code.push(match kind(ty)? {
                    Kind::Int => Instruction::Ineg,
                    Kind::Long => Instruction::Lneg,
                    Kind::Float => Instruction::Fneg,
                    Kind::Double => Instruction::Dneg,
                    _ => unreachable!(),
                });
                self.normalize(ty);
            }
            Op::Not(value) => {
                self.load(value)?;
                if kind(ty)? == Kind::Long {
                    self.assembly
                        .code
                        .extend([get_long_const_instr(self.cp, -1), Instruction::Lxor]);
                } else {
                    self.assembly.code.extend([
                        get_int_const_instr(self.cp, if ty == ScalarType::Bool { 1 } else { -1 }),
                        Instruction::Ixor,
                    ]);
                }
                self.normalize(ty);
            }
            Op::Cast(value) => {
                self.load(value)?;
                self.cast(self.scalar_type(value)?, ty)?;
            }
            _ => return Err(error("operation not yet selected")),
        }
        self.finish_result(result)
    }
    fn finish_result(&mut self, result: ValueId) -> jvm::Result<()> {
        if self.live.values[result.index()] {
            self.store(result)
        } else {
            self.assembly
                .code
                .push(if self.value_kind(result)?.width() == 2 {
                    Instruction::Pop2
                } else {
                    Instruction::Pop
                });
            Ok(())
        }
    }
    pub(super) fn constant(&mut self, value: Scalar) -> jvm::Result<()> {
        let bits = value.bits();
        self.assembly.code.push(match kind(value.ty())? {
            Kind::Int => {
                get_int_const_instr(self.cp, value.signed().unwrap_or(bits as i128) as i32)
            }
            Kind::Long => get_long_const_instr(self.cp, bits as i64),
            Kind::Float => get_float_const_instr(self.cp, f32::from_bits(bits as u32)),
            Kind::Double => get_double_const_instr(self.cp, f64::from_bits(bits as u64)),
            _ => unreachable!(),
        });
        Ok(())
    }
    pub(super) fn normalize(&mut self, ty: ScalarType) {
        use ScalarType::*;
        match ty {
            I8 => self.assembly.code.push(Instruction::I2b),
            I16 => self.assembly.code.push(Instruction::I2s),
            U8 => self
                .assembly
                .code
                .extend([get_int_const_instr(self.cp, 255), Instruction::Iand]),
            U16 => self.assembly.code.push(Instruction::I2c),
            _ => {}
        }
    }
    pub(super) fn binary(
        &mut self,
        op: BinaryOp,
        ty: ScalarType,
        rhs: ScalarType,
    ) -> jvm::Result<()> {
        use BinaryOp::*;
        use Instruction as I;
        let kind = kind(ty)?;
        if matches!(op, Shl | Shr) {
            if slots::kind(rhs)? == Kind::Long {
                self.assembly.code.push(I::L2i);
            }
            let (width, signed) = ty.integer().ok_or_else(|| error("non-integer shift"))?;
            self.assembly
                .code
                .extend([get_int_const_instr(self.cp, (width - 1) as i32), I::Iand]);
            self.assembly.code.push(match (op, kind, signed) {
                (Shl, Kind::Int, _) => I::Ishl,
                (Shl, Kind::Long, _) => I::Lshl,
                (Shr, Kind::Int, true) => I::Ishr,
                (Shr, Kind::Int, false) => I::Iushr,
                (Shr, Kind::Long, true) => I::Lshr,
                (Shr, Kind::Long, false) => I::Lushr,
                _ => return Err(error("invalid shift")),
            });
        } else if matches!(op, Div | Rem) && matches!(ty, ScalarType::U32 | ScalarType::U64) {
            self.unsigned_call(
                ty,
                if op == Div {
                    "divideUnsigned"
                } else {
                    "remainderUnsigned"
                },
                false,
            )?;
        } else {
            macro_rules! numeric { ($($op:ident: $i:ident, $l:ident, $f:ident, $d:ident);*) => {
                match (op, kind) { $((BinaryOp::$op, Kind::Int) => Some(I::$i),
                    (BinaryOp::$op, Kind::Long) => Some(I::$l), (BinaryOp::$op, Kind::Float) => Some(I::$f),
                    (BinaryOp::$op, Kind::Double) => Some(I::$d),)*
                    (BitAnd, Kind::Int) => Some(I::Iand), (BitAnd, Kind::Long) => Some(I::Land),
                    (BitOr, Kind::Int) => Some(I::Ior), (BitOr, Kind::Long) => Some(I::Lor),
                    (BitXor, Kind::Int) => Some(I::Ixor), (BitXor, Kind::Long) => Some(I::Lxor), _ => None }
            }; }
            let instruction = numeric! { Add: Iadd, Ladd, Fadd, Dadd; Sub: Isub, Lsub, Fsub, Dsub;
            Mul: Imul, Lmul, Fmul, Dmul; Div: Idiv, Ldiv, Fdiv, Ddiv; Rem: Irem, Lrem, Frem, Drem };
            self.assembly
                .code
                .push(instruction.ok_or_else(|| error("invalid scalar operation"))?);
        }
        self.normalize(ty);
        Ok(())
    }
    fn unsigned_call(&mut self, ty: ScalarType, name: &str, comparison: bool) -> jvm::Result<()> {
        let long = ty == ScalarType::U64;
        let class = self.cp.add_class(if long {
            "java/lang/Long"
        } else {
            "java/lang/Integer"
        })?;
        let descriptor = if long {
            if comparison { "(JJ)I" } else { "(JJ)J" }
        } else {
            "(II)I"
        };
        self.assembly.code.push(Instruction::Invokestatic(
            self.cp.add_method_ref(class, name, descriptor)?,
        ));
        Ok(())
    }
    pub(super) fn comparison(&mut self, op: BinaryOp, ty: ScalarType) -> jvm::Result<()> {
        use BinaryOp::*;
        use Instruction as I;
        let direct = kind(ty)? == Kind::Int && ty != ScalarType::U32;
        if !direct {
            if matches!(ty, ScalarType::U32 | ScalarType::U64) {
                self.unsigned_call(ty, "compareUnsigned", true)?;
            } else {
                self.assembly.code.push(match kind(ty)? {
                    Kind::Long => I::Lcmp,
                    Kind::Float => {
                        if matches!(op, Lt | Le) {
                            I::Fcmpg
                        } else {
                            I::Fcmpl
                        }
                    }
                    Kind::Double => {
                        if matches!(op, Lt | Le) {
                            I::Dcmpg
                        } else {
                            I::Dcmpl
                        }
                    }
                    _ => unreachable!(),
                });
            }
        }
        let yes = self.assembly.label();
        let end = self.assembly.label();
        let branch = match (op, direct) {
            (Eq, true) => I::If_icmpeq(0),
            (Ne, true) => I::If_icmpne(0),
            (Lt, true) => I::If_icmplt(0),
            (Le, true) => I::If_icmple(0),
            (Gt, true) => I::If_icmpgt(0),
            (Ge, true) => I::If_icmpge(0),
            (Eq, false) => I::Ifeq(0),
            (Ne, false) => I::Ifne(0),
            (Lt, false) => I::Iflt(0),
            (Le, false) => I::Ifle(0),
            (Gt, false) => I::Ifgt(0),
            (Ge, false) => I::Ifge(0),
            _ => unreachable!(),
        };
        self.assembly.branch(branch, yes);
        self.assembly.code.push(I::Iconst_0);
        self.assembly.branch(I::Goto_w(0), end);
        self.assembly.bind(yes);
        self.assembly.code.push(I::Iconst_1);
        self.assembly.bind(end);
        Ok(())
    }
    fn cast(&mut self, from: ScalarType, to: ScalarType) -> jvm::Result<()> {
        if from == to {
            return Ok(());
        }
        if to == ScalarType::Bool {
            if from.integer().is_none() {
                return Err(error("non-integer boolean cast"));
            }
            self.constant(Scalar::integer(from, 0).unwrap())?;
            return self.comparison(BinaryOp::Ne, from);
        }
        self.assembly
            .code
            .extend(super::super::casts::primitive(&from, &to, self.cp)?);
        self.normalize(to);
        Ok(())
    }
}
