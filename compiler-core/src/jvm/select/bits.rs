use super::*;
use crate::scalar::BitOp;

impl Selector<'_> {
    pub(super) fn bit(&mut self, op: BitOp, value: ValueId) -> jvm::Result<()> {
        let ty = self.scalar_type(value)?;
        let (width, _) = ty
            .integer()
            .ok_or_else(|| error("bit operand must be integer"))?;
        self.load(value)?;
        if op == BitOp::SwapBytes && width == 8 {
            return Ok(());
        }
        if width < 32 {
            self.assembly.code.extend([
                get_int_const_instr(self.cp, (1 << width) - 1),
                Instruction::Iand,
            ]);
        }
        let owner = self.cp.add_class(if width == 64 {
            "java/lang/Long"
        } else {
            "java/lang/Integer"
        })?;
        let name = match op {
            BitOp::Count => "bitCount",
            BitOp::LeadingZeros => "numberOfLeadingZeros",
            BitOp::TrailingZeros => "numberOfTrailingZeros",
            BitOp::Reverse => "reverse",
            BitOp::SwapBytes => "reverseBytes",
        };
        let signature = if width == 64 {
            if op.is_count() { "(J)I" } else { "(J)J" }
        } else {
            "(I)I"
        };
        let method = self.cp.add_method_ref(owner, name, signature)?;
        self.assembly.code.push(Instruction::Invokestatic(method));
        if width < 32 {
            match op {
                BitOp::LeadingZeros => self.assembly.code.extend([
                    get_int_const_instr(self.cp, (32 - width) as i32),
                    Instruction::Isub,
                ]),
                BitOp::TrailingZeros => {
                    self.assembly
                        .code
                        .push(get_int_const_instr(self.cp, width as i32));
                    let owner = self.cp.add_class("java/lang/Math")?;
                    self.assembly.code.push(Instruction::Invokestatic(
                        self.cp.add_method_ref(owner, "min", "(II)I")?,
                    ));
                }
                BitOp::Reverse | BitOp::SwapBytes => self.assembly.code.extend([
                    get_int_const_instr(self.cp, (32 - width) as i32),
                    Instruction::Iushr,
                ]),
                _ => {}
            }
        }
        if !op.is_count() {
            self.normalize(ty);
        }
        Ok(())
    }
}
