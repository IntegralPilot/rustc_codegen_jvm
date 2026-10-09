use super::*;

impl Selector<'_> {
    pub(super) fn branch(
        &mut self,
        condition: ValueId,
        yes: EdgeId,
        no: EdgeId,
        fallthrough: Option<BlockId>,
    ) -> jvm::Result<()> {
        // The first edge must skip the last edge's parameter copies.
        let (invert, first, last) = if Some(self.body.edges[no.index()].target) == fallthrough {
            (true, yes, no)
        } else {
            (false, no, yes)
        };
        let last_label = self.assembly.label();
        let branch = if let Some(comparison) = self.pending_branch.take() {
            if invert {
                crate::jvm::flow::invert_conditional_branch(&comparison, 0).unwrap()
            } else {
                comparison
            }
        } else {
            self.load(condition)?;
            if invert {
                Instruction::Ifeq(0)
            } else {
                Instruction::Ifne(0)
            }
        };
        self.assembly.branch(branch, last_label);
        self.jump(first)?;
        self.assembly.bind(last_label);
        self.jump_to(last, fallthrough)
    }

    pub(super) fn switch(
        &mut self,
        value: ValueId,
        list: List,
        otherwise: EdgeId,
        fallthrough: Option<BlockId>,
    ) -> jvm::Result<()> {
        let cases = &self.body.cases[list.range()];
        if cases.is_empty() {
            return self.jump(otherwise);
        }
        let ty = self.scalar_type(value)?;
        if let [(key, edge)] = cases
            && ty == ScalarType::Bool
        {
            let (yes, no) = if key.bits() == 0 {
                (otherwise, *edge)
            } else {
                (*edge, otherwise)
            };
            return self.branch(value, yes, no, fallthrough);
        }
        let labels: Vec<_> = cases.iter().map(|_| self.assembly.label()).collect();
        if kind(ty)? == Kind::Int && cases.len() >= 3 {
            self.load(value)?;
            let default = self.assembly.label();
            self.assembly.switch(
                cases
                    .iter()
                    .zip(&labels)
                    .map(|(&(key, _), &label)| {
                        (key.signed().unwrap_or(key.bits() as i128) as i32, label)
                    })
                    .collect(),
                default,
            );
            self.assembly.bind(default);
        } else {
            for (&(key, _), &label) in cases.iter().zip(&labels) {
                self.load(value)?;
                self.constant(key)?;
                if kind(ty)? == Kind::Long {
                    self.assembly.code.push(Instruction::Lcmp);
                    self.assembly.branch(Instruction::Ifeq(0), label);
                } else {
                    self.assembly.branch(Instruction::If_icmpeq(0), label);
                }
            }
        }
        self.jump(otherwise)?;
        for (&(_, edge), label) in cases.iter().zip(labels) {
            self.assembly.bind(label);
            self.jump(edge)?;
        }
        Ok(())
    }
}
