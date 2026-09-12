use super::*;

impl Selector<'_> {
    pub(super) fn switch(
        &mut self,
        value: ValueId,
        list: List,
        otherwise: EdgeId,
    ) -> jvm::Result<()> {
        let cases = &self.body.cases[list.range()];
        if cases.is_empty() {
            return self.jump(otherwise);
        }
        let ty = self.scalar_type(value)?;
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
