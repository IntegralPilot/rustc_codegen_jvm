use super::*;

#[derive(Clone, Copy)]
pub(super) struct Label(pub usize);

#[derive(Default)]
pub(super) struct Assembly {
    pub code: Vec<Instruction>,
    labels: Vec<Option<usize>>,
    fixups: Vec<(usize, Label)>,
    switches: Vec<(usize, Label, Vec<Label>)>,
}

impl Assembly {
    pub fn label(&mut self) -> Label {
        let label = Label(self.labels.len());
        self.labels.push(None);
        label
    }
    pub fn bind(&mut self, label: Label) {
        assert!(self.labels[label.0].replace(self.code.len()).is_none());
    }
    pub fn branch(&mut self, instruction: Instruction, target: Label) {
        self.fixups.push((self.code.len(), target));
        self.code.push(instruction);
    }
    pub fn switch(&mut self, mut cases: Vec<(i32, Label)>, otherwise: Label) {
        use jvm::attributes::{LookupSwitch, TableSwitch};
        cases.sort_unstable_by_key(|&(key, _)| key);
        let low = cases.first().unwrap().0;
        let high = cases.last().unwrap().0;
        let span = (i64::from(high) - i64::from(low) + 1) as u64;
        let index = self.code.len();
        if 12 + span * 4 <= 8 + cases.len() as u64 * 8 {
            let mut labels = vec![otherwise; span as usize];
            for (key, label) in cases {
                labels[(i64::from(key) - i64::from(low)) as usize] = label;
            }
            self.code
                .push(Instruction::Tableswitch(Box::new(TableSwitch {
                    default: 0,
                    low,
                    high,
                    offsets: vec![0; labels.len()],
                })));
            self.switches.push((index, otherwise, labels));
        } else {
            self.code
                .push(Instruction::Lookupswitch(Box::new(LookupSwitch {
                    default: 0,
                    pairs: cases.iter().map(|&(key, _)| (key, 0)).collect(),
                })));
            self.switches.push((
                index,
                otherwise,
                cases.into_iter().map(|(_, label)| label).collect(),
            ));
        }
    }
    pub fn finish(mut self) -> jvm::Result<Vec<Instruction>> {
        for (index, default, labels) in self.switches {
            let offset = |label: Label| -> jvm::Result<i32> {
                let target = self.labels[label.0].ok_or_else(|| error("unbound switch label"))?;
                Ok(i32::try_from(target as i64 - index as i64)?)
            };
            match &mut self.code[index] {
                Instruction::Tableswitch(table) => {
                    table.default = offset(default)?;
                    for (target, label) in table.offsets.iter_mut().zip(labels) {
                        *target = offset(label)?;
                    }
                }
                Instruction::Lookupswitch(table) => {
                    table.default = offset(default)?;
                    for (target, label) in table.pairs.values_mut().zip(labels) {
                        *target = offset(label)?;
                    }
                }
                _ => unreachable!(),
            }
        }
        let mut candidates = Vec::new();
        for (index, label) in self.fixups {
            let target = self.labels[label.0].ok_or_else(|| error("unbound JVM label"))?;
            let short = target == index + 2
                && matches!(self.code.get(index + 1), Some(Instruction::Goto_w(_)));
            let target = u16::try_from(target)?;
            match &mut self.code[index] {
                Instruction::Goto_w(t) => *t = i32::from(target),
                instruction => {
                    *instruction =
                        crate::jvm::flow::set_conditional_branch_target(instruction, target)
                            .ok_or_else(|| error("invalid symbolic branch"))?;
                    if short {
                        candidates.push(index);
                    }
                }
            }
        }
        if candidates.is_empty() {
            return Ok(self.code);
        }
        let offsets = crate::jvm::encoding::instruction_byte_offsets(&self.code)?;
        let mut targeted = vec![false; self.code.len()];
        for position in self.labels.into_iter().flatten() {
            if position < targeted.len() {
                targeted[position] = true;
            }
        }
        for index in candidates {
            if targeted[index + 1] {
                continue;
            }
            let Instruction::Goto_w(target) = self.code[index + 1] else {
                continue;
            };
            let target = u16::try_from(target)?;
            if i16::try_from(offsets[usize::from(target)] as i64 - offsets[index] as i64).is_ok() {
                self.code[index] =
                    crate::jvm::flow::invert_conditional_branch(&self.code[index], target).unwrap();
                // Retain instruction indices and switch alignment for metadata.
                self.code[index + 1] = Instruction::Nop;
            }
        }
        Ok(self.code)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shorten_only_unshared_conditional_trampolines_within_range() {
        for (distance, shared, short) in
            [(12, false, true), (40000, false, false), (12, true, false)]
        {
            let mut a = Assembly::default();
            let skip = a.label();
            let target = a.label();
            a.branch(Instruction::Ifeq(0), skip);
            if shared {
                let shared = a.label();
                a.bind(shared);
            }
            a.branch(Instruction::Goto_w(0), target);
            a.bind(skip);
            a.code.resize(distance, Instruction::Nop);
            a.bind(target);
            a.code.push(Instruction::Return);
            let code = a.finish().unwrap();
            assert_eq!(
                code[0],
                if short {
                    Instruction::Ifne(distance as u16)
                } else {
                    Instruction::Ifeq(2)
                }
            );
            assert_eq!(matches!(code[1], Instruction::Nop), short);
        }
    }
}
