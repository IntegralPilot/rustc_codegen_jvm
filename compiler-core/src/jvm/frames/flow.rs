use super::*;
use crate::jvm::flow::{visit_branch_targets, visit_instruction_successors};

pub(super) fn assignment_successors(
    index: usize,
    instruction: &Instruction,
    context: &str,
) -> jvm::Result<Vec<usize>> {
    let mut invalid = false;
    visit_branch_targets(index, instruction, |target| invalid |= target < 0);
    if invalid {
        return Err(jvm::Error::VerificationError {
            context: context.into(),
            message: format!("Negative branch target at instruction {index}"),
        });
    }
    let mut successors = Vec::new();
    visit_instruction_successors(index, instruction, usize::MAX, |target| {
        successors.push(target)
    });
    Ok(successors)
}

pub(super) fn branch_targets(instructions: &[Instruction]) -> BTreeSet<u16> {
    let mut targets = BTreeSet::new();
    for (index, instruction) in instructions.iter().enumerate() {
        visit_branch_targets(index, instruction, |target| {
            if let Ok(target) = u16::try_from(target) {
                targets.insert(target);
            }
        });
    }
    targets
}
