//! Symbolic JVM control flow. Branches use instruction indices; switch offsets are relative.
use crate::classfile::attributes::Instruction;

macro_rules! conditional_forms {
    ($($a:ident, $b:ident);* $(;)?) => {
        pub fn conditional_branch_target(instruction: &Instruction) -> Option<u16> {
            match instruction { $(Instruction::$a(target) | Instruction::$b(target) => Some(*target),)* _ => None }
        }
        pub fn set_conditional_branch_target(instruction: &Instruction, target: u16) -> Option<Instruction> {
            Some(match instruction { $(Instruction::$a(_) => Instruction::$a(target), Instruction::$b(_) => Instruction::$b(target),)* _ => return None })
        }
        pub fn invert_conditional_branch(instruction: &Instruction, target: u16) -> Option<Instruction> {
            Some(match instruction { $(Instruction::$a(_) => Instruction::$b(target), Instruction::$b(_) => Instruction::$a(target),)* _ => return None })
        }
    };
}
conditional_forms! {
    Ifeq, Ifne; Iflt, Ifge; Ifgt, Ifle;
    If_icmpeq, If_icmpne; If_icmplt, If_icmpge; If_icmpgt, If_icmple;
    If_acmpeq, If_acmpne; Ifnull, Ifnonnull;
}

pub fn instruction_can_fall_through(instruction: &Instruction) -> bool {
    !matches!(
        instruction,
        Instruction::Goto(_)
            | Instruction::Goto_w(_)
            | Instruction::Jsr(_)
            | Instruction::Jsr_w(_)
            | Instruction::Ret(_)
            | Instruction::Ret_w(_)
            | Instruction::Tableswitch(_)
            | Instruction::Lookupswitch(_)
            | Instruction::Ireturn
            | Instruction::Lreturn
            | Instruction::Freturn
            | Instruction::Dreturn
            | Instruction::Areturn
            | Instruction::Return
            | Instruction::Athrow
    )
}

pub fn visit_instruction_successors(
    index: usize,
    instruction: &Instruction,
    instruction_count: usize,
    mut visitor: impl FnMut(usize),
) {
    visit_branch_targets(index, instruction, |target| {
        if let Ok(target) = usize::try_from(target) {
            visitor(target);
        }
    });
    if instruction_can_fall_through(instruction) && index + 1 < instruction_count {
        visitor(index + 1);
    }
}

pub fn instruction_successors(
    index: usize,
    instruction: &Instruction,
    instruction_count: usize,
) -> Vec<usize> {
    let mut result = Vec::new();
    visit_instruction_successors(index, instruction, instruction_count, |target| {
        result.push(target)
    });
    result
}

pub fn visit_branch_targets(index: usize, instruction: &Instruction, mut visitor: impl FnMut(i64)) {
    use Instruction as I;

    match instruction {
        I::Ifeq(target)
        | I::Ifne(target)
        | I::Iflt(target)
        | I::Ifge(target)
        | I::Ifgt(target)
        | I::Ifle(target)
        | I::If_icmpeq(target)
        | I::If_icmpne(target)
        | I::If_icmplt(target)
        | I::If_icmpge(target)
        | I::If_icmpgt(target)
        | I::If_icmple(target)
        | I::If_acmpeq(target)
        | I::If_acmpne(target)
        | I::Goto(target)
        | I::Jsr(target)
        | I::Ifnull(target)
        | I::Ifnonnull(target) => visitor(i64::from(*target)),
        I::Goto_w(target) | I::Jsr_w(target) => visitor(i64::from(*target)),
        I::Tableswitch(table_switch) => {
            visitor(index as i64 + i64::from(table_switch.default));
            for target in &table_switch.offsets {
                visitor(index as i64 + i64::from(*target));
            }
        }
        I::Lookupswitch(lookup_switch) => {
            visitor(index as i64 + i64::from(lookup_switch.default));
            for target in lookup_switch.pairs.values() {
                visitor(index as i64 + i64::from(*target));
            }
        }
        _ => {}
    }
}
