use super::encoding::compact_stack_frame;
use super::locals::locals_loaded_before_definite_store;
use super::*;

#[test]
fn compact_frames_reuse_or_extend_previous_locals() {
    let previous = vec![VerificationType::Integer];
    assert!(matches!(
        compact_stack_frame(4, &previous, &previous, Vec::new()),
        StackFrame::SameFrameExtended {
            frame_type: 251,
            offset_delta: 4
        }
    ));
    assert!(matches!(
        compact_stack_frame(
            7,
            &previous,
            &[VerificationType::Integer, VerificationType::Float],
            Vec::new()
        ),
        StackFrame::AppendFrame {
            frame_type: 252,
            offset_delta: 7,
            ..
        }
    ));
    assert!(matches!(
        compact_stack_frame(
            2,
            &[VerificationType::Integer, VerificationType::Float],
            &previous,
            Vec::new()
        ),
        StackFrame::ChopFrame {
            frame_type: 250,
            offset_delta: 2
        }
    ));
}

#[test]
fn compact_frames_fall_back_when_stack_or_locals_require_it() {
    let previous = vec![VerificationType::Integer];
    assert!(matches!(
        compact_stack_frame(3, &previous, &previous, vec![VerificationType::Integer]),
        StackFrame::SameLocals1StackItemFrameExtended { .. }
    ));
    assert!(matches!(
        compact_stack_frame(
            3,
            &previous,
            &[VerificationType::Float],
            vec![VerificationType::Integer, VerificationType::Integer]
        ),
        StackFrame::FullFrame { .. }
    ));
}

#[test]
fn definite_assignment_finds_control_flow_guarded_loads() {
    let instructions = vec![
        Instruction::Iconst_0,
        Instruction::Ifeq(4),
        Instruction::Iconst_1,
        Instruction::Istore_1,
        Instruction::Iload_1,
        Instruction::Pop,
        Instruction::Return,
    ];
    let loads = locals_loaded_before_definite_store(&instructions, &[], 2, "test", &[]).unwrap();
    assert_eq!(loads, BTreeMap::from([(1, FrameValue::Integer)]));
}

#[test]
fn definite_assignment_accepts_a_store_on_every_path() {
    let instructions = vec![
        Instruction::Iconst_0,
        Instruction::Ifeq(5),
        Instruction::Iconst_1,
        Instruction::Istore_1,
        Instruction::Goto(7),
        Instruction::Iconst_0,
        Instruction::Istore_1,
        Instruction::Iload_1,
        Instruction::Pop,
        Instruction::Return,
    ];
    let loads = locals_loaded_before_definite_store(&instructions, &[], 2, "test", &[]).unwrap();
    assert!(loads.is_empty());
}
