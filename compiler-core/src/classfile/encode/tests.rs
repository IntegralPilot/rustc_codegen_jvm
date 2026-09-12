use super::*;
use ristretto_classfile::{
    attributes::{
        ExceptionTableEntry, Instruction, LineNumber, LookupSwitch, StackFrame, TableSwitch,
        VerificationType,
    },
    byte_reader::ByteReader,
};

fn compare(code: Vec<Instruction>, nested: Vec<Attribute>, exceptions: Vec<ExceptionTableEntry>) {
    let class = ClassFile {
        methods: vec![Method {
            attributes: vec![Attribute::Code {
                name_index: 1,
                max_stack: 8,
                max_locals: 300,
                code,
                exception_table: exceptions,
                attributes: nested,
            }],
            ..Method::default()
        }],
        ..ClassFile::default()
    };
    let mut expected = vec![1, 2, 3];
    class.to_bytes(&mut expected).unwrap();
    let mut actual = vec![1, 2, 3];
    class_file(&class, &mut actual).unwrap();
    assert_eq!(expected, actual);
}

#[test]
fn instruction_sizes_match_every_decodable_opcode() {
    for opcode in 0..=255 {
        let mut encoded = vec![0; 128];
        encoded[0] = opcode;
        if let Ok(instruction) = Instruction::from_bytes(&mut ByteReader::new(&encoded)) {
            let mut writer = std::io::Cursor::new(Vec::new());
            instruction.to_bytes(&mut writer).unwrap();
            assert_eq!(
                crate::jvm::encoding::instruction_size_at(&instruction, 0),
                writer.get_ref().len(),
                "{instruction:?}"
            );
        }
    }
    for instruction in [
        Instruction::Iload_w(300),
        Instruction::Iinc_w(300, -300),
        Instruction::Ret_w(300),
    ] {
        let mut writer = std::io::Cursor::new(Vec::new());
        instruction.to_bytes(&mut writer).unwrap();
        assert_eq!(
            crate::jvm::encoding::instruction_size_at(&instruction, 0),
            writer.get_ref().len()
        );
    }
}

#[test]
fn branches_switch_alignment_and_large_methods_match_reference() {
    for prefix in 0..4 {
        let mut code = vec![Instruction::Nop; prefix];
        code.extend([
            Instruction::Tableswitch(Box::new(TableSwitch {
                default: 2,
                low: -1,
                high: 0,
                offsets: vec![1, 2],
            })),
            Instruction::Lookupswitch(Box::new(LookupSwitch {
                default: -1,
                pairs: [(1, -1), (42, 1)].into_iter().collect(),
            })),
            Instruction::Goto(0),
            Instruction::Ifnull(prefix as u16),
            Instruction::Goto_w(0),
            Instruction::Return,
        ]);
        compare(code, vec![], vec![]);
    }
    let mut code = vec![Instruction::Iinc(0, 0); 20_000];
    code.extend([Instruction::Goto(20_001), Instruction::Return]);
    compare(code, vec![], vec![]);
}

#[test]
fn frames_lines_and_exception_ranges_match_reference() {
    let code = vec![
        Instruction::Bipush(0),
        Instruction::Iinc_w(300, 1),
        Instruction::Sipush(0),
        Instruction::Nop,
        Instruction::Return,
    ];
    let kinds = vec![
        StackFrame::SameFrame { frame_type: 1 },
        StackFrame::SameLocals1StackItemFrame {
            frame_type: 65,
            stack: vec![VerificationType::Integer],
        },
        StackFrame::SameLocals1StackItemFrameExtended {
            frame_type: 247,
            offset_delta: 1,
            stack: vec![VerificationType::Object { cpool_index: 2 }],
        },
        StackFrame::AppendFrame {
            frame_type: 252,
            offset_delta: 1,
            locals: vec![VerificationType::Long],
        },
        StackFrame::ChopFrame {
            frame_type: 250,
            offset_delta: 1,
        },
        StackFrame::SameFrameExtended {
            frame_type: 251,
            offset_delta: 1,
        },
        StackFrame::FullFrame {
            frame_type: 255,
            offset_delta: 1,
            locals: vec![VerificationType::Integer],
            stack: vec![],
        },
    ];
    for frame in kinds {
        compare(
            code.clone(),
            vec![
                Attribute::StackMapTable {
                    name_index: 3,
                    frames: vec![
                        frame,
                        StackFrame::SameFrameExtended {
                            frame_type: 251,
                            offset_delta: 1,
                        },
                    ],
                },
                Attribute::LineNumberTable {
                    name_index: 4,
                    line_numbers: vec![LineNumber {
                        start_pc: 2,
                        line_number: 123,
                    }],
                },
            ],
            vec![ExceptionTableEntry {
                range_pc: 0..3,
                handler_pc: 4,
                catch_type: 0,
            }],
        );
    }
}

#[test]
fn invalid_branch_and_oversized_code_are_rejected() {
    let mut bytes = Vec::new();
    assert!(matches!(
        code::instructions(&[Instruction::Goto(1)], &mut bytes),
        Err(Error::InvalidInstructionOffset(1))
    ));
    assert!(code::instructions(&vec![Instruction::Nop; 65_536], &mut bytes).is_err());
}
