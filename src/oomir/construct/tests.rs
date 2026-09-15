use super::*;
use oomir::{BasicBlock, CodeBlock, Constant, Function, Instruction, Operand, Signature, Type};

#[test]
fn mutation_uses_its_typed_operand_when_a_temporary_name_is_reused() {
    let array = Type::Array(Box::new(Type::U8));
    let value = Operand::Variable {
        name: "reused".into(),
        ty: array.clone(),
    };
    let function = Function {
        name: "mutation".into(),
        owner_class: None,
        signature: Signature {
            params: vec![("input".into(), array.clone())],
            ret: Box::new(Type::U8),
            is_static: true,
        },
        debug_variables: vec![],
        body: CodeBlock {
            entry: "entry".into(),
            basic_blocks: [(
                "entry".into(),
                BasicBlock {
                    label: "entry".into(),
                    instructions: vec![
                        Instruction::Move {
                            dest: "reused".into(),
                            src: Operand::Constant(Constant::Null(Type::Class(
                                "java/lang/String".into(),
                            ))),
                        },
                        Instruction::Move {
                            dest: "reused".into(),
                            src: Operand::Variable {
                                name: "_1".into(),
                                ty: array,
                            },
                        },
                        Instruction::ArrayStore {
                            array: value.clone(),
                            index: Operand::Constant(Constant::I32(0)),
                            value: Operand::Constant(Constant::U8(234)),
                            copy_value: false,
                        },
                        Instruction::ArrayGet {
                            dest: "result".into(),
                            array: value,
                            index: Operand::Constant(Constant::I32(0)),
                        },
                        Instruction::Return {
                            operand: Some(Operand::Variable {
                                name: "result".into(),
                                ty: Type::U8,
                            }),
                        },
                    ],
                },
            )]
            .into_iter()
            .collect(),
        },
    };
    let sealed = seal(function, &empty_context()).unwrap();
    assert!(sealed.body.debug.is_none());
    assert!(sealed.body.lines.is_none());
    let code = crate::lower2::select::compile(
        &sealed.body,
        &mut Default::default(),
        &mut vec![],
        crate::lower2::DebugInfoOptions {
            local_variables: false,
            line_numbers: false,
        },
        false,
    )
    .unwrap();
    assert!(code.instructions.iter().any(|op| matches!(
        op,
        jvm_compiler_core::classfile::attributes::Instruction::Bastore
    )));
}

fn empty_context() -> Context {
    let module = oomir::Module {
        name: "test".into(),
        source_file: None,
        functions: HashMap::default(),
        data_types: HashMap::default(),
        suppressed_data_types: HashSet::default(),
        shared_data_types: None,
        relative_static_methods: Arc::default(),
        external_interfaces: HashSet::default(),
        statics: HashMap::default(),
    };
    Context::new(&module)
}

#[test]
fn source_entry_only_needs_a_prologue_when_it_has_incoming_edges() {
    for loops in [false, true] {
        let value = Operand::Variable {
            name: "_1".into(),
            ty: Type::I32,
        };
        let instructions = if loops {
            vec![
                Instruction::Binary {
                    op: oomir::BinaryOp::Add,
                    dest: "_1".into(),
                    op1: value,
                    op2: Operand::Constant(Constant::I32(1)),
                },
                Instruction::Jump {
                    target: "entry".into(),
                },
            ]
        } else {
            vec![Instruction::Return {
                operand: Some(value),
            }]
        };
        let function = Function {
            name: "entry".into(),
            owner_class: None,
            debug_variables: vec![],
            signature: Signature {
                params: vec![("input".into(), Type::I32)],
                ret: Box::new(Type::I32),
                is_static: true,
            },
            body: CodeBlock {
                entry: "entry".into(),
                basic_blocks: [(
                    "entry".into(),
                    BasicBlock {
                        label: "entry".into(),
                        instructions,
                    },
                )]
                .into_iter()
                .collect(),
            },
        };
        let sealed = seal(function, &empty_context()).unwrap();
        assert_eq!(sealed.body.ir.blocks.len(), if loops { 2 } else { 1 });
        if !loops {
            assert!(sealed.body.ir.instructions.is_empty());
        }
        ir::verify(&sealed.body.ir, &sealed.body.types).unwrap();
    }
}
