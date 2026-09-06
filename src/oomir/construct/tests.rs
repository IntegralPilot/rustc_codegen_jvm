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
    let sealed = seal(function, &Context::new(&module)).unwrap();
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
