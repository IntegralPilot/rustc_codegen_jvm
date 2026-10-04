use super::*;
use oomir::{BasicBlock, CodeBlock, Constant, Function, Instruction, Operand, Signature, Type};

#[test]
fn unreachable_does_not_construct_an_exception_or_message() {
    let function = Function {
        name: "unreachable".into(),
        owner_class: None,
        signature: Signature {
            params: vec![],
            ret: Box::new(Type::I32),
            is_static: true,
        },
        debug_variables: vec![],
        body: CodeBlock {
            entry: "entry".into(),
            basic_blocks: [(
                "entry".into(),
                BasicBlock {
                    label: "entry".into(),
                    instructions: vec![Instruction::Unreachable],
                },
            )]
            .into_iter()
            .collect(),
        },
    };
    let sealed = seal(function, &empty_context()).unwrap();
    assert!(sealed.body.ir.instructions.is_empty());
    let code = crate::lower2::select::compile(
        None,
        &sealed.body,
        &mut Default::default(),
        &mut vec![],
        crate::lower2::DebugInfoOptions {
            local_variables: false,
            line_numbers: false,
        },
    )
    .unwrap();
    use jvm_compiler_core::classfile::attributes::Instruction as J;
    assert_eq!(code.instructions, vec![J::Aconst_null, J::Athrow]);
}

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
        None,
        &sealed.body,
        &mut Default::default(),
        &mut vec![],
        crate::lower2::DebugInfoOptions {
            local_variables: false,
            line_numbers: false,
        },
    )
    .unwrap();
    // Borrowed arrays can use sliceSetI8 for raw alias coherence. This test checks the operand type
    // after temporary-name reuse.
    assert!(!code.instructions.is_empty());
    assert!(sealed.body.ir.instructions.iter().any(|instruction| {
        let ir::Op::ArraySet { array, value, .. } = instruction.op else {
            return false;
        };
        sealed.body.types.get(sealed.body.ir.value_type(array))
            == Some(ir::Type::Array(sealed.body.ir.value_type(value)))
            && sealed.body.types.get(sealed.body.ir.value_type(value))
                == Some(ir::Type::Scalar(ScalarType::U8))
    }));
}

pub(super) fn empty_context() -> Context {
    Context::new(&empty_module())
}

pub(super) fn empty_module() -> oomir::Module {
    oomir::Module {
        name: "test".into(),
        source_file: None,
        functions: HashMap::default(),
        data_types: HashMap::default(),
        suppressed_data_types: HashSet::default(),
        shared_data_types: None,
        shared_context: None,
        external_interfaces: HashSet::default(),
        statics: HashMap::default(),
    }
}

#[test]
fn stored_borrows_are_direct_fields_but_unsized_tails_are_memory_views() {
    let class = |kind| oomir::DataType::Class {
        kind,
        is_abstract: false,
        super_class: None,
        fields: vec![("tail".into(), Type::Slice(Box::new(Type::U8)))],
        methods: HashMap::default(),
        interfaces: vec![],
    };
    let mut module = oomir::Module {
        name: "test".into(),
        source_file: None,
        functions: HashMap::default(),
        data_types: HashMap::default(),
        suppressed_data_types: HashSet::default(),
        shared_data_types: None,
        shared_context: None,
        external_interfaces: HashSet::default(),
        statics: HashMap::default(),
    };
    module
        .data_types
        .insert("Borrow".into(), class(oomir::ClassKind::Value));
    module
        .data_types
        .insert("Tail".into(), class(oomir::ClassKind::MemoryView));
    let context = Context::new(&module);
    assert!(context.fields["Borrow"].direct);
    assert!(context.fields["Borrow"].split_borrows);
    assert!(!context.fields["Tail"].direct);
    assert!(!context.fields["Tail"].split_borrows);
}

#[test]
fn promoted_pointer_cells_are_decomposed_before_selection() {
    let pointer = Type::pointer(Type::I32);
    let cell = Type::pointer(pointer.clone());
    let variable = |name: &str, ty: &Type| Operand::Variable {
        name: name.into(),
        ty: ty.clone(),
    };
    let instructions = vec![
        Instruction::InvokeStatic {
            dest: Some("cell".into()),
            class_name: oomir::POINTER_CLASS.into(),
            method_name: "cell".into(),
            method_ty: Signature {
                params: vec![
                    ("initial".into(), Type::Class("java/lang/Object".into())),
                    ("size".into(), Type::I32),
                    ("codec".into(), Type::java_string()),
                ],
                ret: Box::new(cell.clone()),
                is_static: true,
            },
            args: vec![
                variable("_1", &pointer),
                Operand::Constant(Constant::I32(8)),
                Operand::Constant(Constant::Null(Type::java_string())),
            ],
        },
        Instruction::InvokeVirtual {
            dest: Some("loaded".into()),
            class_name: oomir::POINTER_CLASS.into(),
            method_name: "getObject".into(),
            operand: variable("cell", &cell),
            args: vec![],
            method_ty: Signature {
                params: vec![],
                ret: Box::new(pointer.clone()),
                is_static: false,
            },
        },
        Instruction::InvokeVirtual {
            dest: Some("value".into()),
            class_name: oomir::POINTER_CLASS.into(),
            method_name: "getI32".into(),
            operand: variable("loaded", &pointer),
            args: vec![],
            method_ty: Signature {
                params: vec![],
                ret: Box::new(Type::I32),
                is_static: false,
            },
        },
        Instruction::Return {
            operand: Some(variable("value", &Type::I32)),
        },
    ];
    let function = Function {
        name: "read".into(),
        owner_class: Some("test/mono/Mono_001".into()),
        debug_variables: vec![],
        signature: Signature {
            params: vec![("input".into(), pointer)],
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
    let body = &sealed.body;
    let live = jvm_compiler_core::opt::live(&body.ir, &body.types);
    assert!(
        body.ir
            .instructions
            .iter()
            .enumerate()
            .all(|(i, inst)| !live.instructions[i]
                || !matches!(
                    inst.op,
                    ir::Op::AddressPack(_) | ir::Op::Load(_) | ir::Op::Call { .. }
                ))
    );
    ir::verify(&body.ir, &body.types).unwrap();
    crate::lower2::select::compile(
        None,
        body,
        &mut Default::default(),
        &mut vec![],
        crate::lower2::DebugInfoOptions {
            local_variables: false,
            line_numbers: false,
        },
    )
    .unwrap();
}

#[test]
fn escaping_scalar_cells_never_enter_the_object_boxing_abi() {
    let pointer = Type::pointer(Type::I64);
    let input = Operand::Variable {
        name: "_1".into(),
        ty: Type::I64,
    };
    let function = Function {
        name: "borrow".into(),
        owner_class: Some("test/mono/Mono_001".into()),
        debug_variables: vec![],
        signature: Signature {
            params: vec![("initial".into(), Type::I64)],
            ret: Box::new(Type::I64),
            is_static: true,
        },
        body: CodeBlock {
            entry: "entry".into(),
            basic_blocks: [(
                "entry".into(),
                BasicBlock {
                    label: "entry".into(),
                    instructions: vec![
                        Instruction::InvokeStatic {
                            dest: Some("cell".into()),
                            class_name: oomir::POINTER_CLASS.into(),
                            method_name: "cell".into(),
                            method_ty: Signature {
                                params: vec![
                                    ("initial".into(), Type::Class("java/lang/Object".into())),
                                    ("size".into(), Type::I32),
                                    ("codec".into(), Type::java_string()),
                                ],
                                ret: Box::new(pointer.clone()),
                                is_static: true,
                            },
                            args: vec![
                                input,
                                Operand::Constant(Constant::I32(8)),
                                Operand::Constant(Constant::Null(Type::java_string())),
                            ],
                        },
                        Instruction::InvokeStatic {
                            dest: Some("result".into()),
                            class_name: "test/mono/Mono_002".into(),
                            method_name: "consume".into(),
                            method_ty: Signature {
                                params: vec![("location".into(), pointer.clone())],
                                ret: Box::new(Type::I64),
                                is_static: true,
                            },
                            args: vec![Operand::Variable {
                                name: "cell".into(),
                                ty: pointer,
                            }],
                        },
                        Instruction::Return {
                            operand: Some(Operand::Variable {
                                name: "result".into(),
                                ty: Type::I64,
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
    let body = &sealed.body.ir;
    let live = jvm_compiler_core::opt::live(body, &sealed.body.types);
    assert!(
        body.instructions
            .iter()
            .any(|i| matches!(i.op, ir::Op::NewArray(_)))
    );
    for (id, instruction) in body.instructions.iter().enumerate() {
        if !live.instructions[id] {
            continue;
        }
        assert!(!matches!(
            instruction.op,
            ir::Op::Adapt(_) | ir::Op::ScalarCell(_) | ir::Op::AddressPack(_)
        ));
        if let ir::Op::Call { method, .. } = instruction.op {
            assert_eq!(body.methods[method.index()].name, "consume");
            assert_eq!(body.methods[method.index()].params.len(), 2);
        }
    }
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
