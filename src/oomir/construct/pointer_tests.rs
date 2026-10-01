use super::*;
use oomir::{BasicBlock, CodeBlock, Function, Instruction, Operand, Signature, Type};

#[test]
fn semantic_field_projection_keeps_layout_and_guards_erased_owners() {
    let owner = "test/Record";
    let pointer = Type::pointer(Type::Class(owner.into()));
    for direct in [true, false] {
        let function = Function {
            name: "read_field".into(),
            owner_class: Some("test/mono/Mono_000".into()),
            debug_variables: vec![],
            signature: Signature {
                params: vec![("record".into(), pointer.clone())],
                ret: Box::new(Type::U32),
                is_static: true,
            },
            body: CodeBlock {
                entry: "entry".into(),
                basic_blocks: HashMap::from_iter([(
                    "entry".into(),
                    BasicBlock {
                        label: "entry".into(),
                        instructions: vec![
                            Instruction::MemoryProject {
                                dest: "field".into(),
                                base: Operand::Variable {
                                    name: "_1".into(),
                                    ty: pointer.clone(),
                                },
                                projection: Box::new(oomir::MemoryProjection {
                                    owner: owner.into(),
                                    field: "value".into(),
                                    pointee: Type::U32,
                                    offset: 12,
                                    size: 4,
                                    codec: None,
                                }),
                            },
                            Instruction::MemoryLoad {
                                dest: "result".into(),
                                pointer: Operand::Variable {
                                    name: "field".into(),
                                    ty: Type::pointer(Type::U32),
                                },
                                pointee: Type::U32,
                                owned: false,
                            },
                            Instruction::Return {
                                operand: Some(Operand::Variable {
                                    name: "result".into(),
                                    ty: Type::U32,
                                }),
                            },
                        ],
                    },
                )]),
            },
        };
        let mut context = tests::empty_context();
        context.fields.insert(
            owner.into(),
            context::FieldLayout {
                members: vec![("value".into(), Type::U32)],
                direct,
                split_borrows: false,
            },
        );
        let sealed = seal(function, &context).unwrap();
        let body = &sealed.body.ir;
        ir::verify(body, &sealed.body.types).unwrap();
        assert_eq!(body.projections.len(), usize::from(direct));
        if direct {
            assert_eq!(body.projections[0].offset, 12);
            assert_eq!(body.projections[0].size, 4);
        }
        assert_eq!(
            body.methods.iter().any(|m| m.name == "projectStructField"),
            !direct
        );
        crate::lower2::select::compile(
            &sealed.body,
            &mut Default::default(),
            &mut vec![],
            crate::lower2::DebugInfoOptions {
                local_variables: false,
                line_numbers: false,
            },
        )
        .unwrap();
    }
}

#[test]
fn sized_aggregate_comparisons_use_locations_but_dst_keeps_its_carrier() {
    for ((pointee, direct), method) in [
        (Type::Class("test/Item".into()), true),
        (Type::Class("test/Item".into()), false),
        (Type::Array(Box::new(Type::U8)), true),
        (
            Type::Array(Box::new(Type::Array(Box::new(Type::U32)))),
            true,
        ),
        (Type::Array(Box::new(Type::pointer(Type::U8))), true),
    ]
    .into_iter()
    .flat_map(|(pointee, direct)| {
        [
            "samePointer",
            "sameAddress",
            "lessThan",
            "lessOrEqual",
            "greaterThan",
            "greaterOrEqual",
        ]
        .map(move |method| ((pointee.clone(), direct), method))
    }) {
        let pointer = Type::pointer(pointee);
        let variable = |name: &str| Operand::Variable {
            name: name.into(),
            ty: pointer.clone(),
        };
        let function = Function {
            name: "same".into(),
            owner_class: Some("test/mono/Mono_000".into()),
            signature: Signature {
                params: vec![
                    ("left".into(), pointer.clone()),
                    ("right".into(), pointer.clone()),
                ],
                ret: Box::new(Type::Boolean),
                is_static: true,
            },
            debug_variables: vec![],
            body: CodeBlock {
                entry: "entry".into(),
                basic_blocks: HashMap::from_iter([(
                    "entry".into(),
                    BasicBlock {
                        label: "entry".into(),
                        instructions: vec![
                            Instruction::InvokeVirtual {
                                dest: Some("equal".into()),
                                class_name: oomir::POINTER_CLASS.into(),
                                method_name: method.into(),
                                method_ty: Signature {
                                    params: vec![
                                        ("self".into(), pointer.clone()),
                                        ("other".into(), pointer.clone()),
                                    ],
                                    ret: Box::new(Type::Boolean),
                                    is_static: false,
                                },
                                operand: variable("_1"),
                                args: vec![variable("_2")],
                            },
                            Instruction::Return {
                                operand: Some(Operand::Variable {
                                    name: "equal".into(),
                                    ty: Type::Boolean,
                                }),
                            },
                        ],
                    },
                )]),
            },
        };
        let mut context = tests::empty_context();
        context.fields.insert(
            "test/Item".into(),
            context::FieldLayout {
                members: vec![("value".into(), Type::U32)],
                direct,
                split_borrows: false,
            },
        );
        let sealed = seal(function, &context).unwrap();
        let body = &sealed.body.ir;
        let types = &sealed.body.types;
        ir::verify(body, types).unwrap();
        assert_eq!(
            body.instructions
                .iter()
                .any(|i| if matches!(method, "samePointer" | "sameAddress") {
                    matches!(i.op, ir::Op::LocationEqual(_))
                } else {
                    matches!(i.op, ir::Op::LocationCompare(_))
                }),
            direct
        );
        let live = jvm_compiler_core::opt::live(body, types);
        assert_eq!(
            body.instructions
                .iter()
                .enumerate()
                .any(|(i, instruction)| {
                    live.instructions[i] && matches!(instruction.op, ir::Op::AddressPack(_))
                }),
            !direct
        );
        jvm_compiler_core::jvm::select::compile(body, types, &mut Default::default()).unwrap();
    }
}

#[test]
fn allocator_calls_keep_storage_components_through_reallocation() {
    let pointer = Type::pointer(Type::U8);
    let variable = |name: &str| Operand::Variable {
        name: name.into(),
        ty: pointer.clone(),
    };
    let integer = |n| Operand::Constant(oomir::Constant::U64(n));
    let call = |operation, result: Option<&str>, args| Instruction::Heap {
        dest: result.map(str::to_owned),
        operation,
        args,
    };
    let function = Function {
        name: "allocation".into(),
        owner_class: Some("test/mono/Mono_001".into()),
        debug_variables: vec![],
        signature: Signature {
            params: vec![],
            ret: Box::new(Type::Unit),
            is_static: true,
        },
        body: CodeBlock {
            entry: "entry".into(),
            basic_blocks: [(
                "entry".into(),
                BasicBlock {
                    label: "entry".into(),
                    instructions: vec![
                        call(
                            oomir::HeapOp::Allocate,
                            Some("first"),
                            vec![integer(16), integer(8)],
                        ),
                        call(
                            oomir::HeapOp::Reallocate,
                            Some("grown"),
                            vec![variable("first"), integer(16), integer(8), integer(32)],
                        ),
                        call(oomir::HeapOp::Deallocate, None, vec![variable("grown")]),
                        Instruction::Return { operand: None },
                    ],
                },
            )]
            .into_iter()
            .collect(),
        },
    };
    let sealed = seal(function, &tests::empty_context()).unwrap();
    let body = &sealed.body.ir;
    let types = &sealed.body.types;
    ir::verify(body, types).unwrap();
    let live = jvm_compiler_core::opt::live(body, types);
    let mut calls = Vec::new();
    for (i, instruction) in body.instructions.iter().enumerate() {
        if !live.instructions[i] {
            continue;
        }
        assert!(!matches!(
            instruction.op,
            ir::Op::AddressPack(_) | ir::Op::AddressPart { .. }
        ));
        assert!(!matches!(instruction.op, ir::Op::Call { .. }));
        if let ir::Op::Heap { operation, .. } = instruction.op {
            calls.push(operation);
        }
    }
    assert_eq!(
        calls,
        [
            ir::HeapOp::Allocate,
            ir::HeapOp::Reallocate,
            ir::HeapOp::Deallocate
        ]
    );
    jvm_compiler_core::jvm::select::compile(body, types, &mut Default::default()).unwrap();
}
