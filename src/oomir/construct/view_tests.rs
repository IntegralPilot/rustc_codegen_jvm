use super::*;
use oomir::{BasicBlock, CodeBlock, Constant, Function, Instruction, Operand, Signature, Type};

#[test]
fn constructed_views_and_scalar_data_extraction_keep_components() {
    let array = Type::Array(Box::new(Type::U32));
    let slice = Type::Slice(Box::new(Type::U32));
    let pointer = Type::pointer(Type::U32);
    let object = Type::Class("java/lang/Object".into());
    let var = |name: &str, ty: &Type| Operand::Variable {
        name: name.into(),
        ty: ty.clone(),
    };
    let mut function = Function {
        name: "read_subslice".into(),
        owner_class: None,
        debug_variables: vec![],
        signature: Signature {
            params: vec![("array".into(), array.clone())],
            ret: Box::new(Type::U32),
            is_static: true,
        },
        body: CodeBlock {
            entry: "entry".into(),
            basic_blocks: [(
                "entry".into(),
                BasicBlock {
                    label: "entry".into(),
                    instructions: vec![
                        Instruction::ConstructObject {
                            dest: "view".into(),
                            class_name: oomir::SLICE_VIEW_CLASS.into(),
                            args: vec![
                                (var("_1", &array), object),
                                (Operand::Constant(Constant::I32(2)), Type::I32),
                                (Operand::Constant(Constant::U64(3)), Type::U64),
                            ],
                        },
                        Instruction::Cast {
                            dest: "slice".into(),
                            op: var("view", &Type::Class(oomir::SLICE_VIEW_CLASS.into())),
                            ty: slice.clone(),
                        },
                        Instruction::ViewAddress {
                            dest: Some("data".into()),
                            source: var("slice", &slice),
                            layout: Box::new(oomir::AddressLayout {
                                pointer_type: pointer.clone(),
                                size: Operand::Constant(Constant::U64(4)),
                                codec: Operand::Constant(Constant::Null(Type::java_string())),
                            }),
                        },
                        Instruction::InvokeVirtual {
                            dest: Some("value".into()),
                            class_name: oomir::POINTER_CLASS.into(),
                            method_name: "getI32".into(),
                            operand: var("data", &pointer),
                            args: vec![],
                            method_ty: Signature {
                                params: vec![],
                                ret: Box::new(Type::U32),
                                is_static: false,
                            },
                        },
                        Instruction::Return {
                            operand: Some(var("value", &Type::U32)),
                        },
                    ],
                },
            )]
            .into_iter()
            .collect(),
        },
    };
    let instructions = &mut function
        .body
        .basic_blocks
        .get_mut("entry")
        .unwrap()
        .instructions;
    instructions.splice(
        3..3,
        [
            Instruction::AddressRetype {
                dest: Some("retyped".into()),
                source: var("data", &pointer),
                layout: Box::new(oomir::AddressLayout {
                    pointer_type: pointer.clone(),
                    size: Operand::Constant(Constant::U64(4)),
                    codec: Operand::Constant(Constant::Null(Type::java_string())),
                }),
            },
            Instruction::InvokeVirtual {
                dest: Some("backing".into()),
                class_name: oomir::POINTER_CLASS.into(),
                method_name: "sliceBackingArray".into(),
                operand: var("retyped", &pointer),
                args: vec![],
                method_ty: Signature {
                    params: vec![],
                    ret: Box::new(Type::Class("java/lang/Object".into())),
                    is_static: false,
                },
            },
            Instruction::InvokeVirtual {
                dest: Some("start".into()),
                class_name: oomir::POINTER_CLASS.into(),
                method_name: "sliceElementOffset".into(),
                operand: var("retyped", &pointer),
                args: vec![],
                method_ty: Signature {
                    params: vec![],
                    ret: Box::new(Type::I32),
                    is_static: false,
                },
            },
            Instruction::ConstructObject {
                dest: "roundtrip".into(),
                class_name: oomir::SLICE_VIEW_CLASS.into(),
                args: vec![
                    (
                        var("backing", &Type::Class("java/lang/Object".into())),
                        Type::Class("java/lang/Object".into()),
                    ),
                    (var("start", &Type::I32), Type::I32),
                    (Operand::Constant(Constant::U64(3)), Type::U64),
                ],
            },
            Instruction::Cast {
                dest: "slice".into(),
                op: var("roundtrip", &Type::Class(oomir::SLICE_VIEW_CLASS.into())),
                ty: slice.clone(),
            },
            Instruction::ViewAddress {
                dest: Some("data".into()),
                source: var("slice", &slice),
                layout: Box::new(oomir::AddressLayout {
                    pointer_type: pointer.clone(),
                    size: Operand::Constant(Constant::U64(4)),
                    codec: Operand::Constant(Constant::Null(Type::java_string())),
                }),
            },
        ],
    );
    let sealed = seal(function, &super::tests::empty_context()).unwrap();
    let body = &sealed.body;
    ir::verify(&body.ir, &body.types).unwrap();
    let live = jvm_compiler_core::opt::live(&body.ir, &body.types);
    for (index, inst) in body.ir.instructions.iter().enumerate() {
        if !live.instructions[index] {
            continue;
        }
        assert!(
            !matches!(
                inst.op,
                ir::Op::ViewPack(_)
                    | ir::Op::AddressPack(_)
                    | ir::Op::ViewAddress { .. }
                    | ir::Op::Load(_)
            ),
            "{inst:?}"
        );
        if let ir::Op::Call { method, .. } = inst.op {
            assert!(matches!(
                body.ir.methods[method.index()].name.as_str(),
                "scalarSliceRoot" | "locationSliceBacking" | "locationSliceOffset"
            ));
        }
    }
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
fn scalar_pointer_slice_builder_does_not_materialize_its_data() {
    let pointer = Type::pointer(Type::U16);
    let mut instructions = Vec::new();
    let view = crate::lower1::place::emit_pointer_slice_view(
        Operand::Variable {
            name: "_1".into(),
            ty: pointer.clone(),
        },
        Operand::Constant(Constant::U64(2)),
        "view",
        &mut instructions,
    );
    instructions.push(Instruction::GetField {
        dest: "backing".into(),
        object: view,
        field_name: "array".into(),
        field_ty: Type::Class("java/lang/Object".into()),
        owner_class: oomir::SLICE_VIEW_CLASS.into(),
    });
    instructions.push(Instruction::Return {
        operand: Some(Operand::Variable {
            name: "backing".into(),
            ty: Type::Class("java/lang/Object".into()),
        }),
    });
    let function = Function {
        name: "scalar_view".into(),
        owner_class: Some("test/mono/Mono_01".into()),
        debug_variables: vec![],
        signature: Signature {
            params: vec![("data".into(), pointer)],
            ret: Box::new(Type::Class("java/lang/Object".into())),
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
    let sealed = seal(function, &super::tests::empty_context()).unwrap();
    let function = &sealed.body;
    ir::verify(&function.ir, &function.types).unwrap();
    let live = jvm_compiler_core::opt::live(&function.ir, &function.types);
    for (index, inst) in function.ir.instructions.iter().enumerate() {
        if live.instructions[index] {
            assert!(!matches!(
                inst.op,
                ir::Op::AddressPack(_) | ir::Op::ViewPack(_) | ir::Op::Adapt(_)
            ));
        }
    }
    assert!(
        function
            .ir
            .methods
            .iter()
            .any(|method| method.name == "locationSliceBacking")
    );
}
