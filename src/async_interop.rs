//! JVM-facing adapters for Rust coroutine values.

use crate::oomir::{
    self, BasicBlock, CodeBlock, DataTypeMethod, Function, Instruction, Operand, Signature, Type,
};
use rustc_hash::FxHashMap as HashMap;

const RUST_FUTURE_INTERFACE: &str = "org/rustlang/runtime/RustFuture";
const KOTLIN_INTEROP_CLASS: &str = "org/rustlang/runtime/KotlinFutureInterop";

#[derive(Clone)]
pub struct PointerLayout {
    pub size: i32,
    pub alignment: i32,
    pub codec: Operand,
}

fn poll_function(
    coroutine_class: &str,
    future: PointerLayout,
    waker_vtable: PointerLayout,
) -> Function {
    let object_type = Type::Class("java/lang/Object".to_string());
    let runnable_type = Type::Class("java/lang/Runnable".to_string());
    let string_type = Type::java_string();
    let instructions = vec![
        Instruction::InvokeStatic {
            dest: Some("result".to_string()),
            class_name: KOTLIN_INTEROP_CLASS.to_string(),
            method_name: "pollRustFuture".to_string(),
            method_ty: Signature {
                params: vec![
                    ("future".to_string(), object_type.clone()),
                    ("wake".to_string(), runnable_type.clone()),
                    ("size".to_string(), Type::I32),
                    ("codec".to_string(), string_type),
                    ("alignment".to_string(), Type::I32),
                    ("waker_vtable_size".to_string(), Type::I32),
                    ("waker_vtable_codec".to_string(), Type::java_string()),
                    ("waker_vtable_alignment".to_string(), Type::I32),
                ],
                ret: Box::new(object_type.clone()),
                is_static: true,
            },
            args: vec![
                Operand::Variable {
                    name: "_1".to_string(),
                    ty: Type::Class(coroutine_class.to_string()),
                },
                Operand::Variable {
                    name: "_2".to_string(),
                    ty: runnable_type.clone(),
                },
                Operand::Constant(oomir::Constant::I32(future.size)),
                future.codec,
                Operand::Constant(oomir::Constant::I32(future.alignment)),
                Operand::Constant(oomir::Constant::I32(waker_vtable.size)),
                waker_vtable.codec,
                Operand::Constant(oomir::Constant::I32(waker_vtable.alignment)),
            ],
        },
        Instruction::Return {
            operand: Some(Operand::Variable {
                name: "result".to_string(),
                ty: object_type.clone(),
            }),
        },
    ];

    Function {
        name: "poll".to_string(),
        owner_class: Some(coroutine_class.to_string()),
        signature: Signature {
            params: vec![
                ("self".to_string(), Type::Class(coroutine_class.to_string())),
                ("wake".to_string(), runnable_type),
            ],
            ret: Box::new(object_type),
            is_static: false,
        },
        debug_variables: Vec::new(),
        body: CodeBlock {
            entry: "entry".to_string(),
            basic_blocks: HashMap::from_iter([(
                "entry".to_string(),
                BasicBlock {
                    label: "entry".to_string(),
                    instructions,
                },
            )]),
        },
    }
}

/// Makes one generated Rust coroutine implement the small runtime future ABI.
///
/// Layout information is embedded directly in the generated bridge. Runtime
/// adapters therefore never need to discover pointer codecs by scanning JARs.
pub fn add_rust_future_bridge(
    data_types: &mut HashMap<String, oomir::DataType>,
    class_name: &str,
    future: PointerLayout,
    waker_vtable: PointerLayout,
) {
    let Some(oomir::DataType::Class {
        methods,
        interfaces,
        ..
    }) = data_types.get_mut(class_name)
    else {
        return;
    };
    if methods.contains_key("poll") {
        return;
    }
    if !interfaces
        .iter()
        .any(|interface| interface == RUST_FUTURE_INTERFACE)
    {
        interfaces.push(RUST_FUTURE_INTERFACE.to_string());
    }
    methods.insert(
        "poll".to_string(),
        DataTypeMethod::Function(poll_function(class_name, future, waker_vtable)),
    );
}
