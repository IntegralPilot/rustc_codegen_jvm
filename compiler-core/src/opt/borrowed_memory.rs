//! Use the component ABI for stored borrows, calls, returns and fields.
//! Run after local promotion so eliminated cells gain no runtime calls.
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};
use rustc_hash::FxHashMap;

const OWNER: &str = "org/rustlang/runtime/Pointer";

pub fn borrowed_memory_method(method: &MethodRef) -> bool {
    method.owner == OWNER
        && matches!(
            method.name.as_str(),
            "loadBorrowedView"
                | "loadBorrowedAddress"
                | "storeBorrowedView"
                | "storeBorrowedUtf8"
                | "storeBorrowedAddress"
        )
}

pub fn lower_borrowed_memory(body: &mut Body, types: &mut Types, debug: Option<&mut DebugInfo>) {
    let mut methods = FxHashMap::default();
    let int = types.scalar(ScalarType::I32);
    let unit = types.intern(Type::Unit);
    let mut sizes = FxHashMap::default();
    let mut prologue = Vec::new();
    for index in 0..body.instructions.len() {
        let instruction = body.instructions[index];
        let (pointer, value, ty) = match instruction.op {
            Op::Load(pointer) | Op::LoadCopy(pointer) => {
                (pointer, None, body.value_type(instruction.result.unwrap()))
            }
            Op::Store { pointer, value } => (pointer, Some(value), body.value_type(value)),
            _ => continue,
        };
        let Some(shape) = ComponentShape::of(types, ty) else {
            continue;
        };
        if shape == ComponentShape::TaggedI64 {
            continue;
        }
        let parameter = body.value_type(pointer);
        let mut params = vec![parameter];
        let mut args = vec![pointer];
        let (name, returns) = if let Some(value) = value {
            params.push(ty);
            args.push(value);
            if shape == ComponentShape::View {
                (
                    if types.get(ty) == Some(Type::Str) {
                        "storeBorrowedUtf8"
                    } else {
                        "storeBorrowedView"
                    },
                    unit,
                )
            } else {
                let size = match types.get(ty) {
                    Some(Type::Pointer(inner)) => crate::jvm::abi::address_plan(types, inner),
                    _ => unreachable!(),
                };
                let constant = *sizes.entry(size).or_insert_with(|| {
                    let id = ConstId::new(body.constants.len());
                    body.constants.push(Constant::Scalar(
                        Scalar::integer(ScalarType::I32, size.into()).unwrap(),
                    ));
                    let inst = InstId::new(body.instructions.len());
                    let value = ValueId::new(body.values.len());
                    body.values.push(Value {
                        ty: int,
                        def: ValueDef::Inst(inst),
                    });
                    body.instructions.push(Inst {
                        op: Op::Constant(id),
                        result: Some(value),
                    });
                    prologue.push(inst);
                    value
                });
                params.push(int);
                args.push(constant);
                ("storeBorrowedAddress", unit)
            }
        } else {
            (
                if shape == ComponentShape::View {
                    "loadBorrowedView"
                } else {
                    "loadBorrowedAddress"
                },
                ty,
            )
        };
        let id = *methods
            .entry((parameter, ty, value.is_some()))
            .or_insert_with(|| {
                body.methods.push(MethodRef {
                    owner: OWNER.into(),
                    name: name.into(),
                    params,
                    returns,
                    interface: false,
                });
                body.methods.len() - 1
            });
        body.instructions[index].op = Op::Call {
            method: MethodId::new(id),
            kind: CallKind::JvmStatic,
            args: List::append(&mut body.args, args),
        };
    }
    let count = prologue.len() as u32;
    prologue.append(&mut body.blocks[body.entry.index()].instructions);
    body.blocks[body.entry.index()].instructions = prologue;
    if let Some(debug) = debug {
        for event in &mut debug.events {
            if event.block == body.entry {
                event.position += count;
            }
        }
    }
}
