use super::*;
use crate::ir::*;
use crate::scalar::ScalarType;

#[test]
fn borrowed_return_components_cross_calls_and_unwind_edges() {
    for unwind in [false, true] {
        for returns_borrow in [false, true] {
            let mut types = Types::default();
            let int = types.scalar(ScalarType::I32);
            let long = types.scalar(ScalarType::U64);
            let signed = types.scalar(ScalarType::I64);
            let pointer = types.intern(Type::Pointer(int));
            let slice = types.intern(Type::Slice(int));
            let object = types.symbol("Pair");
            let object = types.intern(Type::Class(object));
            let storage = types.intern(Type::Pointer(object));
            let tagged = types.intern(Type::TaggedI64);
            for borrowed in [pointer, slice, storage, tagged] {
                let output = if returns_borrow { borrowed } else { long };
                let mut b = Builder::new(&types, output);
                let input = b.parameter(b.current(), borrowed);
                let method = b.method(MethodRef {
                    owner: "Leaf".into(),
                    name: "pick".into(),
                    params: vec![borrowed],
                    returns: borrowed,
                    interface: false,
                });
                let args = b.args([input]);
                let op = Op::Call {
                    method,
                    kind: CallKind::JvmStatic,
                    args,
                };
                let result = if unwind {
                    let handler = b.create_block();
                    let value = b.invoke(op, Some(borrowed), handler).unwrap();
                    let continuation = b.current();
                    b.switch_to(handler);
                    b.terminate(Terminator::Rethrow);
                    b.switch_to(continuation);
                    value
                } else {
                    b.emit(op, Some(borrowed)).unwrap()
                };
                let result = if returns_borrow {
                    result
                } else if borrowed == tagged {
                    let tag = b
                        .emit(
                            Op::TaggedPart {
                                value: result,
                                index: 1,
                            },
                            Some(signed),
                        )
                        .unwrap();
                    b.emit(Op::Cast(tag), Some(long)).unwrap()
                } else if borrowed == slice {
                    b.emit(Op::Length(result), Some(long)).unwrap()
                } else {
                    let tag = b.emit(Op::AddressTag(result), Some(signed)).unwrap();
                    b.emit(Op::Cast(tag), Some(long)).unwrap()
                };
                b.terminate(Terminator::Return(Some(result)));
                let mut body = b.finish().unwrap();
                lower_component_arguments(&mut body, &mut types, true, |_| true, None);
                lower_component_returns(&mut body, &mut types, true, |_| true, None);
                verify(&body, &types).unwrap();
                decompose_tagged(&mut body, &types, None);
                decompose_views(&mut body, &mut types, None);
                decompose_addresses(&mut body, &mut types, None);
                verify(&body, &types).unwrap();
                let live = live(&body, &types);
                assert!(!body.instructions.iter().enumerate().any(
                    |(index, inst)| live.instructions[index]
                        && matches!(
                            inst.op,
                            Op::AddressPack(_) | Op::ViewPack(_) | Op::TaggedPack(_)
                        )
                ));
                assert_eq!(
                    body.instructions
                        .iter()
                        .filter(|i| matches!(i.op, Op::NewArray(_)))
                        .count(),
                    usize::from(!returns_borrow)
                );
                assert_eq!(
                    body.blocks
                        .iter()
                        .filter(|b| matches!(b.terminator, Some(Terminator::Invoke { .. })))
                        .count(),
                    usize::from(unwind)
                );
                let code =
                    crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
                use crate::classfile::attributes::Instruction;
                assert_eq!(
                    code.instructions.contains(&Instruction::Laload),
                    !returns_borrow
                );
                assert_eq!(code.instructions.contains(&Instruction::Lastore), false);
            }
        }
    }
}

#[test]
fn earlier_return_metadata_survives_a_later_component_call() {
    let mut types = Types::default();
    let byte = types.scalar(ScalarType::U8);
    let slice = types.intern(Type::Slice(byte));
    let mut b = Builder::new(&types, slice);
    let input = b.parameter(b.current(), slice);
    let method = b.method(MethodRef {
        owner: "Leaf".into(),
        name: "view".into(),
        params: vec![slice],
        returns: slice,
        interface: false,
    });
    let args = b.args([input]);
    let op = Op::Call {
        method,
        kind: CallKind::RustStatic,
        args,
    };
    let first = b.emit(op, Some(slice)).unwrap();
    b.emit(op, Some(slice));
    b.terminate(Terminator::Return(Some(first)));
    let mut body = b.finish().unwrap();
    lower_component_arguments(&mut body, &mut types, true, |_| true, None);
    lower_component_returns(&mut body, &mut types, true, |_| true, None);
    decompose_views(&mut body, &mut types, None);
    verify(&body, &types).unwrap();
    let code = crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
    use crate::classfile::attributes::Instruction;
    assert_eq!(
        code.instructions
            .iter()
            .filter(|i| **i == Instruction::Laload)
            .count(),
        2
    );
    assert_eq!(
        code.instructions
            .iter()
            .filter(|i| **i == Instruction::Lastore)
            .count(),
        2
    );
}
