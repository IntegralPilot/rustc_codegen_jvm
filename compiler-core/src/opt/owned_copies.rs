//! Reuse private scalar snapshots when neither identity can be changed or observed.
use crate::ir::*;
use rustc_hash::FxHashMap;

pub(super) fn reuse_readonly_copies(
    body: &mut Body,
    types: &Types,
    layout: &mut impl FnMut(&str) -> Option<Vec<FieldRef>>,
) {
    if !body
        .instructions
        .iter()
        .any(|i| matches!(i.op, Op::CopyValue(_)))
    {
        return;
    }
    let mut schemas = FxHashMap::default();
    let mut roots = vec![crate::analysis::NO_ORIGIN; body.values.len()];
    let mut values = Vec::new();
    for inst in &body.instructions {
        if !matches!(
            inst.op,
            Op::CopyValue(_)
                | Op::LoadCopy(_)
                | Op::LoadFieldCopy { .. }
                | Op::LoadStorageFieldCopy { .. }
                | Op::LoadAddressCopy(_)
                | Op::LoadTypedCopy { .. }
                | Op::ArrayGetCopy { .. }
                | Op::ViewGetCopy(_)
                | Op::Call {
                    kind: CallKind::Constructor,
                    ..
                }
        ) {
            continue;
        }
        let Some(value) = inst.result else { continue };
        let ty = body.value_type(value);
        let Some(Type::Class(name)) = types.get(ty) else {
            continue;
        };
        let fields = schemas.entry(ty).or_insert_with(|| {
            layout(types.symbol_name(name).unwrap()).filter(|fields| {
                fields.iter().all(|f| {
                    f.owner == ty
                        && !f.is_static
                        && matches!(types.get(f.ty), Some(Type::Scalar(_)))
                })
            })
        });
        if fields.is_some() {
            roots[value.index()] = values.len() as u32;
            values.push(value);
        }
    }
    if values.is_empty() {
        return;
    }
    let origins = crate::analysis::origins(body, &roots);
    let mut readonly = vec![true; values.len()];
    for inst in &body.instructions {
        inst.op.visit_uses(&body.args, |value| {
            let origin = origins[value.index()];
            if origin == crate::analysis::NO_ORIGIN {
                return;
            }
            let allowed = match inst.op {
                Op::Reinterpret(_) | Op::Refine(_) => true,
                Op::CopyValue(source) => {
                    body.value_type(source) == body.value_type(inst.result.unwrap())
                }
                Op::GetField { object, field } => {
                    object == value
                        && schemas[&body.value_type(values[origin as usize])]
                            .as_ref()
                            .unwrap()
                            .contains(&body.fields[field.index()])
                }
                _ => false,
            };
            readonly[origin as usize] &= allowed;
        });
    }
    let mut escape = |value: ValueId| {
        let origin = origins[value.index()];
        if origin != crate::analysis::NO_ORIGIN {
            readonly[origin as usize] = false;
        }
    };
    for block in &body.blocks {
        block.terminator.unwrap().visit_uses(&mut escape);
    }
    for edge in &body.edges {
        for (&value, &param) in edge
            .args
            .iter()
            .zip(&body.blocks[edge.target.index()].params)
        {
            if origins[value.index()] != origins[param.index()] {
                escape(value);
            }
        }
    }
    for inst in &mut body.instructions {
        let Op::CopyValue(source) = inst.op else {
            continue;
        };
        let a = origins[source.index()];
        let b = origins[inst.result.unwrap().index()];
        if a != crate::analysis::NO_ORIGIN
            && b != crate::analysis::NO_ORIGIN
            && readonly[a as usize]
            && readonly[b as usize]
        {
            inst.op = Op::Reinterpret(source);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scalar::{Scalar, ScalarType};

    #[test]
    fn snapshots_share_across_blocks_only_while_both_remain_private_and_readonly() {
        for array_read in [false, true] {
            for mode in ["plain", "source_write", "copy_write", "escape", "opaque"] {
                let mut types = Types::default();
                let int = types.scalar(ScalarType::I64);
                let name = types.symbol("Pair");
                let pair = types.intern(Type::Class(name));
                let array = types.intern(Type::Array(pair));
                let index = types.scalar(ScalarType::I32);
                let mut b = Builder::new(&types, int);
                let input = b.parameter(b.current(), if array_read { array } else { pair });
                let zero = b.constant(int, Scalar::integer(ScalarType::I64, 0).unwrap());
                let field = FieldRef {
                    owner: pair,
                    name: "x".into(),
                    ty: int,
                    is_static: false,
                };
                let member = b.field(field.clone());
                let source = if array_read {
                    let index = b.constant(index, Scalar::integer(ScalarType::I32, 0).unwrap());
                    b.emit(
                        Op::ArrayGetCopy {
                            array: input,
                            index,
                        },
                        Some(pair),
                    )
                    .unwrap()
                } else {
                    b.emit(Op::CopyValue(input), Some(pair)).unwrap()
                };
                let copy = b.emit(Op::CopyValue(source), Some(pair)).unwrap();
                let ValueDef::Inst(copy_id) = b.body.values[copy.index()].def else {
                    panic!()
                };
                let next = b.create_block();
                b.jump(next, vec![]);
                b.switch_to(next);
                if mode.ends_with("write") {
                    b.emit(
                        Op::SetField {
                            object: if mode == "copy_write" { copy } else { source },
                            field: member,
                            value: zero,
                        },
                        None,
                    );
                }
                if mode == "escape" || mode == "opaque" {
                    let args = b.args(if mode == "escape" { vec![copy] } else { vec![] });
                    let method = b.method(MethodRef {
                        owner: "Opaque".into(),
                        name: "call".into(),
                        params: if mode == "escape" { vec![pair] } else { vec![] },
                        returns: int,
                        interface: false,
                    });
                    b.emit(
                        Op::Call {
                            method,
                            kind: CallKind::JvmStatic,
                            args,
                        },
                        Some(int),
                    );
                }
                b.emit(
                    Op::GetField {
                        object: source,
                        field: member,
                    },
                    Some(int),
                );
                let result = b
                    .emit(
                        Op::GetField {
                            object: copy,
                            field: member,
                        },
                        Some(int),
                    )
                    .unwrap();
                b.terminate(Terminator::Return(Some(result)));
                let mut body = b.finish().unwrap();
                reuse_readonly_copies(&mut body, &types, &mut |_| Some(vec![field.clone()]));
                verify(&body, &types).unwrap();
                assert_eq!(
                    matches!(body.instructions[copy_id.index()].op, Op::Reinterpret(_)),
                    mode == "plain" || mode == "opaque",
                    "{mode} array={array_read}"
                );
                crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
            }
        }
    }
}
