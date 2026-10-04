//! Remove handlers that only pass the caught exception to the caller.
use crate::ir::*;
use rustc_hash::FxHashSet;

pub fn remove_rethrows(body: &mut Body, types: &Types) {
    let mut known = Vec::new();
    for index in 0..body.blocks.len() {
        let Some(term) = body.blocks[index].terminator else {
            continue;
        };
        let unwind = match term {
            Terminator::Invoke { unwind, .. }
            | Terminator::Throw {
                unwind: Some(unwind),
                ..
            } => unwind,
            _ => continue,
        };
        if known.is_empty() {
            known.resize(body.blocks.len(), None);
        }
        let target = body.edges[unwind.index()].target;
        let removable = *known[target.index()].get_or_insert_with(|| rethrows(body, types, target));
        if !removable {
            continue;
        }
        body.blocks[index].terminator = Some(match term {
            Terminator::Invoke { inst, normal, .. } => {
                body.blocks[index].instructions.push(inst);
                Terminator::Jump(normal)
            }
            Terminator::Throw { value, .. } => Terminator::Throw {
                value,
                unwind: None,
            },
            _ => unreachable!(),
        });
    }
}

fn rethrows(body: &Body, types: &Types, mut block: BlockId) -> bool {
    let mut caught = FxHashSet::default();
    // A cycle can do work or never return. Leave it unchanged.
    for _ in 0..body.blocks.len().min(64) {
        let data = &body.blocks[block.index()];
        for &id in &data.instructions {
            let inst = body.instructions[id.index()];
            let identity = match inst.op {
                Op::Nop => false,
                Op::Constant(id)
                    if matches!(
                        body.constants[id.index()],
                        Constant::Scalar(_) | Constant::Null(_) | Constant::Uninit(_)
                    ) =>
                {
                    false
                }
                Op::Exception => true,
                op @ (Op::Reinterpret(source) | Op::Refine(source) | Op::Adapt(source)) => {
                    let ty = body.value_type(inst.result.unwrap());
                    let identity = caught.contains(&body.resolve(source));
                    let safe_cast = identity
                        && matches!(types.get(ty), Some(Type::Class(name))
                            if matches!(types.symbol_name(name),
                                Some("java/lang/Throwable" | "java/lang/Object")));
                    if !safe_cast && (matches!(op, Op::Adapt(_)) || ty != body.value_type(source)) {
                        return false;
                    }
                    identity
                }
                _ => return false,
            };
            if identity {
                caught.insert(body.resolve(inst.result.unwrap()));
            }
        }
        match data.terminator.unwrap() {
            Terminator::Throw {
                value,
                unwind: None,
            } => return caught.contains(&body.resolve(value)),
            Terminator::Jump(edge) => {
                let edge = &body.edges[edge.index()];
                let incoming = edge
                    .args
                    .iter()
                    .map(|&v| caught.contains(&body.resolve(v)))
                    .collect::<Vec<_>>();
                for (&param, identity) in
                    body.blocks[edge.target.index()].params.iter().zip(incoming)
                {
                    if identity {
                        caught.insert(body.resolve(param));
                    } else {
                        caught.remove(&body.resolve(param));
                    }
                }
                block = edge.target;
            }
            _ => return false,
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scalar::{Scalar, ScalarType};

    #[test]
    fn remove_only_identity_unwinds_after_constant_cleanup_branches() {
        for mode in [
            "rethrow", "upcast", "cleanup", "replace", "cast", "cycle", "return",
        ] {
            let mut types = Types::default();
            let name = types.symbol("java/lang/Throwable");
            let throwable = types.intern(Type::Class(name));
            let name = types.symbol("java/lang/Object");
            let object = types.intern(Type::Class(name));
            let name = types.symbol("java/lang/IllegalArgumentException");
            let narrow = types.intern(Type::Class(name));
            let int = types.scalar(ScalarType::I32);
            let boolean = types.scalar(ScalarType::Bool);
            let mut b = Builder::new(&types, int);
            let other = b.parameter(b.current(), throwable);
            let method = b.method(MethodRef {
                owner: "Example".into(),
                name: "effect".into(),
                params: vec![],
                returns: int,
                interface: false,
            });
            let args = b.args(vec![]);
            let call = Op::Call {
                method,
                kind: CallKind::JvmStatic,
                args,
            };
            let handler = b.create_block();
            let value = b.invoke(call, Some(int), handler).unwrap();
            b.terminate(Terminator::Return(Some(value)));
            b.switch_to(handler);
            let caught = b.emit(Op::Exception, Some(throwable)).unwrap();
            let cleanup = b.create_block();
            let exception = b.parameter(cleanup, throwable);
            let dead = b.create_block();
            let condition = b.constant(boolean, Scalar::boolean(false));
            let yes = b.edge(dead, vec![]);
            let no = b.edge(cleanup, vec![caught]);
            b.terminate(Terminator::Branch { condition, yes, no });
            b.switch_to(dead);
            b.emit(call, Some(int));
            b.terminate(Terminator::Unreachable);
            b.switch_to(cleanup);
            if mode == "cleanup" {
                b.emit(call, Some(int));
            }
            let exception = if mode == "cast" {
                b.emit(Op::Refine(exception), Some(narrow)).unwrap()
            } else if mode == "upcast" {
                let erased = b.emit(Op::Adapt(exception), Some(object)).unwrap();
                b.emit(Op::Adapt(erased), Some(throwable)).unwrap()
            } else if mode == "replace" {
                other
            } else {
                exception
            };
            if mode == "cycle" {
                b.jump(cleanup, vec![exception]);
            } else if mode == "return" {
                let zero = b.constant(int, Scalar::integer(ScalarType::I32, 0).unwrap());
                b.terminate(Terminator::Return(Some(zero)));
            } else {
                b.terminate(Terminator::Throw {
                    value: exception,
                    unwind: None,
                });
            }
            let mut body = b.finish().unwrap();
            super::super::simplify_components(&mut body, &types);
            remove_rethrows(&mut body, &types);
            verify(&body, &types).unwrap();
            assert_eq!(
                matches!(
                    body.blocks[body.entry.index()].terminator,
                    Some(Terminator::Jump(_))
                ),
                mode == "rethrow" || mode == "upcast",
                "{mode}"
            );
            crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        }
    }
}
