//! Split stored borrowed values using the same shapes as parameters and SSA joins.
use crate::ir::*;
use crate::jvm::abi::{address_displacement_name, view_field_names};
use rustc_hash::FxHashMap;

fn append(
    body: &mut Body,
    op: Op,
    ty: Option<TypeId>,
    prefix: &mut Vec<InstId>,
) -> Option<ValueId> {
    let id = InstId::new(body.instructions.len());
    let result = ty.map(|ty| {
        let value = ValueId::new(body.values.len());
        body.values.push(Value {
            ty,
            def: ValueDef::Inst(id),
        });
        value
    });
    body.instructions.push(Inst { op, result });
    prefix.push(id);
    result
}

pub fn lower_borrowed_fields(
    body: &mut Body,
    types: &mut Types,
    split: impl Fn(&str) -> bool,
    debug: Option<&mut DebugInfo>,
) {
    let constructors = body
        .methods
        .iter()
        .map(|method| method.name == "<init>" && split(&method.owner))
        .collect::<Vec<_>>();
    let fields = (0..body.fields.len())
        .map(|index| {
            let field = body.fields[index].clone();
            let Type::Class(owner) = types.get(field.owner)? else {
                return None;
            };
            if field.is_static || !split(types.symbol_name(owner)?) {
                return None;
            }
            let shape = ComponentShape::of(types, field.ty)?;
            let names = match types.get(field.ty)? {
                Type::TaggedI64 => crate::jvm::abi::tagged_field_names(&field.name).to_vec(),
                Type::Pointer(_) => vec![
                    field.name.clone(),
                    address_displacement_name(types, field.ty, &field.name),
                ],
                ty => view_field_names(&field.name, matches!(ty, Type::Str)).to_vec(),
            };
            let members = names
                .into_iter()
                .zip(shape.parts(types))
                .map(|(name, ty)| {
                    let id = MemberId::new(body.fields.len());
                    body.fields.push(FieldRef {
                        name,
                        ty,
                        ..field.clone()
                    });
                    id
                })
                .collect::<Vec<_>>();
            Some((shape, members))
        })
        .collect::<Vec<_>>();
    if fields.iter().all(Option::is_none) && !constructors.iter().any(|&v| v) {
        return;
    }
    let mut prefixes = FxHashMap::default();
    for index in 0..body.instructions.len() {
        let mut prefix = Vec::new();
        let original = body.instructions[index].op;
        let replacement = match original {
            Op::GetField { .. }
            | Op::SetField { .. }
            | Op::LoadField { .. }
            | Op::StoreField { .. } => {
                // Projected fields carry a projection ID instead of a member ID.
                let member = match original {
                    Op::LoadField { projection, .. } | Op::StoreField { projection, .. } => {
                        body.projections[projection.index()].field
                    }
                    Op::GetField { field, .. } | Op::SetField { field, .. } => field,
                    _ => unreachable!(),
                };
                let Some((shape, physical)) = fields.get(member.index()).and_then(Option::as_ref)
                else {
                    continue;
                };
                let mut values = Vec::new();
                for (index, &field) in physical.iter().enumerate() {
                    let ty = body.fields[field.index()].ty;
                    let op = match original {
                        Op::GetField { object, .. } => Op::GetField { object, field },
                        Op::LoadField { base, projection } => Op::LoadFieldPart {
                            base,
                            projection,
                            index: index as u8,
                        },
                        Op::SetField { value, .. } | Op::StoreField { value, .. } => {
                            shape.part(value, index as u8)
                        }
                        _ => unreachable!(),
                    };
                    let value = append(body, op, Some(ty), &mut prefix).unwrap();
                    values.push(value);
                    if let Op::SetField { object, .. } = original {
                        append(
                            body,
                            Op::SetField {
                                object,
                                field,
                                value,
                            },
                            None,
                            &mut prefix,
                        );
                    }
                }
                match original {
                    Op::SetField { .. } => Op::Nop,
                    Op::StoreField {
                        base, projection, ..
                    } => Op::StoreFieldParts {
                        base,
                        projection,
                        parts: List::append(&mut body.args, values),
                    },
                    _ => shape.pack(List::append(&mut body.args, values)),
                }
            }
            Op::Call {
                method,
                kind: CallKind::Constructor,
                args,
            } if constructors[method.index()] => {
                let original = body.args[args.range()].to_vec();
                let params = body.methods[method.index()].params.clone();
                let mut values = Vec::new();
                for (value, ty) in original.into_iter().zip(params) {
                    if let Some(shape) = ComponentShape::of(types, ty) {
                        for (index, ty) in shape.parts(types).enumerate() {
                            values.push(
                                append(body, shape.part(value, index as u8), Some(ty), &mut prefix)
                                    .unwrap(),
                            );
                        }
                    } else {
                        values.push(value);
                    }
                }
                Op::Call {
                    method,
                    kind: CallKind::Constructor,
                    args: List::append(&mut body.args, values),
                }
            }
            _ => continue,
        };
        body.instructions[index].op = replacement;
        prefixes.insert(InstId::new(index), prefix);
    }
    for (method, split) in body.methods.iter_mut().zip(constructors) {
        if split {
            let previous = std::mem::take(&mut method.params);
            for ty in previous {
                if let Some(shape) = ComponentShape::of(types, ty) {
                    method.params.extend(shape.parts(types));
                } else {
                    method.params.push(ty);
                }
            }
        }
    }
    let mut positions = debug.as_ref().map(|_| Vec::new());
    for block in &mut body.blocks {
        let previous = std::mem::take(&mut block.instructions);
        let mut mapping = Vec::new();
        for id in previous {
            mapping.push(block.instructions.len() as u32);
            if let Some(prefix) = prefixes.remove(&id) {
                block.instructions.extend(prefix);
            }
            block.instructions.push(id);
        }
        mapping.push(block.instructions.len() as u32);
        if let Some(positions) = &mut positions {
            positions.push(mapping);
        }
        if let Some(Terminator::Invoke { inst, normal, .. }) = block.terminator {
            if let Some(prefix) = prefixes.remove(&inst) {
                block.instructions.extend(prefix);
            }
            if matches!(
                body.instructions[inst.index()].op,
                Op::Nop | Op::AddressPack(_) | Op::ViewPack(_) | Op::TaggedPack(_)
            ) {
                block.instructions.push(inst);
                block.terminator = Some(Terminator::Jump(normal));
            }
        }
    }
    if let (Some(debug), Some(positions)) = (debug, positions) {
        for event in &mut debug.events {
            event.position = positions[event.block.index()][event.position as usize];
        }
    }
}
