//! Keep typed allocations as storage owners instead of eager address wrappers.
//! Private cells already use SSA. Escaping locations retain one owner,
//! including when a store replaces their contents.
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};
use rustc_hash::FxHashMap;

/// The frontend checks positive size, alignment and exact initial contents.
/// It also requires a direct carrier with no DST metadata.
pub fn lower_typed_storage(
    body: &mut Body,
    types: &mut Types,
    cells: &[(ValueId, ValueId)],
    debug: Option<&mut DebugInfo>,
) {
    if cells.is_empty() {
        return;
    }
    let mut parts = ComponentShape::StorageAddress.parts(types);
    let object = parts.next().unwrap();
    let long = parts.next().unwrap();
    let mut prefixes = FxHashMap::default();
    let mut suffixes = FxHashMap::default();
    for &(cell, _) in cells {
        let ValueDef::Inst(id) = body.values[cell.index()].def else {
            continue;
        };
        let mut suffix = Vec::new();
        let (op, root_ty, initial) = match body.instructions[id.index()].op {
            Op::ScalarCell(initial) => {
                let int = types.scalar(ScalarType::I32);
                let (one_inst, one) = integer(body, int, ScalarType::I32, 1);
                prefixes.insert(id, one_inst);
                (
                    Op::NewArray(one),
                    types.intern(Type::Array(body.value_type(initial))),
                    Some(initial),
                )
            }
            Op::Call {
                method,
                kind: CallKind::JvmStatic,
                args,
            } => {
                let mut method = body.methods[method.index()].clone();
                if method.owner != "org/rustlang/runtime/Pointer" {
                    continue;
                }
                let borrowed = match types.get(body.value_type(cell)) {
                    Some(Type::Pointer(inner)) => match types.get(inner) {
                        Some(Type::Pointer(_)) => Some(0),
                        Some(Type::Slice(_)) => Some(1),
                        Some(Type::Str) => Some(2),
                        _ => None,
                    },
                    _ => None,
                };
                method.name = match (method.name.as_str(), borrowed.is_some()) {
                    ("cell", false) => "storage",
                    ("cellAligned", false) => "storageAligned",
                    ("cell", true) => "borrowedStorage",
                    ("cellAligned", true) => "borrowedStorageAligned",
                    _ => continue,
                }
                .into();
                let args = if let Some(kind) = borrowed {
                    let int = types.scalar(ScalarType::I32);
                    let (inst, kind) = integer(body, int, ScalarType::I32, kind);
                    prefixes.insert(id, inst);
                    method.params.push(int);
                    let mut values = body.args[args.range()].to_vec();
                    values.push(kind);
                    List::append(&mut body.args, values)
                } else {
                    args
                };
                method.returns = object;
                let method_id = MethodId::new(body.methods.len());
                body.methods.push(method);
                (
                    Op::Call {
                        method: method_id,
                        kind: CallKind::JvmStatic,
                        args,
                    },
                    object,
                    None,
                )
            }
            _ => continue,
        };
        let root = ValueId::new(body.values.len());
        body.values.push(Value {
            ty: root_ty,
            def: ValueDef::Inst(id),
        });
        body.instructions[id.index()] = Inst {
            op,
            result: Some(root),
        };
        let root = if let Some(initial) = initial {
            let int = types.scalar(ScalarType::I32);
            let (index_inst, index) = integer(body, int, ScalarType::I32, 0);
            suffix.push(index_inst);
            suffix.push(InstId::new(body.instructions.len()));
            body.instructions.push(Inst {
                op: Op::ArraySet {
                    native: false,
                    array: root,
                    index,
                    value: initial,
                },
                result: None,
            });
            let cast = InstId::new(body.instructions.len());
            let erased = ValueId::new(body.values.len());
            body.values.push(Value {
                ty: object,
                def: ValueDef::Inst(cast),
            });
            body.instructions.push(Inst {
                op: Op::Reinterpret(root),
                result: Some(erased),
            });
            suffix.push(cast);
            erased
        } else {
            root
        };
        let (zero_inst, zero) = integer(body, long, ScalarType::I64, 0);
        let pack = InstId::new(body.instructions.len());
        body.values[cell.index()].def = ValueDef::Inst(pack);
        body.instructions.push(Inst {
            op: Op::AddressPack(List::append(&mut body.args, [root, zero])),
            result: Some(cell),
        });
        suffix.extend([zero_inst, pack]);
        suffixes.insert(id, suffix);
    }
    if suffixes.is_empty() {
        return;
    }
    let mut positions = debug.as_ref().map(|_| Vec::new());
    for index in 0..body.blocks.len() {
        let previous = std::mem::take(&mut body.blocks[index].instructions);
        let mut mapping = Vec::new();
        for id in previous {
            mapping.push(body.blocks[index].instructions.len() as u32);
            if let Some(prefix) = prefixes.remove(&id) {
                body.blocks[index].instructions.push(prefix);
            }
            body.blocks[index].instructions.push(id);
            if let Some(suffix) = suffixes.remove(&id) {
                body.blocks[index].instructions.extend(suffix);
            }
        }
        mapping.push(body.blocks[index].instructions.len() as u32);
        if let Some(positions) = &mut positions {
            positions.push(mapping);
        }
        if let Some(Terminator::Invoke {
            inst,
            normal,
            unwind,
        }) = body.blocks[index].terminator
            && let Some(suffix) = suffixes.remove(&inst)
        {
            if let Some(prefix) = prefixes.remove(&inst) {
                body.blocks[index].instructions.push(prefix);
            }
            // Keep allocation in its original unwind region.
            // Pack its result on the normal edge before copies consume the address.
            let continuation = BlockId::new(body.blocks.len());
            body.blocks.push(Block {
                params: Vec::new(),
                instructions: suffix,
                terminator: Some(Terminator::Jump(normal)),
            });
            let edge = EdgeId::new(body.edges.len());
            body.edges.push(Edge {
                target: continuation,
                args: Vec::new(),
            });
            body.blocks[index].terminator = Some(Terminator::Invoke {
                inst,
                normal: edge,
                unwind,
            });
        }
    }
    if let (Some(debug), Some(positions)) = (debug, positions) {
        for event in &mut debug.events {
            event.position = positions[event.block.index()][event.position as usize];
        }
    }
}

fn integer(body: &mut Body, ty: TypeId, scalar: ScalarType, value: u128) -> (InstId, ValueId) {
    let constant = ConstId::new(body.constants.len());
    body.constants
        .push(Constant::Scalar(Scalar::integer(scalar, value).unwrap()));
    super::append_value(body, Op::Constant(constant), ty)
}
