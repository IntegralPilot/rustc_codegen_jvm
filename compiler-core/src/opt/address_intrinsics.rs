//! Use address components for intrinsics with explicit byte widths.
//! Runtime helpers retain synchronization and memory tracking.
use super::address_parts::address_parts;
use crate::ir::*;
use crate::scalar::ScalarType;

fn arity(name: &str) -> Option<usize> {
    Some(match name {
        "atomicLoad" | "writeBytes" => 3,
        "atomicStore" | "atomicExchange" | "atomicAdd" | "atomicSubtract" | "atomicAnd"
        | "atomicNand" | "atomicOr" | "atomicXor" | "atomicMax" | "atomicMin"
        | "atomicUnsignedMax" | "atomicUnsignedMin" => 4,
        "atomicCompareExchange" => 6,
        _ => return None,
    })
}

pub fn lower_address_intrinsics(body: &mut Body, types: &mut Types) {
    let mut component_types = None;
    for index in 0..body.instructions.len() {
        let Op::Call {
            method,
            kind: CallKind::JvmStatic,
            args,
        } = body.instructions[index].op
        else {
            continue;
        };
        let target = &body.methods[method.index()];
        if target.owner != "org/rustlang/runtime/Pointer"
            || arity(&target.name) != Some(args.len as usize)
            || target.params.len() != args.len as usize
            || !matches!(types.get(target.params[0]), Some(Type::Pointer(_)))
        {
            continue;
        }
        if target.name == "writeBytes"
            && !matches!(
                types.get(target.params[2]),
                Some(Type::Scalar(ScalarType::I64 | ScalarType::U64))
            )
        {
            continue;
        }
        let values = &body.args[args.range()];
        let Some(address) = address_parts(body, types, values[0]) else {
            continue;
        };
        let mut new_args = body.args[address.parts.range()].to_vec();
        new_args.extend_from_slice(&values[1..]);
        let mut target = target.clone();
        let [object, long] = *component_types.get_or_insert_with(|| {
            let object = types.symbol("java/lang/Object");
            [
                types.intern(Type::Class(object)),
                types.scalar(ScalarType::I64),
            ]
        });
        target.params.splice(..1, [object, long]);
        let method = MethodId::new(body.methods.len());
        body.methods.push(target);
        body.instructions[index].op = Op::Call {
            method,
            kind: CallKind::JvmStatic,
            args: List::append(&mut body.args, new_args),
        };
    }
}
