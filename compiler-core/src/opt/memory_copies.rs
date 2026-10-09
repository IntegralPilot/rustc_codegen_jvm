//! Pass address components to byte-copy intrinsics.
//! The runtime handles overlap, alias tracking and encoded references.
use super::address_parts::address_parts;
use crate::ir::*;

pub fn lower_memory_copies(body: &mut Body, types: &mut Types) {
    for index in 0..body.instructions.len() {
        let inst = body.instructions[index];
        let Op::Call {
            method,
            kind: CallKind::JvmStatic,
            args,
        } = inst.op
        else {
            continue;
        };
        let method = &body.methods[method.index()];
        if method.owner != "org/rustlang/runtime/Pointer"
            || !matches!(method.name.as_str(), "copy" | "copyNonOverlapping")
            || inst.result.is_some()
            || args.len != 3
        {
            continue;
        }
        let nonoverlapping = method.name == "copyNonOverlapping";
        let args = &body.args[args.range()];
        let count = args[2];
        let (Some(source), Some(destination)) = (
            address_parts(body, types, args[0]),
            address_parts(body, types, args[1]),
        ) else {
            continue;
        };
        let (Some((a, ac)), Some((b, bc))) = (source.layout, destination.layout) else {
            continue;
        };
        let layouts = [(args[0], a, ac), (args[1], b, bc)].map(|(value, size, codec)| {
            types.layout(AddressLayout {
                value: types.pointee(body.value_type(value)).unwrap(),
                size,
                codec,
            })
        });
        let mut parts = body.args[source.parts.range()].to_vec();
        parts.extend_from_slice(&body.args[destination.parts.range()]);
        parts.push(count);
        body.instructions[index].op = Op::CopyStorage {
            parts: List::append(&mut body.args, parts),
            layouts,
            nonoverlapping,
        };
    }
}
