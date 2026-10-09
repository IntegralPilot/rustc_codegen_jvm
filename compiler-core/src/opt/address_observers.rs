//! Observe address components without a Pointer carrier.
//! Keep calls in place to retain provenance and alignment checks under their original handlers.
use super::address_parts::{AddressParts, address_parts};
use crate::ir::*;
use crate::scalar::{Scalar, ScalarType};
use rustc_hash::FxHashMap;

const OWNER: &str = "org/rustlang/runtime/Pointer";

fn stride(address: &AddressParts, types: &Types) -> i64 {
    if let Some((size, _)) = address.layout {
        return i64::from(size);
    }
    match address.pointee.and_then(|ty| types.get(ty)) {
        Some(Type::Pointer(_)) => 8,
        Some(Type::Slice(_) | Type::Str) => 16,
        // Resolve the root's layout in the runtime helper.
        // Keep the original pointer operation's exception handler.
        _ => -1,
    }
}

pub fn lower_address_observers(body: &mut Body, types: &mut Types, debug: Option<&mut DebugInfo>) {
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let long = types.scalar(ScalarType::I64);
    let mut constants = FxHashMap::default();
    let mut prologue = Vec::new();
    let mut constant = |body: &mut Body, bits: i64| {
        *constants.entry(bits).or_insert_with(|| {
            let id = ConstId::new(body.constants.len());
            body.constants.push(Constant::Scalar(
                Scalar::integer(ScalarType::I64, bits as u128).unwrap(),
            ));
            let id_inst = InstId::new(body.instructions.len());
            let result = ValueId::new(body.values.len());
            body.values.push(Value {
                ty: long,
                def: ValueDef::Inst(id_inst),
            });
            body.instructions.push(Inst {
                op: Op::Constant(id),
                result: Some(result),
            });
            prologue.push(id_inst);
            result
        })
    };
    for index in 0..body.instructions.len() {
        let Op::Call { method, kind, args } = body.instructions[index].op else {
            continue;
        };
        let method = &body.methods[method.index()];
        if method.owner != OWNER
            || !matches!(kind, CallKind::JvmStatic | CallKind::Virtual)
            || !matches!(
                types.get(method.returns),
                Some(Type::Scalar(ScalarType::I64 | ScalarType::U64))
            )
        {
            continue;
        }
        let name = match method.name.as_str() {
            "addr" => "locationAddr",
            "offset_from" | "offsetFrom" => "offsetLocations",
            "offset_from_unsigned" => "offsetLocationsUnsigned",
            "byte_offset_from" => "byteOffsetLocations",
            "byte_offset_from_unsigned" => "byteOffsetLocationsUnsigned",
            "align_offset" => "alignLocation",
            _ => continue,
        };
        let count = if name == "locationAddr" { 1 } else { 2 };
        if args.len as usize != count
            || method.params.len() + usize::from(kind == CallKind::Virtual) != count
        {
            continue;
        }
        let returns = method.returns;
        let args = body.args[args.range()].to_vec();
        let first = address_parts(body, types, args[0]);
        let distance = name.contains("Offset") || name.starts_with("offset");
        let second = distance
            .then(|| address_parts(body, types, args[1]))
            .flatten();
        if first.is_none() || (distance && second.is_none()) {
            continue;
        }
        if name == "alignLocation" && types.get(body.value_type(args[1])).unwrap().carrier() != 2 {
            continue;
        }
        let step = first.as_ref().map_or(-1, |address| stride(address, types));
        let parts = |address: Option<AddressParts>| -> [ValueId; 2] {
            body.args[address.unwrap().parts.range()]
                .try_into()
                .unwrap()
        };
        let mut values = parts(first).to_vec();
        let mut params = vec![object, long];
        if distance {
            values.extend(parts(second));
            params.extend([object, long]);
        }
        if name.starts_with("offset") || name == "alignLocation" {
            values.push(constant(body, step));
            params.push(long);
        }
        if name == "alignLocation" {
            values.push(args[1]);
            params.push(body.value_type(args[1]));
        }
        let method = MethodId::new(body.methods.len());
        body.methods.push(MethodRef {
            owner: OWNER.into(),
            name: name.into(),
            params,
            returns,
            interface: false,
        });
        body.instructions[index].op = Op::Call {
            method,
            kind: CallKind::JvmStatic,
            args: List::append(&mut body.args, values),
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
