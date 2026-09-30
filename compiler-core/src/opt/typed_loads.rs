//! Remove exact-layout carriers used only for one borrowed object read.
//! Shared carriers retain their binding for later commits.
use super::address_parts::address_parts;
use crate::ir::*;

fn sole_view_consumer(body: &Body, uses: &[u8], mut value: ValueId) -> bool {
    for _ in 0..32 {
        value = body.resolve(value);
        if uses[value.index()] != 1 {
            return false;
        }
        let ValueDef::Inst(id) = body.values[value.index()].def else {
            return false;
        };
        match body.instructions[id.index()].op {
            Op::AddressPack(_) | Op::TypedAddressPack { .. } => return true,
            Op::Refine(source) | Op::Reinterpret(source) => value = source,
            _ => return false,
        }
    }
    false
}

pub fn lower_typed_loads(body: &mut Body, types: &Types) {
    let candidates = body
        .instructions
        .iter()
        .enumerate()
        .filter_map(|(index, inst)| {
            let result = body.value_type(inst.result?);
            if let Op::Load(pointer) = inst.op {
                let eligible = match types.get(result) {
                    Some(Type::Class(name) | Type::Interface(name)) => {
                        types.symbol_name(name) != Some("java/lang/Object")
                    }
                    Some(Type::Array(_)) => true,
                    _ => false,
                };
                return eligible.then_some((index, pointer, None));
            }
            let Op::Call {
                method,
                kind: CallKind::Virtual,
                args,
            } = inst.op
            else {
                return None;
            };
            let method = &body.methods[method.index()];
            let named = method.name == "getObjectAs" && args.len == 2;
            // getObject's null target differs from getObjectAs(Object).
            // Preserve this distinction when inferring load targets.
            let untyped = method.name == "getObject"
                && args.len == 1
                && matches!(types.get(result), Some(Type::Class(name))
                    if types.symbol_name(name) == Some("java/lang/Object"));
            (method.owner == "org/rustlang/runtime/Pointer" && (named || untyped)).then(|| {
                (
                    index,
                    body.args[args.start as usize],
                    named.then(|| body.args[args.start as usize + 1]),
                )
            })
        })
        .collect::<Vec<_>>();
    if candidates.is_empty() {
        return;
    }
    let mut uses = vec![0_u8; body.values.len()];
    let mut use_value = |value: ValueId| {
        let count = &mut uses[body.resolve(value).index()];
        *count = count.saturating_add(1);
    };
    for inst in &body.instructions {
        inst.op.visit_uses(&body.args, &mut use_value);
    }
    for block in &body.blocks {
        block.terminator.unwrap().visit_uses(&mut use_value);
    }
    for edge in &body.edges {
        for &value in &edge.args {
            use_value(value);
        }
    }
    for (index, receiver, target) in candidates {
        if !sole_view_consumer(body, &uses, receiver) {
            continue;
        }
        let Some(address) = address_parts(body, types, receiver) else {
            continue;
        };
        let Some((size, codec)) = address.layout else {
            continue;
        };
        let mut parts = body.args[address.parts.range()].to_vec();
        parts.extend(target);
        body.instructions[index].op = Op::LoadTyped {
            parts: List::append(&mut body.args, parts),
            size,
            codec,
        };
    }
}
