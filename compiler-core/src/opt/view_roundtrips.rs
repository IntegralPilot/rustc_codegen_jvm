//! Recover slice components from exact, bounded address conversions.
use crate::ir::*;
use crate::scalar::{BinaryOp, Scalar, ScalarType};

fn index_from_offset(body: &Body, types: &Types, offset: ValueId, size: u32) -> Option<Folded> {
    let mut offset = body.resolve(offset);
    if let Some(value) = body.scalar_value(offset).and_then(Scalar::signed) {
        if value % i128::from(size) != 0 {
            return None;
        }
        let index = i32::try_from(value / i128::from(size)).ok()?;
        return Some(Folded::Constant(
            Scalar::integer(ScalarType::I32, index as u128).unwrap(),
        ));
    }
    if size != 1 {
        let ValueDef::Inst(id) = body.values[offset.index()].def else {
            return None;
        };
        let Op::Binary {
            op: BinaryOp::Mul,
            left,
            right,
        } = body.instructions[id.index()].op
        else {
            return None;
        };
        offset = body.resolve(
            if body.scalar_value(right).map(Scalar::bits) == Some(size.into()) {
                left
            } else if body.scalar_value(left).map(Scalar::bits) == Some(size.into()) {
                right
            } else {
                return None;
            },
        );
    }
    let ValueDef::Inst(id) = body.values[offset.index()].def else {
        return None;
    };
    let Op::Cast(index) = body.instructions[id.index()].op else {
        return None;
    };
    (types.get(body.value_type(index)) == Some(Type::Scalar(ScalarType::I32)))
        .then_some(Folded::Value(index))
}

pub fn fold_view_roundtrips(body: &mut Body, types: &Types) -> bool {
    let mut changed = false;
    for position in 0..body.instructions.len() {
        let Op::TypedAddressViewPart {
            parts,
            size,
            codec: Some(codec),
            index,
        } = body.instructions[position].op
        else {
            continue;
        };
        if size == 0 || size > i32::MAX as u32 {
            continue;
        }
        let args = &body.args[parts.range()];
        let root = body.resolve(args[0]);
        let ValueDef::Inst(id) = body.values[root.index()].def else {
            continue;
        };
        if !matches!(body.instructions[id.index()].op,
            Op::ViewRoot { size: actual, codec: Some(recipe), .. }
                if actual == size && recipe == codec)
        {
            continue;
        }
        // Normalization fixes the root layout. The scaled i32 index cannot overflow i64.
        let Some(start) = index_from_offset(body, types, args[1], size) else {
            continue;
        };
        body.instructions[position].op = match (index, start) {
            (0, _) => Op::Reinterpret(root),
            (1, Folded::Value(value)) => Op::Reinterpret(value),
            (1, Folded::Constant(value)) => {
                let id = ConstId::new(body.constants.len());
                body.constants.push(Constant::Scalar(value));
                Op::Constant(id)
            }
            _ => continue,
        };
        changed = true;
    }
    changed
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrips_require_exact_layout_and_a_bounded_element_index() {
        for case in [
            "signed",
            "unsigned",
            "unaligned",
            "oversized",
            "codec",
            "stride",
        ] {
            let mut types = Types::default();
            let int = types.scalar(ScalarType::I32);
            let long = types.scalar(ScalarType::I64);
            let source_type = types.scalar(if case == "unsigned" {
                ScalarType::U32
            } else {
                ScalarType::I32
            });
            let name = types.symbol("java/lang/Object");
            let object = types.intern(Type::Class(name));
            let codec = types.symbol("PairCodec#pair#LPair;#16");
            let other = types.symbol("OtherCodec#pair#LPair;#16");
            let mut b = Builder::new(&types, int);
            let backing = b.parameter(b.current(), object);
            let start = b.parameter(b.current(), source_type);
            let root = b
                .emit(
                    Op::ViewRoot {
                        backing,
                        size: 16,
                        codec: Some(codec),
                    },
                    Some(object),
                )
                .unwrap();
            let wide = b.emit(Op::Cast(start), Some(long)).unwrap();
            let stride = b.constant(
                long,
                Scalar::integer(ScalarType::I64, if case == "stride" { 8 } else { 16 }).unwrap(),
            );
            let offset = match case {
                "unaligned" | "oversized" => b.constant(
                    long,
                    Scalar::integer(
                        ScalarType::I64,
                        if case == "unaligned" {
                            1
                        } else {
                            (i32::MAX as u128 + 1) * 16
                        },
                    )
                    .unwrap(),
                ),
                _ => b
                    .emit(
                        Op::Binary {
                            op: BinaryOp::Mul,
                            left: wide,
                            right: stride,
                        },
                        Some(long),
                    )
                    .unwrap(),
            };
            let parts = b.args([root, offset]);
            let read = b
                .emit(
                    Op::TypedAddressViewPart {
                        parts,
                        size: 16,
                        codec: Some(if case == "codec" { other } else { codec }),
                        index: 1,
                    },
                    Some(int),
                )
                .unwrap();
            b.terminate(Terminator::Return(Some(read)));
            let mut body = b.finish().unwrap();
            assert_eq!(
                fold_view_roundtrips(&mut body, &types),
                case == "signed",
                "{case}"
            );
            super::super::simplify_components(&mut body, &types);
            verify(&body, &types).unwrap();
            if case == "signed" {
                assert_eq!(body.resolve(read), start);
            }
            crate::jvm::select::compile(&body, &types, &mut Default::default()).unwrap();
        }
    }
}
