//! Typed operation and ABI contracts.
use super::*;

pub(super) fn verify_types(inst: &Inst, body: &Body, types: &Types) -> Result<(), VerifyError> {
    let result = inst.result.map(|v| body.value_type(v));
    let ty = |v| body.value_type(v);
    match inst.op {
        Op::ArrayLength(value) => check!(
            result.and_then(|t| types.get(t)) == Some(Type::Scalar(ScalarType::I32))
                && matches!(
                    types.get(ty(value)),
                    Some(Type::Array(_) | Type::Slice(_) | Type::Str)
                ),
            "array length requires an array or view and int result"
        ),
        Op::Reinterpret(value) => {
            check!(
                result.and_then(|r| types.get(r)).map(Type::carrier)
                    == types.get(ty(value)).map(Type::carrier),
                "reinterpretation changes JVM carrier"
            );
        }
        Op::Exception => check!(
            result.is_some_and(|ty| types.get(ty).is_some_and(|ty| ty.carrier() == 5)),
            "exception requires reference result"
        ),
        Op::Adapt(value) => {
            check!(
                types.get(ty(value)).is_some() && result.and_then(|r| types.get(r)).is_some(),
                "invalid ABI adaptation"
            );
        }
        Op::NewArray(size) => {
            check!(
                types.get(ty(size)) == Some(Type::Scalar(ScalarType::I32)),
                "array size requires JVM int"
            );
            check!(
                matches!(result.and_then(|r| types.get(r)), Some(Type::Array(_))),
                "array allocation requires array result"
            );
        }
        Op::FunctionPointer { signature, target } => {
            let signature = body
                .methods
                .get(signature.index())
                .ok_or_else(|| VerifyError("invalid callable signature".into()))?;
            let target = body
                .methods
                .get(target.index())
                .ok_or_else(|| VerifyError("invalid callable target".into()))?;
            check!(
                signature.params == target.params && signature.returns == target.returns,
                "callable descriptors differ"
            );
            check!(
                matches!(result.and_then(|r| types.get(r)), Some(Type::Interface(id) | Type::Class(id)) if types.symbol_name(id) == Some(signature.owner.as_str())),
                "callable result type mismatch"
            );
        }
        Op::Call { method, kind, args } => {
            let method = &body.methods[method.index()];
            check!(
                types.get(method.returns).is_some(),
                "invalid call return type"
            );
            let args = &body.args[args.range()];
            let receiver = usize::from(matches!(kind, CallKind::Virtual | CallKind::Interface));
            check!(
                kind != CallKind::Interface || method.interface,
                "interface call requires interface method reference"
            );
            check!(
                kind != CallKind::Virtual || !method.interface,
                "virtual call requires class method reference"
            );
            check!(
                kind != CallKind::Indirect,
                "indirect calls need an explicit callable signature"
            );
            check!(
                args.len() == method.params.len() + receiver,
                "call argument count mismatch"
            );
            if receiver != 0 {
                check!(
                    matches!(
                        types.get(ty(args[0])),
                        Some(
                            Type::Class(_)
                                | Type::Interface(_)
                                | Type::Pointer(_)
                                | Type::Array(_)
                                | Type::Slice(_)
                                | Type::Str
                        )
                    ),
                    "call receiver is not an object"
                );
            }
            for (&arg, &param) in args[receiver..].iter().zip(&method.params) {
                check!(
                    types.get(param).is_some_and(|t| t != Type::Unit) && ty(arg) == param,
                    "call argument type mismatch"
                );
            }
            if kind == CallKind::Constructor {
                check!(
                    !method.interface
                        && method.name == "<init>"
                        && types.get(method.returns) == Some(Type::Unit),
                    "invalid constructor signature"
                );
                check!(
                    matches!(result.and_then(|id| types.get(id)), Some(Type::Class(symbol)) if types.symbol_name(symbol) == Some(method.owner.as_str())),
                    "constructor result owner mismatch"
                );
            } else {
                check!(
                    method.name != "<init>" && method.name != "<clinit>",
                    "initializer requires construction semantics"
                );
                check!(
                    result
                        == (types.get(method.returns) != Some(Type::Unit))
                            .then_some(method.returns),
                    "call return type mismatch"
                );
            }
        }
        Op::Constant(id) => {
            let actual = match body.constants[id.index()] {
                Constant::Scalar(value) => Type::Scalar(value.ty()),
                Constant::Unit => Type::Unit,
                Constant::External { ty, .. } | Constant::Uninit(ty) => types
                    .get(ty)
                    .ok_or_else(|| VerifyError("invalid external constant type".into()))?,
                Constant::Null(id) => {
                    let ty = types
                        .get(id)
                        .ok_or_else(|| VerifyError("invalid null type".into()))?;
                    check!(
                        !matches!(ty, Type::Scalar(_) | Type::Unit),
                        "null needs reference type"
                    );
                    ty
                }
            };
            check!(
                result.and_then(|id| types.get(id)) == Some(actual),
                "constant type mismatch"
            );
        }
        Op::Binary { op, left, right } => {
            if types.get(ty(left)).is_some_and(|t| t.carrier() == 5) {
                check!(
                    matches!(op, BinaryOp::Eq | BinaryOp::Ne)
                        && types.get(ty(right)).is_some_and(|t| t.carrier() == 5),
                    "invalid reference comparison"
                );
                check!(
                    result.and_then(|t| types.get(t)) == Some(Type::Scalar(ScalarType::Bool)),
                    "reference comparison requires boolean"
                );
                return Ok(());
            }
            let Some(Type::Scalar(left_ty)) = types.get(ty(left)) else {
                return Err(VerifyError("binary operation needs scalar operands".into()));
            };
            let integer = left_ty.integer().is_some();
            let float = matches!(left_ty, ScalarType::F16 | ScalarType::F32 | ScalarType::F64);
            check!(
                match op {
                    BinaryOp::Eq | BinaryOp::Ne => true,
                    BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge =>
                        integer || float || left_ty == ScalarType::Char,
                    BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor =>
                        integer || left_ty == ScalarType::Bool,
                    BinaryOp::Shl | BinaryOp::Shr => integer,
                    _ => integer || float,
                },
                "invalid binary operand category"
            );
            if matches!(op, BinaryOp::Shl | BinaryOp::Shr) {
                check!(
                    matches!(types.get(ty(right)), Some(Type::Scalar(t)) if t.integer().is_some()),
                    "shift count needs integer"
                );
            } else {
                check!(ty(left) == ty(right), "binary operand type mismatch");
            }
            if op.is_comparison() {
                check!(
                    result.and_then(|id| types.get(id)) == Some(Type::Scalar(ScalarType::Bool)),
                    "comparison result needs boolean"
                );
            } else {
                check!(result == Some(ty(left)), "binary result type mismatch");
            }
        }
        Op::Overflow { op, args } => {
            check!(args.len == 3, "overflow needs operands and wrapped result");
            let args = &body.args[args.range()];
            check!(
                matches!(op, BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul),
                "unsupported checked operation"
            );
            check!(
                matches!(types.get(ty(args[0])), Some(Type::Scalar(t)) if t.integer().is_some()),
                "overflow requires integer"
            );
            check!(
                args.iter().all(|&arg| ty(arg) == ty(args[0])),
                "overflow operand type mismatch"
            );
            check!(
                result.and_then(|t| types.get(t)) == Some(Type::Scalar(ScalarType::Bool)),
                "overflow result must be boolean"
            );
        }
        Op::Not(value) | Op::Neg(value) => {
            check!(result == Some(ty(value)), "unary type mismatch");
            let Some(Type::Scalar(t)) = types.get(ty(value)) else {
                return Err(VerifyError("unary operation needs scalar".into()));
            };
            check!(
                t.integer().is_some()
                    || match inst.op {
                        Op::Not(_) => t == ScalarType::Bool,
                        _ => matches!(t, ScalarType::F16 | ScalarType::F32 | ScalarType::F64),
                    },
                "invalid unary operand category"
            );
        }
        Op::Bit { op, value } => {
            check!(
                matches!(types.get(ty(value)), Some(Type::Scalar(t)) if t.integer().is_some()),
                "bit operation requires integer"
            );
            if op.is_count() {
                check!(
                    result.and_then(|t| types.get(t)) == Some(Type::Scalar(ScalarType::U32)),
                    "bit count must return u32"
                );
            } else {
                check!(
                    result == Some(ty(value)),
                    "bit operation changes operand type"
                );
            }
        }
        Op::Opaque(value) => check!(result == Some(ty(value)), "opaque value type mismatch"),
        Op::Project { base, projection } => {
            let projection = body
                .projections
                .get(projection.index())
                .ok_or_else(|| VerifyError("invalid pointer projection".into()))?;
            let field = body
                .fields
                .get(projection.field.index())
                .ok_or_else(|| VerifyError("invalid projection field".into()))?;
            check!(
                !field.is_static && types.get(ty(base)) == Some(Type::Pointer(field.owner)),
                "projection owner mismatch"
            );
            check!(
                result.and_then(|id| types.get(id)) == Some(Type::Pointer(field.ty)),
                "projection result mismatch"
            );
            check!(
                projection.offset <= i64::MAX as u64 && projection.size <= i64::MAX as u64,
                "projection exceeds runtime address space"
            );
        }
        Op::Offset {
            pointer, offset, ..
        } => {
            check!(
                matches!(types.get(ty(pointer)), Some(Type::Pointer(_)))
                    && result == Some(ty(pointer)),
                "pointer offset type mismatch"
            );
            check!(
                matches!(types.get(ty(offset)), Some(Type::Scalar(t)) if t.integer().is_some()),
                "pointer offset requires integer displacement"
            );
        }
        Op::Length(view) => {
            check!(
                matches!(types.get(ty(view)), Some(Type::Slice(_) | Type::Str)),
                "length needs a slice or string"
            );
            check!(
                result.and_then(|id| types.get(id)) == Some(Type::Scalar(ScalarType::U64)),
                "length needs usize result"
            );
        }
        Op::View { data, length } => {
            let element = view_element(result.and_then(|id| types.get(id)), types);
            check!(
                element.is_some() && types.get(ty(data)) == element.map(Type::Pointer),
                "view data type mismatch"
            );
            check!(
                types.get(ty(length)) == Some(Type::Scalar(ScalarType::U64)),
                "view length needs usize"
            );
        }
        Op::ViewData { view, size, codec } => {
            let element = view_element(types.get(ty(view)), types);
            check!(
                element.is_some()
                    && result.and_then(|id| types.get(id)) == element.map(Type::Pointer),
                "view extraction type mismatch"
            );
            check!(
                size <= i32::MAX as u32 && codec.is_none_or(|id| types.symbol_name(id).is_some()),
                "invalid view element layout"
            );
        }
        Op::Cast(value) => {
            check!(result.is_some(), "cast has no result");
            if matches!(types.get(ty(value)), Some(Type::Scalar(_))) {
                check!(
                    matches!(result.and_then(|t| types.get(t)), Some(Type::Scalar(_))),
                    "scalar cast needs scalar result"
                );
            }
        }
        Op::LoadSlot(slot) => check!(
            result == Some(body.slots[slot.index()].ty),
            "slot load type mismatch"
        ),
        Op::AddressOfSlot(slot) => check!(
            result.and_then(|ty| types.get(ty)) == Some(Type::Pointer(body.slots[slot.index()].ty)),
            "slot address type mismatch"
        ),
        Op::Load(pointer) => check!(
            result.is_some() && types.get(ty(pointer)) == result.map(Type::Pointer),
            "pointer load type mismatch"
        ),
        Op::Store { pointer, value } => check!(
            result.is_none() && types.get(ty(pointer)) == Some(Type::Pointer(ty(value))),
            "pointer store type mismatch"
        ),
        Op::StoreSlot { slot, value } => {
            check!(
                result.is_none() && ty(value) == body.slots[slot.index()].ty,
                "slot store type mismatch"
            );
        }
        Op::GetField { field, .. }
        | Op::SetField { field, .. }
        | Op::GetStatic(field)
        | Op::SetStatic { field, .. } => {
            let member = body
                .fields
                .get(field.index())
                .ok_or_else(|| VerifyError("invalid field reference".into()))?;
            let static_access = matches!(inst.op, Op::GetStatic(_) | Op::SetStatic { .. });
            check!(
                member.is_static == static_access,
                "field access kind mismatch"
            );
            if let Op::GetField { object, .. } | Op::SetField { object, .. } = inst.op {
                check!(ty(object) == member.owner, "field receiver type mismatch");
            }
            if let Op::SetField { value, .. } | Op::SetStatic { value, .. } = inst.op {
                check!(
                    result.is_none() && ty(value) == member.ty,
                    "field store type mismatch"
                );
            } else {
                check!(result == Some(member.ty), "field load type mismatch");
            }
        }
        Op::ArraySet { .. } => check!(result.is_none(), "store produces value"),
        _ => {}
    }
    Ok(())
}

fn view_element(ty: Option<Type>, types: &Types) -> Option<TypeId> {
    match ty {
        Some(Type::Slice(element)) => Some(element),
        Some(Type::Str) => types.find(Type::Scalar(ScalarType::U8)),
        _ => None,
    }
}
