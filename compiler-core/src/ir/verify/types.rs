//! Typed operation and ABI contracts.
use super::*;

pub(super) fn verify_types(inst: &Inst, body: &Body, types: &Types) -> Result<(), VerifyError> {
    let result = inst.result.map(|v| body.value_type(v));
    let ty = |v| body.value_type(v);
    match inst.op {
        Op::CopyStorage { parts, layouts, .. } => {
            let parts = body
                .args
                .get(parts.range())
                .ok_or_else(|| VerifyError("invalid copy components".into()))?;
            check!(
                parts.len() == 5 && result.is_none(),
                "invalid memory copy operands"
            );
            for (i, layout) in layouts.into_iter().enumerate() {
                let Some(Type::Layout(layout)) = types.get(layout) else {
                    return Err(VerifyError("copy needs an exact storage layout".into()));
                };
                let AddressLayout { size, codec, .. } = types.get_layout(layout);
                check!(
                    types
                        .get(ty(parts[i * 2]))
                        .is_some_and(|t| t.carrier() == 5)
                        && types.get(ty(parts[i * 2 + 1])) == Some(Type::Scalar(ScalarType::I64)),
                    "invalid copy location"
                );
                check!(
                    size <= i32::MAX as u32 && codec.is_none_or(|s| types.symbol_name(s).is_some()),
                    "invalid copy layout"
                );
            }
            check!(
                matches!(
                    types.get(ty(parts[4])),
                    Some(Type::Scalar(
                        ScalarType::I32 | ScalarType::U32 | ScalarType::I64 | ScalarType::U64
                    ))
                ),
                "copy length needs an integer"
            );
        }
        Op::ViewRoot {
            backing,
            size,
            codec,
        } => check!(
            types.get(ty(backing)).is_some_and(|t| t.carrier() == 5)
                && result.is_some_and(|t| matches!(types.get(t), Some(Type::Class(s))
                    if types.symbol_name(s) == Some("java/lang/Object")))
                && (1..=i32::MAX as u32).contains(&size)
                && codec.is_none_or(|s| types.symbol_name(s).is_some()),
            "invalid slice storage root"
        ),
        Op::Heap { operation, args } => {
            let args = &body.args[args.range()];
            let (count, root, returns) = match operation {
                HeapOp::Allocate => (2, false, true),
                HeapOp::Reallocate => (5, true, true),
                HeapOp::Deallocate => (2, true, false),
            };
            check!(args.len() == count, "invalid heap operation arity");
            check!(
                !root || types.get(ty(args[0])).is_some_and(|t| t.carrier() == 5),
                "heap operation requires a storage root"
            );
            check!(
                args[usize::from(root)..]
                    .iter()
                    .all(|&arg| types.get(ty(arg)) == Some(Type::Scalar(ScalarType::I64))),
                "heap sizes, alignments and offsets must be i64"
            );
            check!(
                if returns {
                    result.is_some_and(|ty| {
                        matches!(types.get(ty), Some(Type::Class(name))
                    if types.symbol_name(name) == Some("java/lang/Object"))
                    })
                } else {
                    result.is_none()
                },
                "invalid heap operation result"
            );
        }
        Op::CopyValue(value) => check!(
            result == Some(ty(value)) && types.get(ty(value)).is_some_and(|t| t.carrier() == 5),
            "copy requires the same reference value type"
        ),
        Op::TaggedPack(parts) => {
            let parts = &body.args[parts.range()];
            check!(
                parts.len() == 2
                    && parts
                        .iter()
                        .all(|&v| types.get(ty(v)) == Some(Type::Scalar(ScalarType::I64)))
                    && result.is_some_and(|t| types.get(t) == Some(Type::TaggedI64)),
                "invalid tagged scalar components"
            );
        }
        Op::TaggedPart { value, index } => {
            check!(
                index < 2
                    && types.get(ty(value)) == Some(Type::TaggedI64)
                    && result.is_some_and(|t| types.get(t) == Some(Type::Scalar(ScalarType::I64))),
                "invalid tagged scalar projection"
            );
        }
        Op::LoadStorageField {
            address,
            projection,
            ..
        }
        | Op::LoadStorageFieldCopy {
            address,
            projection,
        }
        | Op::StoreStorageField {
            args: address,
            projection,
            ..
        } => {
            let parts = body
                .args
                .get(address.range())
                .ok_or_else(|| VerifyError("invalid storage address".into()))?;
            check!(
                parts.len() >= 2
                    && types.get(ty(parts[0])).is_some_and(|t| t.carrier() == 5)
                    && types.get(ty(parts[1])) == Some(Type::Scalar(ScalarType::I64)),
                "invalid storage address components"
            );
            let projection = body
                .projections
                .get(projection.index())
                .ok_or_else(|| VerifyError("invalid storage projection".into()))?;
            let field = body
                .fields
                .get(projection.field.index())
                .ok_or_else(|| VerifyError("invalid storage field".into()))?;
            check!(
                !field.is_static && matches!(types.get(field.owner), Some(Type::Class(_))),
                "storage projection needs a concrete owner"
            );
            match inst.op {
                Op::LoadStorageFieldCopy { .. } => check!(
                    parts.len() == 2
                        && result == Some(field.ty)
                        && types.get(field.ty).is_some_and(|t| t.carrier() == 5),
                    "invalid owned storage field result"
                ),
                Op::LoadStorageField { index, .. } => check!(
                    parts.len() == 2
                        && result.is_some_and(|result| match index {
                            Some(index) =>
                                borrowed_part_matches(types, field.ty, index as usize, result),
                            None => result == field.ty,
                        }),
                    "invalid storage field result"
                ),
                Op::StoreStorageField { split, .. } => check!(
                    result.is_none()
                        && if split {
                            ComponentShape::of(types, field.ty)
                                .is_some_and(|s| s.len() == parts.len() - 2)
                                && parts[2..]
                                    .iter()
                                    .enumerate()
                                    .all(|(i, &p)| borrowed_part_matches(types, field.ty, i, ty(p)))
                        } else {
                            parts.len() == 3 && ty(parts[2]) == field.ty
                        },
                    "invalid storage field values"
                ),
                _ => unreachable!(),
            }
        }
        Op::Nop => check!(result.is_none(), "nop produces a value"),
        Op::ArrayLength(value) => check!(
            result.and_then(|t| types.get(t)) == Some(Type::Scalar(ScalarType::I32))
                && matches!(
                    types.get(ty(value)),
                    Some(Type::Array(_) | Type::Slice(_) | Type::Str)
                ),
            "array length requires an array or view and int result"
        ),
        Op::Refine(value) => {
            check!(
                result.and_then(|r| types.get(r)).map(Type::carrier) == Some(5)
                    && types.get(ty(value)).map(Type::carrier) == Some(5),
                "reference refinement requires reference operands"
            );
        }
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
        Op::ScalarCell(value) => {
            check!(
                StorageSlot::scalar(ty(value), types).is_some()
                    && result.and_then(|r| types.get(r)) == Some(Type::Pointer(ty(value))),
                "scalar storage requires an exact scalar pointee"
            );
        }
        Op::ProjectRoot {
            address,
            projection,
        } => {
            let values = &body.args[address.range()];
            check!(
                values.len() == 2
                    && types.get(ty(values[0])).is_some_and(|t| t.carrier() == 5)
                    && types.get(ty(values[1])) == Some(Type::Scalar(ScalarType::I64))
                    && projection.index() < body.projections.len()
                    && result
                        .and_then(|r| types.get(r))
                        .is_some_and(|t| t.carrier() == 5),
                "invalid typed field location"
            );
        }
        Op::ProjectOffset { root, base, offset } => {
            check!(
                types.get(ty(root)).is_some_and(|t| t.carrier() == 5)
                    && types.get(ty(base)).is_some_and(|t| t.carrier() == 5)
                    && types.get(ty(offset)) == Some(Type::Scalar(ScalarType::I64))
                    && result == Some(ty(offset)),
                "invalid typed field displacement"
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
        Op::ArrayFill { array, value } => {
            check!(
                result.is_none()
                    && types.get(ty(array)) == Some(Type::Array(ty(value)))
                    && StorageSlot::scalar(ty(value), types).is_some(),
                "primitive array fill type mismatch"
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
        Op::AddressTag(value) => {
            check!(
                matches!(types.get(ty(value)), Some(Type::Pointer(_)))
                    && result.and_then(|ty| types.get(ty)) == Some(Type::Scalar(ScalarType::I64)),
                "nullable tag needs pointer operand and i64 result"
            );
        }
        Op::AddressEqual { left, right } | Op::AddressCompare { left, right } => {
            let output = if matches!(inst.op, Op::AddressCompare { .. }) {
                ScalarType::I32
            } else {
                ScalarType::Bool
            };
            check!(
                matches!(types.get(ty(left)), Some(Type::Pointer(_)))
                    && matches!(types.get(ty(right)), Some(Type::Pointer(_)))
                    && result.and_then(|ty| types.get(ty)) == Some(Type::Scalar(output)),
                "invalid address comparison"
            );
        }
        Op::LocationEqual(parts) | Op::LocationCompare(parts) => {
            let output = if matches!(inst.op, Op::LocationCompare(_)) {
                ScalarType::I32
            } else {
                ScalarType::Bool
            };
            let values = body
                .args
                .get(parts.range())
                .ok_or_else(|| VerifyError("invalid location operands".into()))?;
            check!(
                values.len() == 4
                    && result.and_then(|ty| types.get(ty)) == Some(Type::Scalar(output)),
                "invalid location comparison"
            );
            for (index, &value) in values.iter().enumerate() {
                check!(
                    if index % 2 == 0 {
                        matches!(types.get(ty(value)), Some(Type::Class(s)) if types.symbol_name(s) == Some("java/lang/Object"))
                    } else {
                        types.get(ty(value)) == Some(Type::Scalar(ScalarType::I64))
                    },
                    "location component mismatch"
                );
            }
        }
        Op::Project { base, projection }
        | Op::LoadField { base, projection }
        | Op::LoadFieldCopy { base, projection }
        | Op::LoadFieldPart {
            base, projection, ..
        }
        | Op::StoreFieldParts {
            base, projection, ..
        }
        | Op::StoreField {
            base, projection, ..
        } => {
            let projection = body
                .projections
                .get(projection.index())
                .ok_or_else(|| VerifyError("invalid pointer projection".into()))?;
            let field = body
                .fields
                .get(projection.field.index())
                .ok_or_else(|| VerifyError("invalid projection field".into()))?;
            let mut owner = field.owner;
            let mut parent = projection.parent;
            let mut depth = 0;
            while let Some(id) = parent {
                depth += 1;
                check!(depth <= 32, "projection path cycle");
                let previous = body
                    .projections
                    .get(id.index())
                    .ok_or_else(|| VerifyError("invalid projection parent".into()))?;
                let member = body
                    .fields
                    .get(previous.field.index())
                    .ok_or_else(|| VerifyError("invalid projection parent field".into()))?;
                check!(
                    !member.is_static && member.ty == owner,
                    "projection path type mismatch"
                );
                owner = member.owner;
                parent = previous.parent;
            }
            check!(
                !field.is_static && types.pointee(ty(base)) == Some(owner),
                "projection owner mismatch"
            );
            check!(
                projection.parent.is_none()
                    || matches!(
                        inst.op,
                        Op::LoadField { .. } | Op::LoadFieldCopy { .. } | Op::LoadFieldPart { .. }
                    ),
                "projection paths require field reads"
            );
            if !matches!(inst.op, Op::Project { .. } | Op::LoadFieldCopy { .. }) {
                check!(
                    matches!(
                        types.get(field.ty),
                        Some(Type::Scalar(_) | Type::Pointer(_) | Type::Slice(_) | Type::Str)
                    ),
                    "promoted field access requires a scalar or pointer field"
                );
            }
            match inst.op {
                Op::LoadFieldCopy { .. } => check!(
                    result == Some(field.ty)
                        && types.get(field.ty).is_some_and(|t| t.carrier() == 5),
                    "owned field load type mismatch"
                ),
                Op::Project { .. } => check!(
                    result.and_then(|id| types.pointee(id)) == Some(field.ty),
                    "projection result mismatch"
                ),
                Op::LoadField { .. } => {
                    check!(result == Some(field.ty), "field load type mismatch")
                }
                Op::StoreField { value, .. } => check!(
                    result.is_none() && ty(value) == field.ty,
                    "field store type mismatch"
                ),
                Op::LoadFieldPart { index, .. } => {
                    check!(
                        result.is_some_and(|part| borrowed_part_matches(
                            types,
                            field.ty,
                            index as usize,
                            part
                        )),
                        "invalid split field load"
                    );
                }
                Op::StoreFieldParts { parts, .. } => {
                    let values = body
                        .args
                        .get(parts.range())
                        .ok_or_else(|| VerifyError("invalid split field operands".into()))?;
                    check!(
                        result.is_none()
                            && ComponentShape::of(types, field.ty)
                                .is_some_and(|shape| shape.len() == values.len()),
                        "invalid split field store"
                    );
                    check!(
                        values
                            .iter()
                            .enumerate()
                            .all(|(index, &value)| borrowed_part_matches(
                                types,
                                field.ty,
                                index,
                                ty(value)
                            )),
                        "invalid split field components"
                    );
                }
                _ => unreachable!(),
            }
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
        Op::RetypeAddress {
            pointer,
            size,
            codec,
        } => {
            check!(
                matches!(types.get(ty(pointer)), Some(Type::Pointer(_)))
                    && matches!(result.and_then(|t| types.get(t)), Some(Type::Pointer(_))),
                "retyped address needs pointer operands"
            );
            check!(
                size <= i32::MAX as u32 && codec.is_none_or(|id| types.symbol_name(id).is_some()),
                "invalid address layout"
            );
        }
        Op::LocationTag(parts)
        | Op::AddressPack(parts)
        | Op::LoadAddress(parts)
        | Op::LoadAddressCopy(parts)
        | Op::LoadTypedCopy { parts, .. }
        | Op::LoadTyped { parts, .. }
        | Op::StoreTyped { parts, .. }
        | Op::TypedAddressPack { parts, .. }
        | Op::StoreAddress { parts, .. } => {
            let parts = body
                .args
                .get(parts.range())
                .ok_or_else(|| VerifyError("invalid address components".into()))?;
            check!(
                match inst.op {
                    Op::LoadTyped { .. } => matches!(parts.len(), 2 | 3),
                    Op::StoreTyped { .. } => parts.len() == 3,
                    _ => parts.len() == 2,
                },
                "address component count"
            );
            check!(
                types.get(ty(parts[0])).is_some_and(|t| t.carrier() == 5),
                "address root needs a reference"
            );
            check!(
                types.get(ty(parts[1])) == Some(Type::Scalar(ScalarType::I64)),
                "address offset needs signed long"
            );
            if let Op::LoadTypedCopy { size, codec, .. }
            | Op::LoadTyped { size, codec, .. }
            | Op::TypedAddressPack { size, codec, .. }
            | Op::StoreTyped { size, codec, .. } = inst.op
            {
                check!(
                    size > 0
                        && size <= i32::MAX as u32
                        && codec.is_none_or(|id| types.symbol_name(id).is_some()),
                    "invalid typed address layout"
                );
            }
            match inst.op {
                Op::LocationTag(_) => check!(
                    result.and_then(|ty| types.get(ty)) == Some(Type::Scalar(ScalarType::I64)),
                    "nullable tag needs i64 result"
                ),
                Op::AddressPack(_) | Op::TypedAddressPack { .. } => check!(
                    result
                        .and_then(|t| ComponentShape::of(types, t))
                        .is_some_and(ComponentShape::is_address),
                    "address pack needs pointer type"
                ),
                Op::LoadAddressCopy(_) | Op::LoadTypedCopy { .. } => check!(
                    result.is_some_and(|t| types.get(t).is_some_and(|t| t.carrier() == 5)),
                    "owned address load needs a reference carrier"
                ),
                Op::LoadTyped { .. } => {
                    check!(
                        result.is_some_and(|t| types.get(t).is_some_and(|t| t.carrier() == 5)),
                        "typed borrowed load needs a reference carrier"
                    );
                    if let Some(&target) = parts.get(2) {
                        check!(
                            matches!(types.get(ty(target)), Some(Type::Class(name))
                            if types.symbol_name(name) == Some("java/lang/String")),
                            "typed borrowed load needs a class name"
                        );
                    }
                }
                Op::LoadAddress(_) => check!(
                    result.is_some_and(|t| StorageSlot::scalar(t, types).is_some()
                        || types.get(t).is_some_and(|t| t.carrier() == 5)),
                    "address load needs a stored value"
                ),
                Op::StoreTyped { .. } => check!(
                    result.is_none() && types.get(ty(parts[2])).is_some_and(|t| t.carrier() == 5),
                    "typed store needs an aggregate value"
                ),
                Op::StoreAddress { value, .. } => check!(
                    result.is_none()
                        && (StorageSlot::scalar(ty(value), types).is_some()
                            || types.get(ty(value)).is_some_and(|t| t.carrier() == 5)),
                    "address store needs a stored value"
                ),
                _ => unreachable!(),
            }
        }
        Op::AddressPart { address, index } => {
            check!(
                ComponentShape::of(types, ty(address)).is_some_and(ComponentShape::is_address),
                "address part needs pointer type"
            );
            check!(
                match index {
                    0 => result
                        .and_then(|t| types.get(t))
                        .is_some_and(|t| t.carrier() == 5),
                    1 => result.and_then(|t| types.get(t)) == Some(Type::Scalar(ScalarType::I64)),
                    _ => false,
                },
                "invalid address component"
            );
        }
        Op::SlotRoot(slot) => {
            check!(
                body.slots
                    .get(slot.index())
                    .and_then(|s| StorageSlot::scalar(s.ty, types))
                    .is_some(),
                "slot root needs primitive storage"
            );
            check!(
                result
                    .and_then(|t| types.get(t))
                    .is_some_and(|t| t.carrier() == 5),
                "slot root needs reference result"
            );
        }
        Op::ViewPack(parts) | Op::ViewAddress { parts, .. } => {
            let parts = body
                .args
                .get(parts.range())
                .ok_or_else(|| VerifyError("invalid view components".into()))?;
            check!(parts.len() == 3, "view needs backing, start, and length");
            check!(
                types.get(ty(parts[0])).is_some_and(|ty| ty.carrier() == 5),
                "view backing needs a reference"
            );
            check!(
                types.get(ty(parts[1])) == Some(Type::Scalar(ScalarType::I32)),
                "view start needs an int"
            );
            check!(
                types.get(ty(parts[2])) == Some(Type::Scalar(ScalarType::U64)),
                "view length needs usize"
            );
            if let Op::ViewPack(_) = inst.op {
                check!(
                    result.is_some_and(|t| ComponentShape::view_carrier(types, t)),
                    "view pack result"
                );
                return Ok(());
            }
            let Op::ViewAddress { size, codec, .. } = inst.op else {
                unreachable!()
            };
            check!(
                matches!(result.and_then(|ty| types.get(ty)), Some(Type::Pointer(_))),
                "view address needs a pointer"
            );
            check!(
                size <= i32::MAX as u32 && codec.is_none_or(|id| types.symbol_name(id).is_some()),
                "invalid view address layout"
            );
        }
        Op::ViewPart { view, index } => {
            check!(
                ComponentShape::view_carrier(types, ty(view)),
                "view part source"
            );
            check!(
                match index {
                    0 => result
                        .and_then(|t| types.get(t))
                        .is_some_and(|t| t.carrier() == 5),
                    1 => result.and_then(|t| types.get(t)) == Some(Type::Scalar(ScalarType::I32)),
                    2 => result.and_then(|t| types.get(t)) == Some(Type::Scalar(ScalarType::U64)),
                    _ => false,
                },
                "view part result"
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
        Op::AddressViewPart { address, index } => {
            check!(
                ComponentShape::of(types, ty(address)).is_some_and(ComponentShape::is_address),
                "view part needs an address"
            );
            check!(
                match (index, result.and_then(|t| types.get(t))) {
                    (0, Some(Type::Class(s))) => types.symbol_name(s) == Some("java/lang/Object"),
                    (1, Some(Type::Scalar(ScalarType::I32))) => true,
                    _ => false,
                },
                "invalid address view component"
            );
        }
        Op::TypedAddressViewPart {
            parts,
            size,
            codec,
            index,
        } => {
            let parts = &body.args[parts.range()];
            check!(parts.len() == 2, "invalid typed slice location arity");
            check!(
                types.get(ty(parts[0])).is_some_and(|t| t.carrier() == 5)
                    && types.get(ty(parts[1])) == Some(Type::Scalar(ScalarType::I64))
                    && (1..=i32::MAX as u32).contains(&size)
                    && codec.is_none_or(|s| types.symbol_name(s).is_some()),
                "invalid typed slice location"
            );
            check!(
                match (index, result.and_then(|t| types.get(t))) {
                    (0, Some(Type::Class(s))) => types.symbol_name(s) == Some("java/lang/Object"),
                    (1, Some(Type::Scalar(ScalarType::I32))) => true,
                    _ => false,
                },
                "invalid typed slice component"
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
        Op::Commit(pointer) => check!(
            result.is_none() && matches!(types.get(ty(pointer)), Some(Type::Pointer(_))),
            "view commit needs its address owner"
        ),
        Op::LoadCopy(pointer) => check!(
            result.is_some_and(|t| types.get(t).is_some_and(|t| t.carrier() == 5))
                && types.pointee(ty(pointer)) == result,
            "owned pointer load type mismatch"
        ),
        Op::Load(pointer) => check!(
            result.is_some() && types.pointee(ty(pointer)) == result,
            "pointer load type mismatch: address {:?} ({:?}), result {:?} ({:?})",
            pointer,
            types.get(ty(pointer)),
            result,
            result.and_then(|ty| types.get(ty))
        ),
        Op::Store { pointer, value } => check!(
            result.is_none() && types.pointee(ty(pointer)) == Some(ty(value)),
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
        Op::ViewGet(parts) | Op::ViewSet { parts, .. } => {
            let parts = &body.args[parts.range()];
            check!(
                parts.len() == 3
                    && types.get(ty(parts[0])).is_some_and(|t| t.carrier() == 5)
                    && parts[1..]
                        .iter()
                        .all(|&p| types.get(ty(p)) == Some(Type::Scalar(ScalarType::I32))),
                "slice access requires backing, start and index"
            );
            check!(
                matches!(inst.op, Op::ViewGet(_)) == result.is_some(),
                "slice access result mismatch"
            );
        }
        Op::ArrayGet {
            array,
            index,
            native,
        }
        | Op::ArraySet {
            array,
            index,
            native,
            ..
        } => {
            check!(
                !native || matches!(types.get(ty(array)), Some(Type::Array(_))),
                "native scratch access requires a JVM array"
            );
            check!(
                types.get(ty(index)) == Some(Type::Scalar(ScalarType::I32)),
                "array index requires JVM int"
            );
            check!(
                matches!(inst.op, Op::ArrayGet { .. }) == result.is_some(),
                "array access result mismatch"
            );
        }
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

fn borrowed_part_matches(types: &Types, logical: TypeId, index: usize, part: TypeId) -> bool {
    match (ComponentShape::of(types, logical), index, types.get(part)) {
        (Some(ComponentShape::TaggedI64), 0..=1, Some(Type::Scalar(ScalarType::I64))) => true,
        (Some(_), 0, Some(Type::Class(s))) => types.symbol_name(s) == Some("java/lang/Object"),
        (
            Some(ComponentShape::Address | ComponentShape::StorageAddress),
            1,
            Some(Type::Scalar(ScalarType::I64)),
        )
        | (Some(ComponentShape::View), 1, Some(Type::Scalar(ScalarType::I32)))
        | (Some(ComponentShape::View), 2, Some(Type::Scalar(ScalarType::U64))) => true,
        _ => false,
    }
}
