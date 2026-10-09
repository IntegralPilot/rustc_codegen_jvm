//! Semantic emission operations enter the common SSA builder.
use super::*;
use ir::{CallKind, MethodRef, Op, Terminator, TypeId, ValueId};

impl Emission<'_> {
    pub(super) fn call(
        &mut self,
        owner: String,
        mut name: String,
        mut params: Vec<TypeId>,
        returns: TypeId,
        kind: CallKind,
        values: Vec<ValueId>,
    ) -> Result<Option<ValueId>> {
        if kind == CallKind::JvmStatic
            && owner == "org/rustlang/runtime/Intrinsics"
            && name == "selectUnpredictable"
            && values.len() == 3
        {
            let yes = self.adapt(values[1], returns)?;
            let no = self.adapt(values[2], returns)?;
            let left = self.builder.create_block();
            let right = self.builder.create_block();
            let done = self.builder.create_block();
            let result = self.builder.parameter(done, returns);
            self.builder.branch(values[0], left, right);
            self.builder.switch_to(left);
            self.builder.jump(done, vec![yes]);
            self.builder.switch_to(right);
            self.builder.jump(done, vec![no]);
            self.builder.switch_to(done);
            return Ok(Some(result));
        }
        let initial_cell = self.initial_cell(&owner, &name, kind, returns, &values);
        if let Some(initial) = initial_cell
            && ir::StorageSlot::scalar(
                self.builder.body.value_type(initial),
                &self.vocabulary.types,
            )
            .is_some()
        {
            // Scalar storage must reach component analysis before Object ABI conversion boxes its
            // contents.
            let cell = self.emit(Op::ScalarCell(initial), Some(returns)).unwrap();
            self.cells.push((cell, initial));
            return Ok(Some(cell));
        }
        let declared_result = returns;
        let returns = if owner == oomir::POINTER_CLASS
            && matches!(name.as_str(), "addr" | "expose_provenance" | "metadata")
        {
            self.ty(&oomir::Type::U64)
        } else {
            returns
        };
        let kind = if kind == CallKind::Virtual && self.context.interfaces.contains(&owner) {
            CallKind::Interface
        } else {
            kind
        };
        let receiver = usize::from(matches!(kind, CallKind::Virtual | CallKind::Interface));
        if values.len() != params.len() + receiver {
            return Err(format!(
                "call {owner}.{name}: {} arguments, {} parameters",
                values.len(),
                params.len()
            ));
        }
        let mut args = Vec::with_capacity(values.len());
        if receiver != 0 {
            args.push(values[0]);
        }
        for (&value, &ty) in values[receiver..].iter().zip(&params) {
            args.push(self.adapt(value, ty)?);
        }
        if matches!(kind, CallKind::JvmStatic | CallKind::RustStatic)
            && owner.contains("/mono/Mono_")
            && params
                .iter()
                .any(|&ty| matches!(self.vocabulary.types.get(ty), Some(ir::Type::Class(_))))
        {
            let signature = oomir::Signature {
                params: params
                    .iter()
                    .map(|&ty| (String::new(), source_type(&self.vocabulary.types, ty)))
                    .collect(),
                ret: Box::new(source_type(&self.vocabulary.types, returns)),
                is_static: true,
            };
            if let Some(signature) = self.context.record_signature(&owner, &name, &signature) {
                let mut expanded = Vec::new();
                for (&value, &ty) in args.iter().zip(&params) {
                    let source = source_type(&self.vocabulary.types, ty);
                    if let Some(fields) = self.context.scalar_fields(&source) {
                        for (name, member_ty) in fields {
                            let member_ty = self.ty(member_ty);
                            let field = self.builder.field(ir::FieldRef {
                                owner: ty,
                                name: name.clone(),
                                ty: member_ty,
                                is_static: false,
                            });
                            expanded.push(
                                self.emit(
                                    Op::GetField {
                                        object: value,
                                        field,
                                    },
                                    Some(member_ty),
                                )
                                .unwrap(),
                            );
                        }
                    } else {
                        expanded.push(value);
                    }
                }
                args = expanded;
                params = signature.params.iter().map(|(_, ty)| self.ty(ty)).collect();
                name.push_str(RECORD_ENTRY);
            }
        }
        let interface = kind == CallKind::Interface || self.context.interfaces.contains(&owner);
        let method = self.builder.method(MethodRef {
            owner,
            name,
            params,
            returns,
            interface,
        });
        let args = self.builder.args(args);
        let result =
            (self.vocabulary.types.get(returns) != Some(ir::Type::Unit)).then_some(returns);
        let value = self.emit(Op::Call { method, kind, args }, result);
        if let (Some(cell), Some(initial)) = (value, initial_cell) {
            self.cells.push((cell, initial));
        }
        value
            .map(|value| self.adapt(value, declared_result))
            .transpose()
    }
    fn invoke(
        &mut self,
        owner: String,
        name: String,
        signature: oomir::Signature,
        kind: CallKind,
        receiver: Option<oomir::Operand>,
        operands: Vec<oomir::Operand>,
        dest: Option<String>,
    ) -> Result<()> {
        let explicit = signature.explicit_jvm_params().iter().collect::<Vec<_>>();
        if explicit.len() != operands.len() {
            return Err(format!(
                "{owner}.{name}: descriptor/source argument count differs ({} / {})",
                explicit.len(),
                operands.len()
            ));
        }
        let mut args = Vec::new();
        if let Some(receiver) = receiver {
            let value = self.operand(receiver)?;
            args.push(self.adapt(value, self.ty(&oomir::Type::Class(owner.clone())))?);
        }
        let mut params = Vec::new();
        for ((_, ty), operand) in explicit.into_iter().zip(operands) {
            if ty.has_jvm_value() {
                params.push(self.ty(ty));
                args.push(self.operand(operand)?);
            }
        }
        if let Some(value) = self.call(owner, name, params, self.ty(&signature.ret), kind, args)? {
            if let Some(dest) = dest {
                self.write(&dest, value)?;
            }
        }
        Ok(())
    }
    pub(super) fn instruction(&mut self, instruction: oomir::Instruction) -> Result<()> {
        use oomir::Instruction::*;
        match instruction {
            TaggedPack { dest, value, tag } => {
                let long = self.ty(&oomir::Type::I64);
                let value = self.operand(value)?;
                let value = self.adapt(value, long)?;
                let tag = self.operand(tag)?;
                let tag = self.adapt(tag, long)?;
                let parts = ir::List::append(&mut self.builder.body.args, [value, tag]);
                let result = self
                    .emit(
                        Op::TaggedPack(parts),
                        Some(self.ty(&oomir::Type::TaggedI64)),
                    )
                    .unwrap();
                self.write(&dest, result)?;
            }
            TaggedPart { dest, value, index } => {
                let value = self.operand(value)?;
                let result = self
                    .emit(
                        Op::TaggedPart { value, index },
                        Some(self.ty(&oomir::Type::I64)),
                    )
                    .unwrap();
                self.write(&dest, result)?;
            }
            operation @ (AddressRetype { .. } | ViewAddress { .. } | AddressOffset { .. }) => {
                self.address(operation)?;
            }
            Heap {
                operation,
                args,
                dest,
            } => self.heap(operation, args, dest)?,
            operation @ (ValueCopy { .. }
            | MemoryLoad { .. }
            | MemoryStore { .. }
            | MemoryCommit { .. }
            | MemoryProject { .. }) => {
                self.memory(operation)?;
            }
            SourceLocation(location) => {
                self.lines
                    .get_or_insert_with(|| jvm_compiler_core::jvm::select::SourceLines {
                        instructions: vec![None; self.builder.body.instructions.len()],
                        terminators: vec![None; self.builder.body.blocks.len()],
                    });
                self.line = Some(u16::try_from(location.line).unwrap_or(0));
                self.source_file.get_or_insert(location.file_name);
            }
            LocalVariableScope(scope) => {
                let scope = scope
                    .into_iter()
                    .filter(|&i| i < self.debug.variables.len())
                    .map(|i| i as u32)
                    .collect::<Vec<_>>();
                let id = self
                    .debug
                    .scopes
                    .iter()
                    .position(|s| *s == scope)
                    .unwrap_or_else(|| {
                        let id = self.debug.scopes.len();
                        self.debug.scopes.push(scope);
                        id
                    });
                self.debug
                    .push(&self.builder, ir::DebugChange::Scope(id as u32))
                    .line = self.line;
            }
            UnwindStart { target } => self.unwind = Some(self.handlers[&target]),
            UnwindEnd => self.unwind = None,
            Label { name } => {
                let target = self.blocks[&name];
                if self.builder.body.blocks[self.builder.current().index()]
                    .terminator
                    .is_none()
                {
                    self.builder.jump(target, vec![]);
                }
                self.builder.switch_to(target);
            }
            Jump { target } => self.builder.jump(self.blocks[&target], vec![]),
            Branch {
                condition,
                true_block,
                false_block,
            } => {
                let condition = self.operand(condition)?;
                let condition = self.adapt(condition, self.ty(&oomir::Type::Boolean))?;
                self.builder.branch(
                    condition,
                    self.blocks[&true_block],
                    self.blocks[&false_block],
                );
            }
            Switch {
                discr,
                targets,
                otherwise,
            } => {
                let value = self.operand(discr)?;
                let value_type = self.builder.body.value_type(value);
                let Some(ir::Type::Scalar(ty)) = self
                    .vocabulary
                    .types
                    .get(self.builder.body.value_type(value))
                else {
                    let Some(ir::Type::Class(owner)) = self.vocabulary.types.get(value_type) else {
                        return Err("non-integer switch".into());
                    };
                    let owner = self.vocabulary.types.symbol_name(owner).unwrap().to_owned();
                    if owner != "org/rustlang/runtime/I128" && owner != "org/rustlang/runtime/U128"
                    {
                        return Err(format!("non-integer switch {owner}"));
                    }
                    for (key, target) in targets {
                        let key = self.constant(key)?;
                        let order = self
                            .call(
                                owner.clone(),
                                "compareTo".into(),
                                vec![value_type],
                                self.ty(&oomir::Type::I32),
                                CallKind::Virtual,
                                vec![value, key],
                            )?
                            .unwrap();
                        let zero = self.constant(oomir::Constant::I32(0))?;
                        let equal = self
                            .emit(
                                Op::Binary {
                                    op: oomir::BinaryOp::Eq,
                                    left: order,
                                    right: zero,
                                },
                                Some(self.ty(&oomir::Type::Boolean)),
                            )
                            .unwrap();
                        let next = self.builder.create_block();
                        self.builder.branch(equal, self.blocks[&target], next);
                        self.builder.switch_to(next);
                    }
                    self.builder.jump(self.blocks[&otherwise], Vec::new());
                    self.sync_lines();
                    return Ok(());
                };
                let targets = targets
                    .into_iter()
                    .map(|(constant, name)| {
                        let scalar = oomir::scalar::from_constant(&constant)
                            .ok_or_else(|| "non-scalar switch key".to_owned())?;
                        Ok((
                            Scalar::from_bits(ty, scalar.bits())
                                .ok_or_else(|| "invalid switch key".to_owned())?,
                            self.blocks[&name],
                        ))
                    })
                    .collect::<Result<Vec<_>>>()?;
                self.builder.switch(value, targets, self.blocks[&otherwise]);
            }
            Return { operand } => {
                let value = if self.vocabulary.types.get(self.builder.body.return_type)
                    == Some(ir::Type::Unit)
                {
                    None
                } else if let Some(operand) = operand {
                    let value = self.operand(operand)?;
                    Some(self.adapt(value, self.builder.body.return_type)?)
                } else {
                    return Err("missing return value".into());
                };
                self.builder.terminate(Terminator::Return(value));
            }
            Rethrow => {
                let value = self.operand(oomir::Operand::Variable {
                    name: "__rust_unwind_exception".into(),
                    ty: oomir::Type::Class("java/lang/Throwable".into()),
                })?;
                self.builder.terminate(Terminator::Throw {
                    value,
                    unwind: None,
                });
            }
            Move { dest, src } => {
                let value = self.operand(src)?;
                self.write(&dest, value)?;
            }
            Cast { dest, op, ty } => {
                let value = self.operand(op)?;
                let result = self.ty(&ty);
                let value = if matches!(
                    self.vocabulary
                        .types
                        .get(self.builder.body.value_type(value)),
                    Some(ir::Type::Pointer(_))
                ) && matches!(self.vocabulary.types.get(result), Some(ir::Type::Pointer(inner)) if matches!(self.vocabulary.types.get(inner), Some(ir::Type::Scalar(_))))
                {
                    self.emit(Op::Cast(value), Some(result)).unwrap()
                } else {
                    self.adapt(value, result)?
                };
                self.write(&dest, value)?;
            }
            Binary { dest, op, op1, op2 } => self.binary(&dest, op, op1, op2)?,
            Not { dest, src } => self.unary(&dest, src, true)?,
            Neg { dest, src } => self.unary(&dest, src, false)?,
            InvokeStatic {
                class_name,
                method_name,
                method_ty,
                args,
                dest,
            }
            | InvokeRustStatic {
                class_name,
                method_name,
                method_ty,
                args,
                dest,
            } => {
                if class_name == oomir::POINTER_CLASS
                    && method_name == "nullableTag"
                    && args.len() == 1
                {
                    let value = self.operand(args[0].clone())?;
                    let tag = self
                        .emit(Op::AddressTag(value), Some(self.ty(&oomir::Type::I64)))
                        .unwrap();
                    if let Some(dest) = dest {
                        self.write(&dest, tag)?;
                    }
                    return Ok(());
                }
                if class_name == oomir::POINTER_CLASS
                    && self.view_data(&method_name, &args, &dest)?
                {
                    return Ok(());
                }
                self.invoke(
                    class_name,
                    method_name,
                    method_ty,
                    CallKind::JvmStatic,
                    None,
                    args,
                    dest,
                )?;
            }
            InvokeVirtual {
                class_name,
                method_name,
                method_ty,
                args,
                operand,
                dest,
            } => {
                if class_name == oomir::POINTER_CLASS
                    && self.pointer_operation(&method_name, &method_ty, &operand, &args, &dest)?
                {
                    return Ok(());
                }
                self.invoke(
                    class_name,
                    method_name,
                    method_ty,
                    CallKind::Virtual,
                    Some(operand),
                    args,
                    dest,
                )?;
            }
            InvokeInterface {
                class_name,
                method_name,
                method_ty,
                args,
                operand,
                dest,
            } => {
                self.invoke(
                    class_name,
                    method_name,
                    method_ty,
                    CallKind::Interface,
                    Some(operand),
                    args,
                    dest,
                )?;
            }
            CallIndirect {
                function_ptr,
                signature,
                args,
                dest,
            } => {
                let ty = function_ptr.get_type().unwrap();
                let owner = match ty {
                    oomir::Type::Class(name) | oomir::Type::Interface(name) => name,
                    _ => return Err(format!("invalid callable type {ty:?}")),
                };
                self.invoke(
                    owner,
                    "call".into(),
                    signature,
                    CallKind::Interface,
                    Some(*function_ptr),
                    args,
                    dest,
                )?;
            }
            CreateFunctionPointer {
                dest,
                interface_name,
                signature,
                target_class_name,
                target_method_name,
            } => {
                let params = signature
                    .explicit_jvm_params()
                    .iter()
                    .filter(|(_, t)| t.has_jvm_value())
                    .map(|(_, t)| self.ty(t))
                    .collect::<Vec<_>>();
                let returns = self.ty(&signature.ret);
                let sig = self.builder.method(MethodRef {
                    owner: interface_name.clone(),
                    name: "call".into(),
                    params: params.clone(),
                    returns,
                    interface: true,
                });
                let target = self.builder.method(MethodRef {
                    owner: target_class_name,
                    name: target_method_name,
                    params,
                    returns,
                    interface: false,
                });
                let value = self
                    .emit(
                        Op::FunctionPointer {
                            signature: sig,
                            target,
                        },
                        Some(self.ty(&oomir::Type::Interface(interface_name))),
                    )
                    .unwrap();
                self.write(&dest, value)?;
            }
            ConstructObject {
                dest,
                class_name,
                args,
            } => {
                if let Some(value) = self.construct_view(&class_name, &args)? {
                    self.write(&dest, value)?;
                    return Ok(());
                }
                let mut params = Vec::new();
                let mut values = Vec::new();
                let args = args
                    .into_iter()
                    .filter(|(_, ty)| ty.has_jvm_value())
                    .collect::<Vec<_>>();
                let fields = self
                    .context
                    .fields
                    .get(&class_name)
                    .map(|layout| &layout.members)
                    .filter(|fields| fields.len() == args.len());
                for (index, (operand, ty)) in args.into_iter().enumerate() {
                    let ty = fields.map_or(ty, |fields| fields[index].1.clone());
                    {
                        params.push(self.ty(&ty));
                        let value = self.operand(operand)?;
                        values.push(self.adapt(value, self.ty(&ty))?);
                    }
                }
                let method = self.builder.method(MethodRef {
                    owner: class_name.clone(),
                    name: "<init>".into(),
                    params,
                    returns: self.ty(&oomir::Type::Unit),
                    interface: false,
                });
                let args = self.builder.args(values);
                let value = self
                    .emit(
                        Op::Call {
                            method,
                            kind: CallKind::Constructor,
                            args,
                        },
                        Some(self.ty(&oomir::Type::Class(class_name))),
                    )
                    .unwrap();
                self.write(&dest, value)?;
            }
            Unreachable => self.builder.terminate(Terminator::Unreachable),
            ThrowNewWithMessage {
                exception_class,
                message,
            } => {
                let message = self.constant(oomir::Constant::String(message))?;
                let method = self.builder.method(MethodRef {
                    owner: exception_class.clone(),
                    name: "<init>".into(),
                    params: vec![self.ty(&oomir::Type::Class("java/lang/String".into()))],
                    returns: self.ty(&oomir::Type::Unit),
                    interface: false,
                });
                let args = self.builder.args([message]);
                let value = self
                    .emit(
                        Op::Call {
                            method,
                            kind: CallKind::Constructor,
                            args,
                        },
                        Some(self.ty(&oomir::Type::Class(exception_class))),
                    )
                    .unwrap();
                let unwind = self
                    .unwind
                    .map(|target| self.builder.edge(target, Vec::new()));
                self.builder.terminate(Terminator::Throw { value, unwind });
            }
            GetField {
                dest,
                object,
                field_name,
                field_ty,
                owner_class,
            } => self.get_field(dest, object, owner_class, field_name, field_ty)?,
            GetJvmField {
                dest,
                object,
                class_name,
                field_name,
                field_ty,
            } => self.get_field(dest, object, class_name, field_name, field_ty)?,
            SetField {
                object,
                field_name,
                value,
                field_ty,
                owner_class,
            } => {
                let object = self.operand(oomir::Operand::Variable {
                    name: object,
                    ty: oomir::Type::Class(owner_class.clone()),
                })?;
                self.set_field(object, owner_class, field_name, value, field_ty)?;
            }
            SetJvmField {
                object,
                class_name,
                field_name,
                value,
                field_ty,
            } => {
                let object = self.operand(object)?;
                self.set_field(object, class_name, field_name, value, field_ty)?;
            }
            GetStaticField {
                dest,
                class_name,
                field_name,
                field_ty,
            } => {
                if field_ty.has_jvm_value() {
                    let field = self.field(class_name, field_name, &field_ty, true);
                    let value = self
                        .emit(Op::GetStatic(field), Some(self.ty(&field_ty)))
                        .unwrap();
                    self.write(&dest, value)?;
                }
            }
            SetStaticField {
                class_name,
                field_name,
                value,
                field_ty,
            } => {
                if field_ty.has_jvm_value() {
                    let field = self.field(class_name, field_name, &field_ty, true);
                    let value = self.operand(value)?;
                    let value = self.adapt(value, self.ty(&field_ty))?;
                    self.emit(Op::SetStatic { field, value }, None);
                }
            }
            NewArray {
                dest,
                element_type,
                size,
            } => {
                let size = self.operand(size)?;
                let size = self.adapt(size, self.ty(&oomir::Type::I32))?;
                let value = self
                    .emit(
                        Op::NewArray(size),
                        Some(self.ty(&oomir::Type::Array(Box::new(element_type)))),
                    )
                    .unwrap();
                self.write(&dest, value)?;
            }
            ArrayGet { dest, array, index } => self.array_get(dest, array, index)?,
            ArrayStore {
                array,
                index,
                value,
                copy_value,
            } => self.array_store(array, index, value, copy_value)?,
            ArrayFill {
                array,
                value,
                copy_value,
            } => self.array_fill(array, value, copy_value)?,
            Length { dest, array } => self.length(dest, array)?,
        }
        self.sync_lines();
        Ok(())
    }
    fn field(
        &mut self,
        owner: String,
        name: String,
        ty: &oomir::Type,
        is_static: bool,
    ) -> ir::MemberId {
        self.builder.field(ir::FieldRef {
            owner: self.ty(&oomir::Type::Class(owner)),
            name,
            ty: self.ty(ty),
            is_static,
        })
    }
    fn get_field(
        &mut self,
        dest: String,
        object: oomir::Operand,
        owner: String,
        name: String,
        ty: oomir::Type,
    ) -> Result<()> {
        if !ty.has_jvm_value() {
            return Ok(());
        }
        let object = self.operand(object)?;
        if matches!(
            owner.as_str(),
            oomir::SLICE_VIEW_CLASS | oomir::UTF8_VIEW_CLASS
        ) && ir::ComponentShape::view_carrier(
            &self.vocabulary.types,
            self.builder.body.value_type(object),
        ) {
            let part = match name.as_str() {
                "array" => Some((0, types::object())),
                "offset" => Some((1, oomir::Type::I32)),
                "length" | "rustLength" => Some((2, oomir::Type::U64)),
                _ => None,
            };
            if let Some((index, part_ty)) = part {
                let value = self
                    .emit(
                        Op::ViewPart {
                            view: object,
                            index,
                        },
                        Some(self.ty(&part_ty)),
                    )
                    .unwrap();
                let value = self.adapt(value, self.ty(&ty))?;
                return self.write(&dest, value);
            }
        }
        if owner == oomir::SLICE_VIEW_CLASS
            && matches!(
                self.vocabulary
                    .types
                    .get(self.builder.body.value_type(object)),
                Some(ir::Type::Slice(_) | ir::Type::Str)
            )
            && ((name == "rustLength" && ty == oomir::Type::U64)
                || (name == "length" && ty == oomir::Type::I32))
        {
            let op = if name == "rustLength" {
                Op::Length(object)
            } else {
                Op::ArrayLength(object)
            };
            let value = self.emit(op, Some(self.ty(&ty))).unwrap();
            return self.write(&dest, value);
        }
        let object = self.adapt(object, self.ty(&oomir::Type::Class(owner.clone())))?;
        let field = self.field(owner, name, &ty, false);
        let value = self
            .emit(Op::GetField { object, field }, Some(self.ty(&ty)))
            .unwrap();
        self.write(&dest, value)
    }
    fn set_field(
        &mut self,
        object: ValueId,
        owner: String,
        name: String,
        value: oomir::Operand,
        ty: oomir::Type,
    ) -> Result<()> {
        if !ty.has_jvm_value() {
            return Ok(());
        }
        let object = self.adapt(object, self.ty(&oomir::Type::Class(owner.clone())))?;
        let value = self.operand(value)?;
        let value = self.adapt(value, self.ty(&ty))?;
        let field = self.field(owner, name, &ty, false);
        self.emit(
            Op::SetField {
                object,
                field,
                value,
            },
            None,
        );
        Ok(())
    }
}
