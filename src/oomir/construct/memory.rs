//! Semantic places and memory accesses become SSA without helper recognition.
use super::*;
use ir::{CallKind, Op};

impl Emission<'_> {
    pub(super) fn copy_value(&mut self, source: ir::ValueId) -> ir::ValueId {
        let ty = self.builder.body.value_type(source);
        if matches!(
            self.vocabulary.types.get(ty),
            Some(
                ir::Type::TaggedI64
                    | ir::Type::Scalar(_)
                    | ir::Type::Pointer(_)
                    | ir::Type::Slice(_)
                    | ir::Type::Str
            )
        ) {
            source
        } else {
            self.emit(Op::CopyValue(source), Some(ty)).unwrap()
        }
    }

    pub(super) fn memory(&mut self, instruction: oomir::Instruction) -> Result<()> {
        use oomir::Instruction::*;
        match instruction {
            ValueCopy { dest, source } => {
                let source = self.operand(source)?;
                let value = self.copy_value(source);
                self.write(&dest, value)?;
            }
            MemoryLoad {
                dest,
                pointer,
                pointee,
                owned,
            } => {
                let pointer = self.operand(pointer)?;
                let pointer =
                    self.adapt(pointer, self.ty(&oomir::Type::pointer(pointee.clone())))?;
                let value = self
                    .emit(
                        if owned && pointee.is_jvm_reference_type() {
                            Op::LoadCopy(pointer)
                        } else {
                            Op::Load(pointer)
                        },
                        Some(self.ty(&pointee)),
                    )
                    .unwrap();
                self.write(&dest, value)?;
            }
            MemoryStore {
                pointer,
                pointee,
                value,
            } => {
                let pointer = self.operand(pointer)?;
                let pointer =
                    self.adapt(pointer, self.ty(&oomir::Type::pointer(pointee.clone())))?;
                let value = self.operand(value)?;
                let value = self.adapt(value, self.ty(&pointee))?;
                self.emit(Op::Store { pointer, value }, None);
            }
            MemoryCommit { pointer } => {
                let pointer = self.operand(pointer)?;
                self.emit(Op::Commit(pointer), None);
            }
            MemoryProject {
                dest,
                base,
                projection,
            } => {
                let oomir::MemoryProjection {
                    owner,
                    field,
                    pointee,
                    offset,
                    size,
                    codec,
                } = *projection;
                let base_type = oomir::Type::Class(owner.clone());
                let pointer_type = oomir::Type::pointer(base_type.clone());
                let result_type = oomir::Type::pointer(pointee.clone());
                let direct = self.context.fields.get(&owner).is_some_and(|layout| {
                    layout.direct
                        && layout
                            .members
                            .iter()
                            .any(|(name, ty)| name == &field && ty == &pointee)
                }) && base.get_type().as_ref() == Some(&pointer_type);
                let base = self.operand(base)?;
                let value = if direct {
                    let base = self.adapt(base, self.ty(&pointer_type))?;
                    let field = self.builder.field(ir::FieldRef {
                        owner: self.ty(&base_type),
                        name: field,
                        ty: self.ty(&pointee),
                        is_static: false,
                    });
                    let projection = self.builder.projection(ir::PointerProjection {
                        parent: None,
                        field,
                        offset,
                        size,
                        codec,
                    });
                    self.emit(
                        Op::Project { base, projection },
                        Some(self.ty(&result_type)),
                    )
                    .unwrap()
                } else {
                    let mut args = vec![base];
                    for value in [
                        oomir::Constant::String(owner),
                        oomir::Constant::LiteralString(field),
                        oomir::Constant::U64(offset),
                        oomir::Constant::U64(size),
                        codec.map_or_else(
                            || oomir::Constant::Null(oomir::Type::java_string()),
                            oomir::Constant::String,
                        ),
                    ] {
                        args.push(self.constant(value)?);
                    }
                    let string = self.ty(&oomir::Type::java_string());
                    let long = self.ty(&oomir::Type::U64);
                    self.call(
                        oomir::POINTER_CLASS.into(),
                        "projectStructField".into(),
                        vec![string, string, long, long, string],
                        self.ty(&result_type),
                        CallKind::Virtual,
                        args,
                    )?
                    .unwrap()
                };
                self.write(&dest, value)?;
            }
            _ => unreachable!("non-memory instruction"),
        }
        Ok(())
    }
}
