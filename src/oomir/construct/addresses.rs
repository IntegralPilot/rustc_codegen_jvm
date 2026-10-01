//! Rust address operations carry layout and arithmetic independently of Java.
use super::*;

fn static_size(value: &oomir::Operand) -> Option<u32> {
    use oomir::{Constant as C, Operand::Constant};
    let value = match value {
        Constant(C::I32(n)) => u32::try_from(*n).ok()?,
        Constant(C::U32(n)) => *n,
        Constant(C::I64(n)) => u32::try_from(*n).ok()?,
        Constant(C::U64(n)) => u32::try_from(*n).ok()?,
        _ => return None,
    };
    (value <= i32::MAX as u32).then_some(value)
}

impl Emission<'_> {
    pub(super) fn address(&mut self, instruction: oomir::Instruction) -> Result<()> {
        use oomir::Instruction::*;
        let (dest, value) = match instruction {
            AddressOffset {
                dest,
                source,
                count,
                ty,
                bytes,
                wrapping,
                subtract,
            } => {
                let pointer = self.operand(source)?;
                let count = self.operand(count)?;
                let mut offset = self.adapt(count, self.ty(&oomir::Type::I64))?;
                if subtract {
                    offset = self
                        .emit(ir::Op::Neg(offset), Some(self.ty(&oomir::Type::I64)))
                        .unwrap();
                }
                let value = self
                    .emit(
                        ir::Op::Offset {
                            pointer,
                            offset,
                            bytes,
                            wrapping,
                        },
                        Some(self.ty(&ty)),
                    )
                    .unwrap();
                (dest, value)
            }
            AddressRetype {
                dest,
                source,
                layout,
            } => (dest, self.address_layout(source, *layout, false)?),
            ViewAddress {
                dest,
                source,
                layout,
            } => (dest, self.address_layout(source, *layout, true)?),
            _ => unreachable!("non-address instruction"),
        };
        if let Some(dest) = dest {
            self.write(&dest, value)?;
        }
        Ok(())
    }

    pub(super) fn address_layout(
        &mut self,
        source: oomir::Operand,
        layout: oomir::AddressLayout,
        view: bool,
    ) -> Result<ir::ValueId> {
        let oomir::AddressLayout {
            pointer_type,
            size,
            codec,
        } = layout;
        let source_type = source.get_type().expect("typed address source");
        let codec_id = match &codec {
            oomir::Operand::Constant(oomir::Constant::Null(_)) => Some(None),
            oomir::Operand::Constant(oomir::Constant::String(name)) => Some(Some(
                self.vocabulary
                    .types
                    .find_symbol(name)
                    .expect("registered address codec"),
            )),
            _ => None,
        };
        let input = self.operand(source)?;
        let returns = self.ty(&pointer_type);
        // DST casts keep the slice length for later tail construction. Sized casts can discard it.
        let thin_view = match &pointer_type {
            oomir::Type::Pointer(inner) => match inner.as_ref() {
                oomir::Type::Class(name) => self
                    .context
                    .fields
                    .get(name)
                    .is_some_and(|layout| layout.direct),
                oomir::Type::Interface(_) => false,
                _ => true,
            },
            _ => false,
        };
        let typed = matches!(pointer_type, oomir::Type::Pointer(_))
            && if view {
                thin_view && matches!(source_type, oomir::Type::Slice(_) | oomir::Type::Str)
            } else {
                matches!(source_type, oomir::Type::Pointer(_))
            };
        if typed && let (Some(size), Some(codec)) = (static_size(&size), codec_id) {
            let op = if view {
                let mut values = Vec::with_capacity(3);
                for (index, ty) in [types::object(), oomir::Type::I32, oomir::Type::U64]
                    .iter()
                    .enumerate()
                {
                    values.push(
                        self.emit(
                            ir::Op::ViewPart {
                                view: input,
                                index: index as u8,
                            },
                            Some(self.ty(ty)),
                        )
                        .unwrap(),
                    );
                }
                ir::Op::ViewAddress {
                    parts: self.builder.args(values),
                    size,
                    codec,
                }
            } else if pointer_type.scalar_address_size() == Some(size) && codec.is_none() {
                ir::Op::Cast(input)
            } else {
                ir::Op::RetypeAddress {
                    pointer: input,
                    size,
                    codec,
                }
            };
            return Ok(self.emit(op, Some(returns)).unwrap());
        }
        let source_type = if view { types::object() } else { source_type };
        let size = self.operand(size)?;
        let codec = self.operand(codec)?;
        Ok(self
            .call(
                oomir::POINTER_CLASS.into(),
                if view { "fromSlice" } else { "retype" }.into(),
                vec![
                    self.ty(&source_type),
                    self.ty(&oomir::Type::U64),
                    self.ty(&oomir::Type::java_string()),
                ],
                returns,
                ir::CallKind::JvmStatic,
                vec![input, size, codec],
            )?
            .unwrap())
    }
}
