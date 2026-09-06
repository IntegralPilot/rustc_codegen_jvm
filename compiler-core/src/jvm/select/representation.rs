use super::*;

pub use super::super::abi::{POINTER_CLASS, SLICE_VIEW_CLASS, UTF8_VIEW_CLASS};

pub(super) fn value_kind(types: &Types, ty: TypeId) -> jvm::Result<Kind> {
    match types.get(ty) {
        Some(Type::Scalar(ty)) => kind(ty),
        Some(
            Type::Pointer(_)
            | Type::Class(_)
            | Type::Interface(_)
            | Type::Array(_)
            | Type::Slice(_)
            | Type::Str,
        ) => Ok(Kind::Reference),
        _ => Err(error("unsupported JVM value representation")),
    }
}

pub(super) fn descriptor(types: &Types, ty: TypeId, output: &mut String) -> jvm::Result<()> {
    use ScalarType::*;
    match types.get(ty) {
        Some(t @ (Type::Pointer(_) | Type::Slice(_) | Type::Str)) => {
            output.push('L');
            output.push_str(match t {
                Type::Slice(_) => SLICE_VIEW_CLASS,
                Type::Str => UTF8_VIEW_CLASS,
                _ => POINTER_CLASS,
            });
            output.push(';');
        }
        Some(Type::Class(symbol) | Type::Interface(symbol)) => {
            output.push('L');
            output.push_str(
                types
                    .symbol_name(symbol)
                    .ok_or_else(|| error("unknown JVM class"))?,
            );
            output.push(';');
        }
        Some(Type::Array(element)) => {
            output.push('[');
            descriptor(types, element, output)?;
        }
        other => output.push(match other {
            Some(Type::Unit) => 'V',
            Some(Type::Scalar(Bool)) => 'Z',
            Some(Type::Scalar(I8 | U8)) => 'B',
            Some(Type::Scalar(I16 | F16)) => 'S',
            Some(Type::Scalar(U16)) => 'C',
            Some(Type::Scalar(I32 | U32)) => 'I',
            Some(Type::Scalar(I64 | U64)) => 'J',
            Some(Type::Scalar(F32)) => 'F',
            Some(Type::Scalar(F64)) => 'D',
            _ => return Err(error("unsupported JVM ABI type")),
        }),
    }
    Ok(())
}

impl Selector<'_> {
    pub(super) fn argument(&mut self, value: ValueId) -> jvm::Result<()> {
        self.load(value)?;
        // JVM byte parameters are int carriers with a signed-byte contract.
        // SSA keeps u8 zero-extended; Java boxing/indexing relies on receiving
        // -128..127 at calls, even though the Rust value is unsigned.
        if self.types.get(self.body.value_type(value)) == Some(Type::Scalar(ScalarType::U8)) {
            self.assembly.code.push(Instruction::I2b);
        }
        Ok(())
    }
    pub(super) fn value_kind(&self, value: ValueId) -> jvm::Result<Kind> {
        value_kind(self.types, self.body.value_type(value))
    }
    pub(super) fn initial_value(&mut self, value: ValueId) -> jvm::Result<frames::FrameValue> {
        Ok(match self.value_kind(value)? {
            Kind::Int => frames::FrameValue::Integer,
            Kind::Long => frames::FrameValue::Long,
            Kind::Float => frames::FrameValue::Float,
            Kind::Double => frames::FrameValue::Double,
            Kind::Reference => {
                let mut name = String::new();
                descriptor(self.types, self.body.value_type(value), &mut name)?;
                let name = name
                    .strip_prefix('L')
                    .and_then(|s| s.strip_suffix(';'))
                    .unwrap_or(&name);
                frames::FrameValue::Object(name.into())
            }
        })
    }
    pub(super) fn normalize_value(&mut self, value: ValueId) {
        if let Some(Type::Scalar(ty)) = self.types.get(self.body.value_type(value)) {
            self.normalize(ty);
        }
    }
}
