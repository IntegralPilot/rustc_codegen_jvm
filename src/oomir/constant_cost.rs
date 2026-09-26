//! Estimated constant emission work, shared by preparation and outlining.
// A byte becomes one Latin-1 character, at most two modified UTF-8 bytes.
pub(crate) const PACKED_BYTE_CHUNK: usize = 32767;

pub(crate) fn constant_byte(constant: &super::Constant) -> Option<u8> {
    match constant {
        super::Constant::U8(value) => Some(*value),
        super::Constant::I8(value) => Some(*value as u8),
        _ => None,
    }
}

pub(crate) fn is_packed_byte_array(ty: &super::Type, values: &[super::Constant]) -> bool {
    values.len() >= 32
        && matches!(ty, super::Type::U8 | super::Type::I8)
        && values.iter().all(|value| constant_byte(value).is_some())
}

fn byte_array_cost(length: usize) -> usize {
    2usize.saturating_add(if length < 32 {
        length.saturating_mul(4)
    } else {
        length.div_ceil(PACKED_BYTE_CHUNK).saturating_mul(4)
    })
}

pub(crate) fn constant_instruction_cost(constant: &super::Constant) -> usize {
    use super::Constant as C;
    match constant {
        C::Unit => 0,
        C::StaticRef { .. } | C::FactoryCall { .. } => 1,
        C::StaticCall { args, .. } => args.iter().fold(1usize, |cost, arg| {
            cost.saturating_add(constant_instruction_cost(arg))
        }),
        C::FunctionPointer { .. } => 3,
        C::PointerAddress { .. } => 3,
        C::RepeatedBytePointer { .. } => 8,
        C::ByteArrayPointer { bytes, .. } => byte_array_cost(bytes.len()).saturating_add(8),
        C::InternedPointer { value, .. } => 7usize.saturating_add(constant_instruction_cost(value)),
        C::I64(_) | C::U64(_) | C::F64(_) => 1,
        C::I8(_)
        | C::U8(_)
        | C::I16(_)
        | C::U16(_)
        | C::I32(_)
        | C::U32(_)
        | C::F16(_)
        | C::F32(_)
        | C::Boolean(_)
        | C::Char(_)
        | C::String(_)
        | C::Null(_) => 1,
        C::Str(_) => 2,
        C::Array(element_type, elements) | C::Slice(element_type, elements)
            if is_packed_byte_array(element_type, elements) =>
        {
            byte_array_cost(elements.len())
                + if matches!(constant, C::Slice(..)) {
                    5
                } else {
                    0
                }
        }
        C::Array(element_type, elements) => {
            let element_cost = if element_type.has_jvm_value() {
                elements.iter().fold(0usize, |cost, element| {
                    cost.saturating_add(3 + constant_instruction_cost(element))
                })
            } else {
                0
            };
            2usize.saturating_add(element_cost)
        }
        C::Slice(_, elements) => elements.iter().fold(7usize, |cost, element| {
            cost.saturating_add(3 + constant_instruction_cost(element))
        }),
        C::SliceRef { backing, .. } => 5usize.saturating_add(constant_instruction_cost(backing)),
        C::Instance { params, .. } => params.iter().fold(3usize, |cost, param| {
            cost.saturating_add(constant_instruction_cost(param))
        }),
    }
}
