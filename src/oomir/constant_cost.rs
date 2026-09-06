//! Estimated constant emission work, shared by preparation and outlining.
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
        C::ByteArrayPointer { bytes, .. } => bytes.len().saturating_mul(4).saturating_add(8),
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
