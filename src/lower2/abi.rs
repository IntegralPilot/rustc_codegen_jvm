//! Representation conversions shared by every SSA body producer.
use super::*;
use jvm::attributes::Instruction;
use jvm_compiler_core::ir::{TypeId, Types};

pub(super) fn adapt(
    from: TypeId,
    to: TypeId,
    types: &Types,
    code: &mut Vec<Instruction>,
    cp: &mut InternedConstantPool,
) -> jvm::Result<()> {
    let from = oomir::construct::source_type(types, from);
    let to = oomir::construct::source_type(types, to);
    if from == to
        || (from.is_jvm_reference_type()
            && to.is_jvm_reference_type()
            && from.to_jvm_descriptor() == to.to_jvm_descriptor())
    {
        return Ok(());
    }
    if to.to_jvm_descriptor().starts_with('[')
        && from.is_jvm_reference_type()
        && !from.to_jvm_descriptor().starts_with('[')
    {
        let descriptor = to.to_jvm_descriptor();
        code.push(Instruction::Ldc_w(cp.add_string(&descriptor)?));
        let class = cp.add_class(oomir::POINTER_CLASS)?;
        let method = cp.add_method_ref(
            class,
            "arrayCarrier",
            "(Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/Object;",
        )?;
        code.push(Instruction::Invokestatic(method));
        code.push(Instruction::Checkcast(cp.add_class(descriptor)?));
        return Ok(());
    }
    let view_conversion = match (&from, &to) {
        (oomir::Type::Slice(_), oomir::Type::Str) => Some((
            "fromSlice",
            format!(
                "(L{};)L{};",
                oomir::SLICE_VIEW_CLASS,
                oomir::UTF8_VIEW_CLASS
            ),
        )),
        (oomir::Type::Str, oomir::Type::Slice(_)) => Some((
            "asSlice",
            format!(
                "(L{};)L{};",
                oomir::UTF8_VIEW_CLASS,
                oomir::SLICE_VIEW_CLASS
            ),
        )),
        _ => None,
    };
    if let Some((name, descriptor)) = view_conversion {
        let class = cp.add_class(oomir::UTF8_VIEW_CLASS)?;
        code.push(Instruction::Invokestatic(
            cp.add_method_ref(class, name, descriptor)?,
        ));
        return Ok(());
    }
    if from == oomir::Type::U8 && to.is_jvm_reference_type() {
        code.push(Instruction::I2b);
    }
    code.extend(helpers::get_cast_instructions("SSA ABI", &from, &to, cp)?);
    Ok(())
}
