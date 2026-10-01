//! Adapt borrowed return components at generated Java/Rust forwarding boundaries.
use super::*;
use jvm_compiler_core::jvm::locals::LocalKind as Kind;

pub(super) fn prepare(
    cp: &mut InternedConstantPool,
    code: &mut Vec<Instruction>,
    source: bool,
    target: bool,
    parameter_slots: u16,
    locals: &mut u16,
) -> jvm::Result<Option<u16>> {
    if source {
        let slot = parameter_slots - 1;
        if target {
            code.push(Kind::Reference.load(slot));
        }
        Ok(Some(slot))
    } else if target {
        let slot = *locals;
        *locals += 1;
        code.extend([
            get_int_const_instr(cp, 2),
            Instruction::Newarray(jvm::attributes::ArrayType::Long),
            Instruction::Dup,
            Kind::Reference.store(slot),
        ]);
        Ok(Some(slot))
    } else {
        Ok(None)
    }
}

pub(super) fn finish(
    cp: &mut InternedConstantPool,
    code: &mut Vec<Instruction>,
    source: &Type,
    target: &Type,
    source_parts: bool,
    target_parts: bool,
    metadata: Option<u16>,
    locals: &mut u16,
) -> jvm::Result<()> {
    if source_parts && target_parts && source.component_shape() == target.component_shape() {
        code.push(if matches!(source, Type::TaggedI64) {
            Instruction::Lreturn
        } else {
            Instruction::Areturn
        });
        return Ok(());
    }
    let temporary = *locals;
    *locals += if source_parts || target_parts { 2 } else { 0 };
    if target_parts {
        materialize(cp, code, target, metadata.unwrap(), temporary)?;
    }
    if !target.same_jvm_type(source) {
        code.extend(get_cast_instructions(
            "return forwarder",
            target,
            source,
            cp,
        )?);
    }
    if source_parts {
        code.push(Kind::Reference.store(temporary));
        let metadata = metadata.unwrap();
        match source {
            Type::TaggedI64 => {
                let owner = cp.add_class(oomir::TAGGED_LONG_CLASS)?;
                code.extend([
                    Kind::Reference.load(metadata),
                    Instruction::Iconst_0,
                    Kind::Reference.load(temporary),
                    Instruction::Invokestatic(cp.add_method_ref(
                        owner,
                        "tag",
                        "(Lorg/rustlang/runtime/TaggedLong;)J",
                    )?),
                    Instruction::Lastore,
                    Kind::Reference.load(temporary),
                    Instruction::Invokestatic(cp.add_method_ref(
                        owner,
                        "value",
                        "(Lorg/rustlang/runtime/TaggedLong;)J",
                    )?),
                ]);
            }
            Type::Pointer(_) => {
                code.extend([
                    Kind::Reference.load(metadata),
                    Instruction::Iconst_0,
                    Instruction::Lconst_0,
                    Instruction::Lastore,
                    Kind::Reference.load(temporary),
                ]);
            }
            Type::Slice(_) | Type::Str => {
                for index in 0..2 {
                    code.extend([
                        Kind::Reference.load(metadata),
                        get_int_const_instr(cp, index),
                        Kind::Reference.load(temporary),
                        jvm_compiler_core::jvm::abi::view_part_access(cp, index as usize + 1)?,
                    ]);
                    if index == 0 {
                        code.push(Instruction::I2l);
                    }
                    code.push(Instruction::Lastore);
                }
                code.extend([
                    Kind::Reference.load(temporary),
                    jvm_compiler_core::jvm::abi::view_part_access(cp, 0)?,
                ]);
            }
            _ => unreachable!(),
        }
        code.push(if matches!(source, Type::TaggedI64) {
            Instruction::Lreturn
        } else {
            Instruction::Areturn
        });
    } else {
        code.push(return_instruction_for_type(source));
    }
    Ok(())
}

fn materialize(
    cp: &mut InternedConstantPool,
    code: &mut Vec<Instruction>,
    ty: &Type,
    metadata: u16,
    temporary: u16,
) -> jvm::Result<()> {
    if matches!(ty, Type::TaggedI64) {
        let owner = cp.add_class(oomir::TAGGED_LONG_CLASS)?;
        code.extend([
            Kind::Long.store(temporary),
            Instruction::New(owner),
            Instruction::Dup,
            Kind::Long.load(temporary),
            Kind::Reference.load(metadata),
            Instruction::Iconst_0,
            Instruction::Laload,
            Instruction::Invokespecial(cp.add_method_ref(owner, "<init>", "(JJ)V")?),
        ]);
    } else if matches!(ty, Type::Pointer(_)) {
        code.extend([
            Kind::Reference.load(metadata),
            Instruction::Iconst_0,
            Instruction::Laload,
        ]);
        let size = ty.address_plan();
        ty.materialize_address(cp, code)?;
    } else {
        let owner = cp.add_class(if matches!(ty, Type::Str) {
            oomir::UTF8_VIEW_CLASS
        } else {
            oomir::SLICE_VIEW_CLASS
        })?;
        code.extend([
            Kind::Reference.store(temporary),
            Instruction::New(owner),
            Instruction::Dup,
            Kind::Reference.load(temporary),
            Kind::Reference.load(metadata),
            Instruction::Iconst_0,
            Instruction::Laload,
            Instruction::L2i,
            Kind::Reference.load(metadata),
            Instruction::Iconst_1,
            Instruction::Laload,
            Instruction::Invokespecial(cp.add_method_ref(
                owner,
                "<init>",
                "(Ljava/lang/Object;IJ)V",
            )?),
        ]);
    }
    Ok(())
}
