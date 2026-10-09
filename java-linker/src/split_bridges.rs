//! Preserve the runtime's reflective codec protocol after overflow splitting.
use crate::*;
use ristretto_classfile::{BaseType, FieldType, Method, MethodAccessFlags};

pub(crate) fn retain_codec_surface(
    root: &mut ClassFile<'static>,
    moves: &HashMap<(JavaString, JavaString), String>,
) -> io::Result<()> {
    let error = |e| constant_pool_error("split codec bridge", e);
    let mut methods = moves.iter().collect::<Vec<_>>();
    methods.sort_unstable_by(|a, b| a.0.cmp(b.0));
    for ((name, descriptor), target) in methods {
        let (params, result) = FieldType::parse_method_descriptor(descriptor).map_err(error)?;
        let mut code = Vec::with_capacity(params.len() + 2);
        let mut slots = 0_u16;
        for param in params {
            let (load, width) = match param {
                FieldType::Base(BaseType::Long) => (Instruction::Lload(slots as u8), 2),
                FieldType::Base(BaseType::Double) => (Instruction::Dload(slots as u8), 2),
                FieldType::Base(BaseType::Float) => (Instruction::Fload(slots as u8), 1),
                FieldType::Base(_) => (Instruction::Iload(slots as u8), 1),
                FieldType::Object(_) | FieldType::Array(_) => (Instruction::Aload(slots as u8), 1),
            };
            slots += width;
            if slots > 255 {
                return Err(io::Error::other("codec bridge exceeds JVM argument limit"));
            }
            code.push(load);
        }
        let owner = root.constant_pool.add_class(target).map_err(error)?;
        let name_index = root
            .constant_pool
            .add(Constant::Utf8(name.clone().into()))
            .map_err(error)?;
        let descriptor_index = root
            .constant_pool
            .add(Constant::Utf8(descriptor.clone().into()))
            .map_err(error)?;
        let name_and_type_index = root
            .constant_pool
            .add(Constant::NameAndType {
                name_index,
                descriptor_index,
            })
            .map_err(error)?;
        let call = root
            .constant_pool
            .add(Constant::MethodRef {
                class_index: owner,
                name_and_type_index,
            })
            .map_err(error)?;
        code.push(Instruction::Invokestatic(call));
        let (ret, width) = match result {
            None => (Instruction::Return, 0),
            Some(FieldType::Base(BaseType::Long)) => (Instruction::Lreturn, 2),
            Some(FieldType::Base(BaseType::Double)) => (Instruction::Dreturn, 2),
            Some(FieldType::Base(BaseType::Float)) => (Instruction::Freturn, 1),
            Some(FieldType::Base(_)) => (Instruction::Ireturn, 1),
            Some(FieldType::Object(_) | FieldType::Array(_)) => (Instruction::Areturn, 1),
        };
        code.push(ret);
        root.methods.push(Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index,
            descriptor_index,
            attributes: vec![Attribute::Code {
                name_index: root.constant_pool.add_utf8("Code").map_err(error)?,
                max_stack: slots.max(width),
                max_locals: slots,
                code,
                exception_table: Vec::new(),
                attributes: Vec::new(),
            }],
        });
    }
    Ok(())
}
