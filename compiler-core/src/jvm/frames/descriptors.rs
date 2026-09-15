use super::*;

#[derive(Default)]
pub(super) struct SignatureCache {
    methods: HashMap<u16, MethodTransfer>,
    fields: HashMap<u16, FrameValue>,
}

pub(super) struct MethodTransfer {
    pub params: usize,
    pub result: Option<FrameValue>,
    pub constructor: Option<Arc<str>>,
}

impl SignatureCache {
    pub fn method(&mut self, cp: &ConstantPool, index: u16) -> jvm::Result<&MethodTransfer> {
        use std::collections::hash_map::Entry;
        Ok(match self.methods.entry(index) {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => {
                let method = if matches!(cp.try_get(index)?, Constant::InvokeDynamic { .. }) {
                    invoke_dynamic_info(cp, index)?
                } else {
                    static_method_ref_info(cp, index)?
                };
                let descriptor = jvm::JavaString::from(method.descriptor.as_str());
                let (params, result) = FieldType::parse_method_descriptor(&descriptor)?;
                entry.insert(MethodTransfer {
                    params: params.len(),
                    result: result.as_ref().map(frame_value_from_field_type),
                    constructor: (method.method_name == "<init>").then(|| method.class_name.into()),
                })
            }
        })
    }

    pub fn field(&mut self, cp: &ConstantPool, index: u16) -> jvm::Result<FrameValue> {
        use std::collections::hash_map::Entry;
        Ok(match self.fields.entry(index) {
            Entry::Occupied(entry) => entry.get().clone(),
            Entry::Vacant(entry) => entry
                .insert(frame_value_from_field_type(&field_type_for_ref(cp, index)?))
                .clone(),
        })
    }
}

pub(super) struct MethodRefInfo {
    pub(super) class_name: String,
    pub(super) method_name: String,
    pub(super) descriptor: String,
}

pub(super) fn frame_value_from_field_type(field_type: &FieldType) -> FrameValue {
    match field_type {
        FieldType::Base(BaseType::Long) => FrameValue::Long,
        FieldType::Base(BaseType::Float) => FrameValue::Float,
        FieldType::Base(BaseType::Double) => FrameValue::Double,
        FieldType::Base(_) => FrameValue::Integer,
        FieldType::Object(class_name) => {
            FrameValue::Object(normalize_class_name(&class_name.to_string()).into())
        }
        FieldType::Array(_) => FrameValue::Object(field_type.class_name().into()),
    }
}

pub(super) fn field_type_for_ref(
    constant_pool: &ConstantPool,
    field_ref: u16,
) -> jvm::Result<FieldType> {
    let (_, name_and_type_index) = constant_pool.try_get_field_ref(field_ref)?;
    let (_, descriptor_index) = constant_pool.try_get_name_and_type(*name_and_type_index)?;
    let descriptor = constant_pool.try_get_utf8(*descriptor_index)?;
    Ok(FieldType::parse(&descriptor.to_string())?)
}

pub(super) fn method_ref_info(
    constant_pool: &ConstantPool,
    method_ref: u16,
    is_interface: bool,
) -> jvm::Result<MethodRefInfo> {
    let (class_index, name_and_type_index) = if is_interface {
        constant_pool.try_get_interface_method_ref(method_ref)?
    } else {
        constant_pool.try_get_method_ref(method_ref)?
    };
    let class_name = constant_pool.try_get_class(*class_index)?.to_string();
    let (name_index, descriptor_index) =
        constant_pool.try_get_name_and_type(*name_and_type_index)?;
    let method_name = constant_pool.try_get_utf8(*name_index)?.to_string();
    let descriptor = constant_pool.try_get_utf8(*descriptor_index)?.to_string();
    Ok(MethodRefInfo {
        class_name,
        method_name,
        descriptor,
    })
}

pub(super) fn static_method_ref_info(
    constant_pool: &ConstantPool,
    method_ref: u16,
) -> jvm::Result<MethodRefInfo> {
    let is_interface = matches!(
        constant_pool.try_get(method_ref)?,
        Constant::InterfaceMethodRef { .. }
    );
    method_ref_info(constant_pool, method_ref, is_interface)
}

pub(super) fn invoke_dynamic_info(
    constant_pool: &ConstantPool,
    invoke_dynamic_ref: u16,
) -> jvm::Result<MethodRefInfo> {
    let Constant::InvokeDynamic {
        name_and_type_index,
        ..
    } = constant_pool.try_get(invoke_dynamic_ref)?
    else {
        return Err(jvm::Error::VerificationError {
            context: "invokedynamic stack-map transfer".to_string(),
            message: format!(
                "constant-pool entry #{invoke_dynamic_ref} is not an InvokeDynamic constant"
            ),
        });
    };
    let (name_index, descriptor_index) =
        constant_pool.try_get_name_and_type(*name_and_type_index)?;
    Ok(MethodRefInfo {
        class_name: "java/lang/Object".to_string(),
        method_name: constant_pool.try_get_utf8(*name_index)?.to_string(),
        descriptor: constant_pool.try_get_utf8(*descriptor_index)?.to_string(),
    })
}

pub(super) fn describe_instruction(
    instruction: &Instruction,
    constant_pool: &ConstantPool,
) -> String {
    let method = match instruction {
        Instruction::Invokevirtual(index) | Instruction::Invokespecial(index) => {
            method_ref_info(constant_pool, *index, false).ok()
        }
        Instruction::Invokestatic(index) => static_method_ref_info(constant_pool, *index).ok(),
        Instruction::Invokeinterface(index, _) => method_ref_info(constant_pool, *index, true).ok(),
        _ => None,
    };
    method.map_or_else(
        || format!("{instruction:?}"),
        |method| {
            format!(
                "{instruction:?} => {}.{}{}",
                method.class_name, method.method_name, method.descriptor
            )
        },
    )
}

pub(super) fn instruction_window(
    instructions: &[Instruction],
    center: usize,
    constant_pool: &ConstantPool,
) -> String {
    let start = center.saturating_sub(8);
    let end = (center + 4).min(instructions.len().saturating_sub(1));
    (start..=end)
        .map(|index| {
            let marker = if index == center { ">" } else { " " };
            format!(
                "{marker} {index}: {}",
                describe_instruction(&instructions[index], constant_pool)
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}
