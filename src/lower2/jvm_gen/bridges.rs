//! Native JVM bridges emission.
use super::*;

pub(super) fn create_static_instance_bridge(
    cp: &mut InternedConstantPool,
    class_name_jvm: &str,
    method_name: &str,
    signature: &oomir::Signature,
    owner_is_interface: bool,
) -> jvm::Result<jvm::Method> {
    debug_assert!(
        !signature.is_static && !signature.params.is_empty(),
        "static receiver bridges require the first signature parameter to be self"
    );
    let name_index = cp.add_utf8(method_name)?;

    // OOMIR instance method signatures retain self as params[0], while the real
    // JVM instance method descriptor omits it. The static bridge makes that
    // receiver explicit again and delegates to the instance method.
    let mut bridge_params = signature.params.clone();
    if let Some((_, receiver_ty)) = bridge_params.first_mut() {
        *receiver_ty = Type::Class(class_name_jvm.to_string());
    }

    let bridge_signature = Signature {
        params: bridge_params,
        ret: signature.ret.clone(),
        is_static: true,
    };
    let bridge_descriptor = bridge_signature.to_string();
    let descriptor_index = cp.add_utf8(&bridge_descriptor)?;

    let class_index = cp.add_class(class_name_jvm)?;
    let instance_method_ref = if owner_is_interface {
        cp.add_interface_method_ref(class_index, method_name, &signature.to_string())?
    } else {
        cp.add_method_ref(class_index, method_name, &signature.to_string())?
    };

    let mut instructions = Vec::new();
    let mut next_local = 0;

    for (_, param_ty) in &bridge_signature.params {
        if !param_ty.has_jvm_value() {
            continue;
        }
        instructions.push(get_load_instruction(param_ty, next_local)?);
        let size = get_type_size(param_ty);

        next_local += size;
    }
    if owner_is_interface {
        instructions.push(Instruction::Invokeinterface(
            instance_method_ref,
            next_local
                .try_into()
                .map_err(|_| jvm::Error::VerificationError {
                    context: format!("Static receiver bridge {class_name_jvm}::{method_name}"),
                    message: "interface call exceeds 255 JVM parameter slots".to_string(),
                })?,
        ));
    } else {
        instructions.push(Instruction::Invokevirtual(instance_method_ref));
    }
    instructions.push(return_instruction_for_type(bridge_signature.ret.as_ref()));

    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index,
        descriptor_index,
        attributes: vec![code_attribute_for_descriptor(
            cp,
            next_local,
            instructions,
            &bridge_descriptor,
            true,
            None,
            method_name,
        )?],
    })
}
