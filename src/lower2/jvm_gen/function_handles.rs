//! One exact invocation body per function ABI, shared by cold code targets.
use super::*;

/// Function interfaces depend on the physical call ABI. Separate descriptor components permit
/// storage aliases.
pub(super) fn carrier_recipe(signature: &Signature) -> Vec<u8> {
    let mut recipe = String::from("carrier-v2;function-v1;");
    for (_, ty) in signature.explicit_jvm_params() {
        if ty.has_jvm_value() {
            recipe.push_str(&format!("param;0:{};", ty.to_jvm_descriptor()));
        }
    }
    recipe.push_str(&format!("return;0:{};", signature.ret.to_jvm_descriptor()));
    recipe.into_bytes()
}

pub(super) fn bridge(
    cp: &mut InternedConstantPool,
    signature: &Signature,
    bootstrap: &mut Vec<BootstrapMethod>,
) -> jvm::Result<jvm::Method> {
    let descriptor = signature.to_jvm_descriptor_with_explicit_params();
    let bridge = format!("(Ljava/lang/invoke/MethodHandle;{}", &descriptor[1..]);
    let mut code = vec![Instruction::Aload_0];
    let mut local = 1;
    for (_, ty) in signature.explicit_jvm_params() {
        if ty.has_jvm_value() {
            code.push(get_load_instruction(ty, local)?);
            local += get_type_size(ty);
        }
    }
    let owner = cp.add_class("org/rustlang/runtime/FunctionCallSite")?;
    let method = cp.add_method_ref(
        owner,
        "bootstrap",
        concat!(
            "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;",
            "Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
        ),
    )?;
    let bootstrap_method_ref = cp.add_method_handle(jvm::ReferenceKind::InvokeStatic, method)?;
    let index = u16::try_from(bootstrap.len())?;
    bootstrap.push(BootstrapMethod {
        bootstrap_method_ref,
        arguments: vec![],
    });
    let invoke = cp.add_invoke_dynamic(index, "call", &bridge)?;
    code.extend([
        Instruction::Invokedynamic(invoke),
        return_instruction_for_type(&signature.ret),
    ]);
    let body = code_attribute_for_descriptor(cp, local, code, &bridge, true, None, "$rust$invoke")?;
    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC
            | MethodAccessFlags::STATIC
            | MethodAccessFlags::SYNTHETIC,
        name_index: cp.add_utf8("$rust$invoke")?,
        descriptor_index: cp.add_utf8(bridge)?,
        attributes: vec![body],
    })
}
