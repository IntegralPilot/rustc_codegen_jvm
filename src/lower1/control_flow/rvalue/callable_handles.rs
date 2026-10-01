//! Constant callable identities remain symbolic until the JVM links their site.
use super::*;

pub(crate) fn callable_handle(
    signature: &oomir::Signature,
    interface: &str,
    owner: String,
    name: String,
    target_is_interface: bool,
) -> oomir::Constant {
    oomir::Constant::FunctionHandle {
        interface_name: interface.into(),
        owner,
        name,
        descriptor: signature
            .component_signature()
            .to_jvm_descriptor_with_explicit_params(),
        interface: target_is_interface,
    }
}
