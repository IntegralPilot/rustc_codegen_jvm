//! Method dispatch.
use super::*;

pub(super) fn interface<'tcx>(
    tcx: TyCtxt<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    func_instance: Instance<'tcx>,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    method_signature: oomir::Signature,
    receiver_operand: oomir::Operand,
    method_args: Vec<oomir::Operand>,
    declared_method_name: String,
    dispatch_receiver_ty: oomir::Type,
    receiver_self_requires_static_dispatch: bool,
    uses_concrete_trait_default: bool,
    interface_name: String,
) {
    let concrete_provided_trait_method =
        uses_concrete_trait_default && !matches!(dispatch_receiver_ty, oomir::Type::Interface(_));
    if concrete_provided_trait_method
        || receiver_self_requires_static_dispatch
        || requires_compiled_static_dispatch(&dispatch_receiver_ty)
    {
        let target = crate::lower1::naming::mono_fn_name_from_instance(tcx, func_instance);
        let mut static_signature = method_signature;
        static_signature.is_static = true;
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: target
                .class_to_call_on
                .expect("monomorphized functions have JVM owners"),
            method_name: target.method_name,
            method_ty: static_signature,
            args: oomir_operands.clone(),
            dest: effective_dest,
        });
    } else {
        // The method is from an interface - use InvokeInterface
        instructions.push(oomir::Instruction::InvokeInterface {
            class_name: interface_name,
            method_name: declared_method_name,
            method_ty: method_signature,
            args: method_args,
            dest: effective_dest,
            operand: receiver_operand,
        });
    }
}
