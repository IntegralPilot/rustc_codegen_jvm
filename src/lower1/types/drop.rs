//! Java callbacks use Rust destruction. MIR controls drop order and unwind cleanup.
use super::*;
use crate::lower1::context::Definitions;

pub(super) fn managed_drop_glue_function<'tcx>(
    rust_ty: Ty<'tcx>,
    class_name: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Function {
    let self_ty = oomir::Type::Class(class_name.to_string());
    let mut instructions = Vec::new();
    crate::lower1::control_flow::emit_owned_drop(
        rust_ty,
        operand_var("_1", self_ty.clone()),
        "_managed_drop",
        tcx,
        instance_context,
        data_types,
        &mut instructions,
    );
    instructions.push(oomir::Instruction::Return { operand: None });

    oomir::Function {
        name: MANAGED_DROP_METHOD.to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: vec![("self".to_string(), self_ty)],
            ret: Box::new(oomir::Type::Void),
            is_static: false,
        },
        body: oomir::CodeBlock {
            entry: "entry".to_string(),
            basic_blocks: HashMap::from_iter([(
                "entry".to_string(),
                oomir::BasicBlock {
                    label: "entry".to_string(),
                    instructions,
                },
            )]),
        }
        .into(),
    }
}
