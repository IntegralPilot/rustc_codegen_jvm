//! Returned Java closures require a callable interface even without a Rust trait coercion.
use super::*;

pub(crate) fn ensure_exported_closure_calls<'tcx>(
    result: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) {
    let result = normalize_unsize_ty(result, tcx, instance);
    for argument in result.walk() {
        let Some(ty) = argument.as_type() else {
            continue;
        };
        if matches!(ty.kind(), TyKind::Closure(..)) {
            if let Some(abi) = closure_callable_abi(ty, tcx, data_types, instance) {
                ensure_closure_callable_bridge(ty, &abi, data_types, tcx, instance);
            }
        }
    }
}

fn closure_callable_abi<'tcx>(
    closure_ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
) -> Option<crate::lower1::types::CallableTraitObjectAbi<'tcx>> {
    let closure_ty = normalize_unsize_ty(closure_ty, tcx, instance);
    let TyKind::Closure(_, closure_args) = closure_ty.kind() else {
        return None;
    };
    let closure_signature =
        tcx.instantiate_bound_regions_with_erased(closure_args.as_closure().sig());
    let tuple_ty = *closure_signature.inputs().first()?;
    let TyKind::Tuple(tuple_elements) = tuple_ty.kind() else {
        return None;
    };
    let params = tuple_elements
        .iter()
        .enumerate()
        .filter_map(|(index, element_ty)| {
            let ty = ty_to_oomir_type(element_ty, tcx, data_types, instance);
            ty.has_jvm_value().then(|| (format!("arg{index}"), ty))
        })
        .collect();
    let output_ty = closure_signature.output();
    let signature = oomir::Signature {
        params,
        ret: Box::new(ty_to_oomir_type(output_ty, tcx, data_types, instance)),
        is_static: true,
    };
    let interface_name = ensure_fn_ptr_interface(&signature, data_types, tcx, instance);
    Some(crate::lower1::types::CallableTraitObjectAbi {
        tuple_ty,
        signature,
        interface_name,
    })
}
