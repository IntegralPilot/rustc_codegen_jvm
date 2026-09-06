use super::*;
use crate::lower1::context::Definitions;

pub(crate) fn fn_ptr_signature_from_ty<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Signature {
    let sig = ty.fn_sig(tcx).skip_binder();
    let params = sig
        .inputs()
        .iter()
        .enumerate()
        .map(|(i, ty)| {
            (
                format!("arg{}", i),
                ty_to_oomir_type(*ty, tcx, data_types, instance_context),
            )
        })
        .collect();
    let ret = ty_to_oomir_type(sig.output(), tcx, data_types, instance_context);

    oomir::Signature {
        params,
        ret: Box::new(ret),
        is_static: true,
    }
}

pub(crate) fn ensure_fn_ptr_interface<'tcx>(
    signature: &oomir::Signature,
    data_types: &mut Definitions<'tcx>,
    _tcx: TyCtxt<'tcx>,
    _instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    // A function-pointer ABI is identified by its descriptor, not by the
    // crate which happened to instantiate it. Crate-local owners allowed the
    // same core aggregate field to alternate between core.FnPtr_*,
    // compiler_builtins.FnPtr_*, and panic.FnPtr_* as jars were merged.
    let interface_name = format!("org/rustlang/runtime/{}", signature.fn_ptr_interface_name());
    let method_signature = signature.fn_ptr_interface_method_signature();

    match data_types.get_mut(&interface_name) {
        Some(oomir::DataType::Interface { methods, .. }) => {
            methods
                .entry("call".to_string())
                .or_insert(oomir::DataTypeMethod::Abstract(method_signature));
        }
        Some(oomir::DataType::Class { .. }) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!(
                    "Function pointer interface name '{}' already exists as a class",
                    interface_name
                )
            );
        }
        None => {
            data_types.insert(
                interface_name.clone(),
                oomir::DataType::Interface {
                    methods: HashMap::from_iter([(
                        "call".to_string(),
                        oomir::DataTypeMethod::Abstract(method_signature),
                    )]),
                    interfaces: vec![],
                    is_enum: false,
                },
            );
        }
    }

    interface_name
}

pub(crate) fn callable_trait_object_abi<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Option<CallableTraitObjectAbi<'tcx>> {
    let instantiated = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance_context.args)
        .skip_norm_wip();
    let ty = tcx
        .try_normalize_erasing_regions(
            TypingEnv::fully_monomorphized(),
            rustc_middle::ty::Unnormalized::new_wip(instantiated),
        )
        .unwrap_or(instantiated);
    let dynamic_ty = match ty.kind() {
        TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
        _ => ty,
    };
    let TyKind::Dynamic(predicates, _) = dynamic_ty.kind() else {
        return None;
    };
    let principal = predicates.principal()?.skip_binder();
    let lang_items = tcx.lang_items();
    if ![
        lang_items.fn_trait(),
        lang_items.fn_mut_trait(),
        lang_items.fn_once_trait(),
    ]
    .contains(&Some(principal.def_id))
    {
        return None;
    }

    let tuple_ty = principal.args.iter().find_map(|arg| arg.as_type())?;
    let output_ty = predicates.iter().find_map(|predicate| {
        let ExistentialPredicate::Projection(projection) = predicate.skip_binder() else {
            return None;
        };
        (tcx.lang_items().fn_once_output() == Some(projection.def_id))
            .then(|| projection.term.into_arg().as_type())
            .flatten()
    })?;

    let TyKind::Tuple(tuple_elements) = tuple_ty.kind() else {
        return None;
    };
    let params = tuple_elements
        .iter()
        .enumerate()
        .filter_map(|(index, element_ty)| {
            let element_ty = EarlyBinder::bind(tcx, element_ty)
                .instantiate(tcx, instance_context.args)
                .skip_norm_wip();
            let oomir_ty = ty_to_oomir_type(element_ty, tcx, data_types, instance_context);
            oomir_ty
                .has_jvm_value()
                .then(|| (format!("arg{index}"), oomir_ty))
        })
        .collect();
    let output_ty = EarlyBinder::bind(tcx, output_ty)
        .instantiate(tcx, instance_context.args)
        .skip_norm_wip();
    let signature = oomir::Signature {
        params,
        ret: Box::new(ty_to_oomir_type(
            output_ty,
            tcx,
            data_types,
            instance_context,
        )),
        is_static: true,
    };
    let interface_name = ensure_fn_ptr_interface(&signature, data_types, tcx, instance_context);
    Some(CallableTraitObjectAbi {
        tuple_ty,
        signature,
        interface_name,
    })
}

pub(super) fn normalize_open_abi_type<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Ty<'tcx> {
    let instantiated = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance_context.args)
        .skip_norm_wip();
    tcx.try_normalize_erasing_regions(
        TypingEnv::fully_monomorphized(),
        rustc_middle::ty::Unnormalized::new_wip(instantiated),
    )
    .unwrap_or(instantiated)
}

pub(crate) fn has_open_jvm_abi_type<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> bool {
    let resolved = normalize_open_abi_type(ty, tcx, instance_context);
    resolved.has_param()
        || resolved.has_non_region_bound_vars()
        || matches!(resolved.kind(), TyKind::Alias(..))
}

pub(crate) fn ty_to_erased_oomir_type<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Type {
    let resolved = normalize_open_abi_type(ty, tcx, instance_context);

    if resolved.has_param()
        || resolved.has_non_region_bound_vars()
        || matches!(resolved.kind(), TyKind::Alias(..))
    {
        oomir::Type::Class("java/lang/Object".to_string())
    } else {
        ty_to_oomir_type(resolved, tcx, data_types, instance_context)
    }
}
