//! Discover and materialize the Rust library surface exposed to Java.
use super::*;

pub(super) fn ensure_trait_interface<'tcx>(
    tcx: TyCtxt<'tcx>,
    trait_def_id: DefId,
    data_types: &mut Definitions<'tcx>,
) {
    let interface_name = lower1::jvm_names::class_for_def_id(tcx, trait_def_id);
    let methods = trait_interface_methods(tcx, trait_def_id, &interface_name, data_types);

    match data_types.get_mut(&interface_name) {
        Some(oomir::DataType::Interface {
            methods: existing_methods,
            ..
        }) => {
            existing_methods.extend(
                methods
                    .into_iter()
                    .map(|(name, signature)| (name, oomir::DataTypeMethod::Abstract(signature))),
            );
        }
        Some(oomir::DataType::Class { .. }) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "mono-lowering",
                format!(
                    "Trait interface '{}' already exists as a class; leaving it unchanged",
                    interface_name
                )
            );
        }
        None => {
            data_types.insert(
                interface_name,
                oomir::DataType::Interface {
                    methods: methods
                        .into_iter()
                        .map(|(name, signature)| (name, oomir::DataTypeMethod::Abstract(signature)))
                        .collect(),
                    interfaces: vec![],
                    is_enum: false,
                },
            );
        }
    }
}

pub(super) fn trait_interface_methods<'tcx>(
    tcx: TyCtxt<'tcx>,
    trait_def_id: DefId,
    interface_name: &str,
    data_types: &mut Definitions<'tcx>,
) -> HashMap<String, oomir::Signature> {
    let mut methods = HashMap::default();

    for assoc_item in tcx.associated_items(trait_def_id).in_definition_order() {
        let def_id = assoc_item.def_id;
        // Trait functions without a receiver are statically dispatched. JVM
        // interfaces cannot declare an abstract static method, so only methods
        // which participate in interface dispatch belong in this table.
        if !assoc_item.is_method() {
            continue;
        }

        let mir_sig = tcx.type_of(def_id).skip_binder().fn_sig(tcx);
        let params_ty = mir_sig.inputs();
        let return_ty = mir_sig.output();
        let explicit_inputs = params_ty.skip_binder();
        let output = return_ty.skip_binder();
        let instance = Instance::new_raw(
            def_id,
            rustc_middle::ty::GenericArgs::identity_for_item(tcx, def_id),
        );
        let has_open_abi_type = |ty: rustc_middle::ty::Ty<'tcx>| {
            lower1::types::has_open_jvm_abi_type(ty, tcx, instance)
        };
        if explicit_inputs
            .iter()
            .skip(1)
            .copied()
            .any(has_open_abi_type)
            || has_open_abi_type(output)
        {
            continue;
        }
        let params_oomir: Vec<(String, oomir::Type)> = explicit_inputs
            .iter()
            .enumerate()
            .filter_map(|(i, ty)| {
                if assoc_item.is_method() && i == 0 {
                    None
                } else {
                    let param_name = format!("arg{}", i);
                    let oomir_type =
                        lower1::types::ty_to_erased_oomir_type(*ty, tcx, data_types, instance);
                    Some((param_name, oomir_type))
                }
            })
            .collect();
        let return_oomir_ty =
            lower1::types::ty_to_erased_oomir_type(output, tcx, data_types, instance);

        let mut signature = oomir::Signature {
            params: params_oomir,
            ret: Box::new(return_oomir_ty),
            is_static: false,
        };
        let (params_changed, _) = signature.replace_class_in_signature("Self", interface_name);

        if params_changed {
            signature.is_static = false;
        }

        methods.insert(assoc_item.name().as_str().to_string(), signature);
    }

    methods
}

pub(super) fn crate_emits_library_artifact(tcx: TyCtxt<'_>) -> bool {
    tcx.crate_types()
        .iter()
        .any(|crate_type| !matches!(crate_type, CrateType::Executable))
}

pub(super) fn is_lowerable_java_public_function(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    if !matches!(tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn) {
        return false;
    }

    if let Some(assoc_item) = tcx.opt_associated_item(def_id) {
        if assoc_item.trait_container(tcx).is_some() {
            return false;
        }
        if tcx.crate_name(LOCAL_CRATE) == rustc_span::sym::core
            && assoc_item.impl_container(tcx).is_some()
        {
            return false;
        }
    }

    def_id.is_local()
        && !tcx.generics_of(def_id).requires_monomorphization(tcx)
        && tcx.is_mir_available(def_id)
}

pub(super) enum JavaPublicSurface {
    Exported,
    Reachable,
}

pub(super) fn java_public_surface_def_ids(
    tcx: TyCtxt<'_>,
    surface: JavaPublicSurface,
) -> Vec<DefId> {
    let effective_visibilities = tcx.effective_visibilities(());
    let mut def_ids: Vec<_> = effective_visibilities
        .iter()
        .filter_map(|(&local_def_id, _)| {
            let is_public_enough = match surface {
                JavaPublicSurface::Exported => effective_visibilities.is_exported(local_def_id),
                JavaPublicSurface::Reachable => effective_visibilities.is_reachable(local_def_id),
            };
            is_public_enough.then_some(local_def_id.to_def_id())
        })
        .collect();

    def_ids.sort_by_cached_key(|def_id| tcx.def_path_str(*def_id));
    def_ids
}

pub(super) fn materialize_java_public_data_type<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    oomir_module: &mut lower1::context::Module<'tcx>,
) {
    match tcx.def_kind(def_id) {
        DefKind::Struct | DefKind::Enum | DefKind::Union => {
            if !tcx.generics_of(def_id).own_params.is_empty() {
                return;
            }
            let item_ty = tcx.type_of(def_id).instantiate_identity().skip_norm_wip();
            let instance_context =
                Instance::new_raw(def_id, GenericArgs::identity_for_item(tcx, def_id));
            lower1::types::ty_to_oomir_type(
                item_ty,
                tcx,
                &mut oomir_module.data_types,
                instance_context,
            );
        }
        DefKind::Trait => ensure_trait_interface(tcx, def_id, &mut oomir_module.data_types),
        _ => {}
    }
}

pub(super) fn lower_public_library_exports<'tcx>(
    tcx: TyCtxt<'tcx>,
    partitioned_functions: &HashSet<Instance<'tcx>>,
    oomir_module: &mut lower1::context::Module<'tcx>,
    lowered_instances: &mut HashSet<Instance<'tcx>>,
    scanned_instances: &mut HashSet<Instance<'tcx>>,
) {
    if !crate_emits_library_artifact(tcx) {
        return;
    }

    let function_defs = java_public_surface_def_ids(tcx, JavaPublicSurface::Exported)
        .into_iter()
        .filter(|def_id| is_lowerable_java_public_function(tcx, *def_id))
        .collect::<Vec<_>>();
    let mut function_roots = function_defs
        .iter()
        .copied()
        .map(|def_id| Instance::mono(tcx, def_id))
        .collect::<Vec<_>>();
    for function_def in &function_defs {
        let Some(local_def) = function_def.as_local() else {
            continue;
        };
        for nested_def in tcx.nested_bodies_within(local_def) {
            let coroutine_def = nested_def.to_def_id();
            if !tcx.coroutine_is_async(coroutine_def) {
                continue;
            }
            let coroutine_ty = tcx
                .type_of(coroutine_def)
                .instantiate_identity()
                .skip_norm_wip();
            let TyKind::Coroutine(_, args) = coroutine_ty.kind() else {
                continue;
            };
            function_roots.push(Instance::new_raw(coroutine_def, args));
        }
    }
    // Rustc's collector owns ordinary Rust reachability. Java exports are
    // additional roots. Async state-machine bodies are also roots because a
    // JVM caller polls them through RustFuture rather than an ordinary MIR call.
    lower_supplemental_instance_closure(
        tcx,
        function_roots,
        partitioned_functions,
        oomir_module,
        lowered_instances,
        scanned_instances,
    );

    let data_type_defs = java_public_surface_def_ids(tcx, JavaPublicSurface::Reachable);

    for def_id in data_type_defs {
        materialize_java_public_data_type(tcx, def_id, oomir_module);
    }
}
