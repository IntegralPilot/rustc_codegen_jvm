//! Canonical monomorphic bodies and JVM-visible forwarding methods.
use super::*;

pub(super) fn mono_item_name<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    definitions: &Definitions<'tcx>,
) -> lower1::naming::FnNameData {
    definitions.function_name(tcx, instance)
}

fn receiver_pointer<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    receiver_ty: rustc_middle::ty::Ty<'tcx>,
    signature: &oomir::Signature,
    data_types: &mut Definitions<'tcx>,
) -> Option<oomir::ReceiverPointer> {
    if !matches!(signature.params.first(), Some((_, Type::Pointer(_)))) {
        return None;
    }
    let size = lower1::types::layout_size_bytes(tcx, receiver_ty)
        .unwrap_or_else(|error| panic!("could not determine instance receiver layout: {error}"));
    let alignment = lower1::types::layout_align_bytes(tcx, receiver_ty)
        .unwrap_or_else(|error| panic!("could not determine instance receiver alignment: {error}"));
    let oomir::Operand::Constant(codec) =
        lower1::types::pointer_memory_codec_operand(receiver_ty, tcx, data_types, instance)
    else {
        panic!("receiver codec must be constant");
    };
    Some(oomir::ReceiverPointer {
        size: i32::try_from(size).expect("instance receiver exceeds JVM address space"),
        alignment: i32::try_from(alignment)
            .expect("instance receiver alignment exceeds JVM address space"),
        codec,
    })
}

pub(super) fn place_or_insert_mono_function<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    name: &lower1::naming::FnNameData,
    mut oomir_function: oomir::Function,
    oomir_module: &mut lower1::context::Module<'tcx>,
) {
    let has_global_linkage = name
        .class_to_call_on
        .as_deref()
        .is_some_and(lower1::naming::is_global_link_symbol_class);
    if !has_global_linkage && let Some(assoc_item) = tcx.opt_associated_item(instance.def_id()) {
        let clone_shim_self_ty = match instance.def {
            InstanceKind::Shim(ShimKind::Clone(_, self_ty)) => Some(self_ty),
            _ => None,
        };
        let provided_trait_receiver_ty = assoc_item
            .trait_container(tcx)
            .filter(|trait_def_id| {
                tcx.provided_trait_methods(*trait_def_id)
                    .any(|method| method.def_id == assoc_item.def_id)
            })
            .map(|_| instance.args.type_at(0));
        let attachable_to_receiver_class = clone_shim_self_ty.is_some()
            || (provided_trait_receiver_ty.is_some() && assoc_item.is_method())
            || (assoc_item.trait_container(tcx).is_none()
                && (assoc_item.trait_item_def_id().is_none() || assoc_item.is_method()));
        if attachable_to_receiver_class {
            let container_id = assoc_item.container_id(tcx);
            let container_ty = clone_shim_self_ty
                .or(provided_trait_receiver_ty)
                .unwrap_or_else(|| {
                    tcx.type_of(container_id)
                        .instantiate(tcx, instance.args)
                        .skip_norm_wip()
                });
            let receiver_ty = assoc_item.is_method().then(|| {
                tcx.fn_sig(instance.def_id())
                    .instantiate(tcx, instance.args)
                    .skip_binder()
                    .inputs()
                    .first()
                    .copied()
                    .expect("a Rust method has a receiver")
            });
            let has_arbitrary_self_receiver = receiver_ty.is_some_and(|receiver| {
                let receiver_self = match receiver.kind() {
                    TyKind::Ref(_, pointee, _) => *pointee,
                    _ => receiver,
                };
                receiver_self != container_ty
            });
            // The method may be monomorphized in a downstream crate. Emit a
            // receiver-class fragment there so the linker can attach it to the
            // upstream class definition rather than leaving only a static copy.
            let self_oomir_ty = lower1::types::force_define_named_adt(
                container_ty,
                tcx,
                &mut oomir_module.data_types,
                instance,
            );

            if !has_arbitrary_self_receiver && let Type::Class(class_name) = self_oomir_ty {
                let can_extend_compiled_core_class = lower1::jvm_names::uses_compiled_core(tcx)
                    && (instance.def_id().is_local()
                        || lower1::jvm_names::compiles_external_core_instances(tcx));
                let is_runtime_owned_class =
                    class_name.starts_with("org/rustlang/") && !can_extend_compiled_core_class;
                if !class_name.starts_with("java/") && !is_runtime_owned_class {
                    let mut signature = oomir_function.signature.clone();
                    signature.is_static = !assoc_item.is_method();
                    let method_name = lower1::naming::associated_method_name_from_instance(
                        tcx, instance, &signature,
                    );

                    let implemented_trait_def_id = assoc_item
                        .impl_container(tcx)
                        .and_then(|impl_def_id| tcx.impl_opt_trait_ref(impl_def_id))
                        .map(|trait_ref| {
                            trait_ref
                                .instantiate(tcx, instance.args)
                                .skip_norm_wip()
                                .def_id
                        })
                        .or_else(|| assoc_item.trait_container(tcx));
                    let implemented_trait = implemented_trait_def_id.map(|trait_def_id| {
                        let trait_name = lower1::jvm_names::class_for_def_id(tcx, trait_def_id);
                        ensure_trait_interface(tcx, trait_def_id, &mut oomir_module.data_types);
                        signature.replace_class_in_signature(&trait_name, &class_name);
                        trait_name
                    });

                    let receiver = assoc_item
                        .is_method()
                        .then(|| {
                            receiver_pointer(
                                tcx,
                                instance,
                                container_ty,
                                &signature,
                                &mut oomir_module.data_types,
                            )
                        })
                        .flatten();
                    let forwarder = oomir::MethodForwarder {
                        signature,
                        target_owner: oomir_module
                            .owner_class_for_function(&oomir_function)
                            .to_string(),
                        target_name: oomir_function.name.clone(),
                        target_signature: oomir_function.signature.clone(),
                        receiver,
                        source_file: oomir_function.source_file().map(str::to_owned),
                    };

                    let mut has_instance_method = false;
                    if let Some(
                        oomir::DataType::Class {
                            methods,
                            interfaces,
                            ..
                        }
                        | oomir::DataType::Interface {
                            methods,
                            interfaces,
                            ..
                        },
                    ) = oomir_module.data_types.get_mut(&class_name)
                    {
                        let trait_method_matches_existing = implemented_trait.is_some()
                            && methods
                                .get(&method_name)
                                .and_then(oomir::DataTypeMethod::function_signature)
                                .is_some_and(|signature| {
                                    signature.to_string() == forwarder.signature.to_string()
                                });
                        if let Some(trait_name) = implemented_trait {
                            if !interfaces.contains(&trait_name) {
                                interfaces.push(trait_name);
                            }
                        }
                        if trait_method_matches_existing {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "mono-lowering",
                                format!(
                                    "Kept existing {}.{} for matching trait method; emitted {} as a static fallback",
                                    class_name, method_name, name.method_name
                                )
                            );
                            has_instance_method = true;
                        } else {
                            methods.insert(
                                method_name.clone(),
                                oomir::DataTypeMethod::Forwarder(forwarder),
                            );

                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Info,
                                "mono-lowering",
                                format!(
                                    "Placed mono item {} into class {}",
                                    name.method_name, class_name
                                )
                            );
                            // The class entry owns only a forwarding recipe.
                            // The canonical owner below owns the body.
                            has_instance_method = true;
                        }
                    }

                    if !has_instance_method {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Info,
                            "mono-lowering",
                            format!(
                                "Class {} not declared for mono method {}; keeping it as an owned static function",
                                class_name, name.method_name
                            )
                        );
                    }
                }
            }
        }
    }

    // Emit the canonical owner-module form used by statically resolved Rust
    // calls. JVM module methods are static, so a Rust method's receiver must
    // remain an explicit descriptor parameter in this form.
    oomir_function.signature.is_static = true;
    oomir_module.insert_function(oomir_function);
}
