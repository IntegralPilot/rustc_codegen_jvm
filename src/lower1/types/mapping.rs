use super::*;
use crate::lower1::context::Definitions;

pub(super) fn raw_waker_vtable_ty<'tcx>(tcx: TyCtxt<'tcx>) -> Ty<'tcx> {
    let waker_def_id = tcx
        .get_diagnostic_item(Symbol::intern("Waker"))
        .expect("core::task::Waker diagnostic item is unavailable");
    let waker_args = GenericArgs::identity_for_item(tcx, waker_def_id);
    let raw_waker_ty = tcx.adt_def(waker_def_id).non_enum_variant().fields[FieldIdx::from_usize(0)]
        .ty(tcx, waker_args)
        .skip_norm_wip();
    let TyKind::Adt(raw_waker_def, raw_waker_args) = raw_waker_ty.kind() else {
        panic!("core::task::Waker no longer contains a RawWaker");
    };
    let vtable_ref_ty = raw_waker_def.non_enum_variant().fields[FieldIdx::from_usize(1)]
        .ty(tcx, raw_waker_args)
        .skip_norm_wip();
    let TyKind::Ref(_, vtable_ty, _) = vtable_ref_ty.kind() else {
        panic!("core::task::RawWaker no longer contains a RawWakerVTable reference");
    };
    *vtable_ty
}

/// Converts a fully monomorphized Rust MIR type (`Ty`) to an OOMIR type.
pub(crate) fn ty_to_oomir_type<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Type {
    if !ty.has_param()
        && !ty.has_escaping_bound_vars()
        && let Some(cached) = data_types.representations.get(&ty).cloned()
    {
        crate::metrics::record_type_cache_hit();
        return cached;
    }
    let resolved_ty = data_types.normalize(tcx, ty, instance_context);
    if let Some(cached) = data_types.representations.get(&resolved_ty).cloned() {
        crate::metrics::record_type_cache_hit();
        return cached;
    }
    crate::metrics::record_type_cache_miss();
    let lowered = ty_to_oomir_type_resolved(resolved_ty, tcx, data_types, instance_context);
    data_types
        .representations
        .insert(resolved_ty, lowered.clone());
    lowered
}

pub(crate) fn is_codegen_sized<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> bool {
    ty.has_trivial_sizedness(tcx, rustc_middle::ty::SizedTraitKind::Sized)
        || (!ty.has_escaping_bound_vars() && ty.is_sized(tcx, TypingEnv::fully_monomorphized()))
}

pub(super) fn ty_to_oomir_type_resolved<'tcx>(
    resolved_ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Type {
    match resolved_ty.kind() {
        rustc_middle::ty::TyKind::Bool => oomir::Type::Boolean,
        // Rust `char` is a 32-bit Unicode scalar value, unlike the JVM's
        // 16-bit UTF-16 `char` primitive. Keep it as an int so supplementary
        // characters are not truncated.
        rustc_middle::ty::TyKind::Char => oomir::Type::I32,
        rustc_middle::ty::TyKind::Int(int_ty) => match int_ty {
            IntTy::I8 => oomir::Type::I8,
            IntTy::I16 => oomir::Type::I16,
            IntTy::I32 => oomir::Type::I32,
            IntTy::I64 => oomir::Type::I64,
            IntTy::Isize => oomir::Type::I64,
            IntTy::I128 => oomir::Type::Class(crate::lower2::I128_CLASS.to_string()),
        },
        rustc_middle::ty::TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => oomir::Type::U8,
            UintTy::U16 => oomir::Type::U16,
            UintTy::U32 => oomir::Type::U32,
            UintTy::Usize => oomir::Type::U64,
            UintTy::U64 => oomir::Type::U64,
            UintTy::U128 => oomir::Type::Class(crate::lower2::U128_CLASS.to_string()),
        },
        rustc_middle::ty::TyKind::Float(float_ty) => match float_ty {
            FloatTy::F32 => oomir::Type::F32,
            FloatTy::F64 => oomir::Type::F64,
            FloatTy::F16 => oomir::Type::F16,
            FloatTy::F128 => oomir::Type::Class(crate::lower2::F128_CLASS.to_string()),
        },
        rustc_middle::ty::TyKind::Adt(adt_def, substs) => {
            if crate::lower1::is_non_null_lang_item(tcx, adt_def.did())
                && let Some(pointee) = substs.iter().find_map(|arg| arg.as_type())
                && is_codegen_sized(pointee, tcx)
            {
                return oomir::Type::Pointer(Box::new(ty_to_oomir_type(
                    pointee,
                    tcx,
                    data_types,
                    instance_context,
                )));
            }
            let jvm_name_full =
                generate_adt_jvm_class_name(&adt_def, substs, tcx, data_types, instance_context);

            if !should_define_named_data_type(tcx, adt_def.did()) && substs.is_empty() {
                return oomir::Type::Class(jvm_name_full);
            }

            ensure_adt_data_type(
                adt_def,
                substs,
                &jvm_name_full,
                tcx,
                data_types,
                instance_context,
            );
            oomir::Type::Class(jvm_name_full)
        }
        rustc_middle::ty::TyKind::Str => oomir::Type::Str,
        // A linked foreign type names the JVM class carried by pointers and
        // references to that opaque Rust type. Unlinked compiler-internal
        // foreign types (i.e. core's VTable marker) remain valueless.
        // Rust still enforces the extern type's DST restrictions; the backend
        // only supplies its JVM ABI.
        rustc_middle::ty::TyKind::Foreign(def_id) => {
            let Some((link_name, span)) = rustc_hir::find_attr!(
                tcx,
                *def_id,
                LinkName { name, span } => (*name, *span)
            ) else {
                return oomir::Type::Unit;
            };
            let class_name = crate::lower1::naming::parse_jvm_class_link_name(link_name.as_str())
                .unwrap_or_else(|message| tcx.dcx().span_fatal(span, message));
            oomir::Type::Class(class_name)
        }
        rustc_middle::ty::TyKind::Pat(inner_ty, _) => {
            ty_to_oomir_type(*inner_ty, tcx, data_types, instance_context)
        }
        rustc_middle::ty::TyKind::Ref(_, inner_ty, _mutability) => {
            let pointee_oomir_type = ty_to_oomir_type(*inner_ty, tcx, data_types, instance_context);
            if matches!(inner_ty.kind(), rustc_middle::ty::TyKind::Foreign(_))
                && matches!(pointee_oomir_type, oomir::Type::Class(_))
            {
                // A Java object reference is already the thin pointer for an
                // opaque foreign Java type; do not wrap it in runtime Pointer.
                return pointee_oomir_type;
            }
            // For trait objects (&dyn Trait, &mut dyn Trait), represent as direct Interface
            // rather than using the array wrapper, since we call virtual methods on the object
            if matches!(
                inner_ty.kind(),
                rustc_middle::ty::TyKind::Dynamic(_, _)
                    | rustc_middle::ty::TyKind::Slice(_)
                    | rustc_middle::ty::TyKind::Str
            ) {
                pointee_oomir_type
            } else if let rustc_middle::ty::TyKind::Array(element_ty, _) = inner_ty.kind() {
                // Fixed arrays retain their JVM array allocation and use a
                // SliceView when borrowed. Element pointers created from that
                // view still share the original backing allocation.
                oomir::Type::Slice(Box::new(ty_to_oomir_type(
                    *element_ty,
                    tcx,
                    data_types,
                    instance_context,
                )))
            } else {
                // Sized references carry a stable address. Mutability remains
                // a Rust type-system property; both reference kinds use the
                // same JVM pointer representation so reborrows preserve identity.
                oomir::Type::Pointer(Box::new(pointee_oomir_type))
            }
        }
        rustc_middle::ty::TyKind::RawPtr(ty, _mutability) => {
            if ty.is_str() {
                // A raw pointer to a string slice (*const str) is semantically a reference
                // to string data. Its OOMIR representation should be consistent with &str.
                oomir::Type::Str
            } else if ty.is_slice() {
                // Preserve the pointer metadata as a slice view.
                let component_ty = ty.sequence_element_type(tcx);
                let oomir_component_type =
                    ty_to_oomir_type(component_ty, tcx, data_types, instance_context);
                oomir::Type::Slice(Box::new(oomir_component_type))
            } else if matches!(ty.kind(), rustc_middle::ty::TyKind::Foreign(_)) {
                // See the reference case above: the JVM object reference is
                // the linked extern type's native thin-pointer
                // representation. Preserve runtime Pointer for unlinked
                // compiler-internal foreign markers.
                let pointee_oomir_type = ty_to_oomir_type(*ty, tcx, data_types, instance_context);
                if matches!(pointee_oomir_type, oomir::Type::Class(_)) {
                    pointee_oomir_type
                } else {
                    oomir::Type::Pointer(Box::new(pointee_oomir_type))
                }
            } else {
                // Sized raw pointers and sized references intentionally share
                // one address representation. This preserves identity across
                // const/mut casts and makes arithmetic independent of the JVM ABI.
                let oomir_pointee_type = ty_to_oomir_type(*ty, tcx, data_types, instance_context);
                if matches!(oomir_pointee_type, oomir::Type::Void) {
                    oomir::Type::Pointer(Box::new(oomir::Type::Unit))
                } else {
                    oomir::Type::Pointer(Box::new(oomir_pointee_type))
                }
            }
        }
        rustc_middle::ty::TyKind::Array(component_ty, _) => {
            // Special case for arrays of string references
            if let TyKind::Ref(_, inner_ty, _) = component_ty.kind() {
                if inner_ty.is_str() {
                    return oomir::Type::Array(Box::new(oomir::Type::Str));
                }
            }
            // Default array handling
            oomir::Type::Array(Box::new(ty_to_oomir_type(
                *component_ty,
                tcx,
                data_types,
                instance_context,
            )))
        }
        rustc_middle::ty::TyKind::Tuple(tuple_elements) => {
            // Unit is an inhabited Rust value, but occupies no JVM stack or local slot.
            if tuple_elements.is_empty() {
                return oomir::Type::Unit;
            }

            // Handle non-empty tuples -> generate a class
            let element_mir_tys: Vec<Ty<'tcx>> = tuple_elements.iter().collect(); // Collect MIR types

            // Generate the JVM class name for this specific tuple type
            let tuple_class_name =
                generate_tuple_jvm_class_name(&element_mir_tys, tcx, data_types, instance_context);

            // Check if we've already created the DataType for this tuple signature
            if !data_types.contains_key(&tuple_class_name) {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "type-mapping",
                    format!(
                        "Info: Defining new tuple type class: {} for MIR type {:?}",
                        tuple_class_name, resolved_ty
                    )
                );
                // Create the fields ("field0", "field1", ...) and their OOMIR types
                let oomir_fields = element_mir_tys
                    .iter()
                    .enumerate()
                    .map(|(i, &elem_ty)| {
                        let field_name = format!("field{}", i);
                        // Recursively convert element type to OOMIR type
                        let field_oomir_type =
                            ty_to_oomir_type(elem_ty, tcx, data_types, instance_context);
                        (field_name, field_oomir_type)
                    })
                    .collect::<Vec<_>>();

                let mut methods = HashMap::default();
                methods.insert(
                    "eq".to_string(),
                    DataTypeMethod::AdtHelperMethod {
                        kind: oomir::AdtHelperKind::PartialEqClass {
                            fields: oomir_fields.clone(),
                        },
                    },
                );

                // Create and insert the DataType definition
                let tuple_data_type = oomir::DataType::Class {
                    fields: oomir_fields,
                    is_abstract: false,
                    methods,
                    super_class: None,
                    interfaces: vec![],
                };
                data_types.insert(tuple_class_name.clone(), tuple_data_type);
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "type-mapping",
                    format!("   -> Added DataType: {:?}", data_types[&tuple_class_name])
                );
            } else {
                if let Some(oomir::DataType::Class {
                    fields, methods, ..
                }) = data_types.get_mut(&tuple_class_name)
                {
                    methods.entry("eq".to_string()).or_insert_with(|| {
                        DataTypeMethod::AdtHelperMethod {
                            kind: oomir::AdtHelperKind::PartialEqClass {
                                fields: fields.clone(),
                            },
                        }
                    });
                }
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "type-mapping",
                    format!(
                        "Info: Reusing existing tuple type class: {}",
                        tuple_class_name
                    )
                );
            }

            let needs_managed_drop = !resolved_ty.has_param()
                && !resolved_ty.has_escaping_bound_vars()
                && resolved_ty.needs_drop(tcx, TypingEnv::fully_monomorphized())
                && matches!(
                    data_types.get(&tuple_class_name),
                    Some(oomir::DataType::Class { methods, .. })
                        if !methods.contains_key(MANAGED_DROP_METHOD)
                );
            if needs_managed_drop {
                if let Some(oomir::DataType::Class {
                    methods,
                    interfaces,
                    ..
                }) = data_types.get_mut(&tuple_class_name)
                {
                    methods.insert(
                        MANAGED_DROP_METHOD.to_string(),
                        DataTypeMethod::SimpleConstantReturn(oomir::Type::Void, None),
                    );
                    if !interfaces
                        .iter()
                        .any(|interface| interface == MANAGED_DROP_INTERFACE)
                    {
                        interfaces.push(MANAGED_DROP_INTERFACE.to_string());
                    }
                }
                let drop_method = managed_drop_glue_function(
                    resolved_ty,
                    &tuple_class_name,
                    tcx,
                    data_types,
                    instance_context,
                );
                if let Some(oomir::DataType::Class { methods, .. }) =
                    data_types.get_mut(&tuple_class_name)
                {
                    methods.insert(
                        MANAGED_DROP_METHOD.to_string(),
                        DataTypeMethod::Function(drop_method),
                    );
                }
            }

            // Return the OOMIR type as a Class reference
            oomir::Type::Class(tuple_class_name)
        }
        rustc_middle::ty::TyKind::Slice(component_ty) => {
            // Special case for slices of string references
            if let TyKind::Ref(_, inner_ty, _) = component_ty.kind() {
                if inner_ty.is_str() {
                    return oomir::Type::Slice(Box::new(oomir::Type::Str));
                }
            }
            // Default slice handling
            oomir::Type::Slice(Box::new(ty_to_oomir_type(
                *component_ty,
                tcx,
                data_types,
                instance_context,
            )))
        }
        rustc_middle::ty::TyKind::Never => {
            // Handle the never type
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "type-mapping",
                "Info: Mapping Never type to OOMIR Void"
            );
            oomir::Type::Void
        }
        rustc_middle::ty::TyKind::Dynamic(bound_preds, _region) => {
            if let Some(callable_abi) =
                callable_trait_object_abi(resolved_ty, tcx, data_types, instance_context)
            {
                return oomir::Type::Interface(callable_abi.interface_name);
            }
            let needs_specialized_interface =
                bound_preds
                    .iter()
                    .any(|predicate| match predicate.skip_binder() {
                        ExistentialPredicate::Projection(_) => true,
                        ExistentialPredicate::Trait(trait_ref) => trait_ref
                            .args
                            .iter()
                            .any(|arg| arg.as_type().is_some() || arg.as_const().is_some()),
                        ExistentialPredicate::AutoTrait(_) => false,
                    });
            let dynamic_name = needs_specialized_interface.then(|| {
                let mut dynamic_key = String::new();
                let mut readable_parts = Vec::new();
                for predicate in bound_preds.iter() {
                    match predicate.skip_binder() {
                        ExistentialPredicate::Trait(trait_ref) => {
                            dynamic_key.push_str("trait=");
                            dynamic_key.push_str(&stable_def_path(tcx, trait_ref.def_id));
                            dynamic_key.push('[');
                            for arg in trait_ref.args.iter() {
                                if let Some(token) = append_dynamic_generic_arg_key(
                                    &mut dynamic_key,
                                    arg,
                                    tcx,
                                    data_types,
                                    instance_context,
                                ) {
                                    readable_parts.push(sanitize_name_token(&token));
                                }
                            }
                            dynamic_key.push_str("];");
                        }
                        ExistentialPredicate::Projection(projection) => {
                            dynamic_key.push_str("projection=");
                            dynamic_key.push_str(&stable_def_path(tcx, projection.def_id));
                            dynamic_key.push('[');
                            for arg in projection.args.iter() {
                                if let Some(token) = append_dynamic_generic_arg_key(
                                    &mut dynamic_key,
                                    arg,
                                    tcx,
                                    data_types,
                                    instance_context,
                                ) {
                                    readable_parts.push(sanitize_name_token(&token));
                                }
                            }
                            dynamic_key.push_str("]=");
                            if let Some(term) = readable_rust_generic_arg_name(
                                projection.term.into_arg(),
                                tcx,
                                data_types,
                                instance_context,
                            ) {
                                dynamic_key.push_str(&term);
                                readable_parts.push(format!(
                                    "{}_{}",
                                    sanitize_name_token(tcx.item_name(projection.def_id).as_str()),
                                    sanitize_name_token(&term)
                                ));
                            }
                            dynamic_key.push(';');
                        }
                        // Auto traits have no methods or JVM descriptor impact.
                        ExistentialPredicate::AutoTrait(_) => {}
                    }
                }
                (dynamic_key, readable_parts.join("_"))
            });
            // bound_preds is a collection of `Binder<ExistentialPredicate<'tcx>>` entries.
            // Iterate and resolve trait predicates into OOMIR interface types.
            let mut resolved_types: Vec<oomir::Type> = Vec::new();
            for binder in bound_preds.iter() {
                match binder.skip_binder() {
                    ExistentialPredicate::Trait(trait_ref) => {
                        let base_name = data_types.class_name(tcx, trait_ref.def_id);
                        let safe_name = if let Some((dynamic_key, readable_suffix)) = &dynamic_name
                        {
                            crate::stable_hash::readable_or_hashed_name(
                                &format!("{base_name}_Dyn"),
                                readable_suffix,
                                dynamic_key,
                                180,
                            )
                        } else {
                            base_name
                        };
                        if should_define_named_data_type(tcx, trait_ref.def_id) {
                            data_types.entry(safe_name.clone()).or_insert_with(|| {
                                oomir::DataType::Interface {
                                    methods: HashMap::default(),
                                    interfaces: vec![],
                                    is_enum: false,
                                }
                            });
                        }
                        resolved_types.push(oomir::Type::Interface(safe_name));
                    }
                    ExistentialPredicate::AutoTrait(def_id) => {
                        // Auto traits like Send/Sync — treat as interfaces as well.
                        let safe_name = data_types.class_name(tcx, def_id);
                        if should_define_named_data_type(tcx, def_id) {
                            data_types.entry(safe_name.clone()).or_insert_with(|| {
                                oomir::DataType::Interface {
                                    methods: HashMap::default(),
                                    interfaces: vec![],
                                    is_enum: false,
                                }
                            });
                        }
                        resolved_types.push(oomir::Type::Interface(safe_name));
                    }
                    // Associated-type constraints such as `FnOnce<Output = T>`
                    // refine the primary trait and do not represent another JVM
                    // interface in the trait object's runtime carrier.
                    ExistentialPredicate::Projection(_) => {}
                }
            }
            // Return the first resolved bound, or fall back to Object.
            resolved_types
                .get(0)
                .cloned()
                .unwrap_or(oomir::Type::Class("java/lang/Object".to_string()))
        }
        rustc_middle::ty::TyKind::Param(param_ty) => panic!(
            "unresolved generic parameter `{}` reached monomorphic JVM type lowering for {ty:?} in {instance_context:?}",
            param_ty.name,
            ty = resolved_ty,
        ),
        rustc_middle::ty::TyKind::Closure(def_id, args) => {
            let safe_name = data_types.closure_class_name(tcx, *def_id, args, false);

            // Define the closure class struct if not already present
            if !data_types.contains_key(&safe_name) {
                let closure_args = args.as_closure();
                let upvar_tys = closure_args.upvar_tys();

                let mut fields = Vec::new();
                for (i, upvar_ty) in upvar_tys.iter().enumerate() {
                    let field_name = format!("arg{}", i);
                    // Recursively resolve capture types
                    let field_oomir_ty =
                        ty_to_oomir_type(upvar_ty, tcx, data_types, instance_context);
                    fields.push((field_name, field_oomir_ty));
                }

                data_types.insert(
                    safe_name.clone(),
                    oomir::DataType::Class {
                        fields,
                        is_abstract: false,
                        methods: HashMap::default(), // 'call' is handled via MIR lowering logic
                        super_class: Some("java/lang/Object".to_string()),
                        interfaces: vec![],
                    },
                );
            }
            oomir::Type::Class(safe_name)
        }
        rustc_middle::ty::TyKind::Coroutine(def_id, args) => {
            let safe_name = data_types.closure_class_name(tcx, *def_id, args, true);

            if data_types.contains_key(&safe_name) {
                return oomir::Type::Class(safe_name);
            }
            // Install a recursion guard before lowering captured/saved types;
            // a coroutine may retain another value whose type reaches back to
            // this state machine.
            data_types.insert(
                safe_name.clone(),
                oomir::DataType::Class {
                    fields: vec![("__state".to_string(), oomir::Type::I32)],
                    is_abstract: false,
                    methods: HashMap::default(),
                    super_class: Some("java/lang/Object".to_string()),
                    interfaces: vec![],
                },
            );

            // A coroutine is a state-machine object. Captures are prefix
            // fields; locals live across suspension points are a separate,
            // flattened field set shared by all state variants.
            let mut fields = args
                .as_coroutine()
                .upvar_tys()
                .iter()
                .enumerate()
                .filter_map(|(index, upvar_ty)| {
                    let field_ty = ty_to_oomir_type(upvar_ty, tcx, data_types, instance_context);
                    field_ty
                        .has_jvm_value()
                        .then_some((format!("arg{index}"), field_ty))
                })
                .collect::<Vec<_>>();
            fields.push(("__state".to_string(), oomir::Type::I32));
            if let Ok(layout) = tcx.coroutine_layout(*def_id, args) {
                for (saved_local, saved_ty) in layout.field_tys.iter_enumerated() {
                    let saved_ty = EarlyBinder::bind(tcx, saved_ty.ty)
                        .instantiate(tcx, args)
                        .skip_norm_wip();
                    let field_ty = ty_to_oomir_type(saved_ty, tcx, data_types, instance_context);
                    if field_ty.has_jvm_value() {
                        fields.push((format!("state{}", saved_local.as_usize()), field_ty));
                    }
                }
            }
            if let Some(oomir::DataType::Class {
                fields: existing_fields,
                ..
            }) = data_types.get_mut(&safe_name)
            {
                *existing_fields = fields;
            }
            let needs_managed_drop = resolved_ty.needs_drop(tcx, TypingEnv::fully_monomorphized())
                && matches!(
                    data_types.get(&safe_name),
                    Some(oomir::DataType::Class { methods, .. })
                        if !methods.contains_key(MANAGED_DROP_METHOD)
                );
            if needs_managed_drop {
                if let Some(oomir::DataType::Class {
                    methods,
                    interfaces,
                    ..
                }) = data_types.get_mut(&safe_name)
                {
                    methods.insert(
                        MANAGED_DROP_METHOD.to_string(),
                        DataTypeMethod::SimpleConstantReturn(oomir::Type::Void, None),
                    );
                    interfaces.push(MANAGED_DROP_INTERFACE.to_string());
                }
                let drop_method = managed_drop_glue_function(
                    resolved_ty,
                    &safe_name,
                    tcx,
                    data_types,
                    instance_context,
                );
                if let Some(oomir::DataType::Class { methods, .. }) = data_types.get_mut(&safe_name)
                {
                    methods.insert(
                        MANAGED_DROP_METHOD.to_string(),
                        DataTypeMethod::Function(drop_method),
                    );
                }
            }
            if tcx.coroutine_is_async(*def_id) {
                let future_size = layout_size_bytes(tcx, resolved_ty)
                    .unwrap_or_else(|error| panic!("could not size Rust async value: {error}"));
                let future_alignment = layout_align_bytes(tcx, resolved_ty)
                    .unwrap_or_else(|error| panic!("could not align Rust async value: {error}"));
                let future_codec =
                    pointer_memory_codec_operand(resolved_ty, tcx, data_types, instance_context);
                let vtable_ty = raw_waker_vtable_ty(tcx);
                let vtable_size = layout_size_bytes(tcx, vtable_ty)
                    .unwrap_or_else(|error| panic!("could not size RawWakerVTable: {error}"));
                let vtable_alignment = layout_align_bytes(tcx, vtable_ty)
                    .unwrap_or_else(|error| panic!("could not align RawWakerVTable: {error}"));
                let vtable_codec =
                    pointer_memory_codec_operand(vtable_ty, tcx, data_types, instance_context);
                crate::async_interop::add_rust_future_bridge(
                    data_types,
                    &safe_name,
                    crate::async_interop::PointerLayout {
                        size: i32::try_from(future_size)
                            .expect("Rust async value exceeds the JVM address space"),
                        alignment: i32::try_from(future_alignment)
                            .expect("Rust async alignment exceeds the JVM address space"),
                        codec: future_codec,
                    },
                    crate::async_interop::PointerLayout {
                        size: i32::try_from(vtable_size)
                            .expect("RawWakerVTable exceeds the JVM address space"),
                        alignment: i32::try_from(vtable_alignment)
                            .expect("RawWakerVTable alignment exceeds the JVM address space"),
                        codec: vtable_codec,
                    },
                );
            }
            oomir::Type::Class(safe_name)
        }
        rustc_middle::ty::TyKind::FnPtr(_, _) => {
            let signature =
                fn_ptr_signature_from_ty(resolved_ty, tcx, data_types, instance_context);
            let interface_name =
                ensure_fn_ptr_interface(&signature, data_types, tcx, instance_context);
            oomir::Type::Interface(interface_name)
        }
        // Function items have no runtime state. Their Rust definition and
        // generic arguments already participate in naming and call resolution;
        // carrying a separate empty JVM object adds no information.
        rustc_middle::ty::TyKind::FnDef(..) => oomir::Type::Unit,
        rustc_middle::ty::TyKind::Alias(_, alias_ty) => panic!(
            "unresolved type alias/projection {alias_ty:?} reached monomorphic JVM type lowering for {ty:?} in {instance_context:?}",
            ty = resolved_ty
        ),
        _ => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!("Warning: Unhandled type {:?}", resolved_ty)
            );
            oomir::Type::Class("java/lang/Object".to_string())
        }
    }
}

// A helper function to convert MIR integer values to OOMIR Constants, respecting type
pub(crate) fn mir_int_to_oomir_const<'tcx>(
    value: u128,
    ty: Ty<'tcx>,
    _tcx: TyCtxt<'tcx>,
) -> oomir::Constant {
    match ty.kind() {
        TyKind::Int(int_ty) => match int_ty {
            // Cast u128 carefully to avoid panic/wrap-around if value is out of range
            IntTy::I8 => oomir::Constant::I8(value as i8),
            IntTy::I16 => oomir::Constant::I16(value as i16),
            IntTy::I32 => oomir::Constant::I32(value as i32),
            IntTy::I64 => oomir::Constant::I64(value as i64),
            IntTy::Isize => oomir::Constant::I64(value as i64),
            IntTy::I128 => oomir::Constant::Instance {
                class_name: crate::lower2::I128_CLASS.to_string(),
                // MIR carries integer bits in a u128; preserve the signed
                // two's-complement interpretation for i128 constants.
                params: vec![oomir::Constant::String((value as i128).to_string())],
                fields: HashMap::default(),
                param_types: Vec::new(),
            }, // Handle large integers
        },
        TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => oomir::Constant::U8(value as u8),
            UintTy::U16 => oomir::Constant::U16(value as u16),
            UintTy::U32 => oomir::Constant::U32(value as u32),
            UintTy::Usize | UintTy::U64 => oomir::Constant::U64(value as u64),
            UintTy::U128 => oomir::Constant::Instance {
                class_name: crate::lower2::U128_CLASS.to_string(),
                params: vec![oomir::Constant::String(value.to_string())],
                fields: HashMap::default(),
                param_types: Vec::new(),
            },
        },
        TyKind::Bool => oomir::Constant::Boolean(value != 0), // 0 is false, non-zero is true
        TyKind::Char => oomir::Constant::I32(value as i32),
        _ => {
            // This case should ideally not happen if MIR is well-typed
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!(
                    "Warning: Cannot convert MIR integer value {} to OOMIR constant for non-integer type {:?}",
                    value, ty
                )
            );
            oomir::Constant::I32(0) // Default fallback
        }
    }
}
