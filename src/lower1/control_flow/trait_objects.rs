use std::collections::HashMap;

use rustc_middle::ty::{Instance, Ty, TyCtxt, TyKind, TypingEnv, VtblEntry};

use super::super::{
    jvm_names,
    naming::{mono_fn_name_from_instance, mono_owner_class},
    types::{readable_rust_type_name, sanitize_name_token, ty_to_oomir_type},
};
use crate::oomir;

const RUNTIME_TRAIT_OBJECT_CARRIER: &str = "org/rustlang/runtime/TraitObjectCarrier";
const RUNTIME_RUST_DROP: &str = "org/rustlang/runtime/RustDrop";
const RUNTIME_TRAIT_OBJECT_PAYLOAD_METHOD: &str = "rustTraitObjectPayload";
const RUNTIME_TRAIT_OBJECT_SIZE_METHOD: &str = "rustTraitObjectSize";
const RUNTIME_TRAIT_OBJECT_ALIGNMENT_METHOD: &str = "rustTraitObjectAlignment";

pub(super) fn carrier_needs_trait_object_adapter(
    carrier_ty: &oomir::Type,
    interface_name: &str,
    data_types: &HashMap<String, oomir::DataType>,
) -> bool {
    match carrier_ty {
        oomir::Type::Class(class_name) => !matches!(
            data_types.get(class_name),
            Some(oomir::DataType::Class { interfaces, .. })
                if interfaces.iter().any(|interface| interface == interface_name)
        ),
        // A Rust pointer is never itself an implementation of the JVM trait
        // interface, even when its pointee class implements that interface.
        oomir::Type::Pointer(_) => true,
        _ => true,
    }
}

pub(super) fn ensure_trait_object_adapter_class<'tcx>(
    source_mir_ty: Ty<'tcx>,
    target_mir_ty: Ty<'tcx>,
    carrier_ty: &oomir::Type,
    interface_name: &str,
    data_types: &mut HashMap<String, oomir::DataType>,
    tcx: TyCtxt<'tcx>,
    instance_context: Instance<'tcx>,
) -> Result<String, String> {
    let concrete_ty = match source_mir_ty.kind() {
        TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
        _ => {
            return Err(format!(
                "trait-object source is not a pointer or reference: {source_mir_ty:?}"
            ));
        }
    };
    let dynamic_ty = match target_mir_ty.kind() {
        TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
        _ => {
            return Err(format!(
                "trait-object target is not a pointer or reference: {target_mir_ty:?}"
            ));
        }
    };
    ensure_trait_object_adapter_class_for_pointees(
        concrete_ty,
        dynamic_ty,
        carrier_ty,
        interface_name,
        data_types,
        tcx,
        instance_context,
    )
}

pub(crate) fn ensure_trait_object_adapter_class_for_pointees<'tcx>(
    concrete_ty: Ty<'tcx>,
    dynamic_ty: Ty<'tcx>,
    carrier_ty: &oomir::Type,
    interface_name: &str,
    data_types: &mut HashMap<String, oomir::DataType>,
    tcx: TyCtxt<'tcx>,
    instance_context: Instance<'tcx>,
) -> Result<String, String> {
    let TyKind::Dynamic(predicates, _) = dynamic_ty.kind() else {
        return Err(format!(
            "trait-object target is not dynamic: {dynamic_ty:?}"
        ));
    };
    let concrete_size = super::super::types::layout_size_bytes(tcx, concrete_ty)
        .map_err(|error| format!("trait-object pointee size is unavailable: {error}"))?;
    let concrete_alignment = super::super::types::layout_align_bytes(tcx, concrete_ty)
        .map_err(|error| format!("trait-object pointee alignment is unavailable: {error}"))?;
    let trait_ref = predicates.principal().map(|principal| {
        tcx.instantiate_bound_regions_with_erased(principal.with_self_ty(tcx, concrete_ty))
    });
    let callable_abi = super::super::types::callable_trait_object_abi(
        dynamic_ty,
        tcx,
        data_types,
        instance_context,
    );

    let identity = format!(
        "{dynamic_ty:?}:{trait_ref:?}:{}",
        carrier_ty.to_jvm_descriptor()
    );
    let concrete_token = sanitize_name_token(&readable_rust_type_name(
        concrete_ty,
        tcx,
        data_types,
        instance_context,
    ));
    let interface_token = interface_name.rsplit('/').next().unwrap_or(interface_name);
    let local_name = crate::stable_hash::readable_or_hashed_name(
        "TraitObjectCarrier",
        &format!("{concrete_token}_as_{interface_token}"),
        &identity,
        180,
    );
    let class_name = jvm_names::synthetic_class_for_instance(tcx, instance_context, local_name);
    if data_types.contains_key(&class_name) {
        return Ok(class_name);
    }

    let mut adapter_methods = HashMap::new();
    let mut interface_methods = HashMap::new();
    for entry in trait_ref
        .into_iter()
        .flat_map(|trait_ref| tcx.vtable_entries(trait_ref).iter())
    {
        let VtblEntry::Method(target_instance) = entry else {
            continue;
        };

        let target_instance_ty = tcx
            .type_of(target_instance.def_id())
            .instantiate(tcx, target_instance.args)
            .skip_norm_wip();
        if let TyKind::Closure(_, closure_args) = target_instance_ty.kind() {
            let Some(callable_abi) = &callable_abi else {
                return Err(format!(
                    "closure vtable entry does not target a callable trait: {target_instance:?}"
                ));
            };
            let closure_signature = closure_args.as_closure().sig();
            let tuple_ty = *closure_signature
                .inputs()
                .skip_binder()
                .first()
                .ok_or_else(|| {
                    format!("closure call signature has no argument tuple: {target_instance:?}")
                })?;
            let tuple_oomir_ty = ty_to_oomir_type(tuple_ty, tcx, data_types, *target_instance);
            let return_ty = callable_abi.signature.ret.as_ref().clone();
            let payload_name = "_trait_object_payload".to_string();
            let mut instructions = vec![oomir::Instruction::GetField {
                dest: payload_name.clone(),
                object: oomir::Operand::Variable {
                    name: "_1".to_string(),
                    ty: oomir::Type::Class(class_name.clone()),
                },
                field_name: "value".to_string(),
                field_ty: carrier_ty.clone(),
                owner_class: class_name.clone(),
            }];
            let mut target_params = Vec::new();
            let mut target_args = Vec::new();
            if !closure_args.as_closure().upvar_tys().is_empty() {
                target_params.push(("closure_env".to_string(), carrier_ty.clone()));
                target_args.push(oomir::Operand::Variable {
                    name: payload_name,
                    ty: carrier_ty.clone(),
                });
            }
            if tuple_oomir_ty.has_jvm_value() {
                let tuple_name = "_trait_object_call_args".to_string();
                instructions.push(oomir::Instruction::ConstructObject {
                    dest: tuple_name.clone(),
                    class_name: tuple_oomir_ty
                        .get_class_name()
                        .ok_or_else(|| {
                            format!("closure argument tuple has no JVM class: {tuple_oomir_ty:?}")
                        })?
                        .to_string(),
                    args: callable_abi
                        .signature
                        .params
                        .iter()
                        .enumerate()
                        .map(|(index, (_, ty))| {
                            (
                                oomir::Operand::Variable {
                                    name: format!("_{}", index + 2),
                                    ty: ty.clone(),
                                },
                                ty.clone(),
                            )
                        })
                        .collect(),
                });
                target_params.push(("args".to_string(), tuple_oomir_ty.clone()));
                target_args.push(oomir::Operand::Variable {
                    name: tuple_name,
                    ty: tuple_oomir_ty,
                });
            }
            let call_dest = return_ty.has_jvm_value().then(|| "_ret".to_string());
            instructions.extend([
                oomir::Instruction::InvokeStatic {
                    dest: call_dest.clone(),
                    class_name: mono_owner_class(tcx, *target_instance),
                    method_name: super::super::generate_closure_function_name(
                        tcx,
                        *target_instance,
                    ),
                    method_ty: oomir::Signature {
                        params: target_params,
                        ret: Box::new(return_ty.clone()),
                        is_static: true,
                    },
                    args: target_args,
                },
                oomir::Instruction::Return {
                    operand: call_dest.map(|name| oomir::Operand::Variable {
                        name,
                        ty: return_ty.clone(),
                    }),
                },
            ]);

            let interface_params = callable_abi.signature.params.clone();
            let method_name = "call".to_string();
            interface_methods.insert(
                method_name.clone(),
                oomir::Signature {
                    params: interface_params.clone(),
                    ret: Box::new(return_ty.clone()),
                    is_static: false,
                },
            );
            adapter_methods.insert(
                method_name.clone(),
                oomir::DataTypeMethod::Function(oomir::Function {
                    name: method_name,
                    owner_class: None,
                    debug_variables: Vec::new(),
                    signature: oomir::Signature {
                        params: std::iter::once((
                            "self".to_string(),
                            oomir::Type::Class(class_name.clone()),
                        ))
                        .chain(interface_params)
                        .collect(),
                        ret: Box::new(return_ty),
                        is_static: false,
                    },
                    body: oomir::CodeBlock {
                        entry: "bb0".to_string(),
                        basic_blocks: HashMap::from([(
                            "bb0".to_string(),
                            oomir::BasicBlock {
                                label: "bb0".to_string(),
                                instructions,
                            },
                        )]),
                    },
                }),
            );
            continue;
        }
        if callable_abi.is_some() && matches!(concrete_ty.kind(), TyKind::Closure(..)) {
            // The JVM callable interface deliberately exposes one flattened
            // `call` method. The remaining Rust vtable entries are Fn/FnMut/
            // FnOnce shims for the same closure body, not additional methods
            // that belong on that functional interface.
            continue;
        }
        let is_coroutine = matches!(target_instance_ty.kind(), TyKind::Coroutine(..));
        let (target_inputs, target_output) = if is_coroutine {
            let body = tcx.instance_mir(target_instance.def);
            let inputs = (1..=body.arg_count)
                .map(|index| {
                    rustc_middle::ty::EarlyBinder::bind(
                        tcx,
                        body.local_decls[rustc_middle::mir::Local::from_usize(index)].ty,
                    )
                    .instantiate(tcx, target_instance.args)
                    .skip_norm_wip()
                })
                .collect();
            let output = rustc_middle::ty::EarlyBinder::bind(
                tcx,
                body.local_decls[rustc_middle::mir::Local::from_usize(0)].ty,
            )
            .instantiate(tcx, target_instance.args)
            .skip_norm_wip();
            (inputs, output)
        } else {
            let signature = target_instance_ty.fn_sig(tcx).skip_binder();
            (signature.inputs().to_vec(), signature.output())
        };
        let target_params = target_inputs
            .iter()
            .enumerate()
            .map(|(index, ty)| {
                (
                    format!("arg{index}"),
                    ty_to_oomir_type(*ty, tcx, data_types, *target_instance),
                )
            })
            .collect::<Vec<_>>();
        let Some((_, target_receiver_ty)) = target_params.first() else {
            continue;
        };
        let target_receiver_ty = target_receiver_ty.clone();
        let payload_name = "_trait_object_payload".to_string();
        let payload_operand = oomir::Operand::Variable {
            name: payload_name.clone(),
            ty: carrier_ty.clone(),
        };
        let (receiver_setup, receiver_operand) = if target_receiver_ty.to_jvm_descriptor()
            == carrier_ty.to_jvm_descriptor()
        {
            (Vec::new(), payload_operand.clone())
        } else if let oomir::Type::Class(receiver_class) = &target_receiver_ty
            && matches!(
                data_types.get(receiver_class),
                Some(oomir::DataType::Class { fields, .. })
                    if fields.len() == 1
                        && fields[0].1.to_jvm_descriptor()
                            == carrier_ty.to_jvm_descriptor()
            )
        {
            let receiver_name = "_trait_object_receiver".to_string();
            (
                vec![oomir::Instruction::ConstructObject {
                    dest: receiver_name.clone(),
                    class_name: receiver_class.clone(),
                    args: vec![(payload_operand.clone(), carrier_ty.clone())],
                }],
                oomir::Operand::Variable {
                    name: receiver_name,
                    ty: target_receiver_ty.clone(),
                },
            )
        } else {
            return Err(format!(
                "trait method receiver carrier mismatch for {target_instance:?}: stored {}, target {}",
                carrier_ty.to_jvm_descriptor(),
                target_receiver_ty.to_jvm_descriptor()
            ));
        };
        let return_ty = ty_to_oomir_type(target_output, tcx, data_types, *target_instance);
        let target_signature = oomir::Signature {
            params: target_params.clone(),
            ret: Box::new(return_ty.clone()),
            is_static: true,
        };
        let target_name = mono_fn_name_from_instance(tcx, *target_instance);
        let method_def_id = if is_coroutine {
            tcx.opt_associated_item(target_instance.def_id())
                .and_then(|item| item.trait_item_def_id())
                .or_else(|| {
                    let entries = tcx.own_existential_vtable_entries(trait_ref?.def_id);
                    (entries.len() == 1).then_some(entries[0])
                })
                .unwrap_or_else(|| target_instance.def_id())
        } else {
            target_instance.def_id()
        };
        let method_name = jvm_names::method_for_function(tcx, method_def_id);

        let mut call_args = vec![receiver_operand.clone()];
        call_args.extend(
            target_params
                .iter()
                .enumerate()
                .skip(1)
                .map(|(index, (_, ty))| oomir::Operand::Variable {
                    name: format!("_{}", index + 1),
                    ty: ty.clone(),
                }),
        );
        let call_dest = return_ty.has_jvm_value().then(|| "_ret".to_string());
        let get_payload = oomir::Instruction::GetField {
            dest: payload_name,
            object: oomir::Operand::Variable {
                name: "_1".to_string(),
                ty: oomir::Type::Class(class_name.clone()),
            },
            field_name: "value".to_string(),
            field_ty: carrier_ty.clone(),
            owner_class: class_name.clone(),
        };
        let receiver_has_interface = match &target_receiver_ty {
            oomir::Type::Pointer(inner) | oomir::Type::Reference(inner) => match inner.as_ref() {
                oomir::Type::Class(receiver_class) => matches!(
                    data_types.get(receiver_class),
                    Some(oomir::DataType::Class { interfaces, .. })
                        if interfaces.iter().any(|interface| interface == interface_name)
                ),
                _ => false,
            },
            _ => false,
        };
        let call = match &target_receiver_ty {
            oomir::Type::Pointer(inner) | oomir::Type::Reference(inner)
                if matches!(inner.as_ref(), oomir::Type::Class(_)) && receiver_has_interface =>
            {
                let oomir::Type::Class(receiver_class) = inner.as_ref() else {
                    unreachable!()
                };
                let mut method_ty = target_signature.clone();
                method_ty.is_static = false;
                let method_name = super::super::naming::associated_method_name_from_instance(
                    tcx,
                    *target_instance,
                    &method_ty,
                );
                oomir::Instruction::InvokeVirtual {
                    dest: call_dest.clone(),
                    class_name: receiver_class.clone(),
                    method_name,
                    method_ty,
                    args: call_args.into_iter().skip(1).collect(),
                    operand: receiver_operand,
                }
            }
            oomir::Type::Interface(receiver_interface) => {
                let mut method_ty = target_signature.clone();
                method_ty.is_static = false;
                oomir::Instruction::InvokeInterface {
                    dest: call_dest.clone(),
                    class_name: receiver_interface.clone(),
                    method_name: super::super::naming::associated_method_name_from_instance(
                        tcx,
                        *target_instance,
                        &method_ty,
                    ),
                    method_ty,
                    args: call_args.into_iter().skip(1).collect(),
                    operand: receiver_operand,
                }
            }
            _ => oomir::Instruction::InvokeStatic {
                dest: call_dest.clone(),
                class_name: target_name
                    .class_to_call_on
                    .expect("trait-object adapters target functions with JVM owners"),
                method_name: target_name.method_name,
                method_ty: target_signature,
                args: call_args,
            },
        };
        let mut instructions = vec![get_payload];
        instructions.extend(receiver_setup);
        instructions.extend([
            call,
            oomir::Instruction::Return {
                operand: call_dest.map(|name| oomir::Operand::Variable {
                    name,
                    ty: return_ty.clone(),
                }),
            },
        ]);

        let interface_params = target_params.into_iter().skip(1).collect::<Vec<_>>();
        interface_methods.insert(
            method_name.clone(),
            oomir::Signature {
                params: interface_params.clone(),
                ret: Box::new(return_ty.clone()),
                is_static: false,
            },
        );
        adapter_methods.insert(
            method_name.clone(),
            oomir::DataTypeMethod::Function(oomir::Function {
                name: method_name,
                owner_class: None,
                debug_variables: Vec::new(),
                signature: oomir::Signature {
                    params: std::iter::once((
                        "self".to_string(),
                        oomir::Type::Class(class_name.clone()),
                    ))
                    .chain(interface_params)
                    .collect(),
                    ret: Box::new(return_ty),
                    is_static: false,
                },
                body: oomir::CodeBlock {
                    entry: "bb0".to_string(),
                    basic_blocks: HashMap::from([(
                        "bb0".to_string(),
                        oomir::BasicBlock {
                            label: "bb0".to_string(),
                            instructions,
                        },
                    )]),
                },
            }),
        );
    }

    let payload_name = "_trait_object_payload".to_string();
    let boxed_payload_name = "_trait_object_payload_object".to_string();
    adapter_methods.insert(
        RUNTIME_TRAIT_OBJECT_PAYLOAD_METHOD.to_string(),
        oomir::DataTypeMethod::Function(oomir::Function {
            name: RUNTIME_TRAIT_OBJECT_PAYLOAD_METHOD.to_string(),
            owner_class: None,
            debug_variables: Vec::new(),
            signature: oomir::Signature {
                params: vec![("self".to_string(), oomir::Type::Class(class_name.clone()))],
                ret: Box::new(oomir::Type::Class("java/lang/Object".to_string())),
                is_static: false,
            },
            body: oomir::CodeBlock {
                entry: "bb0".to_string(),
                basic_blocks: HashMap::from([(
                    "bb0".to_string(),
                    oomir::BasicBlock {
                        label: "bb0".to_string(),
                        instructions: vec![
                            oomir::Instruction::GetField {
                                dest: payload_name.clone(),
                                object: oomir::Operand::Variable {
                                    name: "_1".to_string(),
                                    ty: oomir::Type::Class(class_name.clone()),
                                },
                                field_name: "value".to_string(),
                                field_ty: carrier_ty.clone(),
                                owner_class: class_name.clone(),
                            },
                            oomir::Instruction::Cast {
                                op: oomir::Operand::Variable {
                                    name: payload_name,
                                    ty: carrier_ty.clone(),
                                },
                                ty: oomir::Type::Class("java/lang/Object".to_string()),
                                dest: boxed_payload_name.clone(),
                            },
                            oomir::Instruction::Return {
                                operand: Some(oomir::Operand::Variable {
                                    name: boxed_payload_name,
                                    ty: oomir::Type::Class("java/lang/Object".to_string()),
                                }),
                            },
                        ],
                    },
                )]),
            },
        }),
    );
    let adapter_drops_payload = concrete_ty.needs_drop(tcx, TypingEnv::fully_monomorphized())
        && matches!(carrier_ty, oomir::Type::Pointer(_));
    if adapter_drops_payload {
        let drop_instance = Instance::resolve_drop_glue(tcx, concrete_ty);
        let target = mono_fn_name_from_instance(tcx, drop_instance);
        let payload_name = "_trait_object_drop_payload".to_string();
        adapter_methods.insert(
            "rustDrop".to_string(),
            oomir::DataTypeMethod::Function(oomir::Function {
                name: "rustDrop".to_string(),
                owner_class: None,
                debug_variables: Vec::new(),
                signature: oomir::Signature {
                    params: vec![("self".to_string(), oomir::Type::Class(class_name.clone()))],
                    ret: Box::new(oomir::Type::Void),
                    is_static: false,
                },
                body: oomir::CodeBlock {
                    entry: "bb0".to_string(),
                    basic_blocks: HashMap::from([(
                        "bb0".to_string(),
                        oomir::BasicBlock {
                            label: "bb0".to_string(),
                            instructions: vec![
                                oomir::Instruction::GetField {
                                    dest: payload_name.clone(),
                                    object: oomir::Operand::Variable {
                                        name: "_1".to_string(),
                                        ty: oomir::Type::Class(class_name.clone()),
                                    },
                                    field_name: "value".to_string(),
                                    field_ty: carrier_ty.clone(),
                                    owner_class: class_name.clone(),
                                },
                                oomir::Instruction::InvokeStatic {
                                    dest: None,
                                    class_name: target
                                        .class_to_call_on
                                        .expect("trait-object payload drop glue has a JVM owner"),
                                    method_name: target.method_name,
                                    method_ty: oomir::Signature {
                                        params: vec![("pointee".to_string(), carrier_ty.clone())],
                                        ret: Box::new(oomir::Type::Void),
                                        is_static: true,
                                    },
                                    args: vec![oomir::Operand::Variable {
                                        name: payload_name,
                                        ty: carrier_ty.clone(),
                                    }],
                                },
                                oomir::Instruction::Return { operand: None },
                            ],
                        },
                    )]),
                },
            }),
        );
    }
    for (method_name, value) in [
        (RUNTIME_TRAIT_OBJECT_SIZE_METHOD, concrete_size as u64),
        (
            RUNTIME_TRAIT_OBJECT_ALIGNMENT_METHOD,
            concrete_alignment as u64,
        ),
    ] {
        adapter_methods.insert(
            method_name.to_string(),
            oomir::DataTypeMethod::Function(oomir::Function {
                name: method_name.to_string(),
                owner_class: None,
                debug_variables: Vec::new(),
                signature: oomir::Signature {
                    params: vec![("self".to_string(), oomir::Type::Class(class_name.clone()))],
                    ret: Box::new(oomir::Type::U64),
                    is_static: false,
                },
                body: oomir::CodeBlock {
                    entry: "bb0".to_string(),
                    basic_blocks: HashMap::from([(
                        "bb0".to_string(),
                        oomir::BasicBlock {
                            label: "bb0".to_string(),
                            instructions: vec![oomir::Instruction::Return {
                                operand: Some(oomir::Operand::Constant(oomir::Constant::U64(
                                    value,
                                ))),
                            }],
                        },
                    )]),
                },
            }),
        );
    }

    match data_types.get_mut(interface_name) {
        Some(oomir::DataType::Interface { methods }) => methods.extend(interface_methods),
        Some(oomir::DataType::Class { .. }) => {
            return Err(format!(
                "trait interface name is already a class: {interface_name}"
            ));
        }
        None => {
            data_types.insert(
                interface_name.to_string(),
                oomir::DataType::Interface {
                    methods: interface_methods,
                },
            );
        }
    }
    let mut interfaces = vec![
        interface_name.to_string(),
        RUNTIME_TRAIT_OBJECT_CARRIER.to_string(),
    ];
    if adapter_drops_payload {
        interfaces.push(RUNTIME_RUST_DROP.to_string());
    }
    data_types.insert(
        class_name.clone(),
        oomir::DataType::Class {
            fields: vec![("value".to_string(), carrier_ty.clone())],
            is_abstract: false,
            methods: adapter_methods,
            super_class: Some("java/lang/Object".to_string()),
            interfaces,
        },
    );
    Ok(class_name)
}
