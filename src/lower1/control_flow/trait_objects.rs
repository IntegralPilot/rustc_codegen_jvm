use std::collections::HashMap;

use rustc_middle::ty::{Instance, Ty, TyCtxt, TyKind, VtblEntry};

use super::super::{
    jvm_names,
    naming::mono_fn_name_from_instance,
    types::{short_hash, ty_to_oomir_type},
};
use crate::oomir;

pub(super) fn carrier_needs_trait_object_adapter(
    carrier_ty: &oomir::Type,
    data_types: &HashMap<String, oomir::DataType>,
) -> bool {
    match carrier_ty {
        oomir::Type::Class(class_name) => !matches!(
            data_types.get(class_name),
            Some(oomir::DataType::Class { .. })
        ),
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
        TyKind::Ref(_, pointee, _) => *pointee,
        _ => {
            return Err(format!(
                "trait-object source is not a reference: {source_mir_ty:?}"
            ));
        }
    };
    let dynamic_ty = match target_mir_ty.kind() {
        TyKind::Ref(_, pointee, _) => *pointee,
        _ => {
            return Err(format!(
                "trait-object target is not a reference: {target_mir_ty:?}"
            ));
        }
    };
    let TyKind::Dynamic(predicates, _) = dynamic_ty.kind() else {
        return Err(format!(
            "trait-object target is not dynamic: {dynamic_ty:?}"
        ));
    };
    let principal = predicates
        .principal()
        .ok_or_else(|| format!("trait object has no principal trait: {dynamic_ty:?}"))?;
    let trait_ref =
        tcx.instantiate_bound_regions_with_erased(principal.with_self_ty(tcx, concrete_ty));

    let identity = format!("{trait_ref:?}:{}", carrier_ty.to_jvm_descriptor());
    let class_name = jvm_names::synthetic_class_for_instance(
        tcx,
        instance_context,
        format!("TraitObjectCarrier_{}", short_hash(&identity, 12)),
    );
    if data_types.contains_key(&class_name) {
        return Ok(class_name);
    }

    let mut adapter_methods = HashMap::new();
    let mut interface_methods = HashMap::new();
    for entry in tcx.vtable_entries(trait_ref) {
        let VtblEntry::Method(target_instance) = entry else {
            continue;
        };

        let target_instance_ty = tcx
            .type_of(target_instance.def_id())
            .instantiate(tcx, target_instance.args)
            .skip_norm_wip();
        let target_fn_sig = target_instance_ty.fn_sig(tcx).skip_binder();
        let target_params = target_fn_sig
            .inputs()
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
        if target_receiver_ty.to_jvm_descriptor() != carrier_ty.to_jvm_descriptor() {
            return Err(format!(
                "trait method receiver carrier mismatch for {target_instance:?}: expected {}, found {}",
                carrier_ty.to_jvm_descriptor(),
                target_receiver_ty.to_jvm_descriptor()
            ));
        }
        let return_ty = ty_to_oomir_type(target_fn_sig.output(), tcx, data_types, *target_instance);
        let target_signature = oomir::Signature {
            params: target_params.clone(),
            ret: Box::new(return_ty.clone()),
            is_static: true,
        };
        let target_name = mono_fn_name_from_instance(tcx, *target_instance);
        let method_name = jvm_names::method_for_function(tcx, target_instance.def_id());

        let payload_name = "_trait_object_payload".to_string();
        let mut call_args = vec![oomir::Operand::Variable {
            name: payload_name.clone(),
            ty: carrier_ty.clone(),
        }];
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
        let instructions = vec![
            oomir::Instruction::GetField {
                dest: payload_name,
                object: oomir::Operand::Variable {
                    name: "_1".to_string(),
                    ty: oomir::Type::Class(class_name.clone()),
                },
                field_name: "value".to_string(),
                field_ty: carrier_ty.clone(),
                owner_class: class_name.clone(),
            },
            oomir::Instruction::Call {
                dest: call_dest.clone(),
                class_name: target_name.class_to_call_on,
                function: target_name.method_name,
                signature: target_signature,
                args: call_args,
            },
            oomir::Instruction::Return {
                operand: call_dest.map(|name| oomir::Operand::Variable {
                    name,
                    ty: return_ty.clone(),
                }),
            },
        ];

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
    data_types.insert(
        class_name.clone(),
        oomir::DataType::Class {
            fields: vec![("value".to_string(), carrier_ty.clone())],
            is_abstract: false,
            methods: adapter_methods,
            super_class: Some("java/lang/Object".to_string()),
            interfaces: vec![interface_name.to_string()],
        },
    );
    Ok(class_name)
}
