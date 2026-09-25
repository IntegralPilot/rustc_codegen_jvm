//! Pointer address.
use super::*;

pub(super) fn copy_or_fill<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    label: &str,
    instructions: &mut Vec<oomir::Instruction>,
    receiver_mir_ty: Ty<'tcx>,
    receiver_operand: oomir::Operand,
    explicit_method_args: &[oomir::Operand],
    declared_method_name: String,
    dispatch_receiver_ty: oomir::Type,
) {
    let receiver_ty = EarlyBinder::bind(tcx, receiver_mir_ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let pointee = match receiver_ty.kind() {
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
        other => panic!("pointer memory method receiver has non-pointer type {other:?}"),
    };
    let element_size =
        crate::lower1::types::layout_size_bytes(tcx, pointee).unwrap_or_else(|error| {
            panic!("could not determine pointer memory method element size: {error}")
        });
    let byte_count_name = format!("{label}_{declared_method_name}_byte_count");
    instructions.push(oomir::Instruction::Binary {
        op: crate::oomir::BinaryOp::Mul,
        dest: byte_count_name.clone(),
        op1: explicit_method_args[1].clone(),
        op2: oomir::Operand::Constant(oomir::Constant::U64(element_size as u64)),
    });
    let byte_count = oomir::Operand::Variable {
        name: byte_count_name,
        ty: oomir::Type::U64,
    };
    if declared_method_name == "write_bytes" {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "writeBytes".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("destination".to_string(), dispatch_receiver_ty.clone()),
                    ("value".to_string(), oomir::Type::I32),
                    ("byte_count".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![
                receiver_operand,
                explicit_method_args[0].clone(),
                byte_count,
            ],
            dest: None,
        });
    } else {
        let from_receiver = declared_method_name.starts_with("copy_to");
        let source = if from_receiver {
            receiver_operand.clone()
        } else {
            explicit_method_args[0].clone()
        };
        let destination = if from_receiver {
            explicit_method_args[0].clone()
        } else {
            receiver_operand
        };
        let source_ty = source.get_type().expect("pointer copy source is typed");
        let destination_ty = destination
            .get_type()
            .expect("pointer copy destination is typed");
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: if declared_method_name.ends_with("nonoverlapping") {
                "copyNonOverlapping".to_string()
            } else {
                "copy".to_string()
            },
            method_ty: oomir::Signature {
                params: vec![
                    ("source".to_string(), source_ty),
                    ("destination".to_string(), destination_ty),
                    ("byte_count".to_string(), oomir::Type::U64),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![source, destination, byte_count],
            dest: None,
        });
    }
}
pub(super) fn map_address<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
    label: &str,
    instructions: &mut Vec<oomir::Instruction>,
    args: &[rustc_span::Spanned<MirOperand<'tcx>>],
    typing_env: TypingEnv<'tcx>,
    effective_dest: Option<String>,
    receiver_operand: oomir::Operand,
    explicit_method_args: &[oomir::Operand],
    dispatch_receiver_ty: oomir::Type,
) {
    // `map_addr` is generic over a Rust `FnOnce`. A closure value is
    // only its captured environment on the JVM; its body is emitted as
    // a static method on the crate module class. Invoke that body here
    // and then apply the returned address while retaining provenance.
    let mapper_ty = EarlyBinder::bind(tcx, args[1].node.ty(mir, tcx))
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let address_dest = format!("{label}_map_addr_input");
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "addr".to_string(),
        method_ty: oomir::Signature {
            params: vec![("pointer".to_string(), dispatch_receiver_ty.clone())],
            ret: Box::new(oomir::Type::U64),
            is_static: true,
        },
        args: vec![receiver_operand.clone()],
        dest: Some(address_dest.clone()),
    });
    let mapped_address_dest = format!("{label}_map_addr_output");
    let address_operand = oomir::Operand::Variable {
        name: address_dest,
        ty: oomir::Type::U64,
    };
    let mapped_address_ty = match mapper_ty.kind() {
        TyKind::Closure(closure_def_id, closure_args) => {
            let closure_instance = Instance::new_raw(*closure_def_id, *closure_args);
            let closure_signature = closure_args.as_closure().sig();
            let closure_inputs = closure_signature.inputs().skip_binder().to_vec();
            let closure_output = closure_signature.output().skip_binder();
            let closure_output_oomir_ty =
                crate::lower1::types::ty_to_oomir_type(closure_output, tcx, data_types, instance);
            let closure_arg_ty = *closure_inputs
                .first()
                .expect("FnOnce closure signature has an argument tuple");
            let closure_arg_oomir_ty =
                crate::lower1::types::ty_to_oomir_type(closure_arg_ty, tcx, data_types, instance);
            let closure_tuple_dest = format!("{label}_map_addr_closure_tuple");
            let closure_tuple_class = closure_arg_oomir_ty
                .get_class_name()
                .expect("FnOnce closure argument tuple is a JVM class")
                .to_string();
            instructions.push(oomir::Instruction::ConstructObject {
                dest: closure_tuple_dest.clone(),
                class_name: closure_tuple_class,
                args: vec![(address_operand.clone(), oomir::Type::U64)],
            });

            let captures = closure_args
                .as_closure()
                .upvar_tys()
                .iter()
                .next()
                .is_some();
            let mut closure_params = closure_inputs
                .iter()
                .enumerate()
                .map(|(index, input)| {
                    (
                        format!("arg{index}"),
                        crate::lower1::types::ty_to_oomir_type(*input, tcx, data_types, instance),
                    )
                })
                .collect::<Vec<_>>();
            let mut closure_call_args = vec![oomir::Operand::Variable {
                name: closure_tuple_dest,
                ty: closure_arg_oomir_ty,
            }];
            if captures {
                let environment_ty = explicit_method_args[0]
                    .get_type()
                    .expect("capturing map_addr closure has an environment");
                closure_params.insert(0, ("closure_env".to_string(), environment_ty));
                closure_call_args.insert(0, explicit_method_args[0].clone());
            }
            let target = data_types.function_name(tcx, closure_instance);
            instructions.push(oomir::Instruction::InvokeRustStatic {
                class_name: target.class_to_call_on.expect("closure has a JVM owner"),
                method_name: target.method_name,
                method_ty: oomir::Signature {
                    params: closure_params,
                    ret: Box::new(closure_output_oomir_ty.clone()),
                    is_static: true,
                },
                args: closure_call_args,
                dest: Some(mapped_address_dest.clone()),
            });
            closure_output_oomir_ty
        }
        TyKind::FnDef(def_id, generic_args) => {
            let mapper_instance = Instance::resolve_for_fn_ptr(
                tcx,
                typing_env,
                *def_id,
                generic_args.no_bound_vars().unwrap(),
            )
            .expect("map_addr function item resolves");
            let target = data_types.function_name(tcx, mapper_instance);
            let mapper_signature = mapper_ty.fn_sig(tcx).skip_binder();
            let mapper_input_oomir_ty = crate::lower1::types::ty_to_oomir_type(
                mapper_signature.inputs()[0],
                tcx,
                data_types,
                instance,
            );
            let mapper_output_oomir_ty = crate::lower1::types::ty_to_oomir_type(
                mapper_signature.output(),
                tcx,
                data_types,
                instance,
            );
            instructions.push(oomir::Instruction::InvokeRustStatic {
                class_name: target
                    .class_to_call_on
                    .expect("map_addr function item has a JVM owner"),
                method_name: target.method_name,
                method_ty: oomir::Signature {
                    params: vec![("address".to_string(), mapper_input_oomir_ty)],
                    ret: Box::new(mapper_output_oomir_ty.clone()),
                    is_static: true,
                },
                args: vec![address_operand],
                dest: Some(mapped_address_dest.clone()),
            });
            mapper_output_oomir_ty
        }
        TyKind::FnPtr(_, _) => {
            let signature = crate::lower1::types::fn_ptr_signature_from_ty(
                mapper_ty, tcx, data_types, instance,
            );
            crate::lower1::types::ensure_fn_ptr_interface(&signature, data_types, tcx, instance);
            let mapper_output_oomir_ty = signature.ret.as_ref().clone();
            instructions.push(oomir::Instruction::CallIndirect {
                dest: Some(mapped_address_dest.clone()),
                function_ptr: Box::new(explicit_method_args[0].clone()),
                args: vec![address_operand],
                signature,
            });
            mapper_output_oomir_ty
        }
        other => panic!("raw pointer map_addr requires a monomorphized callable, got {other:?}"),
    };
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "with_addr".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), dispatch_receiver_ty.clone()),
                ("address".to_string(), oomir::Type::U64),
            ],
            ret: Box::new(dispatch_receiver_ty.clone()),
            is_static: true,
        },
        args: vec![
            receiver_operand,
            oomir::Operand::Variable {
                name: mapped_address_dest,
                ty: mapped_address_ty,
            },
        ],
        dest: effective_dest,
    });
}
