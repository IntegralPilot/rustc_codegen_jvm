//! Bind Rust allocation entry points to the JVM runtime ABI.
use super::*;

pub(super) fn allocator_shim_target_signature(
    method: &rustc_ast::expand::allocator::AllocatorMethod,
) -> oomir::Signature {
    use rustc_ast::expand::allocator::AllocatorTy;

    let mut params = Vec::new();
    for input in method.inputs {
        match input.ty {
            AllocatorTy::Layout => {
                params.push((format!("{}_size", input.name), oomir::Type::U64));
                params.push((format!("{}_align", input.name), oomir::Type::U64));
            }
            AllocatorTy::Ptr => params.push((
                input.name.to_string(),
                oomir::Type::Pointer(Box::new(oomir::Type::U8)),
            )),
            AllocatorTy::Usize => {
                params.push((input.name.to_string(), oomir::Type::U64));
            }
            AllocatorTy::Never | AllocatorTy::ResultPtr | AllocatorTy::Unit => {
                panic!("invalid allocator shim input type")
            }
        }
    }

    let ret = match method.output {
        AllocatorTy::ResultPtr => oomir::Type::Pointer(Box::new(oomir::Type::U8)),
        AllocatorTy::Never | AllocatorTy::Unit => oomir::Type::Void,
        AllocatorTy::Layout | AllocatorTy::Ptr | AllocatorTy::Usize => {
            panic!("invalid allocator shim output type")
        }
    };
    oomir::Signature {
        params,
        ret: Box::new(ret),
        is_static: true,
    }
}

pub(super) fn allocator_shim_source_signature(
    tcx: TyCtxt<'_>,
    source_name: &str,
) -> Option<oomir::Signature> {
    let declaration = std::iter::once(LOCAL_CRATE)
        .chain(tcx.crates(()).iter().copied())
        .flat_map(|crate_num| tcx.foreign_modules(crate_num).values())
        .flat_map(|module| module.foreign_items.iter().copied())
        .find(|def_id| {
            if tcx.def_kind(*def_id) != DefKind::Fn {
                return false;
            }
            let name =
                lower1::naming::mono_fn_name_from_instance(tcx, Instance::mono(tcx, *def_id));
            name.method_name == source_name
                && name
                    .class_to_call_on
                    .as_deref()
                    .is_some_and(lower1::naming::is_global_link_symbol_class)
        })?;
    let instance = Instance::mono(tcx, declaration);
    let instance_ty = tcx
        .type_of(declaration)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    Some(lower1::types::fn_ptr_signature_from_ty(
        instance_ty,
        tcx,
        &mut lower1::context::Definitions::default(),
        instance,
    ))
}

pub(super) fn allocator_shim_call(
    source_signature: &oomir::Signature,
    target_signature: &oomir::Signature,
    target_name: String,
    result: Option<String>,
) -> Vec<oomir::Instruction> {
    assert_eq!(
        source_signature.params.len(),
        target_signature.params.len(),
        "allocator shim source and target parameter counts differ"
    );

    let mut instructions = Vec::new();
    let mut args = Vec::new();
    for (index, ((_, source_ty), (_, target_ty))) in source_signature
        .params
        .iter()
        .zip(&target_signature.params)
        .enumerate()
    {
        let source = oomir::Operand::Variable {
            name: format!("_{}", index + 1),
            ty: source_ty.clone(),
        };
        if source_ty == target_ty {
            args.push(source);
        } else if let (oomir::Type::Class(class_name), oomir::Type::U64) = (source_ty, target_ty) {
            // Rust exposes Alignment nominally, while the default allocator keeps its usize ABI.
            let converted = format!("converted_arg_{index}");
            instructions.push(oomir::Instruction::InvokeStatic {
                class_name: class_name.clone(),
                method_name: "as_usize".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("value".to_string(), source_ty.clone())],
                    ret: Box::new(oomir::Type::U64),
                    is_static: true,
                },
                args: vec![source],
                dest: Some(converted.clone()),
            });
            args.push(oomir::Operand::Variable {
                name: converted,
                ty: oomir::Type::U64,
            });
        } else {
            panic!(
                "unsupported allocator ABI argument conversion from {source_ty:?} to {target_ty:?}"
            );
        }
    }

    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: lower1::naming::global_link_symbol_class(&target_name),
        method_name: target_name,
        method_ty: target_signature.clone(),
        args,
        dest: result,
    });
    instructions
}

pub(super) fn emit_allocator_shims<'tcx>(
    tcx: TyCtxt<'tcx>,
    oomir_module: &mut lower1::context::Module<'tcx>,
) {
    use rustc_ast::expand::allocator::{
        ALLOCATOR_METHODS, AllocatorTy, NO_ALLOC_SHIM_IS_UNSTABLE, default_fn_name, global_fn_name,
    };

    let allocator_kind = rustc_codegen_ssa::base::allocator_kind_for_codegen(tcx);
    let methods = if let Some(kind) = allocator_kind {
        rustc_codegen_ssa::base::allocator_shim_contents(tcx, kind)
    } else {
        // Native rlibs defer their allocator choice until a later link. A
        // packaged JVM JAR is already the runnable artifact, so a crate with
        // the allocator ABI in scope needs default wrappers in its own output.
        let global_alloc =
            lower1::jvm_names::member_name(&global_fn_name(ALLOCATOR_METHODS[0].name));
        if allocator_shim_source_signature(tcx, &global_alloc).is_none() {
            return;
        }
        ALLOCATOR_METHODS.to_vec()
    };
    for method in methods {
        let source_name = lower1::jvm_names::member_name(&global_fn_name(method.name));
        let target_name = lower1::jvm_names::member_name(&default_fn_name(method.name));
        let mut signature = allocator_shim_source_signature(tcx, &source_name)
            .unwrap_or_else(|| panic!("allocator ABI declaration `{source_name}` was not found"));
        let target_signature = allocator_shim_target_signature(&method);
        for ((_, source_ty), (_, target_ty)) in
            signature.params.iter_mut().zip(&target_signature.params)
        {
            if let (oomir::Type::Class(class_name), oomir::Type::Pointer(_)) =
                (&*source_ty, target_ty)
                && oomir::is_non_null_class_name(class_name)
            {
                // Global-link lowering already exposes NonNull as the raw JVM pointer carrier.
                *source_ty = target_ty.clone();
            }
        }
        assert_eq!(
            signature.ret.to_jvm_return_descriptor(),
            target_signature.ret.to_jvm_return_descriptor(),
            "allocator shim source and target JVM return types differ"
        );
        let result = matches!(method.output, AllocatorTy::ResultPtr).then(|| "result".to_string());
        let mut instructions =
            allocator_shim_call(&signature, &target_signature, target_name, result.clone());
        if matches!(method.output, AllocatorTy::Never) {
            instructions.push(oomir::Instruction::ThrowNewWithMessage {
                exception_class: "java/lang/AssertionError".to_string(),
                message: "Diverging allocator call returned unexpectedly".to_string(),
            });
        } else {
            instructions.push(oomir::Instruction::Return {
                operand: result.map(|name| oomir::Operand::Variable {
                    name,
                    ty: signature.ret.as_ref().clone(),
                }),
            });
        }

        let entry = "entry".to_string();
        oomir_module.insert_function(oomir::Function {
            owner_class: Some(lower1::naming::global_link_symbol_class(&source_name)),
            name: source_name,
            signature,
            debug_variables: Vec::new(),
            body: oomir::CodeBlock {
                entry: entry.clone(),
                basic_blocks: HashMap::from_iter([(
                    entry.clone(),
                    oomir::BasicBlock {
                        label: entry,
                        instructions,
                    },
                )]),
            }
            .into(),
        });
    }

    let entry = "entry".to_string();
    let symbol_name = lower1::jvm_names::member_name(NO_ALLOC_SHIM_IS_UNSTABLE);
    oomir_module.insert_function(oomir::Function {
        owner_class: Some(lower1::naming::global_link_symbol_class(&symbol_name)),
        name: symbol_name,
        signature: oomir::Signature {
            params: Vec::new(),
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        debug_variables: Vec::new(),
        body: oomir::CodeBlock {
            entry: entry.clone(),
            basic_blocks: HashMap::from_iter([(
                entry.clone(),
                oomir::BasicBlock {
                    label: entry,
                    instructions: vec![oomir::Instruction::Return { operand: None }],
                },
            )]),
        }
        .into(),
    });
}
