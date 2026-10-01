//! Rust types and locations control destruction. Erased enum wrappers still own their Drop
//! implementation.
use super::*;

pub(in crate::lower1) fn emit_drop_in_place<'tcx>(
    rust_ty: Ty<'tcx>,
    pointer: oomir::Operand,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    let rust_ty = data_types.normalize(tcx, rust_ty, instance);
    if !rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
        return;
    }
    if matches!(rust_ty.kind(), TyKind::Dynamic(..)) {
        // The dyn drop shim calls virtual Drop. Another call to the shim would cause recursion.
        let value_ty = crate::lower1::types::ty_to_oomir_type(rust_ty, tcx, data_types, instance);
        let prefix = format!("dyn_drop_{}", data_types.next_temporary());
        let value = if matches!(pointer.get_type(), Some(oomir::Type::Pointer(_))) {
            emit_pointer_read(pointer, &value_ty, &prefix, instructions)
        } else {
            pointer
        };
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "dropRustValue".to_string(),
            method_ty: oomir::Signature {
                params: vec![(
                    "value".to_string(),
                    oomir::Type::Class("java/lang/Object".to_string()),
                )],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![value],
            dest: None,
        });
        return;
    }
    let drop_instance = Instance::resolve_drop_glue(tcx, rust_ty);
    let target = data_types.function_name(tcx, drop_instance);
    // The shim can take &mut T. Its ABI differs from *mut T for arrays and trait objects.
    let mir = tcx.instance_mir(drop_instance.def);
    let parameter =
        data_types.normalize(tcx, mir.local_decls[Local::from_usize(1)].ty, drop_instance);
    let pointer_ty =
        crate::lower1::types::ty_to_oomir_type(parameter, tcx, data_types, drop_instance);
    let prefix = format!("drop_place_{}", data_types.next_temporary());
    let pointer = crate::lower1::value_repr::adapt_operand_to_rust_type(
        pointer,
        parameter,
        &prefix,
        tcx,
        drop_instance,
        data_types,
        instructions,
    );
    instructions.push(oomir::Instruction::InvokeRustStatic {
        class_name: target.class_to_call_on.expect("drop glue has a JVM owner"),
        method_name: target.method_name,
        method_ty: oomir::Signature {
            params: vec![("place".to_string(), pointer_ty)],
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        args: vec![pointer],
        dest: None,
    });
}

/// Object callbacks supply owned values. MIR drops supply existing locations.
pub(in crate::lower1) fn emit_owned_drop<'tcx>(
    rust_ty: Ty<'tcx>,
    value: oomir::Operand,
    prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    if !rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
        return;
    }
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(rust_ty))
        .expect("owned drop requires a layout");
    let value_ty = crate::lower1::types::ty_to_oomir_type(rust_ty, tcx, data_types, instance);
    let pointer_ty = oomir::Type::pointer(value_ty.clone());
    let dest = format!("{prefix}_place");
    let codec =
        crate::lower1::types::pointer_memory_codec_operand(rust_ty, tcx, data_types, instance);
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "receiverCellAligned".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                (
                    "value".to_string(),
                    oomir::Type::Class("java/lang/Object".to_string()),
                ),
                ("size".to_string(), oomir::Type::I32),
                ("codec".to_string(), oomir::Type::java_string()),
                ("alignment".to_string(), oomir::Type::I32),
            ],
            ret: Box::new(pointer_ty.clone()),
            is_static: true,
        },
        args: vec![
            if value_ty.has_jvm_value() {
                value
            } else {
                oomir::Operand::Constant(oomir::Constant::Unit)
            },
            oomir::Operand::Constant(oomir::Constant::I32(
                i32::try_from(layout.size.bytes()).expect("drop value exceeds JVM size"),
            )),
            codec,
            oomir::Operand::Constant(oomir::Constant::I32(
                i32::try_from(layout.align.abi.bytes()).expect("drop alignment exceeds JVM size"),
            )),
        ],
        dest: Some(dest.clone()),
    });
    emit_drop_in_place(
        rust_ty,
        oomir::Operand::Variable {
            name: dest,
            ty: pointer_ty,
        },
        tcx,
        instance,
        data_types,
        instructions,
    );
}
