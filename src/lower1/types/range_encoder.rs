//! Bounded scalar snapshots for allocation-free, overlap-safe aggregate stores.
use super::*;
use crate::lower1::context::Definitions;

pub(super) fn supports<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> bool {
    fn visit<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>, budget: &mut usize) -> bool {
        if *budget == 0 {
            return false;
        }
        *budget -= 1;
        match ty.kind() {
            TyKind::Bool | TyKind::Char | TyKind::Int(_) | TyKind::Uint(_) => true,
            TyKind::Float(kind) => *kind != FloatTy::F128,
            TyKind::Tuple(fields) => fields.iter().all(|ty| visit(ty, tcx, budget)),
            TyKind::Adt(def, args) if def.is_struct() => def
                .non_enum_variant()
                .fields
                .iter()
                .all(|field| visit(field.ty(tcx, args).skip_norm_wip(), tcx, budget)),
            TyKind::Array(element, length) => {
                length.try_to_target_usize(tcx).is_some_and(|length| {
                    length <= *budget as u64 && (0..length).all(|_| visit(*element, tcx, budget))
                })
            }
            _ => false,
        }
    }
    visit(ty, tcx, &mut 64)
}

pub(super) fn emit<'tcx>(
    ty: Ty<'tcx>,
    source: oomir::Operand,
    storage: &JvmUnionStorage,
    tcx: TyCtxt<'tcx>,
    definitions: &mut Definitions<'tcx>,
    instance: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    counter: &mut usize,
) -> Result<(), String> {
    let mut leaves = Vec::new();
    snapshot(
        ty,
        source,
        0,
        tcx,
        definitions,
        instance,
        instructions,
        counter,
        &mut leaves,
    )?;
    // The source array can also be the destination. All source reads must precede padding and
    // destination writes.
    let offset = storage.byte_index(0, instructions, counter);
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: None,
        class_name: MEMORY_BYTES_CLASS.into(),
        method_name: "clear".into(),
        method_ty: oomir::Signature {
            params: vec![
                ("bytes".into(), byte_array_type()),
                ("offset".into(), oomir::Type::I32),
                ("size".into(), oomir::Type::I32),
            ],
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        args: vec![
            operand_var(storage.bytes_var.clone(), byte_array_type()),
            offset,
            oomir::Operand::Constant(oomir::Constant::I32(layout_size_bytes(tcx, ty)? as i32)),
        ],
    });
    for (ty, value, offset) in leaves {
        emit_scalar_to_union_bytes(
            ty,
            value,
            storage,
            offset,
            tcx,
            definitions,
            instance,
            instructions,
            counter,
        )?;
    }
    Ok(())
}

fn snapshot<'tcx>(
    ty: Ty<'tcx>,
    source: oomir::Operand,
    offset: usize,
    tcx: TyCtxt<'tcx>,
    definitions: &mut Definitions<'tcx>,
    instance: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    counter: &mut usize,
    leaves: &mut Vec<(Ty<'tcx>, oomir::Operand, usize)>,
) -> Result<(), String> {
    let ty = value_scalar_ty(ty, tcx).unwrap_or(ty);
    if let Some(payload) = transparent_payload(ty, tcx) {
        return snapshot(
            payload.ty,
            source,
            offset,
            tcx,
            definitions,
            instance,
            instructions,
            counter,
            leaves,
        );
    }
    if layout_size_bytes(tcx, ty)? == 0 {
        return Ok(());
    }
    match ty.kind() {
        TyKind::Array(element, length) => {
            let length = length
                .try_to_target_usize(tcx)
                .ok_or("unknown range encoder array length")?;
            let stride = layout_size_bytes(tcx, *element)?;
            let element_ty = ty_to_oomir_type(*element, tcx, definitions, instance);
            for index in 0..length {
                let dest = next_union_temp("range_element", counter);
                instructions.push(oomir::Instruction::ArrayGet {
                    dest: dest.clone(),
                    array: source.clone(),
                    index: oomir::Operand::Constant(oomir::Constant::I32(index as i32)),
                });
                snapshot(
                    *element,
                    operand_var(dest, element_ty.clone()),
                    offset + index as usize * stride,
                    tcx,
                    definitions,
                    instance,
                    instructions,
                    counter,
                    leaves,
                )?;
            }
        }
        TyKind::Tuple(_) | TyKind::Adt(_, _) => {
            let layout = union_aggregate_layout(ty, tcx, definitions, instance)?
                .ok_or("range encoder aggregate has no field layout")?;
            for field in layout.fields {
                if layout_size_bytes(tcx, field.rust_ty)? == 0 {
                    continue;
                }
                let dest = next_union_temp("range_field", counter);
                instructions.push(oomir::Instruction::GetField {
                    dest: dest.clone(),
                    object: source.clone(),
                    field_name: field.jvm_name,
                    field_ty: field.jvm_ty.clone(),
                    owner_class: layout.class_name.clone(),
                });
                snapshot(
                    field.rust_ty,
                    operand_var(dest, field.jvm_ty),
                    offset + field.offset,
                    tcx,
                    definitions,
                    instance,
                    instructions,
                    counter,
                    leaves,
                )?;
            }
        }
        _ => leaves.push((ty, source, offset)),
    }
    Ok(())
}
