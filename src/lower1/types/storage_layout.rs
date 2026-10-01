//! Compact typed field paths for borrowed scalar projections.
use super::*;
use crate::lower1::context::Definitions;

pub(crate) fn scalar_storage_layout<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    definitions: &mut Definitions<'tcx>,
    instance: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Operand {
    let ty = definitions.normalize(tcx, ty, instance);
    let descriptor = if let Some(cached) = definitions.storage_layout(ty) {
        cached
    } else {
        let mut fields = Vec::new();
        collect(
            ty,
            0,
            &mut Vec::new(),
            &mut 64,
            &mut fields,
            tcx,
            definitions,
            instance,
        );
        let result = (!fields.is_empty()).then(|| fields.join("\n"));
        definitions.remember_storage_layout(ty, result.clone());
        result
    };
    oomir::Operand::Constant(match descriptor {
        Some(descriptor) => oomir::Constant::String(descriptor),
        None => oomir::Constant::Null(oomir::Type::java_string()),
    })
}

fn collect<'tcx>(
    ty: Ty<'tcx>,
    offset: usize,
    path: &mut Vec<String>,
    budget: &mut usize,
    fields: &mut Vec<String>,
    tcx: TyCtxt<'tcx>,
    definitions: &mut Definitions<'tcx>,
    instance: rustc_middle::ty::Instance<'tcx>,
) {
    if *budget == 0 {
        return;
    }
    *budget -= 1;
    let Ok(ty) = resolve_union_ty(tcx, ty, instance) else {
        return;
    };
    if let Some(payload) = transparent_payload(ty, tcx) {
        collect(
            payload.ty,
            offset,
            path,
            budget,
            fields,
            tcx,
            definitions,
            instance,
        );
        return;
    }
    if !is_codegen_sized(ty, tcx) {
        return;
    }
    if !path.is_empty()
        && matches!(ty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..))
        && let Some(shape) = ty_to_oomir_type(ty, tcx, definitions, instance).component_shape()
        && let Ok(size) = layout_size_bytes(tcx, ty)
        && matches!(size, 8 | 16)
    {
        let codec = match pointer_view_codec_operand(ty, tcx, definitions, instance) {
            oomir::Operand::Constant(oomir::Constant::String(codec)) => codec,
            _ => return,
        };
        let kind = if shape == jvm_compiler_core::ir::ComponentShape::View {
            'v'
        } else {
            'p'
        };
        fields.push(format!(
            "{offset},{size},{},{kind}:{}\n{codec}",
            path.join("/"),
            codec.split('\n').count()
        ));
        return;
    }
    if let TyKind::Array(element, length) = ty.kind()
        && matches!(
            element.kind(),
            TyKind::Bool | TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_)
        )
        && let Some(length) = length.try_to_target_usize(tcx)
        && length > 0
        && length <= i32::MAX as u64
        && let Ok(size) = layout_size_bytes(tcx, *element)
        && matches!(size, 1 | 2 | 4 | 8)
        && length
            .checked_mul(size as u64)
            .is_some_and(|n| n <= i32::MAX as u64)
    {
        // Each access resolves the array field from its current owner. Whole-array replacement can
        // change that field.
        fields.push(format!("{offset},{size},{},{length}", path.join("/")));
        return;
    }
    let scalar = matches!(
        ty.kind(),
        TyKind::Bool | TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_)
    ) || value_scalar_ty(ty, tcx).is_some();
    if scalar {
        if path.is_empty() {
            return;
        }
        let Ok(size) = layout_size_bytes(tcx, ty) else {
            return;
        };
        if matches!(size, 1 | 2 | 4 | 8) {
            fields.push(format!("{offset},{size},{}", path.join("/")));
        }
        return;
    }
    // JVM field order does not describe enum payloads, pointer contents, or aggregate arrays.
    if !matches!(ty.kind(), TyKind::Tuple(_))
        && !matches!(ty.kind(), TyKind::Adt(def, _) if def.is_struct())
    {
        return;
    }
    let Ok(Some(layout)) = union_aggregate_layout(ty, tcx, definitions, instance) else {
        return;
    };
    for field in layout.fields {
        path.push(field.jvm_name);
        collect(
            field.rust_ty,
            offset + field.offset,
            path,
            budget,
            fields,
            tcx,
            definitions,
            instance,
        );
        path.pop();
    }
}
