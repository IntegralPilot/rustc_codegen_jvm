//! Array locations retain their enclosing owner through field replacement.
use super::*;

pub(super) fn array_place_storage<'tcx>(
    place: &Place<'tcx>,
    prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    definitions: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let rust_ty = normalize_unsize_ty(place.ty(&mir.local_decls, tcx).ty, tcx, instance);
    if matches!(rust_ty.kind(), TyKind::Array(..))
        && (!place.projection.is_empty() || definitions.local_uses_stable_cell(place.local))
        && let Some(view) = emit_borrowed_projected_array_view(
            place,
            rust_ty,
            &format!("{prefix}_array_storage"),
            tcx,
            instance,
            mir,
            definitions,
            instructions,
        )
    {
        return view;
    }
    let direct = place
        .projection
        .split_last()
        .and_then(|(projection, prefix)| {
            if !matches!(projection, ProjectionElem::Deref) {
                return None;
            }
            let reference = Place {
                local: place.local,
                projection: tcx.mk_place_elems(prefix),
            };
            matches!(
                get_place_type(&reference, mir, tcx, instance, definitions),
                oomir::Type::Slice(_)
            )
            .then(|| emit_instructions_to_get_on_own(&reference, tcx, instance, mir, definitions))
        });
    let (name, code, ty) = direct
        .unwrap_or_else(|| emit_instructions_to_get_on_own(place, tcx, instance, mir, definitions));
    instructions.extend(code);
    oomir::Operand::Variable { name, ty }
}

pub(super) fn emit_array_pointer(
    array_or_slice: oomir::Operand,
    index: oomir::Operand,
    element_size: oomir::Operand,
    codec: oomir::Operand,
    pointer_ty: &oomir::Type,
    dest: &str,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let source_ty = array_or_slice
        .get_type()
        .expect("array pointer source must be typed");
    if matches!(source_ty, oomir::Type::Slice(_)) {
        let index = if index.get_type() == Some(oomir::Type::U64) {
            index
        } else {
            let index_u64 = format!("{dest}_slice_index_u64");
            instructions.push(oomir::Instruction::Cast {
                dest: index_u64.clone(),
                op: index,
                ty: oomir::Type::U64,
            });
            oomir::Operand::Variable {
                name: index_u64,
                ty: oomir::Type::U64,
            }
        };
        let base_dest = format!("{dest}_slice_base");
        instructions.push(oomir::Instruction::ViewAddress {
            dest: Some(base_dest.clone()),
            source: array_or_slice,
            layout: Box::new(oomir::AddressLayout {
                pointer_type: pointer_ty.clone(),
                size: element_size,
                codec: codec,
            }),
        });
        instructions.push(oomir::Instruction::AddressOffset {
            dest: Some(dest.to_string()),
            source: oomir::Operand::Variable {
                name: base_dest,
                ty: pointer_ty.clone(),
            },
            count: index,
            ty: pointer_ty.clone(),
            bytes: false,
            wrapping: false,
            subtract: false,
        });
        return oomir::Operand::Variable {
            name: dest.to_string(),
            ty: pointer_ty.clone(),
        };
    }
    emit_pointer_factory(
        "array",
        vec![array_or_slice, index, element_size, codec],
        pointer_ty,
        dest,
        instructions,
    )
}
