//! Field projections keep the original storage owner. They do not decode the enclosing allocation.
use super::*;

pub(super) fn indirect_field_address<'tcx>(
    place: &Place<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    definitions: &mut Definitions<'tcx>,
    instructions: &mut Vec<Instruction>,
) -> Option<(Operand, oomir::Type)> {
    let deref = place
        .projection
        .iter()
        .rposition(|p| matches!(p, ProjectionElem::Deref))?;
    let fields = &place.projection[deref + 1..];
    if fields.is_empty()
        || fields.len() > 32
        || !fields
            .iter()
            .all(|p| matches!(p, ProjectionElem::Field(..)))
    {
        return None;
    }
    let reference = projection_prefix_place(place, deref, tcx);
    if !matches!(
        get_place_type(&reference, mir, tcx, instance, definitions),
        oomir::Type::Pointer(_)
    ) {
        return None;
    }
    for index in deref + 1..place.projection.len() {
        let parent = projection_prefix_place(place, index, tcx);
        let ty = definitions.normalize(tcx, parent.ty(&mir.local_decls, tcx).ty, instance);
        if !matches!(ty.kind(), TyKind::Tuple(_))
            && !matches!(ty.kind(), TyKind::Adt(def, _) if def.is_struct())
        {
            return None;
        }
    }
    let rust_ty = definitions.normalize(tcx, place.ty(&mir.local_decls, tcx).ty, instance);
    if !super::super::types::is_codegen_sized(rust_ty, tcx) {
        return None;
    }
    let ty = ty_to_oomir_type(rust_ty, tcx, definitions, instance);
    if !ty.has_jvm_value() {
        return None;
    }
    let pointer = super::super::control_flow::rvalue::emit_pointer_to_place(
        place,
        &oomir::Type::pointer(ty.clone()),
        &format!("{}_storage_field", place_to_string(place, tcx)),
        tcx,
        instance,
        mir,
        definitions,
        instructions,
    );
    Some((pointer, ty))
}
