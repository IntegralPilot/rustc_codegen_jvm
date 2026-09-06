use super::*;

pub(super) fn rust_layout_size_operand<'tcx>(
    ty: rustc_middle::ty::Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> oomir::Operand {
    let ty = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let size = crate::lower1::types::layout_size_bytes(tcx, ty).unwrap_or_else(|error| {
        panic!("could not determine pointer layout size for {ty:?}: {error}")
    });
    oomir::Operand::Constant(oomir::Constant::U64(
        u64::try_from(size).expect("Rust layout size exceeds u64"),
    ))
}

pub(super) fn rust_layout_alignment<'tcx>(
    ty: rustc_middle::ty::Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> usize {
    let ty = EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    crate::lower1::types::layout_align_bytes(tcx, ty).unwrap_or_else(|error| {
        panic!("could not determine pointer layout alignment for {ty:?}: {error}")
    })
}

pub(super) fn pointer_view_size_operand<'tcx>(
    pointer_ty: rustc_middle::ty::Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
) -> oomir::Operand {
    let pointee = match pointer_ty.kind() {
        TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
        other => panic!("expected pointer/reference type, found {other:?}"),
    };
    let pointee = EarlyBinder::bind(tcx, pointee)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let size = crate::lower1::types::layout_size_bytes(tcx, pointee).unwrap_or_else(|error| {
        panic!("could not determine pointer view size for {pointee:?}: {error}")
    });
    oomir::Operand::Constant(oomir::Constant::U64(
        u64::try_from(size).expect("Rust pointer view layout exceeds u64"),
    ))
}

pub(super) fn pointer_pointee_ty<'tcx>(
    pointer_ty: rustc_middle::ty::Ty<'tcx>,
) -> rustc_middle::ty::Ty<'tcx> {
    match pointer_ty.kind() {
        TyKind::Ref(_, pointee, _) | TyKind::RawPtr(pointee, _) => *pointee,
        other => panic!("expected pointer/reference type, found {other:?}"),
    }
}
