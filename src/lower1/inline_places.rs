//! Substitute local places for private reference temporaries.
use rustc_middle::{
    mir::{
        visit::{MutVisitor, PlaceContext, Visitor},
        *,
    },
    ty::{self, TyCtxt},
};

pub(super) fn simplify<'tcx>(tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>) {
    let mut writes = Writes(vec![0; body.local_decls.len()]);
    for local in body.args_iter() {
        writes.0[local.index()] = 2;
    }
    writes.visit_body(body);
    let mut places = vec![None; body.local_decls.len()];
    loop {
        let mut assigned = vec![0; places.len()];
        let mut candidates = vec![None; places.len()];
        let mut conflicts = vec![false; places.len()];
        for block in body.basic_blocks.iter() {
            for statement in &block.statements {
                let StatementKind::Assign(assignment) = &statement.kind else {
                    continue;
                };
                let Some(local) = assignment.0.as_local() else {
                    continue;
                };
                let index = local.index();
                if writes.0[index] == 0 || places[index].is_some() {
                    continue;
                }
                assigned[index] += 1;
                let place = match &assignment.1 {
                    Rvalue::Ref(_, _, place) | Rvalue::RawPtr(_, place) => {
                        Some(expand(tcx, *place, &places))
                    }
                    Rvalue::Use(Operand::Copy(place) | Operand::Move(place), _)
                        if place.projection.is_empty() =>
                    {
                        places[place.local.index()]
                    }
                    _ => None,
                };
                if let Some(place) = place {
                    if candidates[index].is_some_and(|previous| previous != place) {
                        conflicts[index] = true;
                    }
                    candidates[index] = Some(place);
                } else {
                    conflicts[index] = true;
                }
            }
        }
        let mut changed = false;
        for (index, candidate) in candidates.into_iter().enumerate() {
            if conflicts[index] || assigned[index] != writes.0[index] {
                continue;
            }
            let Some(place) = candidate else {
                continue;
            };
            let borrowed = matches!(
                body.local_decls[Local::from_usize(index)].ty.kind(),
                ty::Ref(..)
            );
            let stable = place.iter_projections().all(|(base, projection)| {
                let ty = base.ty(&body.local_decls, tcx).ty;
                match projection {
                    // Union fields share storage and must retain their address.
                    ProjectionElem::Field(..) => match ty.kind() {
                        ty::Adt(def, _) => !def.is_union(),
                        ty::Tuple(_) | ty::Closure(..) => true,
                        _ => false,
                    },
                    // All payload accesses must refer to the same variant storage.
                    ProjectionElem::Downcast(..) if borrowed => matches!(ty.kind(),
                        ty::Adt(def, _) if def.variants().iter().filter(|v| !v.fields.is_empty()).count() == 1),
                    _ => false,
                }
            });
            if place.local.index() == index || !stable {
                continue;
            }
            places[index] = Some(place);
            changed = true;
        }
        if !changed {
            break;
        }
    }
    let mut visitor = Replace {
        tcx,
        places: &places,
    };
    for (bb, data) in body.basic_blocks_mut().iter_enumerated_mut() {
        visitor.visit_basic_block_data(bb, data);
    }
    loop {
        let mut reads = Reads(vec![false; body.local_decls.len()]);
        for (bb, data) in body.basic_blocks.iter_enumerated() {
            reads.visit_basic_block_data(bb, data);
        }
        let mut changed = false;
        for data in body.basic_blocks_mut() {
            for statement in &mut data.statements {
                if let StatementKind::Assign(assignment) = &statement.kind
                    && let Some(local) = assignment.0.as_local()
                    && !reads.0[local.index()]
                    && assignment.1.is_safe_to_remove()
                {
                    statement.kind = StatementKind::Nop;
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
}

fn expand<'tcx>(
    tcx: TyCtxt<'tcx>,
    place: Place<'tcx>,
    places: &[Option<Place<'tcx>>],
) -> Place<'tcx> {
    if place.projection.first() == Some(&ProjectionElem::Deref)
        && let Some(base) = places[place.local.index()]
    {
        return base.project_deeper(&place.projection[1..], tcx);
    }
    place
}

struct Writes(Vec<u32>);
impl<'tcx> Visitor<'tcx> for Writes {
    fn visit_place(&mut self, place: &Place<'tcx>, context: PlaceContext, location: Location) {
        if place.projection.first() != Some(&ProjectionElem::Deref)
            && (context.is_place_assignment() || context.is_borrow() || context.is_address_of())
        {
            self.0[place.local.index()] += 1;
        }
        self.super_place(place, context, location);
    }
}
struct Replace<'a, 'tcx> {
    tcx: TyCtxt<'tcx>,
    places: &'a [Option<Place<'tcx>>],
}
impl<'tcx> MutVisitor<'tcx> for Replace<'_, 'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }
    fn visit_place(&mut self, place: &mut Place<'tcx>, context: PlaceContext, location: Location) {
        *place = expand(self.tcx, *place, self.places);
        self.super_place(place, context, location);
    }
}
struct Reads(Vec<bool>);
impl<'tcx> Visitor<'tcx> for Reads {
    fn visit_place(&mut self, place: &Place<'tcx>, context: PlaceContext, location: Location) {
        if place.projection.is_empty() && context.is_place_assignment() {
            return;
        }
        self.super_place(place, context, location);
    }
    fn visit_local(&mut self, local: Local, context: PlaceContext, _: Location) {
        if context.is_use() {
            self.0[local.index()] = true;
        }
    }
}
