//! Keep private enum discriminants and payloads in separate locals.
use rustc_middle::ty::util::IntTypeExt;
use rustc_middle::{
    mir::{
        visit::{MutVisitor, PlaceContext, Visitor},
        *,
    },
    ty::{self, TyCtxt, TypingEnv},
};

struct Layout<'tcx> {
    tag: Local,
    fields: Vec<Vec<(Local, ty::Ty<'tcx>)>>,
}

pub(super) fn simplify<'tcx>(tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>) -> bool {
    let mut candidates = body
        .local_decls
        .iter_enumerated()
        .map(|(local, decl)| {
            if local.index() <= body.arg_count {
                return None;
            }
            let ty::Adt(def, args) = decl.ty.kind() else {
                return None;
            };
            if !def.is_enum()
                || def.variants().len() > 4
                || decl.ty.needs_drop(tcx, TypingEnv::fully_monomorphized())
            {
                return None;
            }
            let count: usize = def.variants().iter().map(|v| v.fields.len()).sum();
            if count > 8 {
                return None;
            }
            Some((*def, *args))
        })
        .collect::<Vec<_>>();
    for data in body.basic_blocks.as_mut() {
        for statement in &mut data.statements {
            if let StatementKind::Assign(a) = &statement.kind
                && a.0.projection.is_empty()
                && let Some((def, _)) = candidates[a.0.local.index()]
                && let Rvalue::Use(Operand::Constant(constant), _) = &a.1
                && let Some((variant, operands)) =
                    super::inline_records::constant_fields(tcx, constant, true)
            {
                let (_, args) = candidates[a.0.local.index()].unwrap();
                statement.kind = StatementKind::Assign(Box::new((
                    a.0,
                    Rvalue::Aggregate(
                        Box::new(AggregateKind::Adt(def.did(), variant, args, None, None)),
                        operands.into_iter().collect(),
                    ),
                )));
            }
        }
    }
    struct Uses<'a, 'tcx> {
        candidates: &'a mut [Option<(ty::AdtDef<'tcx>, ty::GenericArgsRef<'tcx>)>],
        copies: Vec<(Local, Local)>,
        defined: Vec<bool>,
    }
    impl<'tcx> Visitor<'tcx> for Uses<'_, 'tcx> {
        fn visit_statement(&mut self, statement: &Statement<'tcx>, location: Location) {
            if let StatementKind::Assign(a) = &statement.kind
                && let Some(dest) = a.0.as_local()
            {
                self.defined[dest.index()] = true;
                if let Rvalue::Use(Operand::Copy(source) | Operand::Move(source), _) = a.1
                    && let Some(source) = source.as_local()
                {
                    self.copies.push((source, dest));
                    return;
                }
            }
            match &statement.kind {
                StatementKind::Assign(a)
                    if a.0.projection.is_empty()
                        && matches!(&a.1, Rvalue::Aggregate(_, _) | Rvalue::Discriminant(_)) =>
                {
                    if let Rvalue::Discriminant(p) = &a.1 {
                        if p.projection.is_empty() {
                            return;
                        }
                    }
                    self.visit_rvalue(&a.1, location);
                }
                StatementKind::SetDiscriminant { place, .. } if place.projection.is_empty() => {
                    self.defined[place.local.index()] = true;
                }
                _ => self.super_statement(statement, location),
            }
        }
        fn visit_place(&mut self, place: &Place<'tcx>, context: PlaceContext, location: Location) {
            if !matches!(context, PlaceContext::NonUse(_))
                && (context.is_borrow()
                    || context.is_address_of()
                    || !matches!(
                        place.projection.as_ref(),
                        [ProjectionElem::Downcast(..), ProjectionElem::Field(..), ..]
                    ))
            {
                self.candidates[place.local.index()] = None;
            }
            self.super_place(place, context, location);
        }
    }
    let mut uses = Uses {
        candidates: &mut candidates,
        copies: Vec::new(),
        defined: vec![false; body.local_decls.len()],
    };
    for (bb, data) in body.basic_blocks.iter_enumerated() {
        uses.visit_basic_block_data(bb, data);
    }
    let Uses {
        copies, defined, ..
    } = uses;
    for (candidate, defined) in candidates.iter_mut().zip(defined) {
        if !defined {
            *candidate = None;
        }
    }
    loop {
        let mut changed = false;
        for &(source, dest) in &copies {
            if candidates[dest.index()].is_none() && candidates[source.index()].take().is_some() {
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    if candidates.iter().all(Option::is_none) {
        return false;
    }
    split_copies(tcx, body, &candidates);
    let mut layouts = Vec::new();
    for candidate in &candidates {
        let Some((def, args)) = candidate else {
            layouts.push(None);
            continue;
        };
        let tag = body.local_decls.push(LocalDecl::new(
            def.repr().discr_type().to_ty(tcx),
            body.span,
        ));
        let fields = def
            .variants()
            .iter()
            .map(|v| {
                v.fields
                    .iter()
                    .map(|f| {
                        let ty = f.ty(tcx, args).skip_norm_wip();
                        (body.local_decls.push(LocalDecl::new(ty, body.span)), ty)
                    })
                    .collect()
            })
            .collect();
        layouts.push(Some(Layout { tag, fields }));
    }
    layouts.resize_with(body.local_decls.len(), || None);
    for data in body.basic_blocks_mut() {
        let mut result = Vec::new();
        for mut statement in std::mem::take(&mut data.statements) {
            let source = statement.source_info;
            match &mut statement.kind {
                StatementKind::Assign(a) => {
                    if a.0.projection.is_empty()
                        && let Some(layout) = &layouts[a.0.local.index()]
                    {
                        let Rvalue::Aggregate(kind, operands) = &a.1 else {
                            unreachable!()
                        };
                        let AggregateKind::Adt(_, variant, ..) = **kind else {
                            unreachable!()
                        };
                        let (def, _) = candidates[a.0.local.index()].unwrap();
                        let ty = def.repr().discr_type().to_ty(tcx);
                        let tag = def.discriminant_for_variant(tcx, variant).val;
                        let value = Operand::const_from_scalar(
                            tcx,
                            ty,
                            rustc_middle::mir::interpret::Scalar::from_uint(
                                tag,
                                tcx.layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
                                    .unwrap()
                                    .size,
                            ),
                            source.span,
                        );
                        result.push(Statement::new(
                            source,
                            StatementKind::Assign(Box::new((
                                layout.tag.into(),
                                Rvalue::Use(value, WithRetag::Yes),
                            ))),
                        ));
                        for ((local, _), operand) in
                            layout.fields[variant.index()].iter().zip(operands.iter())
                        {
                            result.push(Statement::new(
                                source,
                                StatementKind::Assign(Box::new((
                                    (*local).into(),
                                    Rvalue::Use(operand.clone(), WithRetag::Yes),
                                ))),
                            ));
                        }
                        continue;
                    }
                    if let Rvalue::Discriminant(place) = a.1
                        && place.projection.is_empty()
                        && let Some(layout) = &layouts[place.local.index()]
                    {
                        a.1 = Rvalue::Use(Operand::Copy(layout.tag.into()), WithRetag::Yes);
                    }
                }
                StatementKind::SetDiscriminant {
                    place,
                    variant_index,
                } if place.projection.is_empty() && layouts[place.local.index()].is_some() => {
                    let (def, _) = candidates[place.local.index()].unwrap();
                    let ty = def.repr().discr_type().to_ty(tcx);
                    let value = Operand::const_from_scalar(
                        tcx,
                        ty,
                        rustc_middle::mir::interpret::Scalar::from_uint(
                            def.discriminant_for_variant(tcx, *variant_index).val,
                            tcx.layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
                                .unwrap()
                                .size,
                        ),
                        source.span,
                    );
                    statement.kind = StatementKind::Assign(Box::new((
                        layouts[place.local.index()].as_ref().unwrap().tag.into(),
                        Rvalue::Use(value, WithRetag::Yes),
                    )));
                }
                _ => {}
            }
            result.push(statement);
        }
        data.statements = result;
    }
    struct Fields<'a, 'tcx> {
        tcx: TyCtxt<'tcx>,
        layouts: &'a [Option<Layout<'tcx>>],
    }
    impl<'tcx> MutVisitor<'tcx> for Fields<'_, 'tcx> {
        fn tcx(&self) -> TyCtxt<'tcx> {
            self.tcx
        }
        fn visit_place(&mut self, place: &mut Place<'tcx>, context: PlaceContext, loc: Location) {
            if let Some(Some(layout)) = self.layouts.get(place.local.index())
                && let [
                    ProjectionElem::Downcast(_, variant),
                    ProjectionElem::Field(field, _),
                    rest @ ..,
                ] = place.projection.as_ref()
            {
                *place = Place::from(layout.fields[variant.index()][field.index()].0)
                    .project_deeper(rest, self.tcx);
            }
            self.super_place(place, context, loc);
        }
    }
    let mut visitor = Fields {
        tcx,
        layouts: &layouts,
    };
    for (bb, data) in body.basic_blocks_mut().iter_enumerated_mut() {
        visitor.visit_basic_block_data(bb, data);
    }
    true
}

fn split_copies<'tcx>(
    tcx: TyCtxt<'tcx>,
    body: &mut Body<'tcx>,
    candidates: &[Option<(ty::AdtDef<'tcx>, ty::GenericArgsRef<'tcx>)>],
) {
    let mut index = 0;
    while index < body.basic_blocks.len() {
        let block = BasicBlock::from_usize(index);
        index += 1;
        let found = body[block]
            .statements
            .iter()
            .enumerate()
            .find_map(|(position, statement)| {
                let StatementKind::Assign(a) = &statement.kind else {
                    return None;
                };
                let Some((def, args)) = candidates.get(a.0.local.index()).copied().flatten() else {
                    return None;
                };
                let Rvalue::Use(Operand::Copy(source) | Operand::Move(source), _) = a.1 else {
                    return None;
                };
                a.0.as_local()
                    .map(|dest| (position, dest, source, def, args, statement.source_info))
            });
        let Some((position, dest, source, def, args, source_info)) = found else {
            continue;
        };
        let is_cleanup = body[block].is_cleanup;
        let statements = body[block].statements.split_off(position + 1);
        body[block].statements.pop();
        let terminator = body[block].terminator.take();
        let continuation = body.basic_blocks_mut().push(BasicBlockData::new_stmts(
            statements, terminator, is_cleanup,
        ));
        let tag = body.local_decls.push(LocalDecl::new(
            def.repr().discr_type().to_ty(tcx),
            source_info.span,
        ));
        body[block].statements.push(Statement::new(
            source_info,
            StatementKind::Assign(Box::new((tag.into(), Rvalue::Discriminant(source)))),
        ));
        let mut targets = Vec::new();
        for (variant, data) in def.variants().iter_enumerated() {
            let mut statements = vec![Statement::new(
                source_info,
                StatementKind::SetDiscriminant {
                    place: Box::new(dest.into()),
                    variant_index: variant,
                },
            )];
            for (field, decl) in data.fields.iter_enumerated() {
                let ty = decl.ty(tcx, args).skip_norm_wip();
                let projections = [
                    ProjectionElem::Downcast(None, variant),
                    ProjectionElem::Field(field, ty),
                ];
                let dest = Place::from(dest).project_deeper(&projections, tcx);
                let source = source.project_deeper(&projections, tcx);
                statements.push(Statement::new(
                    source_info,
                    StatementKind::Assign(Box::new((
                        dest,
                        Rvalue::Use(Operand::Copy(source), WithRetag::Yes),
                    ))),
                ));
            }
            let target = body.basic_blocks_mut().push(BasicBlockData::new_stmts(
                statements,
                Some(Terminator {
                    source_info,
                    kind: TerminatorKind::Goto {
                        target: continuation,
                    },
                    loop_hint_attrs: Default::default(),
                }),
                is_cleanup,
            ));
            targets.push((def.discriminant_for_variant(tcx, variant).val, target));
        }
        let invalid = body.basic_blocks_mut().push(BasicBlockData::new(
            Some(Terminator {
                source_info,
                kind: TerminatorKind::Unreachable,
                loop_hint_attrs: Default::default(),
            }),
            is_cleanup,
        ));
        body[block].terminator = Some(Terminator {
            source_info,
            loop_hint_attrs: Default::default(),
            kind: TerminatorKind::SwitchInt {
                discr: Operand::Copy(tag.into()),
                targets: SwitchTargets::new(targets.into_iter(), invalid),
            },
        });
    }
}
