//! Expose scalar niche casts before local enum decomposition.
use rustc_abi::{TagEncoding, Variants};
use rustc_middle::{
    mir::*,
    ty::{self, TyCtxt, TypingEnv},
};

pub(super) fn expand<'tcx>(tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>) {
    let env = TypingEnv::fully_monomorphized();
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
                let Rvalue::Cast(CastKind::Transmute, operand, ty) = &a.1 else {
                    return None;
                };
                if !operand.ty(&body.local_decls, tcx).is_integral() {
                    return None;
                }
                let ty::Adt(def, args) = ty.kind() else {
                    return None;
                };
                if !def.is_enum() || def.variants().len() != 2 || ty.needs_drop(tcx, env) {
                    return None;
                }
                let layout = tcx.layout_of(env.as_query_input(*ty)).ok()?;
                let Variants::Multiple {
                    tag,
                    tag_encoding:
                        TagEncoding::Niche {
                            untagged_variant,
                            niche_variants,
                            niche_start,
                        },
                    tag_field,
                    ..
                } = &layout.variants
                else {
                    return None;
                };
                if tag.size(&tcx.data_layout) != layout.size
                    || niche_variants.start != niche_variants.last
                    || layout.fields.offset((*tag_field).into()).bytes() != 0
                    || !def.variant(niche_variants.start).fields.is_empty()
                {
                    return None;
                }
                let fields = &def.variant(*untagged_variant).fields;
                if fields.len() != 1 {
                    return None;
                }
                let field = fields[rustc_abi::FieldIdx::ZERO]
                    .ty(tcx, args)
                    .skip_norm_wip();
                if tcx.layout_of(env.as_query_input(field)).ok()?.size != layout.size {
                    return None;
                }
                Some((
                    position,
                    a.0,
                    operand.clone(),
                    *def,
                    *args,
                    *untagged_variant,
                    niche_variants.start,
                    *niche_start,
                    field,
                    statement.source_info,
                ))
            });
        let Some((position, dest, operand, def, args, full, empty, niche, field, source_info)) =
            found
        else {
            continue;
        };
        let cleanup = body[block].is_cleanup;
        let rest = body[block].statements.split_off(position + 1);
        body[block].statements.pop();
        let terminator = body[block].terminator.take();
        let done = body
            .basic_blocks_mut()
            .push(BasicBlockData::new_stmts(rest, terminator, cleanup));
        let payload = body
            .local_decls
            .push(LocalDecl::new(field, source_info.span));
        let mut targets = Vec::new();
        for variant in [empty, full] {
            let mut statements = Vec::new();
            let values = if variant == full {
                statements.push(Statement::new(
                    source_info,
                    StatementKind::Assign(Box::new((
                        payload.into(),
                        Rvalue::Cast(CastKind::Transmute, operand.clone(), field),
                    ))),
                ));
                vec![Operand::Move(payload.into())]
            } else {
                Vec::new()
            };
            statements.push(Statement::new(
                source_info,
                StatementKind::Assign(Box::new((
                    dest,
                    Rvalue::Aggregate(
                        Box::new(AggregateKind::Adt(def.did(), variant, args, None, None)),
                        values.into_iter().collect(),
                    ),
                ))),
            ));
            targets.push(body.basic_blocks_mut().push(BasicBlockData::new_stmts(
                statements,
                Some(Terminator {
                    source_info,
                    kind: TerminatorKind::Goto { target: done },
                    loop_hint_attrs: Default::default(),
                }),
                cleanup,
            )));
        }
        body[block].terminator = Some(Terminator {
            source_info,
            kind: TerminatorKind::SwitchInt {
                discr: operand,
                targets: SwitchTargets::new(std::iter::once((niche, targets[0])), targets[1]),
            },
            loop_hint_attrs: Default::default(),
        });
    }
}
