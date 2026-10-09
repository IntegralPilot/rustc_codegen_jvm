//! Split private records before reference lowering.
use rustc_middle::{
    mir::{
        visit::{MutVisitor, PlaceContext, Visitor},
        *,
    },
    ty::{self, TyCtxt, TypingEnv},
};

pub(super) fn simplify<'tcx>(tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>) -> bool {
    let mut fields = body
        .local_decls
        .iter_enumerated()
        .map(|(local, decl)| {
            if local.index() <= body.arg_count
                || decl.ty.needs_drop(tcx, TypingEnv::fully_monomorphized())
            {
                return None;
            }
            let fields: Vec<_> = match decl.ty.kind() {
                ty::Tuple(fields) => fields.to_vec(),
                ty::Adt(def, args)
                    if def.is_struct()
                        && !tcx.is_lang_item(
                            def.did(),
                            rustc_attr_ir::lang_items::LangItem::UnsafeCell,
                        ) =>
                {
                    def.non_enum_variant()
                        .fields
                        .iter()
                        .map(|f| f.ty(tcx, args).skip_norm_wip())
                        .collect()
                }
                ty::Closure(_, args) => args.as_closure().upvar_tys().iter().collect(),
                _ => return None,
            };
            (fields.len() <= 8).then_some(fields)
        })
        .collect::<Vec<_>>();
    for data in body.basic_blocks.as_mut() {
        for statement in &mut data.statements {
            let StatementKind::Assign(a) = &mut statement.kind else {
                continue;
            };
            let Some(local) = a.0.as_local() else {
                continue;
            };
            if fields[local.index()].is_none() {
                continue;
            }
            let Rvalue::Use(Operand::Constant(constant), _) = &a.1 else {
                continue;
            };
            let ty = body.local_decls[local].ty;
            let Some((_, operands)) = constant_fields(tcx, constant, false) else {
                continue;
            };
            let kind = match ty.kind() {
                ty::Tuple(_) => AggregateKind::Tuple,
                ty::Adt(def, args) => {
                    AggregateKind::Adt(def.did(), rustc_abi::VariantIdx::ZERO, args, None, None)
                }
                ty::Closure(def, args) => AggregateKind::Closure(*def, args),
                _ => continue,
            };
            a.1 = Rvalue::Aggregate(Box::new(kind), operands.into_iter().collect());
        }
    }
    for index in 0..body.basic_blocks.len() {
        let bb = BasicBlock::from_usize(index);
        let mut output = Vec::new();
        for mut statement in std::mem::take(&mut body[bb].statements) {
            if let StatementKind::Assign(a) = &mut statement.kind
                && let Some(local) = a.0.as_local()
                && let Some(record) = &fields[local.index()]
                && record.len() == 1
                && let Rvalue::Cast(CastKind::Transmute, value, ty) = &a.1
                && value.ty(&body.local_decls, tcx).is_integral()
                && let ty::Adt(def, args) = ty.kind()
                && let Ok(layout) =
                    tcx.layout_of(TypingEnv::fully_monomorphized().as_query_input(*ty))
                && let Ok(field_layout) =
                    tcx.layout_of(TypingEnv::fully_monomorphized().as_query_input(record[0]))
                && layout.size == field_layout.size
                && layout.fields.offset(0).bytes() == 0
            {
                let field = body
                    .local_decls
                    .push(LocalDecl::new(record[0], statement.source_info.span));
                output.push(Statement::new(
                    statement.source_info,
                    StatementKind::Assign(Box::new((
                        field.into(),
                        Rvalue::Cast(CastKind::Transmute, value.clone(), record[0]),
                    ))),
                ));
                a.1 = Rvalue::Aggregate(
                    Box::new(AggregateKind::Adt(
                        def.did(),
                        rustc_abi::VariantIdx::ZERO,
                        args,
                        None,
                        None,
                    )),
                    std::iter::once(Operand::Move(field.into())).collect(),
                );
            }
            output.push(statement);
        }
        body[bb].statements = output;
    }
    fields.resize(body.local_decls.len(), None);
    struct Uses<'a, 'tcx> {
        fields: &'a mut [Option<Vec<ty::Ty<'tcx>>>],
        copies: Vec<(Local, Local)>,
        defined: Vec<bool>,
    }
    impl<'tcx> Visitor<'tcx> for Uses<'_, 'tcx> {
        fn visit_statement(&mut self, s: &Statement<'tcx>, loc: Location) {
            if let StatementKind::Assign(a) = &s.kind
                && let Some(dest) = a.0.as_local()
            {
                if matches!(a.1, Rvalue::Aggregate(_, _)) {
                    self.defined[dest.index()] = true;
                    self.visit_rvalue(&a.1, loc);
                    return;
                }
                if let Rvalue::Use(Operand::Copy(source) | Operand::Move(source), _) = a.1
                    && let Some(source) = source.as_local()
                {
                    self.defined[dest.index()] = true;
                    self.copies.push((source, dest));
                    return;
                }
            }
            self.super_statement(s, loc);
        }
        fn visit_place(&mut self, p: &Place<'tcx>, context: PlaceContext, loc: Location) {
            if context.is_use()
                && (context.is_borrow()
                    || context.is_address_of()
                    || !matches!(p.projection.first(), Some(ProjectionElem::Field(..))))
            {
                self.fields[p.local.index()] = None;
            }
            self.super_place(p, context, loc);
        }
    }
    let mut uses = Uses {
        fields: &mut fields,
        copies: Vec::new(),
        defined: vec![false; body.local_decls.len()],
    };
    for (bb, data) in body.basic_blocks.iter_enumerated() {
        uses.visit_basic_block_data(bb, data);
    }
    let Uses {
        copies, defined, ..
    } = uses;
    for (field, defined) in fields.iter_mut().zip(defined) {
        if !defined {
            *field = None;
        }
    }
    loop {
        let mut changed = false;
        for &(source, dest) in &copies {
            if fields[dest.index()].is_none() && fields[source.index()].take().is_some() {
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    if fields.iter().all(Option::is_none) {
        return false;
    }
    let layouts: Vec<_> = fields
        .iter()
        .map(|f| {
            f.as_ref().map(|f| {
                f.iter()
                    .map(|&ty| body.local_decls.push(LocalDecl::new(ty, body.span)))
                    .collect::<Vec<_>>()
            })
        })
        .collect();
    for data in body.basic_blocks_mut() {
        let mut output = Vec::new();
        for statement in std::mem::take(&mut data.statements) {
            if let StatementKind::Assign(a) = &statement.kind
                && let Some(dest) = a.0.as_local()
                && let Some(locals) = &layouts[dest.index()]
            {
                let operands: Vec<_> = match &a.1 {
                    Rvalue::Aggregate(_, operands) => operands.iter().cloned().collect(),
                    Rvalue::Use(Operand::Copy(source) | Operand::Move(source), _) => fields
                        [dest.index()]
                    .as_ref()
                    .unwrap()
                    .iter()
                    .enumerate()
                    .map(|(i, &ty)| {
                        Operand::Copy(tcx.mk_place_field(
                            *source,
                            rustc_abi::FieldIdx::from_usize(i),
                            ty,
                        ))
                    })
                    .collect(),
                    _ => unreachable!(),
                };
                assert_eq!(locals.len(), operands.len());
                for (&local, operand) in locals.iter().zip(operands) {
                    output.push(Statement::new(
                        statement.source_info,
                        StatementKind::Assign(Box::new((
                            local.into(),
                            Rvalue::Use(operand, WithRetag::Yes),
                        ))),
                    ));
                }
            } else {
                output.push(statement);
            }
        }
        data.statements = output;
    }
    struct Fields<'a, 'tcx> {
        tcx: TyCtxt<'tcx>,
        layouts: &'a [Option<Vec<Local>>],
    }
    impl<'tcx> MutVisitor<'tcx> for Fields<'_, 'tcx> {
        fn tcx(&self) -> TyCtxt<'tcx> {
            self.tcx
        }
        fn visit_place(&mut self, place: &mut Place<'tcx>, context: PlaceContext, loc: Location) {
            if let Some(Some(fields)) = self.layouts.get(place.local.index())
                && let [ProjectionElem::Field(field, _), rest @ ..] = place.projection.as_ref()
            {
                *place = Place::from(fields[field.index()]).project_deeper(rest, self.tcx);
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

pub(super) fn constant_fields<'tcx>(
    tcx: TyCtxt<'tcx>,
    constant: &ConstOperand<'tcx>,
    is_enum: bool,
) -> Option<(rustc_abi::VariantIdx, Vec<Operand<'tcx>>)> {
    let env = TypingEnv::fully_monomorphized();
    let value = constant.const_.eval(tcx, env, constant.span).ok()?;
    let (ecx, operand) = rustc_const_eval::const_eval::mk_eval_cx_for_const_val(
        tcx.at(constant.span),
        env,
        value,
        constant.const_.ty(),
    )?;
    let variant = if is_enum {
        ecx.read_discriminant(&operand).discard_err()?
    } else {
        rustc_abi::VariantIdx::ZERO
    };
    let operand = if is_enum {
        ecx.project_downcast(&operand, variant).discard_err()?
    } else {
        operand
    };
    let fields = (0..operand.layout.fields.count())
        .map(|index| {
            let field = ecx
                .project_field(&operand, rustc_abi::FieldIdx::from_usize(index))
                .discard_err()?;
            if field.layout.is_zst() {
                return Some(Operand::zero_sized_constant(field.layout.ty, constant.span));
            }
            if !matches!(
                field.layout.ty.kind(),
                ty::Bool | ty::Char | ty::Int(_) | ty::Uint(_) | ty::Float(_)
            ) {
                return None;
            }
            let immediate = ecx.read_immediate(&field).discard_err()?;
            let rustc_const_eval::interpret::Immediate::Scalar(value) = *immediate else {
                return None;
            };
            Some(Operand::const_from_scalar(
                tcx,
                field.layout.ty,
                value,
                constant.span,
            ))
        })
        .collect::<Option<Vec<_>>>()?;
    Some((variant, fields))
}
