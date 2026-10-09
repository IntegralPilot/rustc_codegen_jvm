//! Expose small monomorphized operations before storage lowering.
use rustc_abi::ExternAbi;
use rustc_attr_ir::InlineAttr;
use rustc_middle::{
    mir::{self, visit::MutVisitor, *},
    ty::{self, EarlyBinder, Instance, InstanceKind, ShimKind, TyCtxt, TypingEnv},
};

pub(super) fn expand<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    original: &Body<'tcx>,
) -> Option<Body<'tcx>> {
    if tcx.sess.opts.optimize == rustc_session::config::OptLevel::No
        || matches!(
            tcx.codegen_fn_attrs(instance.def_id()).optimize,
            rustc_attr_ir::OptimizeAttr::DoNotOptimize
        )
        || !original.basic_blocks.iter().any(|block| {
            matches!(
                block.terminator().kind,
                TerminatorKind::Call {
                    target: Some(_),
                    ..
                }
            )
        })
        || original.coroutine.is_some()
        || crate::lower2::debug_info_options(tcx).local_variables
    {
        return None;
    }
    let mut body = instance
        .try_instantiate_mir_and_normalize_erasing_regions(
            tcx,
            TypingEnv::fully_monomorphized(),
            EarlyBinder::bind(tcx, original.clone()),
        )
        .ok()?;
    let mut history = vec![instance];
    let mut budget = 2400usize;
    let end = body.basic_blocks.len();
    let changed = expand_blocks(tcx, &mut body, 0..end, &mut history, &mut budget);
    if changed {
        super::inline_niches::expand(tcx, &mut body);
        for block in body.basic_blocks.as_mut() {
            if let TerminatorKind::Drop { place, target, .. } = block.terminator().kind
                && !place
                    .ty(&body.local_decls, tcx)
                    .ty
                    .needs_drop(tcx, TypingEnv::fully_monomorphized())
            {
                block.terminator_mut().kind = TerminatorKind::Goto { target };
            }
        }
        super::inline_places::simplify(tcx, &mut body);
        for _ in 0..8 {
            let records = super::inline_records::simplify(tcx, &mut body);
            let enums = super::inline_enums::simplify(tcx, &mut body);
            if !records && !enums {
                break;
            }
            super::inline_places::simplify(tcx, &mut body);
        }
        super::inline_places::simplify(tcx, &mut body);
    }
    changed.then_some(body)
}

fn expand_blocks<'tcx>(
    tcx: TyCtxt<'tcx>,
    body: &mut Body<'tcx>,
    blocks: std::ops::Range<usize>,
    history: &mut Vec<Instance<'tcx>>,
    budget: &mut usize,
) -> bool {
    let mut changed = false;
    for index in blocks {
        if history.len() > 20 || *budget == 0 {
            break;
        }
        let block = BasicBlock::from_usize(index);
        let TerminatorKind::Call {
            func,
            args,
            destination,
            target: Some(target),
            unwind,
            ..
        } = &body[block].terminator().kind
        else {
            continue;
        };
        let Some(destination) = destination.as_local() else {
            continue;
        };
        let ty::FnDef(def_id, generic_args) = *func.ty(&body.local_decls, tcx).kind() else {
            continue;
        };
        let Some(generic_args) = generic_args.no_bound_vars() else {
            continue;
        };
        let Some(instance) =
            Instance::try_resolve(tcx, TypingEnv::fully_monomorphized(), def_id, generic_args)
                .ok()
                .flatten()
        else {
            continue;
        };
        if matches!(
            tcx.def_kind(instance.def_id()),
            rustc_hir::def::DefKind::Ctor(..)
        ) && let ty::Adt(def, args_ty) = *body.local_decls[destination].ty.kind()
            && let Some((variant, _)) = def
                .variants()
                .iter_enumerated()
                .find(|(_, v)| v.ctor_def_id() == Some(instance.def_id()))
        {
            let source = body[block].terminator().source_info;
            let assignment = Rvalue::Aggregate(
                Box::new(AggregateKind::Adt(def.did(), variant, args_ty, None, None)),
                args.iter().map(|arg| arg.node.clone()).collect(),
            );
            let target = *target;
            body[block].statements.push(Statement::new(
                source,
                StatementKind::Assign(Box::new((destination.into(), assignment))),
            ));
            body[block].terminator_mut().kind = TerminatorKind::Goto { target };
            changed = true;
            continue;
        }
        if !matches!(
            instance.def,
            InstanceKind::Item(_)
                | InstanceKind::Shim(ShimKind::ClosureOnce { .. } | ShimKind::FnPtr(..))
        ) || history.contains(&instance)
            || (matches!(instance.def, InstanceKind::Item(_))
                && !tcx.is_mir_available(instance.def_id()))
        {
            continue;
        }
        if matches!(
            tcx.constness(instance.def_id()),
            rustc_hir::Constness::Const { always: true }
        ) {
            continue;
        }
        let attrs = tcx.codegen_fn_attrs(instance.def_id());
        if matches!(attrs.inline, InlineAttr::Never)
            || matches!(attrs.optimize, rustc_attr_ir::OptimizeAttr::DoNotOptimize)
            || !attrs.target_features.is_empty()
        {
            continue;
        }
        let sig = tcx
            .fn_sig(def_id)
            .instantiate(tcx, generic_args)
            .skip_norm_wip();
        if !matches!(sig.abi(), ExternAbi::Rust | ExternAbi::RustCall) {
            continue;
        }
        let original = tcx.instance_mir(instance.def);
        if original.coroutine.is_some() {
            continue;
        }
        let cost: usize = original
            .basic_blocks
            .iter()
            .map(|b| b.statements.len() + 5)
            .sum();
        let scalar_state = tcx.is_closure_like(instance.def_id())
            || sig.inputs_and_output().iter().any(|ty| {
                let ty = tcx.instantiate_bound_regions_with_erased(ty);
                let candidate = match ty.kind() {
                    ty::Tuple(fields) => !fields.is_empty() && fields.len() <= 8,
                    ty::Closure(..) => true,
                    ty::Adt(def, _) => {
                        !def.is_union()
                            && def.variants().len() <= 4
                            && def.variants().iter().all(|v| v.fields.len() <= 8)
                    }
                    _ => false,
                };
                candidate && !ty.needs_drop(tcx, TypingEnv::fully_monomorphized())
            });
        if cost > if scalar_state { 300 } else { 80 }
            || (matches!(attrs.inline, InlineAttr::None)
                && !tcx.is_closure_like(instance.def_id())
                && cost > 150)
            || cost > *budget
            || original.basic_blocks.iter().any(|b| {
                matches!(
                    b.terminator().kind,
                    TerminatorKind::TailCall { .. }
                        | TerminatorKind::InlineAsm { .. }
                        | TerminatorKind::Yield { .. }
                        | TerminatorKind::CoroutineDrop
                )
            })
        {
            continue;
        }
        let Ok(mut callee) = instance.try_instantiate_mir_and_normalize_erasing_regions(
            tcx,
            TypingEnv::fully_monomorphized(),
            EarlyBinder::bind(tcx, original.clone()),
        ) else {
            continue;
        };
        if callee.return_ty() != body.local_decls[destination].ty {
            continue;
        }
        let mut arguments: Vec<_> = args.iter().map(|a| a.node.clone()).collect();
        if sig.abi() == ExternAbi::RustCall && callee.spread_arg.is_none() {
            let Some(packed) = arguments.pop() else {
                continue;
            };
            let ty::Tuple(fields) = packed.ty(&body.local_decls, tcx).kind() else {
                continue;
            };
            let unpacked = if let Some(tuple) = packed.place() {
                fields
                    .iter()
                    .enumerate()
                    .map(|(i, ty)| {
                        Operand::Move(tcx.mk_place_field(
                            tuple,
                            rustc_abi::FieldIdx::from_usize(i),
                            ty,
                        ))
                    })
                    .collect::<Vec<_>>()
            } else if fields.is_empty() {
                Vec::new()
            } else if let Operand::Constant(constant) = packed
                && let Some((_, operands)) =
                    super::inline_records::constant_fields(tcx, &constant, false)
            {
                operands
            } else {
                continue;
            };
            arguments.extend(unpacked);
        }
        if arguments.len() != callee.arg_count
            || arguments
                .iter()
                .zip(callee.args_iter())
                .any(|(a, l)| a.ty(&body.local_decls, tcx) != callee.local_decls[l].ty)
        {
            continue;
        }
        let source_info = body[block].terminator().source_info;
        let target = *target;
        let unwind = *unwind;
        let local_start = body.local_decls.len();
        let block_start = body.basic_blocks.len();
        let mut locals = vec![destination];
        for decl in callee.local_decls.iter().skip(1) {
            locals.push(body.local_decls.push(decl.clone()));
        }
        for (argument, local) in arguments.into_iter().zip(locals.iter().copied().skip(1)) {
            body[block].statements.push(Statement::new(
                source_info,
                StatementKind::Assign(Box::new((
                    local.into(),
                    Rvalue::Use(argument, WithRetag::Yes),
                ))),
            ));
        }
        let cleanup = body[block].is_cleanup;
        let scope_start = body.source_scopes.len();
        let scope_id = |scope: SourceScope| SourceScope::from_usize(scope_start + scope.index());
        let parent = &body.source_scopes[source_info.scope];
        let inlined_parent = if parent.inlined.is_some() {
            Some(source_info.scope)
        } else {
            parent.inlined_parent_scope
        };
        for scope in &mut callee.source_scopes {
            if scope.parent_scope.is_none() {
                scope.parent_scope = Some(source_info.scope);
                scope.inlined_parent_scope = inlined_parent;
                scope.inlined = Some((instance, source_info.span));
            } else {
                scope.parent_scope = scope.parent_scope.map(scope_id);
                scope.inlined_parent_scope = Some(scope_id(
                    scope.inlined_parent_scope.unwrap_or(OUTERMOST_SOURCE_SCOPE),
                ));
            }
        }
        body.source_scopes.append(&mut callee.source_scopes);
        let mut remap = Remap {
            tcx,
            locals: &locals,
            block_start,
            target,
            unwind,
            scope_start,
        };
        for (bb, data) in callee.basic_blocks_mut().iter_enumerated_mut() {
            remap.visit_basic_block_data(bb, data);
            data.is_cleanup |= cleanup;
        }
        for decl in body.local_decls.iter_mut().skip(local_start) {
            decl.source_info.scope = scope_id(decl.source_info.scope);
        }
        body.basic_blocks_mut().append(callee.basic_blocks_mut());
        body[block].terminator_mut().kind = TerminatorKind::Goto {
            target: BasicBlock::from_usize(block_start),
        };
        *budget -= cost;
        changed = true;
        history.push(instance);
        let end = body.basic_blocks.len();
        expand_blocks(tcx, body, block_start..end, history, budget);
        history.pop();
    }
    changed
}

struct Remap<'a, 'tcx> {
    tcx: TyCtxt<'tcx>,
    locals: &'a [Local],
    block_start: usize,
    target: BasicBlock,
    unwind: UnwindAction,
    scope_start: usize,
}
impl Remap<'_, '_> {
    fn block(&self, block: BasicBlock) -> BasicBlock {
        BasicBlock::from_usize(self.block_start + block.index())
    }
    fn unwind(&self, unwind: UnwindAction) -> UnwindAction {
        match unwind {
            UnwindAction::Cleanup(bb) => UnwindAction::Cleanup(self.block(bb)),
            UnwindAction::Continue => self.unwind,
            other => other,
        }
    }
}
impl<'tcx> MutVisitor<'tcx> for Remap<'_, 'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }
    fn visit_local(&mut self, local: &mut Local, _: mir::visit::PlaceContext, _: Location) {
        *local = self.locals[local.index()];
    }
    fn visit_source_scope(&mut self, scope: &mut SourceScope) {
        *scope = SourceScope::from_usize(self.scope_start + scope.index());
    }
    fn visit_terminator(&mut self, term: &mut Terminator<'tcx>, location: Location) {
        if !matches!(term.kind, TerminatorKind::Return) {
            self.super_terminator(term, location);
        } else {
            self.visit_source_info(&mut term.source_info);
        }
        match &mut term.kind {
            TerminatorKind::Return => {
                term.kind = TerminatorKind::Goto {
                    target: self.target,
                }
            }
            TerminatorKind::UnwindResume => {
                term.kind = match self.unwind {
                    UnwindAction::Cleanup(bb) => TerminatorKind::Goto { target: bb },
                    UnwindAction::Continue => TerminatorKind::UnwindResume,
                    UnwindAction::Unreachable => TerminatorKind::Unreachable,
                    UnwindAction::Terminate(reason) => TerminatorKind::UnwindTerminate(reason),
                }
            }
            other => {
                let unwind = other.unwind_mut().map(|u| self.unwind(*u));
                other.successors_mut(|successor| *successor = self.block(*successor));
                if let Some(mapped) = unwind {
                    *other.unwind_mut().unwrap() = mapped;
                }
            }
        }
    }
}
