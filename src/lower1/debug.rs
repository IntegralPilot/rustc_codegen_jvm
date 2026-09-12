//! Shared source locations, lexical shadowing and MIR debug uses.
use crate::oomir;
use std::sync::Arc;
mod scopes;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_middle::{
    mir::{
        Body, Local, Location, SourceScope,
        visit::{PlaceContext, Visitor},
    },
    ty::TyCtxt,
};
use rustc_span::{Span, hygiene};

pub(crate) fn source_location(
    tcx: TyCtxt<'_>,
    function_span: Span,
    span: Span,
) -> Option<oomir::SourceLocation> {
    if !crate::lower2::debug_info_options(tcx).line_numbers || span.is_dummy() {
        return None;
    }

    let span = hygiene::walk_chain_collapsed(span, function_span);
    let location = tcx.sess.source_map().lookup_char_pos(span.lo());
    Some(oomir::SourceLocation {
        file_name: location.file.name.short().to_string(),
        line: u32::try_from(location.line).ok()?,
    })
}

pub(crate) struct DebugScopeCache {
    visible_without_references: Vec<Arc<[usize]>>,
    variables_by_local: HashMap<usize, Vec<usize>>,
}

impl DebugScopeCache {
    pub(super) fn new(
        mir: &Body<'_>,
        debug_variables: &[oomir::DebugVariable],
        debug_variable_scopes: &[SourceScope],
    ) -> Self {
        if debug_variables.is_empty() {
            return Self {
                visible_without_references: Vec::new(),
                variables_by_local: HashMap::default(),
            };
        }
        let parents: Vec<_> = mir
            .source_scopes
            .iter()
            .map(|s| s.parent_scope.map(|p| p.index()))
            .collect();
        let names: Vec<_> = debug_variables.iter().map(|v| v.name.as_str()).collect();
        let scopes: Vec<_> = debug_variable_scopes.iter().map(|s| s.index()).collect();
        let visible_without_references = scopes::visibility(&parents, &names, &scopes);

        let mut variables_by_local = HashMap::<usize, Vec<usize>>::default();
        for (index, variable) in debug_variables.iter().enumerate() {
            let local = variable
                .oomir_name
                .strip_prefix("_cell_")
                .or_else(|| variable.oomir_name.strip_prefix('_'))
                .and_then(|value| value.parse::<usize>().ok());
            if let Some(local) = local {
                variables_by_local.entry(local).or_default().push(index);
            }
        }

        Self {
            visible_without_references,
            variables_by_local,
        }
    }
}

pub(crate) fn local_variable_scope(
    cache: &DebugScopeCache,
    scope: SourceScope,
    referenced_locals: &HashSet<Local>,
    debug_variables: &[oomir::DebugVariable],
) -> oomir::Instruction {
    oomir::Instruction::LocalVariableScope(visible_local_variables(
        cache,
        scope,
        referenced_locals,
        debug_variables,
    ))
}

pub(crate) fn visible_local_variables(
    cache: &DebugScopeCache,
    scope: SourceScope,
    referenced_locals: &HashSet<Local>,
    debug_variables: &[oomir::DebugVariable],
) -> Vec<usize> {
    if referenced_locals.is_empty() {
        return cache
            .visible_without_references
            .get(scope.index())
            .map_or_else(Vec::new, |values| values.to_vec());
    }
    let mut visible_by_name = HashMap::<&str, usize>::default();
    for index in cache
        .visible_without_references
        .get(scope.index())
        .into_iter()
        .flat_map(|values| values.iter())
        .copied()
    {
        let Some(variable) = debug_variables.get(index) else {
            continue;
        };
        visible_by_name.insert(&variable.name, index);
    }

    for local in referenced_locals {
        for index in cache
            .variables_by_local
            .get(&local.index())
            .into_iter()
            .flat_map(|values| values.iter())
            .copied()
        {
            let Some(variable) = debug_variables.get(index) else {
                continue;
            };
            // MIR optimizations can move a binding's definition or use into
            // its parent source scope. A referenced debug local is still the
            // visible binding and shadows an outer binding by source name.
            visible_by_name.insert(&variable.name, index);
        }
    }

    let mut visible = visible_by_name.into_values().collect::<Vec<_>>();
    visible.sort_unstable();
    visible
}

#[derive(Default)]
pub(super) struct DebugLocalCollector {
    pub(super) locals: HashSet<Local>,
}

impl<'tcx> Visitor<'tcx> for DebugLocalCollector {
    fn visit_local(&mut self, local: Local, context: PlaceContext, _: Location) {
        if !matches!(context, PlaceContext::NonUse(_)) {
            self.locals.insert(local);
        }
    }
}
