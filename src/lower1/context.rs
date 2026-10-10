//! Lowering state has an explicit owner. Shard caches are released at handoff;
//! crate caches are released once all MIR has been lowered.
//! Rust type keys are scoped to their `TyCtxt`; definitions and construction
//! caches share a lifetime, rather than relying on a mutable map's address.
use crate::oomir;
use rustc_data_structures::sync::Lock;
mod names;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_middle::{
    mir::{self, interpret::AllocId},
    ty::{EarlyBinder, GenericArgsRef, Instance, Ty, TyCtxt, TypingEnv},
};
use std::{
    ops::{Deref, DerefMut},
    sync::Arc,
};

pub(crate) type Module<'tcx> = oomir::Module<Definitions<'tcx>>;
pub(crate) type Shared<'tcx> = Arc<CrateContext<'tcx>>;
pub(crate) type CheckedIntrinsic = (String, String, String);

/// Shared by the lowering shards of one crate on rustc query threads.
/// No completed function bodies or serialized output are retained here.
#[derive(Default)]
pub(crate) struct CrateContext<'tcx> {
    pub(crate) upstream_symbols: HashSet<u64>,
    foreign_interface_impls: crate::foreign_interfaces::Index,
    provided_symbols: Lock<HashSet<u64>>,
    normalized: Lock<HashMap<(Ty<'tcx>, GenericArgsRef<'tcx>), Ty<'tcx>>>,
    tuple_abis: Lock<HashMap<String, Vec<oomir::Type>>>,
    allocations: Lock<HashMap<AllocId, String>>,
    checked_intrinsics: Lock<HashSet<CheckedIntrinsic>>,
    union_bodies: Lock<HashSet<Ty<'tcx>>>,
    storage_objects: Lock<HashMap<Ty<'tcx>, bool>>,
    byte_support: Lock<HashMap<Ty<'tcx>, Result<(), String>>>,
    completed_codecs: Lock<HashMap<Ty<'tcx>, super::types::PointerMemoryCodec>>,
    caller_locations: Lock<HashMap<rustc_span::Span, oomir::Constant>>,
    names: names::Names<'tcx>,
    references: Lock<Vec<Instance<'tcx>>>,
}

impl<'tcx> CrateContext<'tcx> {
    pub(crate) fn with_upstream_symbols(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            foreign_interface_impls: crate::foreign_interfaces::Index::new(tcx),
            upstream_symbols: crate::symbols::upstream(tcx)
                .expect("could not read JVM symbol indexes"),
            ..Self::default()
        }
    }
    pub(crate) fn provided_symbols(&self) -> Vec<u64> {
        let mut symbols = self
            .provided_symbols
            .borrow()
            .iter()
            .copied()
            .collect::<Vec<_>>();
        symbols.sort_unstable();
        symbols
    }

    pub(crate) fn take_references(&self) -> Vec<Instance<'tcx>> {
        std::mem::take(&mut *self.references.borrow_mut())
    }
}

#[derive(Default)]
pub(crate) struct Definitions<'tcx> {
    values: HashMap<String, oomir::DataType>,
    // Owner metadata for imports and generated function-pointer adapters;
    // foreign interfaces must never acquire generated classfile definitions.
    pub(crate) foreign_interfaces: Lock<HashSet<String>>,
    pub(crate) foreign_methods: HashMap<Ty<'tcx>, HashMap<String, Instance<'tcx>>>,
    pub(super) representations: HashMap<Ty<'tcx>, oomir::Type>,
    pub(super) defined_enums: HashSet<String>,
    shared: Shared<'tcx>,
    checked_intrinsics: Vec<CheckedIntrinsic>,
    next_temporary: usize,
    body: BodyFacts,
}

impl<'tcx> Definitions<'tcx> {
    pub(crate) fn new(shared: Shared<'tcx>) -> Self {
        Self {
            shared,
            values: HashMap::default(),
            foreign_interfaces: Lock::default(),
            foreign_methods: HashMap::default(),
            representations: HashMap::default(),
            defined_enums: HashSet::default(),
            checked_intrinsics: Vec::new(),
            next_temporary: 0,
            body: BodyFacts::default(),
        }
    }

    pub(crate) fn foreign_interface_candidates(
        &self,
        def_id: rustc_span::def_id::DefId,
    ) -> &[rustc_span::def_id::DefId] {
        self.shared.foreign_interface_impls.candidates(def_id)
    }

    /// Nested closure lowering temporarily owns its own function facts. The
    /// enclosing body is restored before lowering its next instruction.
    pub(super) fn with_body<R>(
        &mut self,
        mir: &mir::Body<'tcx>,
        lower: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let previous = std::mem::replace(&mut self.body, BodyFacts::new(mir));
        let result = lower(self);
        self.body = previous;
        result
    }

    pub(super) fn local_uses_stable_cell(&self, local: mir::Local) -> bool {
        self.body.stable_cells[local.index()]
    }

    pub(crate) fn normalize(
        &mut self,
        tcx: TyCtxt<'tcx>,
        ty: Ty<'tcx>,
        instance: Instance<'tcx>,
    ) -> Ty<'tcx> {
        let args = instance.args;
        let key = (ty, args);
        if let Some(&resolved) = self.shared.normalized.borrow().get(&key) {
            return resolved;
        }
        let instantiated = EarlyBinder::bind(tcx, ty).instantiate(tcx, args);
        let resolved = tcx
            .try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), instantiated)
            .unwrap_or_else(|_| instantiated.skip_norm_wip());
        self.shared.normalized.borrow_mut().insert(key, resolved);
        resolved
    }

    pub(crate) fn tuple_name_conflicts(&self, name: &str, fields: &[oomir::Type]) -> bool {
        let mut abis = self.shared.tuple_abis.borrow_mut();
        match abis.get(name) {
            Some(previous) => previous != fields,
            None => {
                abis.insert(name.to_owned(), fields.to_vec());
                false
            }
        }
    }

    pub(crate) fn allocation_identity(&self, id: AllocId, candidate: String) -> String {
        self.shared
            .allocations
            .borrow_mut()
            .entry(id)
            .or_insert(candidate)
            .clone()
    }

    pub(crate) fn has_upstream_body(&self, key: u64) -> bool {
        self.shared.upstream_symbols.contains(&key)
    }
    pub(crate) fn record_provided_body(&self, key: u64) {
        self.shared.provided_symbols.borrow_mut().insert(key);
    }

    pub(crate) fn claim_union_body(&self, ty: Ty<'tcx>) -> bool {
        self.shared.union_bodies.borrow_mut().insert(ty)
    }

    pub(crate) fn storage_objects(&self, ty: Ty<'tcx>) -> Option<bool> {
        self.shared.storage_objects.borrow().get(&ty).copied()
    }
    pub(crate) fn remember_storage_objects(&self, ty: Ty<'tcx>, value: bool) {
        self.shared.storage_objects.borrow_mut().insert(ty, value);
    }
    pub(crate) fn byte_support(&self, ty: Ty<'tcx>) -> Option<Result<(), String>> {
        self.shared.byte_support.borrow().get(&ty).cloned()
    }
    pub(crate) fn remember_byte_support(&self, ty: Ty<'tcx>, value: Result<(), String>) {
        self.shared.byte_support.borrow_mut().insert(ty, value);
    }

    pub(super) fn completed_codec(&self, ty: Ty<'tcx>) -> Option<super::types::PointerMemoryCodec> {
        self.shared.completed_codecs.borrow().get(&ty).cloned()
    }

    pub(super) fn complete_codec(&self, ty: Ty<'tcx>, codec: super::types::PointerMemoryCodec) {
        self.shared.completed_codecs.borrow_mut().insert(ty, codec);
    }

    pub(super) fn caller_location(&self, span: rustc_span::Span) -> Option<oomir::Constant> {
        self.shared.caller_locations.borrow().get(&span).cloned()
    }

    pub(super) fn remember_caller_location(&self, span: rustc_span::Span, value: oomir::Constant) {
        self.shared
            .caller_locations
            .borrow_mut()
            .insert(span, value);
    }

    /// The first requesting shard owns construction of this canonical helper.
    pub(crate) fn request_checked_intrinsic(&mut self, operation: &str, ty: &str, tuple: &str) {
        let key = (operation.to_owned(), ty.to_owned(), tuple.to_owned());
        if self
            .shared
            .checked_intrinsics
            .borrow_mut()
            .insert(key.clone())
        {
            self.checked_intrinsics.push(key);
        }
    }

    pub(crate) fn next_temporary(&mut self) -> usize {
        let id = self.next_temporary;
        self.next_temporary = id.checked_add(1).expect("temporary ID capacity");
        id
    }

    pub(crate) fn finish(mut self) -> HashMap<String, oomir::DataType> {
        if !self.checked_intrinsics.is_empty() {
            let helpers = super::control_flow::checked_intrinsics::emit_all_needed_intrinsics(
                &self.checked_intrinsics,
            );
            self.values
                .insert("RustcCodegenJVMIntrinsics".into(), helpers);
        }
        self.values
    }
}

/// Facts are constructed once and released with their MIR body, independent
/// of thread identity and of the lifetime of generated type definitions.
#[derive(Default)]
struct BodyFacts {
    stable_cells: Vec<bool>,
}

impl BodyFacts {
    fn new(body: &mir::Body<'_>) -> Self {
        let mut stable_cells = vec![false; body.local_decls.len()];
        for block in body.basic_blocks.iter() {
            for statement in &block.statements {
                let mir::StatementKind::Assign(assignment) = &statement.kind else {
                    continue;
                };
                let place = match &assignment.1 {
                    mir::Rvalue::Ref(_, _, place) | mir::Rvalue::RawPtr(_, place) => place,
                    _ => continue,
                };
                // A field borrow retains provenance for the root allocation.
                if !matches!(place.projection.first(), Some(mir::ProjectionElem::Deref)) {
                    stable_cells[place.local.index()] = true;
                }
            }
        }
        for index in 1..=body.arg_count {
            if matches!(
                body.local_decls[mir::Local::from_usize(index)].ty.kind(),
                rustc_middle::ty::TyKind::Ref(..) | rustc_middle::ty::TyKind::RawPtr(..)
            ) {
                // Incoming pointers already are stable addresses.
                stable_cells[index] = false;
            }
        }
        Self { stable_cells }
    }
}

impl Deref for Definitions<'_> {
    type Target = HashMap<String, oomir::DataType>;
    fn deref(&self) -> &Self::Target {
        &self.values
    }
}
impl DerefMut for Definitions<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.values
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonical_requests_are_shared_by_shards_and_isolated_between_crates() {
        let shared = Shared::default();
        let mut first = Definitions::new(Arc::clone(&shared));
        let mut second = Definitions::new(shared);
        let mut separate = Definitions::default();
        for definitions in [&mut first, &mut second, &mut separate] {
            definitions.request_checked_intrinsic("add", "i32", "test/Pair");
            definitions.request_checked_intrinsic("add", "i32", "test/Pair");
        }
        assert_eq!(first.checked_intrinsics.len(), 1);
        assert!(second.checked_intrinsics.is_empty());
        assert_eq!(separate.checked_intrinsics.len(), 1);
        assert!(!first.tuple_name_conflicts("Tuple", &[oomir::Type::I32]));
        assert!(!second.tuple_name_conflicts("Tuple", &[oomir::Type::I32]));
        assert!(second.tuple_name_conflicts("Tuple", &[oomir::Type::I64]));
        assert!(!separate.tuple_name_conflicts("Tuple", &[oomir::Type::I64]));
        assert_eq!(
            (
                first.next_temporary(),
                first.next_temporary(),
                second.next_temporary()
            ),
            (0, 1, 0)
        );
        assert!(first.finish().contains_key("RustcCodegenJVMIntrinsics"));
        assert!(!second.finish().contains_key("RustcCodegenJVMIntrinsics"));
    }
}
