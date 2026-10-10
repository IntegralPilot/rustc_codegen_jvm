use crate::{lower1, oomir};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_middle::ty::{
    GenericArgs, Instance, PseudoCanonicalInput, TraitRef, Ty, TyCtxt, TyKind, TypeVisitableExt,
    TypingEnv,
};
use rustc_span::def_id::DefId;

fn attribute(tcx: TyCtxt<'_>, def_id: DefId, name: &str) -> Option<String> {
    #[allow(deprecated)]
    tcx.get_all_attrs(def_id).iter().find_map(|attr| {
        let path = attr.path();
        (path.len() == 2 && path[0].as_str() == "jvm_codegen" && path[1].as_str() == name)
            .then(|| attr.value_str().map(|value| value.to_string()))
            .flatten()
    })
}

pub(crate) fn interface_name(tcx: TyCtxt<'_>, def_id: DefId) -> Option<String> {
    let name = attribute(tcx, def_id, "interface")?;
    Some(
        lower1::naming::parse_jvm_class_link_name(&name)
            .unwrap_or_else(|error| tcx.dcx().span_fatal(tcx.def_span(def_id), error)),
    )
}

pub(crate) fn method_name(tcx: TyCtxt<'_>, def_id: DefId) -> Option<String> {
    let item = tcx.opt_associated_item(def_id)?;
    let decl = item.trait_item_def_id().unwrap_or(def_id);
    let trait_id = tcx.associated_item(decl).trait_container(tcx)?;
    interface_name(tcx, trait_id)?;
    Some(attribute(tcx, decl, "method").unwrap_or_else(|| tcx.item_name(decl).to_string()))
}

#[derive(Default)]
pub(crate) struct Index(HashMap<DefId, Vec<DefId>>);

impl Index {
    pub(crate) fn new(tcx: TyCtxt<'_>) -> Self {
        let mut index: HashMap<DefId, Vec<DefId>> = HashMap::default();
        let crates = std::iter::once(rustc_span::def_id::LOCAL_CRATE)
            .chain(tcx.crates(()).iter().copied())
            .filter(|&krate| {
                krate == rustc_span::def_id::LOCAL_CRATE
                    || !lower1::jvm_names::is_runtime_crate(tcx, krate)
            });
        for trait_id in crates.flat_map(|krate| tcx.traits(krate).iter().copied()) {
            if interface_name(tcx, trait_id).is_none() {
                continue;
            }
            for impl_id in tcx.all_impls(trait_id) {
                let ty = tcx.type_of(impl_id).instantiate_identity().skip_norm_wip();
                let TyKind::Adt(def, _) = ty.kind() else {
                    tcx.dcx().span_fatal(
                        tcx.def_span(impl_id),
                        "foreign JVM interfaces require a Rust struct implementor",
                    );
                };
                if !def.is_struct() {
                    tcx.dcx().span_fatal(
                        tcx.def_span(impl_id),
                        "foreign JVM interfaces require a Rust struct implementor",
                    );
                }
                let traits = index.entry(def.did()).or_default();
                if !traits.contains(&trait_id) {
                    traits.push(trait_id);
                }
            }
        }
        Self(index)
    }

    pub(crate) fn candidates(&self, def_id: DefId) -> &[DefId] {
        self.0.get(&def_id).map_or(&[], Vec::as_slice)
    }
}

pub(crate) fn register_implementations<'tcx>(
    tcx: TyCtxt<'tcx>,
    ty: Ty<'tcx>,
    class: &str,
    data_types: &mut lower1::context::Definitions<'tcx>,
) {
    let TyKind::Adt(def, _) = ty.kind() else {
        return;
    };
    if ty.has_param() || ty.has_escaping_bound_vars() {
        return;
    }
    let candidates = data_types.foreign_interface_candidates(def.did()).to_vec();
    if candidates.is_empty() || data_types.foreign_methods.contains_key(&ty) {
        return;
    }
    data_types.foreign_methods.insert(ty, HashMap::default());
    for trait_id in candidates {
        let trait_ref = TraitRef::new(tcx, trait_id, [ty]);
        if tcx
            .codegen_select_candidate(PseudoCanonicalInput {
                typing_env: TypingEnv::fully_monomorphized(),
                value: trait_ref,
            })
            .is_err()
        {
            continue;
        }
        let name = interface_name(tcx, trait_id).unwrap();
        data_types
            .foreign_interfaces
            .borrow_mut()
            .insert(name.clone());
        if let Some(oomir::DataType::Class { interfaces, .. }) = data_types.get_mut(class) {
            if !interfaces.contains(&name) {
                interfaces.push(name);
            }
        }
        for method in tcx.associated_items(trait_id).in_definition_order() {
            if !method.is_method() {
                continue;
            }
            let instance = Instance::expect_resolve(
                tcx,
                TypingEnv::fully_monomorphized(),
                method.def_id,
                trait_ref.args,
                tcx.def_span(def.did()),
            );
            let name = method_name(tcx, method.def_id).unwrap();
            if let Some(previous) = data_types
                .foreign_methods
                .get_mut(&ty)
                .unwrap()
                .insert(name.clone(), instance)
                && previous != instance
            {
                tcx.dcx().span_fatal(
                    tcx.def_span(def.did()),
                    format!("multiple Rust implementations map to JVM interface method `{name}`"),
                );
            }
            // Queue methods that are reachable only from Java.
            data_types.function_name(tcx, instance);
        }
    }
}

pub(crate) fn lower_implementations<'tcx>(
    tcx: TyCtxt<'tcx>,
    module: &mut lower1::context::Module<'tcx>,
) {
    let mut types = HashSet::default();
    for (&trait_id, impls) in tcx.all_local_trait_impls(()) {
        if interface_name(tcx, trait_id).is_none() {
            continue;
        }
        for &impl_id in impls {
            let impl_id = impl_id.to_def_id();
            if tcx.generics_of(impl_id).requires_monomorphization(tcx) {
                continue;
            }
            let args = GenericArgs::identity_for_item(tcx, impl_id);
            let ty = tcx.type_of(impl_id).instantiate(tcx, args).skip_norm_wip();
            if types.insert(ty) {
                lower1::types::force_define_named_adt(
                    ty,
                    tcx,
                    &mut module.data_types,
                    Instance::new_raw(impl_id, args),
                );
            }
        }
    }
}
