//! Pure JVM identities can be reused across every shard in the crate. Type
//! construction still owns its schema contributions independently.
use super::*;
use crate::lower1::{jvm_names, naming};
use rustc_middle::ty::TyKind;
use rustc_span::def_id::DefId;

#[derive(Default)]
pub(super) struct Names<'tcx> {
    functions: HashMap<Instance<'tcx>, naming::FnNameData>,
    classes: HashMap<DefId, String>,
    readable: HashMap<DefId, String>,
    closures: HashMap<(DefId, GenericArgsRef<'tcx>, GenericArgsRef<'tcx>, bool), String>,
}

impl<'tcx> Definitions<'tcx> {
    pub(crate) fn function_name(
        &self,
        tcx: TyCtxt<'tcx>,
        instance: Instance<'tcx>,
    ) -> naming::FnNameData {
        if let Some(name) = self.shared.borrow().names.functions.get(&instance) {
            return name.clone();
        }
        let instance_ty = tcx
            .type_of(instance.def_id())
            .instantiate(tcx, instance.args)
            .skip_norm_wip();
        let name = if matches!(instance_ty.kind(), TyKind::Closure(..)) {
            naming::FnNameData {
                class_to_call_on: Some(naming::mono_owner_class(tcx, instance)),
                method_name: self.closure_method_name(tcx, instance),
            }
        } else {
            naming::mono_fn_name_from_instance(tcx, instance)
        };
        self.shared
            .borrow_mut()
            .names
            .functions
            .insert(instance, name.clone());
        name
    }

    pub(crate) fn class_name(&self, tcx: TyCtxt<'tcx>, def_id: DefId) -> String {
        self.shared
            .borrow_mut()
            .names
            .classes
            .entry(def_id)
            .or_insert_with(|| jvm_names::class_for_def_id(tcx, def_id))
            .clone()
    }

    pub(crate) fn readable_class_name(&self, tcx: TyCtxt<'tcx>, def_id: DefId) -> String {
        if let Some(name) = self.shared.borrow().names.readable.get(&def_id) {
            return name.clone();
        }
        let class = self.class_name(tcx, def_id);
        let class = class.strip_prefix("org/rustlang/").unwrap_or(&class);
        let name = crate::lower1::types::sanitize_name_token(&class.replace('/', "_"));
        self.shared
            .borrow_mut()
            .names
            .readable
            .insert(def_id, name.clone());
        name
    }

    pub(crate) fn closure_class_name(
        &self,
        tcx: TyCtxt<'tcx>,
        def_id: DefId,
        args: GenericArgsRef<'tcx>,
        instance: Instance<'tcx>,
        coroutine: bool,
    ) -> String {
        let key = (def_id, args, instance.args, coroutine);
        if let Some(name) = self.shared.borrow().names.closures.get(&key) {
            return name.clone();
        }
        let name = if coroutine {
            jvm_names::coroutine_class_for_args(tcx, def_id, args, instance)
        } else {
            jvm_names::closure_class_for_args(tcx, def_id, args, instance)
        };
        self.shared
            .borrow_mut()
            .names
            .closures
            .insert(key, name.clone());
        name
    }

    pub(crate) fn closure_method_name(
        &self,
        tcx: TyCtxt<'tcx>,
        instance: Instance<'tcx>,
    ) -> String {
        self.closure_class_name(tcx, instance.def_id(), instance.args, instance, false)
            .rsplit('/')
            .next()
            .expect("closure class has a final path segment")
            .to_owned()
    }
}
