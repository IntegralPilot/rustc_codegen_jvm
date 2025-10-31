use rustc_hir::{ImplItemId, TraitItemId};
use rustc_middle::ty::TyCtxt;
use rustc_span::symbol::Ident;

pub trait ToIdent {
    fn to_ident(&self, tcx: TyCtxt<'_>) -> Ident;
}

impl ToIdent for &ImplItemId {
    // TODO: Filter results to get exactly what we need, instead of relying on iterating.
    fn to_ident(&self, tcx: TyCtxt<'_>) -> Ident {
        tcx.associated_item(self.owner_id).ident(tcx)
    }
}

impl ToIdent for &TraitItemId {
    // TODO: Filter results to get exactly what we need, instead of relying on iterating.
    fn to_ident(&self, tcx: TyCtxt<'_>) -> Ident {
        breadcrumbs::log!(breadcrumbs::LogLevel::Info, "utils", "associated items: ");
        tcx.associated_item(self.owner_id).ident(tcx)
    }
}
