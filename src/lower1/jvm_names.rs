use rustc_hir::def::{CtorOf, DefKind};
use rustc_middle::ty::{GenericArgsRef, Instance, TyCtxt};
use rustc_span::def_id::{CrateNum, DefId, LOCAL_CRATE};
use rustc_span::sym;

const RUNTIME_CRATES: &[rustc_span::Symbol] = &[sym::core, sym::alloc, sym::std];

fn is_runtime_crate_name(crate_name: rustc_span::Symbol) -> bool {
    RUNTIME_CRATES.contains(&crate_name)
}

pub fn uses_compiled_core(tcx: TyCtxt<'_>) -> bool {
    tcx.sess.target.llvm_target.as_ref() == "jvm-unknown-jvm"
}

pub fn compiles_external_core_instances(tcx: TyCtxt<'_>) -> bool {
    uses_compiled_core(tcx) && tcx.crate_name(LOCAL_CRATE) != sym::core
}

pub fn is_runtime_crate<'tcx>(tcx: TyCtxt<'tcx>, krate: CrateNum) -> bool {
    let crate_name = tcx.crate_name(krate);
    crate_name == sym::core
        || tcx
            .used_crate_source(krate)
            .paths()
            .any(|path| path.starts_with(tcx.sess.opts.sysroot.path()))
}

pub fn member_name(raw: &str) -> String {
    jvm_identifier(raw)
}

pub fn path_segment(raw: &str) -> String {
    normalize_def_path_segment(raw)
}

pub fn crate_root<'tcx>(tcx: TyCtxt<'tcx>, krate: CrateNum) -> String {
    let crate_name = tcx.crate_name(krate);
    if is_runtime_crate_name(crate_name) {
        format!("org/rustlang/{}", jvm_identifier(crate_name.as_str()))
    } else {
        jvm_identifier(crate_name.as_str())
    }
}

pub fn crate_module_class<'tcx>(tcx: TyCtxt<'tcx>, krate: CrateNum) -> String {
    let root = crate_root(tcx, krate);
    let crate_name = tcx.crate_name(krate);
    if is_runtime_crate_name(crate_name) {
        root
    } else {
        format!("{root}/{}", jvm_identifier(crate_name.as_str()))
    }
}

pub fn class_for_def_id<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> String {
    let root = crate_root(tcx, def_id.krate);
    let mut segments = def_path_segments(tcx, def_id);
    if segments.is_empty() {
        return root;
    }
    segments.insert(0, root);
    segments.join("/")
}

pub fn function_item_class_for_def_id<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> String {
    let base = class_for_def_id(tcx, def_id);
    if matches!(tcx.def_kind(def_id), DefKind::Ctor(CtorOf::Struct, _)) {
        format!("{base}$FnItem")
    } else {
        base
    }
}

pub fn disambiguated_def_path_token(tcx: TyCtxt<'_>, def_id: DefId) -> String {
    tcx.def_path(def_id)
        .data
        .iter()
        .map(|component| jvm_identifier(component.as_sym(true).as_str()))
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<_>>()
        .join("_")
}

/// Anonymous carriers are internal ABI identities; constructing their names
/// must not instantiate the captured types or build a second type schema.
pub fn anonymous_class_for_args<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    args: GenericArgsRef<'tcx>,
    coroutine: bool,
) -> String {
    let kind = if coroutine { "Coroutine" } else { "Closure" };
    // Synthetic capture and signature arguments can differ between the
    // defining MIR and an opaque return type's revealed view. The definition
    // and its enclosing generic arguments uniquely determine the carrier.
    let parent_args = if coroutine {
        args.as_coroutine().parent_args()
    } else {
        args.as_closure().parent_args()
    };
    let identity = super::types::stable_instance_identity(tcx, def_id, tcx.mk_args(parent_args));
    format!("{}/{kind}_{identity}", crate_root(tcx, def_id.krate))
}

pub fn owner_class_for_function<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> String {
    let crate_name = tcx.crate_name(def_id.krate);
    let root = crate_root(tcx, def_id.krate);
    let mut segments = def_path_segments(tcx, def_id);
    if !segments.is_empty() {
        segments.pop();
    }

    if segments.is_empty() {
        if is_runtime_crate_name(crate_name) {
            root
        } else {
            crate_module_class(tcx, def_id.krate)
        }
    } else {
        segments.insert(0, root);
        segments.join("/")
    }
}

pub fn method_for_function<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> String {
    let mut current = def_id;
    loop {
        let key = tcx.def_key(current);
        if let Some(name) = key.disambiguated_data.data.get_opt_name() {
            return normalize_def_path_segment(name.as_str());
        }
        let Some(parent) = key.parent else {
            return member_name(&tcx.def_path_str(def_id));
        };
        current = DefId {
            krate: def_id.krate,
            index: parent,
        };
    }
}

pub fn synthetic_class_for_instance<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    local_name: impl AsRef<str>,
) -> String {
    format!(
        "{}/{}",
        crate_root(tcx, instance.def_id().krate),
        path_segment(local_name.as_ref())
    )
}

fn def_path_segments<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> Vec<String> {
    tcx.def_path(def_id)
        .data
        .iter()
        .filter_map(|component| component.data.get_opt_name())
        .map(|name| normalize_def_path_segment(name.as_str()))
        .filter(|segment| !segment.is_empty())
        .collect()
}

fn normalize_def_path_segment(raw: &str) -> String {
    let mut segment = raw.trim();
    if segment.starts_with('<') && segment.ends_with('>') && segment.len() > 2 {
        segment = segment[1..segment.len() - 1].trim();
    }

    if let Some((_, trait_part)) = segment.split_once(" as ") {
        segment = trait_part.split('<').next().unwrap_or(trait_part).trim();
    } else if segment.starts_with("impl") {
        segment = segment
            .split_once(" for ")
            .map(|(trait_part, _)| trait_part)
            .unwrap_or(segment);
        segment = segment.trim_start_matches("impl").trim();
        if segment.starts_with('<')
            && let Some(end) = matching_angle_end(segment)
        {
            segment = segment[end + 1..].trim();
        }
        segment = segment.split('<').next().unwrap_or(segment).trim();
    }

    jvm_identifier(segment)
}

fn matching_angle_end(input: &str) -> Option<usize> {
    let mut depth = 0usize;
    for (idx, ch) in input.char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
    }
    None
}

fn jvm_identifier(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len().max(1));
    let mut previous_was_separator = false;

    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
            previous_was_separator = false;
        } else if !previous_was_separator && !out.is_empty() {
            out.push('_');
            previous_was_separator = true;
        }
    }

    while out.ends_with('_') {
        out.pop();
    }

    if out.is_empty() {
        "jvm_unnamed".to_string()
    } else if out.as_bytes()[0].is_ascii_digit() {
        format!("_{out}")
    } else {
        out
    }
}
