use rustc_hir::def_id::{CrateNum, DefId};
use rustc_middle::ty::{Instance, TyCtxt};

const RUNTIME_CRATES: &[&str] = &["core"];

pub fn member_name(raw: &str) -> String {
    jvm_identifier(raw)
}

pub fn path_segment(raw: &str) -> String {
    normalize_def_path_segment(raw)
}

pub fn crate_root<'tcx>(tcx: TyCtxt<'tcx>, krate: CrateNum) -> String {
    let crate_name = tcx.crate_name(krate).to_string();
    if RUNTIME_CRATES.contains(&crate_name.as_str()) {
        format!("org/rustlang/{}", jvm_identifier(&crate_name))
    } else {
        jvm_identifier(&crate_name)
    }
}

pub fn crate_module_class<'tcx>(tcx: TyCtxt<'tcx>, krate: CrateNum) -> String {
    let root = crate_root(tcx, krate);
    let crate_name = tcx.crate_name(krate).to_string();
    if RUNTIME_CRATES.contains(&crate_name.as_str()) {
        root
    } else {
        format!("{root}/{}", jvm_identifier(&crate_name))
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

pub fn closure_class_for_def_id<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> String {
    format!(
        "{}/{}",
        crate_root(tcx, def_id.krate),
        path_segment(&tcx.def_path_str(def_id))
    )
}

pub fn owner_class_for_function<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> String {
    let crate_name = tcx.crate_name(def_id.krate).to_string();
    let root = crate_root(tcx, def_id.krate);
    let mut segments = def_path_segments(tcx, def_id);
    if !segments.is_empty() {
        segments.pop();
    }

    if segments.is_empty() {
        if RUNTIME_CRATES.contains(&crate_name.as_str()) {
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
    def_path_segments(tcx, def_id)
        .pop()
        .unwrap_or_else(|| member_name(&tcx.def_path_str(def_id)))
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
