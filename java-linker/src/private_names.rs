//! Shorten proven private class names. Relocate symbolic strings but preserve ordinary literals.
use crate::*;

pub(crate) fn compact(index: &mut inputs::Index, names: &mut HashMap<String, String>) {
    let private = std::mem::take(&mut index.private_classes);
    let occupied = std::mem::take(&mut index.occupied_packages);
    if private.is_empty() {
        return;
    }
    // Kotlin finds the companion by appending "$Body". Keep both class names unchanged.
    let companions = index
        .groups
        .iter()
        .filter_map(|group| group.name.strip_suffix("$Body.class"))
        .collect::<HashSet<_>>();
    let mut prefix = "j".to_string();
    for i in 1.. {
        if !occupied.contains(&prefix) {
            break;
        }
        prefix = format!("j{i:x}");
    }
    let eligible = |name: &str| {
        private.contains(name)
            && !companions.contains(name)
            // Pointer's callable codec recognizes this runtime protocol.
            && !name.starts_with("org/rustlang/runtime/FnPtr_")
    };
    let mut targets = index
        .groups
        .iter()
        .filter_map(|group| {
            let name = group.name.trim_end_matches(".class");
            (!group.resource && eligible(name))
                .then(|| names.get(name).map_or(name, String::as_str).to_owned())
        })
        .collect::<Vec<_>>();
    targets.sort_unstable();
    targets.dedup();
    let short = targets
        .into_iter()
        .enumerate()
        .map(|(i, name)| {
            let target = if jvm_compiler_core::classfile::names::codec_owner(&name) {
                format!("{prefix}/Codecs_{i:x}")
            } else if name.contains("/mono/Mono") {
                // Preserve eligibility for exceptional constant-pool splitting.
                format!("{prefix}/mono/Mono_{i:x}")
            } else {
                format!("{prefix}/C{i:x}")
            };
            (name, target)
        })
        .collect::<HashMap<_, _>>();
    for group in &index.groups {
        let name = group.name.trim_end_matches(".class");
        if !group.resource && eligible(name) {
            let target = names.get(name).map_or(name, String::as_str);
            names.insert(name.to_owned(), short[target].clone());
        }
    }
}
