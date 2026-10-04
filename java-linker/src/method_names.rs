//! Assign one short name per private method across shared constants and packed classes.
use crate::*;
use std::sync::Arc;

pub(crate) fn mangled(name: &str) -> bool {
    name.is_ascii()
        && !name.bytes().any(|b| b.is_ascii_control())
        && !name.contains(['/', '.', '(', ')', ';', ':', '#', '[', ' '])
        && name.rsplit_once('$').is_some_and(|(_, suffix)| {
            suffix.len() == 16 && suffix.bytes().all(|b| b.is_ascii_hexdigit())
        })
}

pub(crate) fn plan(
    methods: impl Iterator<Item = (u32, bool, bool)>,
    spellings: &[Arc<str>],
    fixed: &HashSet<u32>,
    reflective: &HashMap<u32, u32>,
    classes: impl Iterator<Item = impl AsRef<str>>,
) -> HashMap<Vec<u8>, Vec<u8>> {
    let mut candidates = HashSet::default();
    let mut pinned = fixed.clone();
    for (name, live, private) in methods {
        if !private {
            pinned.insert(name);
        } else if live {
            candidates.insert(name);
        }
    }
    let mut names = candidates
        .into_iter()
        .filter(|name| !pinned.contains(name) && !reflective.contains_key(name))
        .map(|name| spellings[name as usize].as_ref())
        .filter(|name| mangled(name))
        .collect::<HashSet<_>>();
    // A default-package class can share a UTF8 entry with a member name.
    for class in classes {
        names.remove(class.as_ref());
    }
    let mut names = names.into_iter().collect::<Vec<_>>();
    names.sort_unstable();
    let mut prefix = "$m".to_owned();
    while spellings.iter().any(|s| s.contains(&prefix)) {
        prefix.push('$');
    }
    names
        .into_iter()
        .enumerate()
        .map(|(i, name)| {
            // Keep source function names readable in stack traces.
            let readable = name.rsplit_once('$').unwrap().0;
            (
                name.as_bytes().to_vec(),
                format!("{readable}{prefix}{i:x}").into_bytes(),
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nominal_unresolved_and_reflective_names_prevent_global_shortening() {
        let names = [
            "local$0123456789abcdef",
            "external$0123456789abcdef",
            "reflective$0123456789abcdef",
            "field$0123456789abcdef",
            "class$0123456789abcdef",
            "unicode😀$0123456789abcdef",
        ]
        .map(Arc::<str>::from);
        let methods = [
            (0, true, true),
            (1, true, true),
            (1, false, false),
            (2, true, true),
            (3, true, true),
            (4, true, true),
            (5, true, true),
        ];
        let renamed = plan(
            methods.into_iter(),
            &names,
            &HashSet::from_iter([3]),
            &HashMap::from_iter([(2, 0)]),
            [names[4].as_ref()].into_iter(),
        );
        assert_eq!(renamed.len(), 1);
        assert_eq!(renamed[names[0].as_bytes()], b"local$m0");
    }
}
