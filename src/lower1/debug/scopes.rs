//! Resolve lexical shadowing in one traversal, sharing unchanged scope lists.
use rustc_hash::FxHashMap;
use std::sync::Arc;

pub(super) fn visibility(
    parents: &[Option<usize>],
    names: &[&str],
    scopes: &[usize],
) -> Vec<Arc<[usize]>> {
    let empty: Arc<[usize]> = Arc::from([]);
    let mut result = vec![Arc::clone(&empty); parents.len()];
    if names.is_empty() {
        return result;
    }
    let mut children = vec![None; parents.len()];
    let mut siblings = children.clone();
    let mut pending = Vec::new();
    for (scope, &parent) in parents.iter().enumerate().rev() {
        if let Some(parent) = parent {
            siblings[scope] = children[parent].replace(scope);
        } else {
            pending.push((scope, None));
        }
    }
    let mut bindings = vec![None; parents.len()];
    let mut next_binding = vec![None; names.len()];
    for (index, &scope) in scopes.iter().take(names.len()).enumerate().rev() {
        next_binding[index] = bindings[scope].replace(index);
    }
    let mut visible = FxHashMap::<&str, usize>::default();
    let mut undo = Vec::new();
    while let Some((scope, restore)) = pending.pop() {
        if let Some(start) = restore {
            for (name, old) in undo.drain(start..).rev() {
                if let Some(old) = old {
                    visible.insert(name, old);
                } else {
                    visible.remove(name);
                }
            }
            continue;
        }
        let start = undo.len();
        let mut binding = bindings[scope];
        while let Some(index) = binding {
            let name = names[index];
            undo.push((name, visible.insert(name, index)));
            binding = next_binding[index];
        }
        result[scope] = if bindings[scope].is_none() {
            parents[scope].map_or_else(|| Arc::clone(&empty), |parent| Arc::clone(&result[parent]))
        } else {
            let mut values: Vec<_> = visible.values().copied().collect();
            values.sort_unstable();
            values.into()
        };
        pending.push((scope, Some(start)));
        let mut child = children[scope];
        while let Some(scope) = child {
            pending.push((scope, None));
            child = siblings[scope];
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn contains(parents: &[Option<usize>], ancestor: usize, mut scope: usize) -> bool {
        loop {
            if scope == ancestor {
                return true;
            }
            let Some(parent) = parents[scope] else {
                return false;
            };
            scope = parent;
        }
    }

    #[test]
    fn lexical_visibility_matches_ancestor_search_including_shadowing() {
        for seed in 0..40 {
            let parents: Vec<_> = (0..60)
                .map(|n| (n != 0).then(|| (n * 17 + seed) % n))
                .collect();
            let names: Vec<_> = (0..150).map(|n| format!("local{}", n % 13)).collect();
            let names: Vec<_> = names.iter().map(String::as_str).collect();
            let scopes: Vec<_> = (0..names.len())
                .map(|n| (n * 23 + seed) % parents.len())
                .collect();
            let actual = visibility(&parents, &names, &scopes);
            for scope in 0..parents.len() {
                let mut expected = FxHashMap::default();
                for (index, &binding) in scopes.iter().enumerate() {
                    if contains(&parents, binding, scope)
                        && expected
                            .get(names[index])
                            .is_none_or(|&old: &usize| contains(&parents, scopes[old], binding))
                    {
                        expected.insert(names[index], index);
                    }
                }
                let mut expected: Vec<_> = expected.into_values().collect();
                expected.sort_unstable();
                assert_eq!(
                    actual[scope].as_ref(),
                    expected,
                    "seed {seed}, scope {scope}"
                );
            }
        }
    }

    #[test]
    fn deep_scopes_share_unchanged_visibility_without_recursion() {
        let parents: Vec<_> = (0usize..20_000).map(|n| n.checked_sub(1)).collect();
        let scopes = visibility(&parents, &["argument"], &[0]);
        assert!(scopes.iter().all(|scope| Arc::ptr_eq(scope, &scopes[0])));
        assert_eq!(scopes[19_999].as_ref(), &[0]);
    }
}
