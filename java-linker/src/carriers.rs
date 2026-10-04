//! Share proven private storage shapes, including recursive shapes. Keep unproven class identities distinct.
use crate::*;

/// Retain the live helpers from every alias before pruning. Otherwise, pruning can split equivalent classes.
pub(crate) fn live_representatives(
    carriers: &[(&str, &[u8])],
    dead_methods: &HashMap<String, HashSet<(JavaString, JavaString)>>,
) -> Vec<usize> {
    let initial = representatives(carriers);
    let mut dead_by_group = HashMap::<usize, HashSet<JavaString>>::default();
    for (i, (name, recipe)) in carriers.iter().enumerate() {
        if !recipe.starts_with(b"carrier-v2;enum-v1;") {
            continue;
        }
        let dead = dead_methods
            .get(*name)
            .map(|methods| {
                methods
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect::<HashSet<_>>()
            })
            .unwrap_or_default();
        dead_by_group
            .entry(initial[i])
            .and_modify(|common| common.retain(|method| dead.contains(method)))
            .or_insert(dead);
    }
    let live = carriers
        .iter()
        .enumerate()
        .map(|(i, (_, recipe))| {
            dead_by_group
                .get(&initial[i])
                .map_or(std::borrow::Cow::Borrowed(*recipe), |dead| {
                    live_recipe(recipe, dead)
                })
        })
        .collect::<Vec<_>>();
    if live
        .iter()
        .all(|recipe| matches!(recipe, std::borrow::Cow::Borrowed(_)))
    {
        return initial;
    }
    let recipes = carriers
        .iter()
        .zip(&live)
        .map(|((name, _), recipe)| (*name, recipe.as_ref()))
        .collect::<Vec<_>>();
    representatives(&recipes)
}

fn live_recipe<'a>(recipe: &'a [u8], dead: &HashSet<JavaString>) -> std::borrow::Cow<'a, [u8]> {
    use std::borrow::Cow;
    if dead.is_empty() || !recipe.starts_with(b"carrier-v2;enum-v1;") {
        return Cow::Borrowed(recipe);
    }
    // Each enum method recipe includes its nested field recipes and class references.
    let starts = recipe
        .windows(8)
        .enumerate()
        .filter_map(|(i, w)| (w == b";method=").then_some(i + 1))
        .collect::<Vec<_>>();
    let mut result = Vec::new();
    let mut cursor = 0;
    for (n, &start) in starts.iter().enumerate() {
        let name_start = start + 7;
        let Some(length) = recipe[name_start..].iter().position(|&b| b == b';') else {
            return Cow::Borrowed(recipe);
        };
        let name = &recipe[name_start..name_start + length];
        if dead.iter().any(|method| method.as_bytes() == name) {
            result.extend_from_slice(&recipe[cursor..start]);
            cursor = starts.get(n + 1).copied().unwrap_or(recipe.len());
        }
    }
    if cursor == 0 {
        return Cow::Borrowed(recipe);
    }
    result.extend_from_slice(&recipe[cursor..]);
    Cow::Owned(result)
}

pub(crate) fn representatives<'a>(carriers: &[(&'a str, &[u8])]) -> Vec<usize> {
    let names = carriers
        .iter()
        .enumerate()
        .map(|(i, (name, _))| (*name, i))
        .collect::<HashMap<_, _>>();
    // Intern fixed recipe bytes once. Refinement then hashes only partition numbers.
    let mut templates = HashMap::default();
    let shapes = carriers
        .iter()
        .enumerate()
        .map(|(i, (_, recipe))| {
            let refs = references(recipe, &names);
            let mut key = Vec::with_capacity(recipe.len());
            let mut start = 0;
            for &(begin, end, _) in &refs {
                key.extend_from_slice(&((begin - start) as u64).to_le_bytes());
                key.extend_from_slice(&recipe[start..begin]);
                start = end;
            }
            key.extend_from_slice(&((recipe.len() - start) as u64).to_le_bytes());
            key.extend_from_slice(&recipe[start..]);
            let template = *templates.entry(key).or_insert(i);
            (
                template,
                refs.into_iter()
                    .map(|(_, _, target)| target)
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<Vec<_>>();
    drop(templates);
    let mut partitions = shapes
        .iter()
        .map(|(template, _)| *template)
        .collect::<Vec<_>>();
    loop {
        let mut groups = HashMap::default();
        let next = shapes
            .iter()
            .enumerate()
            .map(|(i, (template, refs))| {
                let key = (
                    *template,
                    refs.iter()
                        .map(|&target| partitions[target])
                        .collect::<Vec<_>>(),
                );
                *groups.entry(key).or_insert(i)
            })
            .collect::<Vec<_>>();
        if next == partitions {
            return next;
        }
        partitions = next;
    }
}

fn references(recipe: &[u8], names: &HashMap<&str, usize>) -> Vec<(usize, usize, usize)> {
    // Length prefixes separate field names from descriptors. Rewrite only proven class references.
    if !recipe.starts_with(b"carrier-v2;") {
        return Vec::new();
    }
    let mut result = Vec::new();
    let mut start = 0;
    while start < recipe.len() {
        let end = recipe[start..]
            .iter()
            .position(|&b| b == b';')
            .map_or(recipe.len(), |i| start + i);
        let part = &recipe[start..end];
        if let Some(colon) = part.iter().position(|&b| b == b':') {
            if let Some(length) = std::str::from_utf8(&part[..colon])
                .ok()
                .and_then(|n| n.parse::<usize>().ok())
            {
                if let Some(descriptor) = (colon + 1)
                    .checked_add(length)
                    .and_then(|offset| part.get(offset..))
                {
                    let scalar = descriptor
                        .iter()
                        .position(|&b| b != b'[')
                        .unwrap_or(descriptor.len());
                    if let Some(name) = descriptor.get(scalar..).and_then(|d| d.strip_prefix(b"L"))
                    {
                        if let Some(&target) =
                            std::str::from_utf8(name).ok().and_then(|n| names.get(n))
                        {
                            result.push((end - name.len(), end, target));
                        }
                    }
                }
            }
        }
        start = end + 1;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_and_recursive_storage_shares_but_nominal_boundaries_do_not() {
        let carriers: Vec<(&str, &[u8])> = vec![
            ("a/Leaf", b"carrier-v2;eq;scalar;1:xI;1:xI;"),
            ("b/Leaf", b"carrier-v2;eq;scalar;1:xI;1:xI;"),
            (
                "a/Outer",
                b"carrier-v2;eq;object:Class;1:xLa/Leaf;;1:xLa/Leaf;;",
            ),
            (
                "b/Outer",
                b"carrier-v2;eq;object:Class;1:xLb/Leaf;;1:xLb/Leaf;;",
            ),
            (
                "a/Cycle",
                b"carrier-v2;eq;object:Class;1:xLa/Cycle;;1:xLa/Cycle;;",
            ),
            (
                "b/Cycle",
                b"carrier-v2;eq;object:Class;1:xLb/Cycle;;1:xLb/Cycle;;",
            ),
            (
                "a/Public",
                b"carrier-v2;eq;object:Class;1:xLpublic/A;;1:xLpublic/A;;",
            ),
            (
                "b/Public",
                b"carrier-v2;eq;object:Class;1:xLpublic/B;;1:xLpublic/B;;",
            ),
            ("a/OtherField", b"carrier-v2;eq;scalar;1:yI;1:yI;"),
            (
                "a/OtherDispatch",
                b"carrier-v2;eq;object:Enum;1:xLb/Leaf;;1:xLb/Leaf;;",
            ),
        ];
        assert_eq!(
            representatives(&carriers),
            vec![0, 0, 2, 2, 4, 4, 6, 7, 8, 9]
        );
        // A class identity difference must propagate through multiple containing layers.
        let carriers: Vec<(&str, &[u8])> = vec![
            ("a/Outer", b"carrier-v2;plain;1:xLa/Inner;;"),
            ("b/Outer", b"carrier-v2;plain;1:xLb/Inner;;"),
            ("a/Inner", b"carrier-v2;plain;1:xLpublic/A;;"),
            ("b/Inner", b"carrier-v2;plain;1:xLpublic/B;;"),
        ];
        assert_eq!(representatives(&carriers), vec![0, 1, 2, 3]);
    }

    #[test]
    fn callable_shapes_follow_storage_aliases_and_preserve_argument_order() {
        let carriers: Vec<(&str, &[u8])> = vec![
            ("a/Value", b"carrier-v2;plain;1:xI;"),
            ("b/Value", b"carrier-v2;plain;1:xI;"),
            (
                "a/Call",
                b"carrier-v2;function-v1;param;0:La/Value;;return;0:J;",
            ),
            (
                "b/Call",
                b"carrier-v2;function-v1;param;0:Lb/Value;;return;0:J;",
            ),
            (
                "a/Reverse",
                b"carrier-v2;function-v1;param;0:J;return;0:La/Value;;",
            ),
            (
                "a/Nominal",
                b"carrier-v2;function-v1;param;0:Lpublic/Value;;return;0:J;",
            ),
        ];
        assert_eq!(representatives(&carriers), vec![0, 0, 2, 2, 4, 5]);
    }
}
