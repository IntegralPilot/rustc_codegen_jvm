//! Pack private stateless classes within fixed limits. Prefer classes that call each other.
use crate::*;

const MAX_BYTES: usize = 2 * 1024 * 1024;
const MAX_METHODS: usize = 2048;
const MAX_OWNERS: usize = 256;

#[derive(Default)]
pub(crate) struct Candidate {
    pub(crate) methods: Vec<(u32, u32)>,
    pub(crate) neighbors: Vec<String>,
    pub(crate) bytes: usize,
}

pub(crate) struct Unit {
    pub(crate) groups: Vec<usize>,
    /// Input bytes include duplicate and dead fragments. Output size excludes them.
    pub(crate) bytes: usize,
}

#[derive(Default)]
pub(crate) struct Plan {
    pub(crate) units: Vec<Unit>,
    pub(crate) names: HashMap<String, String>,
    pub(crate) shared_carriers: usize,
}

fn owner(group: &inputs::Group) -> &str {
    group.name.trim_end_matches(".class")
}

fn package(name: &str) -> &str {
    name.rsplit_once('/').map_or("", |(p, _)| p)
}

impl Plan {
    pub(crate) fn build(index: &mut inputs::Index) -> Self {
        let candidates = std::mem::take(&mut index.packable);
        let positions = index
            .groups
            .iter()
            .enumerate()
            .map(|(i, group)| (owner(group), i))
            .collect::<HashMap<_, _>>();
        let mut order = (0..index.groups.len()).collect::<Vec<_>>();
        order.sort_unstable_by(|&a, &b| index.groups[a].name.cmp(&index.groups[b].name));
        let mut seen = vec![false; index.groups.len()];
        let mut plan = Self::default();
        if index.share_carriers {
            let carriers = order
                .iter()
                .copied()
                .filter(|&i| index.groups[i].carrier.is_some())
                .collect::<Vec<_>>();
            let recipes = carriers
                .iter()
                .map(|&i| {
                    let group = &index.groups[i];
                    (owner(group), group.carrier.as_deref().unwrap())
                })
                .collect::<Vec<_>>();
            let representatives =
                crate::carriers::live_representatives(&recipes, &index.dead_methods);
            for (i, &position) in carriers.iter().enumerate() {
                let group = &index.groups[position];
                let name = owner(group);
                let representative = owner(&index.groups[carriers[representatives[i]]]);
                if representative != name {
                    plan.names.insert(name.into(), representative.into());
                    // Keep each alias's live helpers. Retain all overloads because aliased field types can change descriptors.
                    let dead = index
                        .dead_methods
                        .get(name)
                        .map(|methods| {
                            methods
                                .iter()
                                .map(|(name, _)| name.clone())
                                .collect::<HashSet<_>>()
                        })
                        .unwrap_or_default();
                    if let Some(methods) = index.dead_methods.get_mut(representative) {
                        methods.retain(|(method, _)| dead.contains(method));
                    }
                    plan.shared_carriers += 1;
                    seen[position] = true;
                }
            }
            if index.compact_carriers {
                let short = carriers
                    .iter()
                    .enumerate()
                    .filter(|(i, _)| representatives[*i] == *i)
                    .enumerate()
                    .map(|(i, (_, &position))| {
                        (
                            owner(&index.groups[position]),
                            // Pointer and FunctionPointers identify callable carriers by this prefix.
                            if owner(&index.groups[position])
                                .starts_with("org/rustlang/runtime/FnPtr_")
                            {
                                owner(&index.groups[position]).to_owned()
                            } else {
                                format!("org/rustlang/shape/S{i:x}")
                            },
                        )
                    })
                    .collect::<HashMap<_, _>>();
                for &position in &carriers {
                    let name = owner(&index.groups[position]);
                    let representative = plan.names.get(name).map_or(name, String::as_str);
                    plan.names
                        .insert(name.into(), short[representative].clone());
                }
            }
        }
        let mut targets = HashSet::default();
        for (position, &start) in order.iter().enumerate() {
            if seen[start] {
                continue;
            }
            seen[start] = true;
            let source = &index.groups[start];
            let name = owner(source);
            let mut unit = Unit {
                groups: vec![start],
                bytes: source.bytes,
            };
            let Some(candidate) = candidates
                .get(name)
                .filter(|c| c.bytes < MAX_BYTES && c.methods.len() < MAX_METHODS)
            else {
                plan.units.push(unit);
                continue;
            };
            let mut methods = candidate.methods.iter().copied().collect::<HashSet<_>>();
            let mut emitted_bytes = candidate.bytes;
            let mut pending = candidate
                .neighbors
                .iter()
                .filter_map(|name| positions.get(name.as_str()).copied())
                .collect::<Vec<_>>();
            // Limit each search to nearby classes in the same package.
            pending.extend(order.iter().skip(position + 1).take(MAX_OWNERS).copied());
            for next in pending {
                if unit.groups.len() == MAX_OWNERS {
                    break;
                }
                if seen[next] {
                    continue;
                }
                let group = &index.groups[next];
                let target = owner(group);
                let Some(candidate) = candidates.get(target) else {
                    continue;
                };
                if package(name) != package(target)
                    || emitted_bytes.saturating_add(candidate.bytes) > MAX_BYTES
                    || methods.len() + candidate.methods.len() > MAX_METHODS
                    || candidate.methods.iter().any(|m| methods.contains(m))
                {
                    continue;
                }
                methods.extend(candidate.methods.iter().copied());
                unit.groups.push(next);
                unit.bytes += group.bytes;
                emitted_bytes += candidate.bytes;
                seen[next] = true;
            }
            if unit.groups.len() > 1 {
                unit.groups
                    .sort_unstable_by(|&a, &b| index.groups[a].name.cmp(&index.groups[b].name));
                let mut hash = 0xcbf29ce484222325u64;
                for &i in &unit.groups {
                    for byte in owner(&index.groups[i]).bytes().chain([0]) {
                        hash = (hash ^ u64::from(byte)).wrapping_mul(0x100000001b3);
                    }
                }
                let target = if jvm_compiler_core::classfile::names::codec_owner(name) {
                    format!("{}/Codecs_pack_{hash:016x}", package(name))
                } else {
                    format!("{}/MonoBucket_{hash:016x}", package(name))
                };
                if positions.contains_key(target.as_str()) || !targets.insert(target.clone()) {
                    return Self::unpacked(index);
                }
                for &i in &unit.groups {
                    plan.names
                        .insert(owner(&index.groups[i]).into(), target.clone());
                }
            }
            plan.units.push(unit);
        }
        crate::private_names::compact(index, &mut plan.names);
        plan
    }

    fn unpacked(index: &inputs::Index) -> Self {
        Self {
            units: index
                .groups
                .iter()
                .enumerate()
                .map(|(i, g)| Unit {
                    groups: vec![i],
                    bytes: g.bytes,
                })
                .collect(),
            names: HashMap::default(),
            shared_carriers: 0,
        }
    }
}
