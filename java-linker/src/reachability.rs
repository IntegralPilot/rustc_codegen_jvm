//! Trace class dependencies during input indexing. Remove only classes marked private by the compiler.
use crate::*;
use jvm_compiler_core::classfile::{JavaStr, names, summary};
use std::borrow::Cow;

fn text(bytes: &[u8]) -> Option<Cow<'_, str>> {
    match std::str::from_utf8(bytes) {
        Ok(text) => Some(Cow::Borrowed(text)),
        Err(_) => JavaStr::from_mutf8(bytes)
            .ok()
            .map(|text| Cow::Owned(text.to_rust_string())),
    }
}

#[derive(Default)]
struct Node {
    edges: Vec<u32>,
    root: bool,
    whole: bool,
    defined_method: bool,
    pinned: bool,
    public_method: bool,
    forward_seen: bool,
    forward: Option<u32>,
    bytes: usize,
}

#[derive(Default)]
pub(crate) struct Graph {
    names: HashMap<String, u32>,
    nodes: Vec<Node>,
    opaque: bool,
    symbols: HashMap<std::sync::Arc<str>, u32>,
    spellings: Vec<std::sync::Arc<str>>,
    methods: HashMap<(u32, u32, u32), u32>,
    reflective_names: HashMap<u32, u32>,
    fixed_names: HashSet<u32>,
}

#[derive(Default)]
pub(crate) struct Demands {
    pub(crate) share_carriers: bool,
    pub(crate) compact_carriers: bool,
    pub(crate) pinned_classes: HashSet<String>,
    pub(crate) classes: Vec<bool>,
    pub(crate) dead_methods: HashMap<String, HashSet<(JavaString, JavaString)>>,
    pub(crate) packable: HashMap<String, crate::packing::Candidate>,
    pub(crate) aliases: crate::aliases::Aliases,
    pub(crate) private_classes: HashSet<String>,
    pub(crate) occupied_packages: HashSet<String>,
    pub(crate) short_methods: HashMap<Vec<u8>, Vec<u8>>,
}

impl Graph {
    pub(crate) fn dependency(&mut self, dependency: summary::Dependency<'_>, edges: &mut Vec<u32>) {
        match dependency {
            summary::Dependency::Definition { .. } => {}
            summary::Dependency::FixedMemberName(bytes) => {
                if let Some(name) = text(bytes) {
                    let symbol = self.symbol(&name);
                    self.fixed_names.insert(symbol);
                }
            }
            summary::Dependency::String(bytes) => {
                if bytes.starts_with(names::LITERAL_STRING.as_bytes()) {
                    return;
                }
                self.references(bytes, edges);
                if let Some(value) = text(bytes) {
                    let value = value.strip_prefix(names::NAME_STRING).unwrap_or(&value);
                    self.reflective_name(value, edges);
                    self.codec_recipe(value, edges);
                }
            }
            summary::Dependency::Method(method) => {
                if let Some(id) = self.method(method) {
                    edges.push(id);
                }
            }
            summary::Dependency::Text(bytes) => {
                self.references(bytes, edges);
                // Annotation and debug metadata can require the original member name.
                if let Some(name) = text(bytes)
                    && crate::method_names::mangled(&name)
                {
                    let symbol = self.symbol(&name);
                    self.fixed_names.insert(symbol);
                }
            }
            summary::Dependency::Class(name) => {
                if let Some(name) = text(name) {
                    if name.starts_with('[') {
                        self.references(name.as_bytes(), edges);
                    } else {
                        edges.push(self.intern(&name));
                    }
                }
            }
        }
    }
    fn symbol(&mut self, name: &str) -> u32 {
        if let Some(&id) = self.symbols.get(name) {
            return id;
        }
        let id = self.spellings.len() as u32;
        let name: std::sync::Arc<str> = name.into();
        self.symbols.insert(name.clone(), id);
        self.spellings.push(name);
        id
    }
    fn method(&mut self, key: summary::MethodKey<'_>) -> Option<u32> {
        let owner = text(key.owner)?;
        let name = text(key.name)?;
        if !summary::method_owner(&owner) && !summary::enum_helper(&name) {
            let symbol = self.symbol(&name);
            self.fixed_names.insert(symbol);
            return None;
        }
        let descriptor = text(key.descriptor)?;
        let owner = self.intern(&owner);
        let key = (owner, self.symbol(&name), self.symbol(&descriptor));
        if let Some(&id) = self.methods.get(&key) {
            return Some(id);
        }
        let id = self.nodes.len() as u32;
        self.nodes.push(Node {
            edges: vec![owner],
            ..Node::default()
        });
        self.methods.insert(key, id);
        Some(id)
    }
    pub(crate) fn scan(
        &mut self,
        bytes: &[u8],
        library: bool,
    ) -> io::Result<(summary::Summary, u32)> {
        let mut edges = Vec::new();
        let mut previous = None;
        let mut source = None;
        let mut outgoing = Vec::new();
        fn finish_method(nodes: &mut [Node], source: Option<u32>, outgoing: &mut Vec<u32>) {
            if let Some(source) = source {
                let node = &mut nodes[source as usize];
                node.defined_method = true;
                // Combine repeated upstream method edges before they accumulate across input fragments.
                node.edges.append(outgoing);
                node.edges.sort_unstable();
                node.edges.dedup();
            }
        }
        let metadata = summary::read_demands(bytes, |scope, dependency| {
            if library {
                if let summary::Dependency::Class(name) = dependency {
                    if let Some(name) = text(name) {
                        let id = self.intern(&name);
                        self.nodes[id as usize].pinned = true;
                    }
                }
            }
            if scope != previous {
                finish_method(&mut self.nodes, source, &mut outgoing);
                source = scope.and_then(|m| self.method(m));
                previous = scope;
            }
            if let (
                Some(source),
                summary::Dependency::Definition {
                    public,
                    forward,
                    bytes,
                },
            ) = (source, dependency)
            {
                let target = forward.and_then(|method| self.method(method));
                let node = &mut self.nodes[source as usize];
                node.bytes = node.bytes.max(bytes);
                if node.forward_seen {
                    node.public_method &= public;
                    if node.forward != target {
                        node.forward = None;
                    }
                } else {
                    node.forward_seen = true;
                    node.public_method = public;
                    node.forward = target;
                }
            }
            if source.is_some() {
                self.dependency(dependency, &mut outgoing);
            } else {
                self.dependency(dependency, &mut edges);
            }
        })?;
        finish_method(&mut self.nodes, source, &mut outgoing);
        let id = self.record(&metadata, edges, library);
        Ok((metadata, id))
    }
    fn intern(&mut self, name: &str) -> u32 {
        if let Some(&id) = self.names.get(name) {
            return id;
        }
        let id = u32::try_from(self.nodes.len()).expect("too many JVM dependency names");
        self.names.insert(name.into(), id);
        self.nodes.push(Node::default());
        id
    }

    pub(crate) fn resource(&mut self, name: &str) -> u32 {
        self.intern(name)
    }

    pub(crate) fn references(&mut self, bytes: &[u8], edges: &mut Vec<u32>) {
        // Do not interpret user literals as compiler symbols. Unknown reflection disables pruning.
        if bytes.starts_with(names::LITERAL_STRING.as_bytes()) {
            return;
        }
        let Some(text) = text(bytes) else {
            return;
        };
        let text = text.strip_prefix(names::NAME_STRING).unwrap_or(&text);
        let resource = text.trim_start_matches('/');
        if jvm_compiler_core::classfile::resources::valid_name(resource) {
            edges.push(self.intern(resource));
            return;
        }
        let descriptor =
            text.starts_with(['(', '[']) || (text.starts_with('L') && text.ends_with(';'));
        for token in text.split(|c: char| {
            c.is_whitespace()
                || matches!(
                    c,
                    '(' | ')' | ';' | '[' | ']' | '<' | '>' | ':' | '#' | ',' | '\u{1}'
                )
        }) {
            if !token.contains('/') && !token.contains('.') {
                if descriptor
                    && let Some(name) = token
                        .trim_start_matches(['B', 'C', 'D', 'F', 'I', 'J', 'S', 'Z', 'V'])
                        .strip_prefix('L')
                {
                    edges.push(self.intern(name));
                }
                continue;
            }
            // Descriptors, codec recipes, and class names share this token grammar.
            let token = if token.contains('.') {
                std::borrow::Cow::Owned(token.replace('.', "/"))
            } else {
                std::borrow::Cow::Borrowed(token)
            };
            edges.push(self.intern(&token));
            // Skip primitive parameter types before a reference descriptor.
            let descriptor =
                token.trim_start_matches(['B', 'C', 'D', 'F', 'I', 'J', 'S', 'Z', 'V']);
            if let Some(class) = descriptor.strip_prefix('L') {
                edges.push(self.intern(class));
            }
        }
    }

    fn reflective_name(&mut self, name: &str, edges: &mut Vec<u32>) {
        // Function-pointer adapters use owner::method:descriptor strings. Preserve their method names.
        if let Some((owner, target)) = name.split_once("::")
            && let Some((method, descriptor)) = target.split_once(':')
        {
            let symbol = self.symbol(method);
            self.fixed_names.insert(symbol);
            if let Some(id) = self.method(summary::MethodKey {
                owner: owner.replace('.', "/").as_bytes(),
                name: method.as_bytes(),
                descriptor: descriptor.as_bytes(),
            }) {
                edges.push(id);
            }
        }
        // Runtime protocols can pass owner, method, and descriptor as separate unmarked strings.
        if name.is_empty()
            || name.starts_with(names::STRING_TAG)
            || name.contains(['/', '.', '(', ')', ';', '#', '[', '\0', ' ', '\n'])
        {
            return;
        }
        let symbol = self.symbol(name);
        let id = *self.reflective_names.entry(symbol).or_insert_with(|| {
            let id = self.nodes.len() as u32;
            self.nodes.push(Node::default());
            id
        });
        edges.push(id);
    }

    fn codec_recipe(&mut self, value: &str, edges: &mut Vec<u32>) {
        // The runtime resolves every hook, including hooks in nested codec recipes.
        for (_, key) in names::codec_recipes(value) {
            for prefix in ["e$", "d$", "a$", "w$", "b$", "s$", "c$"] {
                self.reflective_name(&format!("{prefix}{key}"), edges);
            }
        }
    }

    pub(crate) fn record(
        &mut self,
        summary: &summary::Summary,
        mut edges: Vec<u32>,
        library: bool,
    ) -> u32 {
        let id = self.intern(&summary.name);
        // Unknown reflection and native code can reach any generated class.
        self.opaque |=
            summary.opaque_reflection && !summary.name.starts_with("org/rustlang/runtime/");
        if library {
            for &edge in &edges {
                self.nodes[edge as usize].pinned = true;
            }
        }
        let node = &mut self.nodes[id as usize];
        node.root |= library || !summary.private || summary.has_main;
        node.whole |= library || !summary.method_demands;
        edges.extend_from_slice(&node.edges);
        edges.sort_unstable();
        edges.dedup();
        node.edges = edges;
        id
    }

    pub(crate) fn libraries(&mut self, libraries: &[String]) -> io::Result<()> {
        let mut bytes = Vec::new();
        for path in libraries {
            let mut jar = ZipArchive::new(BufReader::new(fs::File::open(path)?))?;
            for i in 0..jar.len() {
                let mut entry = jar.by_index(i)?;
                if !entry.name().ends_with(".class") {
                    continue;
                }
                bytes.clear();
                entry.read_to_end(&mut bytes)?;
                match self.scan(&bytes, true) {
                    Ok(_) => {}
                    // Preserve unreadable library classes through the opaque-JAR copy path.
                    Err(_) => self.opaque = true,
                }
            }
        }
        Ok(())
    }

    pub(crate) fn finish(mut self, executable: bool) -> Demands {
        if !executable || self.opaque {
            return Demands {
                classes: vec![true; self.nodes.len()],
                ..Demands::default()
            };
        }
        for (&(owner, _, _), &method) in &self.methods {
            if self.nodes[owner as usize].whole || self.nodes[owner as usize].pinned {
                self.nodes[owner as usize].edges.push(method);
            }
        }
        // Retain reflective methods only when their owner is live. Unknown reflection already disables pruning.
        let mut reflective = HashMap::<u32, Vec<(u32, u32)>>::default();
        for (&(owner, name, _), &method) in &self.methods {
            if let Some(&node) = self.reflective_names.get(&name) {
                reflective.entry(node).or_default().push((owner, method));
                reflective.entry(owner).or_default().push((node, method));
            }
        }
        for node in &mut self.nodes {
            node.edges.sort_unstable();
            node.edges.dedup();
        }
        let mut live = vec![false; self.nodes.len()];
        let mut pending = self
            .nodes
            .iter()
            .enumerate()
            .filter_map(|(i, n)| n.root.then_some(i as u32))
            .collect::<Vec<_>>();
        while let Some(id) = pending.pop() {
            if !std::mem::replace(&mut live[id as usize], true) {
                pending.extend_from_slice(&self.nodes[id as usize].edges);
                if let Some(methods) = reflective.get(&id) {
                    pending.extend(
                        methods
                            .iter()
                            .filter_map(|&(other, method)| live[other as usize].then_some(method)),
                    );
                }
            }
        }
        drop(reflective);
        let owners = self
            .names
            .into_iter()
            .map(|(name, id)| (id, name))
            .collect::<HashMap<_, _>>();
        // Reserve external package names even when their classes are unreachable.
        let compact_carriers = !owners.values().any(|name| {
            name.starts_with("org/rustlang/shape/")
                || name.strip_prefix("org").is_some_and(|suffix| {
                    suffix
                        .as_bytes()
                        .get(..names::CRATE_MARKER_LEN)
                        .is_some_and(names::is_crate_marker)
                        && suffix[names::CRATE_MARKER_LEN..].starts_with("/rustlang/shape/")
                })
        });
        let mut packable = HashMap::<String, crate::packing::Candidate>::default();
        let keys = self
            .methods
            .iter()
            .map(|(&key, &id)| (id, key))
            .collect::<HashMap<_, _>>();
        let private_public = |id: u32| {
            let Some(&(owner, _, _)) = keys.get(&id) else {
                return false;
            };
            let node = &self.nodes[owner as usize];
            self.nodes[id as usize].defined_method
                && self.nodes[id as usize].public_method
                && !node.whole
                && !node.root
                && !node.pinned
                && owners[&owner].contains("/mono/Mono_")
        };
        let removable =
            |id: u32| private_public(id) && !self.reflective_names.contains_key(&keys[&id].1);
        let mut alias_targets = HashMap::default();
        for &id in self.methods.values() {
            if !live[id as usize] || !removable(id) {
                continue;
            }
            let Some(mut target) = self.nodes[id as usize].forward else {
                continue;
            };
            for _ in 0..64 {
                if target == id || !private_public(target) {
                    break;
                }
                if removable(target)
                    && let Some(next) = self.nodes[target as usize].forward
                {
                    target = next;
                } else {
                    alias_targets.insert(id, target);
                    break;
                }
            }
        }
        let mut aliases = crate::aliases::Aliases::default();
        for (&id, &target) in &alias_targets {
            let (owner, name, descriptor) = keys[&id];
            let (target_owner, target_name, _) = keys[&target];
            aliases
                .entry(JavaString::from(owners[&owner].as_str()))
                .or_default()
                .insert(
                    (
                        JavaString::from(self.spellings[name as usize].as_ref()),
                        JavaString::from(self.spellings[descriptor as usize].as_ref()),
                    ),
                    (
                        JavaString::from(owners[&target_owner].as_str()),
                        JavaString::from(self.spellings[target_name as usize].as_ref()),
                    ),
                );
        }
        for (&(owner, name, descriptor), &id) in &self.methods {
            let node = &self.nodes[owner as usize];
            if !live[id as usize]
                || !self.nodes[id as usize].defined_method
                || node.whole
                || node.root
                || node.pinned
                || !summary::method_owner(&owners[&owner])
            {
                continue;
            }
            let candidate = packable.entry(owners[&owner].clone()).or_default();
            if !alias_targets.contains_key(&id) {
                candidate.methods.push((name, descriptor));
                candidate.bytes = candidate
                    .bytes
                    .saturating_add(self.nodes[id as usize].bytes);
            }
            for edge in &self.nodes[id as usize].edges {
                let target = keys.get(edge).map_or(edge, |key| &key.0);
                if *target != owner {
                    if let Some(name) = owners.get(target) {
                        if summary::method_owner(name) {
                            candidate.neighbors.push(name.clone());
                        }
                    }
                }
            }
        }
        for candidate in packable.values_mut() {
            candidate.neighbors.sort_unstable();
            candidate.neighbors.dedup();
        }
        let mut dead_methods = HashMap::<String, HashSet<_>>::default();
        let short_methods = crate::method_names::plan(
            self.methods.iter().map(|(&(owner, name, _), &id)| {
                let node = &self.nodes[owner as usize];
                (
                    name,
                    live[id as usize] && !alias_targets.contains_key(&id),
                    self.nodes[id as usize].defined_method
                        && !node.whole
                        && !node.root
                        && !node.pinned
                        && owners[&owner].contains("/mono/Mono_"),
                )
            }),
            &self.spellings,
            &self.fixed_names,
            &self.reflective_names,
            owners.values().map(String::as_str),
        );
        for ((owner, name, descriptor), id) in self.methods {
            if (!live[id as usize] || alias_targets.contains_key(&id))
                && self.nodes[id as usize].defined_method
            {
                dead_methods
                    .entry(owners[&owner].clone())
                    .or_default()
                    .insert((
                        JavaString::from(self.spellings[name as usize].as_ref()),
                        JavaString::from(self.spellings[descriptor as usize].as_ref()),
                    ));
            }
        }
        Demands {
            share_carriers: true,
            compact_carriers,
            pinned_classes: owners
                .iter()
                .filter_map(|(&id, name)| self.nodes[id as usize].pinned.then(|| name.clone()))
                .collect(),
            private_classes: owners
                .iter()
                .filter_map(|(&id, name)| {
                    let node = &self.nodes[id as usize];
                    (live[id as usize] && !node.root && !node.pinned).then(|| name.clone())
                })
                .collect(),
            occupied_packages: owners
                .values()
                .map(|name| {
                    name.split('/')
                        .next()
                        .unwrap()
                        .split(names::CRATE_MARKER)
                        .next()
                        .unwrap()
                        .to_owned()
                })
                .collect(),
            classes: live,
            dead_methods,
            packable,
            aliases,
            short_methods,
        }
    }
    #[cfg(test)]
    pub(crate) fn live(self, executable: bool) -> Vec<bool> {
        self.finish(executable).classes
    }
}
