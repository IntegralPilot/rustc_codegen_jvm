//! Low-overhead structural metrics for compiler performance work.
//!
//! Native profilers are better at measuring time.  This module records facts
//! they cannot recover from samples: work amplification between IR stages,
//! SSA and bytecode sizes, local allocation, and generated-class
//! duplication.  Metrics are accumulated in memory and written once per rustc
//! process when `RCGJ_METRICS_DIR` is set.

use crate::oomir::{self, DataType, DataTypeMethod};
use rustc_hash::FxHashMap as HashMap;
use serde::Serialize;
use std::{
    cell::RefCell,
    env,
    fs::File,
    io::{self, BufWriter},
    path::{Path, PathBuf},
    sync::{
        Mutex, OnceLock,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
};

const METRICS_DIR_ENV: &str = "RCGJ_METRICS_DIR";
const TOP_LIMIT: usize = 24;

struct Collector {
    directory: PathBuf,
    metrics: Mutex<CompilerMetrics>,
    type_cache_hits: AtomicU64,
    type_cache_misses: AtomicU64,
    finished: AtomicBool,
}

static COLLECTOR: OnceLock<Option<Collector>> = OnceLock::new();

fn collector() -> Option<&'static Collector> {
    COLLECTOR
        .get_or_init(|| {
            let directory = env::var_os(METRICS_DIR_ENV)?;
            (!directory.is_empty()).then(|| Collector {
                directory: PathBuf::from(directory),
                metrics: Mutex::new(CompilerMetrics::default()),
                type_cache_hits: AtomicU64::new(0),
                type_cache_misses: AtomicU64::new(0),
                finished: AtomicBool::new(false),
            })
        })
        .as_ref()
}

#[inline]
pub(crate) fn enabled() -> bool {
    collector().is_some()
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum ClassOrigin {
    Runtime,
    Module,
    DataTypeClass,
    DataTypeInterface,
}

impl ClassOrigin {
    const COUNT: usize = 4;

    fn index(self) -> usize {
        self as usize
    }

    fn name(self) -> &'static str {
        match self {
            Self::Runtime => "runtime",
            Self::Module => "module",
            Self::DataTypeClass => "data_type_class",
            Self::DataTypeInterface => "data_type_interface",
        }
    }

    fn all() -> [Self; Self::COUNT] {
        [
            Self::Runtime,
            Self::Module,
            Self::DataTypeClass,
            Self::DataTypeInterface,
        ]
    }
}

#[derive(Clone, Copy, Debug, Default, Serialize)]
struct OomirStats {
    functions: u64,
    ssa_functions: u64,
    basic_blocks: u64,
    instructions: u64,
    data_types: u64,
    data_type_methods: u64,
    statics: u64,
}

impl OomirStats {
    fn from_module(module: &oomir::Module) -> Self {
        let mut stats = Self {
            data_types: module.data_types.len() as u64,
            statics: module.statics.len() as u64,
            ..Self::default()
        };
        for function in module.functions.values() {
            stats.add_function(function);
        }
        for data_type in module.data_types.values() {
            match data_type {
                DataType::Class { methods, .. } | DataType::Interface { methods, .. } => {
                    stats.data_type_methods += methods.len() as u64;
                    for method in methods.values() {
                        if let DataTypeMethod::Function(function) = method {
                            stats.add_function(function);
                        }
                    }
                }
            }
        }
        stats
    }

    fn add_function(&mut self, function: &oomir::Function) {
        self.functions += 1;
        self.basic_blocks += function.body.basic_blocks.len() as u64;
        self.instructions += function
            .body
            .basic_blocks
            .values()
            .map(|block| block.instructions.len() as u64)
            .sum::<u64>();
    }

    fn merge(&mut self, other: Self) {
        self.functions += other.functions;
        self.ssa_functions += other.ssa_functions;
        self.basic_blocks += other.basic_blocks;
        self.instructions += other.instructions;
        self.data_types += other.data_types;
        self.data_type_methods += other.data_type_methods;
        self.statics += other.statics;
    }
}

#[derive(Clone, Copy, Debug, Default, Serialize)]
struct SelectionStats {
    methods: u64,
    ssa_instructions: u64,
    jvm_instructions: u64,
    locals: u64,
    max_method_instructions: u64,
    max_method_locals: u64,
}
impl SelectionStats {
    fn merge(&mut self, other: Self) {
        self.methods += other.methods;
        self.ssa_instructions += other.ssa_instructions;
        self.jvm_instructions += other.jvm_instructions;
        self.locals += other.locals;
        self.max_method_instructions = self
            .max_method_instructions
            .max(other.max_method_instructions);
        self.max_method_locals = self.max_method_locals.max(other.max_method_locals);
    }
}
#[derive(Clone, Debug, Serialize)]
struct MethodShape {
    item: String,
    shard: String,
    #[serde(flatten)]
    stats: SelectionStats,
}

#[derive(Clone, Debug, Serialize)]
struct ShardShape {
    shard: String,
    construction: OomirStats,
    sealed: OomirStats,
}

#[derive(Clone, Copy, Debug, Default, Serialize)]
struct ClassfileTotals {
    attempts: u64,
    attempted_bytes: u64,
    emitted_variants: u64,
    emitted_bytes: u64,
    exact_duplicates: u64,
    exact_duplicate_bytes: u64,
    name_collisions: u64,
}

impl ClassfileTotals {
    fn merge(&mut self, other: Self) {
        self.attempts += other.attempts;
        self.attempted_bytes += other.attempted_bytes;
        self.emitted_variants += other.emitted_variants;
        self.emitted_bytes += other.emitted_bytes;
        self.exact_duplicates += other.exact_duplicates;
        self.exact_duplicate_bytes += other.exact_duplicate_bytes;
        self.name_collisions += other.name_collisions;
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct ClassAmplification {
    attempts: u64,
    attempted_bytes: u64,
    emitted_variants: u64,
    exact_duplicates: u64,
}

#[derive(Clone, Debug, Serialize)]
struct NamedClassAmplification {
    class: String,
    attempts: u64,
    attempted_bytes: u64,
    emitted_variants: u64,
    exact_duplicates: u64,
}

#[derive(Debug)]
struct ShardMetrics {
    name: String,
    construction: OomirStats,
    sealed: Option<OomirStats>,
    selection: SelectionStats,
    top_methods: Vec<MethodShape>,
    data_type_definitions: HashMap<String, u64>,
    classfiles: [ClassfileTotals; ClassOrigin::COUNT],
    classes: HashMap<String, ClassAmplification>,
}

impl ShardMetrics {
    fn new(name: &str, module: &oomir::Module) -> Self {
        let data_type_definitions = module
            .data_types
            .keys()
            .map(|name| (name.clone(), 1))
            .collect();
        Self {
            name: name.to_string(),
            construction: OomirStats::from_module(module),
            sealed: None,
            selection: SelectionStats::default(),
            top_methods: Vec::new(),
            data_type_definitions,
            classfiles: [ClassfileTotals::default(); ClassOrigin::COUNT],
            classes: HashMap::default(),
        }
    }
}

thread_local! {
    static ACTIVE_SHARD: RefCell<Option<ShardMetrics>> = const { RefCell::new(None) };
}

pub(crate) struct ShardGuard {
    active: bool,
}

pub(crate) fn begin_shard(shard_name: &str, module: &oomir::Module) -> ShardGuard {
    if !enabled() {
        return ShardGuard { active: false };
    }
    ACTIVE_SHARD.with(|slot| {
        let previous = slot.replace(Some(ShardMetrics::new(shard_name, module)));
        assert!(
            previous.is_none(),
            "compiler metrics shards must not be nested"
        );
    });
    ShardGuard { active: true }
}

impl Drop for ShardGuard {
    fn drop(&mut self) {
        if !self.active {
            return;
        }
        let shard = ACTIVE_SHARD.with(|slot| slot.borrow_mut().take());
        let Some(shard) = shard else {
            return;
        };
        let Some(collector) = collector() else {
            return;
        };
        if let Ok(mut metrics) = collector.metrics.lock() {
            metrics.merge_shard(shard);
        }
    }
}

fn with_shard(action: impl FnOnce(&mut ShardMetrics)) {
    if !enabled() {
        return;
    }
    ACTIVE_SHARD.with(|slot| {
        if let Some(shard) = slot.borrow_mut().as_mut() {
            action(shard);
        }
    });
}

pub(crate) fn record_sealed_function(function: &oomir::SsaFunction) {
    with_shard(|shard| {
        let stats = shard.sealed.get_or_insert(OomirStats {
            data_types: shard.construction.data_types,
            data_type_methods: shard.construction.data_type_methods,
            statics: shard.construction.statics,
            ..Default::default()
        });
        stats.functions += 1;
        stats.ssa_functions += 1;
        stats.basic_blocks += function.body.ir.blocks.len() as u64;
        stats.instructions += function.body.ir.instructions.len() as u64;
    });
}

pub(crate) fn record_selection_method(
    body: &oomir::SsaBody,
    code: &jvm_compiler_core::jvm::MethodCode,
    item: impl FnOnce() -> String,
) {
    with_shard(|shard| {
        let stats = SelectionStats {
            methods: 1,
            ssa_instructions: body.ir.instructions.len() as u64,
            jvm_instructions: code.instructions.len() as u64,
            locals: code.max_locals as u64,
            max_method_instructions: code.instructions.len() as u64,
            max_method_locals: code.max_locals as u64,
        };
        if shard.top_methods.len() < TOP_LIMIT
            || shard
                .top_methods
                .last()
                .is_some_and(|m| stats.ssa_instructions > m.stats.ssa_instructions)
        {
            shard.top_methods.push(MethodShape {
                item: item(),
                shard: shard.name.clone(),
                stats,
            });
            shard
                .top_methods
                .sort_by_key(|method| std::cmp::Reverse(method.stats.ssa_instructions));
            shard.top_methods.truncate(TOP_LIMIT);
        }
        shard.selection.merge(stats);
    });
}

pub(crate) fn record_classfile_attempt(class_name: &str, origin: ClassOrigin, bytes: usize) {
    with_shard(|shard| {
        let totals = &mut shard.classfiles[origin.index()];
        totals.attempts += 1;
        totals.attempted_bytes += bytes as u64;
        let class = shard.classes.entry(class_name.to_string()).or_default();
        class.attempts += 1;
        class.attempted_bytes += bytes as u64;
    });
}

pub(crate) fn record_classfile_emitted(
    class_name: &str,
    origin: ClassOrigin,
    bytes: usize,
    name_collision: bool,
) {
    with_shard(|shard| {
        let totals = &mut shard.classfiles[origin.index()];
        totals.emitted_variants += 1;
        totals.emitted_bytes += bytes as u64;
        totals.name_collisions += u64::from(name_collision);
        shard
            .classes
            .entry(class_name.to_string())
            .or_default()
            .emitted_variants += 1;
    });
}

pub(crate) fn record_classfile_exact_duplicate(
    class_name: &str,
    origin: ClassOrigin,
    bytes: usize,
) {
    with_shard(|shard| {
        let totals = &mut shard.classfiles[origin.index()];
        totals.exact_duplicates += 1;
        totals.exact_duplicate_bytes += bytes as u64;
        shard
            .classes
            .entry(class_name.to_string())
            .or_default()
            .exact_duplicates += 1;
    });
}

#[inline]
pub(crate) fn record_type_cache_hit() {
    if let Some(collector) = collector() {
        collector.type_cache_hits.fetch_add(1, Ordering::Relaxed);
    }
}

#[inline]
pub(crate) fn record_type_cache_miss() {
    if let Some(collector) = collector() {
        collector.type_cache_misses.fetch_add(1, Ordering::Relaxed);
    }
}

#[derive(Debug, Default)]
struct CompilerMetrics {
    shards: u64,
    oomir_construction: OomirStats,
    oomir_sealed: OomirStats,
    selection: SelectionStats,
    top_shards: Vec<ShardShape>,
    top_methods: Vec<MethodShape>,
    data_type_definitions: HashMap<String, u64>,
    classfiles: [ClassfileTotals; ClassOrigin::COUNT],
    classes: HashMap<String, ClassAmplification>,
}

impl CompilerMetrics {
    fn merge_shard(&mut self, shard: ShardMetrics) {
        self.shards += 1;
        self.oomir_construction.merge(shard.construction);
        let after = shard.sealed.unwrap_or(shard.construction);
        self.oomir_sealed.merge(after);
        self.selection.merge(shard.selection);
        self.top_shards.push(ShardShape {
            shard: shard.name,
            construction: shard.construction,
            sealed: after,
        });
        self.top_shards.sort_by_key(|shard| {
            std::cmp::Reverse(shard.construction.instructions + shard.construction.data_types)
        });
        self.top_shards.truncate(TOP_LIMIT);
        self.top_methods.extend(shard.top_methods);
        self.top_methods
            .sort_by_key(|method| std::cmp::Reverse(method.stats.ssa_instructions));
        self.top_methods.truncate(TOP_LIMIT);
        for (name, attempts) in shard.data_type_definitions {
            *self.data_type_definitions.entry(name).or_default() += attempts;
        }
        for (total, shard) in self.classfiles.iter_mut().zip(shard.classfiles) {
            total.merge(shard);
        }
        for (name, shard_class) in shard.classes {
            let class = self.classes.entry(name).or_default();
            class.attempts += shard_class.attempts;
            class.attempted_bytes += shard_class.attempted_bytes;
            class.emitted_variants += shard_class.emitted_variants;
            class.exact_duplicates += shard_class.exact_duplicates;
        }
    }
}

#[derive(Serialize)]
struct NamedClassfileTotals {
    origin: &'static str,
    #[serde(flatten)]
    totals: ClassfileTotals,
}

#[derive(Serialize)]
struct RepeatedDataType {
    data_type: String,
    shards: u64,
}

#[derive(Serialize)]
struct TypeCacheMetrics {
    hits: u64,
    misses: u64,
}

#[derive(Serialize)]
struct MetricsOutput<'a> {
    schema_version: u8,
    kind: &'static str,
    crate_name: &'a str,
    pid: u32,
    shards: u64,
    oomir_construction: OomirStats,
    oomir_sealed: OomirStats,
    type_lowering_cache: TypeCacheMetrics,
    selection: SelectionStats,
    top_methods: &'a [MethodShape],
    classfiles_by_origin: Vec<NamedClassfileTotals>,
    repeated_data_types: Vec<RepeatedDataType>,
    top_classfile_amplification: Vec<NamedClassAmplification>,
    largest_shards: &'a [ShardShape],
}

fn safe_file_component(value: &str) -> String {
    value
        .chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() || matches!(character, '-' | '_') {
                character
            } else {
                '_'
            }
        })
        .collect()
}

fn write_output(
    directory: &Path,
    crate_name: &str,
    metrics: &CompilerMetrics,
    type_cache_hits: u64,
    type_cache_misses: u64,
) -> io::Result<PathBuf> {
    std::fs::create_dir_all(directory)?;
    let mut repeated_data_types = metrics
        .data_type_definitions
        .iter()
        .filter(|(_, attempts)| **attempts > 1)
        .map(|(name, attempts)| RepeatedDataType {
            data_type: name.clone(),
            shards: *attempts,
        })
        .collect::<Vec<_>>();
    repeated_data_types.sort_by_key(|item| std::cmp::Reverse(item.shards));
    repeated_data_types.truncate(TOP_LIMIT);

    let mut class_amplification = metrics
        .classes
        .iter()
        .filter(|(_, class)| class.attempts > 1 || class.exact_duplicates > 0)
        .map(|(name, class)| NamedClassAmplification {
            class: name.clone(),
            attempts: class.attempts,
            attempted_bytes: class.attempted_bytes,
            emitted_variants: class.emitted_variants,
            exact_duplicates: class.exact_duplicates,
        })
        .collect::<Vec<_>>();
    class_amplification.sort_by_key(|item| {
        std::cmp::Reverse((item.exact_duplicates, item.attempted_bytes, item.attempts))
    });
    class_amplification.truncate(TOP_LIMIT);

    let output = MetricsOutput {
        schema_version: 3,
        kind: "compiler_work_metrics",
        crate_name,
        pid: std::process::id(),
        shards: metrics.shards,
        oomir_construction: metrics.oomir_construction,
        oomir_sealed: metrics.oomir_sealed,
        type_lowering_cache: TypeCacheMetrics {
            hits: type_cache_hits,
            misses: type_cache_misses,
        },
        selection: metrics.selection,
        top_methods: &metrics.top_methods,
        classfiles_by_origin: ClassOrigin::all()
            .into_iter()
            .map(|origin| NamedClassfileTotals {
                origin: origin.name(),
                totals: metrics.classfiles[origin.index()],
            })
            .collect(),
        repeated_data_types,
        top_classfile_amplification: class_amplification,
        largest_shards: &metrics.top_shards,
    };
    let path = directory.join(format!(
        "{}-compiler-{}.json",
        std::process::id(),
        safe_file_component(crate_name)
    ));
    let mut writer = BufWriter::new(File::create(&path)?);
    serde_json::to_writer_pretty(&mut writer, &output)?;
    Ok(path)
}

pub(crate) fn finish_crate(crate_name: &str) -> io::Result<Option<PathBuf>> {
    let Some(collector) = collector() else {
        return Ok(None);
    };
    if collector.finished.swap(true, Ordering::AcqRel) {
        return Ok(None);
    }
    let metrics = collector
        .metrics
        .lock()
        .map_err(|_| io::Error::other("compiler metrics lock was poisoned"))?;
    write_output(
        &collector.directory,
        crate_name,
        &metrics,
        collector.type_cache_hits.load(Ordering::Relaxed),
        collector.type_cache_misses.load(Ordering::Relaxed),
    )
    .map(Some)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_components_do_not_create_paths() {
        assert_eq!(safe_file_component("core/test:crate"), "core_test_crate");
    }
}
