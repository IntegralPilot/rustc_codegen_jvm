//! Low-overhead structural metrics for compiler performance work.
//!
//! Native profilers are better at measuring time.  This module records facts
//! they cannot recover from samples: work amplification between IR stages,
//! optimiser effectiveness, liveness problem sizes, and generated-class
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

#[derive(Clone, Copy, Debug)]
pub(crate) enum Optimise2Pass {
    BooleanMaterialisation,
    RedundantInstructionsBefore,
    ThreadJumpsBefore,
    BranchOverGotoBefore,
    UnreachableBefore,
    StoreLoadPairs,
    IincPatterns,
    NullComparisons,
    BooleanZeroComparisons,
    StackBooleanZeroComparisons,
    DeadDuplicateStores,
    ThreadJumpsAfter,
    BranchOverGotoAfter,
    UnreachableAfter,
    RedundantInstructionsAfter,
    RewriteLocals,
}

impl Optimise2Pass {
    const COUNT: usize = 16;

    fn index(self) -> usize {
        self as usize
    }

    fn name(self) -> &'static str {
        match self {
            Self::BooleanMaterialisation => "boolean_materialisation",
            Self::RedundantInstructionsBefore => "redundant_instructions_before",
            Self::ThreadJumpsBefore => "thread_jumps_before",
            Self::BranchOverGotoBefore => "branch_over_goto_before",
            Self::UnreachableBefore => "unreachable_before",
            Self::StoreLoadPairs => "store_load_pairs",
            Self::IincPatterns => "iinc_patterns",
            Self::NullComparisons => "null_comparisons",
            Self::BooleanZeroComparisons => "boolean_zero_comparisons",
            Self::StackBooleanZeroComparisons => "stack_boolean_zero_comparisons",
            Self::DeadDuplicateStores => "dead_duplicate_stores",
            Self::ThreadJumpsAfter => "thread_jumps_after",
            Self::BranchOverGotoAfter => "branch_over_goto_after",
            Self::UnreachableAfter => "unreachable_after",
            Self::RedundantInstructionsAfter => "redundant_instructions_after",
            Self::RewriteLocals => "rewrite_locals",
        }
    }

    fn all() -> [Self; Self::COUNT] {
        [
            Self::BooleanMaterialisation,
            Self::RedundantInstructionsBefore,
            Self::ThreadJumpsBefore,
            Self::BranchOverGotoBefore,
            Self::UnreachableBefore,
            Self::StoreLoadPairs,
            Self::IincPatterns,
            Self::NullComparisons,
            Self::BooleanZeroComparisons,
            Self::StackBooleanZeroComparisons,
            Self::DeadDuplicateStores,
            Self::ThreadJumpsAfter,
            Self::BranchOverGotoAfter,
            Self::UnreachableAfter,
            Self::RedundantInstructionsAfter,
            Self::RewriteLocals,
        ]
    }
}

#[derive(Clone, Copy, Debug, Default, Serialize)]
struct PassWork {
    invocations: u64,
    input_instructions: u64,
    output_instructions: u64,
    instructions_removed: u64,
    instructions_added: u64,
    length_changing_invocations: u64,
}

impl PassWork {
    fn observe(&mut self, before: usize, after: usize) {
        self.invocations += 1;
        self.input_instructions += before as u64;
        self.output_instructions += after as u64;
        self.instructions_removed += before.saturating_sub(after) as u64;
        self.instructions_added += after.saturating_sub(before) as u64;
        self.length_changing_invocations += u64::from(before != after);
    }

    fn merge(&mut self, other: Self) {
        self.invocations += other.invocations;
        self.input_instructions += other.input_instructions;
        self.output_instructions += other.output_instructions;
        self.instructions_removed += other.instructions_removed;
        self.instructions_added += other.instructions_added;
        self.length_changing_invocations += other.length_changing_invocations;
    }
}

#[derive(Clone, Copy, Debug, Default, Serialize)]
struct LivenessWork {
    analyses: u64,
    instructions: u64,
    locals: u64,
    matrix_words: u64,
    successor_edges: u64,
    worklist_pops: u64,
    max_matrix_words: u64,
}

impl LivenessWork {
    fn merge(&mut self, other: Self) {
        self.analyses += other.analyses;
        self.instructions += other.instructions;
        self.locals += other.locals;
        self.matrix_words += other.matrix_words;
        self.successor_edges += other.successor_edges;
        self.worklist_pops += other.worklist_pops;
        self.max_matrix_words = self.max_matrix_words.max(other.max_matrix_words);
    }
}

#[derive(Debug)]
pub(crate) struct Optimise2MethodMetrics {
    input_instructions: u64,
    output_instructions: u64,
    input_max_locals: u64,
    output_max_locals: u64,
    passes: [PassWork; Optimise2Pass::COUNT],
    liveness: LivenessWork,
}

impl Optimise2MethodMetrics {
    pub(crate) fn new(input_instructions: usize, input_max_locals: u16) -> Option<Self> {
        enabled().then(|| Self {
            input_instructions: input_instructions as u64,
            output_instructions: 0,
            input_max_locals: u64::from(input_max_locals),
            output_max_locals: 0,
            passes: [PassWork::default(); Optimise2Pass::COUNT],
            liveness: LivenessWork::default(),
        })
    }

    pub(crate) fn observe_pass(&mut self, pass: Optimise2Pass, before: usize, after: usize) {
        self.passes[pass.index()].observe(before, after);
    }

    pub(crate) fn record_liveness(
        &mut self,
        instructions: usize,
        locals: usize,
        matrix_words: usize,
        successor_edges: usize,
        worklist_pops: usize,
    ) {
        self.liveness.analyses += 1;
        self.liveness.instructions += instructions as u64;
        self.liveness.locals += locals as u64;
        self.liveness.matrix_words += matrix_words as u64;
        self.liveness.successor_edges += successor_edges as u64;
        self.liveness.worklist_pops += worklist_pops as u64;
        self.liveness.max_matrix_words = self.liveness.max_matrix_words.max(matrix_words as u64);
    }

    pub(crate) fn finish(&mut self, output_instructions: usize, output_max_locals: u16) {
        self.output_instructions = output_instructions as u64;
        self.output_max_locals = u64::from(output_max_locals);
    }

    fn work_units(&self) -> u64 {
        self.passes
            .iter()
            .map(|pass| pass.input_instructions)
            .sum::<u64>()
            .saturating_add(self.liveness.matrix_words)
            .saturating_add(self.liveness.worklist_pops)
    }
}

#[derive(Clone, Copy, Debug, Default, Serialize)]
struct OomirStats {
    functions: u64,
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
                DataType::Class { methods, .. } => {
                    stats.data_type_methods += methods.len() as u64;
                    for method in methods.values() {
                        if let DataTypeMethod::Function(function) = method {
                            stats.add_function(function);
                        }
                    }
                }
                DataType::Interface { methods, .. } => {
                    stats.data_type_methods += methods.len() as u64;
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
        self.basic_blocks += other.basic_blocks;
        self.instructions += other.instructions;
        self.data_types += other.data_types;
        self.data_type_methods += other.data_type_methods;
        self.statics += other.statics;
    }
}

#[derive(Debug, Default)]
struct Optimise2Summary {
    methods: u64,
    input_instructions: u64,
    output_instructions: u64,
    input_max_locals: u64,
    output_max_locals: u64,
    max_method_instructions: u64,
    max_method_locals: u64,
    passes: [PassWork; Optimise2Pass::COUNT],
    liveness: LivenessWork,
}

impl Optimise2Summary {
    fn merge_method(&mut self, metrics: &Optimise2MethodMetrics) {
        self.methods += 1;
        self.input_instructions += metrics.input_instructions;
        self.output_instructions += metrics.output_instructions;
        self.input_max_locals += metrics.input_max_locals;
        self.output_max_locals += metrics.output_max_locals;
        self.max_method_instructions = self.max_method_instructions.max(metrics.input_instructions);
        self.max_method_locals = self.max_method_locals.max(metrics.input_max_locals);
        for (total, method) in self.passes.iter_mut().zip(metrics.passes) {
            total.merge(method);
        }
        self.liveness.merge(metrics.liveness);
    }

    fn merge(&mut self, other: Self) {
        self.methods += other.methods;
        self.input_instructions += other.input_instructions;
        self.output_instructions += other.output_instructions;
        self.input_max_locals += other.input_max_locals;
        self.output_max_locals += other.output_max_locals;
        self.max_method_instructions = self
            .max_method_instructions
            .max(other.max_method_instructions);
        self.max_method_locals = self.max_method_locals.max(other.max_method_locals);
        for (total, shard) in self.passes.iter_mut().zip(other.passes) {
            total.merge(shard);
        }
        self.liveness.merge(other.liveness);
    }
}

#[derive(Clone, Debug, Serialize)]
struct MethodShape {
    item: String,
    shard: String,
    work_units: u64,
    input_instructions: u64,
    output_instructions: u64,
    input_max_locals: u64,
    output_max_locals: u64,
    liveness_analyses: u64,
    liveness_matrix_words: u64,
    liveness_worklist_pops: u64,
}

#[derive(Clone, Debug, Serialize)]
struct ShardShape {
    shard: String,
    before_optimise1: OomirStats,
    after_optimise1: OomirStats,
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
    before_optimise1: OomirStats,
    after_optimise1: Option<OomirStats>,
    optimise2: Optimise2Summary,
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
            before_optimise1: OomirStats::from_module(module),
            after_optimise1: None,
            optimise2: Optimise2Summary::default(),
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

pub(crate) fn record_oomir_after_optimise1(module: &oomir::Module) {
    with_shard(|shard| shard.after_optimise1 = Some(OomirStats::from_module(module)));
}

pub(crate) fn record_optimise2_method(
    metrics: Optimise2MethodMetrics,
    item: impl FnOnce() -> String,
) {
    with_shard(|shard| {
        let work_units = metrics.work_units();
        let qualifies = shard.top_methods.len() < TOP_LIMIT
            || shard
                .top_methods
                .last()
                .is_some_and(|smallest| work_units > smallest.work_units);
        if qualifies {
            shard.top_methods.push(MethodShape {
                item: item(),
                shard: shard.name.clone(),
                work_units,
                input_instructions: metrics.input_instructions,
                output_instructions: metrics.output_instructions,
                input_max_locals: metrics.input_max_locals,
                output_max_locals: metrics.output_max_locals,
                liveness_analyses: metrics.liveness.analyses,
                liveness_matrix_words: metrics.liveness.matrix_words,
                liveness_worklist_pops: metrics.liveness.worklist_pops,
            });
            shard
                .top_methods
                .sort_by_key(|method| std::cmp::Reverse(method.work_units));
            shard.top_methods.truncate(TOP_LIMIT);
        }
        shard.optimise2.merge_method(&metrics);
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
    oomir_before_optimise1: OomirStats,
    oomir_after_optimise1: OomirStats,
    optimise2: Optimise2Summary,
    top_shards: Vec<ShardShape>,
    top_methods: Vec<MethodShape>,
    data_type_definitions: HashMap<String, u64>,
    classfiles: [ClassfileTotals; ClassOrigin::COUNT],
    classes: HashMap<String, ClassAmplification>,
}

impl CompilerMetrics {
    fn merge_shard(&mut self, shard: ShardMetrics) {
        self.shards += 1;
        self.oomir_before_optimise1.merge(shard.before_optimise1);
        let after = shard.after_optimise1.unwrap_or(shard.before_optimise1);
        self.oomir_after_optimise1.merge(after);
        self.optimise2.merge(shard.optimise2);
        self.top_shards.push(ShardShape {
            shard: shard.name,
            before_optimise1: shard.before_optimise1,
            after_optimise1: after,
        });
        self.top_shards.sort_by_key(|shard| {
            std::cmp::Reverse(
                shard.before_optimise1.instructions + shard.before_optimise1.data_types,
            )
        });
        self.top_shards.truncate(TOP_LIMIT);
        self.top_methods.extend(shard.top_methods);
        self.top_methods
            .sort_by_key(|method| std::cmp::Reverse(method.work_units));
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
struct NamedPassWork {
    pass: &'static str,
    #[serde(flatten)]
    work: PassWork,
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
struct Optimise2Output<'a> {
    methods: u64,
    input_instructions: u64,
    output_instructions: u64,
    input_max_locals: u64,
    output_max_locals: u64,
    max_method_instructions: u64,
    max_method_locals: u64,
    passes: Vec<NamedPassWork>,
    liveness: LivenessWork,
    top_methods_by_structural_work: &'a [MethodShape],
}

#[derive(Serialize)]
struct MetricsOutput<'a> {
    schema_version: u8,
    kind: &'static str,
    crate_name: &'a str,
    pid: u32,
    shards: u64,
    oomir_before_optimise1: OomirStats,
    oomir_after_optimise1: OomirStats,
    type_lowering_cache: TypeCacheMetrics,
    optimise2: Optimise2Output<'a>,
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
        schema_version: 2,
        kind: "compiler_work_metrics",
        crate_name,
        pid: std::process::id(),
        shards: metrics.shards,
        oomir_before_optimise1: metrics.oomir_before_optimise1,
        oomir_after_optimise1: metrics.oomir_after_optimise1,
        type_lowering_cache: TypeCacheMetrics {
            hits: type_cache_hits,
            misses: type_cache_misses,
        },
        optimise2: Optimise2Output {
            methods: metrics.optimise2.methods,
            input_instructions: metrics.optimise2.input_instructions,
            output_instructions: metrics.optimise2.output_instructions,
            input_max_locals: metrics.optimise2.input_max_locals,
            output_max_locals: metrics.optimise2.output_max_locals,
            max_method_instructions: metrics.optimise2.max_method_instructions,
            max_method_locals: metrics.optimise2.max_method_locals,
            passes: Optimise2Pass::all()
                .into_iter()
                .map(|pass| NamedPassWork {
                    pass: pass.name(),
                    work: metrics.optimise2.passes[pass.index()],
                })
                .collect(),
            liveness: metrics.optimise2.liveness,
            top_methods_by_structural_work: &metrics.top_methods,
        },
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
    fn pass_work_separates_scanning_from_effectiveness() {
        let mut work = PassWork::default();
        work.observe(100, 90);
        work.observe(80, 80);
        assert_eq!(work.invocations, 2);
        assert_eq!(work.input_instructions, 180);
        assert_eq!(work.instructions_removed, 10);
        assert_eq!(work.length_changing_invocations, 1);
    }

    #[test]
    fn method_work_includes_pass_and_dataflow_sizes() {
        let mut metrics = Optimise2MethodMetrics {
            input_instructions: 100,
            output_instructions: 90,
            input_max_locals: 8,
            output_max_locals: 5,
            passes: [PassWork::default(); Optimise2Pass::COUNT],
            liveness: LivenessWork::default(),
        };
        metrics.observe_pass(Optimise2Pass::IincPatterns, 100, 97);
        metrics.record_liveness(97, 8, 194, 100, 120);
        assert_eq!(metrics.work_units(), 100 + 194 + 120);
    }

    #[test]
    fn file_components_do_not_create_paths() {
        assert_eq!(safe_file_component("core/test:crate"), "core_test_crate");
    }
}
