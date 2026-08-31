#!/usr/bin/env python3
"""Build tests with structural compiler metrics enabled.

This deliberately does not time compilation.  Use a native profiler or an
external benchmark for time; this report explains how much compiler work was
created and where it was amplified or discarded.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import asdict, dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from test_harness import (
    ROOT,
    TARGET_SPEC,
    TEST_TARGET_DIR,
    TestCase,
    build_test,
    cargo_jobs,
    discover_tests,
    prepare_shared_cache,
    prime_core,
    resolve_workers,
    run_command,
)


@dataclass
class Result:
    test_name: str
    test_type: str
    mode: str
    success: bool
    metrics_dir: str
    summary: dict[str, Any] = field(default_factory=dict)
    notes: list[str] = field(default_factory=list)


def bootstrap() -> None:
    subprocess.run([sys.executable, "build.py", "all"], cwd=ROOT, check=True)


def comma_set(value: str | None) -> set[str] | None:
    return None if value is None else {item.strip() for item in value.split(",") if item.strip()}


def clean_test_artifact(test: TestCase, release: bool) -> subprocess.CompletedProcess[str]:
    command = [
        "cargo",
        "clean",
        "--manifest-path",
        str(test.directory / "Cargo.toml"),
        "--target-dir",
        str(TEST_TARGET_DIR),
        "--target",
        str(TARGET_SPEC),
        "-Zjson-target-spec",
        "-p",
        test.package_name,
    ]
    if release:
        command.append("--release")
    return run_command(command)


def load_records(directory: Path) -> list[dict[str, Any]]:
    records = []
    for path in sorted(directory.glob("*.json")):
        try:
            records.append(json.loads(path.read_text(encoding="utf-8")))
        except (OSError, json.JSONDecodeError) as error:
            records.append({"kind": "parse_error", "path": str(path), "error": str(error)})
    return records


def add_fields(target: dict[str, int], source: dict[str, Any], fields: tuple[str, ...]) -> None:
    for name in fields:
        target[name] += int(source.get(name, 0))


def summarize(records: list[dict[str, Any]]) -> dict[str, Any]:
    compilers = [record for record in records if record.get("kind") == "compiler_work_metrics"]
    linkers = [record for record in records if record.get("kind") == "linker_work_metrics"]
    parse_errors = [record for record in records if record.get("kind") == "parse_error"]
    oomir_fields = (
        "functions",
        "basic_blocks",
        "instructions",
        "data_types",
        "data_type_methods",
        "statics",
    )
    oomir_before: dict[str, int] = defaultdict(int)
    oomir_after: dict[str, int] = defaultdict(int)
    optimise2: dict[str, int] = defaultdict(int)
    liveness: dict[str, int] = defaultdict(int)
    type_cache: dict[str, int] = defaultdict(int)
    classfiles: dict[str, int] = defaultdict(int)
    passes: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
    repeated_types: dict[str, int] = defaultdict(int)
    amplified_classes: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
    top_methods: list[dict[str, Any]] = []
    largest_shards: list[dict[str, Any]] = []

    for record in compilers:
        add_fields(oomir_before, record.get("oomir_before_optimise1", {}), oomir_fields)
        add_fields(oomir_after, record.get("oomir_after_optimise1", {}), oomir_fields)
        compiler_optimise2 = record.get("optimise2", {})
        add_fields(
            optimise2,
            compiler_optimise2,
            (
                "methods",
                "input_instructions",
                "output_instructions",
                "input_max_locals",
                "output_max_locals",
            ),
        )
        add_fields(
            liveness,
            compiler_optimise2.get("liveness", {}),
            (
                "analyses",
                "instructions",
                "locals",
                "matrix_words",
                "successor_edges",
                "worklist_pops",
            ),
        )
        add_fields(type_cache, record.get("type_lowering_cache", {}), ("hits", "misses"))
        for origin in record.get("classfiles_by_origin", []):
            add_fields(
                classfiles,
                origin,
                (
                    "attempts",
                    "attempted_bytes",
                    "emitted_variants",
                    "emitted_bytes",
                    "exact_duplicates",
                    "exact_duplicate_bytes",
                    "name_collisions",
                ),
            )
        for item in compiler_optimise2.get("passes", []):
            add_fields(
                passes[item.get("pass", "<unknown>")],
                item,
                (
                    "invocations",
                    "input_instructions",
                    "output_instructions",
                    "instructions_removed",
                    "instructions_added",
                    "length_changing_invocations",
                ),
            )
        for item in compiler_optimise2.get("top_methods_by_structural_work", []):
            top_methods.append({"crate": record.get("crate_name", "<unknown>"), **item})
        for item in record.get("largest_shards", []):
            largest_shards.append({"crate": record.get("crate_name", "<unknown>"), **item})
        for item in record.get("repeated_data_types", []):
            repeated_types[item.get("data_type", "<unknown>")] += int(item.get("shards", 0))
        for item in record.get("top_classfile_amplification", []):
            add_fields(
                amplified_classes[item.get("class", "<unknown>")],
                item,
                ("attempts", "attempted_bytes", "emitted_variants", "exact_duplicates"),
            )

    linker: dict[str, int] = defaultdict(int)
    linker_duplicates: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
    for record in linkers:
        add_fields(
            linker,
            record,
            (
                "input_fragments",
                "input_fragment_bytes",
                "unique_class_names",
                "duplicate_class_names",
                "duplicate_fragments",
                "merged_classes",
                "merged_class_bytes",
                "library_jars",
                "library_jar_bytes",
                "output_jar_bytes",
            ),
        )
        for item in record.get("top_duplicate_classes", []):
            add_fields(
                linker_duplicates[item.get("class", "<unknown>")],
                item,
                ("fragments", "input_bytes"),
            )

    return {
        "compiler_processes": len(compilers),
        "linker_processes": len(linkers),
        "parse_errors": parse_errors,
        "oomir_before_optimise1": dict(oomir_before),
        "oomir_after_optimise1": dict(oomir_after),
        "type_lowering_cache": dict(type_cache),
        "optimise2": dict(optimise2),
        "liveness": dict(liveness),
        "classfiles": dict(classfiles),
        "passes": {name: dict(values) for name, values in passes.items()},
        "repeated_data_types": sorted(
            repeated_types.items(), key=lambda item: item[1], reverse=True
        )[:20],
        "amplified_classes": sorted(
            ((name, dict(values)) for name, values in amplified_classes.items()),
            key=lambda item: (
                item[1].get("exact_duplicates", 0),
                item[1].get("attempted_bytes", 0),
            ),
            reverse=True,
        )[:20],
        "top_methods": sorted(
            top_methods, key=lambda item: item.get("work_units", 0), reverse=True
        )[:20],
        "largest_shards": sorted(
            largest_shards,
            key=lambda item: (
                item.get("before_optimise1", {}).get("instructions", 0),
                item.get("before_optimise1", {}).get("data_types", 0),
            ),
            reverse=True,
        )[:20],
        "linker": dict(linker),
        "linker_duplicates": sorted(
            ((name, dict(values)) for name, values in linker_duplicates.items()),
            key=lambda item: (
                item[1].get("fragments", 0),
                item[1].get("input_bytes", 0),
            ),
            reverse=True,
        )[:20],
    }


def format_result(result: Result, top: int) -> str:
    summary = result.summary
    lines = [
        f"{result.test_name} [{result.test_type}, {result.mode}]: "
        f"{'PASS' if result.success else 'FAIL'}",
        f"  records: {summary.get('compiler_processes', 0)} compiler, "
        f"{summary.get('linker_processes', 0)} linker",
    ]
    before = summary.get("oomir_before_optimise1", {})
    after = summary.get("oomir_after_optimise1", {})
    lines.append(
        "  OOMIR: "
        f"{before.get('instructions', 0):,} -> {after.get('instructions', 0):,} instructions; "
        f"{before.get('data_types', 0):,} shard-local data-type definitions"
    )
    optimise2 = summary.get("optimise2", {})
    lines.append(
        "  optimise2: "
        f"{optimise2.get('methods', 0):,} methods, "
        f"{optimise2.get('input_instructions', 0):,} -> "
        f"{optimise2.get('output_instructions', 0):,} bytecode instructions"
    )
    liveness = summary.get("liveness", {})
    lines.append(
        "  liveness: "
        f"{liveness.get('analyses', 0):,} analyses, "
        f"{liveness.get('matrix_words', 0):,} matrix words allocated, "
        f"{liveness.get('worklist_pops', 0):,} worklist pops"
    )
    cache = summary.get("type_lowering_cache", {})
    hits = cache.get("hits", 0)
    misses = cache.get("misses", 0)
    rate = hits / (hits + misses) if hits + misses else 0.0
    lines.append(f"  type cache: {hits:,} hits / {misses:,} misses ({rate:.1%} hit rate)")
    classes = summary.get("classfiles", {})
    lines.append(
        "  classfiles: "
        f"{classes.get('attempts', 0):,} built, "
        f"{classes.get('emitted_variants', 0):,} emitted, "
        f"{classes.get('exact_duplicates', 0):,} exact duplicates discarded "
        f"({classes.get('exact_duplicate_bytes', 0):,} generated bytes)"
    )
    hottest_passes = sorted(
        summary.get("passes", {}).items(),
        key=lambda item: item[1].get("input_instructions", 0),
        reverse=True,
    )[:top]
    lines.append("  largest optimise2 pass inputs:")
    for name, values in hottest_passes:
        lines.append(
            f"    {name}: received {values.get('input_instructions', 0):,}, "
            f"removed {values.get('instructions_removed', 0):,}"
        )
    repeated = summary.get("repeated_data_types", [])[:top]
    if repeated:
        lines.append("  most repeated shard-local data types:")
        lines.extend(f"    {name}: {count:,} shards" for name, count in repeated)
    amplified = summary.get("amplified_classes", [])[:top]
    if amplified:
        lines.append("  largest classfile amplification:")
        lines.extend(
            f"    {name}: {values.get('attempts', 0):,} builds, "
            f"{values.get('exact_duplicates', 0):,} exact duplicates"
            for name, values in amplified
        )
    methods = summary.get("top_methods", [])[:top]
    if methods:
        lines.append("  highest optimise2 structural work:")
        lines.extend(
            f"    {item.get('crate', '<unknown>')}::{item.get('item', '<unknown>')}: "
            f"{item.get('work_units', 0):,} work units, "
            f"{item.get('input_instructions', 0):,} input instructions"
            for item in methods
        )
    shards = summary.get("largest_shards", [])[:top]
    if shards:
        lines.append("  largest OOMIR shards:")
        lines.extend(
            f"    {item.get('crate', '<unknown>')}::{item.get('shard', '<unknown>')}: "
            f"{item.get('before_optimise1', {}).get('instructions', 0):,} instructions, "
            f"{item.get('before_optimise1', {}).get('data_types', 0):,} data types"
            for item in shards
        )
    linker = summary.get("linker", {})
    if summary.get("linker_processes", 0):
        lines.append(
            "  linker: "
            f"{linker.get('input_fragments', 0):,} fragments / "
            f"{linker.get('unique_class_names', 0):,} class names, "
            f"{linker.get('duplicate_fragments', 0):,} fragments merged away, "
            f"{linker.get('input_fragment_bytes', 0):,} -> "
            f"{linker.get('merged_class_bytes', 0):,} class bytes"
        )
        duplicates = summary.get("linker_duplicates", [])[:top]
        if duplicates:
            lines.append("  largest linker fragment amplification:")
            lines.extend(
                f"    {name}: {values.get('fragments', 0):,} fragments, "
                f"{values.get('input_bytes', 0):,} input bytes"
                for name, values in duplicates
            )
    lines.extend(f"  note: {note.strip()}" for note in result.notes if note.strip())
    return "\n".join(lines)


def collect_test(
    test: TestCase,
    release: bool,
    out_dir: Path,
    build_jobs: int,
) -> Result:
    mode = "release" if release else "debug"
    metrics_dir = out_dir / f"{test.kind}-{test.name}-{mode}"
    metrics_dir.mkdir(parents=True, exist_ok=True)
    for path in metrics_dir.glob("*.json"):
        path.unlink()
    clean = clean_test_artifact(test, release)
    if clean.returncode != 0:
        return Result(
            test.name,
            test.kind,
            mode,
            False,
            str(metrics_dir),
            notes=[f"cargo clean failed: {clean.stderr}"],
        )
    environment = os.environ.copy()
    environment["RCGJ_METRICS_DIR"] = str(metrics_dir)
    build = build_test(test, release, build_jobs, env=environment)
    records = load_records(metrics_dir)
    result = Result(
        test.name,
        test.kind,
        mode,
        build.returncode == 0,
        str(metrics_dir),
        summarize(records),
    )
    if build.returncode != 0:
        result.notes.append(build.stderr)
    if not records and build.returncode == 0:
        result.success = False
        result.notes.append("build produced no metrics records")
    return result


def main() -> int:
    parser = argparse.ArgumentParser(description="Collect JVM backend structural metrics")
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--release", action="store_true")
    parser.add_argument("--only-run")
    parser.add_argument("--dont-run")
    parser.add_argument("-j", "--jobs", type=int)
    parser.add_argument("--top", type=int, default=8)
    parser.add_argument("--out-dir", type=Path)
    parser.add_argument("--skip-bootstrap", action="store_true")
    args = parser.parse_args()

    try:
        workers = resolve_workers(args.jobs)
    except ValueError as error:
        parser.error(str(error))
    modes = []
    if args.debug:
        modes.append(False)
    if args.release:
        modes.append(True)
    if not modes:
        modes = [False, True]
    tests = discover_tests(comma_set(args.only_run), comma_set(args.dont_run) or set())
    if not tests:
        print("No tests matched the filters.")
        return 0
    if not args.skip_bootstrap:
        bootstrap()
    prepare_shared_cache()
    out_dir = args.out_dir or (
        ROOT / ".generated" / "metrics" / datetime.now().strftime("%Y%m%d-%H%M%S")
    )
    if not out_dir.is_absolute():
        out_dir = ROOT / out_dir
    out_dir.mkdir(parents=True, exist_ok=True)

    results = []
    for release in modes:
        mode = "release" if release else "debug"
        print(f"Building shared {mode} core/compiler_builtins cache...")
        prime = prime_core(release)
        if prime.returncode != 0:
            print(prime.stderr, file=sys.stderr)
            return 1
        with ThreadPoolExecutor(max_workers=workers) as executor:
            futures = [
                executor.submit(
                    collect_test,
                    test,
                    release,
                    out_dir,
                    cargo_jobs(workers),
                )
                for test in tests
            ]
            for future in as_completed(futures):
                result = future.result()
                results.append(result)
                print(format_result(result, args.top), end="\n\n")

    report = "\n\n".join(format_result(result, args.top) for result in results) + "\n"
    (out_dir / "report.txt").write_text(report, encoding="utf-8")
    (out_dir / "report.json").write_text(
        json.dumps([asdict(result) for result in results], indent=2), encoding="utf-8"
    )
    print(f"Reports written to {out_dir}")
    return 0 if all(result.success for result in results) else 1


if __name__ == "__main__":
    raise SystemExit(main())
