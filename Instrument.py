#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import time
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
    jar_path,
    prepare_shared_cache,
    prime_core,
    resolve_workers,
    run_command,
)

PHASES = ("lower1", "optimise1", "lower2")


@dataclass
class Result:
    test_name: str
    test_type: str
    mode: str
    success: bool
    build_seconds: float = 0.0
    java_seconds: float = 0.0
    frontend_seconds: float = 0.0
    phase_totals: dict[str, float] = field(default_factory=dict)
    function_totals: dict[str, dict[str, float]] = field(default_factory=dict)
    notes: list[str] = field(default_factory=list)
    event_path: str = ""
    parse_errors: int = 0


def bootstrap() -> None:
    subprocess.run([sys.executable, "build.py", "all"], cwd=ROOT, check=True)


def parse_events(path: Path) -> tuple[list[dict[str, Any]], int]:
    events = []
    errors = 0
    if not path.exists():
        return events, errors
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        if not line.strip():
            continue
        try:
            events.append(json.loads(line))
        except json.JSONDecodeError:
            errors += 1
    return events, errors


def summarize(events: list[dict[str, Any]], build_seconds: float):
    phases: dict[str, float] = defaultdict(float)
    functions: dict[str, dict[str, float]] = defaultdict(lambda: defaultdict(float))
    for event in events:
        stage = event.get("stage")
        if not stage:
            continue
        seconds = float(event.get("seconds") or 0.0)
        if event.get("kind") == "phase":
            phases[stage] += seconds
        elif event.get("kind") == "function":
            functions[stage][event.get("item") or "<unknown>"] += seconds
    backend = sum(phases.get(phase, 0.0) for phase in PHASES)
    frontend = max(build_seconds - backend - phases.get("java-linker", 0.0), 0.0)
    return dict(phases), {stage: dict(items) for stage, items in functions.items()}, frontend


def clean_test_artifact(
    test: TestCase, release: bool
) -> subprocess.CompletedProcess[str]:
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


def output_matches(proc, test: TestCase, release: bool) -> bool:
    code_path = test.directory / "java-returncode.expected"
    expected_code = int(code_path.read_text().strip()) if code_path.exists() else 0
    if proc.returncode != expected_code:
        return False
    output_path = test.directory / (
        "java-output.release.expected" if release else "java-output.expected"
    )
    if release and not output_path.exists():
        output_path = test.directory / "java-output.expected"
    if not output_path.exists():
        return True
    expected = "".join(output_path.read_text(encoding="utf-8").strip().split())
    actual = "".join(f"STDOUT:{proc.stdout.strip()}STDERR:{proc.stderr.strip()}".split())
    return actual == expected


def run_java(test: TestCase, jar: Path, release: bool):
    if test.kind == "integration":
        files = sorted(path.name for path in test.directory.glob("*.java"))
        classpath = os.pathsep.join([".", str(jar)])
        javac = run_command(["javac", "-cp", classpath, *files], cwd=test.directory)
        if javac.returncode != 0:
            return False, 0.0, f"javac failed: {javac.stderr}"
        command = ["java", "-cp", classpath, "Main"]
        cwd = test.directory
    else:
        command = ["java", "-cp", str(jar), f"{test.artifact_name}.{test.artifact_name}"]
        cwd = ROOT
    start = time.perf_counter()
    proc = run_command(command, cwd=cwd)
    elapsed = time.perf_counter() - start
    return output_matches(proc, test, release), elapsed, proc.stderr


def instrument_test(
    test: TestCase,
    release: bool,
    out_dir: Path,
    build_jobs: int,
    execute: bool,
) -> Result:
    mode = "release" if release else "debug"
    event_path = out_dir / f"{test.kind}-{test.name}-{mode}.jsonl"
    event_path.unlink(missing_ok=True)
    clean = clean_test_artifact(test, release)
    if clean.returncode != 0:
        return Result(
            test.name,
            test.kind,
            mode,
            False,
            notes=[f"cargo clean failed: {clean.stderr}"],
            event_path=str(event_path),
        )

    env = os.environ.copy()
    env["RCGJ_INSTRUMENT_PATH"] = str(event_path)
    env["RCGJ_INSTRUMENT_TEST"] = test.name
    env["RCGJ_INSTRUMENT_MODE"] = mode
    start = time.perf_counter()
    proc = build_test(test, release, build_jobs, env=env)
    build_seconds = time.perf_counter() - start
    events, parse_errors = parse_events(event_path)
    phases, functions, frontend = summarize(events, build_seconds)
    result = Result(
        test.name,
        test.kind,
        mode,
        proc.returncode == 0,
        build_seconds=build_seconds,
        frontend_seconds=frontend,
        phase_totals=phases,
        function_totals=functions,
        event_path=str(event_path),
        parse_errors=parse_errors,
    )
    if proc.returncode != 0:
        result.notes.append(proc.stderr)
        return result

    jar = jar_path(test, release)
    if not jar.exists():
        result.success = False
        result.notes.append(f"missing JAR: {jar}")
        return result
    if execute:
        result.success, result.java_seconds, note = run_java(test, jar, release)
        if not result.success:
            result.notes.append(note)
    return result


def format_result(result: Result, top: int) -> str:
    lines = [
        f"{result.test_name} [{result.test_type}, {result.mode}]: "
        f"{'PASS' if result.success else 'FAIL'}",
        f"  cargo build: {result.build_seconds:.3f} s",
        f"  rustc frontend/link orchestration: {result.frontend_seconds:.3f} s",
    ]
    for phase in PHASES:
        lines.append(f"  {phase}: {result.phase_totals.get(phase, 0.0):.3f} s")
        hottest = sorted(
            result.function_totals.get(phase, {}).items(),
            key=lambda item: item[1],
            reverse=True,
        )[:top]
        for index, (name, seconds) in enumerate(hottest, 1):
            lines.append(f"    {index}. {name}: {seconds:.3f} s")
    lines.append(f"  java-linker: {result.phase_totals.get('java-linker', 0.0):.3f} s")
    if result.java_seconds:
        lines.append(f"  java: {result.java_seconds:.3f} s")
    if result.parse_errors:
        lines.append(f"  malformed event lines: {result.parse_errors}")
    lines.extend(f"  note: {note.strip()}" for note in result.notes if note.strip())
    return "\n".join(lines)


def comma_set(value: str | None) -> set[str] | None:
    return None if value is None else {item.strip() for item in value.split(",") if item.strip()}


def main() -> int:
    parser = argparse.ArgumentParser(description="Instrument JVM-target test builds")
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--release", action="store_true")
    parser.add_argument("--only-run")
    parser.add_argument("--dont-run")
    parser.add_argument("-j", "--jobs", type=int)
    parser.add_argument("--top", type=int, default=10)
    parser.add_argument("--out-dir", type=Path)
    parser.add_argument("--skip-bootstrap", action="store_true")
    parser.add_argument("--no-run", action="store_true")
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
        ROOT / ".generated" / "instrument" / datetime.now().strftime("%Y%m%d-%H%M%S")
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
                    instrument_test,
                    test,
                    release,
                    out_dir,
                    cargo_jobs(workers),
                    not args.no_run,
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
        json.dumps([asdict(result) for result in results], indent=2),
        encoding="utf-8",
    )
    print(f"Reports written to {out_dir}")
    return 0 if all(result.success for result in results) else 1


if __name__ == "__main__":
    raise SystemExit(main())
