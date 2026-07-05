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
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parent

PHASES = ("lower1", "optimise1", "lower2")


@dataclass
class InstrumentedResult:
    test_name: str
    test_type: str
    mode: str
    success: bool
    skipped: bool = False
    skip_reason: str = ""
    build_seconds: float = 0.0
    java_seconds: float = 0.0
    frontend_seconds: float = 0.0
    phase_totals: dict[str, float] = field(default_factory=dict)
    function_totals: dict[str, dict[str, float]] = field(default_factory=dict)
    logs: list[str] = field(default_factory=list)
    event_path: Path | None = None
    parse_errors: int = 0


def read_from_file(path: Path) -> str:
    return path.read_text()


def write_to_file(path: Path, content: str):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)


def run_command(cmd: list[str], cwd: Path | None = None, env: dict[str, str] | None = None):
    return subprocess.run(
        cmd,
        cwd=cwd,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace",
    )


def fmt_seconds(value: float) -> str:
    return f"{value:.3f} s"


def normalize_name(test_name: str) -> str:
    return test_name.replace("_", " ").capitalize()


def resolve_jobs(requested_jobs: int | None) -> int:
    if requested_jobs is None:
        return os.cpu_count() or 1
    if requested_jobs < 1:
        raise ValueError("--jobs must be at least 1")
    return requested_jobs


def parallelism_description(jobs: int, requested_jobs: int | None) -> str:
    if requested_jobs is None:
        return f"{jobs} concurrent workers (auto; CPU core count; override with -j/--jobs)"
    return f"{jobs} concurrent workers"


def bootstrap_tools():
    for target in ("rust", "java-linker"):
        print(f"Bootstrapping {target}...")
        subprocess.run([sys.executable, "build.py", target], cwd=ROOT, check=True)


def discover_tests(only_run: set[str] | None, dont_run: set[str]) -> list[tuple[Path, str]]:
    tasks: list[tuple[Path, str]] = []
    for test_type in ("binary", "integration"):
        type_dir = ROOT / "tests" / test_type
        if not type_dir.is_dir():
            continue
        for test_dir in sorted(path for path in type_dir.iterdir() if path.is_dir()):
            test_name = test_dir.name
            if only_run is not None and test_name not in only_run:
                continue
            if test_name in dont_run:
                continue
            tasks.append((test_dir, test_type))
    return tasks


def check_results(proc, test_dir: Path, release_mode: bool, logs: list[str]) -> bool:
    expected_returncode_file = test_dir / "java-returncode.expected"
    if expected_returncode_file.exists():
        expected_returncode = int(read_from_file(expected_returncode_file).strip())
        if proc.returncode != expected_returncode:
            fail_path = test_dir / "java-returncode-fail.generated"
            output = (
                f"Expected return code: {expected_returncode}\n"
                f"Actual return code: {proc.returncode}\n\n"
                f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
            )
            write_to_file(fail_path, output)
            logs.append(f"java exited with code {proc.returncode}, expected {expected_returncode}")
            return False
    elif proc.returncode != 0:
        fail_path = test_dir / "java-fail.generated"
        write_to_file(fail_path, f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}")
        logs.append(f"java exited with code {proc.returncode}")
        return False

    expected_file = (
        test_dir / "java-output.release.expected"
        if release_mode
        else test_dir / "java-output.expected"
    )
    if not expected_file.exists() and release_mode:
        expected_file = test_dir / "java-output.expected"

    if expected_file.exists():
        expected_output = "".join(read_from_file(expected_file).strip().split())
        actual_output = f"STDOUT:{proc.stdout.strip()}STDERR:{proc.stderr.strip()}".strip()
        actual_output = "".join(actual_output.split())

        if actual_output != expected_output:
            diff_path = test_dir / "output-diff.generated"
            diff_content = (
                f"--- EXPECTED ---\n{read_from_file(expected_file)}\n\n"
                f"--- ACTUAL STDOUT ---\n{proc.stdout}\n\n"
                f"--- ACTUAL STDERR ---\n{proc.stderr}\n"
            )
            write_to_file(diff_path, diff_content)
            logs.append("java output did not match expected output")
            return False

    return True


def build_rust_code(
    test_dir: Path,
    release_mode: bool,
    logs: list[str],
    event_path: Path,
):
    build_cmd = ["cargo", "build", "--release"] if release_mode else ["cargo", "build"]
    use_target_json = test_dir / "use_target_json.flag"
    if use_target_json.exists():
        build_cmd.extend(["--target", "../../../jvm-unknown-unknown.json"])
        build_cmd.extend(["-Zjson-target-spec"])

    env = os.environ.copy()
    env["RCGJ_INSTRUMENT_PATH"] = str(event_path)
    env["RCGJ_INSTRUMENT_TEST"] = test_dir.name
    env["RCGJ_INSTRUMENT_MODE"] = "release" if release_mode else "debug"

    start = time.perf_counter()
    proc = run_command(build_cmd, cwd=test_dir, env=env)
    elapsed = time.perf_counter() - start
    target_dir = "release" if release_mode else "debug"

    if proc.returncode != 0:
        fail_path = test_dir / "cargo-build-fail.generated"
        output = f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
        write_to_file(fail_path, output)
        logs.append(f"cargo build exited with code {proc.returncode}")
        return False, "", elapsed

    return True, target_dir, elapsed


def find_and_prepare_jar(
    test_dir: Path,
    test_name: str,
    target_dir: str,
    logs: list[str],
):
    use_target_json = test_dir / "use_target_json.flag"
    if not use_target_json.exists():
        deps_dir = test_dir / "target" / target_dir / "deps"
        jar_file = None
        try:
            for file in deps_dir.iterdir():
                if file.name.startswith(test_name) and file.name.endswith(".jar"):
                    jar_file = file
                    break
        except FileNotFoundError:
            logs.append(f"dependency directory not found: {deps_dir}")
            return False, Path()

        if jar_file is None:
            logs.append(f"no jar file found for {test_name} in target/{target_dir}/deps")
            return False, Path()

        dest_dir = test_dir / "target" / "jvm-unknown-unknown" / target_dir
        dest_dir.mkdir(parents=True, exist_ok=True)
        jar_file.replace(dest_dir / f"{test_name}.jar")

    jar_path = test_dir / "target" / "jvm-unknown-unknown" / target_dir / f"{test_name}.jar"
    if not jar_path.exists():
        logs.append(f"jar file not found at expected path: {jar_path}")
        return False, Path()
    return True, jar_path


def run_binary_test(
    test_dir: Path,
    release_mode: bool,
    jar_path: Path,
    logs: list[str],
):
    test_name = test_dir.name
    start = time.perf_counter()
    proc = run_command(["java", "-cp", str(jar_path), test_name])
    elapsed = time.perf_counter() - start
    return check_results(proc, test_dir, release_mode, logs), elapsed


def run_integration_test(
    test_dir: Path,
    release_mode: bool,
    jar_path: Path,
    logs: list[str],
):
    java_files = [path.name for path in test_dir.glob("*.java")]
    if not java_files:
        logs.append("no .java files found in test directory")
        return False, 0.0

    relative_jar_path = os.path.relpath(jar_path, test_dir)
    javac_cp = os.pathsep.join([".", relative_jar_path])

    proc = run_command(["javac", "-cp", javac_cp] + java_files, cwd=test_dir)
    if proc.returncode != 0:
        fail_path = test_dir / "javac-fail.generated"
        write_to_file(fail_path, f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}")
        logs.append(f"javac exited with code {proc.returncode}")
        return False, 0.0

    start = time.perf_counter()
    proc = run_command(["java", "-cp", javac_cp, "Main"], cwd=test_dir)
    elapsed = time.perf_counter() - start
    return check_results(proc, test_dir, release_mode, logs), elapsed


def parse_events(event_path: Path):
    events: list[dict[str, Any]] = []
    parse_errors = 0
    if not event_path.exists():
        return events, parse_errors

    for line in event_path.read_text(errors="replace").splitlines():
        if not line.strip():
            continue
        try:
            events.append(json.loads(line))
        except json.JSONDecodeError:
            parse_errors += 1
    return events, parse_errors


def summarize_events(events: list[dict[str, Any]], build_seconds: float):
    phase_totals: dict[str, float] = defaultdict(float)
    function_totals: dict[str, dict[str, float]] = defaultdict(lambda: defaultdict(float))

    for event in events:
        stage = event.get("stage")
        seconds = float(event.get("seconds") or 0.0)
        if not stage:
            continue
        if event.get("kind") == "phase":
            phase_totals[stage] += seconds
        elif event.get("kind") == "function":
            item = event.get("item") or "<unknown>"
            function_totals[stage][item] += seconds

    backend_seconds = sum(phase_totals.get(phase, 0.0) for phase in PHASES)
    linker_total = phase_totals.get("java-linker", 0.0)
    frontend_seconds = max(build_seconds - backend_seconds - linker_total, 0.0)

    return dict(phase_totals), {
        stage: dict(items)
        for stage, items in function_totals.items()
    }, frontend_seconds


def run_instrumented_test(
    test_dir: Path,
    test_type: str,
    mode: str,
    out_dir: Path,
    run_java: bool,
) -> InstrumentedResult:
    release_mode = mode == "release"
    test_name = test_dir.name
    normalized = normalize_name(test_name)
    logs = [f"Test {test_name} ({normalized}) [{test_type}, {mode}]"]
    event_path = out_dir / f"{test_type}-{test_name}-{mode}.jsonl"
    if event_path.exists():
        event_path.unlink()

    no_release_file = test_dir / "no_release.flag"
    if release_mode and no_release_file.exists():
        reason = read_from_file(no_release_file).strip()
        return InstrumentedResult(
            test_name=test_name,
            test_type=test_type,
            mode=mode,
            success=True,
            skipped=True,
            skip_reason=reason,
            logs=logs,
            event_path=event_path,
        )

    clean_proc = run_command(["cargo", "clean"], cwd=test_dir)
    if clean_proc.returncode != 0:
        write_to_file(
            test_dir / "cargo-clean-fail.generated",
            f"STDOUT:\n{clean_proc.stdout}\n\nSTDERR:\n{clean_proc.stderr}",
        )
        logs.append(f"cargo clean exited with code {clean_proc.returncode}")
        return InstrumentedResult(
            test_name=test_name,
            test_type=test_type,
            mode=mode,
            success=False,
            logs=logs,
            event_path=event_path,
        )

    build_ok, target_dir, build_seconds = build_rust_code(
        test_dir,
        release_mode,
        logs,
        event_path,
    )

    events, parse_errors = parse_events(event_path)
    phase_totals, function_totals, frontend_seconds = summarize_events(events, build_seconds)

    if not build_ok:
        return InstrumentedResult(
            test_name=test_name,
            test_type=test_type,
            mode=mode,
            success=False,
            build_seconds=build_seconds,
            frontend_seconds=frontend_seconds,
            phase_totals=phase_totals,
            function_totals=function_totals,
            logs=logs,
            event_path=event_path,
            parse_errors=parse_errors,
        )

    jar_ok, jar_path = find_and_prepare_jar(test_dir, test_name, target_dir, logs)
    if not jar_ok:
        return InstrumentedResult(
            test_name=test_name,
            test_type=test_type,
            mode=mode,
            success=False,
            build_seconds=build_seconds,
            frontend_seconds=frontend_seconds,
            phase_totals=phase_totals,
            function_totals=function_totals,
            logs=logs,
            event_path=event_path,
            parse_errors=parse_errors,
        )

    java_seconds = 0.0
    success = True
    if run_java:
        if test_type == "binary":
            success, java_seconds = run_binary_test(test_dir, release_mode, jar_path, logs)
        else:
            success, java_seconds = run_integration_test(test_dir, release_mode, jar_path, logs)

    return InstrumentedResult(
        test_name=test_name,
        test_type=test_type,
        mode=mode,
        success=success,
        build_seconds=build_seconds,
        java_seconds=java_seconds,
        frontend_seconds=frontend_seconds,
        phase_totals=phase_totals,
        function_totals=function_totals,
        logs=logs,
        event_path=event_path,
        parse_errors=parse_errors,
    )


def sorted_hot_functions(result: InstrumentedResult, stage: str, top_n: int):
    functions = result.function_totals.get(stage, {})
    return sorted(functions.items(), key=lambda item: item[1], reverse=True)[:top_n]


def format_result(result: InstrumentedResult, top_n: int) -> str:
    status = "SKIPPED" if result.skipped else ("PASS" if result.success else "FAIL")
    lines = [
        f"{result.test_name} [{result.test_type}] {result.mode}: {status}",
    ]

    if result.skipped:
        lines.append(f"  reason: {result.skip_reason}")
        return "\n".join(lines)

    lines.append(f"  cargo build total: {fmt_seconds(result.build_seconds)}")
    lines.append(f"  rustc frontend: {fmt_seconds(result.frontend_seconds)} (residual)")
    for phase in PHASES:
        lines.append(f"  {phase}: {fmt_seconds(result.phase_totals.get(phase, 0.0))}")
        hot_functions = sorted_hot_functions(result, phase, top_n)
        if hot_functions:
            for index, (name, seconds) in enumerate(hot_functions, start=1):
                lines.append(f"    {index}. {name}: {fmt_seconds(seconds)}")
        else:
            lines.append("    no function-level events")

    lines.append(f"  java-linker: {fmt_seconds(result.phase_totals.get('java-linker', 0.0))}")
    if result.java_seconds:
        lines.append(f"  java run: {fmt_seconds(result.java_seconds)}")
    if result.parse_errors:
        lines.append(f"  warning: {result.parse_errors} malformed instrumentation line(s)")
    if result.logs[1:]:
        lines.append("  notes:")
        for log in result.logs[1:]:
            lines.append(f"    {log}")
    return "\n".join(lines)


def result_to_json(result: InstrumentedResult):
    return {
        "test_name": result.test_name,
        "test_type": result.test_type,
        "mode": result.mode,
        "success": result.success,
        "skipped": result.skipped,
        "skip_reason": result.skip_reason,
        "build_seconds": result.build_seconds,
        "java_seconds": result.java_seconds,
        "frontend_seconds": result.frontend_seconds,
        "phase_totals": result.phase_totals,
        "function_totals": result.function_totals,
        "event_path": str(result.event_path) if result.event_path else None,
        "parse_errors": result.parse_errors,
        "logs": result.logs,
    }


def main():
    parser = argparse.ArgumentParser(
        description="Run rustc_codegen_jvm tests with phase and function timing instrumentation."
    )
    parser.add_argument("--debug", action="store_true", help="Run debug tests")
    parser.add_argument("--release", action="store_true", help="Run release tests")
    parser.add_argument("--only-run", type=str, help="Comma-separated test names to run")
    parser.add_argument("--dont-run", type=str, help="Comma-separated test names to exclude")
    parser.add_argument(
        "-j",
        "--jobs",
        type=int,
        default=None,
        help="Number of concurrent tests per mode (default: auto)",
    )
    parser.add_argument("--top", type=int, default=10, help="Number of hot functions per phase")
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=None,
        help="Directory for JSONL events and reports",
    )
    parser.add_argument(
        "--skip-bootstrap",
        action="store_true",
        help="Do not rebuild the backend/linker before measuring",
    )
    parser.add_argument(
        "--no-run",
        action="store_true",
        help="Only build tests; skip Java execution",
    )
    args = parser.parse_args()
    try:
        jobs = resolve_jobs(args.jobs)
    except ValueError as e:
        parser.error(str(e))

    modes = []
    if args.debug:
        modes.append("debug")
    if args.release:
        modes.append("release")
    if not modes:
        modes = ["debug", "release"]

    only_run = {name.strip() for name in args.only_run.split(",")} if args.only_run else None
    dont_run = {name.strip() for name in args.dont_run.split(",")} if args.dont_run else set()
    tasks = discover_tests(only_run, dont_run)
    if not tasks:
        print("No tests matched the specified filters.")
        return 0

    if args.out_dir is None:
        stamp = datetime.now().strftime("%Y%m%d-%H%M%S")
        out_dir = ROOT / ".generated" / "instrument" / stamp
    else:
        out_dir = args.out_dir
        if not out_dir.is_absolute():
            out_dir = ROOT / out_dir
    out_dir.mkdir(parents=True, exist_ok=True)

    if not args.skip_bootstrap:
        bootstrap_tools()

    print(f"Writing instrumentation to {out_dir}")
    print(f"Running {len(tasks)} test(s) for mode(s): {', '.join(modes)}")
    print(f"Using {parallelism_description(jobs, args.jobs)}")

    all_results: list[InstrumentedResult] = []
    overall_success = True
    for mode in modes:
        print(f"\n=== {mode} ===")
        with ThreadPoolExecutor(max_workers=jobs) as executor:
            futures = {
                executor.submit(
                    run_instrumented_test,
                    test_dir,
                    test_type,
                    mode,
                    out_dir,
                    not args.no_run,
                ): (test_dir, test_type)
                for test_dir, test_type in tasks
            }
            for future in as_completed(futures):
                result = future.result()
                all_results.append(result)
                if not result.success:
                    overall_success = False
                print(format_result(result, args.top))
                print()

    report = "\n\n".join(format_result(result, args.top) for result in all_results)
    write_to_file(out_dir / "report.txt", report + "\n")
    write_to_file(
        out_dir / "summary.json",
        json.dumps([result_to_json(result) for result in all_results], indent=2) + "\n",
    )

    print(f"Report written to {out_dir / 'report.txt'}")
    print(f"Summary JSON written to {out_dir / 'summary.json'}")
    return 0 if overall_success else 1


if __name__ == "__main__":
    sys.exit(main())
