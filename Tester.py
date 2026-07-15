#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import re
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

from test_harness import (
    TestCase,
    build_test,
    cargo_jobs,
    discover_tests,
    jar_path,
    prepare_shared_cache,
    prime_core,
    resolve_workers,
    run_command,
    validate_configuration,
)


def read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def write_failure(path: Path, proc) -> None:
    path.write_text(
        f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}",
        encoding="utf-8",
    )


def ci_diagnostic(logs: list[str], content: str) -> None:
    if "CI" in os.environ:
        indented = "\n".join(f"|   > {line}" for line in content.splitlines())
        logs.append(f"|---- CI diagnostic output:\n{indented}")


def check_results(proc, test: TestCase, release: bool, logs: list[str]) -> bool:
    expected_code_path = test.directory / "java-returncode.expected"
    if expected_code_path.exists():
        expected_code = int(read(expected_code_path).strip())
        if proc.returncode != expected_code:
            write_failure(test.directory / "java-returncode-fail.generated", proc)
            logs.append(
                f"|---- ❌ java exited with code {proc.returncode}, expected {expected_code}"
            )
            return False
    elif proc.returncode != 0:
        write_failure(test.directory / "java-fail.generated", proc)
        logs.append(f"|---- ❌ java exited with code {proc.returncode}")
        ci_diagnostic(logs, f"STDOUT:\n{proc.stdout}\nSTDERR:\n{proc.stderr}")
        return False

    expected_path = test.directory / (
        "java-output.release.expected" if release else "java-output.expected"
    )
    if release and not expected_path.exists():
        expected_path = test.directory / "java-output.expected"
    if not expected_path.exists():
        return True

    # so it works on Windows
    expected = "".join(read(expected_path).strip().split()).replace("\\", "/")
    actual = "".join(f"STDOUT:{proc.stdout.strip()}STDERR:{proc.stderr.strip()}".split()).replace("\\", "/")
    
    if actual == expected:
        logs.append("|--- ✅ Output matches expected output!")
        return True

    diff = (
        f"--- EXPECTED ---\n{read(expected_path)}\n\n"
        f"--- ACTUAL STDOUT ---\n{proc.stdout}\n\n"
        f"--- ACTUAL STDERR ---\n{proc.stderr}\n"
    )
    (test.directory / "output-diff.generated").write_text(diff, encoding="utf-8")
    logs.append("|---- ❌ java output did not match expected output")
    ci_diagnostic(logs, diff)
    return False

def run_java(test: TestCase, jar: Path, release: bool, logs: list[str]) -> bool:
    if test.kind != "integration":
        logs.append("|--- 🤖 Running with Java...")
        proc = run_command(["java", "-cp", str(jar), f"{test.artifact_name}.{test.artifact_name}"])
        return check_results(proc, test, release, logs)

    java_files = sorted(path.name for path in test.directory.glob("*.java"))
    if not java_files:
        logs.append("|---- ❌ No .java files found in integration test directory")
        return False

    classpath = os.pathsep.join([".", str(jar)])
    logs.append("|--- ☕ Compiling Java test source...")
    proc = run_command(
        ["javac", "-cp", classpath, *java_files],
        cwd=test.directory,
    )
    if proc.returncode != 0:
        write_failure(test.directory / "javac-fail.generated", proc)
        logs.append(f"|---- ❌ javac exited with code {proc.returncode}")
        ci_diagnostic(logs, f"STDOUT:\n{proc.stdout}\nSTDERR:\n{proc.stderr}")
        return False

    logs.append("|--- 🤖 Running with Java...")
    proc = run_command(["java", "-cp", classpath, "Main"], cwd=test.directory)
    return check_results(proc, test, release, logs)


def javap_debug_info(output: str) -> str:
    """Keep method context plus line/local-variable tables from javap output."""
    source: str | None = None
    methods: dict[str, list[str]] = {}
    current_method: str | None = None
    table: str | None = None

    for line in output.splitlines():
        stripped = line.strip()
        if stripped.startswith("Compiled from "):
            source = stripped
            continue
        if (
            line.startswith("  ")
            and not line.startswith("    ")
            and "(" in stripped
            and stripped.endswith(";")
        ):
            current_method = stripped
            table = None
            continue
        if stripped in {"LineNumberTable:", "LocalVariableTable:"}:
            if current_method is None:
                continue
            methods.setdefault(current_method, []).append(stripped)
            table = stripped
            continue
        if table == "LineNumberTable:" and re.fullmatch(r"line \d+: \d+", stripped):
            methods[current_method].append(stripped)
            continue
        if table == "LocalVariableTable:":
            if stripped == "Start  Length  Slot  Name   Signature":
                methods[current_method].append("Start Length Slot Name Signature")
                continue
            if re.fullmatch(r"\d+\s+\d+\s+\d+\s+\S+\s+\S+", stripped):
                methods[current_method].append(" ".join(stripped.split()))
                continue
        table = None

    result = [source] if source is not None else []
    for method in sorted(methods):
        if result:
            result.append("")
        result.append(method)
        result.extend(methods[method])
    return "\n".join(result).strip()


def check_javap_debug_info(
    test: TestCase, jar: Path, release: bool, logs: list[str]
) -> bool:
    expected_path = test.directory / (
        "javap-debug.release.expected" if release else "javap-debug.expected"
    )
    if release and not expected_path.exists():
        expected_path = test.directory / "javap-debug.expected"
    if not expected_path.exists():
        return True

    class_name = f"{test.artifact_name}.{test.artifact_name}"
    logs.append("|--- 🔎 Checking JVM debug metadata with javap...")
    proc = run_command(["javap", "-classpath", str(jar), "-c", "-l", "-p", class_name])
    if proc.returncode != 0:
        write_failure(test.directory / "javap-fail.generated", proc)
        logs.append(f"|---- ❌ javap exited with code {proc.returncode}")
        return False

    actual = javap_debug_info(proc.stdout)
    expected = read(expected_path).strip()
    if actual == expected:
        logs.append("|--- ✅ JVM debug metadata matches expected output!")
        return True

    diff = f"--- EXPECTED ---\n{expected}\n\n--- ACTUAL ---\n{actual}\n"
    (test.directory / "javap-debug-diff.generated").write_text(diff, encoding="utf-8")
    logs.append("|---- ❌ JVM debug metadata did not match expected output")
    ci_diagnostic(logs, diff)
    return False


def run_test(test: TestCase, release: bool, build_jobs: int) -> tuple[bool, list[str]]:
    logs = [f"|-- Test '{test.name}' ({test.kind})"]
    proc = build_test(test, release, build_jobs)
    if proc.returncode != 0:
        write_failure(test.directory / "cargo-build-fail.generated", proc)
        logs.append(f"|---- ❌ cargo build exited with code {proc.returncode}")
        ci_diagnostic(logs, f"STDOUT:\n{proc.stdout}\nSTDERR:\n{proc.stderr}")
        return False, logs

    jar = jar_path(test, release)
    if not jar.exists():
        logs.append(f"|---- ❌ JAR not found at expected target path: {jar}")
        return False, logs
    if not run_java(test, jar, release, logs):
        return False, logs
    if not check_javap_debug_info(test, jar, release, logs):
        return False, logs
    logs.append("|--- ✅ Test passed!")
    return True, logs


def comma_set(value: str | None) -> set[str] | None:
    if value is None:
        return None
    return {name.strip() for name in value.split(",") if name.strip()}


def main() -> int:
    parser = argparse.ArgumentParser(description="Test rustc_codegen_jvm against the JVM target")
    parser.add_argument("--release", action="store_true", help="Use Cargo's release profile")
    parser.add_argument("--only-run", help="Comma-separated test names to run")
    parser.add_argument("--dont-run", help="Comma-separated test names to exclude")
    parser.add_argument("-j", "--jobs", type=int, help="Maximum concurrent test builds")
    args = parser.parse_args()

    try:
        workers = resolve_workers(args.jobs)
        validate_configuration()
        cache_invalidated = prepare_shared_cache()
    except (ValueError, RuntimeError) as error:
        parser.error(str(error))

    tests = discover_tests(comma_set(args.only_run), comma_set(args.dont_run) or set())
    if not tests:
        print("No tests matched the specified filters.")
        return 0

    mode = "release" if args.release else "debug"
    per_build_jobs = cargo_jobs(workers)
    print("🧪 rustc_codegen_jvm test suite")
    print(f"|- Target: {mode} jvm-unknown-unknown with real core")
    print(f"|- Parallelism: {workers} test worker(s), {per_build_jobs} Cargo job(s) each")
    if cache_invalidated:
        print("|- Compiler inputs changed; reset the shared test cache")
    print("|- Building shared core/compiler_builtins cache once...")
    prime = prime_core(args.release)
    if prime.returncode != 0:
        print(prime.stdout)
        print(prime.stderr, file=sys.stderr)
        print("|- ❌ Shared core build failed")
        return 1
    print("|- ✅ Shared core cache is ready")
    print(f"|- Running {len(tests)} test(s)...\n")

    success = True
    with ThreadPoolExecutor(max_workers=workers) as executor:
        futures = {
            executor.submit(run_test, test, args.release, per_build_jobs): test for test in tests
        }
        for future in as_completed(futures):
            passed, logs = future.result()
            print("\n".join(logs))
            print()
            success &= passed

    print("|- ✅ All tests passed!" if success else "|- ❌ Some tests failed!")
    return 0 if success else 1


if __name__ == "__main__":
    raise SystemExit(main())
