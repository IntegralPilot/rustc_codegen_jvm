#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
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

def java_process_inputs(test: TestCase) -> tuple[list[str], str | None]:
    arguments_path = test.directory / "java-args.json"
    arguments: list[str] = []
    if arguments_path.exists():
        parsed = json.loads(read(arguments_path))
        if not isinstance(parsed, list) or not all(isinstance(value, str) for value in parsed):
            raise ValueError(f"{arguments_path} must contain a JSON array of strings")
        arguments = parsed
    stdin_path = test.directory / "java-stdin.txt"
    stdin = read(stdin_path) if stdin_path.exists() else None
    return arguments, stdin


def jvm_timeout_diagnostics(pid: int) -> str:
    def decoded(value: str | bytes | None) -> str:
        if value is None:
            return ""
        return value.decode("utf-8", errors="replace") if isinstance(value, bytes) else value

    sections: list[str] = []
    for command in (["Thread.print", "-l"], ["VM.command_line"], ["GC.heap_info"]):
        label = " ".join(command)
        try:
            result = subprocess.run(
                ["jcmd", str(pid), *command],
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                encoding="utf-8",
                errors="replace",
                timeout=15,
            )
            sections.append(f"===== jcmd {label} =====\n{result.stdout}")
        except FileNotFoundError:
            sections.append("jcmd is unavailable; no JVM diagnostic could be captured.")
            break
        except subprocess.TimeoutExpired as error:
            partial = decoded(error.stdout)
            sections.append(f"===== jcmd {label} timed out =====\n{partial}")
        except OSError as error:
            sections.append(f"===== jcmd {label} failed =====\n{error}")
    return "\n".join(sections)


def terminate_process_tree(process: subprocess.Popen[bytes]) -> None:
    if os.name == "nt":
        subprocess.run(
            ["taskkill", "/PID", str(process.pid), "/T", "/F"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
        )
    else:
        try:
            process.kill()
        except ProcessLookupError:
            pass


def run_java_command(
    command: list[str],
    *,
    timeout: float,
    cwd: Path | None = None,
    input_text: str | None = None,
) -> tuple[subprocess.CompletedProcess[str], str | None]:
    process = subprocess.Popen(
        command,
        cwd=cwd or Path(__file__).resolve().parent,
        stdin=subprocess.PIPE if input_text is not None else None,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    try:
        stdout, stderr = process.communicate(
            None if input_text is None else input_text.encode("utf-8"),
            timeout=timeout,
        )
        diagnostics = None
    except subprocess.TimeoutExpired:
        diagnostics = jvm_timeout_diagnostics(process.pid)
        terminate_process_tree(process)
        try:
            stdout, stderr = process.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            process.kill()
            stdout, stderr = process.communicate()
        returncode = 124
    else:
        returncode = process.returncode
    return (
        subprocess.CompletedProcess(
            command,
            returncode,
            stdout.decode("utf-8", errors="replace"),
            stderr.decode("utf-8", errors="replace"),
        ),
        diagnostics,
    )


def run_java(
    test: TestCase,
    jar: Path,
    release: bool,
    logs: list[str],
    java_timeout: float,
) -> bool:
    try:
        java_arguments, stdin = java_process_inputs(test)
    except (json.JSONDecodeError, ValueError) as error:
        logs.append(f"|---- ❌ Invalid Java process test input: {error}")
        return False

    if test.kind != "integration":
        logs.append("|--- 🤖 Running with Java...")
        proc, diagnostics = run_java_command(
            [
                "java",
                "-cp",
                str(jar),
                f"{test.artifact_name}.{test.artifact_name}",
                *java_arguments,
            ],
            timeout=java_timeout,
            input_text=stdin,
        )
        if diagnostics is not None:
            output = (
                f"Java process exceeded the {java_timeout:g}s timeout.\n\n"
                f"{diagnostics}\n\nSTDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
            )
            (test.directory / "java-timeout.generated").write_text(
                output, encoding="utf-8"
            )
            logs.append(f"|---- ❌ Java process timed out after {java_timeout:g}s")
            ci_diagnostic(logs, output)
            return False
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
    proc, diagnostics = run_java_command(
        ["java", "-cp", classpath, "Main", *java_arguments],
        timeout=java_timeout,
        cwd=test.directory,
        input_text=stdin,
    )
    if diagnostics is not None:
        output = (
            f"Java process exceeded the {java_timeout:g}s timeout.\n\n"
            f"{diagnostics}\n\nSTDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
        )
        (test.directory / "java-timeout.generated").write_text(
            output, encoding="utf-8"
        )
        logs.append(f"|---- ❌ Java process timed out after {java_timeout:g}s")
        ci_diagnostic(logs, output)
        return False
    return check_results(proc, test, release, logs)


def javap_debug_info(output: str, included_methods: set[str] | None = None) -> str:
    """Keep portable semantic information from javap's debug tables.

    Bytecode offsets and local-variable live ranges legitimately vary with
    constant-pool layout, the host toolchain, and optimization details. The
    source-line sequence and each variable's slot/name/signature are the stable
    metadata this test is intended to protect.
    """
    source: str | None = None
    methods: dict[str, list[str]] = {}
    local_rows: dict[str, set[str]] = {}
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
            current_method = (
                stripped
                if included_methods is None or stripped in included_methods
                else None
            )
            table = None
            continue
        if stripped in {"LineNumberTable:", "LocalVariableTable:"}:
            if current_method is None:
                continue
            methods.setdefault(current_method, []).append(stripped)
            table = stripped
            continue
        if table == "LineNumberTable:" and re.fullmatch(r"line \d+: \d+", stripped):
            methods[current_method].append(stripped.split(":", 1)[0])
            continue
        if table == "LocalVariableTable:":
            if re.fullmatch(
                r"Start\s+Length\s+Slot\s+Name\s+Signature", stripped
            ):
                methods[current_method].append("Slot Name Signature")
                continue
            if re.fullmatch(r"\d+\s+\d+\s+\d+\s+\S+\s+\S+", stripped):
                row = " ".join(stripped.split()[2:])
                seen = local_rows.setdefault(current_method, set())
                if row not in seen:
                    methods[current_method].append(row)
                    seen.add(row)
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

    expected = read(expected_path).strip()
    expected_methods = {
        line.strip()
        for line in expected.splitlines()
        if "(" in line and line.strip().endswith(";")
    }
    actual = javap_debug_info(proc.stdout, expected_methods)
    if actual == expected:
        logs.append("|--- ✅ JVM debug metadata matches expected output!")
        return True

    diff = f"--- EXPECTED ---\n{expected}\n\n--- ACTUAL ---\n{actual}\n"
    (test.directory / "javap-debug-diff.generated").write_text(diff, encoding="utf-8")
    logs.append("|---- ❌ JVM debug metadata did not match expected output")
    ci_diagnostic(logs, diff)
    return False


def run_test(
    test: TestCase,
    release: bool,
    build_jobs: int,
    java_timeout: float,
) -> tuple[bool, list[str]]:
    logs = [f"|-- Test '{test.name}' ({test.kind})"]
    proc = build_test(test, release, build_jobs)
    if proc.returncode != 0:
        write_failure(test.directory / "cargo-build-fail.generated", proc)
        operation = "cargo-jvm workflow" if test.kind == "cargo_jvm" else "cargo build"
        logs.append(f"|---- ❌ {operation} exited with code {proc.returncode}")
        ci_diagnostic(logs, f"STDOUT:\n{proc.stdout}\nSTDERR:\n{proc.stderr}")
        return False, logs

    if test.kind == "cargo_jvm":
        logs.append("|--- 🧰 Exercised build, run, test, and package workflows")
        logs.append("|--- ✅ Test passed!")
        return True, logs

    jar = jar_path(test, release)
    if not jar.exists():
        logs.append(f"|---- ❌ JAR not found at expected target path: {jar}")
        return False, logs
    if not run_java(test, jar, release, logs, java_timeout):
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
    parser.add_argument(
        "--java-timeout",
        type=float,
        default=180,
        help="Seconds allowed for each Java self-test process (default: 180)",
    )
    args = parser.parse_args()

    try:
        if args.java_timeout <= 0:
            raise ValueError("--java-timeout must be greater than zero")
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
    print(f"|- Target: {mode} jvm-unknown-jvm with real std")
    print(f"|- Parallelism: {workers} test worker(s), {per_build_jobs} Cargo job(s) each")
    print(f"|- Java process timeout: {args.java_timeout:g}s")
    if cache_invalidated:
        print("|- Compiler inputs changed; reset the shared test cache")
    print("|- Building the shared standard-library cache once...")
    prime = prime_core(args.release)
    if prime.returncode != 0:
        print(prime.stdout)
        print(prime.stderr, file=sys.stderr)
        print("|- ❌ Shared standard-library build failed")
        return 1
    print("|- ✅ Shared standard-library cache is ready")
    print(f"|- Running {len(tests)} test(s)...\n")

    success = True
    with ThreadPoolExecutor(max_workers=workers) as executor:
        futures = {
            executor.submit(
                run_test,
                test,
                args.release,
                per_build_jobs,
                args.java_timeout,
            ): test
            for test in tests
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
