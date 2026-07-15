#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import zipfile
from pathlib import Path

from test_harness import (
    ROOT,
    TestCase,
    build_test,
    cpu_count,
    discover_tests,
    jar_path,
    prepare_shared_cache,
    prime_core,
    run_command,
)


def write_process_log(
    path: Path, command: list[str], proc: subprocess.CompletedProcess[str]
) -> None:
    if command:
        rendered_command = " ".join(command)
    elif isinstance(proc.args, str):
        rendered_command = proc.args
    else:
        rendered_command = " ".join(str(argument) for argument in proc.args)
    path.write_text(
        f"--- COMMAND ---\n{rendered_command}\n\n"
        f"--- RETURN CODE: {proc.returncode} ---\n\n"
        f"--- STDOUT ---\n{proc.stdout}\n\n--- STDERR ---\n{proc.stderr}",
        encoding="utf-8",
    )


def find_test(name: str) -> TestCase:
    matches = [test for test in discover_tests() if test.name == name]
    if not matches:
        raise ValueError(f"unknown test: {name}")
    if len(matches) > 1:
        kinds = ", ".join(test.kind for test in matches)
        raise ValueError(f"test name {name!r} is ambiguous across: {kinds}")
    return matches[0]


def investigate(name: str, release: bool) -> int:
    test = find_test(name)
    mode = "release" if release else "debug"
    output_dir = ROOT / "investigation" / f"{test.name}_{mode}"
    extracted_dir = output_dir / "extracted_jar"
    javap_dir = output_dir / "javap_output"

    if output_dir.exists():
        shutil.rmtree(output_dir)
    extracted_dir.mkdir(parents=True)
    javap_dir.mkdir()

    invalidated = prepare_shared_cache()
    print(f"🔬 Investigating {test.name} [{test.kind}, {mode}, jvm-unknown-unknown]")
    if invalidated:
        print("|- Compiler inputs changed; reset the shared test cache")
    print("|- Building shared core/compiler_builtins cache...")
    proc = prime_core(release)
    write_process_log(output_dir / "core_build.log", [], proc)
    if proc.returncode != 0:
        print(f"|- ❌ Shared core build failed; see {output_dir / 'core_build.log'}")
        return 1

    print("|- Building test with the JVM target...")
    proc = build_test(test, release, cpu_count())
    write_process_log(output_dir / "cargo_build.log", [], proc)
    if proc.returncode != 0:
        print(f"|- ❌ Cargo build failed; see {output_dir / 'cargo_build.log'}")
        return 1

    jar = jar_path(test, release)
    if not jar.exists():
        print(f"|- ❌ Expected JAR is missing: {jar}")
        return 1
    print(f"|- JAR: {jar}")

    with zipfile.ZipFile(jar) as archive:
        archive.extractall(extracted_dir)
        (output_dir / "jar_contents.txt").write_text(
            "\n".join(sorted(archive.namelist())) + "\n",
            encoding="utf-8",
        )

    class_files = sorted(extracted_dir.rglob("*.class"))
    print(f"|- Decompiling {len(class_files)} class file(s)...")
    for class_file in class_files:
        relative = class_file.relative_to(extracted_dir)
        output = javap_dir / relative.with_suffix(".javap.txt")
        output.parent.mkdir(parents=True, exist_ok=True)
        proc = run_command(["javap", "-v", "-p", str(class_file)])
        write_process_log(output, ["javap", "-v", "-p", str(class_file)], proc)

    if test.kind == "integration":
        java_files = sorted(path.name for path in test.directory.glob("*.java"))
        classpath = os.pathsep.join([".", str(jar)])
        javac = run_command(["javac", "-cp", classpath, *java_files], cwd=test.directory)
        write_process_log(output_dir / "javac.log", ["javac", "-cp", classpath, *java_files], javac)
        if javac.returncode != 0:
            print(f"|- ❌ javac failed; see {output_dir / 'javac.log'}")
            return 1
        command = ["java", "-cp", classpath, "Main"]
        proc = run_command(command, cwd=test.directory)
    else:
        command = ["java", "-cp", str(jar), f"{test.artifact_name}.{test.artifact_name}"]
        proc = run_command(command)
    write_process_log(output_dir / "java_run.log", command, proc)

    status = "✅" if proc.returncode == 0 else "⚠️"
    print(f"|- {status} Java exited with code {proc.returncode}")
    print(f"✨ Investigation artifacts: {output_dir}")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description="Inspect one JVM-target test build")
    parser.add_argument("test_name")
    parser.add_argument("--release", action="store_true")
    args = parser.parse_args()
    try:
        return investigate(args.test_name, args.release)
    except (RuntimeError, ValueError) as error:
        parser.error(str(error))
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
