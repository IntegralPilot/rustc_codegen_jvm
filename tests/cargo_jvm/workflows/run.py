#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path


def run(command: list[str], *, cwd: Path | None = None) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(
        command,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace",
    )
    if result.returncode != 0:
        print(f"command failed: {' '.join(command)}", file=sys.stderr)
        print(result.stdout, file=sys.stderr)
        print(result.stderr, file=sys.stderr)
        raise SystemExit(result.returncode)
    return result


def expect(output: str, expected: str, operation: str) -> None:
    if expected not in output:
        raise SystemExit(
            f"{operation} output did not contain {expected!r}:\n{output}"
        )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--cargo-jvm", type=Path, required=True)
    parser.add_argument("--backend", type=Path, required=True)
    parser.add_argument("--target-dir", type=Path, required=True)
    parser.add_argument("--release", action="store_true")
    args = parser.parse_args()

    root = Path(__file__).resolve().parent
    profile = "release" if args.release else "debug"
    output = args.target_dir / "cargo-jvm-workflows" / profile
    output.mkdir(parents=True, exist_ok=True)
    binary_jar = output / "workflow-binary.jar"
    library_jar = output / "workflow-library.jar"
    java_classes = output / "java"
    java_classes.mkdir(parents=True, exist_ok=True)

    command = [
        str(args.cargo_jvm),
        "--backend-path",
        str(args.backend),
    ]
    cargo_arguments = [
        "--manifest-path",
        str(root / "Cargo.toml"),
        "--target-dir",
        str(args.target_dir),
    ]
    if args.release:
        cargo_arguments.append("--release")

    run([*command, "build", *cargo_arguments])

    result = run(
        [
            *command,
            "run",
            "--bin",
            "cargo_jvm_workflow",
            *cargo_arguments,
            "--",
            "workflow-argument",
        ]
    )
    expect(
        result.stdout,
        "cargo-jvm run: workflow-argument: 42",
        "cargo jvm run",
    )

    result = run([*command, "test", *cargo_arguments, "--", "--nocapture"])
    expect(result.stdout, "1 passed", "cargo jvm test")

    run(
        [
            *command,
            "package",
            "--bin",
            "cargo_jvm_workflow",
            "--output",
            str(binary_jar),
            *cargo_arguments,
        ]
    )
    result = run(["java", "-jar", str(binary_jar), "workflow-argument"])
    expect(
        result.stdout,
        "cargo-jvm run: workflow-argument: 42",
        "packaged binary",
    )

    run(
        [
            *command,
            "package",
            "--lib",
            "--output",
            str(library_jar),
            *cargo_arguments,
        ]
    )
    listing = run(["jar", "tf", str(library_jar)]).stdout
    expect(
        listing,
        "org/rustlang/runtime/Pointer.class",
        "packaged runtime classes",
    )
    run(
        [
            "javac",
            "-cp",
            str(library_jar),
            "-d",
            str(java_classes),
            str(root / "Main.java"),
        ]
    )
    result = run(
        [
            "java",
            "-cp",
            os.pathsep.join([str(java_classes), str(library_jar)]),
            "Main",
        ]
    )
    expect(
        result.stdout,
        "cargo-jvm library package: 42",
        "packaged library",
    )

    print("cargo-jvm build, run, test, and binary/library package workflows passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
