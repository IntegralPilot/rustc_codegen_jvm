#!/usr/bin/env python3
"""Build the pinned reporter checkout and render Hello World on the JVM.

The CI checkout revision is pinned in .github/workflows/ci.yml. For local runs,
pass --source to an existing checkout after running python build.py all.
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

from Tester import run_java_command
from test_harness import TARGET_SPEC, TEST_CONFIG, stdlib_build_environment, validate_configuration


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=ROOT / "target/typst/source")
    parser.add_argument("--target-dir", type=Path, default=ROOT / "target/typst/build")
    parser.add_argument("--reports", type=Path, default=ROOT / "target/typst/reports")
    parser.add_argument("--timeout", type=float, default=180)
    args = parser.parse_args()
    if args.timeout <= 0:
        parser.error("--timeout must be positive")
    source, target, reports = (
        path.resolve() for path in (args.source, args.target_dir, args.reports)
    )
    validate_configuration()
    manifest = source / "Cargo.toml"
    entry = source / "crates/typst-shared/src/main.rs"
    if not entry.is_file():
        parser.error(f"missing Typst reporter checkout: {source}")
    # Cargo does not track changes to the backend library or bundled runtime.
    # Rebuild JVM artifacts each time, retaining compiled host build tools.
    jvm_target = target / TARGET_SPEC.stem
    if jvm_target.exists():
        shutil.rmtree(jvm_target)
    reports.mkdir(parents=True, exist_ok=True)
    # Never accept a rendered file left behind by an earlier successful run.
    for name in ("page.png", "runtime.stdout.log", "runtime.stderr.log", "timeout.log", "result.json"):
        (reports / name).unlink(missing_ok=True)

    toolchain = tomllib.loads((ROOT / "rust-toolchain.toml").read_text())["toolchain"][
        "channel"
    ]
    env = stdlib_build_environment()
    env["RUSTUP_TOOLCHAIN"] = toolchain
    env["CARGO_INCREMENTAL"] = "0"
    env["CARGO_ENCODED_RUSTFLAGS"] = "\x1f".join(
        tomllib.loads(TEST_CONFIG.read_text())["build"]["rustflags"]
    )
    command = [
        "cargo", "build", "--locked", "--manifest-path", str(manifest),
        "-p", "typst-shared", "--target", str(TARGET_SPEC),
        "-Zjson-target-spec", "-Zbuild-std=std,panic_unwind",
        "-Zbuild-std-features=panic-unwind", "--target-dir", str(target), "-j2",
    ]
    revision = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], cwd=source, text=True
    ).strip()
    report = {
        "passed": False,
        "source_revision": revision,
        "toolchain": toolchain,
        "build_command": command,
    }
    original = entry.read_bytes()
    started = time.monotonic()
    try:
        shutil.copyfile(Path(__file__).with_name("hello_world.rs"), entry)
        print("Building Typst with the current backend and runtime...", flush=True)
        # Avoid inheriting Cargo configuration from the checkout's parent dirs.
        with (
            tempfile.TemporaryDirectory(prefix="typst-build-") as cwd,
            (reports / "build.stdout.log").open("w") as stdout,
            (reports / "build.stderr.log").open("w") as stderr,
        ):
            built = subprocess.run(command, cwd=cwd, env=env, stdout=stdout, stderr=stderr)
        report["build_exit_code"] = built.returncode
        report["build_seconds"] = time.monotonic() - started
        if built.returncode:
            print((reports / "build.stderr.log").read_text(), file=sys.stderr)
            return 1

        jar = target / "jvm-unknown-jvm/debug/typst-shared.jar"
        command = ["java", "-Xmx2g", "-Xverify:all", "-jar", str(jar)]
        print("Rendering Hello World and waiting for JVM shutdown...", flush=True)
        started = time.monotonic()
        ran, diagnostics = run_java_command(command, timeout=args.timeout, cwd=reports)
        report.update(
            run_command=command,
            run_exit_code=ran.returncode,
            run_seconds=time.monotonic() - started,
        )
        (reports / "runtime.stdout.log").write_text(ran.stdout, encoding="utf-8")
        (reports / "runtime.stderr.log").write_text(ran.stderr, encoding="utf-8")
        if diagnostics is not None:
            (reports / "timeout.log").write_text(diagnostics, encoding="utf-8")
        print(ran.stdout, end="")
        print(ran.stderr, end="", file=sys.stderr)
        if ran.returncode or diagnostics is not None:
            print("Typst failed or the JVM did not exit within the timeout", file=sys.stderr)
            return 1
        if ran.stdout.strip() != "Typst Hello World passed" or ran.stderr:
            print("Unexpected Typst output", file=sys.stderr)
            return 1
        png = reports / "page.png"
        if not png.is_file() or not png.read_bytes().startswith(b"\x89PNG\r\n\x1a\n"):
            print("Missing or invalid rendered PNG", file=sys.stderr)
            return 1
        report["passed"] = True
        return 0
    finally:
        entry.write_bytes(original)
        (reports / "result.json").write_text(
            json.dumps(report, indent=2) + "\n", encoding="utf-8"
        )


if __name__ == "__main__":
    raise SystemExit(main())
