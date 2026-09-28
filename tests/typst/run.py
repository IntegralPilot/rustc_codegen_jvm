#!/usr/bin/env python3
"""Build the pinned reporter checkout and render representative documents on the JVM.

The CI checkout revision is pinned in .github/workflows/ci.yml. For local runs,
pass --source to an existing checkout after running python build.py all.
"""
from __future__ import annotations

import argparse
import hashlib
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


CASES = ("hello", "layout", "tables", "math", "references", "graphics", "data")


def run_document(jar: Path, case: str, reports: Path, timeout: float) -> dict:
    fixture = Path(__file__).resolve().parent
    output = reports / case
    if output.exists():
        shutil.rmtree(output)
    output.mkdir(parents=True)
    expected = json.loads((fixture / "expected" / f"{case}.json").read_text(encoding="utf-8"))
    command = [
        "java", "-Xmx2g", "-Xverify:all", "-jar", str(jar),
        str(fixture / "documents" / f"{case}.typ"), str(output),
    ]
    print(f"Rendering {case} and waiting for JVM shutdown...", flush=True)
    started = time.monotonic()
    ran, diagnostics = run_java_command(command, timeout=timeout, cwd=output)
    report = {
        "passed": False,
        "command": command,
        "exit_code": ran.returncode,
        "seconds": time.monotonic() - started,
    }
    (output / "runtime.stdout.log").write_text(ran.stdout, encoding="utf-8")
    (output / "runtime.stderr.log").write_text(ran.stderr, encoding="utf-8")
    if diagnostics is not None:
        (output / "timeout.log").write_text(diagnostics, encoding="utf-8")
    print(ran.stdout, end="")
    print(ran.stderr, end="", file=sys.stderr)
    try:
        assert ran.returncode == 0 and diagnostics is None, "JVM failed or timed out"
        assert not ran.stderr, "unexpected runtime diagnostics"
        assert ran.stdout.strip() == f"Typst document passed: {len(expected)} page(s)"
        pages = json.loads((output / "pages.json").read_text(encoding="utf-8"))
        # Golden text, dimensions and rounded text positions come from the same
        # pinned native build with embedded fonts and a fixed clock. Hashes are diagnostic
        # only: small floating-point differences can vary across platforms.
        assert pages == expected, "page text, dimensions or layout differ from native output"
        pngs = sorted(output.glob("page-*.png"))
        assert len(pngs) == len(expected), "unexpected number of rendered pages"
        assert all(p.read_bytes().startswith(b"\x89PNG\r\n\x1a\n") for p in pngs)
        report["png_sha256"] = {
            p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in pngs
        }
        report["passed"] = True
    except (AssertionError, OSError, ValueError) as error:
        report["error"] = str(error) or "unexpected document output"
        print(f"{case}: {report['error']}", file=sys.stderr)
    (output / "result.json").write_text(json.dumps(report, indent=2) + "\n")
    return report


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=ROOT / "target/typst/source")
    parser.add_argument("--target-dir", type=Path, default=ROOT / "target/typst/build")
    parser.add_argument("--reports", type=Path, default=ROOT / "target/typst/reports")
    parser.add_argument("--timeout", type=float, default=180)
    parser.add_argument("--release", action="store_true", help="Build with Cargo's release profile")
    parser.add_argument("--case", action="append", choices=CASES, dest="cases")
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
    profile = "release" if args.release else "debug"
    jvm_target = target / TARGET_SPEC.stem / profile
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
    if args.release:
        command.append("--release")
    revision = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], cwd=source, text=True
    ).strip()
    report = {
        "passed": False,
        "source_revision": revision,
        "toolchain": toolchain,
        "profile": profile,
        "build_command": command,
    }
    original = entry.read_bytes()
    started = time.monotonic()
    try:
        shutil.copyfile(Path(__file__).with_name("document.rs"), entry)
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

        jar = jvm_target / "typst-shared.jar"
        report["documents"] = {
            case: run_document(jar, case, reports, args.timeout)
            for case in args.cases or CASES
        }
        report["passed"] = all(case["passed"] for case in report["documents"].values())
        return 0 if report["passed"] else 1
    finally:
        entry.write_bytes(original)
        (reports / "result.json").write_text(
            json.dumps(report, indent=2) + "\n", encoding="utf-8"
        )


if __name__ == "__main__":
    raise SystemExit(main())
