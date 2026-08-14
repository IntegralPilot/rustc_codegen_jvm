"""Shared runner for upstream Rust standard-library test crates."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import re
import shutil
import subprocess
import sys
import time
from dataclasses import asdict, dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Sequence

from stdlib_overlay import (
    STDLIB_OVERLAY_ROOT,
    STDLIB_PATCH_ROOT,
    prepare_stdlib_source,
    rustc_info,
)


ROOT = Path(__file__).resolve().parent
TARGET_SPEC = ROOT / "jvm-unknown-jvm.json"
TEST_CONFIG = ROOT / "config.toml"
MAX_CAPTURE_CHARS = 64 * 1024
MAX_BUILD_CACHE_ENTRIES = 2
HARNESS_STARTUP_GRACE_SECONDS = 15.0


@dataclass(frozen=True)
class SuiteConfig:
    name: str
    description: str
    source_directory: str
    cargo_test_target: str
    target_root: Path
    patch_root: Path
    build_schema: str
    default_ignored_tests: dict[str, str]
    dependency_packages: tuple[str, ...] = ("rand", "rand_core", "rand_xorshift")
    java_options: tuple[str, ...] = ()
    include_upstream_ignored_with_defaults: bool = False
    default_jobs: int | None = None


@dataclass(frozen=True)
class TestResult:
    name: str
    status: str
    duration_seconds: float
    returncode: int | None
    stdout: str
    stderr: str


def run(
    command: list[str],
    *,
    cwd: Path = ROOT,
    timeout: float | None = None,
    env: dict[str, str] | None = None,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        cwd=cwd,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace",
        timeout=timeout,
    )


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def tree_digest(root: Path) -> str:
    digest = hashlib.sha256()
    for path in sorted(root.rglob("*")):
        if not path.is_file() or path.name == "Cargo.lock":
            continue
        digest.update(path.relative_to(root).as_posix().encode("utf-8"))
        digest.update(path.read_bytes())
    return digest.hexdigest()


def toolchain_fingerprint(
    config: SuiteConfig,
    rustc_commit: str,
    suite_hash: str,
    lockfile: Path,
) -> tuple[str, dict[str, str]]:
    candidates = [
        TARGET_SPEC,
        TEST_CONFIG,
        lockfile,
        ROOT / "runtime" / "build" / "libs" / "runtime-0.1.0.jar",
        ROOT
        / "java-linker"
        / "target"
        / "release"
        / ("java-linker.exe" if os.name == "nt" else "java-linker"),
    ]
    candidates.extend(
        path
        for path in (ROOT / "target" / "release").glob("*rustc_codegen_jvm.*")
        if path.suffix in {".dll", ".dylib", ".so"}
    )
    candidates.extend(path for path in STDLIB_OVERLAY_ROOT.rglob("*") if path.is_file())
    candidates.extend(path for path in STDLIB_PATCH_ROOT.rglob("*.patch") if path.is_file())
    candidates.extend(path for path in config.patch_root.rglob("*.patch") if path.is_file())
    hashes: dict[str, str] = {}
    digest = hashlib.sha256()
    digest.update(config.build_schema.encode("utf-8"))
    digest.update(rustc_commit.encode("utf-8"))
    digest.update(suite_hash.encode("utf-8"))
    for path in sorted(set(candidates)):
        relative = str(path.relative_to(ROOT)) if path.is_relative_to(ROOT) else str(path)
        value = sha256_file(path) if path.is_file() else "missing"
        hashes[relative] = value
        digest.update(relative.encode("utf-8"))
        digest.update(value.encode("ascii"))
    return digest.hexdigest(), hashes


def prune_build_cache(mode_root: Path, current: Path) -> int:
    if not mode_root.exists():
        return 0
    stale = sorted(
        (path for path in mode_root.iterdir() if path.is_dir() and path != current),
        key=lambda path: path.stat().st_mtime,
        reverse=True,
    )
    removed = 0
    for path in stale[MAX_BUILD_CACHE_ENTRIES - 1 :]:
        shutil.rmtree(path)
        removed += 1
    return removed


def package_version(lockfile: Path, package: str) -> str:
    matches: list[str] = []
    for block in lockfile.read_text(encoding="utf-8").split("[[package]]")[1:]:
        name = re.search(r'^\s*name\s*=\s*"([^"]+)"', block, re.MULTILINE)
        version = re.search(r'^\s*version\s*=\s*"([^"]+)"', block, re.MULTILINE)
        if name is not None and version is not None and name.group(1) == package:
            matches.append(version.group(1))
    if len(matches) != 1:
        raise RuntimeError(
            f"expected one {package} package in the installed library Cargo.lock"
        )
    return matches[0]


def prepare_suite_source(
    config: SuiteConfig,
    sysroot: Path,
    commit: str,
) -> tuple[Path, str, str, Path]:
    library = sysroot / "lib" / "rustlib" / "src" / "rust" / "library"
    installed = library / config.source_directory
    manifest = installed / "Cargo.toml"
    library_lockfile = library / "Cargo.lock"
    if not manifest.exists() or not library_lockfile.exists():
        raise RuntimeError(
            f"{config.name} was not found at {installed}; install it with "
            "`rustup component add rust-src`"
        )
    source_hash = tree_digest(installed)
    patch_hash = tree_digest(config.patch_root)
    destination = (
        config.target_root
        / "source"
        / f"{commit[:12]}-{source_hash[:12]}-{patch_hash[:12]}"
    )
    if not destination.exists():
        destination.parent.mkdir(parents=True, exist_ok=True)
        try:
            shutil.copytree(installed, destination)
            patch_directory = destination.relative_to(ROOT)
            for patch in sorted(config.patch_root.glob("*.patch")):
                applied = run(
                    [
                        "git",
                        "apply",
                        "--whitespace=nowarn",
                        f"--directory={patch_directory}",
                        str(patch.resolve()),
                    ],
                    cwd=ROOT,
                )
                if applied.returncode != 0:
                    raise RuntimeError(
                        f"{config.name} patch {patch.name} does not apply to rustc "
                        f"{commit}:\n{applied.stdout}{applied.stderr}"
                    )
        except Exception:
            shutil.rmtree(destination, ignore_errors=True)
            raise

    shutil.copy2(manifest, destination / "Cargo.toml")
    lockfile = destination / "Cargo.lock"
    marker = destination / ".canonical-library-lock"
    canonical_hash = sha256_file(library_lockfile)
    if (
        not lockfile.exists()
        or not marker.exists()
        or marker.read_text().strip() != canonical_hash
    ):
        lockfile.unlink(missing_ok=True)
        resolution = run(
            ["cargo", "generate-lockfile", "--manifest-path", str(destination / "Cargo.toml")]
        )
        if resolution.returncode != 0:
            raise RuntimeError(
                f"could not resolve {config.name} dependencies:\n"
                f"{resolution.stdout}{resolution.stderr}"
            )
        for package in config.dependency_packages:
            update = run(
                [
                    "cargo",
                    "update",
                    "--manifest-path",
                    str(destination / "Cargo.toml"),
                    "--package",
                    package,
                    "--precise",
                    package_version(library_lockfile, package),
                ]
            )
            if update.returncode != 0:
                raise RuntimeError(
                    f"could not pin {config.name} dependency {package}:\n"
                    f"{update.stdout}{update.stderr}"
                )
        marker.write_text(canonical_hash + "\n", encoding="utf-8")
    return destination, source_hash, patch_hash, lockfile


def cargo_build_command(
    config: SuiteConfig,
    manifest: Path,
    target_dir: Path,
    release: bool,
    jobs: int,
) -> list[str]:
    command = [
        "cargo",
        "test",
        "--manifest-path",
        str(manifest),
        "--test",
        config.cargo_test_target,
        "--no-run",
        "--locked",
        "--target",
        str(TARGET_SPEC),
        "--target-dir",
        str(target_dir),
        "--config",
        str(TEST_CONFIG),
        "--config",
        "profile.dev.debug=0",
        "--config",
        "profile.dev.incremental=false",
        "--jobs",
        str(jobs),
        "--message-format=json-render-diagnostics",
        "-Zjson-target-spec",
        "-Zbuild-std=std,panic_unwind",
        "-Zbuild-std-features=",
    ]
    if release:
        command.append("--release")
    return command


def rendered_cargo_output(proc: subprocess.CompletedProcess[str]) -> str:
    rendered: list[str] = []
    for line in proc.stdout.splitlines():
        try:
            message = json.loads(line)
        except json.JSONDecodeError:
            rendered.append(line)
            continue
        compiler_message = message.get("message")
        diagnostic = (
            compiler_message.get("rendered") if isinstance(compiler_message, dict) else None
        )
        if diagnostic:
            rendered.append(diagnostic.rstrip())
    if proc.stderr:
        rendered.append(proc.stderr.rstrip())
    return "\n".join(rendered).strip()


def executable_from_messages(output: str, target_name: str) -> Path | None:
    executable: Path | None = None
    for line in output.splitlines():
        try:
            message = json.loads(line)
        except json.JSONDecodeError:
            continue
        target = message.get("target", {})
        candidate = message.get("executable")
        if (
            message.get("reason") == "compiler-artifact"
            and target.get("name") == target_name
            and candidate
        ):
            executable = Path(candidate)
    return executable


def find_suite_jar(
    config: SuiteConfig,
    target_dir: Path,
    release: bool,
) -> Path | None:
    profile = "release" if release else "debug"
    candidates = list(
        (target_dir / "jvm-unknown-jvm" / profile / "deps").glob(
            f"{config.cargo_test_target}-*.jar"
        )
    )
    return max(candidates, key=lambda path: path.stat().st_mtime_ns) if candidates else None


def listed_tests(
    config: SuiteConfig,
    jar: Path,
    timeout: float,
    *,
    ignored: bool = False,
) -> list[str]:
    command = [
        "java",
        *config.java_options,
        "-jar",
        str(jar),
        "--list",
        "--format",
        "terse",
    ]
    if ignored:
        command.append("--ignored")
    env = os.environ.copy()
    env.pop("__RUST_TEST_INVOKE", None)
    env.pop("__RUST_TEST_BENCH_BENCHMARKS", None)
    proc = run(command, timeout=timeout, env=env)
    output = proc.stdout + proc.stderr
    if proc.returncode != 0:
        raise RuntimeError(
            f"{config.name} --list failed with code {proc.returncode}:\n{output}"
        )
    tests = []
    for line in proc.stdout.splitlines():
        name, separator, kind = line.rpartition(": ")
        if separator and kind.strip() == "test" and name.strip():
            tests.append(name.strip())
    if not tests and not ignored:
        raise RuntimeError(
            f"the {config.name} harness listed no tests; JVM command-line arguments "
            "may not be reaching std::env::args"
        )
    return tests


def list_tests(
    config: SuiteConfig,
    jar: Path,
    timeout: float,
) -> tuple[list[str], set[str]]:
    tests = listed_tests(config, jar, timeout)
    ignored = set(listed_tests(config, jar, timeout, ignored=True))
    return tests, ignored


def decoded_timeout_output(value: str | bytes | None) -> str:
    if value is None:
        return ""
    if isinstance(value, bytes):
        return value.decode("utf-8", errors="replace")
    return value


def capped_output(value: str) -> str:
    if len(value) <= MAX_CAPTURE_CHARS:
        return value
    head = MAX_CAPTURE_CHARS // 8
    tail = MAX_CAPTURE_CHARS - head
    return value[:head] + "\n... output truncated ...\n" + value[-tail:]


def run_test_harness(
    config: SuiteConfig,
    jar: Path,
    tests: list[str],
    ignored_tests: set[str],
    default_ignored_tests: set[str],
    jobs: int,
    per_test_timeout: float,
    name_filter: str | None,
    include_upstream_ignored: bool,
) -> tuple[list[TestResult], int | None, float, str, str, bool]:
    """Run all selected tests in one JVM and decode libtest's output."""
    command = [
        "java",
        *config.java_options,
        "-jar",
        str(jar),
        "--format",
        "pretty",
        f"--test-threads={jobs}",
    ]
    if include_upstream_ignored:
        command.append("--include-ignored")
    if default_ignored_tests:
        command.append("--exact")
        if name_filter:
            command.extend(tests)
        for name in sorted(default_ignored_tests):
            command.extend(["--skip", name])
    elif name_filter:
        command.append(name_filter)

    runnable_count = len(tests) - len(ignored_tests)
    wave_count = max(1, math.ceil(runnable_count / jobs))
    harness_timeout = per_test_timeout * wave_count + HARNESS_STARTUP_GRACE_SECONDS
    env = os.environ.copy()
    env.pop("__RUST_TEST_INVOKE", None)
    env.pop("__RUST_TEST_BENCH_BENCHMARKS", None)
    started = time.monotonic()
    timed_out = False
    try:
        proc = run(command, timeout=harness_timeout, env=env)
    except subprocess.TimeoutExpired as error:
        timed_out = True
        returncode = None
        raw_stdout = decoded_timeout_output(error.stdout)
        raw_stderr = decoded_timeout_output(error.stderr)
    else:
        returncode = proc.returncode
        raw_stdout = proc.stdout
        raw_stderr = proc.stderr

    duration = time.monotonic() - started
    selected = set(tests)
    results_by_name = {
        name: TestResult(
            name=name,
            status="ignored",
            duration_seconds=0.0,
            returncode=0,
            stdout=f"ignored by rustc_codegen_jvm's default {config.name} policy",
            stderr="",
        )
        for name in default_ignored_tests
    }
    timeout_warnings: set[str] = set()
    test_name = r'(?:("(?:\\.|[^"\\])*")|(.+?))'
    result_line = re.compile(
        rf"^test\s+{test_name}\s*(?:- [^.]+\s+)?\.\.\.\s+"
        r"(ok|FAILED|ignored(?:,.*)?)(?:\s+<([0-9.]+)s>)?$"
    )
    timeout_line = re.compile(rf"^test\s+{test_name} has been running for over ")

    def matched_test_name(match: re.Match[str]) -> str:
        return json.loads(match.group(1)) if match.group(1) else match.group(2)

    for line in raw_stdout.splitlines():
        if timeout_match := timeout_line.match(line):
            timeout_warnings.add(matched_test_name(timeout_match))
            continue
        match = result_line.match(line)
        if match is None:
            continue
        name = matched_test_name(match)
        if name not in selected:
            continue
        outcome = match.group(3).split(",", 1)[0]
        status = {
            "ok": "passed",
            "FAILED": "failed",
            "ignored": "ignored",
        }.get(outcome)
        if status is None:
            continue
        results_by_name[name] = TestResult(
            name=name,
            status=status,
            duration_seconds=float(match.group(4) or 0.0),
            returncode=101 if status == "failed" else 0,
            stdout="",
            stderr="",
        )

    missing = [name for name in tests if name not in results_by_name]
    for name in missing:
        status = "timeout" if timed_out and name in timeout_warnings else "crashed"
        explanation = (
            "libtest reported that this test exceeded its time limit before the "
            "single JVM harness timed out"
            if status == "timeout"
            else "the single JVM harness terminated before reporting this test"
        )
        results_by_name[name] = TestResult(
            name=name,
            status=status,
            duration_seconds=0.0,
            returncode=returncode,
            stdout=explanation,
            stderr="",
        )

    return (
        [results_by_name[name] for name in tests],
        returncode,
        duration,
        capped_output(raw_stdout),
        capped_output(raw_stderr),
        timed_out,
    )


def counts_for(results: list[TestResult]) -> dict[str, int]:
    counts = {
        status: 0
        for status in ("passed", "failed", "crashed", "timeout", "ignored")
    }
    for result in results:
        counts[result.status] += 1
    return counts


def percentage(counts: dict[str, int]) -> float | None:
    measured = sum(
        counts[status] for status in ("passed", "failed", "crashed", "timeout")
    )
    return 100.0 * counts["passed"] / measured if measured else None


def markdown_summary(config: SuiteConfig, report: dict[str, Any]) -> str:
    lines = [f"# rustc_codegen_jvm {config.name}", ""]
    if report["status"] == "listed":
        lines.extend(
            [
                "Status: **test inventory listed successfully**",
                "",
                f"Selected: **{report['selected_tests']}** of "
                f"{report['listed_tests']} listed tests.",
            ]
        )
        return "\n".join(lines) + "\n"
    if report["status"] != "complete":
        lines.extend(
            [
                f"Status: **{report['status']}** (no score)",
                "",
                report.get("error", "The upstream harness could not be prepared."),
            ]
        )
        return "\n".join(lines) + "\n"

    counts = report["counts"]
    score = report["pass_percentage"]
    measured = report["measured_tests"]
    scope = "Filtered pass rate" if report.get("filter") else "Pass rate"
    score_text = f"{score:.2f}%" if score is not None else "no measured tests"
    lines.extend(
        [
            f"{scope}: **{score_text}**",
            "",
            "| Result | Count |",
            "|---|---:|",
            f"| Passed | {counts['passed']} |",
            f"| Failed | {counts['failed']} |",
            f"| Crashed | {counts['crashed']} |",
            f"| Timed out | {counts['timeout']} |",
            f"| Ignored | {counts['ignored']} |",
            f"| Measured | {measured} |",
            f"| Selected | {report['selected_tests']} |",
            f"| Listed | {report['listed_tests']} |",
        ]
    )
    return "\n".join(lines) + "\n"


def write_report(config: SuiteConfig, report: dict[str, Any], path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    summary = markdown_summary(config, report)
    path.with_suffix(".md").write_text(summary, encoding="utf-8")
    github_summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if github_summary and report.get("status") != "running":
        with Path(github_summary).open("a", encoding="utf-8") as destination:
            destination.write(summary)


def run_suite(config: SuiteConfig, argv: Sequence[str] | None = None) -> int:
    available_jobs = (
        os.process_cpu_count()
        if hasattr(os, "process_cpu_count")
        else os.cpu_count()
    ) or 1
    default_jobs = min(available_jobs, config.default_jobs or 4)
    parser = argparse.ArgumentParser(description=config.description)
    parser.add_argument("--release", action="store_true", help="Build in release mode")
    parser.add_argument("--filter", help="Only run test names containing this text")
    parser.add_argument(
        "--list", action="store_true", help="List target tests without running them"
    )
    parser.add_argument(
        "-j",
        "--jobs",
        type=int,
        default=default_jobs,
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        help="Seconds budgeted per test when calculating the one-JVM harness timeout",
    )
    parser.add_argument(
        "--list-timeout",
        type=float,
        default=180.0,
        help=f"Seconds allowed for each {config.name} discovery invocation",
    )
    parser.add_argument(
        "--build-timeout",
        type=float,
        default=1800.0,
        help="Seconds allowed for the harness build",
    )
    parser.add_argument(
        "--allow-test-failures",
        action="store_true",
        help="Return success after a completed measurement even when tests fail",
    )
    parser.add_argument(
        "--include-default-ignored",
        action="store_true",
        help=f"Run {config.name} ignored by default for pathological runtime cost",
    )
    parser.add_argument("--report", type=Path, help="JSON report path")
    args = parser.parse_args(argv)
    if args.jobs < 1:
        parser.error("--jobs must be at least 1")
    if args.timeout <= 0:
        parser.error("--timeout must be positive")
    if args.list_timeout <= 0:
        parser.error("--list-timeout must be positive")
    if args.build_timeout <= 0:
        parser.error("--build-timeout must be positive")

    mode = "release" if args.release else "debug"
    report_name = mode
    if args.filter:
        filter_id = hashlib.sha256(args.filter.encode("utf-8")).hexdigest()[:10]
        report_name += f"-filtered-{filter_id}"
    report_path = args.report or config.target_root / "reports" / f"{report_name}.json"
    report: dict[str, Any] = {
        "schema_version": 1,
        "suite": config.name,
        "status": "preparing",
        "profile": mode,
        "filter": args.filter,
        "generated_at": datetime.now(timezone.utc).isoformat(),
    }

    try:
        missing = [path for path in (TARGET_SPEC, TEST_CONFIG) if not path.exists()]
        if missing:
            raise RuntimeError("missing generated configuration; run `python3 build.py all`")
        sysroot, version, commit = rustc_info()
        stdlib_source, stdlib_overlay_hash = prepare_stdlib_source(sysroot, commit)
        source, source_hash, suite_patch_hash, lockfile = prepare_suite_source(
            config, sysroot, commit
        )
        manifest = source / "Cargo.toml"
        fingerprint, input_hashes = toolchain_fingerprint(
            config, commit, source_hash, lockfile
        )
        target_dir = config.target_root / "build" / mode / fingerprint[:16]
        removed_caches = prune_build_cache(target_dir.parent, target_dir)
        if removed_caches:
            print(
                f"🧹 Removed {removed_caches} stale {config.name} build cache(s).",
                flush=True,
            )
        java_version = run(["java", "-version"])
        report.update(
            {
                "rustc": version,
                "rustc_commit": commit,
                f"{config.name}_source_sha256": source_hash,
                f"{config.name}_patch_sha256": suite_patch_hash,
                "stdlib_overlay_sha256": stdlib_overlay_hash,
                "stdlib_source": str(stdlib_source.relative_to(ROOT)),
                "target_spec_sha256": sha256_file(TARGET_SPEC),
                "cargo_lock_sha256": sha256_file(lockfile),
                "cargo_lock_source": (
                    "standalone lock pinned to installed rust-src library/Cargo.lock"
                ),
                "toolchain_fingerprint": fingerprint,
                "input_hashes": input_hashes,
                "java": (java_version.stdout + java_version.stderr).strip(),
            }
        )

        print(f"🧪 Building upstream {config.name} for JVM ({mode})...", flush=True)
        build_env = os.environ.copy()
        build_env["__CARGO_TESTS_ONLY_SRC_ROOT"] = str(stdlib_source)
        build_env["CARGO_INCREMENTAL"] = "0"
        build = run(
            cargo_build_command(
                config, manifest, target_dir, args.release, args.jobs
            ),
            timeout=args.build_timeout,
            env=build_env,
        )
        build_log = config.target_root / "logs" / f"build-{mode}.log"
        build_log.parent.mkdir(parents=True, exist_ok=True)
        build_log.write_text(rendered_cargo_output(build) + "\n", encoding="utf-8")
        report["build_log"] = str(build_log.relative_to(ROOT))
        if build.returncode != 0:
            report.update(
                {
                    "status": "compile-failed",
                    "error": f"The official {config.name} harness did not compile, "
                    "so no pass percentage is available.",
                }
            )
            write_report(config, report, report_path)
            output = rendered_cargo_output(build)
            print(
                f"❌ {config.name} harness compilation failed; no score is available.",
                file=sys.stderr,
            )
            if output:
                print("\n".join(output.splitlines()[-80:]), file=sys.stderr)
            print(f"Report: {report_path}", file=sys.stderr)
            return 1

        jar = executable_from_messages(
            build.stdout, config.cargo_test_target
        ) or find_suite_jar(config, target_dir, args.release)
        if jar is None or not jar.exists():
            raise RuntimeError(
                f"Cargo succeeded but the {config.name} JAR could not be located"
            )
        tests, upstream_ignored_tests = list_tests(config, jar, args.list_timeout)
        report["listed_tests"] = len(tests)
        if args.filter:
            tests = [name for name in tests if args.filter in name]
            upstream_ignored_tests = {
                name for name in upstream_ignored_tests if args.filter in name
            }
        include_upstream_ignored = (
            args.include_default_ignored
            and config.include_upstream_ignored_with_defaults
        )
        ignored_tests = (
            set() if include_upstream_ignored else set(upstream_ignored_tests)
        )
        default_ignored_tests = (
            set()
            if args.include_default_ignored
            else set(tests).intersection(config.default_ignored_tests)
        )
        ignored_tests.update(default_ignored_tests)
        report["selected_tests"] = len(tests)
        report["upstream_ignored_tests"] = sorted(upstream_ignored_tests)
        report["default_ignored_tests"] = {
            name: config.default_ignored_tests[name]
            for name in sorted(default_ignored_tests)
        }

        if args.list:
            for name in tests:
                print(name)
            report.update({"status": "listed", "tests": tests})
            write_report(config, report, report_path)
            return 0
        if not tests:
            raise RuntimeError(f"no {config.name} matched the requested filter")

        measured_tests = len(tests) - len(ignored_tests)
        print(
            f"▶ Measuring {measured_tests} of {len(tests)} selected test(s) "
            f"in one JVM with {args.jobs} libtest worker thread(s)...",
            flush=True,
        )
        report.update({"status": "running", "results": []})
        write_report(config, report, report_path)

        (
            results,
            harness_returncode,
            harness_duration,
            harness_stdout,
            harness_stderr,
            harness_timed_out,
        ) = run_test_harness(
            config,
            jar,
            tests,
            ignored_tests,
            default_ignored_tests,
            args.jobs,
            args.timeout,
            args.filter,
            include_upstream_ignored,
        )
        for completed, result in enumerate(results, 1):
            print(f"[{completed}/{len(tests)}] {result.status:7} {result.name}")

        results.sort(key=lambda result: result.name)
        counts = counts_for(results)
        score = percentage(counts)
        measured_tests = sum(
            counts[status]
            for status in ("passed", "failed", "crashed", "timeout")
        )
        report.update(
            {
                "status": "complete",
                "counts": counts,
                "measured_tests": measured_tests,
                "pass_percentage": score,
                "results": [asdict(result) for result in results],
                "harness": {
                    "jvm_processes": 1,
                    "test_threads": args.jobs,
                    "duration_seconds": harness_duration,
                    "returncode": harness_returncode,
                    "timed_out": harness_timed_out,
                    "stdout": harness_stdout,
                    "stderr": harness_stderr,
                },
            }
        )
        write_report(config, report, report_path)
        print(markdown_summary(config, report).strip())
        print(f"Report: {report_path}")
        failures = counts["failed"] + counts["crashed"] + counts["timeout"]
        return 0 if args.allow_test_failures or failures == 0 else 1
    except (OSError, RuntimeError, subprocess.TimeoutExpired) as error:
        report.update({"status": "infrastructure-failed", "error": str(error)})
        write_report(config, report, report_path)
        print(f"❌ {error}", file=sys.stderr)
        print(f"Report: {report_path}", file=sys.stderr)
        return 1
