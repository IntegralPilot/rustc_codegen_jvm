from __future__ import annotations

import hashlib
import os
import re
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parent
TARGET_SPEC = ROOT / "jvm-unknown-unknown.json"
TEST_TARGET_DIR = ROOT / "target" / "test-suite"
TEST_CONFIG = ROOT / "config.toml"
CORE_BUILD_MANIFEST = ROOT / "tests" / "support" / "core_build" / "Cargo.toml"
TEST_TYPES = ("binary", "multicrate", "integration")
CACHE_TAG = (
    "Signature: 8a477f597d28d172789f06886806bc55\n"
    "# This file is a cache directory tag created by rustc_codegen_jvm.\n"
)


@dataclass(frozen=True)
class TestCase:
    directory: Path
    kind: str
    package_name: str

    @property
    def name(self) -> str:
        return self.directory.name

    @property
    def artifact_name(self) -> str:
        return self.package_name.replace("-", "_")


def cpu_count() -> int:
    return os.cpu_count() or 1


def resolve_workers(requested: int | None) -> int:
    if requested is not None:
        if requested < 1:
            raise ValueError("--jobs must be at least 1")
        return requested
    return min(cpu_count(), 4)


def cargo_jobs(workers: int) -> int:
    return max(1, cpu_count() // workers)


def package_name(manifest: Path) -> str:
    in_package = False
    for line in manifest.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if stripped.startswith("["):
            in_package = stripped == "[package]"
            continue
        if in_package:
            match = re.fullmatch(r'name\s*=\s*"([^"]+)"', stripped)
            if match:
                return match.group(1)
    raise ValueError(f"missing package name in {manifest}")


def discover_tests(
    only_run: set[str] | None = None,
    dont_run: set[str] | None = None,
) -> list[TestCase]:
    excluded = dont_run or set()
    tests: list[TestCase] = []
    for kind in TEST_TYPES:
        parent = ROOT / "tests" / kind
        if not parent.is_dir():
            continue
        for directory in sorted(path for path in parent.iterdir() if path.is_dir()):
            manifest = directory / "Cargo.toml"
            if not manifest.exists():
                continue
            if only_run is not None and directory.name not in only_run:
                continue
            if directory.name in excluded:
                continue
            tests.append(TestCase(directory, kind, package_name(manifest)))
    return tests


def validate_configuration() -> None:
    missing = [path for path in (TARGET_SPEC, TEST_CONFIG) if not path.exists()]
    if missing:
        paths = ", ".join(str(path) for path in missing)
        raise RuntimeError(
            f"missing generated test configuration: {paths}; run `python3 build.py all`"
        )


def prepare_shared_cache() -> bool:
    """Invalidate all test artifacts when the compiler toolchain inputs change."""
    validate_configuration()
    inputs = [
        TARGET_SPEC,
        TEST_CONFIG,
        ROOT / "tests" / "support" / "test_prelude.rs",
        ROOT / "runtime" / "build" / "libs" / "runtime-0.1.0.jar",
        ROOT / "java-linker" / "target" / "release" / (
            "java-linker.exe" if os.name == "nt" else "java-linker"
        ),
    ]
    backend_candidates = list((ROOT / "target" / "release").glob("*rustc_codegen_jvm.*"))
    inputs.extend(path for path in backend_candidates if path.suffix in {".dll", ".dylib", ".so"})

    digest = hashlib.sha256()
    for path in sorted(inputs):
        if not path.exists():
            raise RuntimeError(f"missing test toolchain input: {path}; run `python3 build.py all`")
        digest.update(str(path).encode())
        digest.update(path.read_bytes())
    fingerprint = digest.hexdigest()
    marker = TEST_TARGET_DIR / ".harness-fingerprint"
    if marker.exists() and marker.read_text(encoding="utf-8") == fingerprint:
        (TEST_TARGET_DIR / "CACHEDIR.TAG").write_text(CACHE_TAG, encoding="utf-8")
        return False

    if TEST_TARGET_DIR.exists():
        shutil.rmtree(TEST_TARGET_DIR)
    TEST_TARGET_DIR.mkdir(parents=True)
    (TEST_TARGET_DIR / "CACHEDIR.TAG").write_text(CACHE_TAG, encoding="utf-8")
    marker.write_text(fingerprint, encoding="utf-8")
    return True


def cargo_build_command(manifest: Path, release: bool, jobs: int) -> list[str]:
    command = [
        "cargo",
        "build",
        "--manifest-path",
        str(manifest),
        "--target",
        str(TARGET_SPEC),
        "-Zjson-target-spec",
        "-Zbuild-std=core,compiler_builtins,alloc",
        "--target-dir",
        str(TEST_TARGET_DIR),
        "--config",
        str(TEST_CONFIG),
        "--jobs",
        str(jobs),
    ]
    if release:
        command.append("--release")
    return command


def run_command(
    command: list[str],
    *,
    cwd: Path | None = None,
    env: dict[str, str] | None = None,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        cwd=cwd or ROOT,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace",
    )


def prime_core(release: bool) -> subprocess.CompletedProcess[str]:
    validate_configuration()
    return run_command(
        cargo_build_command(CORE_BUILD_MANIFEST, release, cpu_count()),
    )


def build_test(
    test: TestCase,
    release: bool,
    jobs: int,
    *,
    env: dict[str, str] | None = None,
) -> subprocess.CompletedProcess[str]:
    return run_command(
        cargo_build_command(test.directory / "Cargo.toml", release, jobs),
        env=env,
    )


def jar_path(test: TestCase, release: bool) -> Path:
    profile = "release" if release else "debug"
    profile_dir = TEST_TARGET_DIR / "jvm-unknown-unknown" / profile
    direct = profile_dir / f"{test.artifact_name}.jar"
    if direct.exists():
        return direct

    candidates = list((profile_dir / "deps").glob(f"{test.artifact_name}-*.jar"))
    if candidates:
        return max(candidates, key=lambda path: path.stat().st_mtime_ns)
    return direct
