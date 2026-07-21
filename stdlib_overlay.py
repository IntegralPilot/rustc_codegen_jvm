from __future__ import annotations

import hashlib
import json
import os
import shutil
import subprocess
from pathlib import Path


ROOT = Path(__file__).resolve().parent
STDLIB_OVERLAY_ROOT = ROOT / "std" / "overlay"
STDLIB_PATCH_ROOT = ROOT / "std" / "patches"
STDLIB_BUILD_ROOT = ROOT / "target" / "stdlib-overlay"


def run(command: list[str], *, cwd: Path = ROOT) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace",
    )


def rustc_info() -> tuple[Path, str, str]:
    sysroot_proc = run(["rustc", "--print", "sysroot"])
    version_proc = run(["rustc", "-vV"])
    if sysroot_proc.returncode != 0 or version_proc.returncode != 0:
        raise RuntimeError("could not query the active Rust toolchain")
    sysroot = Path(sysroot_proc.stdout.strip())
    version = version_proc.stdout.strip()
    commit = next(
        (
            line.split(":", 1)[1].strip()
            for line in version.splitlines()
            if line.startswith("commit-hash:")
        ),
        "unknown",
    )
    return sysroot, version, commit


def stdlib_overlay_digest() -> str:
    digest = hashlib.sha256()
    for root in (STDLIB_OVERLAY_ROOT, STDLIB_PATCH_ROOT):
        if not root.is_dir():
            raise RuntimeError(f"missing JVM std overlay inputs at {root}")
        for path in sorted(candidate for candidate in root.rglob("*") if candidate.is_file()):
            digest.update(root.name.encode("utf-8"))
            digest.update(path.relative_to(root).as_posix().encode("utf-8"))
            digest.update(path.read_bytes())
    return digest.hexdigest()


def prepare_stdlib_source(sysroot: Path, commit: str) -> tuple[Path, str]:
    installed = sysroot / "lib" / "rustlib" / "src" / "rust" / "library"
    if not (installed / "Cargo.toml").exists() or not (installed / "Cargo.lock").exists():
        raise RuntimeError(
            f"standard-library sources were not found at {installed}; install them with "
            "`rustup component add rust-src`"
        )

    overlay_hash = stdlib_overlay_digest()
    destination = (
        STDLIB_BUILD_ROOT / "rust-src" / f"{commit[:12]}-{overlay_hash[:12]}" / "library"
    )
    marker = destination.parent / "overlay.json"
    expected_marker = {
        "schema_version": 1,
        "rustc_commit": commit,
        "overlay_sha256": overlay_hash,
    }
    if destination.is_dir() and marker.is_file():
        try:
            if json.loads(marker.read_text(encoding="utf-8")) == expected_marker:
                return destination, overlay_hash
        except json.JSONDecodeError:
            pass

    temporary = destination.parent.with_name(destination.parent.name + f".tmp-{os.getpid()}")
    shutil.rmtree(temporary, ignore_errors=True)
    temporary_library = temporary / "library"
    temporary.parent.mkdir(parents=True, exist_ok=True)
    try:
        shutil.copytree(installed, temporary_library)
        shutil.copytree(STDLIB_OVERLAY_ROOT, temporary_library, dirs_exist_ok=True)
        patch_directory = temporary_library.relative_to(ROOT)
        for patch in sorted(STDLIB_PATCH_ROOT.glob("*.patch")):
            applied = run(
                [
                    "git",
                    "apply",
                    "--whitespace=nowarn",
                    f"--directory={patch_directory}",
                    str(patch.resolve()),
                ]
            )
            if applied.returncode != 0:
                raise RuntimeError(
                    f"JVM std overlay patch {patch.name} does not apply to rustc {commit}:\n"
                    + applied.stdout
                    + applied.stderr
                )
        (temporary / "overlay.json").write_text(
            json.dumps(expected_marker, indent=2) + "\n", encoding="utf-8"
        )
        if destination.parent.exists():
            shutil.rmtree(destination.parent)
        temporary.rename(destination.parent)
    except Exception:
        shutil.rmtree(temporary, ignore_errors=True)
        raise
    return destination, overlay_hash


def main() -> None:
    sysroot, _, commit = rustc_info()
    source, _ = prepare_stdlib_source(sysroot, commit)
    print(source)


if __name__ == "__main__":
    main()
