#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import os
import shutil
import urllib.request
import zipfile
from pathlib import Path

VERSION = "2.4.10"
SHA256 = "473dd66c7a3ef4b182065b3da670466c1bf2773a9dbb0ed8b33a39fe9d4f876d"
URL = (
    "https://github.com/JetBrains/kotlin/releases/download/"
    f"v{VERSION}/kotlin-compiler-{VERSION}.zip"
)
ROOT = Path(__file__).resolve().parents[2]
INSTALL_ROOT = ROOT / "target" / "tools" / f"kotlin-{VERSION}"


def extract_bundle(bundle: zipfile.ZipFile, destination: Path) -> None:
    for member in bundle.infolist():
        extracted = Path(bundle.extract(member, destination))
        if os.name == "nt" or not extracted.is_file():
            continue
        executable_bits = (member.external_attr >> 16) & 0o111
        if executable_bits:
            extracted.chmod(extracted.stat().st_mode | executable_bits)


def main() -> int:
    parser = argparse.ArgumentParser(description="Install the Kotlin compiler used by tests")
    parser.add_argument(
        "--github-path",
        action="store_true",
        help="Append kotlinc's bin directory to the GitHub Actions PATH file",
    )
    args = parser.parse_args()

    executable = "kotlinc.bat" if os.name == "nt" else "kotlinc"
    bin_directory = INSTALL_ROOT / "kotlinc" / "bin"
    if not (bin_directory / executable).is_file():
        archive = INSTALL_ROOT.with_suffix(".zip")
        archive.parent.mkdir(parents=True, exist_ok=True)
        print(f"Downloading Kotlin compiler {VERSION}...")
        with urllib.request.urlopen(URL) as response, archive.open("wb") as output:
            shutil.copyfileobj(response, output)
        actual = hashlib.sha256(archive.read_bytes()).hexdigest()
        if actual != SHA256:
            archive.unlink(missing_ok=True)
            raise SystemExit(
                f"Kotlin compiler checksum mismatch: expected {SHA256}, found {actual}"
            )
        INSTALL_ROOT.mkdir(parents=True, exist_ok=True)
        with zipfile.ZipFile(archive) as bundle:
            extract_bundle(bundle, INSTALL_ROOT)
        archive.unlink()

    if args.github_path:
        github_path_file = Path(os.environ["GITHUB_PATH"])
        with github_path_file.open("a", encoding="utf-8") as github_path:
            github_path.write(f"{bin_directory}\n")
    print(bin_directory)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
