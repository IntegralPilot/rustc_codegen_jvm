#!/usr/bin/env python3
"""Build and measure upstream Rust alloctests with rustc_codegen_jvm."""

from stdlib_test_harness import ROOT, SuiteConfig, run_suite


DEFAULT_IGNORED_TESTS = {
    "str::strslice_issue_16589": "prohibitively slow exhaustive substring stress test",
    "str::test_strslice_contains": "prohibitively slow substring permutation stress test",
}


CONFIG = SuiteConfig(
    name="alloctests",
    description="Measure rustc_codegen_jvm against upstream Rust alloctests",
    source_directory="alloctests",
    cargo_test_target="alloctests",
    target_root=ROOT / "target" / "alloctests",
    patch_root=ROOT / "alloctests" / "alloctests-patches",
    build_schema="alloctests-build-v1",
    default_ignored_tests=DEFAULT_IGNORED_TESTS,
    java_options=("-XX:+UseParallelGC",),
    include_upstream_ignored_with_defaults=True,
)


def main() -> int:
    return run_suite(CONFIG)


if __name__ == "__main__":
    raise SystemExit(main())
