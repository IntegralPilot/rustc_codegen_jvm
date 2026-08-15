#!/usr/bin/env python3
"""Build and measure upstream Rust alloctests with rustc_codegen_jvm."""

from stdlib_test_harness import ROOT, SuiteConfig, run_suite


# Remaining tests over 20 seconds; none uses the reduced sort length table.
SLOW_TEST_SECONDS = {
    "sort::tests::stable::stability_legacy": 29.173,
    "str::utf8_char_counts": 48.417,
    "vec_deque::test_append_permutations": 25.902,
}


DEFAULT_IGNORED_TESTS = {
    name: f"took {seconds:.1f} seconds in the six-thread full-suite profile"
    for name, seconds in SLOW_TEST_SECONDS.items()
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
    default_jobs=6,
)


def main() -> int:
    return run_suite(CONFIG)


if __name__ == "__main__":
    raise SystemExit(main())
