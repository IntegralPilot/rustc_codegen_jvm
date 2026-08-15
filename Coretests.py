#!/usr/bin/env python3
"""Build and measure upstream Rust coretests with rustc_codegen_jvm."""

from stdlib_test_harness import ROOT, SuiteConfig, run_suite


DEFAULT_IGNORED_TESTS = {
    "fmt::num::test_format_int_exp_precision": (
        "takes about 36 seconds in isolation, roughly as long as the rest of coretests"
    ),
    "slice::select_nth_unstable": (
        "exceeds two minutes in isolation during cubic slice-selection validation"
    ),
    "unicode::to_casefold": (
        "takes about 102 seconds in isolation while scanning Unicode code points"
    ),
}


CONFIG = SuiteConfig(
    name="coretests",
    description="Measure rustc_codegen_jvm against upstream Rust coretests",
    source_directory="coretests",
    cargo_test_target="coretests",
    target_root=ROOT / "target" / "coretests",
    patch_root=ROOT / "coretests" / "coretests-patches",
    build_schema="coretests-build-v2",
    default_ignored_tests=DEFAULT_IGNORED_TESTS,
    java_options=("-XX:+UseParallelGC",),
    default_jobs=8,
)


def main() -> int:
    return run_suite(CONFIG)


if __name__ == "__main__":
    raise SystemExit(main())
