#!/usr/bin/env python3
"""Build and measure upstream Rust coretests with rustc_codegen_jvm."""

from stdlib_test_harness import ROOT, SuiteConfig, run_suite


DEFAULT_IGNORED_TESTS = {
    "fmt::num::test_format_int_exp_precision": (
        "takes over three CPU-minutes in JVM integer-formatting pointer operations"
    ),
    "num::flt2dec::random::exact_f32_random_equivalence_test": (
        "takes over a minute when run concurrently with the complete coretests suite"
    ),
    "num::flt2dec::random::exact_f64_random_equivalence_test": (
        "takes several CPU-minutes in JVM aggregate pointer-view synchronization"
    ),
    "num::flt2dec::random::shortest_random_equivalence_test": (
        "takes several CPU-minutes in JVM aggregate pointer-view synchronization"
    ),
    "num::flt2dec::random::shortest_f16_exhaustive_equivalence_test": (
        "takes over a minute when run concurrently with the complete coretests suite"
    ),
    "num::flt2dec::strategy::dragon::exact_sanity_test": (
        "takes more than six CPU-minutes in JVM Big32x40 pointer-view synchronization"
    ),
    "num::flt2dec::strategy::grisu::exact_sanity_test": (
        "takes more than six CPU-minutes in JVM Big32x40 pointer-view synchronization"
    ),
    "slice::select_nth_unstable": (
        "performs cubic validation through billions of addressable slice element reads"
    ),
    "unicode::to_casefold": (
        "exhaustively scans Unicode code points and is pointer-bound on the JVM"
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
    default_jobs=4,
)


def main() -> int:
    return run_suite(CONFIG)


if __name__ == "__main__":
    raise SystemExit(main())
