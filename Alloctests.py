#!/usr/bin/env python3
"""Build and measure upstream Rust alloctests with rustc_codegen_jvm."""

from stdlib_test_harness import ROOT, SuiteConfig, run_suite


DEFAULT_IGNORED_TESTS = {
    "str::slice_index::simple_big": "upstream marks this allocation stress test too slow for Miri",
    "str::strslice_issue_16589": "upstream marks this exhaustive substring test too slow for Miri",
    "str::test_chars_decoding": "exhaustively checks every Unicode scalar value",
    "str::test_chars_rev_decoding": "exhaustively checks every Unicode scalar value",
    "str::test_strslice_contains": (
        "upstream marks this substring permutation test too slow for Miri"
    ),
    "str::test_unsafe_slice": "upstream marks this test too slow for Miri",
    "string::test_try_reserve": "requires signalling an intentionally enormous allocation failure",
    "string::test_try_reserve_exact": (
        "requires signalling an intentionally enormous allocation failure"
    ),
    "string::test_try_with_capacity": (
        "requires signalling an intentionally enormous allocation failure"
    ),
    "vec::test_try_reserve": "requires signalling an intentionally enormous allocation failure",
    "vec::test_try_reserve_exact": (
        "requires signalling an intentionally enormous allocation failure"
    ),
    "vec::test_try_with_capacity": (
        "requires signalling an intentionally enormous allocation failure"
    ),
    "vec_deque::test_try_reserve": (
        "requires signalling an intentionally enormous allocation failure"
    ),
    "vec_deque::test_try_reserve_exact": (
        "requires signalling an intentionally enormous allocation failure"
    ),
    "vec_deque::test_try_with_capacity": (
        "requires signalling an intentionally enormous allocation failure"
    ),
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
    include_upstream_ignored_with_defaults=True,
    default_jobs=1,
)


def main() -> int:
    return run_suite(CONFIG)


if __name__ == "__main__":
    raise SystemExit(main())
