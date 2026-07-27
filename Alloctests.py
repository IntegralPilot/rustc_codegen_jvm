#!/usr/bin/env python3
"""Build and measure upstream Rust alloctests with rustc_codegen_jvm."""

from stdlib_test_harness import ROOT, SuiteConfig, run_suite


DEFAULT_IGNORED_TESTS = {
    "sort::tests::stable::correct_dyn_val_ascending": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::stable::correct_dyn_val_descending": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::stable::correct_dyn_val_random": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::stable::correct_dyn_val_random_d2": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::stable::correct_dyn_val_random_d20": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::stable::correct_dyn_val_random_s95": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::stable::correct_dyn_val_random_z1": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::stable::correct_dyn_val_saw_mixed": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::stable::stability_legacy": (
        "the legacy stable-sort stability check currently fails"
    ),
    "sort::tests::unstable::correct_dyn_val_ascending": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::unstable::correct_dyn_val_descending": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::unstable::correct_dyn_val_random": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::unstable::correct_dyn_val_random_d2": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::unstable::correct_dyn_val_random_d20": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::unstable::correct_dyn_val_random_s95": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::unstable::correct_dyn_val_random_z1": (
        "sorting fat trait-object values currently aborts"
    ),
    "sort::tests::unstable::correct_dyn_val_saw_mixed": (
        "sorting fat trait-object values currently aborts"
    ),
    "str::strslice_issue_16589": (
        "upstream marks this exhaustive substring test too slow for Miri"
    ),
    "str::test_strslice_contains": (
        "upstream marks this substring permutation test too slow for Miri"
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
