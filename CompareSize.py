#!/usr/bin/env python3
import os
import subprocess
import sys
import argparse
import time
import glob # Needed for finding pattern-based JARs

def run_command(cmd, cwd=None, check=False):
    """Runs a command and returns its process object."""
    print(f"|------ Running: {' '.join(cmd)} {f'(in {cwd})' if cwd else ''}")
    proc = subprocess.run(cmd, cwd=cwd,
                          stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE,
                          text=True,
                          encoding='utf-8', errors='replace')
    if check and proc.returncode != 0:
        print(f"❌ Command {' '.join(cmd)} failed (code {proc.returncode})")
        print("------ STDOUT ------")
        print(proc.stdout)
        print("------ STDERR ------", file=sys.stderr)
        print(proc.stderr, file=sys.stderr)
        print("--------------------", file=sys.stderr)
        sys.exit(proc.returncode)
    return proc

def find_final_jar(test_dir):
    """
    Finds the final output JAR for a given test directory.
    Prioritizes the location suggested by the hint script.
    """
    test_name = os.path.basename(test_dir)
    possible_locations = []

    # Prioritize specific target locations first (release then debug)
    for target_mode in ["release", "debug"]:
        # Path suggested by hint script
        jvm_target_path = os.path.join(test_dir, "target", "jvm-unknown-unknown", target_mode, f"{test_name}.jar")
        if os.path.isfile(jvm_target_path):
            print(f"|------ Found JAR at specific JVM target path: {jvm_target_path}")
            return jvm_target_path

        # Check deps directory (less specific, but common)
        deps_dir = os.path.join(test_dir, "target", target_mode, "deps")
        if os.path.isdir(deps_dir):
            # Look for JARs matching the test name pattern
            pattern = os.path.join(deps_dir, f"{test_name}-*.jar")
            matches = glob.glob(pattern)
            if matches:
                 # Sort to get a deterministic result if multiple matches (e.g., by timestamp or name)
                matches.sort()
                print(f"|------ Found JAR in deps dir: {matches[0]} (matched pattern {pattern})")
                return matches[0] # Return the first match

            # Fallback: Check for a JAR named exactly like the test (less likely in deps)
            exact_deps_path = os.path.join(deps_dir, f"{test_name}.jar")
            if os.path.isfile(exact_deps_path):
                print(f"|------ Found JAR in deps dir (exact name): {exact_deps_path}")
                return exact_deps_path

    # If not found in prioritized locations, return None
    print(f"|------ ⚠️ Could not find final JAR for test '{test_name}' in expected locations.")
    return None


def measure_jar_size(jar_path):
    """Measures the file size of the specified JAR file."""
    if not jar_path or not os.path.isfile(jar_path):
        # find_final_jar already prints a warning if it returns None
        return None
    try:
        size = os.path.getsize(jar_path)
        return size
    except FileNotFoundError:
        # This case should be rare if os.path.isfile passed, but handle anyway
        print(f"|------ ⚠️ File not found during size measurement: {jar_path}")
        return None
    except OSError as e:
        print(f"|------ ⚠️ OS error getting size for {jar_path}: {e}")
        return None


def collect_tests(binary_dir, only_run, dont_run):
    """Collects list of test directories based on filters and skip flag."""
    try:
        all_dirs = sorted([d for d in os.listdir(binary_dir)
                           if os.path.isdir(os.path.join(binary_dir, d))])
    except FileNotFoundError:
        print(f"❌ Error: Test directory '{binary_dir}' not found.")
        sys.exit(1)

    candidate_tests = [os.path.join(binary_dir, d) for d in all_dirs]
    if only_run:
        keep = set(n.strip() for n in only_run.split(","))
        candidate_tests = [t for t in candidate_tests if os.path.basename(t) in keep]
    if dont_run:
        skip = set(n.strip() for n in dont_run.split(","))
        candidate_tests = [t for t in candidate_tests if os.path.basename(t) not in skip]

    final_tests = []
    skip_flag_name = "use_target_json.flag"
    for test_dir in candidate_tests:
        flag_path = os.path.join(test_dir, skip_flag_name)
        if not os.path.exists(flag_path):
            final_tests.append(test_dir)

    if not final_tests:
        print(f"❌ No tests remain to be processed in '{binary_dir}' after filtering.")
        print("Continuing with zero tests.")

    return final_tests

def stash_based(tests, binary_dir):
    """Compares working dir vs. stashed state, measures JAR sizes and per-test build times."""
    # Phase 1A: build main project WITH changes
    print("|--- 🛠️ Building main project WITH unstaged changes…")
    run_command(["make", "clean"], check=True)
    run_command(["make", "all"],   check=True)

    # Phase 1B: build each test and time it
    times_with = {}
    print("|--- ⏱️ Building tests WITH timing…")
    for t in tests:
        name = os.path.basename(t)
        print(f"|---- Building test: {name}")
        t0 = time.time()
        # Note: The original script didn't specify --release or --target here.
        # The actual JAR location might depend on how 'make all' configures things.
        run_command(["cargo", "clean"], cwd=t, check=True)
        run_command(["cargo", "build"], cwd=t, check=True)
        elapsed = time.time() - t0
        times_with[name] = elapsed
        print(f"|------ [{name}] build took {elapsed:.2f} sec")

    # Measure JAR sizes WITH changes
    sizes_with = {}
    print("\n|--- 📏 Measuring JAR sizes WITH changes...")
    for t in tests:
        name = os.path.basename(t)
        jar_path = find_final_jar(t) # Use the new finder
        sz = measure_jar_size(jar_path) # Use the new measurement function
        sizes_with[name] = sz
        status = f"{sz} bytes" if sz is not None else 'N/A (JAR not found/error)'
        print(f"|------ [{name}] WITH: {status} (path: {jar_path if jar_path else 'None'})")

    # Stash working tree if needed
    print("\n|--- 🧹 Stashing working tree…")
    status_proc = run_command(["git", "status", "--porcelain"])
    if status_proc.stdout.strip():
        run_command(["git", "stash", "push", "--include-untracked", "--quiet", "-m", "jar_size_script_stash"], check=True)
        stashed = True
    else:
        print("|------ Working directory clean, nothing to stash.")
        stashed = False

    # Phase 2A: build main project at HEAD
    print("\n|--- 🛠️ Building main project at HEAD (no changes)…")
    run_command(["make", "clean"], check=True)
    run_command(["make", "all"],   check=True)

    # Phase 2B: build each test at HEAD and time it
    times_without = {}
    print("|--- ⏱️ Building tests WITHOUT timing…")
    for t in tests:
        name = os.path.basename(t)
        print(f"|---- Building test: {name}")
        t0 = time.time()
        run_command(["cargo", "clean"], cwd=t, check=True)
        run_command(["cargo", "build"], cwd=t, check=True)
        elapsed = time.time() - t0
        times_without[name] = elapsed
        print(f"|------ [{name}] build took {elapsed:.2f} sec")

    # Measure JAR sizes WITHOUT changes
    sizes_without = {}
    print("\n|--- 📏 Measuring JAR sizes WITHOUT changes (HEAD)...")
    for t in tests:
        name = os.path.basename(t)
        jar_path = find_final_jar(t) # Use the new finder
        sz = measure_jar_size(jar_path) # Use the new measurement function
        sizes_without[name] = sz
        status = f"{sz} bytes" if sz is not None else 'N/A (JAR not found/error)'
        print(f"|------ [{name}] WITHOUT: {status} (path: {jar_path if jar_path else 'None'})")

    # Restore stashed changes
    if stashed:
        print("\n|--- 🔄 Restoring working tree…")
        stash_list = run_command(["git", "stash", "list"]).stdout
        if "jar_size_script_stash" in stash_list:
            run_command(["git", "stash", "pop", "--quiet"])
        else:
            print("|------ Script stash not found.")

    # Report size diffs
    print("\n|--- 📊 JAR Size comparison (stash-based):")
    failed_measurements = 0
    for name in sorted(os.path.basename(t) for t in tests):
        w = sizes_with.get(name)
        wo = sizes_without.get(name)
        if w is None or wo is None:
            print(f"|---- [{name}] ⚠️ skipped (measurement failed or JAR not found)")
            failed_measurements += 1
            continue
        delta = w - wo
        change_desc = f"increase {delta:+}" if delta > 0 else f"decrease {delta}" if delta < 0 else "no change"
        print(f"|---- [{name}] {change_desc} ({wo:,} → {w:,} bytes)") # Added commas for readability

    if failed_measurements:
        print(f"|---- ⚠️ {failed_measurements} test(s) had measurement failures or missing JARs.")

    # Per-test build-time comparison
    print("\n|--- ⏱️ Per-test build time (WITH vs WITHOUT):")
    for name in sorted(times_with):
        w = times_with[name]
        wo = times_without.get(name)
        if wo is None:
            print(f"|---- [{name}] ⚠️ missing WITHOUT timing")
            continue
        delta = w - wo
        sign = "+" if delta >= 0 else ""
        print(f"|---- [{name}] WITH {w:.2f}s vs WITHOUT {wo:.2f}s → EXTRA {sign}{delta:.2f}s")

    return True

def commit_based(tests, binary_dir, n):
    """Compares HEAD vs HEAD~N, measures JAR sizes and per-test build times."""
    print("|--- 🧹 Stashing working-tree changes (if any) for clean checkout…")
    stash_message = "jar_size_script_stash_commit"
    status_proc = run_command(["git", "status", "--porcelain"])
    if status_proc.stdout.strip():
        run_command(["git", "stash", "push", "--include-untracked", "--quiet", "-m", stash_message], check=True)
        stashed = True
    else:
        print("|------ Working directory clean, nothing to stash.")
        stashed = False

    orig_head = run_command(["git", "rev-parse", "HEAD"], check=True).stdout.strip()
    print(f"|--- Current HEAD is: {orig_head[:10]}...")

    try:
        target_ref = f"HEAD~{n}"
        target_hash = run_command(["git", "rev-parse", "--verify", target_ref], check=True).stdout.strip()
        print(f"|--- Target commit {target_ref} resolved to {target_hash[:10]}...")
    except SystemExit:
        print(f"❌ Failed to verify {target_ref}. Aborting.")
        if stashed:
            print("|--- 🔄 Popping stash before exit…")
            run_command(["git", "stash", "pop", "--quiet"])
        sys.exit(1)

    # Phase 1A: build main project at HEAD
    print(f"\n|--- 🛠️ Building main project at HEAD ({orig_head[:10]})…")
    run_command(["make", "clean"], check=True)
    run_command(["make", "all"],   check=True)

    # Phase 1B: build & time each test at HEAD
    times_head = {}
    print("|--- ⏱️ Building tests at HEAD…")
    for t in tests:
        name = os.path.basename(t)
        print(f"|---- Building test: {name}")
        t0 = time.time()
        run_command(["cargo", "clean"], cwd=t, check=True)
        run_command(["cargo", "build"], cwd=t, check=True)
        times_head[name] = time.time() - t0
        print(f"|------ [{name}] HEAD build took {times_head[name]:.2f} sec")

    # Measure JAR sizes at HEAD
    sizes_head = {}
    print("\n|--- 📏 Measuring JAR sizes at HEAD...")
    for t in tests:
        name = os.path.basename(t)
        jar_path = find_final_jar(t) # Use the new finder
        sz = measure_jar_size(jar_path) # Use the new measurement function
        sizes_head[name] = sz
        status = f"{sz} bytes" if sz is not None else 'N/A (JAR not found/error)'
        print(f"|------ [{name}] HEAD: {status} (path: {jar_path if jar_path else 'None'})")

    # Checkout old commit
    print(f"\n|--- 🔨 Checking out {target_hash[:10]} (HEAD~{n})…")
    run_command(["git", "checkout", target_hash, "--quiet"], check=True)

    # Phase 2A: build main project at HEAD~N
    print(f"\n|--- 🛠️ Building main project at HEAD~{n}…")
    run_command(["make", "clean"], check=True)
    run_command(["make", "all"],   check=True)

    # Phase 2B: build & time each test at HEAD~N
    times_old = {}
    print(f"|--- ⏱️ Building tests at HEAD~{n}…")
    for t in tests:
        name = os.path.basename(t)
        print(f"|---- Building test: {name}")
        t0 = time.time()
        run_command(["cargo", "clean"], cwd=t, check=True)
        run_command(["cargo", "build"], cwd=t, check=True)
        times_old[name] = time.time() - t0
        print(f"|------ [{name}] HEAD~{n} build took {times_old[name]:.2f} sec")

    # Measure JAR sizes at HEAD~N
    sizes_old = {}
    print(f"\n|--- 📏 Measuring JAR sizes at HEAD~{n}…")
    for t in tests:
        name = os.path.basename(t)
        jar_path = find_final_jar(t) # Use the new finder
        sz = measure_jar_size(jar_path) # Use the new measurement function
        sizes_old[name] = sz
        status = f"{sz} bytes" if sz is not None else 'N/A (JAR not found/error)'
        print(f"|------ [{name}] HEAD~{n}: {status} (path: {jar_path if jar_path else 'None'})")

    # Restore original HEAD
    print(f"\n|--- 🔄 Restoring HEAD ({orig_head[:10]})…")
    run_command(["git", "checkout", orig_head, "--quiet"], check=True)
    if stashed:
        print("|--- 🔄 Popping stash…")
        stash_list = run_command(["git", "stash", "list"]).stdout
        if stash_message in stash_list:
            run_command(["git", "stash", "pop", "--quiet"])

    # Report size diffs
    print(f"\n|--- 📊 JAR Size comparison (HEAD vs HEAD~{n}):")
    failed_measurements = 0
    for name in sorted(os.path.basename(t) for t in tests):
        h = sizes_head.get(name)
        o = sizes_old.get(name)
        if h is None or o is None:
            print(f"|---- [{name}] ⚠️ skipped (measurement failed or JAR not found)")
            failed_measurements += 1
            continue
        delta = h - o
        change_desc = f"increase {delta:+}" if delta > 0 else f"decrease {delta}" if delta < 0 else "no change"
        print(f"|---- [{name}] {change_desc} ({o:,} → {h:,} bytes)") # Added commas for readability

    if failed_measurements:
        print(f"|---- ⚠️ {failed_measurements} test(s) had measurement failures or missing JARs.")

    # Per-test build-time comparison
    print(f"\n|--- ⏱️ Per-test build time (HEAD vs HEAD~{n}):")
    for name in sorted(times_head):
        h = times_head[name]
        o = times_old.get(name)
        if o is None:
            print(f"|---- [{name}] ⚠️ missing HEAD~{n} timing")
            continue
        delta = h - o
        sign = "+" if delta >= 0 else ""
        print(f"|---- [{name}] HEAD {h:.2f}s vs HEAD~{n} {o:.2f}s → EXTRA {sign}{delta:.2f}s")

    return True

def main():
    parser = argparse.ArgumentParser(
        description="Measure impact on final JAR size and build time: stash-based or HEAD~N based.", # Updated description
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument(
        "-u", "--undo-commits",
        metavar="N",
        type=int,
        help="Compare HEAD vs HEAD~N; otherwise compares working dir vs stashed HEAD."
    )
    parser.add_argument(
        "--binary-dir",
        default=os.path.join("tests", "binary"),
        help="Directory containing the test subdirectories (default: tests/binary)"
    )
    parser.add_argument("--only-run",  help="Comma-separated list of test names to include")
    parser.add_argument("--dont-run",  help="Comma-separated list of test names to exclude")
    args = parser.parse_args()

    binary_dir = args.binary_dir
    if not os.path.isdir(binary_dir):
        print(f"❌ Error: Specified binary directory not found: {binary_dir}")
        sys.exit(1)

    tests = collect_tests(binary_dir, args.only_run, args.dont_run)
    if not tests:
        print(f"|- ⚠️ No tests selected to run in '{binary_dir}'. Exiting.")
        sys.exit(0)

    print(f"|- 📦 Processing {len(tests)} test(s) in '{binary_dir}'…")
    print("-" * 40)

    start_time = time.time()
    success = False # Default to False
    try:
        if args.undo_commits is None:
            print("|- Mode: Stash-based comparison (Working Directory vs HEAD)")
            success = stash_based(tests, binary_dir)
        else:
            if args.undo_commits <= 0:
                print("❌ Error: --undo-commits N must be a positive integer.")
                sys.exit(1)
            print(f"|- Mode: Commit-based comparison (HEAD vs HEAD~{args.undo_commits})")
            success = commit_based(tests, binary_dir, args.undo_commits)
    except Exception as e:
        print(f"\n❌ An unexpected error occurred: {e}")
        import traceback; traceback.print_exc()
        # success remains False
    finally:
        end_time = time.time()
        print("-" * 40)
        print(f"|- Total script time: {end_time - start_time:.2f} seconds")
        if success:
            print("\n|- ✅ Done. Measurements completed.")
            sys.exit(0)
        else:
            # Make sure to exit non-zero if success is False, even if no exception occurred
            # (e.g., measurement failures might have happened but didn't raise exceptions)
            print("\n|- ❌ Failed. Some measurements may have failed or an error occurred.")
            sys.exit(1)

if __name__ == "__main__":
    main()