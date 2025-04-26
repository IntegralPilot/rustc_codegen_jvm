#!/usr/bin/env python3
import os
import subprocess
import sys
import argparse
import zipfile
import time

def run_command(cmd, cwd=None, check=False):
    """Runs a command and returns its process object."""
    print(f"|------ Running: {' '.join(cmd)} {f'(in {cwd})' if cwd else ''}")
    proc = subprocess.run(cmd, cwd=cwd,
                          stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE,
                          text=True,
                          encoding='utf-8', errors='replace') # Added encoding/errors
    if check and proc.returncode != 0:
        print(f"❌ Command {' '.join(cmd)} failed (code {proc.returncode})")
        print("------ STDOUT ------")
        print(proc.stdout)
        print("------ STDERR ------", file=sys.stderr)
        print(proc.stderr, file=sys.stderr)
        print("--------------------", file=sys.stderr)
        sys.exit(proc.returncode)
    return proc

def find_jar(test_dir):
    """Finds the relevant JAR file within a test directory."""
    jars = []
    # Common Cargo output dirs
    target_dirs = [
        os.path.join(test_dir, "target", "debug"),
        os.path.join(test_dir, "target", "release") # Check release too
    ]

    # Search target dirs first
    for build_dir in target_dirs:
        if os.path.isdir(build_dir):
            for root, _, files in os.walk(build_dir):
                for f in files:
                    if f.endswith(".jar"):
                        jars.append(os.path.join(root, f))

    # Fallback: check the root test_dir (avoiding target dirs)
    for root, dirs, files in os.walk(test_dir):
        # Prune target directory searching if already checked
        dirs[:] = [d for d in dirs if os.path.join(root, d) not in target_dirs]
        for f in files:
            if f.endswith(".jar"):
                full_path = os.path.join(root, f)
                if full_path not in jars: # Avoid duplicates
                    jars.append(full_path)

    if not jars:
        # Don't print warning here, let the caller handle None
        return None

    name = os.path.basename(test_dir)
    # Prefer JARs matching the directory name
    for j in jars:
        if name in os.path.basename(j):
            # print(f"|------ Found JAR (name match): {j}") # Less verbose
            return j

    # Fallback to the first found JAR if no name match
    # print(f"|------ Found JAR (fallback): {jars[0]}") # Less verbose
    return jars[0]

def measure_class_size(jar_path, class_name):
    """Measures the size of a specific class file within a JAR."""
    if not jar_path or not os.path.exists(jar_path):
         # print(f"|------ ⚠️ JAR path invalid or not found: {jar_path}") # Let caller handle None
         return None
    try:
        with zipfile.ZipFile(jar_path, 'r') as z:
            # Try direct name first (e.g., "MyClass.class")
            entry_simple = class_name + ".class"
            # Try common Java package structure (ends with /ClassName.class)
            entry_suffix = "/" + class_name + ".class"

            possible_entries = []
            for item in z.infolist():
                # Check both exact match at root and suffix match for packages
                if item.filename == entry_simple or item.filename.endswith(entry_suffix):
                     possible_entries.append(item)

            if not possible_entries:
                print(f"|------ ⚠️ Class '{class_name}.class' not found in {jar_path}")
                # Optional: list entries for debugging
                # print("|-------- Available entries:")
                # for item in z.infolist(): print(f"|---------- {item.filename}")
                return None

            # If multiple matches, warn and take the first.
            if len(possible_entries) > 1:
                 print(f"|------ ⚠️ Multiple possible class files found for '{class_name}' in {jar_path}. Using first: {possible_entries[0].filename}")

            info = possible_entries[0]
            # print(f"|------ Measuring class: {info.filename} in {jar_path}")
            return info.file_size

    except zipfile.BadZipFile:
        print(f"|------ ⚠️ Bad ZIP file: {jar_path}")
        return None
    except FileNotFoundError:
         print(f"|------ ⚠️ File not found during measurement: {jar_path}")
         return None

def collect_tests(binary_dir, only_run, dont_run):
    """Collects list of test directories based on filters and skip flag."""
    try:
        all_dirs = sorted([d for d in os.listdir(binary_dir)
                           if os.path.isdir(os.path.join(binary_dir, d))])
    except FileNotFoundError:
        print(f"❌ Error: Test directory '{binary_dir}' not found.")
        sys.exit(1)

    # Initial list of full paths
    candidate_tests = [os.path.join(binary_dir, d) for d in all_dirs]
    filtered_tests = []

    # Apply only_run and dont_run filters
    if only_run:
        keep = set(n.strip() for n in only_run.split(","))
        candidate_tests = [t for t in candidate_tests if os.path.basename(t) in keep]
    if dont_run:
        skip = set(n.strip() for n in dont_run.split(","))
        candidate_tests = [t for t in candidate_tests if os.path.basename(t) not in skip]

    # Apply the no_jvm_target.flag filter - if it doesn't exist, skip the test
    final_tests = []
    skipped_no_jvm = []
    skip_flag_name = "no_jvm_target.flag"
    for test_dir in candidate_tests:
        flag_path = os.path.join(test_dir, skip_flag_name)
        if not os.path.exists(flag_path):
            skipped_no_jvm.append(os.path.basename(test_dir))
        else:
            final_tests.append(test_dir)

    if skipped_no_jvm:
        print(f"|-- Skipping tests due to '{skip_flag_name}': {', '.join(skipped_no_jvm)}")

    if not final_tests:
         print(f"❌ No tests remain to be processed in '{binary_dir}' after filtering.")
         # Decide if this should be an error or just an empty run
         # sys.exit(1) # Exit if no tests is considered an error
         print("Continuing with zero tests.") # Or just continue

    return final_tests


def build_tests(tests_to_build):
    """Cleans and builds each specified test using Cargo."""
    if not tests_to_build:
        print("|--- No tests selected for Cargo build.")
        return # Nothing to do

    print("|--- 🛠️ Building test targets (Cargo)...")
    for test_dir in tests_to_build:
        name = os.path.basename(test_dir)
        print(f"|---- Building test: {name} (in {test_dir})")
        # Clean first
        run_command(["cargo", "clean"], cwd=test_dir, check=True)
        # Then build - ADD --release or other flags if necessary
        run_command(["cargo", "build"], cwd=test_dir, check=True)
        # run_command(["cargo", "build", "--release"], cwd=test_dir, check=True)
    print("|--- ✅ Test building (Cargo) complete.")

def stash_based(tests, binary_dir):
    """Compares working dir vs. stashed state."""
    print("|--- 🛠️ Building main project AND tests WITH unstaged changes…")
    print("|---- Cleaning main project (make clean)...")
    run_command(["make", "clean"], check=True)
    print("|---- Building main project (make all)...")
    run_command(["make", "all"], check=True)
    build_tests(tests) # Runs cargo clean/build for selected tests

    sizes_with = {}
    print("\n|--- 📏 Measuring sizes WITH changes...")
    for t in tests: # Iterate only over tests that passed the filters
        name = os.path.basename(t)
        print(f"|---- Measuring test: {name}")
        jar = find_jar(t)
        if not jar:
             print(f"|------ ⚠️ No JAR found for test {name}, skipping measurement.")
             sizes_with[name] = None
             continue
        sz = measure_class_size(jar, name) # Pass class name = dir name
        sizes_with[name] = sz
        print(f"|------ [{name}] WITH: {sz if sz is not None else 'N/A'} bytes")

    print("\n|--- 🧹 Stashing working tree…")
    # Check if there are changes to stash first
    status_proc = run_command(["git", "status", "--porcelain"])
    if status_proc.stdout.strip():
        run_command(["git", "stash", "push", "--include-untracked", "--quiet", "-m", "class_size_script_stash"], check=True)
        stashed = True
    else:
        print("|------ Working directory clean, nothing to stash.")
        stashed = False


    # --- Build without changes ---
    print("\n|--- 🛠️ Building main project AND tests at HEAD (no changes)…")
    print("|---- Cleaning main project (make clean)...")
    run_command(["make", "clean"], check=True)
    print("|---- Building main project (make all)...")
    run_command(["make", "all"], check=True)
    build_tests(tests) # Runs cargo clean/build for selected tests

    sizes_without = {}
    print("\n|--- 📏 Measuring sizes WITHOUT changes (HEAD)...")
    for t in tests: # Iterate only over tests that passed the filters
        name = os.path.basename(t)
        print(f"|---- Measuring test: {name}")
        jar = find_jar(t)
        if not jar:
             print(f"|------ ⚠️ No JAR found for test {name}, skipping measurement.")
             sizes_without[name] = None
             continue
        sz = measure_class_size(jar, name)
        sizes_without[name] = sz
        print(f"|------ [{name}] WITHOUT: {sz if sz is not None else 'N/A'} bytes")

    if stashed:
        print("\n|--- 🔄 Restoring working tree…")
        # Check if stash exists before trying to pop (using the message)
        stash_list = run_command(["git", "stash", "list"]).stdout
        if "class_size_script_stash" in stash_list:
            print("|------ Popping script stash...")
            run_command(["git", "stash", "pop", "--quiet"]) # Don't check=True, pop can have conflicts
        else:
            print("|------ Script stash not found (already popped or error?).")
    else:
        print("\n|--- (No stash was created, skipping restore)")


    # report
    print("\n|--- 📊 Size comparison (stash-based):")
    ok = True
    failed_measurements = 0
    if not tests:
        print("|---- No tests were measured.")
        return True # Or False if no tests is failure

    for name in sorted(os.path.basename(t) for t in tests): # Iterate based on collected tests
        w = sizes_with.get(name)
        wo = sizes_without.get(name)

        if w is None or wo is None:
            print(f"|---- [{name}] ⚠️ skipped (measurement failed or JAR not found)")
            # ok = False # Don't mark overall as failed if JAR just wasn't found as expected
            failed_measurements += 1
            continue

        delta = w - wo
        sign = "+" if delta >= 0 else "" # Sign is implicit in negative numbers
        change_desc = "no change"
        if delta > 0:
            change_desc = f"increase {delta:+}"
        elif delta < 0:
            change_desc = f"decrease {delta}"

        print(f"|---- [{name}] {change_desc} ({wo} → {w} bytes)")

        # Ensure output directory exists
        output_dir = os.path.join(binary_dir, name)
        os.makedirs(output_dir, exist_ok=True)
        diff_path = os.path.join(output_dir, "class-size.diff")
        try:
            with open(diff_path, "w") as f:
                 f.write(f"WITH:    {w}\nWITHOUT: {wo}\nDELTA:   {delta}\n")
        except IOError as e:
            print(f"|------ ⚠️ Could not write diff file {diff_path}: {e}")
            ok = False


    if failed_measurements > 0:
        print(f"|---- ⚠️ {failed_measurements} test(s) had measurement failures or missing JARs.")

    return ok

def commit_based(tests, binary_dir, n):
    """Compares HEAD vs HEAD~N."""
    print("|--- 🧹 Stashing working-tree changes (if any) for clean checkout…")
    # Use a specific message for the stash
    stash_message = "class_size_script_stash_commit"
    status_proc = run_command(["git", "status", "--porcelain"])
    stashed = False
    if status_proc.stdout.strip():
        run_command(["git", "stash", "push", "--include-untracked", "--quiet", "-m", stash_message], check=True)
        stashed = True
        print("|------ Stashed current changes.")
    else:
        print("|------ Working directory clean, nothing to stash.")

    orig_head = ""
    target_commit = ""
    try:
        orig_head = run_command(["git", "rev-parse", "HEAD"], check=True).stdout.strip()
        print(f"|--- Current HEAD is: {orig_head[:10]}...") # Short hash

        # Check if HEAD~N exists before trying to check out
        target_commit = f"HEAD~{n}" # Use relative ref initially
        print(f"|--- Verifying existence of target commit {target_commit}...")
        target_hash = run_command(["git", "rev-parse", "--verify", target_commit], check=True).stdout.strip()
        print(f"|--- Target commit {target_commit} resolved to {target_hash[:10]}...")
        target_commit = target_hash # Use the actual hash for checkout

    except SystemExit: # Catch exit from run_command check=True failure
         print(f"❌ Failed to get current HEAD or verify {target_commit}. Aborting.")
         # Try to pop stash if we stashed anything
         if stashed:
             print("|--- 🔄 Popping stash before exiting...")
             # Check specifically for our stash message
             stash_list = run_command(["git", "stash", "list"]).stdout
             if stash_message in stash_list:
                  run_command(["git", "stash", "pop", "--quiet"])
             else:
                  print("|------ Script stash not found.")
         sys.exit(1)


    # --- Build at HEAD ---
    print(f"\n|--- 🛠️ Building main project AND tests at HEAD ({orig_head[:10]}...)…")
    print("|---- Cleaning main project (make clean)...")
    run_command(["make", "clean"], check=True)
    print("|---- Building main project (make all)...")
    run_command(["make", "all"], check=True)
    build_tests(tests) # Runs cargo clean/build for selected tests

    sizes_head = {}
    print("\n|--- 📏 Measuring sizes at HEAD...")
    for t in tests: # Iterate only over tests that passed the filters
        name = os.path.basename(t)
        print(f"|---- Measuring test: {name}")
        jar = find_jar(t)
        if not jar:
             print(f"|------ ⚠️ No JAR found for test {name}, skipping measurement.")
             sizes_head[name] = None
             continue
        sz = measure_class_size(jar, name)
        sizes_head[name] = sz
        print(f"|------ [{name}] HEAD: {sz if sz is not None else 'N/A'} bytes")


    print(f"\n|--- 🔨 Checking out {target_commit[:10]}…")
    run_command(["git", "checkout", target_commit, "--quiet"], check=True)

    # --- Build at HEAD~N ---
    print(f"\n|--- 🛠️ Building main project AND tests at {target_commit[:10]}… (HEAD~{n})")
    print("|---- Cleaning main project (make clean)...")
    run_command(["make", "clean"], check=True)
    print("|---- Building main project (make all)...")
    run_command(["make", "all"], check=True)
    build_tests(tests) # Runs cargo clean/build for selected tests

    sizes_old = {}
    print(f"\n|--- 📏 Measuring sizes at {target_commit[:10]}… (HEAD~{n})")
    for t in tests: # Iterate only over tests that passed the filters
        name = os.path.basename(t)
        print(f"|---- Measuring test: {name}")
        jar = find_jar(t)
        if not jar:
             print(f"|------ ⚠️ No JAR found for test {name}, skipping measurement.")
             sizes_old[name] = None
             continue
        sz = measure_class_size(jar, name)
        sizes_old[name] = sz
        print(f"|------ [{name}] HEAD~{n}: {sz if sz is not None else 'N/A'} bytes")


    print(f"\n|--- 🔄 Restoring HEAD ({orig_head[:10]}...)…")
    run_command(["git", "checkout", orig_head, "--quiet"], check=True)

    if stashed:
        print("|--- 🔄 Popping stash…")
        # Check specifically for our stash message
        stash_list = run_command(["git", "stash", "list"]).stdout
        if stash_message in stash_list:
            run_command(["git", "stash", "pop", "--quiet"]) # Don't check=True, pop can have conflicts
        else:
            print("|------ Script stash not found (already popped or error?).")

    else:
         print("|--- (No stash was created)")


    # report
    print(f"\n|--- 📊 Size comparison (HEAD vs HEAD~{n}):")
    ok = True
    failed_measurements = 0
    if not tests:
        print("|---- No tests were measured.")
        return True # Or False if no tests is failure

    for name in sorted(os.path.basename(t) for t in tests): # Iterate based on collected tests
        h = sizes_head.get(name)
        o = sizes_old.get(name)

        if h is None or o is None:
            print(f"|---- [{name}] ⚠️ skipped (measurement failed or JAR not found)")
            # ok = False # Don't mark overall as failed if JAR just wasn't found
            failed_measurements += 1
            continue

        delta = h - o
        sign = "+" if delta >= 0 else "" # Sign is implicit in negative numbers
        change_desc = "no change"
        if delta > 0:
            change_desc = f"increase {delta:+}"
        elif delta < 0:
            change_desc = f"decrease {delta}"

        print(f"|---- [{name}] {change_desc} ({o} → {h} bytes)")

        # Ensure output directory exists
        output_dir = os.path.join(binary_dir, name)
        os.makedirs(output_dir, exist_ok=True)
        diff_path = os.path.join(output_dir, "class-size.diff")
        try:
            with open(diff_path, "w") as f:
                f.write(f"HEAD:     {h}\nHEAD~{n}: {o}\nDELTA:    {delta}\n")
        except IOError as e:
            print(f"|------ ⚠️ Could not write diff file {diff_path}: {e}")
            ok = False

    if failed_measurements > 0:
        print(f"|---- ⚠️ {failed_measurements} test(s) had measurement failures or missing JARs.")

    return ok


def main():
    parser = argparse.ArgumentParser(
        description="Measure impact on .class size within JARs: stash-based or HEAD~N based.\n"
                    "Assumes 'make' builds the main project and 'cargo build' within each test dir builds the test JAR.\n"
                    "Tests with a 'no_jvm_target.flag' file in their directory are skipped.",
        formatter_class=argparse.RawTextHelpFormatter # Nicer help text
    )
    parser.add_argument(
        "-u", "--undo-commits",
        metavar="N", # Better help hint
        type=int,
        help="Compare HEAD vs HEAD~N; otherwise compares working dir vs stashed HEAD."
    )
    parser.add_argument(
        "--binary-dir",
        default=os.path.join("tests", "binary"),
        help="Directory containing the test subdirectories (default: tests/binary)"
    )
    parser.add_argument("--only-run",  help="Comma-separated list of test names (dir names) to include")
    parser.add_argument("--dont-run",  help="Comma-separated list of test names (dir names) to exclude")
    args = parser.parse_args()

    binary_dir = args.binary_dir
    if not os.path.isdir(binary_dir):
        print(f"❌ Error: Specified binary directory not found: {binary_dir}")
        sys.exit(1)

    # Collect tests, applying all filters including the flag file check
    tests = collect_tests(binary_dir, args.only_run, args.dont_run)

    if not tests:
        print(f"|- ⚠️ No tests selected to run in '{binary_dir}'. Exiting.")
        sys.exit(0) # Exit cleanly if no tests are selected/remain

    print(f"|- 📦 Processing {len(tests)} test(s) in '{binary_dir}'…")
    # Optional: print list of tests being processed
    # print("|- Tests to process:", [os.path.basename(t) for t in tests])
    print("-" * 40) # Separator

    start_time = time.time()
    success = False # Default to failure
    orig_head_for_cleanup = "" # Store original HEAD for potential cleanup
    stashed_for_cleanup = False # Track if stash was used

    try:
        if args.undo_commits is None:
            print("|- Mode: Stash-based comparison (Working Directory vs HEAD)")
            # Need to know if stash_based actually stashed something for potential cleanup
            success = stash_based(tests, binary_dir)
        else:
            if args.undo_commits <= 0:
                 print("❌ Error: --undo-commits N must be a positive integer.")
                 sys.exit(1)
            print(f"|- Mode: Commit-based comparison (HEAD vs HEAD~{args.undo_commits})")
            # Store state for potential cleanup in commit_based
            orig_head_for_cleanup = run_command(["git", "rev-parse", "HEAD"], check=True).stdout.strip()
            status_proc = run_command(["git", "status", "--porcelain"])
            stashed_for_cleanup = bool(status_proc.stdout.strip()) # Check if stashing will happen
            success = commit_based(tests, binary_dir, args.undo_commits)

    except Exception as e:
         print(f"\n❌ An unexpected error occurred: {e}")
         import traceback
         traceback.print_exc()
         success = False # Ensure failure state

         # --- Attempt cleanup after error ---
         print("\n|--- ⚠️ Attempting cleanup after error...")
         current_head_after_error = run_command(["git", "rev-parse", "--abbrev-ref", "HEAD"]).stdout.strip()
         current_hash_after_error = run_command(["git", "rev-parse", "HEAD"]).stdout.strip()

         # If we were in commit mode and not back at original HEAD
         if args.undo_commits is not None and orig_head_for_cleanup and current_hash_after_error != orig_head_for_cleanup:
              print(f"|------ Restoring original HEAD ({orig_head_for_cleanup[:10]}...)")
              run_command(["git", "checkout", orig_head_for_cleanup, "--quiet"])
         else:
              print(f"|------ Already at original HEAD ({current_head_after_error}) or not in commit mode.")

         # Attempt stash pop if stash might have been created
         # Check for specific messages if possible, otherwise check general list
         stash_list = run_command(["git", "stash", "list"]).stdout
         stash_popped = False
         for msg in ["class_size_script_stash_commit", "class_size_script_stash"]:
              if msg in stash_list:
                   print(f"|------ Popping stash '{msg}'...")
                   run_command(["git", "stash", "pop", "--quiet"])
                   stash_popped = True
                   break # Pop only one
         if not stash_popped:
             print("|------ No known script stash found to pop.")


    finally:
        end_time = time.time()
        print("-" * 40) # Separator
        print(f"|- Total time: {end_time - start_time:.2f} seconds")

        if success:
            print("\n|- ✅ Done. Measurements completed (check warnings for skips).")
            sys.exit(0)
        else:
            print("\n|- ❌ Failed. Some measurements failed, tests were skipped unexpectedly, or an error occurred.")
            sys.exit(1)

if __name__ == "__main__":
    main()