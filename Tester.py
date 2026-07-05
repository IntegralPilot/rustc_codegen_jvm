#!/usr/bin/env python3
import os
import subprocess
import sys
import argparse
import glob
from concurrent.futures import ThreadPoolExecutor, as_completed


def resolve_jobs(requested_jobs: int | None) -> int:
    if requested_jobs is None:
        return os.cpu_count() or 1
    if requested_jobs < 1:
        raise ValueError("--jobs must be at least 1")
    return requested_jobs


def parallelism_description(jobs: int, requested_jobs: int | None) -> str:
    if requested_jobs is None:
        return f"{jobs} concurrent workers (auto; CPU core count; override with -j/--jobs)"
    return f"{jobs} concurrent workers"


def read_from_file(path: str) -> str:
    with open(path, "r") as f:
        return f.read()

def write_to_file(path: str, content: str):
    with open(path, "w") as f:
        f.write(content)

def normalize_name(test_name: str) -> str:
    return test_name.replace("_", " ").capitalize()

def run_command(cmd: list, cwd=None):
    proc = subprocess.run(cmd, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    return proc

def build_rust_code(test_dir: str, release_mode: bool, logs: list) -> tuple[bool, str]:
    """Builds the Rust code and returns (success, target_dir_name)."""
    logs.append("|--- ⚒️ Building with Cargo...")
    build_cmd = ["cargo", "build", "--release"] if release_mode else ["cargo", "build"]
    use_target_json = os.path.join(test_dir, "use_target_json.flag")
    if os.path.exists(use_target_json):
        logs.append("|---- 🛠️ Building with JVM target JSON...")
        build_cmd.extend(["--target", "../../../jvm-unknown-unknown.json"])
        build_cmd.extend(["-Zjson-target-spec"])
    
    proc = run_command(build_cmd, cwd=test_dir)
    target_dir = "release" if release_mode else "debug"
    
    if proc.returncode != 0:
        fail_path = os.path.join(test_dir, "cargo-build-fail.generated")
        output = f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
        write_to_file(fail_path, output)
        logs.append(f"|---- ❌ cargo build exited with code {proc.returncode}")
        return False, ""

    return True, target_dir

def find_and_prepare_jar(test_dir: str, test_name: str, target_dir: str, logs: list) -> tuple[bool, str]:
    """Finds the generated JAR and moves it to a predictable location."""
    # If using a custom target, cargo places artifacts in a different folder structure.
    # If not, it's in target/{debug|release}/deps and needs to be moved.
    use_target_json = os.path.join(test_dir, "use_target_json.flag")
    if not os.path.exists(use_target_json):
        deps_dir = os.path.join(test_dir, "target", target_dir, "deps")
        jar_file = None
        try:
            for file in os.listdir(deps_dir):
                if file.startswith(test_name) and file.endswith(".jar"):
                    jar_file = file
                    break
        except FileNotFoundError:
            logs.append(f"|---- ❌ Dependency directory not found: {deps_dir}")
            return False, ""
            
        if jar_file is None:
            logs.append(f"|---- ❌ No jar file found for '{test_name}' in target/{target_dir}/deps")
            return False, ""
        
        # Move jar to a predictable location
        dest_dir = os.path.join(test_dir, "target", "jvm-unknown-unknown", target_dir)
        os.makedirs(dest_dir, exist_ok=True)
        try:
            os.rename(os.path.join(deps_dir, jar_file), os.path.join(dest_dir, f"{test_name}.jar"))
        except FileExistsError:
            # Handle potential race or override
            os.replace(os.path.join(deps_dir, jar_file), os.path.join(dest_dir, f"{test_name}.jar"))

    jar_path = os.path.join(test_dir, "target", "jvm-unknown-unknown", target_dir, f"{test_name}.jar")
    if not os.path.exists(jar_path):
        logs.append(f"|---- ❌ JAR file not found at expected path: {jar_path}")
        return False, ""
    return True, jar_path

def check_results(proc, test_dir: str, release_mode: bool, logs: list) -> bool:
    """Checks the return code and output of a completed process."""
    # Check return code
    expected_returncode_file = os.path.join(test_dir, "java-returncode.expected")
    if os.path.exists(expected_returncode_file):
        expected_returncode = int(read_from_file(expected_returncode_file).strip())
        if proc.returncode != expected_returncode:
            fail_path = os.path.join(test_dir, "java-returncode-fail.generated")
            output = f"Expected return code: {expected_returncode}\nActual return code: {proc.returncode}\n\nSTDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
            write_to_file(fail_path, output)
            logs.append(f"|---- ❌ java exited with code {proc.returncode}, expected {expected_returncode}")
            return False
    elif proc.returncode != 0:
        fail_path = os.path.join(test_dir, "java-fail.generated")
        output = f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
        write_to_file(fail_path, output)
        logs.append(f"|---- ❌ java exited with code {proc.returncode}")
        return False

    # Check output
    expected_file = os.path.join(test_dir, "java-output.release.expected") if release_mode else os.path.join(test_dir, "java-output.expected")
    if not os.path.exists(expected_file) and release_mode:
        expected_file = os.path.join(test_dir, "java-output.expected")

    if os.path.exists(expected_file):
        expected_output = read_from_file(expected_file).strip()
        actual_output = f"STDOUT:{proc.stdout.strip()}STDERR:{proc.stderr.strip()}".strip()

        # Normalize to handle different line endings and trailing whitespace
        expected_output = "".join(expected_output.split())
        actual_output = "".join(actual_output.split())

        if actual_output != expected_output:
            diff_path = os.path.join(test_dir, "output-diff.generated")
            # Write a more human-readable diff file
            diff_content = f"--- EXPECTED ---\n{read_from_file(expected_file)}\n\n--- ACTUAL STDOUT ---\n{proc.stdout}\n\n--- ACTUAL STDERR ---\n{proc.stderr}\n"
            write_to_file(diff_path, diff_content)
            logs.append("|---- ❌ java output did not match expected output")
            return False
        else:
            logs.append("|--- ✅ Output matches expected output!")

    return True

def process_binary_test(test_dir: str, release_mode: bool, logs: list) -> bool:
    test_name = os.path.basename(test_dir)
    normalized = normalize_name(test_name)
    logs.append(f"|-- Test '{test_name}' ({normalized})")

    # If running in release mode, allow tests to opt-out by creating a
    # `no_release.flag` file containing a short justification. When present
    # we skip the test and show the justification to the user.
    no_release_file = os.path.join(test_dir, "no_release.flag")
    if release_mode and os.path.exists(no_release_file):
        reason = read_from_file(no_release_file).strip()
        logs.append(f"Skipping: {reason}")
        return True

    logs.append("|--- 🧼 Cleaning test folder...")
    proc = run_command(["cargo", "clean"], cwd=test_dir)
    if proc.returncode != 0:
        fail_path = os.path.join(test_dir, "cargo-clean-fail.generated")
        write_to_file(fail_path, f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}")
        logs.append(f"|---- ❌ cargo clean exited with code {proc.returncode}")
        return False

    build_ok, target_dir = build_rust_code(test_dir, release_mode, logs)
    if not build_ok:
        return False

    jar_ok, jar_path = find_and_prepare_jar(test_dir, test_name, target_dir, logs)
    if not jar_ok:
        return False

    logs.append("|--- 🤖 Running with Java...")
    java_cp = jar_path
    proc = run_command(["java", "-cp", java_cp, test_name]) 
    
    if not check_results(proc, test_dir, release_mode, logs):
        return False
    
    logs.append("|--- ✅ Binary test passed!")
    return True

def process_integration_test(test_dir: str, release_mode: bool, logs: list) -> bool:
    test_name = os.path.basename(test_dir)
    normalized = normalize_name(test_name)
    logs.append(f"|-- Test '{test_name}' ({normalized})")

    # If running in release mode, allow tests to opt-out by creating a
    # `no_release.flag` file containing a short justification. When present
    # we skip the test and show the justification to the user.
    no_release_file = os.path.join(test_dir, "no_release.flag")
    if release_mode and os.path.exists(no_release_file):
        reason = read_from_file(no_release_file).strip()
        logs.append(f"Skipping: {reason}")
        return True

    logs.append("|--- 🧼 Cleaning test folder...")
    run_command(["cargo", "clean"], cwd=test_dir) # Ignore clean failure for now

    build_ok, target_dir = build_rust_code(test_dir, release_mode, logs)
    if not build_ok:
        return False

    jar_ok, jar_path = find_and_prepare_jar(test_dir, test_name, target_dir, logs)
    if not jar_ok:
        return False
    
    logs.append("|--- ☕ Compiling Java test source...")
    abs_java_files = glob.glob(os.path.join(test_dir, "*.java"))
    java_files = [os.path.basename(f) for f in abs_java_files]
    if not java_files:
        logs.append("|---- ❌ No .java files found in test directory.")
        return False
    
    relative_jar_path = os.path.relpath(jar_path, test_dir)
    
    javac_cp = os.pathsep.join(['.', relative_jar_path])

    javac_cmd = ["javac", "-cp", javac_cp] + java_files
    proc = run_command(javac_cmd, cwd=test_dir)
    if proc.returncode != 0:
        fail_path = os.path.join(test_dir, "javac-fail.generated")
        write_to_file(fail_path, f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}")
        logs.append(f"|---- ❌ javac exited with code {proc.returncode}")
        return False

    logs.append("|--- 🤖 Running with Java...")
    # Convention: The main class for integration tests is 'Main'
    main_class = "Main"
    
    java_cp = javac_cp
    proc = run_command(["java", "-cp", java_cp, main_class], cwd=test_dir)

    if not check_results(proc, test_dir, release_mode, logs):
        return False

    logs.append("|--- ✅ Integration test passed!")
    return True

def run_single_test(test_dir: str, test_type: str, release_mode: bool) -> tuple[bool, str, list]:
    """Helper used to delegate execution within worker threads."""
    logs = []
    if test_type == "binary":
        success = process_binary_test(test_dir, release_mode, logs)
    else:
        success = process_integration_test(test_dir, release_mode, logs)
    return success, test_dir, logs

def main():
    parser = argparse.ArgumentParser(description="Tester for Rustc's JVM Codegen Backend")
    parser.add_argument("--release", action="store_true", help="Run cargo in release mode")
    parser.add_argument("--only-run", type=str, help="Comma-separated list of specific test names to run")
    parser.add_argument("--dont-run", type=str, help="Comma-separated list of specific test names to exclude")
    parser.add_argument(
        "-j", "--jobs", 
        type=int, 
        default=None,
        help="Number of concurrent test executions (default: auto)"
    )
    args = parser.parse_args()
    try:
        jobs = resolve_jobs(args.jobs)
    except ValueError as e:
        parser.error(str(e))

    print("🧪 Tester for Rustc's JVM Codegen Backend started!")
    overall_success = True

    if args.release:
        print("|- ⚒️ Running in release mode")
    print(f"|- 🧵 Using {parallelism_description(jobs, args.jobs)}")
    print(" ")

    # --- Gather and filter tests ---
    only_run_set = set([name.strip() for name in args.only_run.split(",")]) if args.only_run else None
    dont_run_set = set([name.strip() for name in args.dont_run.split(",")]) if args.dont_run else set()

    def discover_tests(test_type_dir):
        if not os.path.isdir(test_type_dir):
            return []
        all_tests = [os.path.join(test_type_dir, d) for d in os.listdir(test_type_dir) if os.path.isdir(os.path.join(test_type_dir, d))]
        
        # Filter tests
        if only_run_set:
            all_tests = [t for t in all_tests if os.path.basename(t) in only_run_set]
        all_tests = [t for t in all_tests if os.path.basename(t) not in dont_run_set]
        return all_tests

    binary_tests = sorted(discover_tests(os.path.join("tests", "binary")))
    integration_tests = sorted(discover_tests(os.path.join("tests", "integration")))
    
    tasks = []
    for test_dir in binary_tests:
        tasks.append((test_dir, "binary"))
    for test_dir in integration_tests:
        tasks.append((test_dir, "integration"))

    if not tasks:
        print("No tests matched the specified filters.")
        sys.exit(0)

    # --- Parallel test execution pool ---
    print(f"|- 📦 Running {len(tasks)} test(s) in parallel...")
    print(" ")

    with ThreadPoolExecutor(max_workers=jobs) as executor:
        futures = {
            executor.submit(run_single_test, test_dir, t_type, args.release): (test_dir, t_type)
            for test_dir, t_type in tasks
        }
        
        for future in as_completed(futures):
            success, test_dir, logs = future.result()
            
            # Print each test output block atomically to prevent interleaving
            print("\n".join(logs))
            print(" ")
            
            if not success:
                overall_success = False

    # --- Final Summary ---
    if overall_success:
        print("|-✅ All tests passed!")
        sys.exit(0)
    else:
        print("|- ❌ Some tests failed!")
        sys.exit(1)

if __name__ == "__main__":
    main()
