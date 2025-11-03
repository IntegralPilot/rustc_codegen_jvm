#!/usr/bin/env python3
import os
import subprocess
import sys
import argparse
import glob

# A constant for the base classpath needed by both test types
# NOTE: We may need to update the kotlin version in the future
RUNTIME_CLASSPATH_BASE = os.pathsep.join([
    "library/build/distributions/library-0.1.0/lib/library-0.1.0.jar",
    "library/build/distributions/library-0.1.0/lib/kotlin-stdlib-2.1.20.jar",
])

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

def build_rust_code(test_dir: str, release_mode: bool) -> tuple[bool, str]:
    """Builds the Rust code and returns (success, target_dir_name)."""
    print("|--- ⚒️ Building with Cargo...")
    build_cmd = ["cargo", "build", "--release"] if release_mode else ["cargo", "build"]
    use_target_json = os.path.join(test_dir, "use_target_json.flag")
    if os.path.exists(use_target_json):
        print("|---- 🛠️ Building with JVM target JSON...")
        build_cmd.extend(["--target", "../../../jvm-unknown-unknown.json"])
    
    proc = run_command(build_cmd, cwd=test_dir)
    target_dir = "release" if release_mode else "debug"
    
    if proc.returncode != 0:
        fail_path = os.path.join(test_dir, "cargo-build-fail.generated")
        output = f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
        write_to_file(fail_path, output)
        print(f"|---- ❌ cargo build exited with code {proc.returncode}")
        
        # Check if this is an R8 linking error (indicates invalid JVM bytecode)
        error_output = proc.stdout + proc.stderr
        if "R8" in error_output:
            print("|---- 🔍 Detected R8 error - attempting to run with -noverify for better diagnostics...")
            test_name = os.path.basename(test_dir)
            
            # Try to run the main class with -noverify to get actual JVM error
            deps_dir = os.path.join(test_dir, "target", target_dir, "deps")
            if os.path.exists(deps_dir):
                # Check if the main class file exists
                main_class = os.path.join(deps_dir, f"{test_name}.class")
                if os.path.exists(main_class):
                    # Build classpath: runtime libraries + deps directory
                    java_cp = f"{RUNTIME_CLASSPATH_BASE}{os.pathsep}{deps_dir}"
                    verify_proc = run_command(
                        ["java", "-noverify", "-cp", java_cp, test_name]
                    )
                    
                    # Always write output if we got any
                    verify_output = f"\n\n--- JVM OUTPUT WITH -noverify ---\n"
                    verify_output += f"Command: java -noverify -cp {java_cp} {test_name}\n"
                    verify_output += f"STDOUT:\n{verify_proc.stdout}\n\nSTDERR:\n{verify_proc.stderr}\n"
                    verify_output += f"Return code: {verify_proc.returncode}\n"
                    
                    # Append to the fail file
                    full_output = output + verify_output
                    write_to_file(fail_path, full_output)
                    print("|---- 📝 JVM diagnostics written to cargo-build-fail.generated")
                    
                    # Show relevant error info
                    if verify_proc.stderr:
                        # Filter out the deprecation warning about -noverify
                        error_lines = [line for line in verify_proc.stderr.split('\n') 
                                     if '-noverify' not in line and '-Xverify:none' not in line and line.strip()]
                        if error_lines:
                            print(f"|---- JVM error: {error_lines[0][:200]}")
                    elif verify_proc.returncode != 0:
                        print(f"|---- JVM exited with code {verify_proc.returncode}")
                else:
                    print(f"|---- ⚠️ Main class file not found: {main_class}")
            else:
                print(f"|---- ⚠️ Deps directory not found: {deps_dir}")
        
        return False, ""

    return True, target_dir

def find_and_prepare_jar(test_dir: str, test_name: str, target_dir: str) -> tuple[bool, str]:
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
            print(f"|---- ❌ Dependency directory not found: {deps_dir}")
            return False, ""
            
        if jar_file is None:
            print(f"|---- ❌ No jar file found for '{test_name}' in target/{target_dir}/deps")
            return False, ""
        
        # Move jar to a predictable location
        dest_dir = os.path.join(test_dir, "target", "jvm-unknown-unknown", target_dir)
        os.makedirs(dest_dir, exist_ok=True)
        os.rename(os.path.join(deps_dir, jar_file), os.path.join(dest_dir, f"{test_name}.jar"))

    jar_path = os.path.join(test_dir, "target", "jvm-unknown-unknown", target_dir, f"{test_name}.jar")
    if not os.path.exists(jar_path):
        print(f"|---- ❌ JAR file not found at expected path: {jar_path}")
        return False, ""
    return True, jar_path

def check_results(proc, test_dir: str, release_mode: bool) -> bool:
    """Checks the return code and output of a completed process."""
    # Check return code
    expected_returncode_file = os.path.join(test_dir, "java-returncode.expected")
    if os.path.exists(expected_returncode_file):
        expected_returncode = int(read_from_file(expected_returncode_file).strip())
        if proc.returncode != expected_returncode:
            fail_path = os.path.join(test_dir, "java-returncode-fail.generated")
            output = f"Expected return code: {expected_returncode}\nActual return code: {proc.returncode}\n\nSTDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
            write_to_file(fail_path, output)
            print(f"|---- ❌ java exited with code {proc.returncode}, expected {expected_returncode}")
            return False
    elif proc.returncode != 0:
        fail_path = os.path.join(test_dir, "java-fail.generated")
        output = f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
        write_to_file(fail_path, output)
        print(f"|---- ❌ java exited with code {proc.returncode}")
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
            print("|---- ❌ java output did not match expected output")
            return False
        else:
            print("|--- ✅ Output matches expected output!")

    return True

def process_binary_test(test_dir: str, release_mode: bool) -> bool:
    test_name = os.path.basename(test_dir)
    normalized = normalize_name(test_name)
    print(f"|-- Test '{test_name}' ({normalized})")

    # If running in release mode, allow tests to opt-out by creating a
    # `no_release.flag` file containing a short justification. When present
    # we skip the test and show the justification to the user.
    no_release_file = os.path.join(test_dir, "no_release.flag")
    if release_mode and os.path.exists(no_release_file):
        reason = read_from_file(no_release_file).strip()
        # Per request: show the justification when skipping
        print(f"Skipping: {reason}")
        return True

    print("|--- 🧼 Cleaning test folder...")
    proc = run_command(["cargo", "clean"], cwd=test_dir)
    if proc.returncode != 0:
        fail_path = os.path.join(test_dir, "cargo-clean-fail.generated")
        write_to_file(fail_path, f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}")
        print(f"|---- ❌ cargo clean exited with code {proc.returncode}")
        return False

    build_ok, target_dir = build_rust_code(test_dir, release_mode)
    if not build_ok:
        return False

    jar_ok, jar_path = find_and_prepare_jar(test_dir, test_name, target_dir)
    if not jar_ok:
        return False

    print("|--- 🤖 Running with Java...")
    java_cp = f"{RUNTIME_CLASSPATH_BASE}:{jar_path}"
    proc = run_command(["java", "-cp", java_cp, test_name]) 
    
    if not check_results(proc, test_dir, release_mode):
        return False
    
    print("|--- ✅ Binary test passed!")
    return True

def process_integration_test(test_dir: str, release_mode: bool) -> bool:
    test_name = os.path.basename(test_dir)
    normalized = normalize_name(test_name)
    print(f"|-- Test '{test_name}' ({normalized})")

    # If running in release mode, allow tests to opt-out by creating a
    # `no_release.flag` file containing a short justification. When present
    # we skip the test and show the justification to the user.
    no_release_file = os.path.join(test_dir, "no_release.flag")
    if release_mode and os.path.exists(no_release_file):
        reason = read_from_file(no_release_file).strip()
        # Per request: show the justification when skipping
        print(f"Skipping: {reason}")
        return True

    print("|--- 🧼 Cleaning test folder...")
    run_command(["cargo", "clean"], cwd=test_dir) # Ignore clean failure for now

    build_ok, target_dir = build_rust_code(test_dir, release_mode)
    if not build_ok:
        return False

    jar_ok, jar_path = find_and_prepare_jar(test_dir, test_name, target_dir)
    if not jar_ok:
        return False
    
    print("|--- ☕ Compiling Java test source...")
    abs_java_files = glob.glob(os.path.join(test_dir, "*.java"))
    java_files = [os.path.basename(f) for f in abs_java_files]
    if not java_files:
        print("|---- ❌ No .java files found in test directory.")
        return False
    
    base_cp_components = [os.path.relpath(p, test_dir) for p in RUNTIME_CLASSPATH_BASE.split(':')]
    relative_jar_path = os.path.relpath(jar_path, test_dir)
    
    javac_cp_list = ['.'] + base_cp_components + [relative_jar_path]
    javac_cp = os.pathsep.join(javac_cp_list)

    javac_cmd = ["javac", "-cp", javac_cp] + java_files
    proc = run_command(javac_cmd, cwd=test_dir)
    if proc.returncode != 0:
        fail_path = os.path.join(test_dir, "javac-fail.generated")
        write_to_file(fail_path, f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}")
        print(f"|---- ❌ javac exited with code {proc.returncode}")
        return False

    print("|--- 🤖 Running with Java...")
    # Convention: The main class for integration tests is 'Main'
    main_class = "Main"
    
    java_cp = javac_cp
    proc = run_command(["java", "-cp", java_cp, main_class], cwd=test_dir)

    if not check_results(proc, test_dir, release_mode):
        return False

    print("|--- ✅ Integration test passed!")
    return True

def main():
    parser = argparse.ArgumentParser(description="Tester for Rustc's JVM Codegen Backend")
    parser.add_argument("--release", action="store_true", help="Run cargo in release mode")
    parser.add_argument("--only-run", type=str, help="Comma-separated list of specific test names to run")
    parser.add_argument("--dont-run", type=str, help="Comma-separated list of specific test names to exclude")
    args = parser.parse_args()

    print("🧪 Tester for Rustc's JVM Codegen Backend started!")
    overall_success = True

    if args.release:
        print("|- ⚒️ Running in release mode")
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

    binary_tests = discover_tests(os.path.join("tests", "binary"))
    integration_tests = discover_tests(os.path.join("tests", "integration"))
    
    # --- Run Binary Tests ---
    if binary_tests:
        print(f"|- 📦 Running {len(binary_tests)} binary test(s)...")
        for test_dir in sorted(binary_tests):
            if not process_binary_test(test_dir, args.release):
                overall_success = False
        print(" ")
        
    # --- Run Integration Tests ---
    if integration_tests:
        print(f"|- 🔗 Running {len(integration_tests)} integration test(s)...")
        for test_dir in sorted(integration_tests):
            if not process_integration_test(test_dir, args.release):
                overall_success = False
        print(" ")

    # --- Final Summary ---
    if overall_success:
        print("|-✅ All tests passed!")
        sys.exit(0)
    else:
        print("|- ❌ Some tests failed!")
        sys.exit(1)

if __name__ == "__main__":
    main()