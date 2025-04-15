#!/usr/bin/env python3
import os
import subprocess
import sys
import argparse

def read_from_file(path: str) -> str:
    with open(path, "r") as f:
        return f.read()

def normalize_name(test_name: str) -> str:
    return test_name.replace("_", " ").capitalize()

def run_command(cmd: list, cwd=None):
    proc = subprocess.run(cmd, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    return proc

def write_to_file(path: str, content: str):
    with open(path, "w") as f:
        f.write(content)

def process_test(test_dir: str, release_mode: bool):
    test_name = os.path.basename(test_dir)
    normalized = normalize_name(test_name)
    print(f"|-- Test '{test_name}' ({normalized})")
    
    print("|--- 🧼 Cleaning test folder...")
    proc = run_command(["cargo", "clean"], cwd=test_dir)
    if proc.returncode != 0:
        fail_path = os.path.join(test_dir, "cargo-clean-fail.generated")
        output = f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
        write_to_file(fail_path, output)
        print(f"|---- ❌ cargo clean exited with code {proc.returncode}")
        return False

    print("|--- ⚒️ Building with Cargo...")
    build_cmd = ["cargo", "build", "--release"] if release_mode else ["cargo", "build"]
    no_jvm_target = os.path.join(test_dir, "no_jvm_target.flag")
    if not os.path.exists(no_jvm_target):
        print("|---- 🛠️ Building with JVM target...")
        build_cmd.extend(["--target", "../../../jvm-unknown-unknown.json"])
    proc = run_command(build_cmd, cwd=test_dir)
    if proc.returncode != 0:
        fail_path = os.path.join(test_dir, "cargo-build-fail.generated")
        output = f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
        write_to_file(fail_path, output)
        print(f"|---- ❌ cargo build exited with code {proc.returncode}")
        return False

    print("|--- 🤖 Running with Java...")
    target_dir = "release" if release_mode else "debug"
    if os.path.exists(no_jvm_target):
        jar_path = os.path.join(test_dir, "target", target_dir, "deps", f"{test_name}-*.jar")
        jar_file = None
        for file in os.listdir(os.path.join(test_dir, "target", target_dir, "deps")):
            if file.startswith(test_name) and file.endswith(".jar"):
                jar_file = file
                break
        if jar_file is None:
            print("|---- ❌ No jar file found in target/{target_dir}/deps")
            return False
        os.makedirs(os.path.join(test_dir, "target", "jvm-unknown-unknown", target_dir), exist_ok=True)
        os.rename(os.path.join(test_dir, "target", target_dir, "deps", jar_file),
                  os.path.join(test_dir, "target", "jvm-unknown-unknown", target_dir, f"{test_name}.jar"))

    jar_path = os.path.join(test_dir, "target", "jvm-unknown-unknown", target_dir, f"{test_name}.jar")
    proc = run_command(["java", "-jar", jar_path]) 
    
    expected_returncode_file = os.path.join(test_dir, "java-returncode.expected")
    if os.path.exists(expected_returncode_file):
        expected_returncode = int(read_from_file(expected_returncode_file).strip())
        if proc.returncode != expected_returncode:
            fail_path = os.path.join(test_dir, "java-returncode-fail.generated")
            output = f"Expected return code: {expected_returncode}\nActual return code: {proc.returncode}\n\nSTDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
            write_to_file(fail_path, output)
            print(f"|---- ❌ java exited with code {proc.returncode}, expected {expected_returncode}")
            return False
    else:
        if proc.returncode != 0:
            fail_path = os.path.join(test_dir, "java-fail.generated")
            output = f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
            write_to_file(fail_path, output)
            print(f"|---- ❌ java exited with code {proc.returncode}")
            return False

    expected_file = os.path.join(test_dir, "java-output.expected")
    if os.path.exists(expected_file):
        expected_output = read_from_file(expected_file)
        if expected_output.strip() == "":
            expected_output = "STDOUT:STDERR:"
        else:
            expected_output = expected_output.replace("\n", "")
        actual_output = f"STDOUT:{proc.stdout.strip()}STDERR:{proc.stderr.strip()}"
        actual_output = actual_output.replace("\n", "")
        if actual_output != expected_output.strip():
            diff_path = os.path.join(test_dir, "output-diff.generated")
            write_to_file(diff_path, actual_output)
            print("|---- ❌ java output did not match expected output")
            return False
        else:
            print("|--- ✅ Output matches expected output!")
    else:
        print("|--- ⚠️ Expected output file not found. Skipping comparison.")
    
    print("|--- ✅ Binary test passed!")
    return True

def main():
    parser = argparse.ArgumentParser(description="Tester for Rustc's JVM Codegen Backend")
    parser.add_argument("--release", action="store_true", help="Run cargo in release mode")
    parser.add_argument("--only-run", type=str, help="Comma-separated list of specific test names to run")
    args = parser.parse_args()

    print("🧪 Tester for Rustc's JVM Codegen Backend started!")
    overall_success = True

    if args.release:
        print("|- ⚒️ Running in release mode")

    print(" ")

    # Gather test directories
    binary_dir = os.path.join("tests", "binary")
    if os.path.isdir(binary_dir):
        binary_tests = [os.path.join(binary_dir, d) for d in os.listdir(binary_dir) if os.path.isdir(os.path.join(binary_dir, d))]
    else:
        binary_tests = []

    # Filter based on --only-run
    if args.only_run:
        requested_tests = set([name.strip() for name in args.only_run.split(",")])
        binary_tests = [t for t in binary_tests if os.path.basename(t) in requested_tests]

    print(f"|- 📦 Running {len(binary_tests)} binary build test(s)...")
    for test_dir in binary_tests:
        if not process_test(test_dir, args.release):
            overall_success = False

    print("")
    if overall_success:
        print("|-✅ All tests passed!")
        sys.exit(0)
    else:
        print("|- ❌ Some tests failed!")
        sys.exit(1)

if __name__ == "__main__":
    main()
