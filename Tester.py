#!/usr/bin/env python3
import os
import subprocess
import sys

def read_from_file(path: str) -> str:
    with open(path, "r") as f:
        return f.read()

def normalize_name(test_name: str) -> str:
    # Replace underscores with spaces and capitalize the first letter.
    return test_name.replace("_", " ").capitalize()

def run_command(cmd: list, cwd=None):
    proc = subprocess.run(cmd, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    return proc

def write_to_file(path: str, content: str):
    with open(path, "w") as f:
        f.write(content)

def process_test(test_dir: str):
    test_name = os.path.basename(test_dir)
    normalized = normalize_name(test_name)
    print(f"|-- Test '{test_name}' ({normalized})")
    
    # Clean the folder: with cargo clean
    print("|--- 🧼 Cleaning test folder...")
    proc = run_command(["cargo", "clean"], cwd=test_dir)
    if proc.returncode != 0:
        fail_path = os.path.join(test_dir, "cargo-clean-fail.generated")
        output = f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
        write_to_file(fail_path, output)
        print(f"|---- ❌ cargo clean exited with code {proc.returncode}")
        return False

    # Run cargo build.
    print("|--- ⚒️ Building with Cargo...")
    # see if the file no_jvm_target.flag exists in the test directory
    no_jvm_target = os.path.join(test_dir, "no_jvm_target.flag")
    if os.path.exists(no_jvm_target):
        print("|---- ⚠️ Skipping JVM target build due to no_jvm_target.flag")
        proc = run_command(["cargo", "build"], cwd=test_dir)
    else:
        print("|---- 🛠️ Building with JVM target...")
        proc = run_command(["cargo", "build", "--target", "../../../jvm-unknown-unknown.json"], cwd=test_dir)
    if proc.returncode != 0:
        fail_path = os.path.join(test_dir, "cargo-build-fail.generated")
        output = f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
        write_to_file(fail_path, output)
        print(f"|---- ❌ cargo build exited with code {proc.returncode}")
        return False

    # Run java with the generated jar.
    print("|--- 🤖 Running with Java...")
    # if no_jvm_target flag is set, we first need to move target/debug/deps/{test_name}-{hash}.jar to target/jvm-unknown-unknown/debug/{test_name}.jar
    # we might need to make the directory first
    if os.path.exists(no_jvm_target):
        print("|---- ⚠️ Doing some needed moving around due to no_jvm_target.flag")
        # move the jar file to the target/jvm-unknown-unknown/debug directory
        jar_path = os.path.join(test_dir, "target", "debug", "deps", f"{test_name}-*.jar")
        # find the jar file
        jar_file = None
        for file in os.listdir(os.path.join(test_dir, "target", "debug", "deps")):
            if file.startswith(test_name) and file.endswith(".jar"):
                jar_file = file
                break
        if jar_file is None:
            print("|---- ❌ No jar file found in target/debug/deps")
            return False
        # move the file
        os.makedirs(os.path.join(test_dir, "target", "jvm-unknown-unknown", "debug"), exist_ok=True)
        os.rename(os.path.join(test_dir, "target", "debug", "deps", jar_file), os.path.join(test_dir, "target", "jvm-unknown-unknown", "debug", f"{test_name}.jar"))
    jar_path = os.path.join(test_dir, "target", "jvm-unknown-unknown", "debug", f"{test_name}.jar")
    proc = run_command(["java", "-jar", jar_path])
    if proc.returncode != 0:
        fail_path = os.path.join(test_dir, "java-fail.generated")
        output = f"STDOUT:\n{proc.stdout}\n\nSTDERR:\n{proc.stderr}"
        write_to_file(fail_path, output)
        print(f"|---- ❌ java exited with code {proc.returncode}")
        return False

    # Compare the STDOUT to {test_dir}/java_output.expected
    expected_file = os.path.join(test_dir, "java_output.expected")
    if os.path.exists(expected_file):
        expected_output = read_from_file(expected_file)
        actual_output = proc.stdout.strip()
        if actual_output != expected_output.strip():
            diff_path = os.path.join(test_dir, "output-diff.generated")
            diff_output = f"Expected:\n{expected_output}\n\nGot:\n{actual_output}"
            write_to_file(diff_path, diff_output)
            print("|---- ❌ java output did not match expected output")
            return False
        else:
            print("|--- ✅ Output matches expected output!")
    else:
        print("|--- ⚠️ Expected output file not found. Skipping comparison.")
    
    print("|--- ✅ Binary test passed!")
    return True

def main():
    print("🧪 Tester for Rustc's JVM Codegen Backend started!")
    overall_success = True

    print(" ")

    # Process binary tests.
    binary_dir = os.path.join("tests", "binary")
    if os.path.isdir(binary_dir):
        binary_tests = [os.path.join(binary_dir, d) for d in os.listdir(binary_dir) if os.path.isdir(os.path.join(binary_dir, d))]
    else:
        binary_tests = []
    print(f"|- 📦 Running {len(binary_tests)} binary build tests...")
    for idx, test_dir in enumerate(binary_tests):
        if not process_test(test_dir):
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
