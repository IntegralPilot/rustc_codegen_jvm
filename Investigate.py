#!/usr/bin/env python3

import os
import subprocess
import sys
import argparse
import shutil

RUNTIME_JAR = os.path.join("library", "build", "libs", "library-0.1.0.jar")


# --- Helper Functions ---

def run_command(cmd: list, cwd=None):
    """Runs a command and captures its output, printing the command first."""
    print(f"|-----> Running: {' '.join(cmd)}")
    proc = subprocess.run(
        cmd, 
        cwd=cwd, 
        stdout=subprocess.PIPE, 
        stderr=subprocess.PIPE, 
        text=True, 
        encoding='utf-8', 
        errors='replace'
    )
    return proc

def write_to_file(path: str, content: str):
    """Writes content to a file, creating parent directories if they don't exist."""
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding='utf-8') as f:
        f.write(content)

# --- Main Investigation Logic ---

def investigate_test(test_name: str, release_mode: bool):
    """Runs a single test and gathers detailed investigation artifacts."""
    mode = "release" if release_mode else "debug"
    print(f"🔬 Starting investigation for test '{test_name}' in '{mode}' mode.")

    # 1. Set up paths
    test_dir = os.path.join("tests", "binary", test_name)
    investigation_dir = os.path.join("investigation", f"{test_name}_{mode}")
    javap_dir = os.path.join(investigation_dir, "javap_output")
    extracted_jar_dir = os.path.join(investigation_dir, "extracted_jar")

    if not os.path.isdir(test_dir):
        print(f"❌ Error: Test directory not found at '{test_dir}'")
        sys.exit(1)

    # 2. Create a clean investigation directory
    print(f"|-- 📁 Setting up investigation directory: '{investigation_dir}'")
    if os.path.exists(investigation_dir):
        shutil.rmtree(investigation_dir)
    os.makedirs(javap_dir)
    os.makedirs(extracted_jar_dir)

    # 3. Clean the project
    print("|-- 🧼 Cleaning test folder...")
    run_command(["cargo", "clean"], cwd=test_dir)

    # 4. Build with Cargo and capture output
    print("|-- ⚒️ Building with Cargo and capturing logs...")
    build_cmd = ["cargo", "build", "--release"] if release_mode else ["cargo", "build"]
    use_target_json = os.path.join(test_dir, "use_target_json.flag")
    if os.path.exists(use_target_json):
        build_cmd.extend(["--target", "../../../jvm-unknown-unknown.json"])

    proc = run_command(build_cmd, cwd=test_dir)
    build_log_content = f"--- COMMAND ---\n{' '.join(build_cmd)}\n\n--- RETURN CODE: {proc.returncode} ---\n\n--- STDOUT ---\n{proc.stdout}\n\n--- STDERR ---\n{proc.stderr}"
    write_to_file(os.path.join(investigation_dir, "cargo_build.log"), build_log_content)

    if proc.returncode != 0:
        print(f"|---- ❌ cargo build failed with code {proc.returncode}. See cargo_build.log for details.")
    else:
        print("|---- ✅ cargo build succeeded.")
    
    # 5. Locate the JAR file, handling special cases
    print("|-- 🔎 Locating generated JAR file...")
    target_dir = "release" if release_mode else "debug"
    
    # Handle the case where no custom target JSON is used ('use_target_json.flag' doesn't exist)
    if not os.path.exists(use_target_json):
        print("|---- No 'use_target_json.flag' found, searching for JAR in standard deps folder...")
        deps_dir_temp = os.path.join(test_dir, "target", target_dir, "deps")
        jar_file_name = None
        if os.path.isdir(deps_dir_temp):
            for file in os.listdir(deps_dir_temp):
                if file.startswith(test_name) and file.endswith(".jar"):
                    jar_file_name = file
                    break
        
        if jar_file_name is not None:
            # Move the jar to the location expected by the rest of the script for consistency
            src_path = os.path.join(deps_dir_temp, jar_file_name)
            dest_dir = os.path.join(test_dir, "target", "jvm-unknown-unknown", target_dir)
            os.makedirs(dest_dir, exist_ok=True)
            dest_path = os.path.join(dest_dir, f"{test_name}.jar")
            shutil.move(src_path, dest_path)
            print(f"|---- Moved JAR to {dest_path}")
        else:
            print(f"|---- ⚠️ No JAR file found in {deps_dir_temp}. Will attempt to use raw classfiles later.")

    jar_path = os.path.join(test_dir, "target", "jvm-unknown-unknown", target_dir, f"{test_name}.jar")

    # Check if JAR exists, if not fall back to using raw classfiles from deps
    use_raw_classfiles = False
    deps_dir = None
    
    if not os.path.exists(jar_path):
        print(f"|---- ⚠️ JAR file not found at expected path: {jar_path}")
        print("|---- Attempting to use raw .class files from deps directory...")
        
        # Fall back to using the deps directory
        deps_dir = os.path.join(test_dir, "target", target_dir, "deps")
        if os.path.exists(deps_dir):
            main_class = os.path.join(deps_dir, f"{test_name}.class")
            if os.path.exists(main_class):
                print(f"|---- ✅ Found main class file: {main_class}")
                print(f"|---- Continuing investigation with raw classfiles from: {deps_dir}")
                use_raw_classfiles = True
            else:
                print(f"|---- ❌ Main class file not found: {main_class}")
                print("|---- Investigation cannot proceed. Exiting.")
                return
        else:
            print(f"|---- ❌ Deps directory not found: {deps_dir}")
            print("|---- Investigation cannot proceed. Exiting.")
            return
    else:
        print(f"|---- ✅ Found JAR: {jar_path}")

    # 6. Analyze the class files
    if use_raw_classfiles and deps_dir is not None:
        # Using raw classfiles from deps directory
        print("|-- 📋 Listing class files from deps directory...")
        class_files = []
        for file in os.listdir(deps_dir):
            if file.endswith(".class"):
                class_files.append(file)
        
        contents_txt = "\n".join(sorted(class_files))
        write_to_file(os.path.join(investigation_dir, "class_files_list.txt"), contents_txt)
        print(f"|---- Found {len(class_files)} class file(s)")
        
        # Copy class files to extracted_jar_dir for consistency
        print("|-- 📦 Copying class files for analysis...")
        for file in class_files:
            src = os.path.join(deps_dir, file)
            dst = os.path.join(extracted_jar_dir, file)
            shutil.copy(src, dst)
        
        # 6c. `javap -v -p` on all .class files
        print("|-- 뜯 Decompiling .class files (javap -v -p)...")
        class_count = 0
        for file in class_files:
            class_count += 1
            class_file_path = os.path.join(extracted_jar_dir, file)
            output_file_path = os.path.join(javap_dir, file.replace(".class", ".javap.txt"))

            # Run javap on the .class file
            proc = run_command(["javap", "-v", "-p", class_file_path])
            output_content = f"--- COMMAND ---\njavap -v -p {class_file_path}\n\n--- STDOUT ---\n{proc.stdout}\n\n--- STDERR ---\n{proc.stderr}"
            write_to_file(output_file_path, output_content)

        print(f"|---- ✅ Decompiled {class_count} class file(s).")
    else:
        # Using JAR file
        # 6a. `jar tf` - List JAR contents
        print("|-- 🔍 Listing JAR contents (jar tf)...")
        proc = run_command(["jar", "tf", jar_path])
        write_to_file(os.path.join(investigation_dir, "jar_contents.txt"), proc.stdout)
        
        # 6b. `jar xf` - Extract JAR
        print("|-- 📦 Extracting JAR contents for analysis...")
        run_command(["jar", "xf", os.path.abspath(jar_path)], cwd=extracted_jar_dir)
        
        # 6c. `javap -v -p` on all .class files
        print("|-- 뜯 Decompiling .class files (javap -v -p)...")
        class_count = 0
        for root, _, files in os.walk(extracted_jar_dir):
            for file in files:
                if file.endswith(".class"):
                    class_count += 1
                    class_file_path = os.path.join(root, file)
                    relative_path = os.path.relpath(class_file_path, extracted_jar_dir)
                    
                    # Create a mirrored directory structure for the output
                    output_path_dir = os.path.join(javap_dir, os.path.dirname(relative_path))
                    output_file_path = os.path.join(output_path_dir, os.path.basename(relative_path).replace(".class", ".javap.txt"))

                    # Run javap on the .class file
                    proc = run_command(["javap", "-v", "-p", class_file_path])
                    output_content = f"--- COMMAND ---\njavap -v -p {class_file_path}\n\n--- STDOUT ---\n{proc.stdout}\n\n--- STDERR ---\n{proc.stderr}"
                    write_to_file(output_file_path, output_content)

        print(f"|---- ✅ Decompiled {class_count} class file(s).")
    
    # 7. Run with Java and capture output
    print("|-- 🤖 Running with Java and capturing logs...")
    
    if use_raw_classfiles and deps_dir is not None:
        classpath = os.pathsep.join([RUNTIME_JAR, deps_dir])
        print(f"|---- Using deps directory in classpath: {deps_dir}")
        java_cmd = ["java", "-cp", classpath, test_name]
    else:
        classpath = jar_path
        java_cmd = ["java", "-cp", classpath, test_name]
    
    proc = run_command(java_cmd)
    
    run_log_content = f"--- COMMAND ---\n{' '.join(java_cmd)}\n\n--- RETURN CODE: {proc.returncode} ---\n\n--- STDOUT ---\n{proc.stdout}\n\n--- STDERR ---\n{proc.stderr}"
    write_to_file(os.path.join(investigation_dir, "java_run.log"), run_log_content)

    if proc.returncode != 0:
        print(f"|---- ❌ Java process exited with non-zero code: {proc.returncode}. See java_run.log for details.")
    else:
        print("|---- ✅ Java process exited successfully (code 0).")
        
    print(f"\n✨ Investigation complete! All artifacts are in '{investigation_dir}'")

def main():
    parser = argparse.ArgumentParser(
        description="Run a single Rustc JVM backend test and gather detailed artifacts for investigation.",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument("test_name", help="The name of the test directory to investigate (e.g., 'hello_world').")
    parser.add_argument("--release", action="store_true", help="Run the test in release mode (cargo build --release).")
    args = parser.parse_args()

    investigate_test(args.test_name, args.release)

if __name__ == "__main__":
    main()
