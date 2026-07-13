#!/usr/bin/env python3

import os
import sys
import subprocess
import shutil
import argparse
import platform
from pathlib import Path
from typing import List

# --- Terminal Colors ---
class Colors:
    GREEN = '\033[92m'
    CYAN = '\033[96m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    RESET = '\033[0m'

# --- Configuration ---
# All paths and versions are defined here for easy updates.
class Config:
    ROOT_DIR = Path(__file__).parent.resolve()
    IS_CI = os.getenv("IS_CI") == "1"

    # Subproject Directories
    JAVA_LINKER_DIR = ROOT_DIR / "java-linker"
    LIBRARY_DIR = ROOT_DIR / "library"

    # Versions (centralized management)
    LIBRARY_VERSION = "0.1.0"

    # Source File Lists (used for dependency tracking)
    # Using glob patterns for automatic discovery
    LIBRARY_SOURCES = sorted(LIBRARY_DIR.glob("src/**/*.java"))
    BACKEND_RUST_SOURCES = list(ROOT_DIR.glob("src/**/*.rs"))
    LINKER_RUST_SOURCES = list(JAVA_LINKER_DIR.glob("src/**/*.rs"))

    # Key Target Files
    LIBRARY_JAR = LIBRARY_DIR / f"build/libs/library-{LIBRARY_VERSION}.jar"
    LIBRARY_CLASSES_DIR = LIBRARY_DIR / "build/classes"
    CONFIG_TOML = ROOT_DIR / "config.toml"
    JVM_TARGET_JSON = ROOT_DIR / "jvm-unknown-unknown.json"

    # Platform-specific details
    if platform.system() == "Windows":
        DYLIB_PREFIX = ""
        DYLIB_SUFFIX = ".dll"
    elif platform.system() == "Darwin":
        DYLIB_PREFIX = "lib"
        DYLIB_SUFFIX = ".dylib"
    else:
        DYLIB_PREFIX = "lib"
        DYLIB_SUFFIX = ".so"
        
    RUST_BACKEND_DYLIB = ROOT_DIR / "target/release" / f"{DYLIB_PREFIX}rustc_codegen_jvm{DYLIB_SUFFIX}"
    JAVA_LINKER_EXE = JAVA_LINKER_DIR / "target/release/java-linker"

# --- Helper Functions ---

def run_command(cmd: List[str], cwd: Path = None, check: bool = True):
    """Runs a command, prints it, and checks for errors."""
    cwd_str = f" (in {cwd})" if cwd else ""
    print(f"{Colors.YELLOW}▶️  Running: {' '.join(cmd)}{cwd_str}{Colors.RESET}")
    try:
        proc = subprocess.run(
            cmd, 
            cwd=cwd, 
            check=check,
            stdout=sys.stdout, # Stream output directly
            stderr=sys.stderr
        )
        return proc
    except FileNotFoundError as e:
        print(f"{Colors.RED}❌ Error: Command '{cmd[0]}' not found. Is it in your PATH?{Colors.RESET}")
        print(f"   Details: {e}")
        sys.exit(1)
    except subprocess.CalledProcessError as e:
        print(f"{Colors.RED}❌ Command failed with exit code {e.returncode}.{Colors.RESET}")
        sys.exit(e.returncode)

def is_stale(target: Path, sources: List[Path]) -> bool:
    """Checks if the target file is older than any source file."""
    if not target.exists():
        return True
    target_mtime = target.stat().st_mtime
    for source in sources:
        if not source.exists():
             # If a source is missing, something is wrong, but we can't compare.
             # This might warrant an error or warning depending on the case.
             continue
        if source.stat().st_mtime > target_mtime:
            return True
    return False

# --- Build Tasks (Replaces Makefile Targets) ---

def clean():
    """Cleans all generated files and build artifacts."""
    print(f"{Colors.CYAN}🧹 Cleaning all components...{Colors.RESET}")
    
    # Root cargo clean
    run_command(["cargo", "clean"], cwd=Config.ROOT_DIR)
    
    # Subproject cargo clean
    run_command(["cargo", "clean"], cwd=Config.JAVA_LINKER_DIR)

    # Remove specific files and directories
    for path in [
        Config.CONFIG_TOML, 
        Config.JVM_TARGET_JSON,
        Config.LIBRARY_DIR / "build"
    ]:
        try:
            if path.is_dir():
                shutil.rmtree(path)
            elif path.is_file():
                path.unlink()
        except FileNotFoundError:
            pass # It's already clean
    print(f"{Colors.GREEN}🧼 All clean!{Colors.RESET}")

def install_rust_components():
    """Installs required Rust nightly components."""
    print(f"{Colors.CYAN}🔧 Installing Rust components...{Colors.RESET}")
    run_command(["rustup", "component", "add", "rustc-dev", "llvm-tools"])

def build_library():
    """Builds the Java standard library shim."""
    if not is_stale(Config.LIBRARY_JAR, Config.LIBRARY_SOURCES):
        print(f"{Colors.GREEN}✅ Java library shim is up to date.{Colors.RESET}")
        return

    if not Config.LIBRARY_SOURCES:
        print(f"{Colors.RED}❌ No Java sources found under {Config.LIBRARY_DIR / 'src'}{Colors.RESET}")
        sys.exit(1)

    print(f"{Colors.CYAN}📚 Building Java library shim...{Colors.RESET}")

    if Config.LIBRARY_CLASSES_DIR.exists():
        shutil.rmtree(Config.LIBRARY_CLASSES_DIR)
    Config.LIBRARY_CLASSES_DIR.mkdir(parents=True, exist_ok=True)
    Config.LIBRARY_JAR.parent.mkdir(parents=True, exist_ok=True)

    source_paths = [str(path) for path in Config.LIBRARY_SOURCES]
    javac_base = ["javac", "-Xlint:-options", "-d", str(Config.LIBRARY_CLASSES_DIR)]
    proc = run_command(["javac", "--release", "8"] + javac_base[1:] + source_paths, check=False)
    if proc.returncode != 0:
        print(
            f"{Colors.YELLOW}⚠️  javac --release 8 failed; retrying with -source 8 -target 8.{Colors.RESET}"
        )
        run_command(["javac", "-source", "8", "-target", "8"] + javac_base[1:] + source_paths)

    run_command([
        "jar",
        "cf",
        str(Config.LIBRARY_JAR),
        "-C",
        str(Config.LIBRARY_CLASSES_DIR),
        ".",
    ])

    if not Config.LIBRARY_JAR.exists():
        print(f"{Colors.RED}❌ Expected library JAR was not created: {Config.LIBRARY_JAR}{Colors.RESET}")
        sys.exit(1)

    print(f"{Colors.GREEN}   Java library shim built successfully.{Colors.RESET}")

def build_rust_backend():
    """Builds the main rustc_codegen_jvm backend."""
    sources = Config.BACKEND_RUST_SOURCES
    if not is_stale(Config.RUST_BACKEND_DYLIB, sources):
        print(f"{Colors.GREEN}✅ Rust codegen backend is up to date.{Colors.RESET}")
        return
    
    print(f"{Colors.CYAN}📦 Building Rust codegen backend...{Colors.RESET}")
    # Setting RUSTFLAGS as an environment variable is more robust.
    env = os.environ.copy()
    env["RUSTFLAGS"] = "-Awarnings"
    subprocess.run(["cargo", "build", "--release"], cwd=Config.ROOT_DIR, check=True, env=env)
    print(f"{Colors.GREEN}   Rust codegen backend built successfully.{Colors.RESET}")

def build_java_linker():
    """Builds the java-linker subproject."""
    if not is_stale(Config.JAVA_LINKER_EXE, Config.LINKER_RUST_SOURCES):
        print(f"{Colors.GREEN}✅ Java linker is up to date.{Colors.RESET}")
        return
        
    print(f"{Colors.CYAN}📦 Building Java linker...{Colors.RESET}")
    env = os.environ.copy()
    env["RUSTFLAGS"] = "-Awarnings"
    subprocess.run(["cargo", "build", "--release"], cwd=Config.JAVA_LINKER_DIR, check=True, env=env)
    print(f"{Colors.GREEN}   Java linker built successfully.{Colors.RESET}")
    
def generate_config_files():
    """Generates config.toml and jvm-unknown-unknown.json from templates."""
    print(f"{Colors.CYAN}🛠️  Generating configuration files from templates...{Colors.RESET}")

    # Logic from GenerateFiles.py is now integrated here
    replacements = {
        "../../../": Config.ROOT_DIR.as_posix() + "/",
        "@RUST_BACKEND_DYLIB@": Config.RUST_BACKEND_DYLIB.name,
    }

    for template_path in Config.ROOT_DIR.glob("*.template"):
        content = template_path.read_text()
        for old, new in replacements.items():
            content = content.replace(old, new)
        
        target_path = template_path.with_suffix("") # Removes .template
        target_path.write_text(content)
        print(f"   Generated {target_path.name}")
    print(f"{Colors.GREEN}   Configuration files generated.{Colors.RESET}")
    
def all_tasks():
    """Runs all necessary build tasks in the correct order."""
    if not Config.IS_CI:
        install_rust_components()

    generate_config_files()
    
    # The order defines the dependency chain.
    build_library()
    build_rust_backend()
    build_java_linker()

    print(f"\n{Colors.GREEN}✨ Build complete! ✨{Colors.RESET}")

def help_message():
    print(f"{Colors.CYAN}🛠️  Build script for rustc_codegen_jvm{Colors.RESET}")
    print("\nAvailable commands:")
    for cmd, (func, help_text) in TARGETS.items():
        print(f"  {cmd:<20} - {help_text}")
    print("\nDefault command is 'all'.")

# --- Main Execution ---

# Map command-line arguments to functions
TARGETS = {
    "all": (all_tasks, "Build all components (default)."),
    "clean": (clean, "Clean all build artifacts."),
    "ci": (all_tasks, "Build all components in CI mode."),
    "rust-components": (install_rust_components, "Install needed Rust components."),
    "rust": (build_rust_backend, "Build the Rust root project."),
    "java-linker": (build_java_linker, "Build the Java Linker subproject."),
    "library": (build_library, "Build the standard library shim."),
    "gen-files": (generate_config_files, "Generate necessary files from templates."),
    "help": (help_message, "Show this help message."),
}

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Build script for rustc_codegen_jvm.")
    parser.add_argument(
        "target", 
        nargs="?", 
        default="all", 
        choices=TARGETS.keys(),
        help=f"The build target to run. One of: {', '.join(TARGETS.keys())}"
    )
    args = parser.parse_args()

    # Set CI flag if target is 'ci'
    if args.target == 'ci':
        os.environ['IS_CI'] = '1'
        Config.IS_CI = True

    # Execute the chosen target function
    target_func, _ = TARGETS[args.target]
    target_func()
