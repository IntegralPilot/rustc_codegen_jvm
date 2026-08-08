#![cfg(unix)]

use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

fn cargo_jvm(temp: &TempDir) -> Command {
    let rustup = temp.path().join("rustup");
    executable(
        &rustup,
        "#!/bin/sh\nif [ \"$1\" = component ] && [ \"$2\" = list ]; then\n  printf '%s\\n' rustc-dev-test-host rust-src llvm-tools-test-host\n  exit 0\nfi\nif [ \"$1\" = run ]; then\n  shift 2\n  exec \"$@\"\nfi\nexit 0\n",
    );
    let mut command = Command::new(env!("CARGO_BIN_EXE_cargo-jvm"));
    command.env("RUSTUP", rustup);
    command
}

fn executable(path: &Path, contents: &str) {
    fs::write(path, contents).unwrap();
    let mut permissions = fs::metadata(path).unwrap().permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(path, permissions).unwrap();
}

fn fake_backend(temp: &TempDir) -> PathBuf {
    let root = temp.path().join("backend");
    fs::create_dir_all(root.join("java-linker/target/release")).unwrap();
    fs::create_dir_all(root.join("runtime/build/libs")).unwrap();
    fs::write(root.join("jvm-unknown-unknown.json"), "{}").unwrap();
    fs::write(root.join("config.toml"), "").unwrap();
    fs::write(root.join("Cargo.toml"), "[package]\nname = \"backend\"\n").unwrap();
    fs::write(
        root.join("rust-toolchain.toml"),
        "[toolchain]\nchannel = \"nightly-2099-01-01\"\nprofile = \"minimal\"\ncomponents = [\"rustc-dev\", \"rust-src\", \"llvm-tools-preview\"]\n",
    )
    .unwrap();
    fs::write(root.join("stdlib_overlay.py"), "").unwrap();
    fs::write(root.join("build.py"), "").unwrap();
    fs::write(root.join("runtime/build/libs/runtime-0.1.0.jar"), "").unwrap();
    executable(
        &root.join("java-linker/target/release/java-linker"),
        "#!/bin/sh\nexit 0\n",
    );
    root
}

fn fake_python(temp: &TempDir) -> (PathBuf, PathBuf) {
    let capture = temp.path().join("python-arguments");
    let python = temp.path().join("python");
    executable(
        &python,
        "#!/bin/sh\nif [ \"$1\" = --version ]; then exit 0; fi\nprintf '%s\\n' \"$@\" > \"$PYTHON_CAPTURE\"\n",
    );
    (python, capture)
}

fn commit_fake_backend(backend: &Path, subject: &str) -> String {
    let git = |arguments: &[&str]| {
        Command::new("git")
            .arg("-C")
            .arg(backend)
            .args(arguments)
            .output()
            .unwrap()
    };
    assert!(git(&["init", "--quiet"]).status.success());
    assert!(git(&["add", "."]).status.success());
    assert!(
        git(&[
            "-c",
            "user.name=cargo-jvm tests",
            "-c",
            "user.email=cargo-jvm@example.invalid",
            "commit",
            "--quiet",
            "-m",
            subject,
        ])
        .status
        .success()
    );
    String::from_utf8(git(&["rev-parse", "HEAD"]).stdout)
        .unwrap()
        .trim()
        .to_string()
}

#[test]
fn setup_persists_the_backend_path() {
    let temp = TempDir::new().unwrap();
    let backend = fake_backend(&temp);
    let config = temp.path().join("config/config.toml");
    let (python, capture) = fake_python(&temp);
    let output = cargo_jvm(&temp)
        .env("CARGO_JVM_CONFIG", &config)
        .env("PYTHON", python)
        .env("PYTHON_CAPTURE", capture)
        .arg("setup")
        .arg(&backend)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let contents = fs::read_to_string(config).unwrap();
    assert!(contents.contains(&backend.canonicalize().unwrap().display().to_string()));
}

#[test]
fn setup_installs_the_backend_pinned_toolchain_when_missing() {
    let temp = TempDir::new().unwrap();
    let backend = fake_backend(&temp);
    let config = temp.path().join("config/config.toml");
    let (python, capture) = fake_python(&temp);
    let rustup_capture = temp.path().join("rustup-arguments");
    let rustup = temp.path().join("rustup-install");
    executable(
        &rustup,
        "#!/bin/sh\nif [ \"$1\" = component ] && [ \"$2\" = list ]; then exit 1; fi\nprintf '%s\\n' \"$@\" > \"$RUSTUP_CAPTURE\"\n",
    );

    let output = cargo_jvm(&temp)
        .env("CARGO_JVM_CONFIG", &config)
        .env("PYTHON", python)
        .env("PYTHON_CAPTURE", capture)
        .env("RUSTUP", rustup)
        .env("RUSTUP_CAPTURE", &rustup_capture)
        .arg("setup")
        .arg(&backend)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let arguments = fs::read_to_string(rustup_capture).unwrap();
    assert!(arguments.starts_with("toolchain\ninstall\nnightly-2099-01-01\n"));
    assert!(arguments.contains("--profile\nminimal\n"));
    assert!(arguments.contains("--component\nrustc-dev,rust-src,llvm-tools-preview\n"));
}

#[test]
fn doctor_reports_tool_and_backend_versions() {
    let temp = TempDir::new().unwrap();
    let backend = fake_backend(&temp);
    let subject = "backend commit shown by doctor";
    let commit = commit_fake_backend(&backend, subject);

    let cargo = temp.path().join("cargo");
    let rustc = temp.path().join("rustc");
    let java = temp.path().join("java");
    executable(&cargo, "#!/bin/sh\nprintf 'cargo test-version\\n'\n");
    executable(&rustc, "#!/bin/sh\nprintf 'rustc test-version\\n'\n");
    executable(&java, "#!/bin/sh\nprintf 'java test-version\\n' >&2\n");

    let output = cargo_jvm(&temp)
        .env("CARGO_JVM_CARGO", cargo)
        .env("RUSTC", rustc)
        .env("JAVA", java)
        .arg("--backend-path")
        .arg(&backend)
        .arg("doctor")
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains(&format!("cargo-jvm: {}", env!("CARGO_PKG_VERSION"))));
    assert!(stdout.contains(&format!("rustc_codegen_jvm HEAD: {commit}\t{subject}")));
    assert!(stdout.contains("pinned Rust toolchain: nightly-2099-01-01"));
    assert!(stdout.contains("cargo: cargo test-version"));
    assert!(stdout.contains("rustc: rustc test-version"));
    assert!(stdout.contains("java: java test-version"));
}

#[test]
fn build_forwards_cargo_arguments_and_adds_the_jvm_toolchain() {
    let temp = TempDir::new().unwrap();
    let backend = fake_backend(&temp);
    let overlay = temp.path().join("overlay");
    fs::create_dir(&overlay).unwrap();
    let capture = temp.path().join("cargo-arguments");
    let toolchain_capture = temp.path().join("cargo-toolchain");
    let cargo = temp.path().join("cargo");
    executable(
        &cargo,
        "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$CARGO_CAPTURE\"\nprintf '%s\\n' \"$RUSTUP_TOOLCHAIN\" > \"$TOOLCHAIN_CAPTURE\"\n",
    );

    let status = cargo_jvm(&temp)
        .env("CARGO_JVM_CARGO", cargo)
        .env("CARGO_CAPTURE", &capture)
        .env("TOOLCHAIN_CAPTURE", &toolchain_capture)
        .env("__CARGO_TESTS_ONLY_SRC_ROOT", overlay)
        .arg("--backend-path")
        .arg(&backend)
        .args(["build", "--release", "--features", "demo"])
        .status()
        .unwrap();

    assert!(status.success());
    let arguments = fs::read_to_string(capture).unwrap();
    assert!(arguments.starts_with("build\n--target\n"));
    assert!(arguments.contains("-Zbuild-std=std,panic_unwind\n"));
    assert!(arguments.ends_with("--release\n--features\ndemo\n"));
    assert_eq!(
        fs::read_to_string(toolchain_capture).unwrap(),
        "nightly-2099-01-01\n"
    );
}

#[test]
fn run_launches_the_reported_binary_jar_with_java_options() {
    let temp = TempDir::new().unwrap();
    let backend = fake_backend(&temp);
    let overlay = temp.path().join("overlay");
    fs::create_dir(&overlay).unwrap();
    let jar = temp.path().join("demo.jar");
    fs::write(&jar, "").unwrap();
    let cargo = temp.path().join("cargo");
    executable(
        &cargo,
        &format!(
            "#!/bin/sh\nprintf '%s\\n' '{}'\n",
            serde_json::json!({
                "reason": "compiler-artifact",
                "package_id": "path+file:///demo#0.1.0",
                "target": {"name": "demo", "kind": ["bin"]},
                "profile": {"test": false},
                "filenames": [jar],
                "executable": jar,
            })
        ),
    );
    let java_capture = temp.path().join("java-arguments");
    let java = temp.path().join("java");
    executable(
        &java,
        "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$JAVA_CAPTURE\"\n",
    );

    let status = cargo_jvm(&temp)
        .env("CARGO_JVM_CARGO", cargo)
        .env("JAVA", java)
        .env("JAVA_CAPTURE", &java_capture)
        .env("__CARGO_TESTS_ONLY_SRC_ROOT", overlay)
        .arg("--backend-path")
        .arg(&backend)
        .args([
            "run",
            "--stack",
            "8m",
            "--java-arg=-ea",
            "--release",
            "--",
            "hello",
        ])
        .status()
        .unwrap();

    assert!(status.success());
    let arguments = fs::read_to_string(java_capture).unwrap();
    assert_eq!(
        arguments,
        format!("-Xss8m\n-ea\n-jar\n{}\nhello\n", jar.display())
    );
}

#[test]
fn package_links_library_rlibs_and_the_runtime_into_the_output_jar() {
    let temp = TempDir::new().unwrap();
    let backend = fake_backend(&temp);
    let overlay = temp.path().join("overlay");
    fs::create_dir(&overlay).unwrap();
    let rlib = temp.path().join("libdemo.rlib");
    fs::write(&rlib, "").unwrap();
    let target = temp.path().join("target");
    let cargo = temp.path().join("cargo");
    executable(
        &cargo,
        &format!(
            "#!/bin/sh\nif [ \"$1\" = metadata ]; then\n  printf '%s\\n' '{}'\nelse\n  printf '%s\\n' '{}'\nfi\n",
            serde_json::json!({
                "workspace_members": ["path+file:///demo#0.1.0"],
                "target_directory": target,
            }),
            serde_json::json!({
                "reason": "compiler-artifact",
                "package_id": "path+file:///demo#0.1.0",
                "target": {"name": "demo", "kind": ["lib"]},
                "profile": {"test": false},
                "filenames": [rlib],
                "executable": null,
            }),
        ),
    );
    let linker_capture = temp.path().join("linker-arguments");
    executable(
        &backend.join("java-linker/target/release/java-linker"),
        "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$LINKER_CAPTURE\"\nwhile [ \"$#\" -gt 0 ]; do\n  if [ \"$1\" = -o ]; then\n    shift\n    : > \"$1\"\n    exit 0\n  fi\n  shift\ndone\nexit 1\n",
    );
    let output = temp.path().join("dist/demo.jar");

    let status = cargo_jvm(&temp)
        .env("CARGO_JVM_CARGO", cargo)
        .env("LINKER_CAPTURE", &linker_capture)
        .env("__CARGO_TESTS_ONLY_SRC_ROOT", overlay)
        .arg("--backend-path")
        .arg(&backend)
        .args(["package", "--output"])
        .arg(&output)
        .status()
        .unwrap();

    assert!(status.success());
    assert!(output.is_file());
    let arguments = fs::read_to_string(linker_capture).unwrap();
    assert!(arguments.contains(&rlib.display().to_string()));
    assert!(arguments.contains("runtime-0.1.0.jar"));
    assert!(arguments.ends_with(&format!("-o\n{}\n", output.display())));
}

#[test]
fn test_launches_each_cargo_test_artifact() {
    let temp = TempDir::new().unwrap();
    let backend = fake_backend(&temp);
    let overlay = temp.path().join("overlay");
    fs::create_dir(&overlay).unwrap();
    let jar = temp.path().join("demo-test.jar");
    fs::write(&jar, "").unwrap();
    let cargo = temp.path().join("cargo");
    executable(
        &cargo,
        &format!(
            "#!/bin/sh\nprintf '%s\\n' '{}'\n",
            serde_json::json!({
                "reason": "compiler-artifact",
                "package_id": "path+file:///demo#0.1.0",
                "target": {"name": "demo", "kind": ["lib"]},
                "profile": {"test": true},
                "filenames": [jar],
                "executable": jar,
            })
        ),
    );
    let java_capture = temp.path().join("java-arguments");
    let java = temp.path().join("java");
    executable(
        &java,
        "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$JAVA_CAPTURE\"\n",
    );

    let status = cargo_jvm(&temp)
        .env("CARGO_JVM_CARGO", cargo)
        .env("JAVA", java)
        .env("JAVA_CAPTURE", &java_capture)
        .env("__CARGO_TESTS_ONLY_SRC_ROOT", overlay)
        .arg("--backend-path")
        .arg(&backend)
        .args(["test", "--", "--nocapture"])
        .status()
        .unwrap();

    assert!(status.success());
    assert_eq!(
        fs::read_to_string(java_capture).unwrap(),
        format!("-Xss16m\n-jar\n{}\n--nocapture\n", jar.display())
    );
}

#[test]
fn update_pulls_and_rebuilds_the_configured_checkout() {
    let temp = TempDir::new().unwrap();
    let backend = fake_backend(&temp);
    fs::create_dir(backend.join(".git")).unwrap();
    let config = temp.path().join("config/config.toml");
    let (python, python_capture) = fake_python(&temp);
    assert!(
        cargo_jvm(&temp)
            .env("CARGO_JVM_CONFIG", &config)
            .env("PYTHON", &python)
            .env("PYTHON_CAPTURE", &python_capture)
            .arg("setup")
            .arg(&backend)
            .status()
            .unwrap()
            .success()
    );

    let git_capture = temp.path().join("git-arguments");
    let git = temp.path().join("git");
    executable(
        &git,
        "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$GIT_CAPTURE\"\n",
    );
    let status = cargo_jvm(&temp)
        .env("CARGO_JVM_CONFIG", &config)
        .env("GIT", git)
        .env("GIT_CAPTURE", &git_capture)
        .env("PYTHON", python)
        .env("PYTHON_CAPTURE", &python_capture)
        .arg("update")
        .status()
        .unwrap();

    assert!(status.success());
    let git_arguments = fs::read_to_string(git_capture).unwrap();
    assert!(git_arguments.contains("-C\n"));
    assert!(git_arguments.ends_with("pull\n--ff-only\n"));
    assert!(
        fs::read_to_string(python_capture)
            .unwrap()
            .contains("build.py\nall\n")
    );
}

#[test]
fn setup_without_arguments_clones_builds_and_configures_a_backend() {
    let temp = TempDir::new().unwrap();
    let source = fake_backend(&temp);
    let destination = temp.path().join("installed-backend");
    let config = temp.path().join("config/config.toml");
    let git_capture = temp.path().join("git-arguments");
    let git = temp.path().join("git");
    executable(
        &git,
        "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$GIT_CAPTURE\"\ncp -R \"$FAKE_SOURCE\" \"$3\"\n",
    );
    let (python, python_capture) = fake_python(&temp);

    let status = cargo_jvm(&temp)
        .env("CARGO_JVM_CONFIG", &config)
        .env("GIT", git)
        .env("GIT_CAPTURE", &git_capture)
        .env("FAKE_SOURCE", &source)
        .env("CARGO_JVM_HOME", &destination)
        .env("PYTHON", python)
        .env("PYTHON_CAPTURE", &python_capture)
        .arg("setup")
        .status()
        .unwrap();

    assert!(status.success());
    assert_eq!(
        fs::read_to_string(git_capture).unwrap(),
        format!(
            "clone\nhttps://github.com/IntegralPilot/rustc_codegen_jvm.git\n{}\n",
            destination.display()
        )
    );
    assert!(
        fs::read_to_string(python_capture)
            .unwrap()
            .contains("build.py\nall\n")
    );
    assert!(
        fs::read_to_string(config)
            .unwrap()
            .contains(&destination.canonicalize().unwrap().display().to_string())
    );
}
