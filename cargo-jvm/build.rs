use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-env-changed=CARGO_JVM_GIT_HASH");
    println!("cargo:rerun-if-changed=Cargo.toml");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src");

    if let Ok(hash) = env::var("CARGO_JVM_GIT_HASH") {
        emit_hash(hash);
        return;
    }

    let manifest = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let Some(repository) = manifest.parent().filter(|root| root.join(".git").exists()) else {
        emit_packaged_git_identity(&manifest);
        return;
    };

    if let Some(git_dir) = git_dir(repository) {
        println!("cargo:rerun-if-changed={}", git_dir.join("HEAD").display());
        println!(
            "cargo:rerun-if-changed={}",
            git_dir.join("logs/HEAD").display()
        );
    }

    let output = Command::new("git")
        .arg("-C")
        .arg(repository)
        .args(["rev-parse", "--short=12", "HEAD"])
        .output();
    if let Ok(output) = output
        && output.status.success()
    {
        emit_hash(String::from_utf8_lossy(&output.stdout).into_owned());
    }

    let status = Command::new("git")
        .arg("-C")
        .arg(repository)
        .args([
            "status",
            "--porcelain",
            "--",
            "cargo-jvm/Cargo.toml",
            "cargo-jvm/build.rs",
            "cargo-jvm/src",
        ])
        .output();
    if let Ok(status) = status
        && status.status.success()
        && !status.stdout.is_empty()
    {
        emit_dirty();
    }
}

fn emit_packaged_git_identity(manifest: &Path) {
    let vcs_info = manifest.join(".cargo_vcs_info.json");
    println!("cargo:rerun-if-changed={}", vcs_info.display());
    let Ok(contents) = fs::read_to_string(vcs_info) else {
        return;
    };
    if let Some(hash) = json_value(&contents, "sha1") {
        emit_hash(hash.chars().take(12).collect());
    }
    if json_value(&contents, "dirty").as_deref() == Some("true") {
        emit_dirty();
    }
}

fn json_value(contents: &str, key: &str) -> Option<String> {
    let value = contents.split_once(&format!("\"{key}\""))?.1;
    let value = value.split_once(':')?.1.trim_start();
    if let Some(value) = value.strip_prefix('"') {
        return Some(value.split_once('"')?.0.to_string());
    }
    Some(
        value
            .split(|character: char| {
                character == ',' || character == '}' || character.is_whitespace()
            })
            .next()?
            .to_string(),
    )
}

fn git_dir(repository: &Path) -> Option<PathBuf> {
    let dot_git = repository.join(".git");
    if dot_git.is_dir() {
        return Some(dot_git);
    }
    let contents = fs::read_to_string(dot_git).ok()?;
    let path = contents.trim().strip_prefix("gitdir: ")?;
    let path = PathBuf::from(path);
    Some(if path.is_absolute() {
        path
    } else {
        repository.join(path)
    })
}

fn emit_hash(hash: String) {
    let hash = hash.trim();
    if !hash.is_empty() {
        println!("cargo:rustc-env=CARGO_JVM_GIT_HASH={hash}");
    }
}

fn emit_dirty() {
    println!("cargo:rustc-env=CARGO_JVM_GIT_DIRTY=1");
}
