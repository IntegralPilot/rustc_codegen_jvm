use directories::BaseDirs;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeSet, HashSet};
use std::env;
use std::error::Error;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io::{self, BufRead, BufReader, IsTerminal, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

type DynResult<T> = Result<T, Box<dyn Error>>;

const DEFAULT_STACK: &str = "16m";
const DEFAULT_REPOSITORY: &str = "https://github.com/IntegralPilot/rustc_codegen_jvm.git";
const UPDATE_CHECK_INTERVAL_SECONDS: u64 = 24 * 60 * 60;

#[derive(Clone, Debug, Deserialize)]
struct RustToolchainFile {
    toolchain: RustToolchain,
}

#[derive(Clone, Debug, Deserialize)]
struct RustToolchain {
    channel: String,
    #[serde(default)]
    components: Vec<String>,
    #[serde(default)]
    profile: Option<String>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
struct UserConfig {
    backend_path: PathBuf,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    last_update_check: Option<u64>,
}

#[derive(Clone, Debug)]
pub struct Backend {
    root: PathBuf,
    toolchain: RustToolchain,
    target_spec: PathBuf,
    cargo_config: PathBuf,
    overlay_script: PathBuf,
    build_script: PathBuf,
    linker: PathBuf,
    runtime_jar: PathBuf,
}

impl Backend {
    fn discover(root: impl AsRef<Path>) -> DynResult<Self> {
        let root = canonicalize_existing(root.as_ref(), "rustc_codegen_jvm directory")?;
        let executable_suffix = if cfg!(windows) { ".exe" } else { "" };
        let backend = Self {
            toolchain: load_toolchain(&root)?,
            target_spec: root.join("jvm-unknown-jvm.json"),
            cargo_config: root.join("config.toml"),
            overlay_script: root.join("stdlib_overlay.py"),
            build_script: root.join("build.py"),
            linker: root
                .join("java-linker")
                .join("target")
                .join("release")
                .join(format!("java-linker{executable_suffix}")),
            runtime_jar: root
                .join("runtime")
                .join("build")
                .join("libs")
                .join("runtime-0.1.0.jar"),
            root,
        };
        backend.validate()?;
        Ok(backend)
    }

    fn validate(&self) -> DynResult<()> {
        let required = [
            (&self.target_spec, "generated JVM target specification"),
            (&self.cargo_config, "generated Cargo configuration"),
            (&self.overlay_script, "standard-library overlay script"),
            (&self.build_script, "backend build script"),
            (&self.linker, "JVM linker"),
            (&self.runtime_jar, "JVM runtime JAR"),
        ];
        let missing = required
            .iter()
            .filter(|(path, _)| !path.is_file())
            .map(|(path, description)| format!("{description}: {}", path.display()))
            .collect::<Vec<_>>();
        if missing.is_empty() {
            Ok(())
        } else {
            Err(user_error(format!(
                "the configured rustc_codegen_jvm checkout is not built:\n  {}\nrerun `cargo jvm setup {}`",
                missing.join("\n  "),
                self.root.display()
            )))
        }
    }
}

#[derive(Clone, Debug)]
struct Artifact {
    package_id: String,
    target_name: String,
    kinds: Vec<String>,
    executable: Option<PathBuf>,
    linked_library: Option<PathBuf>,
    rlibs: Vec<PathBuf>,
    is_test: bool,
}

impl Artifact {
    fn is_binary(&self) -> bool {
        self.kinds.iter().any(|kind| kind == "bin")
    }

    fn is_example(&self) -> bool {
        self.kinds.iter().any(|kind| kind == "example")
    }

    fn is_executable(&self) -> bool {
        self.is_binary() || self.is_example()
    }

    fn is_cdylib(&self) -> bool {
        self.kinds.iter().any(|kind| kind == "cdylib")
    }

    fn is_library(&self) -> bool {
        (self
            .kinds
            .iter()
            .any(|kind| matches!(kind.as_str(), "lib" | "rlib"))
            && !self.rlibs.is_empty())
            || (self.is_cdylib() && self.linked_library.is_some())
    }
}

#[derive(Debug, Default)]
struct BuildOutput {
    artifacts: Vec<Artifact>,
    all_rlibs: BTreeSet<PathBuf>,
}

#[derive(Debug)]
struct Metadata {
    workspace_members: HashSet<String>,
    target_directory: PathBuf,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct JavaOptions {
    stack: String,
    java_args: Vec<OsString>,
    cargo_args: Vec<OsString>,
    program_args: Vec<OsString>,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct PackageOptions {
    output: Option<PathBuf>,
    cargo_args: Vec<OsString>,
}

#[derive(Debug, PartialEq, Eq)]
struct SetupOptions {
    path: Option<PathBuf>,
    repository: OsString,
    branch: Option<OsString>,
}

pub fn main_entry(arguments: impl IntoIterator<Item = OsString>) -> i32 {
    match run(arguments) {
        Ok(code) => code,
        Err(error) => {
            eprintln!("cargo-jvm: {error}");
            1
        }
    }
}

fn run(arguments: impl IntoIterator<Item = OsString>) -> DynResult<i32> {
    let mut arguments = arguments.into_iter().collect::<Vec<_>>();
    if !arguments.is_empty() {
        arguments.remove(0);
    }
    if arguments.first().is_some_and(|argument| argument == "jvm") {
        arguments.remove(0);
    }

    let (backend_override, mut arguments) = take_global_backend_override(arguments)?;
    let Some(command) = arguments.first().and_then(|value| value.to_str()) else {
        print_help();
        return Ok(0);
    };
    let command = command.to_string();
    arguments.remove(0);

    if matches!(command.as_str(), "-h" | "--help" | "help") {
        print_help();
        return Ok(0);
    }
    if matches!(command.as_str(), "-V" | "--version" | "version") {
        println!("cargo-jvm {}", env!("CARGO_PKG_VERSION"));
        return Ok(0);
    }
    if command == "setup" {
        if let Some(path) = backend_override {
            arguments.insert(0, path.into_os_string());
        }
        return setup(parse_setup_options(arguments)?).map(|()| 0);
    }

    let explicit_backend =
        backend_override.or_else(|| env::var_os("CARGO_JVM_BACKEND_PATH").map(PathBuf::from));
    if command == "update" {
        let (root, mut config) = configured_root(explicit_backend)?;
        update_backend(&root)?;
        if let Some(config) = &mut config {
            config.last_update_check = Some(unix_time());
            save_config(config)?;
        }
        return Ok(0);
    }
    let (backend, mut config) = configured_backend(explicit_backend)?;
    if command == "doctor" {
        print_doctor(&backend)?;
        check_for_updates(&backend, &mut config, true)?;
        return Ok(0);
    }
    check_for_updates(&backend, &mut config, false)?;

    match command.as_str() {
        "build" => run_build(&backend, arguments),
        "run" => run_binary(&backend, parse_java_options(arguments)?),
        "package" => run_package(&backend, parse_package_options(arguments)?),
        "test" => run_tests(&backend, parse_java_options(arguments)?),
        "clean" => run_clean(&backend, arguments),
        other => Err(user_error(format!(
            "unknown command `{other}`; run `cargo jvm help`"
        ))),
    }
}

fn print_help() {
    println!(
        "\
cargo-jvm — Cargo workflow for rustc_codegen_jvm

USAGE:
    cargo jvm <COMMAND> [OPTIONS]

COMMANDS:
    setup [PATH]        Clone or configure and build rustc_codegen_jvm
    update              Pull and rebuild the configured backend
    build [CARGO ARGS]  Build binaries or libraries for jvm-unknown-jvm
    run [CARGO ARGS] [-- PROGRAM ARGS]
                        Build and run one binary with Java
    test [CARGO ARGS] [-- TEST ARGS]
                        Build Rust tests and run their test JARs with Java
    package [OPTIONS] [CARGO ARGS]
                        Produce distributable binary or library JARs
    clean [CARGO ARGS]  Clean JVM target artifacts
    doctor              Validate Java, Cargo, rustc, and backend installation

SETUP:
    cargo jvm setup
    cargo jvm setup /path/to/rustc_codegen_jvm
    cargo jvm setup --path /path/to/rustc_codegen_jvm
    cargo jvm setup --path /where/to/clone --repository URL --branch NAME

SETUP OPTIONS:
    --path PATH         Existing checkout or clone destination
    --repository URL   Git repository to clone
    --branch NAME      Branch or tag to clone

RUN/TEST OPTIONS:
    --stack SIZE        JVM thread stack size (default: 16m)
    --java-arg ARG      Additional JVM option; repeat as needed

PACKAGE OPTIONS:
    -o, --output PATH   Output path; valid when exactly one artifact is selected

GLOBAL:
    --backend-path PATH Use a checkout without changing the saved setup

All unrecognized command arguments are forwarded to Cargo. Set
CARGO_JVM_STACK, CARGO_JVM_HOME, CARGO_JVM_BACKEND_PATH, CARGO_JVM_CARGO,
RUSTC, RUSTUP, JAVA, GIT, or PYTHON to override the corresponding defaults."
    );
}

fn take_global_backend_override(
    arguments: Vec<OsString>,
) -> DynResult<(Option<PathBuf>, Vec<OsString>)> {
    let mut backend = None;
    let mut retained = Vec::with_capacity(arguments.len());
    let mut iter = arguments.into_iter();
    while let Some(argument) = iter.next() {
        if argument == "--" {
            retained.push(argument);
            retained.extend(iter);
            break;
        }
        if argument == "--backend-path" {
            backend =
                Some(PathBuf::from(iter.next().ok_or_else(|| {
                    user_error("--backend-path requires a path")
                })?));
        } else if let Some(value) = argument
            .to_str()
            .and_then(|value| value.strip_prefix("--backend-path="))
        {
            backend = Some(PathBuf::from(value));
        } else {
            retained.push(argument);
        }
    }
    Ok((backend, retained))
}

fn setup(options: SetupOptions) -> DynResult<()> {
    let path = options.path.unwrap_or(default_install_path()?);
    let cloned = if path.exists() {
        if options.branch.is_some() || options.repository != DEFAULT_REPOSITORY {
            return Err(user_error(format!(
                "{} already exists; clone options can only be used with a new destination",
                path.display()
            )));
        }
        false
    } else {
        clone_checkout(&path, &options.repository, options.branch.as_deref())?;
        true
    };

    let root = canonicalize_existing(&path, "rustc_codegen_jvm directory")?;
    validate_checkout(&root)?;

    build_backend(&root)?;
    Backend::discover(&root)?;
    let config = UserConfig {
        backend_path: root,
        last_update_check: cloned.then(unix_time),
    };
    save_config(&config)?;
    println!(
        "Saved rustc_codegen_jvm path in {}",
        config_path()?.display()
    );
    println!("cargo-jvm is ready.");
    Ok(())
}

fn clone_checkout(destination: &Path, repository: &OsStr, branch: Option<&OsStr>) -> DynResult<()> {
    if destination.exists() {
        return Err(user_error(format!(
            "clone destination {} already exists",
            destination.display(),
        )));
    }
    if let Some(parent) = destination.parent() {
        fs::create_dir_all(parent)?;
    }

    println!(
        "Cloning rustc_codegen_jvm into {}...",
        destination.display()
    );
    let mut clone = Command::new(git_executable());
    clone.arg("clone");
    if let Some(branch) = branch {
        clone.arg("--branch").arg(branch);
    }
    let status = clone.arg(repository).arg(destination).status()?;
    require_success(status, "rustc_codegen_jvm clone")?;
    Ok(())
}

fn update_backend(root: &Path) -> DynResult<()> {
    validate_checkout(root)?;
    if !root.join(".git").exists() {
        return Err(user_error(format!(
            "{} is not a Git checkout; update it manually, then rerun `cargo jvm setup {}`",
            root.display(),
            root.display()
        )));
    }
    println!("Updating rustc_codegen_jvm in {}...", root.display());
    let status = git(root)
        .env("GIT_TERMINAL_PROMPT", "0")
        .args(["pull", "--ff-only"])
        .status()?;
    require_success(status, "rustc_codegen_jvm update")?;
    build_backend(root)?;
    Backend::discover(root)?;
    println!("rustc_codegen_jvm is up to date.");
    Ok(())
}

fn default_install_path() -> DynResult<PathBuf> {
    if let Some(path) = env::var_os("CARGO_JVM_HOME") {
        return Ok(PathBuf::from(path));
    }
    let base =
        BaseDirs::new().ok_or_else(|| user_error("cannot locate the user data directory"))?;
    Ok(base
        .data_local_dir()
        .join("cargo-jvm")
        .join("rustc_codegen_jvm"))
}

fn configured_root(explicit: Option<PathBuf>) -> DynResult<(PathBuf, Option<UserConfig>)> {
    if let Some(path) = explicit {
        let root = canonicalize_existing(&path, "rustc_codegen_jvm directory")?;
        validate_checkout(&root)?;
        return Ok((root, None));
    }
    match load_config()? {
        Some(config) => {
            let root = canonicalize_existing(&config.backend_path, "rustc_codegen_jvm directory")?;
            validate_checkout(&root)?;
            Ok((root, Some(config)))
        }
        None => {
            if !io::stdin().is_terminal() {
                return Err(user_error(
                    "cargo-jvm has not been set up; run `cargo jvm setup` to install the backend or pass an existing checkout path",
                ));
            }
            println!("cargo-jvm needs rustc_codegen_jvm and can clone and build it now.");
            print!("Continue with setup? [Y/n] ");
            io::stdout().flush()?;
            let mut answer = String::new();
            io::stdin().read_line(&mut answer)?;
            if !answer.trim().is_empty()
                && !matches!(answer.trim().to_ascii_lowercase().as_str(), "y" | "yes")
            {
                return Err(user_error("setup cancelled"));
            }
            setup(default_setup_options())?;
            let config = load_config()?.ok_or_else(|| user_error("setup did not save a config"))?;
            let root = canonicalize_existing(&config.backend_path, "rustc_codegen_jvm directory")?;
            Ok((root, Some(config)))
        }
    }
}

fn configured_backend(explicit: Option<PathBuf>) -> DynResult<(Backend, Option<UserConfig>)> {
    let (root, config) = configured_root(explicit)?;
    let backend = Backend::discover(root)?;
    ensure_toolchain(&backend.toolchain)?;
    Ok((backend, config))
}

fn validate_checkout(root: &Path) -> DynResult<()> {
    if root.join("build.py").is_file()
        && root.join("Cargo.toml").is_file()
        && root.join("rust-toolchain.toml").is_file()
    {
        Ok(())
    } else {
        Err(user_error(format!(
            "{} does not look like a rustc_codegen_jvm checkout",
            root.display()
        )))
    }
}

fn load_toolchain(root: &Path) -> DynResult<RustToolchain> {
    let path = root.join("rust-toolchain.toml");
    let contents = fs::read_to_string(&path).map_err(|error| {
        user_error(format!(
            "could not read the backend toolchain pin at {}: {error}",
            path.display()
        ))
    })?;
    let file: RustToolchainFile = toml::from_str(&contents)
        .map_err(|error| user_error(format!("invalid {}: {error}", path.display())))?;
    if !is_pinned_nightly(&file.toolchain.channel) {
        return Err(user_error(format!(
            "{} must pin a dated nightly such as `nightly-2026-08-08`; found `{}`",
            path.display(),
            file.toolchain.channel
        )));
    }
    Ok(file.toolchain)
}

fn is_pinned_nightly(channel: &str) -> bool {
    let Some(date) = channel.strip_prefix("nightly-") else {
        return false;
    };
    date.len() == 10
        && date.bytes().enumerate().all(|(index, byte)| match index {
            4 | 7 => byte == b'-',
            _ => byte.is_ascii_digit(),
        })
}

fn ensure_toolchain(toolchain: &RustToolchain) -> DynResult<()> {
    let rustup = rustup_executable();
    let installed = Command::new(&rustup)
        .args(["component", "list", "--toolchain"])
        .arg(&toolchain.channel)
        .args(["--installed", "--quiet"])
        .output()
        .map_err(|error| {
            user_error(format!(
                "rustup is required to install {}: {error}",
                toolchain.channel
            ))
        })?;

    if !installed.status.success() {
        println!(
            "Installing required Rust toolchain {}...",
            toolchain.channel
        );
        let mut command = Command::new(&rustup);
        command
            .args(["toolchain", "install"])
            .arg(&toolchain.channel)
            .arg("--profile")
            .arg(toolchain.profile.as_deref().unwrap_or("minimal"));
        if !toolchain.components.is_empty() {
            command
                .arg("--component")
                .arg(toolchain.components.join(","));
        }
        return require_success(command.status()?, "Rust toolchain installation");
    }

    let installed = String::from_utf8(installed.stdout)?;
    let missing = toolchain
        .components
        .iter()
        .filter(|component| !component_is_installed(component, &installed))
        .collect::<Vec<_>>();
    if missing.is_empty() {
        return Ok(());
    }

    println!(
        "Installing missing {} component(s): {}...",
        toolchain.channel,
        missing
            .iter()
            .map(|component| component.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    );
    let mut command = Command::new(&rustup);
    command
        .args(["component", "add", "--toolchain"])
        .arg(&toolchain.channel)
        .args(missing);
    require_success(command.status()?, "Rust component installation")
}

fn component_is_installed(component: &str, installed: &str) -> bool {
    let component = component.strip_suffix("-preview").unwrap_or(component);
    installed.lines().any(|installed| {
        installed == component
            || installed
                .strip_prefix(component)
                .is_some_and(|suffix| suffix.starts_with('-'))
    })
}

fn build_backend(root: &Path) -> DynResult<()> {
    validate_checkout(root)?;
    let toolchain = load_toolchain(root)?;
    ensure_toolchain(&toolchain)?;
    println!("Building rustc_codegen_jvm in {}...", root.display());
    let status = python_command(root)?
        .arg(root.join("build.py"))
        .arg("all")
        .env("RUSTUP_TOOLCHAIN", &toolchain.channel)
        .current_dir(root)
        .status()?;
    require_success(status, "rustc_codegen_jvm build")?;
    println!("rustc_codegen_jvm is built and ready.");
    Ok(())
}

fn config_path() -> DynResult<PathBuf> {
    if let Some(path) = env::var_os("CARGO_JVM_CONFIG") {
        return Ok(PathBuf::from(path));
    }
    let base =
        BaseDirs::new().ok_or_else(|| user_error("cannot locate the user config directory"))?;
    Ok(base.config_dir().join("cargo-jvm").join("config.toml"))
}

fn load_config() -> DynResult<Option<UserConfig>> {
    let path = config_path()?;
    if !path.exists() {
        return Ok(None);
    }
    let contents = fs::read_to_string(&path)?;
    let config = toml::from_str(&contents)
        .map_err(|error| user_error(format!("invalid {}: {error}", path.display())))?;
    Ok(Some(config))
}

fn save_config(config: &UserConfig) -> DynResult<()> {
    let path = config_path()?;
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(path, toml::to_string_pretty(config)?)?;
    Ok(())
}

fn check_for_updates(
    backend: &Backend,
    config: &mut Option<UserConfig>,
    force: bool,
) -> DynResult<()> {
    let Some(config) = config else {
        return Ok(());
    };
    let now = unix_time();
    if !force
        && config
            .last_update_check
            .is_some_and(|checked| now.saturating_sub(checked) < UPDATE_CHECK_INTERVAL_SECONDS)
    {
        return Ok(());
    }
    config.last_update_check = Some(now);
    save_config(config)?;

    if !backend.root.join(".git").exists() {
        return Ok(());
    }
    let fetch = git(&backend.root)
        .env("GIT_TERMINAL_PROMPT", "0")
        .args(["fetch", "--quiet", "--no-tags"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();
    if !matches!(fetch, Ok(status) if status.success()) {
        eprintln!("cargo-jvm: could not check the backend remote for updates");
        return Ok(());
    }
    let behind = git_output(&backend.root, &["rev-list", "--count", "HEAD..@{upstream}"]);
    if let Ok(count) = behind
        && count.trim().parse::<u64>().unwrap_or(0) > 0
    {
        eprintln!(
            "cargo-jvm: a newer rustc_codegen_jvm commit is available ({count} commit(s)); run `cargo jvm update` (or `git -C \"{}\" pull` followed by `cargo jvm setup \"{}\"`)",
            backend.root.display(),
            backend.root.display(),
        );
    } else if force {
        println!("rustc_codegen_jvm is up to date with its configured upstream.");
    }
    Ok(())
}

fn run_build(backend: &Backend, cargo_args: Vec<OsString>) -> DynResult<i32> {
    let status = cargo_command(backend, "build", &cargo_args, false)?.status()?;
    Ok(status.code().unwrap_or(1))
}

fn run_clean(backend: &Backend, cargo_args: Vec<OsString>) -> DynResult<i32> {
    let mut command = pinned_cargo_command(&backend.toolchain);
    command
        .arg("clean")
        .arg("--target")
        .arg(&backend.target_spec)
        .arg("--config")
        .arg(&backend.cargo_config)
        .env("RUSTUP_TOOLCHAIN", &backend.toolchain.channel)
        .args(cargo_args);
    Ok(command.status()?.code().unwrap_or(1))
}

fn run_binary(backend: &Backend, options: JavaOptions) -> DynResult<i32> {
    let build = capture_cargo(backend, "build", &options.cargo_args)?;
    let mut binaries = build
        .artifacts
        .into_iter()
        .filter(|artifact| artifact.is_executable() && artifact.executable.is_some())
        .collect::<Vec<_>>();
    if let Some(selected) = selected_value(&options.cargo_args, "--bin") {
        binaries.retain(|artifact| artifact.target_name == selected);
    } else if let Some(selected) = selected_value(&options.cargo_args, "--example") {
        binaries.retain(|artifact| artifact.target_name == selected);
    }
    let artifact = exactly_one(
        binaries,
        "runnable binary or example",
        "select one with Cargo's `--bin <name>` or `--example <name>` option",
    )?;
    let jar = artifact.executable.expect("filtered executable");
    run_java(&jar, &options, false)
}

fn run_tests(backend: &Backend, mut options: JavaOptions) -> DynResult<i32> {
    if !options
        .cargo_args
        .iter()
        .any(|argument| argument == "--no-run")
    {
        options.cargo_args.push(OsString::from("--no-run"));
    }
    let build = capture_cargo(backend, "test", &options.cargo_args)?;
    let tests = build
        .artifacts
        .into_iter()
        .filter(|artifact| artifact.is_test && artifact.executable.is_some())
        .collect::<Vec<_>>();
    if tests.is_empty() {
        return Err(user_error("Cargo did not produce any JVM test executables"));
    }
    let mut exit_code = 0;
    for test in tests {
        let jar = test.executable.expect("filtered executable");
        println!("Running {} ({})", test.target_name, jar.display());
        let code = run_java(&jar, &options, true)?;
        if code != 0 {
            exit_code = code;
        }
    }
    Ok(exit_code)
}

fn run_package(backend: &Backend, options: PackageOptions) -> DynResult<i32> {
    let metadata = cargo_metadata(backend, &options.cargo_args)?;
    let build = capture_cargo(backend, "build", &options.cargo_args)?;
    let mut selected = build
        .artifacts
        .iter()
        .filter(|artifact| metadata.workspace_members.contains(&artifact.package_id))
        .filter(|artifact| package_target_selected(artifact, &options.cargo_args))
        .filter(|artifact| {
            (artifact.is_executable() && artifact.executable.is_some()) || artifact.is_library()
        })
        .collect::<Vec<_>>();
    selected.sort_by(|left, right| {
        left.target_name
            .cmp(&right.target_name)
            .then_with(|| left.kinds.cmp(&right.kinds))
    });
    selected.dedup_by(|left, right| {
        left.target_name == right.target_name
            && left.executable == right.executable
            && left.linked_library == right.linked_library
            && left.rlibs == right.rlibs
    });
    if selected.is_empty() {
        return Err(user_error(
            "Cargo did not produce a packageable workspace binary or library",
        ));
    }
    if options.output.is_some() && selected.len() != 1 {
        return Err(user_error(
            "--output can only be used when Cargo selects exactly one binary or library",
        ));
    }

    let profile = cargo_profile(&options.cargo_args);
    let default_output = metadata.target_directory.join("jvm-package").join(profile);
    fs::create_dir_all(&default_output)?;

    let has_binary_names = selected
        .iter()
        .filter(|artifact| artifact.is_executable())
        .map(|artifact| artifact.target_name.as_str())
        .collect::<HashSet<_>>();
    for artifact in selected {
        let file_name =
            if artifact.is_library() && has_binary_names.contains(artifact.target_name.as_str()) {
                format!("{}-lib.jar", artifact.target_name)
            } else {
                format!("{}.jar", artifact.target_name)
            };
        let output = options
            .output
            .clone()
            .unwrap_or_else(|| default_output.join(file_name));
        if let Some(parent) = output.parent() {
            fs::create_dir_all(parent)?;
        }

        if let Some(jar) = artifact
            .executable
            .as_ref()
            .filter(|_| artifact.is_executable())
            .or(artifact.linked_library.as_ref())
        {
            let same_file = jar == &output
                || matches!(
                    (fs::canonicalize(jar), fs::canonicalize(&output)),
                    (Ok(source), Ok(destination)) if source == destination
                );
            if !same_file {
                fs::copy(jar, &output)?;
            }
        } else {
            package_library(backend, &build.all_rlibs, &output)?;
        }
        println!("Packaged {}", output.display());
    }
    Ok(0)
}

fn package_target_selected(artifact: &Artifact, cargo_args: &[OsString]) -> bool {
    let binary = selected_value(cargo_args, "--bin");
    let example = selected_value(cargo_args, "--example");
    let libraries = cargo_args.iter().any(|argument| argument == "--lib");
    let binaries = cargo_args.iter().any(|argument| argument == "--bins");
    let examples = cargo_args.iter().any(|argument| argument == "--examples");
    let explicitly_selected =
        binary.is_some() || example.is_some() || libraries || binaries || examples;
    if !explicitly_selected {
        return true;
    }
    (libraries && artifact.is_library())
        || (binaries && artifact.is_binary())
        || (examples && artifact.is_example())
        || (artifact.is_binary() && binary.as_deref() == Some(artifact.target_name.as_str()))
        || (artifact.is_example() && example.as_deref() == Some(artifact.target_name.as_str()))
}

fn package_library(backend: &Backend, rlibs: &BTreeSet<PathBuf>, output: &Path) -> DynResult<()> {
    if rlibs.is_empty() {
        return Err(user_error("Cargo did not report any library archives"));
    }
    let has_panic_unwind = rlibs.iter().any(|path| rlib_crate_name(path) == Some("panic_unwind"));
    let selected_rlibs = rlibs.iter().filter(|path| {
        // `-Zbuild-std=std,panic_unwind` may still report panic_abort as a
        // compiler artifact. A native linker selects one panic runtime; a JAR
        // merge must make that selection explicitly to avoid duplicate global
        // symbols being won by panic_abort's archive ordering.
        !(has_panic_unwind && rlib_crate_name(path) == Some("panic_abort"))
    });
    let status = Command::new(&backend.linker)
        .args(selected_rlibs)
        .arg(&backend.runtime_jar)
        .arg("-o")
        .arg(output)
        .status()?;
    require_success(status, "library JAR packaging")
}

fn rlib_crate_name(path: &Path) -> Option<&str> {
    let name = path.file_stem()?.to_str()?.strip_prefix("lib")?;
    Some(name.split_once('-').map_or(name, |(crate_name, _)| crate_name))
}

fn cargo_command(
    backend: &Backend,
    subcommand: &str,
    cargo_args: &[OsString],
    capture: bool,
) -> DynResult<Command> {
    let overlay = prepare_overlay(backend)?;
    let mut command = pinned_cargo_command(&backend.toolchain);
    command
        .arg(subcommand)
        .arg("--target")
        .arg(&backend.target_spec)
        .arg("-Zjson-target-spec")
        .arg("-Zbuild-std=std,panic_unwind")
        .arg("-Zbuild-std-features=panic-unwind")
        .arg("--config")
        .arg(&backend.cargo_config)
        .env("RUSTUP_TOOLCHAIN", &backend.toolchain.channel)
        .env("__CARGO_TESTS_ONLY_SRC_ROOT", overlay);
    if capture {
        command.arg("--message-format=json-render-diagnostics");
    }
    command.args(strip_message_format(cargo_args));
    Ok(command)
}

fn capture_cargo(
    backend: &Backend,
    subcommand: &str,
    cargo_args: &[OsString],
) -> DynResult<BuildOutput> {
    let mut child = cargo_command(backend, subcommand, cargo_args, true)?
        .stdout(Stdio::piped())
        .spawn()?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| user_error("failed to capture Cargo output"))?;
    let mut output = BuildOutput::default();
    for line in BufReader::new(stdout).lines() {
        let line = line?;
        let Ok(message) = serde_json::from_str::<Value>(&line) else {
            println!("{line}");
            continue;
        };
        match message.get("reason").and_then(Value::as_str) {
            Some("compiler-message") => {
                if let Some(rendered) = message.pointer("/message/rendered").and_then(Value::as_str)
                {
                    eprint!("{rendered}");
                }
            }
            Some("compiler-artifact") => {
                if let Some(artifact) = parse_artifact(&message) {
                    output.all_rlibs.extend(artifact.rlibs.iter().cloned());
                    output.artifacts.push(artifact);
                }
            }
            Some("build-finished") | Some("build-script-executed") => {}
            _ => {}
        }
    }
    let status = child.wait()?;
    require_success(status, &format!("Cargo {subcommand}"))?;
    Ok(output)
}

fn parse_artifact(message: &Value) -> Option<Artifact> {
    let package_id = message.get("package_id")?.as_str()?.to_string();
    let target = message.get("target")?;
    let target_name = target.get("name")?.as_str()?.to_string();
    let kinds = target
        .get("kind")?
        .as_array()?
        .iter()
        .filter_map(Value::as_str)
        .map(str::to_string)
        .collect::<Vec<_>>();
    let executable = message
        .get("executable")
        .and_then(Value::as_str)
        .map(PathBuf::from)
        .filter(|path| path.extension().is_some_and(|extension| extension == "jar"));
    let linked_library = if kinds.iter().any(|kind| kind == "cdylib") {
        message
            .get("filenames")?
            .as_array()?
            .iter()
            .filter_map(Value::as_str)
            .map(PathBuf::from)
            .find(|path| path.extension().is_some_and(|extension| extension == "jar"))
    } else {
        None
    };
    let rlibs = message
        .get("filenames")?
        .as_array()?
        .iter()
        .filter_map(Value::as_str)
        .map(PathBuf::from)
        .filter(|path| {
            path.extension()
                .is_some_and(|extension| extension == "rlib")
        })
        .collect();
    let is_test = message
        .pointer("/profile/test")
        .and_then(Value::as_bool)
        .unwrap_or(false);
    Some(Artifact {
        package_id,
        target_name,
        kinds,
        executable,
        linked_library,
        rlibs,
        is_test,
    })
}

fn cargo_metadata(backend: &Backend, cargo_args: &[OsString]) -> DynResult<Metadata> {
    let mut command = pinned_cargo_command(&backend.toolchain);
    command
        .args(["metadata", "--format-version", "1", "--no-deps"])
        .env("RUSTUP_TOOLCHAIN", &backend.toolchain.channel);
    if let Some(manifest) = selected_value_os(cargo_args, "--manifest-path") {
        command.arg("--manifest-path").arg(manifest);
    }
    let output = command.output()?;
    if !output.status.success() {
        return Err(user_error(format!(
            "cargo metadata failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        )));
    }
    let metadata: Value = serde_json::from_slice(&output.stdout)?;
    let workspace_members = metadata["workspace_members"]
        .as_array()
        .into_iter()
        .flatten()
        .filter_map(Value::as_str)
        .map(str::to_string)
        .collect();
    let target_directory = PathBuf::from(
        metadata["target_directory"]
            .as_str()
            .ok_or_else(|| user_error("cargo metadata omitted target_directory"))?,
    );
    Ok(Metadata {
        workspace_members,
        target_directory,
    })
}

fn parse_java_options(arguments: Vec<OsString>) -> DynResult<JavaOptions> {
    let mut options = JavaOptions {
        stack: env::var("CARGO_JVM_STACK").unwrap_or_else(|_| DEFAULT_STACK.to_string()),
        ..JavaOptions::default()
    };
    let separator = arguments.iter().position(|argument| argument == "--");
    let (cargo_side, program_side) = separator.map_or_else(
        || (arguments.as_slice(), &[][..]),
        |index| (&arguments[..index], &arguments[index + 1..]),
    );
    options.program_args.extend_from_slice(program_side);

    let mut iter = cargo_side.iter();
    while let Some(argument) = iter.next() {
        if argument == "--stack" {
            options.stack = required_utf8(iter.next(), "--stack")?.to_string();
        } else if let Some(value) = argument
            .to_str()
            .and_then(|value| value.strip_prefix("--stack="))
        {
            options.stack = value.to_string();
        } else if argument == "--java-arg" {
            options.java_args.push(
                iter.next()
                    .ok_or_else(|| user_error("--java-arg requires a value"))?
                    .clone(),
            );
        } else if let Some(value) = argument
            .to_str()
            .and_then(|value| value.strip_prefix("--java-arg="))
        {
            options.java_args.push(OsString::from(value));
        } else {
            options.cargo_args.push(argument.clone());
        }
    }
    validate_stack(&options.stack)?;
    Ok(options)
}

fn parse_package_options(arguments: Vec<OsString>) -> DynResult<PackageOptions> {
    let mut options = PackageOptions::default();
    let mut iter = arguments.into_iter();
    while let Some(argument) = iter.next() {
        if matches!(argument.to_str(), Some("-o" | "--output")) {
            options.output = Some(PathBuf::from(
                iter.next()
                    .ok_or_else(|| user_error("--output requires a path"))?,
            ));
        } else if let Some(value) = argument
            .to_str()
            .and_then(|value| value.strip_prefix("--output="))
        {
            options.output = Some(PathBuf::from(value));
        } else {
            options.cargo_args.push(argument);
        }
    }
    Ok(options)
}

fn default_setup_options() -> SetupOptions {
    SetupOptions {
        path: None,
        repository: OsString::from(DEFAULT_REPOSITORY),
        branch: None,
    }
}

fn parse_setup_options(arguments: Vec<OsString>) -> DynResult<SetupOptions> {
    let mut options = default_setup_options();
    let mut iter = arguments.into_iter();
    while let Some(argument) = iter.next() {
        if matches!(argument.to_str(), Some("-p" | "--path")) {
            let path = PathBuf::from(
                iter.next()
                    .ok_or_else(|| user_error("--path requires a destination"))?,
            );
            if options.path.replace(path).is_some() {
                return Err(user_error("setup accepts only one backend path"));
            }
        } else if argument == "--repository" {
            options.repository = iter
                .next()
                .ok_or_else(|| user_error("--repository requires a URL or path"))?;
        } else if argument == "--branch" {
            options.branch = Some(
                iter.next()
                    .ok_or_else(|| user_error("--branch requires a name"))?,
            );
        } else if let Some(value) = argument
            .to_str()
            .and_then(|value| value.strip_prefix("--path="))
        {
            if options.path.replace(PathBuf::from(value)).is_some() {
                return Err(user_error("setup accepts only one backend path"));
            }
        } else if let Some(value) = argument
            .to_str()
            .and_then(|value| value.strip_prefix("--repository="))
        {
            options.repository = OsString::from(value);
        } else if let Some(value) = argument
            .to_str()
            .and_then(|value| value.strip_prefix("--branch="))
        {
            options.branch = Some(OsString::from(value));
        } else if argument.to_string_lossy().starts_with('-') {
            return Err(user_error(format!(
                "unknown setup option `{}`",
                argument.display()
            )));
        } else if options.path.replace(PathBuf::from(&argument)).is_some() {
            return Err(user_error("setup accepts only one backend path"));
        }
    }
    Ok(options)
}

fn run_java(jar: &Path, options: &JavaOptions, test: bool) -> DynResult<i32> {
    let mut command = Command::new(env::var_os("JAVA").unwrap_or_else(|| OsString::from("java")));
    command
        .arg(format!("-Xss{}", options.stack))
        .args(&options.java_args)
        .arg("-jar")
        .arg(jar)
        .args(&options.program_args);
    if test {
        command.env(
            "RUST_BACKTRACE",
            env::var_os("RUST_BACKTRACE").unwrap_or_default(),
        );
    }
    Ok(command.status()?.code().unwrap_or(1))
}

fn prepare_overlay(backend: &Backend) -> DynResult<PathBuf> {
    if let Some(path) = env::var_os("__CARGO_TESTS_ONLY_SRC_ROOT") {
        let path = PathBuf::from(path);
        if path.is_dir() {
            return Ok(path);
        }
    }
    let output = python_command(&backend.root)?
        .arg(&backend.overlay_script)
        .env("RUSTUP_TOOLCHAIN", &backend.toolchain.channel)
        .current_dir(&backend.root)
        .output()?;
    if !output.status.success() {
        return Err(user_error(format!(
            "standard-library overlay preparation failed:\n{}",
            String::from_utf8_lossy(&output.stderr)
        )));
    }
    let path = String::from_utf8(output.stdout)?.trim().to_string();
    if path.is_empty() {
        return Err(user_error(
            "standard-library overlay script returned an empty path",
        ));
    }
    Ok(PathBuf::from(path))
}

fn python_command(_backend_root: &Path) -> DynResult<Command> {
    let candidates = env::var_os("PYTHON")
        .into_iter()
        .chain(env::var_os("CARGO_JVM_PYTHON"))
        .chain([OsString::from("python3"), OsString::from("python")]);
    for candidate in candidates {
        if Command::new(&candidate)
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|status| status.success())
        {
            return Ok(Command::new(candidate));
        }
    }
    Err(user_error(
        "Python 3 is required to prepare the Rust standard-library overlay",
    ))
}

fn pinned_cargo_command(toolchain: &RustToolchain) -> Command {
    let mut command = Command::new(rustup_executable());
    command
        .arg("run")
        .arg(&toolchain.channel)
        .arg(env::var_os("CARGO_JVM_CARGO").unwrap_or_else(|| OsString::from("cargo")));
    command
}

fn rustup_executable() -> OsString {
    env::var_os("RUSTUP").unwrap_or_else(|| OsString::from("rustup"))
}

fn git_executable() -> OsString {
    env::var_os("GIT").unwrap_or_else(|| OsString::from("git"))
}

fn git(root: &Path) -> Command {
    let mut command = Command::new(git_executable());
    command.arg("-C").arg(root);
    command
}

fn git_output(root: &Path, arguments: &[&str]) -> DynResult<String> {
    let output = git(root).args(arguments).output()?;
    if !output.status.success() {
        return Err(user_error(String::from_utf8_lossy(&output.stderr)));
    }
    Ok(String::from_utf8(output.stdout)?)
}

fn print_doctor(backend: &Backend) -> DynResult<()> {
    print!("cargo-jvm: {}", env!("CARGO_PKG_VERSION"));
    if let Some(commit) = option_env!("CARGO_JVM_GIT_HASH") {
        print!(" (commit {commit})");
    }
    if option_env!("CARGO_JVM_GIT_DIRTY").is_some() {
        print!(" (dirty)");
    }
    println!();
    println!("rustc_codegen_jvm: {}", backend.root.display());
    println!("pinned Rust toolchain: {}", backend.toolchain.channel);
    match git_output(&backend.root, &["log", "-1", "--format=%H%x09%s"]) {
        Ok(commit) => println!("rustc_codegen_jvm HEAD: {}", commit.trim()),
        Err(_) => println!("rustc_codegen_jvm HEAD: unavailable (not a Git checkout)"),
    }
    let cargo_version = pinned_cargo_command(&backend.toolchain)
        .arg("--version")
        .output()?;
    if !cargo_version.status.success() {
        return Err(user_error("cargo is not working"));
    }
    println!(
        "cargo: {}",
        String::from_utf8_lossy(&cargo_version.stdout).trim()
    );
    for (name, executable, version_argument, pinned) in [
        (
            "rustc",
            env::var_os("RUSTC").unwrap_or_else(|| OsString::from("rustc")),
            "--version",
            true,
        ),
        (
            "java",
            env::var_os("JAVA").unwrap_or_else(|| OsString::from("java")),
            "-version",
            false,
        ),
    ] {
        let mut command = Command::new(&executable);
        command.arg(version_argument);
        if pinned {
            command.env("RUSTUP_TOOLCHAIN", &backend.toolchain.channel);
        }
        let output = command.output()?;
        if !output.status.success() {
            return Err(user_error(format!("{name} is not working")));
        }
        let version = if output.stdout.is_empty() {
            &output.stderr
        } else {
            &output.stdout
        };
        println!("{name}: {}", String::from_utf8_lossy(version).trim());
    }
    println!("target: {}", backend.target_spec.display());
    println!("linker: {}", backend.linker.display());
    println!("runtime: {}", backend.runtime_jar.display());
    Ok(())
}

fn selected_value(arguments: &[OsString], option: &str) -> Option<String> {
    selected_value_os(arguments, option).and_then(|value| value.to_str().map(str::to_string))
}

fn selected_value_os<'a>(arguments: &'a [OsString], option: &str) -> Option<&'a OsStr> {
    for (index, argument) in arguments.iter().enumerate() {
        if argument == option {
            return arguments.get(index + 1).map(OsString::as_os_str);
        }
        if let Some(value) = argument
            .to_str()
            .and_then(|argument| argument.strip_prefix(&format!("{option}=")))
        {
            return Some(OsStr::new(value));
        }
    }
    None
}

fn cargo_profile(arguments: &[OsString]) -> String {
    if let Some(profile) = selected_value(arguments, "--profile") {
        profile
    } else if arguments.iter().any(|argument| argument == "--release") {
        "release".to_string()
    } else {
        "debug".to_string()
    }
}

fn strip_message_format(arguments: &[OsString]) -> Vec<&OsStr> {
    let mut retained = Vec::new();
    let mut skip_next = false;
    for argument in arguments {
        if skip_next {
            skip_next = false;
            continue;
        }
        if argument == "--message-format" {
            skip_next = true;
        } else if !argument
            .to_str()
            .is_some_and(|value| value.starts_with("--message-format="))
        {
            retained.push(argument.as_os_str());
        }
    }
    retained
}

fn exactly_one<T>(mut values: Vec<T>, description: &str, suggestion: &str) -> DynResult<T> {
    match values.len() {
        1 => Ok(values.pop().expect("one value")),
        0 => Err(user_error(format!("Cargo produced no {description}"))),
        count => Err(user_error(format!(
            "Cargo produced {count} {description}s; {suggestion}"
        ))),
    }
}

fn required_utf8<'a>(value: Option<&'a OsString>, option: &str) -> DynResult<&'a str> {
    value
        .ok_or_else(|| user_error(format!("{option} requires a value")))?
        .to_str()
        .ok_or_else(|| user_error(format!("{option} must be valid UTF-8")))
}

fn validate_stack(stack: &str) -> DynResult<()> {
    let valid_suffix = stack
        .ends_with(|character: char| matches!(character.to_ascii_lowercase(), 'k' | 'm' | 'g'));
    let digits = if valid_suffix {
        &stack[..stack.len() - 1]
    } else {
        stack
    };
    if digits.is_empty() || !digits.chars().all(|character| character.is_ascii_digit()) {
        return Err(user_error(format!(
            "invalid JVM stack size `{stack}`; use values such as 4m or 16m"
        )));
    }
    Ok(())
}

fn canonicalize_existing(path: &Path, description: &str) -> DynResult<PathBuf> {
    path.canonicalize().map_err(|error| {
        user_error(format!(
            "cannot open {description} {}: {error}",
            path.display()
        ))
    })
}

fn require_success(status: ExitStatus, operation: &str) -> DynResult<()> {
    if status.success() {
        Ok(())
    } else {
        Err(user_error(format!(
            "{operation} failed with status {status}"
        )))
    }
}

fn user_error(message: impl Into<String>) -> Box<dyn Error> {
    Box::new(io::Error::other(message.into()))
}

fn unix_time() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

trait OsStringDisplay {
    fn display(&self) -> String;
}

impl OsStringDisplay for OsString {
    fn display(&self) -> String {
        self.to_string_lossy().into_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn os(values: &[&str]) -> Vec<OsString> {
        values.iter().map(OsString::from).collect()
    }

    #[test]
    fn cargo_subcommand_prefix_is_optional() {
        let (backend, rest) = take_global_backend_override(os(&[
            "build",
            "--backend-path",
            "/tmp/backend",
            "--release",
        ]))
        .unwrap();
        assert_eq!(backend, Some(PathBuf::from("/tmp/backend")));
        assert_eq!(rest, os(&["build", "--release"]));
    }

    #[test]
    fn backend_override_does_not_consume_program_arguments() {
        let (backend, rest) =
            take_global_backend_override(os(&["run", "--", "--backend-path", "program-value"]))
                .unwrap();
        assert_eq!(backend, None);
        assert_eq!(rest, os(&["run", "--", "--backend-path", "program-value"]));
    }

    #[test]
    fn parses_java_and_program_arguments_without_hiding_cargo_arguments() {
        assert_eq!(
            parse_java_options(os(&[
                "--release",
                "--stack",
                "8m",
                "--java-arg=-ea",
                "--features",
                "demo",
                "--",
                "hello",
                "--flag",
            ]))
            .unwrap(),
            JavaOptions {
                stack: "8m".to_string(),
                java_args: os(&["-ea"]),
                cargo_args: os(&["--release", "--features", "demo"]),
                program_args: os(&["hello", "--flag"]),
            }
        );
    }

    #[test]
    fn parses_package_output_without_consuming_cargo_arguments() {
        assert_eq!(
            parse_package_options(os(&[
                "--release",
                "--output",
                "dist/demo.jar",
                "--features=demo",
            ]))
            .unwrap(),
            PackageOptions {
                output: Some(PathBuf::from("dist/demo.jar")),
                cargo_args: os(&["--release", "--features=demo"]),
            }
        );
    }

    #[test]
    fn parses_setup_path_repository_and_branch() {
        assert_eq!(
            parse_setup_options(os(&[
                "--path",
                "/tmp/backend",
                "--repository=local-repository",
                "--branch",
                "next",
            ]))
            .unwrap(),
            SetupOptions {
                path: Some(PathBuf::from("/tmp/backend")),
                repository: OsString::from("local-repository"),
                branch: Some(OsString::from("next")),
            }
        );
    }

    #[test]
    fn validates_stack_sizes() {
        for valid in ["1024", "512k", "8m", "1G"] {
            validate_stack(valid).unwrap();
        }
        for invalid in ["", "m", "-1m", "eight"] {
            assert!(validate_stack(invalid).is_err(), "{invalid}");
        }
    }

    #[test]
    fn requires_a_dated_nightly_pin() {
        assert!(is_pinned_nightly("nightly-2026-08-08"));
        for unpinned in [
            "nightly",
            "stable",
            "nightly-2026-8-8",
            "nightly-2026-08-08-x86_64-unknown-linux-gnu",
        ] {
            assert!(!is_pinned_nightly(unpinned), "{unpinned}");
        }
    }

    #[test]
    fn recognizes_rustup_component_names_with_host_suffixes() {
        let installed =
            "rustc-dev-x86_64-unknown-linux-gnu\nrust-src\nllvm-tools-x86_64-unknown-linux-gnu\n";
        assert!(component_is_installed("rustc-dev", installed));
        assert!(component_is_installed("rust-src", installed));
        assert!(component_is_installed("llvm-tools-preview", installed));
        assert!(!component_is_installed("rustfmt", installed));
    }

    #[test]
    fn removes_user_message_format_for_internal_artifact_capture() {
        assert_eq!(
            strip_message_format(&os(&[
                "--release",
                "--message-format",
                "short",
                "--message-format=json",
                "--features",
                "demo",
            ])),
            vec![
                OsStr::new("--release"),
                OsStr::new("--features"),
                OsStr::new("demo"),
            ]
        );
    }

    #[test]
    fn profile_follows_cargo_options() {
        assert_eq!(cargo_profile(&os(&[])), "debug");
        assert_eq!(cargo_profile(&os(&["--release"])), "release");
        assert_eq!(
            cargo_profile(&os(&["--profile", "distribution"])),
            "distribution"
        );
    }

    #[test]
    fn parses_compiler_artifacts() {
        let value = serde_json::json!({
            "reason": "compiler-artifact",
            "package_id": "path+file:///demo#0.1.0",
            "target": {"name": "demo", "kind": ["lib"]},
            "profile": {"test": false},
            "filenames": ["/tmp/libdemo.rlib", "/tmp/libdemo.rmeta"],
            "executable": null
        });
        let artifact = parse_artifact(&value).unwrap();
        assert!(artifact.is_library());
        assert_eq!(artifact.rlibs, [PathBuf::from("/tmp/libdemo.rlib")]);
        assert!(artifact.linked_library.is_none());
    }

    #[test]
    fn parses_linked_cdylib_artifacts() {
        let value = serde_json::json!({
            "reason": "compiler-artifact",
            "package_id": "path+file:///demo#0.1.0",
            "target": {"name": "demo", "kind": ["cdylib"]},
            "profile": {"test": false},
            "filenames": ["/tmp/demo.jar"],
            "executable": null
        });
        let artifact = parse_artifact(&value).unwrap();
        assert!(artifact.is_cdylib());
        assert!(artifact.is_library());
        assert_eq!(
            artifact.linked_library,
            Some(PathBuf::from("/tmp/demo.jar"))
        );
    }

    #[test]
    fn extracts_crate_names_from_rlib_artifacts() {
        assert_eq!(
            rlib_crate_name(Path::new("/tmp/libpanic_unwind-35f4b19ff65eda2b.rlib")),
            Some("panic_unwind")
        );
        assert_eq!(rlib_crate_name(Path::new("/tmp/libdemo.rlib")), Some("demo"));
        assert_eq!(rlib_crate_name(Path::new("/tmp/demo.jar")), None);
    }

    #[test]
    fn package_target_flags_exclude_implicit_sibling_targets() {
        let binary = Artifact {
            package_id: "demo".to_string(),
            target_name: "demo".to_string(),
            kinds: vec!["bin".to_string()],
            executable: Some(PathBuf::from("demo.jar")),
            linked_library: None,
            rlibs: Vec::new(),
            is_test: false,
        };
        let library = Artifact {
            package_id: "demo".to_string(),
            target_name: "demo".to_string(),
            kinds: vec!["lib".to_string()],
            executable: None,
            linked_library: None,
            rlibs: vec![PathBuf::from("libdemo.rlib")],
            is_test: false,
        };
        assert!(package_target_selected(&binary, &os(&["--bin", "demo"])));
        assert!(!package_target_selected(&library, &os(&["--bin", "demo"])));
        assert!(package_target_selected(&library, &os(&["--lib"])));
        assert!(!package_target_selected(&binary, &os(&["--lib"])));
    }

    #[test]
    fn config_round_trips() {
        let config = UserConfig {
            backend_path: PathBuf::from("/tmp/backend"),
            last_update_check: Some(42),
        };
        let encoded = toml::to_string(&config).unwrap();
        let decoded: UserConfig = toml::from_str(&encoded).unwrap();
        assert_eq!(decoded.backend_path, config.backend_path);
        assert_eq!(decoded.last_update_check, Some(42));
    }
}
