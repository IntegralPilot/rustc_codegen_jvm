# cargo-jvm

`cargo-jvm` is the Cargo frontend for
[`rustc_codegen_jvm`](https://github.com/IntegralPilot/rustc_codegen_jvm).

It installs and maintains the compiler backend, supplies its custom target and
standard library configuration to Cargo, launches Rust programs and tests on
Java, and creates distributable JARs.

## Requirements

- Rust nightly
- JDK 8 or newer (`java`, `javac`, and `jar`)
- Python 3.8 or newer
- Git

## Install

### From crates.io

```bash
cargo install cargo-jvm
cargo jvm setup
```

`cargo jvm setup` clones `rustc_codegen_jvm` into the platform's user data
directory, builds it, and remembers that location. 

A custom destination, repository, or branch can be selected:

```bash
cargo jvm setup --path ~/tools/rustc_codegen_jvm
cargo jvm setup --repository https://github.com/example/fork.git --branch next
```

### From a source checkout

To install `cargo-jvm` directly from a source checkout:

```bash
git clone https://github.com/IntegralPilot/rustc_codegen_jvm
cd rustc_codegen_jvm
cargo install --path cargo-jvm
cargo jvm setup --path "$PWD"
```

## Update

You can update the `rustc_codegen_jvm` backend to a newer commit if available with:

```bash
cargo jvm update
```

`cargo-jvm` itself can be updated with `cargo install cargo-jvm --force`.

## Use in a Rust project

All ordinary Cargo selection and feature arguments are forwarded:

```bash
cargo jvm build
cargo jvm build --release --workspace --features serde
```

Build and launch a binary or example:

```bash
cargo jvm run
cargo jvm run --release --bin server -- --listen 127.0.0.1:8080
cargo jvm run --example demo
```

`run` uses a 16 MiB JVM thread stack by default. Override it or pass JVM
arguments explicitly when needed:

```bash
cargo jvm run --stack 32m --java-arg=-ea -- program-argument
```

Compile Rust test targets and run their libtest JARs:

```bash
cargo jvm test
cargo jvm test --release --workspace
cargo jvm test -- --nocapture
```

Create self-contained JARs containing the required Rust JVM runtime:

```bash
cargo jvm package --release
cargo jvm package --bin my-app --output dist/my-app.jar
cargo jvm package --lib --output dist/my-library.jar
```

Binary packages are directly executable with `java -jar`.

You can use `cargo jvm doctor` to report the installed `cargo-jvm` version and
source commit (when available), the configured backend's current Git commit,
and other environment details. Please run this if you are reporting a bug.

The backend checkout can be overridden without changing saved configuration
using `--backend-path PATH` or `CARGO_JVM_BACKEND_PATH`. `CARGO_JVM_HOME`
changes the default no-argument setup destination.

The `CARGO`, `RUSTC`, `JAVA`, `GIT`, `PYTHON`, and `CARGO_JVM_STACK` environment
variables override their corresponding executables or defaults.

Run `cargo jvm help` for the complete command reference.

## License

Licensed under either Apache-2.0 or MIT, at your option.
