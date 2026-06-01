# rustc_codegen_jvm

[![License: MIT/Apache-2.0](https://img.shields.io/badge/license-MIT%20%7C%20Apache--2.0-blue.svg)](https://opensource.org/licenses/MIT)  
[![CI](https://github.com/IntegralPilot/rustc_codegen_jvm/actions/workflows/ci.yml/badge.svg)](https://github.com/IntegralPilot/rustc_codegen_jvm/actions)

A custom Rust compiler backend that emits Java Virtual Machine bytecode.  
Compile Rust code into a runnable `.jar` compatible with JVM 8+.

---

## Table of Contents

1. [Demos](#demos)
2. [Features](#features)
3. [How It Works](#how-it-works)
4. [Prerequisites](#prerequisites)
5. [Installation & Build](#installation--build)
6. [Usage](#usage)
7. [Running Tests](#running-tests)
8. [Project Structure](#project-structure)
9. [Contributing](#contributing)
10. [License](#license) 

---

## Demos

These examples are located in `tests/binary`, compiled to JVM bytecode, and verified on CI during integration testing. Examples include:

- **[RSA](tests/binary/rsa/src/main.rs)** encryption/decryption  
- **[Binary search](tests/binary/binsearch/src/main.rs)** algorithm  
- **[Fibonacci](tests/binary/fibonacci/src/main.rs)** sequence generator  
- **[Collatz conjecture](tests/binary/collatz/src/main.rs)** verifier  
- **[Large prime](tests/binary/primes/src/main.rs)** generator  
- **[Enums](tests/binary/enums/src/main.rs)** and **[Structs](tests/binary/structs/src/main.rs)** (nested data structures: structs, tuples, arrays, and slices)  
- **[Implementation blocks](tests/binary/impl/src/main.rs)** and **[Traits](tests/binary/traits/src/main.rs)** (including dynamic dispatch)
- **[Unions](tests/binary/unions/src/main.rs)** (demonstrating certain `unsafe` operations)

---

## Features

- **Optimisations**: Constant folding, constant propagation, and dead code elimination to generate clean JVM bytecode.
- **Standard Library Support**: Basic `core` support on host target for JVM output.
- **Arithmetic**: Support for integers, floats, and checked operations.
- **Operations**: Comparisons, bitwise, and logical operations.
- **Control Flow**: Support for `if`/`else`, `match`, `for`, `while`, and `loop`.
- **Type Handling**: Type casting (`as`) and primitive types.
- **Functions**: Function calls, recursion, and function pointers in multiple contexts (within ADTs, as variables, parameters, return values, or generics).
- **Data Structures**: Arrays, slices, structs, tuples, and enums (C-like and Rust-style).
- **Memory Management**: Mutable borrowing, references, and dereferencing.
- **Object-Oriented Constructs**: Implementations for ADTs, including `self`, `&self`, and `&mut self`.
- **Traits & Closures**: Dynamic dispatch (`&dyn Trait`) and closure capturing.
- **Unions**: Supported for basic types (`bool`, `i8`/`u8`, `i16`/`u16`, `i32`/`u32`, `f32`, `f64`) and structs containing combinations of these types.
- **Output**: Executable `.jar` generation for binary crates.
- **Testing**: Comprehensive integration tests covering these features in both debug and release modes.

*Current Milestone:* Full support for the Rust `core` crate.

---

## How It Works

1. **Rustc Frontend → MIR**  
   The standard `rustc` compiler parses your code into Mid-level IR (MIR).
2. **MIR → OOMIR**  
   A custom "Object-Oriented MIR" layer simplifies MIR into OOP-style constructs (defined in `src/lower1.rs`).  
3. **OOMIR Optimiser**  
   Optimises OOMIR (defined in `src/optimise1.rs`) using:
   - **Constant Folding**: Evaluates constant expressions at compile time.  
   - **Constant Propagation**: Replaces variables with their constant values.  
   - **Dead Code Elimination**: Removes unused execution paths.  
   - **Algebraic Simplification**: Simplifies expressions using algebraic identities.
4. **OOMIR → JVM Classfile**  
   Translates OOMIR to `.class` files using `ristretto_classfile` (defined in `src/lower2.rs`).  
5. **R8 Pass**  
   Invokes `r8` to add stack map frames (required for JVM 8+) and apply further Optimisation passes.
6. **Link & Package**  
   Uses `java-linker` to bundle `.class` files into a runnable `.jar` with an appropriate `META-INF/MANIFEST.MF`.

---

## Prerequisites

- **Rust Nightly** (`rustup default nightly`)  
- **Gradle 8.5+** (`gradle` must be in system PATH)
- **JDK 8+** (`java` must be in system PATH, with `JAVA_HOME` set)
- **Python 3** (`python3` must be in system PATH)

---

## Installation & Build

Clone the repository and build all components using the main build script:

```bash
# Clone the repository
git clone https://github.com/IntegralPilot/rustc_codegen_jvm.git
cd rustc_codegen_jvm

# Build all components using Python
# On Linux or macOS:
./build.py all

# On Windows:
python build.py all
```

This script builds the necessary components in the correct dependency order:
- The Kotlin library shim (`library/`)
- The shim metadata file (`core.json`)
- The `java-linker` executable
- The `rustc_codegen_jvm` backend library
- Configuration files (`config.toml`, `jvm-unknown-unknown.json`)
- Vendored dependencies (such as R8)

Subsequent runs of `build.py` check file timestamps and will only rebuild modified components.

---

## Usage

1. **Configure Your Project**  
   In your target Rust project directory, create or update `.cargo/config.toml` by copying the generated template located in the root of this repository.

   Ensure your `Cargo.toml` contains the following feature flag to support separate compilation configurations:
   ```toml
   cargo-features = ["profile-rustflags"]
   ```

2. **Build with Cargo**  
   ```bash
   cargo build           # Debug build
   cargo build --release # Optimised build
   ```

3. **Run the JAR File**  
   ```bash
   java -jar target/debug/deps/your_crate*.jar   # Run debug build
   java -jar target/release/deps/your_crate*.jar # Run release build
   ```

---

## Running Tests

First, ensure the toolchain is built:

```bash
# On Linux/macOS:
./build.py all

# On Windows:
python build.py all
```

Run the test suite with the test runner:

```bash
# Run tests in debug mode
python Tester.py

# Run tests in release mode
python Tester.py --release
```

Test results will output to the console. Temporary test artifacts are written to `.generated/` for debugging.

---

## Project Structure

```
.
├── src/                      # rustc_codegen_jvm compiler backend
│   ├── lib.rs
│   ├── lower1.rs             # MIR → OOMIR conversion
│   ├── lower2.rs             # OOMIR → JVM bytecode translation
│   └── oomir.rs              # OOMIR data definitions
├── java-linker/              # Bundles compiled .class files into .jar archives
├── tests/binary/             # Integration tests and source examples
├── library/                  # Kotlin shim implementation for the Rust core library
├── shim-metadata-gen/        # Tool to generate core.json metadata
├── proguard/                 # Proguard / R8 configuration rules
├── build.py                  # Orchestrator build script
├── config.toml.template      # Configuration template for cargo projects
├── jvm-unknown-unknown.json.template
├── Tester.py                 # Automated test runner
└── LICENSE, LICENSE-Apache
```

---

## Contributing

Contributions, issues, and pull requests are welcome.

---

## License

This project is dual-licensed under the **MIT License** and the **Apache License, Version 2.0** at your option:
- <https://opensource.org/licenses/MIT>
- <https://www.apache.org/licenses/LICENSE-2.0>
