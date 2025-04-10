# Rustc Codegen JVM 🚀

[![License: MIT OR Apache-2.0](https://img.shields.io/badge/License-MIT%20OR%20Apache--2.0-blue.svg)](https://opensource.org/licenses/MIT)

Welcome! This project provides a custom Rust compiler backend (`rustc_codegen_jvm`) that compiles Rust code into Java Virtual Machine (JVM) bytecode. This allows Rust programs to run on the JVM. Currently, the generated bytecode supports JRE 8 and later versions.

## How It Works

The toolchain transforms Rust code into executable `.jar` files through several stages:

1.  **Rustc Frontend (Parsing & MIR Generation)**
    *   Your Rust code is parsed and lowered to *Mid-level Intermediate Representation (MIR)* by the standard `rustc` frontend.

2.  **MIR to OOMIR (Object-Oriented MIR)**
    *   The MIR is lowered further into a custom intermediate representation called OOMIR. This step simplifies MIR constructs into a representation closer to object-oriented concepts, making the translation to JVM bytecode more manageable. (See `src/lower1.rs`).

3.  **OOMIR to JVM Classfile**
    *   The OOMIR is then translated into standard Java `.class` files containing JVM bytecode. This is the core task of the `rustc_codegen_jvm` library in this repository (See `src/lower2.rs`). It utilizes the `ristretto_classfile` library for bytecode generation.

4.  **Classfile Post-Processing (Stack Map Frames)**
    *   The generated `.class` files are processed by a dedicated tool (`asm-processor`, written in Kotlin using the ASM library). This tool calculates and inserts *Stack Map Frames*, which are required for class verification in modern JVM versions (Java 7+).

5.  **Linking & `.jar` Generation**
    *   Finally, the processed `.class` files for the crate and its dependencies (though dependency handling is currently basic) are linked together into a single executable `.jar` file. This step is handled by `java-linker`, a custom linker built as part of this project (See `java-linker/`). It also generates the necessary `META-INF/MANIFEST.MF` file, marking the main class if applicable.

## Current Capabilities

This backend currently supports a subset of Rust features:

*   ✅ Compiling minimal `no_std` & `no_core` Rust programs (like an empty `main`) using the `jvm-unknown-unknown` target.
*   ✅ Compiling simple programs using basic `core` features (like the `is_even_plus_one` test) using the host target.
*   ✅ Basic integer arithmetic operations on all types of numbers:
    *   Addition (`+`), Subtraction (`-`), Multiplication (`*`), Division (`/`), Remainder (`%`).
    *   Checked addition and subtraction (`checked_add`, `checked_sub`) returning `(result, overflowed_bool)` tuples.
*   ✅ Comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`).
*   ✅ Bitwise operations (`&`, `|`, `^`, `<<`, `>>`).
*   ✅ Basic control flow: `if`/`else` statements, `panic!` / `assert!` (with simple string messages).
*   ✅ Calling other `static` functions within the same crate (including recursion).
*   ✅ Basic variable assignment (`let x = y;`).
*   ✅ Arrays and slices.
*   ✅ Generating executable `.jar` files for binary crates.

### Next Milestone:
🚧 **Full support for the `core` crate** is the next major goal.

## Prerequisites

*   **Rust Nightly:** Ensure you are using the latest **nightly** Rust toolchain. You can set this up with `rustup default nightly`.
*   **Rust Components:** Install necessary components:
    ```bash
    rustup component add rust-src rustc-dev llvm-tools-preview
    ```
    *(Note: The `rust-toolchain.toml` file in the repository should handle this automatically if you use `cargo` within the project.)*
*   **Java Development Kit (JDK):** A working JDK (version 8 or later recommended) is required to run the generated `.jar` files and the `asm-processor`. Make sure `java` is in your PATH.
*   **Gradle:** Required to build the `asm-processor` Kotlin project. Make sure `gradle` is in your PATH.

## Building the Toolchain

1.  **Run Setup Script (if needed):**
    *   The `setup.sh` script mainly adds rustup components, which `rust-toolchain.toml` might already handle. Ensure the components listed in Prerequisites are installed.
2.  **Build All Components:**
    *   You can use the provided Makefile or build script:
      ```bash
      # Using Make
      make all
      ```
    *   This will:
        *   Build the main `rustc_codegen_jvm` library (`target/debug/librustc_codegen_jvm.dylib`).
        *   Build the `java-linker` (`java-linker/target/debug/java-linker`).
        *   Build the `asm-processor` (`asm-processor/build/libs/asm-processor-*.jar`).

## Using the Toolchain (Compiling Another Rust Project)

To compile *your own* Rust project using this backend:

1.  **Get the Toolchain:** Clone this repository and build it as described above. Let's assume you cloned it to `/path/to/rustc_codegen_jvm`. Ensure you build the toolchain first, as described in the previous section.


2.  **Configure Your Rust Project:**
    *   In your *own* Rust project's directory, create a `.cargo/config.toml` file (if it doesn't exist).
    *   Add the following content to the file:
        ```toml
        [build]
        rustflags = [
          "-Z", "codegen-backend=/path/to/rustc_codegen_jvm/target/debug/librustc_codegen_jvm.dylib",
          "-C", "linker=/path/to/rustc_codegen_jvm/java-linker/target/debug/java-linker",
          "-C", "link-args=--asm-processor /path/to/rustc_codegen_jvm/asm-processor/build/libs/asm-processor-1.0-SNAPSHOT-all.jar"
        ]
        ```
    *   **Important:** Replace `/path/to/rustc_codegen_jvm/...` with the path to where you cloned the repository. If you're not on macOS, changed `.dylib` to `.so` for Linux or `.dll` for Windows.

3.  **Build Your Project:**
    *   Run the standard Cargo build command, targeting the host target (using the `jvm-unknown-unknown` target is an emerging feature and means you can't even use `core`, for now, so not recommended). Cargo will read the `.cargo/config.toml` and use the specified target and flags automatically.
    ```bash
    cargo build
    # Or for release builds:
    # cargo build --release
    ```

4.  **Find & Run the Generated `.jar`:**
    *   The compiled `.jar` file will be located in your project's `target` directory:
        *   Debug: `target/debug/deps/[your_crate_name].jar`
        *   Release: `target/release/deps/[your_crate_name].jar`
    *   If your crate is a binary (`[[bin]]` or `src/main.rs`), run it using the `java` command:
        ```bash
        java -jar target/jvm-unknown-unknown/debug/[your_crate_name].jar
        ```

## Running This Project's Tests

This project includes integration tests managed by a Python script.

1.  **Ensure Toolchain is Built:** Build the project using `make all`.
2.  **Check Target JSON:** Make sure the `jvm-unknown-unknown.json` file in the *root* of this repository has the **relative paths** starting with `../../../` for the linker and backend, as the tester expects this structure when running tests from subdirectories. If you changed them to absolute paths for external use, change them back temporarily.
3.  **Run the Tester:**
    ```bash
    python3 Tester.py
    # For release mode tests:
    # python3 Tester.py --release
    ```
4.  **Check Output:** Look for the final "✅ All tests passed!" message. If tests fail, the script will generate `.generated` files in the respective test directories (`tests/binary/*`) containing error details or output diffs.

## Project Structure

*   `src/`: Contains the source code for the `rustc_codegen_jvm` backend library.
    *   `lib.rs`: Main backend integration point.
    *   `lower1.rs`: MIR to OOMIR lowering pass.
    *   `lower2.rs`: OOMIR to JVM bytecode generation pass.
    *   `oomir.rs`: Definition of the Object-Oriented MIR (OOMIR).
*   `java-linker/`: Source code for the custom linker that creates `.jar` filesn from the generated `.class` files.
*   `asm-processor/`: Kotlin/Gradle project for the ASM-based bytecode post-processor that adds stack map frames to the generated `.class` files.
*   `tests/`: Integration tests.
    *   `binary/`: Tests for compiling executable Rust crates.
*   `library/`: Source code for a minimal Kotlin-based implementation of the Rust core library. This serves as a temporary substitute to bootstrap the project until the backend can fully compile the Rust core library itself.
*   `shim-metadata-gen`: A tool for generating metadata for the Kotlin core library, called at compiletime to provide nessecary info (descriptors) to the codegen backend. It's generated metadata is embedded in the generated library, so not needed at runtime.
    *   `core.json`: Metadata for the core library, generated by this tool.
*   `jvm-unknown-unknown.json`: The Rust target specification file.
*   `Tester.py`: Python script for running integration tests.
*   `Makefile` Scripts for building the entire toolchain.
*   `setup.sh`: Script to install Rust components.
*   `Cargo.toml`, `Cargo.lock`: Rust package definition and dependencies for the backend.
*   `LICENSE`, `LICENSE-Apache`: Project licenses.

## Contributing

Contributions are welcome! Feel free to open issues or pull requests.

## License

This project is dual-licensed under either of:

*   MIT License ([LICENSE-MIT](LICENSE) or http://opensource.org/licenses/MIT)
*   Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-Apache) or http://www.apache.org/licenses/LICENSE-2.0)

at your option. 