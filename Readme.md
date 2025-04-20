# rustc_codegen_jvm 🚀

[![License: MIT/Apache-2.0](https://img.shields.io/badge/license-MIT%20%7C%20Apache--2.0-blue.svg)](https://opensource.org/licenses/MIT)  
[![CI](https://github.com/IntegralPilot/rustc_codegen_jvm/actions/workflows/ci.yml/badge.svg)](https://github.com/IntegralPilot/rustc_codegen_jvm/actions)

A **custom Rust compiler backend** that emits Java Virtual Machine bytecode.  
Compile your Rust code into a runnable `.jar` on JVM 8+!

---

## 📖 Table of Contents

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

## 🔥 Demos

All examples live in `tests/binary` and are compiled to JVM bytecode & run/tested on the CI on every commit. Some exicting demos made in pure-Rust include:

- **RSA** encryption/decryption (`rsa`) 
- **Fibonacci** sequence generator (`fibonacci`)  
- **Collatz conjecture** verifier (`collatz`)  
- **Large prime** generator (`primes`)
- Nested data structures: enums, structs, tuples, arrays, slices (`enums`, `structs`)  
- …and more!

---

## ✨ Features

- **Minimal `no_std` & `no_core`** programs via `jvm-unknown-unknown`  
- Basic `core` support on host target for JVM output  
- Arithmetic (integers + floats, incl. checked ops)  
- Comparisons, bitwise & logical ops  
- Control flow: `if`/`else`, `match`, `for`, `while`, `loop`  
- Type casting (`as`), primitive types  
- Function calls (recursion supported)  
- Arrays & slices with nested indexing  
- Structs, tuples, enums (both C‑like and Rust‑style)  
- Executable `.jar` generation for binary crates  

🚧 **Next Milestone:** Full support for the Rust `core` crate.

---

## ⚙️ How It Works

1. **Rustc Frontend → MIR**  
   Standard `rustc` parses your code into Mid‑level IR (MIR).
2. **MIR → OOMIR**  
   Custom “Object‑Oriented MIR” simplifies MIR into OOP‑style constructs.  
   _(see `src/lower1.rs`)_  
3. **OOMIR → JVM Classfile**  
   Translate to `.class` files using `ristretto_classfile`.  
   _(see `src/lower2.rs`)_  
4. **Post‑Process Stack Map Frames**  
   Kotlin `asm-processor` (ASM library) adds verification frames.  
5. **Link & Package**  
   `java-linker` bundles `.class` files into a runnable `.jar` with `META-INF/MANIFEST.MF`.

---

## 🛠 Prerequisites

- **Rust Nightly** (`rustup default nightly`)  
- **JDK 8+** (`java` in PATH)  
- **Gradle** (`gradle` in PATH)  
- **Python 3** (`python3` in PATH)

---

## 🏗 Installation & Build

```bash
# Clone & enter repo
git clone https://github.com/your-org/rustc_codegen_jvm.git
cd rustc_codegen_jvm

# Build everything
make all
```

This will compile:

- `rustc_codegen_jvm` backend library  
- `java-linker`  
- `asm-processor`  
- Kotlin shim for `core`  
- Generate `config.toml` & `jvm-unknown-unknown.json`  

If you relocate the repo, re-run:
```bash
make gen-files
```

---

## 🚀 Usage

1. **Configure your project**  
   In *your* Rust project directory, create or update `.cargo/config.toml` with the generated template.

2. **Build with Cargo**  
   ```bash
   cargo build           # debug
   cargo build --release # optimized
   ```

3. **Run the `.jar`**  
   ```bash
   java -jar target/debug/deps/your_crate.jar
   ```

---

## 🧪 Running Tests

Ensure the toolchain is built:

```bash
make all
# If you moved the repo:
make gen-files
```

Then:

```bash
python3 Tester.py
# or with --release for release‑mode tests
```

Look for `✅ All tests passed!` or inspect `.generated` files on failure.

---

## 📂 Project Structure

```
.
├── src/                   # rustc_codegen_jvm backend
│   ├── lib.rs
│   ├── lower1.rs          # MIR → OOMIR
│   ├── lower2.rs          # OOMIR → JVM bytecode
│   └── oomir.rs           # OOMIR definitions
├── java-linker/           # Bundles .class files into .jar
├── asm-processor/         # Kotlin ASM post‑processor
├── tests/binary/          # Integration tests
├── library/               # Kotlin shim for Rust core library
├── shim-metadata-gen/     # Generates core.json metadata
├── Makefile               # build & gen-files
├── config.toml.template
├── jvm-unknown-unknown.json.template
├── Tester.py              # test runner
├── GenerateFiles.py       # regenerates config & target spec
└── LICENSE, LICENSE-Apache
```

---

## 🤝 Contributing

Contributions, issues & PRs welcome! :)

---

## 📄 License

Dual‑licensed under **MIT** OR **Apache 2.0** at your option:  
<https://opensource.org/licenses/MIT>  
<https://www.apache.org/licenses/LICENSE-2.0>