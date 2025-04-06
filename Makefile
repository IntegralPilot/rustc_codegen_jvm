# Makefile

.PHONY: all rust java-linker asm-processor clean clean-rust clean-java-linker clean-asm-processor

all: rust java-linker asm-processor

# --- Rust root project ---
rust:
	cargo build

clean-rust:
	cargo clean

# --- Java Linker Rust Subproject ---
java-linker:
	cd java-linker && cargo build

clean-java-linker:
	cd java-linker && cargo clean

# --- ASM Processor (Gradle) ---
asm-processor:
	cd asm-processor && gradle shadowJar

clean-asm-processor:
	cd asm-processor && gradle clean

# --- Clean everything ---
clean: clean-rust clean-java-linker clean-asm-processor
