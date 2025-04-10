# === Phony Targets ===
.PHONY: all clean help rust clean-rust java-linker clean-java-linker \
        shim-metadata-gen clean-shim-metadata-gen \
        asm-processor clean-asm-processor \
        library clean-library

# === Terminal Colors ===
GREEN  := \033[1;32m
CYAN   := \033[1;36m
RESET  := \033[0m

# === Default Target ===
all: rust java-linker asm-processor
	@echo "$(GREEN)✨ Build complete! ✨$(RESET)"

# === Help ===
help:
	@echo "$(CYAN)🛠️  Makefile for building the project$(RESET)"
	@echo ""
	@echo "Available targets:"
	@echo "  make all                  - Build all components"
	@echo "  make clean                - Clean all components"
	@echo "  make rust                 - Build the Rust root project"
	@echo "  make java-linker          - Build the Java Linker subproject"
	@echo "  make shim-metadata-gen    - Generate library shim metadata"
	@echo "  make asm-processor        - Build the ASM processor"
	@echo "  make library              - Build the standard library shim"
	@echo "  make clean-*              - Clean individual components"

# === Rust root project (Cargo) ===
rust: shim-metadata-gen
	@echo "$(CYAN)📦 Building Rust root project...$(RESET)"
	cargo build

clean-rust:
	@echo "$(CYAN)🧹 Cleaning Rust root project...$(RESET)"
	cargo clean

# === Java Linker Subproject ===
java-linker:
	@echo "$(CYAN)📦 Building Java Linker...$(RESET)"
	cd java-linker && cargo build

clean-java-linker:
	@echo "$(CYAN)🧹 Cleaning Java Linker...$(RESET)"
	cd java-linker && cargo clean

# === Library shim metadata generator ===
shim-metadata-gen: library
	@echo "$(CYAN)🔧 Generating shim metadata...$(RESET)"
	cd shim-metadata-gen && rm -f core.json && cargo run -- ../library/build/libs/library-0.1.0.jar ./core.json

clean-shim-metadata-gen:
	@echo "$(CYAN)🧹 Cleaning shim-metadata-gen...$(RESET)"
	cd shim-metadata-gen && cargo clean

# === ASM Processor (Gradle) ===
asm-processor:
	@echo "$(CYAN)⚙️  Building ASM processor...$(RESET)"
	cd asm-processor && gradle shadowJar

clean-asm-processor:
	@echo "$(CYAN)🧹 Cleaning ASM processor...$(RESET)"
	cd asm-processor && gradle clean

# === Standard Library Shim (Gradle) ===
library:
	@echo "$(CYAN)📚 Building standard library shim...$(RESET)"
	cd library && gradle build

clean-library:
	@echo "$(CYAN)🧹 Cleaning library shim...$(RESET)"
	cd library && gradle clean

# === Clean All ===
clean: clean-rust clean-java-linker clean-asm-processor clean-library clean-shim-metadata-gen
	@echo "$(GREEN)🧼 All clean!$(RESET)"