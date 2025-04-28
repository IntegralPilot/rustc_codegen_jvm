# === Phony Targets ===
.PHONY: all help clean rust-components rust clean-rust java-linker clean-java-linker \
        shim-metadata-gen clean-shim-metadata-gen vendor-r8 clean-vendor-r8 \
        library clean-library gen-files clean-gen-files ci

# === Terminal Colors ===
GREEN  := \033[1;32m
CYAN   := \033[1;36m
RESET  := \033[0m

# === Directory Variables ===
JAVA_LINKER_DIR := java-linker
SHIM_METADATA_GEN_DIR := shim-metadata-gen
LIBRARY_DIR := library
LIBRARY_JAR := $(LIBRARY_DIR)/build/libs/library-0.1.0.jar
RUST_SOURCES := $(shell find $(SHIM_METADATA_GEN_DIR)/src -type f -name '*.rs')

# === Default Target ===
ifeq ($(IS_CI),1)
all: rust java-linker vendor-r8
	@echo "$(GREEN)✨ Build complete in CI mode! ✨$(RESET)"
else
all: rust gen-files java-linker vendor-r8
	@echo "$(GREEN)✨ Build complete! ✨$(RESET)"
endif

# === CI Target ===
ci:
	$(MAKE) all IS_CI=1

# === Help ===
help:
	@echo "$(CYAN)🛠️  Makefile for building the project$(RESET)"
	@echo ""
	@echo "Available targets:"
	@echo "  make all                  - Build all components"
	@echo "  make ci                   - Build all components in CI mode (skips rust-components and shim-metadata-gen)"
	@echo "  make clean                - Clean all components"
	@echo "  make rust-components      - Install needed Rust components"
	@echo "  make rust                 - Build the Rust root project"
	@echo "  make java-linker          - Build the Java Linker subproject"
	@echo "  make library              - Build the standard library shim"
	@echo "  make gen-files            - Generate necessary files from templates"
	@echo "  make clean-*              - Clean individual components"

# === Needed rust components ===
rust-components:
	@echo "$(CYAN)🔧 Installing Rust components...$(RESET)"
	rustup component add rustc-dev llvm-tools

# === Rust root project (Cargo) ===
ifeq ($(IS_CI),1)
rust: $(SHIM_METADATA_GEN_DIR)/core.json
	@echo "$(CYAN)📦 Building Rust root project...$(RESET)"
	RUSTFLAGS="-Awarnings" cargo build
else
rust: $(SHIM_METADATA_GEN_DIR)/core.json rust-components
	@echo "$(CYAN)📦 Building Rust root project...$(RESET)"
	RUSTFLAGS="-Awarnings" cargo build
endif

clean-rust:
	@echo "$(CYAN)🧹 Cleaning Rust root project...$(RESET)"
	cargo clean

# === Java Linker Subproject ===
java-linker:
	@echo "$(CYAN)📦 Building Java Linker...$(RESET)"
	cd $(JAVA_LINKER_DIR) && RUSTFLAGS="-Awarnings" cargo build

clean-java-linker:
	@echo "$(CYAN)🧹 Cleaning Java Linker...$(RESET)"
	cd $(JAVA_LINKER_DIR) && cargo clean

# === Library Shim Metadata Generator ===
$(SHIM_METADATA_GEN_DIR)/core.json: $(RUST_SOURCES) library clean-shim-metadata-gen-json-files
	@if [ "$(IS_CI)" = "1" ]; then \
	    echo "$(CYAN)CI mode: skipping shim-metadata-gen$(RESET)"; \
	else \
	    echo "$(CYAN)🛠️  Generating library shim metadata...$(RESET)"; \
	    cd $(SHIM_METADATA_GEN_DIR) && cargo run -- ../$(LIBRARY_JAR) ./core.json; \
	fi

clean-shim-metadata-gen:
	@echo "$(CYAN)🧹 Cleaning shim-metadata-gen...$(RESET)"
	cd $(SHIM_METADATA_GEN_DIR) && cargo clean

clean-shim-metadata-gen-json-files:
	@if [ "$(IS_CI)" = "1" ]; then \
	    echo "$(CYAN)CI mode: skipping cleaning shim-metadata-gen JSON files$(RESET)"; \
	else \
	    echo "$(CYAN)🧹 Cleaning shim-metadata-gen JSON files...$(RESET)"; \
	    rm -f $(SHIM_METADATA_GEN_DIR)/*.json; \
	fi

# === Standard Library Shim (Gradle) ===
library: $(LIBRARY_JAR)

$(LIBRARY_JAR):
	@echo "$(CYAN)📚 Building standard library shim...$(RESET)"
ifeq ($(IS_CI),1)
	cd $(LIBRARY_DIR) && gradle --no-daemon build && cd build/distributions && unzip -o library-0.1.0.zip
else
	cd $(LIBRARY_DIR) && gradle build && cd build/distributions && unzip -o library-0.1.0.zip
endif

clean-library:
	@echo "$(CYAN)🧹 Cleaning library shim...$(RESET)"
ifeq ($(IS_CI),1)
	cd $(LIBRARY_DIR) && gradle --no-daemon clean
else
	cd $(LIBRARY_DIR) && gradle clean
endif

# === Generate files from templates ===
gen-files: clean-gen-files
	@echo "$(CYAN)🛠️  Generating files from templates...$(RESET)"
	python3 GenerateFiles.py
	@echo "$(CYAN)🛠️  Files generated!$(RESET)"

clean-gen-files:
	@echo "$(CYAN)🧹 Cleaning template generated files...$(RESET)"
	rm -f jvm-unknown-unknown.json config.toml

# === Vendoring of R8 ===
vendor-r8:
	@echo "$(CYAN)📦 Vendoring R8...$(RESET)"
	mkdir -p ./vendor && curl -L -o ./vendor/r8.jar https://maven.google.com/com/android/tools/r8/8.9.35/r8-8.9.35.jar
	@echo "$(CYAN)📦 R8 vendored!$(RESET)"

clean-vendor-r8:
	@echo "$(CYAN)🧹 Cleaning vendored R8...$(RESET)"
	rm -rf ./vendor/r8.jar

# === Clean All ===
clean: clean-rust clean-java-linker clean-library clean-shim-metadata-gen clean-gen-files clean-vendor-r8
	@echo "$(GREEN)🧼 All clean!$(RESET)"

rebuild-shim: clean-library library shim-metadata-gen/core.json rust