cargo clean
cargo build
pushd java-linker
cargo clean
cargo build
popd
pushd asm-processor
gradle clean
gradle shadowJar
popd