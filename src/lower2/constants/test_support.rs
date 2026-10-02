use super::jvm;
use std::{
    path::{Path, PathBuf},
    process::Command,
};

/// Constants contain linker tags even in these hand-built unit-test classes.
pub(super) fn link(
    directory: &Path,
    class: &jvm::ClassFile<'_>,
    resources: Vec<jvm::resources::Resource>,
) -> PathBuf {
    let bundle = directory.join("constants.jvmbundle");
    let mut writer = jvm::bundle::Writer::create(&bundle).unwrap();
    let registry = jvm::registry::ClassRegistry::default();
    let mut bytes = Vec::new();
    jvm::encode::class_file(class, &mut bytes).unwrap();
    let name = class.constant_pool.try_get_class(class.this_class).unwrap();
    registry
        .emit(&mut writer, &name.to_rust_string(), &bytes)
        .unwrap();
    for resource in resources {
        registry
            .emit(
                &mut writer,
                &format!("{}{}", jvm::resources::BUNDLE_PREFIX, resource.name),
                &resource.bytes,
            )
            .unwrap();
    }
    writer.finish().unwrap();
    let output = directory.join("constants.jar");
    let linker = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("java-linker/target/release/java-linker")
        .with_extension(std::env::consts::EXE_EXTENSION);
    let result = Command::new(linker)
        .arg(bundle)
        .arg("-o")
        .arg(&output)
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    output
}
