//! Per-shard output ownership and serialization diagnostics.
use super::jvm::{self, ClassFile};
use jvm_compiler_core::classfile::{
    bundle,
    registry::{ClassRegistry, Emission},
};
use std::{
    path::PathBuf,
    sync::atomic::{AtomicUsize, Ordering},
};

static NEXT_DIRECTORY: AtomicUsize = AtomicUsize::new(0);

pub(super) struct ClassOutput {
    writer: bundle::Writer,
    path: PathBuf,
    directory: PathBuf,
}

fn io_error(context: &str, error: std::io::Error) -> jvm::Error {
    jvm::Error::VerificationError {
        context: context.into(),
        message: format!("Class bundle I/O failed: {error}"),
    }
}

impl ClassOutput {
    pub fn create(module: &str) -> jvm::Result<Self> {
        let ordinal = NEXT_DIRECTORY.fetch_add(1, Ordering::Relaxed);
        let directory = std::env::temp_dir().join(format!(
            "rustc-codegen-jvm-{}-{}-{ordinal}",
            std::process::id(),
            crate::stable_hash::short_hash(module, 12)
        ));
        std::fs::create_dir_all(&directory).map_err(|e| io_error(module, e))?;
        let path = directory.join("classes.jvmbundle");
        let writer = bundle::Writer::create(&path).map_err(|e| io_error(module, e))?;
        Ok(Self {
            writer,
            path,
            directory,
        })
    }

    pub fn emit(
        &mut self,
        registry: &ClassRegistry,
        name: String,
        bytes: Vec<u8>,
        origin: crate::metrics::ClassOrigin,
    ) -> jvm::Result<()> {
        crate::metrics::record_classfile_attempt(&name, origin, bytes.len());
        match registry
            .emit(&mut self.writer, &name, &bytes)
            .map_err(|e| io_error(&name, e))?
        {
            Emission::Duplicate => {
                crate::metrics::record_classfile_exact_duplicate(&name, origin, bytes.len())
            }
            Emission::Written { name_collision } => {
                crate::metrics::record_classfile_emitted(&name, origin, bytes.len(), name_collision)
            }
        }
        Ok(())
    }

    pub fn finish(self, module: String) -> jvm::Result<Vec<(String, PathBuf)>> {
        let Self {
            writer,
            path,
            directory,
        } = self;
        let empty = writer.is_empty();
        writer.finish().map_err(|e| io_error(&module, e))?;
        if empty {
            std::fs::remove_file(path).map_err(|e| io_error(&module, e))?;
            std::fs::remove_dir(directory).map_err(|e| io_error(&module, e))?;
            Ok(Vec::new())
        } else {
            Ok(vec![(module, path)])
        }
    }
}

pub(super) fn serialize_class_file(
    class_file: &ClassFile<'_>,
    context: &str,
) -> jvm::Result<Vec<u8>> {
    let mut bytecode = Vec::new();
    if let Err(error) = class_file.to_bytes(&mut bytecode) {
        let failing_method = class_file.methods.iter().find_map(|method| {
            let mut method_bytes = Vec::new();
            method
                .to_bytes(&mut method_bytes)
                .err()
                .map(|method_error| {
                    let name = class_file
                        .constant_pool
                        .try_get_utf8(method.name_index)
                        .map_or_else(|_| format!("#{}", method.name_index), ToString::to_string);
                    let descriptor = class_file
                        .constant_pool
                        .try_get_utf8(method.descriptor_index)
                        .map_or_else(
                            |_| format!("#{}", method.descriptor_index),
                            ToString::to_string,
                        );
                    format!("method {name}{descriptor}: {method_error:?}")
                })
        });
        return Err(jvm::Error::VerificationError {
            context: context.to_string(),
            message: failing_method.map_or_else(
                || format!("Failed to serialize class file: {error:?}"),
                |method| format!("Failed to serialize class file ({method})"),
            ),
        });
    }
    Ok(bytecode)
}
