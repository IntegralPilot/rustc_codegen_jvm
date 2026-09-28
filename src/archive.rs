//! Keep JVM payloads when rustc filters native objects out of LTO dependencies.
use rustc_codegen_ssa::back::archive::{
    AddArchiveKind, ArArchiveBuilder, ArchiveBuilder, ArchiveBuilderBuilder, ArchiveEntryKind,
    ArchiveSymbols, DEFAULT_OBJECT_READER, ImportLibraryItem,
};
use rustc_session::Session;
use std::{io, path::Path};

pub(super) struct RlibArchiveBuilder;

impl ArchiveBuilderBuilder for RlibArchiveBuilder {
    fn new_archive_builder<'a>(&self, sess: &'a Session) -> Box<dyn ArchiveBuilder + 'a> {
        Box::new(JvmArchiveBuilder(ArArchiveBuilder::new(
            sess,
            &DEFAULT_OBJECT_READER,
        )))
    }

    fn create_dll_import_lib(
        &self,
        _sess: &Session,
        _lib_name: &str,
        _dll_imports: Vec<ImportLibraryItem>,
        _tmpdir: &Path,
    ) {
        unimplemented!("creating dll imports is not supported");
    }
}

struct JvmArchiveBuilder<'a>(ArArchiveBuilder<'a>);

impl ArchiveBuilder for JvmArchiveBuilder<'_> {
    fn add_file(&mut self, path: &Path, kind: ArchiveEntryKind) {
        self.0.add_file(path, kind);
    }

    fn add_archive(&mut self, path: &Path, kind: AddArchiveKind<'_>) -> io::Result<()> {
        match kind {
            AddArchiveKind::Rlib(cache, skip) => {
                // Native LTO has already consumed upstream Rust objects. JVM
                // bundles are instead merged by java-linker and must reach it,
                // even when the Cargo profile requests thin or fat LTO.
                let skip = |name: &str, kind| {
                    let jvm_payload = name.ends_with(".jvmbundle") || name.ends_with(".jvmsymbols");
                    !jvm_payload && skip(name, kind)
                };
                self.0.add_archive(path, AddArchiveKind::Rlib(cache, &skip))
            }
            AddArchiveKind::Other => self.0.add_archive(path, kind),
        }
    }

    fn build(self: Box<Self>, output: &Path, symbols: Option<ArchiveSymbols>) -> bool {
        Box::new(self.0).build(output, symbols)
    }
}
