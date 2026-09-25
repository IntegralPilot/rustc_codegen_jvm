/// A Rust source position attached to generated code.
///
/// JVM line tables only store line numbers; the corresponding file name is
/// stored once on the containing class through its `SourceFile` attribute.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    pub file_name: String,
    pub line: u32,
}
