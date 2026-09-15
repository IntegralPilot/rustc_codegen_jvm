//! Completed computational bodies contain compact typed SSA only.
use jvm_compiler_core::ir;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SsaBody {
    pub ir: ir::Body,
    pub types: Arc<ir::Types>,
    pub lines: Option<jvm_compiler_core::jvm::select::SourceLines>,
    pub source_file: Option<String>,
    pub constants: Vec<super::Constant>,
    pub debug: Option<ir::DebugInfo>,
}
