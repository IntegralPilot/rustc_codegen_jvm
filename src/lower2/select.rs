//! Compile every computational method from typed SSA using the shared JVM selector.
use super::*;
use jvm::attributes::Instruction;

pub(crate) fn compile(
    body: &oomir::SsaBody,
    constant_pool: &mut InternedConstantPool,
    bootstrap_methods: &mut Vec<BootstrapMethod>,
    debug_info: DebugInfoOptions,
    relative_pointer_abi: bool,
) -> jvm::Result<MethodCode> {
    static VERIFY: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    struct Constants<'a>(&'a [oomir::Constant]);
    impl jvm_compiler_core::jvm::select::Constants for Constants<'_> {
        fn adapt(
            &self,
            from: jvm_compiler_core::ir::TypeId,
            to: jvm_compiler_core::ir::TypeId,
            types: &jvm_compiler_core::ir::Types,
            code: &mut Vec<Instruction>,
            cp: &mut InternedConstantPool,
        ) -> jvm::Result<()> {
            super::abi::adapt(from, to, types, code, cp)
        }
        fn emit(
            &self,
            index: u32,
            code: &mut Vec<Instruction>,
            cp: &mut InternedConstantPool,
        ) -> jvm::Result<()> {
            let constant =
                self.0
                    .get(index as usize)
                    .ok_or_else(|| jvm::Error::VerificationError {
                        context: "SSA constant pool".into(),
                        message: "invalid constant handle".into(),
                    })?;
            super::constants::load_constant(code, cp, constant)
        }
    }
    jvm_compiler_core::jvm::select::compile_with_options(
        &body.ir,
        &body.types,
        constant_pool,
        jvm_compiler_core::jvm::select::Options {
            verify: *VERIFY.get_or_init(|| std::env::var_os("RCGJ_VERIFY_SSA").is_some()),
            lines: if debug_info.line_numbers {
                body.lines.as_ref()
            } else {
                None
            },
            relative_pointer_abi,
            bootstrap: Some(bootstrap_methods),
            constants: Some(&Constants(&body.constants)),
            debug: if debug_info.local_variables {
                body.debug.as_ref()
            } else {
                None
            },
        },
    )
}
