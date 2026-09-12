//! Rust-to-JVM representation queries and generated type support.
use super::jvm_names;
use crate::oomir::{self, DataType, DataTypeMethod};

use rustc_abi::{FieldIdx, TagEncoding, VariantIdx, Variants};
use rustc_data_structures::stable_hash::{StableHash, StableHasher};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_hashes::Hash64;
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::print::{with_no_trimmed_paths, with_resolve_crate_name};
use rustc_middle::ty::{
    AdtDef, EarlyBinder, ExistentialPredicate, FloatTy, GenericArgs, GenericArgsRef, IntTy, Region,
    Ty, TyCtxt, TyKind, TypeFoldable, TypeFolder, TypeVisitableExt, TypingEnv, UintTy,
};
use rustc_span::{Symbol, def_id::DefId, sym};

mod mapping;
pub(crate) use mapping::{is_codegen_sized, mir_int_to_oomir_const, ty_to_oomir_type};
mod enums;
use enums::*;
pub(crate) use enums::{
    adapt_simple_enum_operand, enum_scoped_method_name, enum_variant_field_name,
    jvm_subtype_payload_ty, union_from_method_name, union_getter_method_name,
    union_setter_method_name,
};
mod abi;
pub(crate) use abi::{
    callable_trait_object_abi, ensure_fn_ptr_interface, fn_ptr_signature_from_ty,
    has_open_jvm_abi_type, ty_to_erased_oomir_type,
};
mod storage;
use storage::*;
mod layout;
use layout::*;
pub(crate) use layout::{
    enum_union_discriminant_supported, layout_align_bytes, layout_size_bytes,
    simple_enum_union_size,
};
mod adt;
use adt::*;
pub(crate) use adt::{force_define_named_adt, should_define_named_data_type};
mod drop;
use drop::*;
mod scalars;
use scalars::*;
mod bytes;
use bytes::*;
mod enum_codecs;
use enum_codecs::*;
mod write;
use write::*;
mod read;
use read::*;
mod transmute;
pub(crate) use transmute::ensure_exact_transmute_helper;
mod pointer_codecs;
use pointer_codecs::*;
pub(crate) use pointer_codecs::{
    ensure_pointer_memory_codec, pointer_memory_codec_operand, pointer_view_codec_operand,
};
mod coroutines;
use coroutines::*;
mod unions;
pub(crate) use unions::ensure_union_data_type;
use unions::*;
mod names;
use names::*;
pub(crate) use names::{
    generate_adt_jvm_class_name, generate_tuple_jvm_class_name, get_field_name_from_index,
    readable_rust_generic_arg_name, readable_rust_type_name, sanitize_name_token, short_hash,
    stable_def_identity, stable_def_path, stable_instance_identity, stable_instance_key,
    stable_normalized_instance_key, stable_type_identity,
};

pub const UNION_BYTES_FIELD: &str = "_bytes";
pub const UNION_OBJECTS_FIELD: &str = "_objects";
pub const MANAGED_OBJECT_POINTER_VIEW_CODEC: &str = "@managed-object";
pub const RAW_POINTER_VIEW_CODEC: &str = "@raw-pointer";
const ARRAY_REFERENCE_VIEW_CODEC_PREFIX: &str = "@array-reference\n";
const SLICE_POINTER_VIEW_CODEC_PREFIX: &str = "@slice-pointer\n";
const STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX: &str = "@struct-tail-pointer\n";
const TRAIT_POINTER_VIEW_CODEC_PREFIX: &str = "@trait-pointer\n";
pub(super) const ENUM_UNION_DISCRIMINANT_METHOD: &str = "_unionDiscriminant";
const ENUM_FROM_UNION_DISCRIMINANT_METHOD: &str = "_fromUnionDiscriminant";
const ENUM_WRITE_UNION_STORAGE_METHOD: &str = "_writeUnionStorage";
const ENUM_READ_UNION_STORAGE_METHOD: &str = "_readUnionStorage";
const ENUM_DROP_FIELDS_METHOD: &str = "_rust_drop_fields";
const MANAGED_DROP_METHOD: &str = "rustDrop";
const MANAGED_DROP_INTERFACE: &str = "org/rustlang/runtime/RustDrop";

#[derive(Clone)]
pub struct CallableTraitObjectAbi<'tcx> {
    pub tuple_ty: Ty<'tcx>,
    pub signature: oomir::Signature,
    pub interface_name: String,
}

#[derive(Clone)]
struct JvmUnionStorage {
    bytes_var: String,
    objects_var: String,
    base_offset: oomir::Operand,
}

impl JvmUnionStorage {
    fn at_start(bytes_var: impl Into<String>, objects_var: impl Into<String>) -> Self {
        Self {
            bytes_var: bytes_var.into(),
            objects_var: objects_var.into(),
            base_offset: oomir::Operand::Constant(oomir::Constant::I32(0)),
        }
    }

    fn at_offset(
        bytes_var: impl Into<String>,
        objects_var: impl Into<String>,
        base_offset: oomir::Operand,
    ) -> Self {
        Self {
            bytes_var: bytes_var.into(),
            objects_var: objects_var.into(),
            base_offset,
        }
    }

    fn byte_index(
        &self,
        relative_offset: usize,
        instructions: &mut Vec<oomir::Instruction>,
        temp_counter: &mut usize,
    ) -> oomir::Operand {
        if let oomir::Operand::Constant(oomir::Constant::I32(base)) = &self.base_offset {
            return oomir::Operand::Constant(oomir::Constant::I32(
                base.saturating_add(relative_offset as i32),
            ));
        }
        if relative_offset == 0 {
            return self.base_offset.clone();
        }

        let dest = next_union_temp("union_storage_offset", temp_counter);
        instructions.push(oomir::Instruction::Binary {
            op: crate::oomir::BinaryOp::Add,
            dest: dest.clone(),
            op1: self.base_offset.clone(),
            op2: oomir::Operand::Constant(oomir::Constant::I32(relative_offset as i32)),
        });
        operand_var(dest, oomir::Type::I32)
    }
}

#[derive(Clone)]
struct UnionAggregateField<'tcx> {
    rust_ty: Ty<'tcx>,
    jvm_ty: oomir::Type,
    jvm_name: String,
    offset: usize,
}

#[derive(Clone)]
struct UnionAggregateLayout<'tcx> {
    class_name: String,
    fields: Vec<UnionAggregateField<'tcx>>,
}

#[derive(Clone)]
struct CoroutineMemoryLayout<'tcx> {
    class_name: String,
    size: usize,
    state_offset: usize,
    state_size: usize,
    upvars: Vec<UnionAggregateField<'tcx>>,
    saved: Vec<UnionAggregateField<'tcx>>,
    variant_saved_offsets: Vec<Vec<(usize, usize)>>,
}

#[derive(Clone)]
enum UnionEnumTag {
    Single {
        variant: VariantIdx,
    },
    Direct {
        offset: usize,
        size: usize,
    },
    Niche {
        offset: usize,
        size: usize,
        untagged_variant: VariantIdx,
        niche_start_variant: VariantIdx,
        niche_end_variant: VariantIdx,
        niche_start: u128,
    },
}

struct AllRegionEraser<'tcx> {
    tcx: TyCtxt<'tcx>,
}

impl<'tcx> TypeFolder<TyCtxt<'tcx>> for AllRegionEraser<'tcx> {
    fn cx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }

    fn fold_region(&mut self, _region: Region<'tcx>) -> Region<'tcx> {
        self.tcx.lifetimes.re_erased
    }
}

#[derive(Clone)]
pub(crate) struct ExactTransmuteHelper {
    pub class_name: String,
    pub method_name: String,
    pub signature: oomir::Signature,
}

#[derive(Clone)]
pub(crate) struct PointerMemoryCodec {
    pub class_name: String,
}

// Keep ordinary nested generic/tuple names readable for Java callers. Hashing
// is only a last-resort guard against unwieldy class-file names, consistent
// with the other generated-name families.
const MAX_TUPLE_NAME_LEN: usize = 180;

#[cfg(test)]
mod tests {
    use super::sanitize_name_token;

    #[test]
    fn generated_name_tokens_preserve_rust_underscores() {
        assert_eq!(sanitize_name_token("Tuple_"), "Tuple");
        assert_eq!(
            sanitize_name_token("Result<Type__Name::Error>"),
            "Result_Type__Name_Error"
        );
        assert_eq!(sanitize_name_token("___"), "Type");
    }
}
