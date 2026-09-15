//! Rvalue dispatch and expression-scoped lowering state.
use super::{
    super::{
        jvm_names,
        operand::{const_eval, convert_operand, get_placeholder_operand},
        place::{
            coroutine_saved_field_name, emit_instructions_to_get_on_own, emit_pointer_read,
            emit_pointer_slice_parts, emit_retyped_slice_data_pointer, emit_slice_view,
            get_place_type, place_to_string,
        },
        types::{
            ENUM_UNION_DISCRIMINANT_METHOD, adapt_simple_enum_operand,
            ensure_exact_transmute_helper, ensure_fn_ptr_interface, ensure_union_data_type,
            enum_union_discriminant_supported, fn_ptr_signature_from_ty, force_define_named_adt,
            generate_adt_jvm_class_name, jvm_subtype_payload_ty, should_define_named_data_type,
            ty_to_oomir_type, union_from_method_name,
        },
    },
    checked_ops::emit_checked_arithmetic_oomir_instructions,
    oomir::{self, DataTypeMethod},
    trait_objects::{carrier_needs_trait_object_adapter, ensure_trait_object_adapter_class},
};
use crate::lower1::context::Definitions;
use rustc_abi::FieldIdx;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use rustc_middle::{
    mir::{
        BinOp, Body, BorrowKind as MirBorrowKind, CastKind, Operand as MirOperand, Place,
        ProjectionElem, Rvalue, UnOp,
    },
    ty::{
        EarlyBinder, Instance, InstanceKind, Ty, TyCtxt, TyKind, TypingEnv,
        adjustment::PointerCoercion,
    },
};

mod pointer_layout;
use pointer_layout::*;
mod struct_tails;
use struct_tails::*;
mod unsize;
use unsize::*;
mod array_views;
use array_views::*;
mod addressing;
use addressing::*;
mod aggregates;
use aggregates::*;
mod function_pointers;
pub(crate) use function_pointers::*;
mod closures;
pub(crate) use closures::*;
mod arithmetic;
mod borrows;
mod casts;
mod values;

struct RvalueContext<'b, 'tcx> {
    original_dest_place: &'b Place<'tcx>,
    mir: &'b Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &'b mut Definitions<'tcx>,
    external_interfaces: &'b mut HashSet<String>,
    pointer_origins: &'b crate::lower1::control_flow::MutableBorrowMap<'tcx>,
    available_pointer_locals: &'b HashSet<rustc_middle::mir::Local>,
}

fn generate_temp_var_name(data_types: &mut Definitions<'_>, base_name: &str) -> String {
    let count = data_types.next_temporary();
    format!("{}_tmp{}", jvm_names::member_name(base_name), count)
}

impl<'tcx> RvalueContext<'_, 'tcx> {
    fn lower_fallback(self, rvalue: &Rvalue<'tcx>) -> (Vec<oomir::Instruction>, oomir::Operand) {
        let Self {
            original_dest_place,
            mir,
            tcx,
            instance,
            data_types,
            ..
        } = self;
        let instructions = Vec::new();
        let result_operand;
        match rvalue {
            _ => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "mir-lowering",
                    format!(
                        "Warning: Unhandled Rvalue: {:?} for temp based on {:?}. Emitting placeholder.",
                        rvalue, original_dest_place
                    )
                );
                result_operand =
                    get_placeholder_operand(original_dest_place, mir, tcx, instance, data_types);
                // No instructions needed to "calculate" a placeholder
            }
        }
        (instructions, result_operand)
    }
}

/// Evaluates an Rvalue and returns the resulting OOMIR Operand and any
/// intermediate instructions needed to calculate it.
///
/// The `original_dest_place` is used *only* for naming temporary variables
/// to make debugging easier, not for the final assignment.
pub(super) fn convert_rvalue_to_operand<'a>(
    rvalue: &Rvalue<'a>,
    original_dest_place: &Place<'a>, // Used for naming temps
    mir: &Body<'a>,
    tcx: TyCtxt<'a>,
    instance: Instance<'a>,
    data_types: &mut Definitions<'a>,
    external_interfaces: &mut HashSet<String>,
    pointer_origins: &super::MutableBorrowMap<'a>,
    available_pointer_locals: &HashSet<rustc_middle::mir::Local>,
) -> (Vec<oomir::Instruction>, oomir::Operand) {
    breadcrumbs::log!(
        breadcrumbs::LogLevel::Info,
        "mir-lowering",
        format!("lowering rvalue {rvalue:?} into {original_dest_place:?}")
    );
    let context = RvalueContext {
        original_dest_place,
        mir,
        tcx,
        instance,
        data_types,
        external_interfaces,
        pointer_origins,
        available_pointer_locals,
    };
    match rvalue {
        Rvalue::Use(..) => context.lower_values(rvalue),
        Rvalue::Repeat(..) => context.lower_values(rvalue),
        Rvalue::Ref(..) => context.lower_borrows(rvalue),
        Rvalue::Cast(..) => context.lower_casts(rvalue),
        Rvalue::BinaryOp(..) => context.lower_arithmetic(rvalue),
        Rvalue::UnaryOp(..) => context.lower_arithmetic(rvalue),
        Rvalue::Aggregate(..) => context.lower_aggregates(rvalue),
        Rvalue::RawPtr(..) => context.lower_borrows(rvalue),
        Rvalue::Discriminant(..) => context.lower_values(rvalue),
        Rvalue::CopyForDeref(..) => context.lower_values(rvalue),
        _ => context.lower_fallback(rvalue),
    }
}
