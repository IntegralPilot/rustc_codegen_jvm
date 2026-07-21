use super::{
    jvm_names,
    operand::convert_operand,
    place::{
        emit_instructions_to_get_on_own, emit_instructions_to_set_value, emit_pointer_read,
        place_to_string,
    },
    types::mir_int_to_oomir_const,
};
use crate::oomir;

use rustc_hir::def::DefKind;
use rustc_middle::{
    mir::{
        BasicBlock, BasicBlockData, Body, Local, Location, NonDivergingIntrinsic,
        Operand as MirOperand, Place, SourceInfo, StatementKind, TerminatorKind, UnwindAction,
        visit::{PlaceContext, Visitor},
    },
    ty::{EarlyBinder, Instance, InstanceKind, ShimKind, Ty, TyCtxt, TyKind, TypingEnv},
};
use rustc_span::{Symbol, sym};
use std::collections::{HashMap, HashSet};

mod checked_intrinsic_registry;
pub mod checked_intrinsics;
mod checked_ops;
pub(crate) mod rvalue;
mod trait_objects;

pub use checked_intrinsic_registry::take_needed_intrinsics;

fn is_core_ptr_metadata_api(tcx: TyCtxt<'_>, def_id: rustc_span::def_id::DefId) -> bool {
    let metadata = Symbol::intern("metadata");
    let Some(metadata_module) = tcx.opt_parent(def_id) else {
        return false;
    };
    let Some(ptr_module) = tcx.opt_parent(metadata_module) else {
        return false;
    };
    tcx.crate_name(def_id.krate) == sym::core
        && tcx.opt_item_name(def_id) == Some(metadata)
        && tcx.opt_item_name(metadata_module) == Some(metadata)
        && tcx.opt_item_name(ptr_module) == Some(sym::ptr)
}

fn caller_location_operand<'tcx>(
    source_info: SourceInfo,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_prefix: &str,
) -> oomir::Operand {
    let location_ty = tcx.caller_location_ty();
    let location_oomir_ty = super::types::ty_to_oomir_type(location_ty, tcx, data_types, instance);
    let inherited_location =
        instance
            .def
            .requires_caller_location(tcx)
            .then(|| oomir::Operand::Variable {
                name: oomir::CALLER_LOCATION_PARAM_NAME.to_string(),
                ty: location_oomir_ty,
            });

    mir.caller_location_span(source_info, inherited_location, tcx, |span| {
        let raw_location = super::operand::handle_const_value(
            None,
            tcx.span_as_caller_location(span),
            &location_ty,
            tcx,
            data_types,
            instance,
        );
        super::value_repr::adapt_operand_to_rust_type(
            raw_location,
            location_ty,
            temp_prefix,
            tcx,
            instance,
            data_types,
            instructions,
        )
    })
}

fn atomic_value_as_bits(
    value: oomir::Operand,
    temp_name: &str,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let value_ty = value
        .get_type()
        .expect("Rust atomic value must have a JVM carrier type");
    if matches!(value_ty, oomir::Type::Pointer(_)) {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "address".to_string(),
            method_ty: oomir::Signature {
                params: vec![("pointer".to_string(), value_ty)],
                ret: Box::new(oomir::Type::U64),
                is_static: true,
            },
            args: vec![value],
            dest: Some(temp_name.to_string()),
        });
    } else {
        instructions.push(oomir::Instruction::Cast {
            op: value,
            ty: oomir::Type::U64,
            dest: temp_name.to_string(),
        });
    }
    oomir::Operand::Variable {
        name: temp_name.to_string(),
        ty: oomir::Type::U64,
    }
}

fn emit_atomic_value_from_bits<'tcx>(
    bits: oomir::Operand,
    atomic_ty: Ty<'tcx>,
    atomic_oomir_ty: &oomir::Type,
    dest: String,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    if let TyKind::RawPtr(pointee, _) = atomic_ty.kind() {
        let view_size = super::types::layout_size_bytes(tcx, *pointee).unwrap_or_else(|error| {
            panic!("could not determine atomic pointer target size for {pointee:?}: {error}")
        });
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "fromAddress".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("address".to_string(), oomir::Type::U64),
                    ("view_size".to_string(), oomir::Type::U64),
                    ("view_codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(atomic_oomir_ty.clone()),
                is_static: true,
            },
            args: vec![
                bits,
                oomir::Operand::Constant(oomir::Constant::U64(
                    u64::try_from(view_size).expect("Rust atomic pointee layout exceeds u64"),
                )),
                super::types::pointer_view_codec_operand(*pointee, tcx, data_types, instance),
            ],
            dest: Some(dest),
        });
    } else {
        instructions.push(oomir::Instruction::Cast {
            op: bits,
            ty: atomic_oomir_ty.clone(),
            dest,
        });
    }
}

fn atomic_ordering_code(instance: Instance<'_>, const_index: usize) -> i32 {
    let ordering = instance.args.const_at(const_index).to_value();
    let discriminant = ordering
        .to_branch()
        .first()
        .expect("Rust atomic ordering must have an enum discriminant")
        .to_leaf()
        .to_i32();
    assert!(
        (0..=4).contains(&discriminant),
        "unknown Rust atomic ordering discriminant {discriminant}"
    );
    discriminant
}

fn atomic_ordering_operand(instance: Instance<'_>, const_index: usize) -> oomir::Operand {
    oomir::Operand::Constant(oomir::Constant::I32(atomic_ordering_code(
        instance,
        const_index,
    )))
}

#[allow(clippy::too_many_arguments)]
fn emit_atomic_intrinsic<'tcx>(
    intrinsic_name: &str,
    intrinsic_instance: Instance<'tcx>,
    caller_instance: Instance<'tcx>,
    oomir_operands: &[oomir::Operand],
    oomir_output_type: &oomir::Type,
    effective_dest: Option<String>,
    label: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> bool {
    if matches!(intrinsic_name, "atomic_fence" | "atomic_singlethreadfence") {
        let ordering = atomic_ordering_operand(intrinsic_instance, 0);
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "atomicFence".to_string(),
            method_ty: oomir::Signature {
                params: vec![("ordering".to_string(), oomir::Type::I32)],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![ordering],
            dest: None,
        });
        return true;
    }

    let runtime_method = match intrinsic_name {
        "atomic_load" => "atomicLoad",
        "atomic_store" => "atomicStore",
        "atomic_xchg" => "atomicExchange",
        "atomic_xadd" => "atomicAdd",
        "atomic_xsub" => "atomicSubtract",
        "atomic_and" => "atomicAnd",
        "atomic_nand" => "atomicNand",
        "atomic_or" => "atomicOr",
        "atomic_xor" => "atomicXor",
        "atomic_max" => "atomicMax",
        "atomic_min" => "atomicMin",
        "atomic_umax" => "atomicUnsignedMax",
        "atomic_umin" => "atomicUnsignedMin",
        "atomic_cxchg" | "atomic_cxchgweak" => "atomicCompareExchange",
        _ => return false,
    };

    let atomic_ty = intrinsic_instance.args.type_at(0);
    let atomic_oomir_ty =
        super::types::ty_to_oomir_type(atomic_ty, tcx, data_types, caller_instance);
    let byte_count = super::types::layout_size_bytes(tcx, atomic_ty).unwrap_or_else(|error| {
        panic!("could not determine atomic scalar size for {atomic_ty:?}: {error}")
    });
    let byte_count = oomir::Operand::Constant(oomir::Constant::I32(
        i32::try_from(byte_count).expect("Rust atomic scalar width exceeds i32"),
    ));
    let pointer = oomir_operands
        .first()
        .cloned()
        .expect("Rust memory atomic requires a destination pointer");
    let pointer_ty = pointer
        .get_type()
        .expect("Rust atomic destination pointer must be typed");
    let ordering_const_index = if matches!(
        intrinsic_name,
        "atomic_xadd" | "atomic_xsub" | "atomic_and" | "atomic_nand" | "atomic_or" | "atomic_xor"
    ) {
        2
    } else {
        1
    };
    let ordering = atomic_ordering_operand(intrinsic_instance, ordering_const_index);

    if intrinsic_name == "atomic_load" {
        let raw_dest = format!("{label}_atomic_bits");
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: runtime_method.to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), pointer_ty),
                    ("byte_count".to_string(), oomir::Type::I32),
                    ("ordering".to_string(), oomir::Type::I32),
                ],
                ret: Box::new(oomir::Type::U64),
                is_static: true,
            },
            args: vec![pointer, byte_count, ordering],
            dest: Some(raw_dest.clone()),
        });
        emit_atomic_value_from_bits(
            oomir::Operand::Variable {
                name: raw_dest,
                ty: oomir::Type::U64,
            },
            atomic_ty,
            &atomic_oomir_ty,
            effective_dest.expect("atomic_load returns a value"),
            tcx,
            caller_instance,
            data_types,
            instructions,
        );
        return true;
    }

    if intrinsic_name == "atomic_store" {
        let value = atomic_value_as_bits(
            oomir_operands
                .get(1)
                .cloned()
                .expect("atomic_store requires a value"),
            &format!("{label}_atomic_store_bits"),
            instructions,
        );
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: runtime_method.to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), pointer_ty),
                    ("value".to_string(), oomir::Type::U64),
                    ("byte_count".to_string(), oomir::Type::I32),
                    ("ordering".to_string(), oomir::Type::I32),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: vec![pointer, value, byte_count, ordering],
            dest: None,
        });
        return true;
    }

    if matches!(intrinsic_name, "atomic_cxchg" | "atomic_cxchgweak") {
        let expected_value = oomir_operands
            .get(1)
            .cloned()
            .expect("atomic compare/exchange requires an expected value");
        let expected_bits = atomic_value_as_bits(
            expected_value.clone(),
            &format!("{label}_atomic_expected_bits"),
            instructions,
        );
        let replacement_bits = atomic_value_as_bits(
            oomir_operands
                .get(2)
                .cloned()
                .expect("atomic compare/exchange requires a replacement value"),
            &format!("{label}_atomic_replacement_bits"),
            instructions,
        );
        let old_bits_name = format!("{label}_atomic_old_bits");
        let failure_ordering = atomic_ordering_operand(intrinsic_instance, 2);
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: runtime_method.to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), pointer_ty),
                    ("expected".to_string(), oomir::Type::U64),
                    ("value".to_string(), oomir::Type::U64),
                    ("byte_count".to_string(), oomir::Type::I32),
                    ("success_ordering".to_string(), oomir::Type::I32),
                    ("failure_ordering".to_string(), oomir::Type::I32),
                ],
                ret: Box::new(oomir::Type::U64),
                is_static: true,
            },
            args: vec![
                pointer,
                expected_bits.clone(),
                replacement_bits,
                byte_count,
                ordering,
                failure_ordering,
            ],
            dest: Some(old_bits_name.clone()),
        });
        let old_bits = oomir::Operand::Variable {
            name: old_bits_name,
            ty: oomir::Type::U64,
        };
        let old_value_name = format!("{label}_atomic_old_value");
        emit_atomic_value_from_bits(
            old_bits.clone(),
            atomic_ty,
            &atomic_oomir_ty,
            old_value_name.clone(),
            tcx,
            caller_instance,
            data_types,
            instructions,
        );
        let success_name = format!("{label}_atomic_success");
        let compares_pointer_bits = matches!(atomic_ty.kind(), TyKind::RawPtr(..));
        instructions.push(oomir::Instruction::Eq {
            dest: success_name.clone(),
            op1: if compares_pointer_bits {
                old_bits
            } else {
                oomir::Operand::Variable {
                    name: old_value_name.clone(),
                    ty: atomic_oomir_ty.clone(),
                }
            },
            op2: if compares_pointer_bits {
                expected_bits
            } else {
                expected_value
            },
        });
        let tuple_class = oomir_output_type
            .get_class_name()
            .expect("atomic compare/exchange result must be a tuple class")
            .to_string();
        instructions.push(oomir::Instruction::ConstructObject {
            dest: effective_dest.expect("atomic compare/exchange returns a value"),
            class_name: tuple_class,
            args: vec![
                (
                    oomir::Operand::Variable {
                        name: old_value_name,
                        ty: atomic_oomir_ty.clone(),
                    },
                    atomic_oomir_ty,
                ),
                (
                    oomir::Operand::Variable {
                        name: success_name,
                        ty: oomir::Type::Boolean,
                    },
                    oomir::Type::Boolean,
                ),
            ],
        });
        return true;
    }

    let value = atomic_value_as_bits(
        oomir_operands
            .get(1)
            .cloned()
            .expect("atomic read/modify/write requires an operand"),
        &format!("{label}_atomic_operand_bits"),
        instructions,
    );
    let raw_dest = format!("{label}_atomic_old_bits");
    instructions.push(oomir::Instruction::InvokeStatic {
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: runtime_method.to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), pointer_ty),
                ("value".to_string(), oomir::Type::U64),
                ("byte_count".to_string(), oomir::Type::I32),
                ("ordering".to_string(), oomir::Type::I32),
            ],
            ret: Box::new(oomir::Type::U64),
            is_static: true,
        },
        args: vec![pointer, value, byte_count, ordering],
        dest: Some(raw_dest.clone()),
    });
    emit_atomic_value_from_bits(
        oomir::Operand::Variable {
            name: raw_dest,
            ty: oomir::Type::U64,
        },
        atomic_ty,
        &atomic_oomir_ty,
        effective_dest.expect("atomic read/modify/write returns a value"),
        tcx,
        caller_instance,
        data_types,
        instructions,
    );
    true
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct PointerOrigin<'tcx> {
    pub original_place: Place<'tcx>,
    pub carrier_name: String,
    pub pointee_type: oomir::Type,
    pub writable: bool,
}

pub(super) type MutableBorrowMap<'tcx> = HashMap<Local, PointerOrigin<'tcx>>;

#[derive(Default)]
struct DebugLocalCollector {
    locals: HashSet<Local>,
}

impl<'tcx> Visitor<'tcx> for DebugLocalCollector {
    fn visit_local(&mut self, local: Local, context: PlaceContext, _: Location) {
        if !matches!(context, PlaceContext::NonUse(_)) {
            self.locals.insert(local);
        }
    }
}

fn emit_trait_object_metadata(
    pointer: oomir::Operand,
    output_type: &oomir::Type,
    dest: String,
    temp_prefix: &str,
    data_types: &HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    let metadata_class = output_type
        .get_class_name()
        .expect("trait-object metadata must be represented by a JVM class")
        .to_string();
    let metadata_fields = match data_types.get(&metadata_class) {
        Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
        other => panic!("trait-object metadata class is unavailable: {other:?}"),
    };
    let [(_, vtable_type), (_, phantom_type)] = metadata_fields.as_slice() else {
        panic!("trait-object metadata must contain vtable and phantom fields")
    };
    let vtable_class = vtable_type
        .get_class_name()
        .expect("trait-object vtable pointer must be represented by a JVM class")
        .to_string();
    let vtable_fields = match data_types.get(&vtable_class) {
        Some(oomir::DataType::Class { fields, .. }) => fields.clone(),
        other => panic!("trait-object vtable pointer class is unavailable: {other:?}"),
    };
    let [(_, marker_type)] = vtable_fields.as_slice() else {
        panic!("trait-object vtable pointer must contain exactly one pointer field")
    };
    let phantom_class = phantom_type
        .get_class_name()
        .expect("trait-object metadata phantom field must be a JVM class")
        .to_string();

    let marker_name = format!("{temp_prefix}_vtable_marker");
    let pointer_type = pointer
        .get_type()
        .expect("trait-object metadata source must be typed");
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(marker_name.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "traitMetadataMarker".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                ("pointer".to_string(), pointer_type),
                ("metadata_class".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(marker_type.clone()),
            is_static: true,
        },
        args: vec![
            pointer,
            oomir::Operand::Constant(oomir::Constant::String(metadata_class.clone())),
        ],
    });

    let vtable_name = format!("{temp_prefix}_vtable");
    instructions.push(oomir::Instruction::ConstructObject {
        dest: vtable_name.clone(),
        class_name: vtable_class,
        args: vec![(
            oomir::Operand::Variable {
                name: marker_name,
                ty: marker_type.clone(),
            },
            marker_type.clone(),
        )],
    });
    let phantom_name = format!("{temp_prefix}_phantom");
    instructions.push(oomir::Instruction::ConstructObject {
        dest: phantom_name.clone(),
        class_name: phantom_class,
        args: Vec::new(),
    });
    instructions.push(oomir::Instruction::ConstructObject {
        dest,
        class_name: metadata_class,
        args: vec![
            (
                oomir::Operand::Variable {
                    name: vtable_name,
                    ty: vtable_type.clone(),
                },
                vtable_type.clone(),
            ),
            (
                oomir::Operand::Variable {
                    name: phantom_name,
                    ty: phantom_type.clone(),
                },
                phantom_type.clone(),
            ),
        ],
    });
}

pub(super) fn emit_rust_drop_value<'tcx>(
    rust_ty: Ty<'tcx>,
    mut value: oomir::Operand,
    temp_prefix: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    let instantiated = EarlyBinder::bind(tcx, rust_ty).instantiate(tcx, instance.args);
    let rust_ty = tcx
        .try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), instantiated)
        .unwrap_or_else(|_| instantiated.skip_norm_wip());
    let oomir_ty = super::types::ty_to_oomir_type(rust_ty, tcx, data_types, instance);
    if !oomir_ty.has_jvm_value() {
        let Some(materialized) = super::value_repr::materialize_implicit_zst(
            rust_ty,
            &format!("{temp_prefix}_zst"),
            tcx,
            instance,
            data_types,
            instructions,
        ) else {
            return;
        };
        value = materialized;
    }

    match rust_ty.kind() {
        TyKind::Adt(adt_def, substs) if adt_def.is_struct() => {
            if adt_def.destructor(tcx).is_some() {
                let class_name = oomir_ty
                    .get_class_name()
                    .expect("a Rust Drop ADT has a JVM class")
                    .to_string();
                instructions.push(oomir::Instruction::InvokeStatic {
                    class_name,
                    method_name: "drop".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![("self".to_string(), oomir_ty.clone())],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![value.clone()],
                    dest: None,
                });
            }

            for (field_index, field) in adt_def
                .variant(rustc_abi::VariantIdx::from_usize(0))
                .fields
                .iter()
                .enumerate()
            {
                let field_rust_ty = field.ty(tcx, substs).skip_norm_wip();
                if !field_rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    continue;
                }
                let field_oomir_ty =
                    super::types::ty_to_oomir_type(field_rust_ty, tcx, data_types, instance);
                let field_value = if field_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_field_{field_index}");
                    instructions.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: value.clone(),
                        field_name: field.ident(tcx).to_string(),
                        field_ty: field_oomir_ty.clone(),
                        owner_class: oomir_ty
                            .get_class_name()
                            .expect("a struct field owner has a JVM class")
                            .to_string(),
                    });
                    oomir::Operand::Variable {
                        name: dest,
                        ty: field_oomir_ty,
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_rust_drop_value(
                    field_rust_ty,
                    field_value,
                    &format!("{temp_prefix}_field_{field_index}_drop"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                );
            }
        }
        TyKind::Tuple(fields) => {
            for (field_index, field_rust_ty) in fields.iter().enumerate() {
                if !field_rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    continue;
                }
                let field_oomir_ty =
                    super::types::ty_to_oomir_type(field_rust_ty, tcx, data_types, instance);
                let field_value = if field_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_tuple_{field_index}");
                    instructions.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: value.clone(),
                        field_name: format!("field{field_index}"),
                        field_ty: field_oomir_ty.clone(),
                        owner_class: oomir_ty
                            .get_class_name()
                            .expect("a non-empty tuple has a JVM class")
                            .to_string(),
                    });
                    oomir::Operand::Variable {
                        name: dest,
                        ty: field_oomir_ty,
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_rust_drop_value(
                    field_rust_ty,
                    field_value,
                    &format!("{temp_prefix}_tuple_{field_index}_drop"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                );
            }
        }
        TyKind::Closure(_, closure_args) => {
            for (capture_index, capture_rust_ty) in
                closure_args.as_closure().upvar_tys().iter().enumerate()
            {
                if !capture_rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    continue;
                }
                let capture_oomir_ty =
                    super::types::ty_to_oomir_type(capture_rust_ty, tcx, data_types, instance);
                let capture_value = if capture_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_capture_{capture_index}");
                    instructions.push(oomir::Instruction::GetField {
                        dest: dest.clone(),
                        object: value.clone(),
                        field_name: format!("arg{capture_index}"),
                        field_ty: capture_oomir_ty.clone(),
                        owner_class: oomir_ty
                            .get_class_name()
                            .expect("a closure with captures has a JVM class")
                            .to_string(),
                    });
                    oomir::Operand::Variable {
                        name: dest,
                        ty: capture_oomir_ty,
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_rust_drop_value(
                    capture_rust_ty,
                    capture_value,
                    &format!("{temp_prefix}_capture_{capture_index}_drop"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                );
            }
        }
        TyKind::Array(element_ty, length) => {
            let Some(length) = length.try_to_target_usize(tcx) else {
                return;
            };
            if !element_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                return;
            }
            let element_oomir_ty =
                super::types::ty_to_oomir_type(*element_ty, tcx, data_types, instance);
            for index in 0..length {
                let element_value = if element_oomir_ty.has_jvm_value() {
                    let dest = format!("{temp_prefix}_array_{index}");
                    instructions.push(oomir::Instruction::ArrayGet {
                        dest: dest.clone(),
                        array: value.clone(),
                        index: oomir::Operand::Constant(oomir::Constant::I32(index as i32)),
                    });
                    oomir::Operand::Variable {
                        name: dest,
                        ty: element_oomir_ty.clone(),
                    }
                } else {
                    oomir::Operand::Constant(oomir::Constant::Unit)
                };
                emit_rust_drop_value(
                    *element_ty,
                    element_value,
                    &format!("{temp_prefix}_array_{index}_drop"),
                    tcx,
                    instance,
                    data_types,
                    instructions,
                );
            }
        }
        TyKind::Slice(element_ty) => {
            if !element_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                return;
            }
            let drop_instance = Instance::resolve_drop_glue(tcx, *element_ty);
            let target = super::naming::mono_fn_name_from_instance(tcx, drop_instance);
            instructions.push(oomir::Instruction::InvokeStatic {
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "dropSlice".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        (
                            "slice".to_string(),
                            oomir::Type::Class("java/lang/Object".to_string()),
                        ),
                        ("owner".to_string(), oomir::Type::java_string()),
                        ("method".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(oomir::Type::Void),
                    is_static: true,
                },
                args: vec![
                    value,
                    oomir::Operand::Constant(oomir::Constant::String(
                        target.class_to_call_on.expect("drop glue has a JVM owner"),
                    )),
                    oomir::Operand::Constant(oomir::Constant::String(target.method_name)),
                ],
                dest: None,
            });
        }
        TyKind::Dynamic(..) => {
            instructions.push(oomir::Instruction::InvokeStatic {
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "dropRustValue".to_string(),
                method_ty: oomir::Signature {
                    params: vec![(
                        "value".to_string(),
                        oomir::Type::Class("java/lang/Object".to_string()),
                    )],
                    ret: Box::new(oomir::Type::Void),
                    is_static: true,
                },
                args: vec![value],
                dest: None,
            });
        }
        // Rust unions require ManuallyDrop fields. Enums use a virtual helper
        // generated on every variant so the active payload is destroyed without
        // trying to reinterpret the JVM subclass as an inactive variant.
        TyKind::Adt(adt_def, _) if adt_def.is_enum() => {
            let class_name = oomir_ty
                .get_class_name()
                .expect("a Rust enum has a JVM class")
                .to_string();
            if adt_def.destructor(tcx).is_some() {
                instructions.push(oomir::Instruction::InvokeStatic {
                    class_name: class_name.clone(),
                    method_name: "drop".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![("self".to_string(), oomir_ty.clone())],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![value.clone()],
                    dest: None,
                });
            }
            instructions.push(oomir::Instruction::InvokeVirtual {
                class_name,
                method_name: "_rust_drop_fields".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("self".to_string(), oomir_ty)],
                    ret: Box::new(oomir::Type::Void),
                    is_static: false,
                },
                args: Vec::new(),
                dest: None,
                operand: value,
            });
        }
        _ => {}
    }
}

pub(super) fn collect_pointer_origins<'tcx>(
    mir: &Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> MutableBorrowMap<'tcx> {
    let mut origins = MutableBorrowMap::new();
    let assignment_count = mir
        .basic_blocks
        .iter()
        .map(|block| block.statements.len())
        .sum::<usize>();

    for _ in 0..=assignment_count {
        let mut changed = false;
        for block in mir.basic_blocks.iter() {
            for statement in &block.statements {
                let StatementKind::Assign(box (destination, rvalue)) = &statement.kind else {
                    continue;
                };
                if !destination.projection.is_empty() {
                    continue;
                }
                let destination_ty =
                    super::place::get_place_type(destination, mir, tcx, instance, data_types);
                let oomir::Type::Pointer(pointee_ty) = destination_ty else {
                    continue;
                };

                let inherit_deref_origin = |place: &Place<'tcx>| {
                    place
                        .projection
                        .last()
                        .filter(|projection| {
                            matches!(projection, rustc_middle::mir::ProjectionElem::Deref)
                        })
                        .and_then(|_| origins.get(&place.local))
                        .map(|origin| origin.original_place.clone())
                        .unwrap_or_else(|| place.clone())
                };
                let origin = match rvalue {
                    rustc_middle::mir::Rvalue::Ref(_, borrow_kind, place) => Some((
                        inherit_deref_origin(place),
                        pointee_ty.as_ref().clone(),
                        matches!(borrow_kind, rustc_middle::mir::BorrowKind::Mut { .. }),
                    )),
                    rustc_middle::mir::Rvalue::RawPtr(pointer_kind, place) => Some((
                        inherit_deref_origin(place),
                        pointee_ty.as_ref().clone(),
                        matches!(pointer_kind, rustc_middle::mir::RawPtrKind::Mut),
                    )),
                    rustc_middle::mir::Rvalue::Use(
                        MirOperand::Copy(place) | MirOperand::Move(place),
                        _,
                    ) => origins.get(&place.local).map(|entry| {
                        (
                            entry.original_place.clone(),
                            pointee_ty.as_ref().clone(),
                            entry.writable,
                        )
                    }),
                    rustc_middle::mir::Rvalue::Cast(
                        _,
                        MirOperand::Copy(place) | MirOperand::Move(place),
                        _,
                    ) => origins.get(&place.local).map(|entry| {
                        (
                            entry.original_place.clone(),
                            pointee_ty.as_ref().clone(),
                            entry.writable,
                        )
                    }),
                    _ => None,
                };
                let Some((origin, storage_ty, writable)) = origin else {
                    continue;
                };
                let entry = PointerOrigin {
                    original_place: origin,
                    carrier_name: super::place::place_to_string(destination, tcx),
                    pointee_type: storage_ty,
                    writable,
                };
                if origins.get(&destination.local) != Some(&entry) {
                    origins.insert(destination.local, entry);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    origins
}

fn emit_mutable_borrow_writeback<'tcx>(
    borrow_local: Local,
    mutable_borrows: &MutableBorrowMap<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    visited: &mut HashSet<Local>,
) -> Vec<oomir::Instruction> {
    if !visited.insert(borrow_local) {
        return Vec::new();
    }
    let mut instructions = Vec::new();
    let Some(origin) = mutable_borrows.get(&borrow_local).cloned() else {
        return instructions;
    };
    if !origin.writable {
        return instructions;
    }
    let original_place = origin.original_place;
    let carrier_name = origin.carrier_name;
    let carrier_pointee_ty = origin.pointee_type;
    if super::place::local_uses_stable_cell(original_place.local, mir) {
        // The pointer and all projected field views already share the root
        // allocation. Copying a derived/address-only view back would both be
        // redundant and could overwrite the field with the enclosing object.
        return instructions;
    }
    if let Some((rustc_middle::mir::ProjectionElem::Field(_, _), base_projection)) =
        original_place.projection.split_last()
    {
        let base_place = Place {
            local: original_place.local,
            projection: tcx.mk_place_elems(base_projection),
        };
        let base_ty = EarlyBinder::bind(tcx, base_place.ty(&mir.local_decls, tcx).ty)
            .instantiate(tcx, instance.args)
            .skip_norm_wip();
        if matches!(
            base_ty.kind(),
            TyKind::Adt(adt_def, _) if adt_def.is_struct() || adt_def.is_enum()
        ) {
            return instructions;
        }
    }
    let storage_rust_ty = EarlyBinder::bind(tcx, original_place.ty(&mir.local_decls, tcx).ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    let storage_ty = super::types::ty_to_oomir_type(storage_rust_ty, tcx, data_types, instance);
    if !storage_ty.has_jvm_value() {
        return instructions;
    }
    let value_name = format!(
        "_writeback_{}_{}",
        borrow_local.index(),
        original_place.local.index()
    );
    let carrier_ty = mutable_borrows
        .get(&borrow_local)
        .map(|origin| oomir::Type::Pointer(Box::new(origin.pointee_type.clone())))
        .unwrap_or_else(|| oomir::Type::Pointer(Box::new(carrier_pointee_ty.clone())));
    let mut carrier = oomir::Operand::Variable {
        name: carrier_name,
        ty: carrier_ty,
    };
    if carrier_pointee_ty != storage_ty {
        let storage_pointer_ty = oomir::Type::Pointer(Box::new(storage_ty.clone()));
        let retyped_name = format!("{value_name}_storage_pointer");
        instructions.push(oomir::Instruction::InvokeVirtual {
            dest: Some(retyped_name.clone()),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "retype".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("self".to_string(), carrier.get_type().unwrap()),
                    ("size".to_string(), oomir::Type::U64),
                    ("codec".to_string(), oomir::Type::java_string()),
                ],
                ret: Box::new(storage_pointer_ty.clone()),
                is_static: false,
            },
            args: vec![
                oomir::Operand::Constant(oomir::Constant::U64(
                    u64::try_from(
                        super::types::layout_size_bytes(tcx, storage_rust_ty).unwrap_or_else(
                            |error| {
                                panic!("could not determine pointer write-back layout: {error}")
                            },
                        ),
                    )
                    .expect("Rust pointer write-back layout exceeds u64"),
                )),
                super::types::pointer_view_codec_operand(
                    storage_rust_ty,
                    tcx,
                    data_types,
                    instance,
                ),
            ],
            operand: carrier,
        });
        carrier = oomir::Operand::Variable {
            name: retyped_name,
            ty: storage_pointer_ty,
        };
    }
    emit_pointer_read(carrier, &storage_ty, &value_name, &mut instructions);
    instructions.extend(emit_instructions_to_set_value(
        &original_place,
        oomir::Operand::Variable {
            name: value_name,
            ty: storage_ty,
        },
        tcx,
        instance,
        mir,
        data_types,
    ));
    if mutable_borrows.contains_key(&original_place.local) {
        instructions.extend(emit_mutable_borrow_writeback(
            original_place.local,
            mutable_borrows,
            tcx,
            instance,
            mir,
            data_types,
            visited,
        ));
    }
    instructions
}

fn emit_pointer_origin_refreshes<'tcx>(
    updated_place: &Place<'tcx>,
    available_pointer_locals: &HashSet<Local>,
    pointer_origins: &MutableBorrowMap<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> Vec<oomir::Instruction> {
    let matching_origins = available_pointer_locals
        .iter()
        .filter_map(|local| pointer_origins.get(local))
        .filter(|origin| origin.original_place == *updated_place)
        .filter(|origin| !super::place::local_uses_stable_cell(origin.original_place.local, mir))
        .cloned()
        .collect::<Vec<_>>();
    if matching_origins.is_empty() {
        return Vec::new();
    }

    let (value_name, mut instructions, value_type) =
        emit_instructions_to_get_on_own(updated_place, tcx, instance, mir, data_types);
    let updated_rust_ty = EarlyBinder::bind(tcx, updated_place.ty(&mir.local_decls, tcx).ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    for origin in matching_origins {
        let mut carrier = oomir::Operand::Variable {
            name: origin.carrier_name,
            ty: oomir::Type::Pointer(Box::new(origin.pointee_type.clone())),
        };
        if origin.pointee_type != value_type {
            let storage_pointer_ty = oomir::Type::Pointer(Box::new(value_type.clone()));
            let retyped_name = format!("{value_name}_refresh_pointer");
            instructions.push(oomir::Instruction::InvokeVirtual {
                dest: Some(retyped_name.clone()),
                class_name: oomir::POINTER_CLASS.to_string(),
                method_name: "retype".to_string(),
                method_ty: oomir::Signature {
                    params: vec![
                        ("self".to_string(), carrier.get_type().unwrap()),
                        ("size".to_string(), oomir::Type::U64),
                        ("codec".to_string(), oomir::Type::java_string()),
                    ],
                    ret: Box::new(storage_pointer_ty.clone()),
                    is_static: false,
                },
                args: vec![
                    oomir::Operand::Constant(oomir::Constant::U64(
                        u64::try_from(
                            super::types::layout_size_bytes(tcx, updated_rust_ty).unwrap_or_else(
                                |error| {
                                    panic!("could not determine pointer refresh layout: {error}")
                                },
                            ),
                        )
                        .expect("Rust pointer refresh layout exceeds u64"),
                    )),
                    super::types::pointer_view_codec_operand(
                        updated_rust_ty,
                        tcx,
                        data_types,
                        instance,
                    ),
                ],
                operand: carrier,
            });
            carrier = oomir::Operand::Variable {
                name: retyped_name,
                ty: storage_pointer_ty,
            };
        }
        super::place::emit_pointer_write(
            carrier,
            &value_type,
            oomir::Operand::Variable {
                name: value_name.clone(),
                ty: value_type.clone(),
            },
            &mut instructions,
        );
    }
    instructions
}

fn emit_selected_mutable_borrow_writebacks<'tcx>(
    borrow_locals: impl IntoIterator<Item = Local>,
    mutable_borrows: &MutableBorrowMap<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> Vec<oomir::Instruction> {
    let mut instructions = Vec::new();
    let mut visited = HashSet::new();
    for borrow_local in borrow_locals {
        instructions.extend(emit_mutable_borrow_writeback(
            borrow_local,
            mutable_borrows,
            tcx,
            instance,
            mir,
            data_types,
            &mut visited,
        ));
    }
    instructions
}

fn requires_compiled_static_dispatch(ty: &oomir::Type) -> bool {
    if let oomir::Type::MutableReference(inner)
    | oomir::Type::Reference(inner)
    | oomir::Type::Pointer(inner) = ty
    {
        return requires_compiled_static_dispatch(inner);
    }
    matches!(ty, oomir::Type::Unit | oomir::Type::Void)
        || ty.is_jvm_primitive_like()
        || matches!(
            ty,
            oomir::Type::Class(class_name)
                if class_name == crate::lower2::I128_CLASS
                    || class_name == crate::lower2::U128_CLASS
                    || class_name == crate::lower2::F128_CLASS
        )
        || matches!(
            ty,
            oomir::Type::Array(_) | oomir::Type::Slice(_) | oomir::Type::Str
        )
}

fn emit_raw_eq_pointer<'tcx>(
    operand: oomir::Operand,
    compared_ty: Ty<'tcx>,
    temp_name: &str,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    if matches!(operand.get_type(), Some(oomir::Type::Pointer(_))) {
        return operand;
    }

    let element_ty = match compared_ty.kind() {
        TyKind::Array(element, _) | TyKind::Slice(element) => *element,
        TyKind::Str => tcx.types.u8,
        other => panic!("raw_eq received a non-pointer JVM carrier for {other:?}"),
    };
    assert!(
        matches!(
            operand.get_type(),
            Some(oomir::Type::Slice(_) | oomir::Type::Str)
        ),
        "raw_eq slice conversion received unexpected carrier {:?}",
        operand.get_type()
    );
    let element_oomir_ty = super::types::ty_to_oomir_type(element_ty, tcx, data_types, instance);
    let pointer_ty = oomir::Type::Pointer(Box::new(element_oomir_ty));
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(temp_name.to_string()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "fromSlice".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                (
                    "slice".to_string(),
                    oomir::Type::Class("java/lang/Object".to_string()),
                ),
                ("element_size".to_string(), oomir::Type::U64),
                ("codec".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(pointer_ty.clone()),
            is_static: true,
        },
        args: vec![
            operand,
            oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(
                    super::types::layout_size_bytes(tcx, element_ty)
                        .expect("raw_eq element has a concrete layout"),
                )
                .expect("Rust raw_eq element layout exceeds u64"),
            )),
            super::types::pointer_view_codec_operand(element_ty, tcx, data_types, instance),
        ],
    });
    oomir::Operand::Variable {
        name: temp_name.to_string(),
        ty: pointer_ty,
    }
}

fn comparison_value_type<'tcx>(mut ty: oomir::Type, mut mir_ty: Ty<'tcx>) -> oomir::Type {
    while let TyKind::Ref(_, pointee, _) = mir_ty.kind() {
        let oomir::Type::Pointer(inner) = ty else {
            break;
        };
        ty = *inner;
        mir_ty = *pointee;
    }
    ty
}

fn emit_comparison_value<'tcx>(
    mut operand: oomir::Operand,
    mut mir_ty: Ty<'tcx>,
    dest_prefix: &str,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let mut depth = 0;
    while let TyKind::Ref(_, pointee, _) = mir_ty.kind() {
        let Some(oomir::Type::Pointer(inner)) = operand.get_type() else {
            break;
        };
        operand = emit_pointer_read(
            operand,
            inner.as_ref(),
            &format!("{dest_prefix}_{depth}"),
            instructions,
        );
        mir_ty = *pointee;
        depth += 1;
    }
    operand
}

fn supports_direct_equality(ty: &oomir::Type) -> bool {
    matches!(ty, oomir::Type::Unit)
        || ty.is_jvm_primitive_like()
        || matches!(ty, oomir::Type::Pointer(_))
        || matches!(
            ty,
            oomir::Type::Class(class_name)
                if class_name == crate::lower2::I128_CLASS
                    || class_name == crate::lower2::U128_CLASS
                    || class_name == crate::lower2::F128_CLASS
        )
        || matches!(ty, oomir::Type::Str)
}

fn supports_direct_ordering(ty: &oomir::Type) -> bool {
    ty.is_jvm_primitive_like()
        || matches!(ty, oomir::Type::Pointer(_))
        || matches!(
            ty,
            oomir::Type::Class(class_name)
                if class_name == crate::lower2::I128_CLASS
                    || class_name == crate::lower2::U128_CLASS
                    || class_name == crate::lower2::F128_CLASS
        )
}

/// Convert a single MIR basic block into an OOMIR basic block.
pub(super) fn convert_basic_block<'tcx>(
    bb: BasicBlock,
    bb_data: &BasicBlockData<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    return_oomir_type: &oomir::Type, // Pass function return type
    basic_blocks: &mut HashMap<String, oomir::BasicBlock>,
    data_types: &mut HashMap<String, oomir::DataType>,
    mutable_borrow_arrays: &mut MutableBorrowMap<'tcx>,
    debug_variables: &[oomir::DebugVariable],
    debug_variable_scopes: &[rustc_middle::mir::SourceScope],
    debug_scope_cache: &super::DebugScopeCache,
    initially_available_pointer_locals: HashSet<Local>,
) -> oomir::BasicBlock {
    // Use the basic block index as its label.
    let label = format!("bb{}", bb.index());
    let mut instructions = Vec::new();
    let mut initialized_borrows = initially_available_pointer_locals;
    // Convert each MIR statement in the block.
    for (statement_index, stmt) in bb_data.statements.iter().enumerate() {
        let statement_location = Location {
            block: bb,
            statement_index,
        };
        let mut debug_local_collector = DebugLocalCollector::default();
        debug_local_collector.visit_statement(stmt, statement_location);
        let instruction_start = instructions.len();
        match &stmt.kind {
            StatementKind::Assign(box (place, rvalue)) => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!("Assign statement: place={:?}, rvalue={:?}", place, rvalue)
                );
                // 1. Evaluate the Rvalue to get the source operand and temp instructions
                let (rvalue_instructions, source_operand) = rvalue::convert_rvalue_to_operand(
                    // Call the refactored function
                    rvalue,
                    place, // Pass original destination for temp naming hints
                    mir,
                    tcx,
                    instance,
                    data_types,
                    mutable_borrow_arrays,
                    &initialized_borrows,
                );

                // Add instructions needed to calculate the Rvalue
                instructions.extend(rvalue_instructions);

                if let rustc_middle::mir::Rvalue::Ref(_, _, borrowed_place) = rvalue {
                    let dest_ty = place.ty(&mir.local_decls, tcx).ty;
                    let borrowed_is_trait_object = matches!(dest_ty.kind(), TyKind::Ref(..))
                        && matches!(source_operand.get_type(), Some(oomir::Type::Interface(_)));
                    if borrowed_is_trait_object {
                        // Trait objects do not use the MutableReference array wrapper
                    } else {
                        // Check if the destination is a simple local (most common case for &mut assignment)
                        if place.projection.is_empty() {
                            if let oomir::Operand::Variable {
                                name: array_var_name,
                                ty: array_ty,
                            } = &source_operand
                            {
                                match array_ty {
                                    oomir::Type::Pointer(element_ty) => {
                                        breadcrumbs::log!(
                                            breadcrumbs::LogLevel::Info,
                                            "mir-lowering",
                                            format!(
                                                "Info: Tracking mutable borrow array for place {:?} stored in local {:?}. Original: {:?}, ArrayVar: {}, ElementTy: {:?}",
                                                place,
                                                place.local,
                                                borrowed_place,
                                                array_var_name,
                                                element_ty
                                            )
                                        );
                                        mutable_borrow_arrays.insert(
                                            place.local,
                                            PointerOrigin {
                                                original_place: borrowed_place.clone(),
                                                carrier_name: place_to_string(place, tcx),
                                                pointee_type: *element_ty.clone(),
                                                writable: matches!(
                                                    rvalue,
                                                    rustc_middle::mir::Rvalue::Ref(
                                                        _,
                                                        rustc_middle::mir::BorrowKind::Mut { .. },
                                                        _
                                                    )
                                                ),
                                            },
                                        );
                                        initialized_borrows.insert(place.local);
                                    }
                                    oomir::Type::Slice(_) | oomir::Type::Str => {
                                        // Slice views already alias their backing array, so writes
                                        // are visible directly and need no copy-out bookkeeping.
                                        // Utf8View has the same canonical aliasing behaviour for
                                        // `str` references.
                                    }
                                    _ => {
                                        breadcrumbs::log!(
                                            breadcrumbs::LogLevel::Warn,
                                            "mir-lowering",
                                            format!(
                                                "Warning: Expected mutable-reference or slice representation, found {:?}",
                                                array_ty
                                            )
                                        );
                                    }
                                }
                            } else {
                                breadcrumbs::log!(
                                    breadcrumbs::LogLevel::Warn,
                                    "mir-lowering",
                                    format!(
                                        "Warning: Expected variable operand for mutable borrow ref assignment result, found {:?}",
                                        source_operand
                                    )
                                );
                            }
                        } else {
                            breadcrumbs::log!(
                                breadcrumbs::LogLevel::Warn,
                                "mir-lowering",
                                format!(
                                    "Warning: Mutable borrow assigned to complex place {:?}, write-back might not work correctly.",
                                    place
                                )
                            );
                        }
                    }
                }

                if let rustc_middle::mir::Rvalue::RawPtr(
                    rustc_middle::mir::RawPtrKind::Const | rustc_middle::mir::RawPtrKind::Mut,
                    pointed_place,
                ) = rvalue
                    && place.projection.is_empty()
                    && let oomir::Operand::Variable {
                        name: _,
                        ty: oomir::Type::Pointer(element_ty),
                    } = &source_operand
                {
                    let inherited_origin = pointed_place
                        .projection
                        .last()
                        .filter(|projection| {
                            matches!(projection, rustc_middle::mir::ProjectionElem::Deref)
                        })
                        .and_then(|_| mutable_borrow_arrays.get(&pointed_place.local))
                        .map(|origin| origin.original_place.clone());
                    mutable_borrow_arrays.insert(
                        place.local,
                        PointerOrigin {
                            original_place: inherited_origin
                                .unwrap_or_else(|| pointed_place.clone()),
                            // The assigned local is the stable carrier across
                            // block boundaries, not the rvalue temporary.
                            carrier_name: place_to_string(place, tcx),
                            pointee_type: element_ty.as_ref().clone(),
                            writable: matches!(
                                rvalue,
                                rustc_middle::mir::Rvalue::RawPtr(
                                    rustc_middle::mir::RawPtrKind::Mut,
                                    _
                                )
                            ),
                        },
                    );
                    initialized_borrows.insert(place.local);
                }

                // 2. Generate instructions to store the computed value into the destination place
                let assignment_instructions = emit_instructions_to_set_value(
                    place,          // The actual destination Place
                    source_operand, // The OOMIR operand holding the value from the Rvalue
                    tcx,
                    instance,
                    mir,
                    data_types,
                );

                // Add the final assignment instructions (Move, SetField, ArrayStore)
                instructions.extend(assignment_instructions);
                instructions.extend(emit_pointer_origin_refreshes(
                    place,
                    &initialized_borrows,
                    mutable_borrow_arrays,
                    tcx,
                    instance,
                    mir,
                    data_types,
                ));
                if place.projection.iter().any(|projection| {
                    matches!(projection, rustc_middle::mir::ProjectionElem::Deref)
                }) {
                    instructions.extend(emit_selected_mutable_borrow_writebacks(
                        [place.local],
                        mutable_borrow_arrays,
                        tcx,
                        instance,
                        mir,
                        data_types,
                    ));
                }
            }
            StatementKind::StorageLive(_) | StatementKind::StorageDead(_) => {
                // no-op, currently
            }
            StatementKind::Nop | StatementKind::ConstEvalCounter => {
                // Literally a no-op
            }
            StatementKind::Intrinsic(box NonDivergingIntrinsic::Assume(operand)) => {
                // `assume(false)` is UB, so a valid program never needs a JVM-side
                // branch here. Still lower the operand so any place projection is
                // evaluated consistently with other MIR operands.
                let _ = convert_operand(operand, tcx, instance, mir, data_types, &mut instructions);
            }
            StatementKind::Intrinsic(box NonDivergingIntrinsic::CopyNonOverlapping(copy)) => {
                let source =
                    convert_operand(&copy.src, tcx, instance, mir, data_types, &mut instructions);
                let destination =
                    convert_operand(&copy.dst, tcx, instance, mir, data_types, &mut instructions);
                let count = convert_operand(
                    &copy.count,
                    tcx,
                    instance,
                    mir,
                    data_types,
                    &mut instructions,
                );
                let source_rust_ty = copy.src.ty(&mir.local_decls, tcx);
                let pointee = match source_rust_ty.kind() {
                    TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
                    other => {
                        panic!("copy_nonoverlapping MIR statement has non-pointer source {other:?}")
                    }
                };
                let (method_name, count) = if let Ok(element_size) =
                    super::types::layout_size_bytes(tcx, pointee)
                {
                    let byte_count_name = format!("{label}_copy_nonoverlapping_bytes");
                    instructions.push(oomir::Instruction::Mul {
                        dest: byte_count_name.clone(),
                        op1: count,
                        op2: oomir::Operand::Constant(oomir::Constant::U64(element_size as u64)),
                    });
                    (
                        "copyNonOverlapping".to_string(),
                        oomir::Operand::Variable {
                            name: byte_count_name,
                            ty: oomir::Type::U64,
                        },
                    )
                } else {
                    // Generic `core` bodies can retain `T` here. Pointer values carry
                    // their concrete view size, so defer the sizeof(T) multiplication
                    // to the runtime in that case.
                    ("copyNonOverlappingElements".to_string(), count)
                };
                let source_ty = source
                    .get_type()
                    .expect("copy_nonoverlapping source is typed");
                let destination_ty = destination
                    .get_type()
                    .expect("copy_nonoverlapping destination is typed");
                instructions.push(oomir::Instruction::InvokeStatic {
                    class_name: oomir::POINTER_CLASS.to_string(),
                    method_name,
                    method_ty: oomir::Signature {
                        params: vec![
                            ("source".to_string(), source_ty),
                            ("destination".to_string(), destination_ty),
                            ("byte_count".to_string(), oomir::Type::U64),
                        ],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![source, destination, count],
                    dest: None,
                });
            }
            StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => {
                let enum_ty = EarlyBinder::bind(tcx, place.ty(&mir.local_decls, tcx).ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                if matches!(enum_ty.kind(), TyKind::Coroutine(..)) {
                    let (object_name, object_instructions, object_ty) =
                        emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
                    instructions.extend(object_instructions);
                    let oomir::Type::Class(owner_class) = object_ty else {
                        panic!("coroutine discriminant target is not a JVM class");
                    };
                    instructions.push(oomir::Instruction::SetField {
                        object: object_name,
                        field_name: "__state".to_string(),
                        field_ty: oomir::Type::I32,
                        value: oomir::Operand::Constant(oomir::Constant::I32(
                            variant_index.as_u32() as i32,
                        )),
                        owner_class,
                    });
                } else if let Some(value) = super::value_repr::construct_fieldless_enum_variant(
                    enum_ty,
                    *variant_index,
                    &format!("{}_variant", super::place::place_to_string(place, tcx)),
                    tcx,
                    instance,
                    data_types,
                    &mut instructions,
                ) {
                    instructions.extend(emit_instructions_to_set_value(
                        place, value, tcx, instance, mir, data_types,
                    ));
                } else {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Warn,
                        "mir-lowering",
                        format!(
                            "Warning: SetDiscriminant for data-carrying or non-enum place is unsupported. Place: {:?}, Index: {:?}",
                            place, variant_index
                        )
                    );
                }
            }
            // Handle other StatementKind variants if necessary
            _ => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "mir-lowering",
                    format!("Warning: Unhandled StatementKind: {:?}", stmt.kind)
                );
            }
        }
        if instructions.len() > instruction_start {
            let mut metadata = Vec::new();
            if let Some(location) = super::source_location(tcx, mir.span, stmt.source_info.span) {
                metadata.push(oomir::Instruction::SourceLocation(location));
            }
            metadata.push(super::local_variable_scope(
                debug_scope_cache,
                stmt.source_info.scope,
                &debug_local_collector.locals,
                debug_variables,
                debug_variable_scopes,
            ));
            instructions.splice(instruction_start..instruction_start, metadata);
        }
    }

    // Convert the MIR terminator into corresponding OOMIR instructions.
    if let Some(terminator) = &bb_data.terminator {
        let terminator_location = Location {
            block: bb,
            statement_index: bb_data.statements.len(),
        };
        let mut debug_local_collector = DebugLocalCollector::default();
        debug_local_collector.visit_terminator(terminator, terminator_location);
        let instruction_start = instructions.len();
        let unwind_target = match &terminator.kind {
            TerminatorKind::Call {
                unwind: UnwindAction::Cleanup(target),
                ..
            }
            | TerminatorKind::Assert {
                unwind: UnwindAction::Cleanup(target),
                ..
            }
            | TerminatorKind::Drop {
                unwind: UnwindAction::Cleanup(target),
                ..
            } => Some(format!("bb{}", target.index())),
            _ => None,
        };
        if let Some(target) = &unwind_target {
            instructions.push(oomir::Instruction::UnwindStart {
                target: target.clone(),
            });
        }
        match &terminator.kind {
            TerminatorKind::Return => {
                // Handle Return without operand
                if *return_oomir_type == oomir::Type::Void {
                    instructions.push(oomir::Instruction::Return { operand: None });
                } else {
                    let return_operand = convert_operand(
                        &MirOperand::Move(Place::return_place()),
                        tcx,
                        instance,
                        mir,
                        data_types,
                        &mut instructions,
                    );
                    let return_operand = super::value_repr::adapt_operand_to_rust_type(
                        return_operand,
                        Place::return_place().ty(&mir.local_decls, tcx).ty,
                        &format!("{}_return", label),
                        tcx,
                        instance,
                        data_types,
                        &mut instructions,
                    );
                    instructions.push(oomir::Instruction::Return {
                        operand: Some(return_operand),
                    });
                }
            }
            TerminatorKind::Goto { target } => {
                let target_label = format!("bb{}", target.index());
                instructions.push(oomir::Instruction::Jump {
                    target: target_label,
                });
            }
            TerminatorKind::SwitchInt { discr, targets, .. } => {
                let discr_operand =
                    convert_operand(discr, tcx, instance, mir, data_types, &mut instructions);
                let discr_ty = discr.ty(&mir.local_decls, tcx);

                let oomir_targets: Vec<(oomir::Constant, String)> = targets
                    .iter()
                    .map(|(value, target_bb)| {
                        let oomir_const = mir_int_to_oomir_const(value, discr_ty, tcx);
                        if !oomir_const.is_integer_like() {
                            breadcrumbs::log!(breadcrumbs::LogLevel::Warn, "mir-lowering", format!("Warning: SwitchInt target value {:?} for type {:?} cannot be directly used in JVM switch. Block: {}", oomir_const, discr_ty, label));
                        }
                        let target_label = format!("bb{}", target_bb.index());
                        (oomir_const, target_label)
                    })
                    .collect();

                let otherwise_label = format!("bb{}", targets.otherwise().index());

                // Add the single OOMIR Switch instruction
                instructions.push(oomir::Instruction::Switch {
                    discr: discr_operand,
                    targets: oomir_targets,
                    otherwise: otherwise_label,
                });
                // This Switch instruction terminates the current OOMIR basic block.
            }
            TerminatorKind::Call {
                func,
                args,
                destination,
                target,
                ..
            } => {
                let mut pre_call_instructions = Vec::new();
                let mut oomir_operands: Vec<oomir::Operand> = args
                    .iter()
                    .map(|arg| {
                        convert_operand(
                            &arg.node,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut pre_call_instructions,
                        )
                    })
                    .collect();
                instructions.extend(pre_call_instructions);
                for (index, arg) in args.iter().enumerate() {
                    let source = oomir_operands[index].clone();
                    oomir_operands[index] = super::value_repr::adapt_operand_to_rust_type(
                        source,
                        arg.node.ty(mir, tcx),
                        &format!("{}_call_operand_{}", label, index),
                        tcx,
                        instance,
                        data_types,
                        &mut instructions,
                    );
                }

                let destination_oomir_type =
                    super::place::get_place_type(destination, mir, tcx, instance, data_types);
                let destination_pointer_pointee = match &destination_oomir_type {
                    oomir::Type::Pointer(pointee) => Some(pointee.as_ref().clone()),
                    _ => None,
                };
                let deferred_destination_store = !destination.projection.is_empty()
                    || super::place::local_uses_stable_cell(destination.local, mir);
                let dest_var_name = destination_oomir_type.has_jvm_value().then(|| {
                    if deferred_destination_store {
                        format!("{}_call_result", label)
                    } else {
                        format!("_{}", destination.local.index())
                    }
                });

                let typing_env = TypingEnv::post_analysis(tcx, mir.source.def_id());
                let instantiated_func_ty =
                    EarlyBinder::bind(tcx, func.ty(mir, tcx)).instantiate(tcx, instance.args);
                let func_ty = tcx
                    .try_normalize_erasing_regions(typing_env, instantiated_func_ty)
                    .unwrap_or_else(|_| instantiated_func_ty.skip_norm_wip());
                if let rustc_middle::ty::TyKind::FnDef(def_id, substs) = func_ty.kind() {
                    // Resolve the instance
                    let func_instance = rustc_middle::ty::Instance::expect_resolve(
                        tcx,
                        typing_env,
                        *def_id,
                        substs.no_bound_vars().unwrap(),
                        terminator.source_info.span,
                    );

                    let instance_ty = func_instance.ty(tcx, typing_env);
                    let (fn_inputs, fn_output) = match instance_ty.kind() {
                        TyKind::Closure(_, args) => {
                            let sig = args.as_closure().sig();
                            (
                                sig.inputs().skip_binder().to_vec(),
                                sig.output().skip_binder(),
                            )
                        }
                        TyKind::FnDef(..) | TyKind::FnPtr(..) => {
                            let sig = instance_ty.fn_sig(tcx).skip_binder();
                            (sig.inputs().to_vec(), sig.output())
                        }
                        _ => {
                            // Compiler-generated callable bodies have no `FnSig`;
                            // their MIR argument locals and return place define the ABI.
                            let body = tcx.instance_mir(func_instance.def);
                            let inputs = (1..=body.arg_count)
                                .map(|index| {
                                    EarlyBinder::bind(
                                        tcx,
                                        body.local_decls[Local::from_usize(index)].ty,
                                    )
                                    .instantiate(tcx, func_instance.args)
                                    .skip_norm_wip()
                                })
                                .collect();
                            let output =
                                EarlyBinder::bind(tcx, body.local_decls[Local::from_usize(0)].ty)
                                    .instantiate(tcx, func_instance.args)
                                    .skip_norm_wip();
                            (inputs, output)
                        }
                    };

                    if !matches!(instance_ty.kind(), TyKind::Closure(..)) {
                        for (index, target_ty) in fn_inputs.iter().enumerate() {
                            let Some(source) = oomir_operands.get(index).cloned() else {
                                break;
                            };
                            oomir_operands[index] = super::value_repr::adapt_operand_to_rust_type(
                                source,
                                *target_ty,
                                &format!("{}_call_arg_{}", label, index),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                        }
                    }

                    let oomir_output_type =
                        super::types::ty_to_oomir_type(fn_output, tcx, data_types, instance);

                    let effective_dest = if !oomir_output_type.has_jvm_value() {
                        None
                    } else {
                        dest_var_name.clone()
                    };

                    let oomir_input_types: Vec<oomir::Type> = fn_inputs
                        .iter()
                        .map(|ty| super::types::ty_to_oomir_type(*ty, tcx, data_types, instance))
                        .collect();

                    let oomir_params: Vec<(String, oomir::Type)> = oomir_input_types
                        .into_iter()
                        .enumerate()
                        .map(|(i, ty)| (format!("arg{}", i), ty))
                        .collect();

                    let mut method_signature = oomir::Signature {
                        params: oomir_params,
                        ret: Box::new(oomir_output_type.clone()),
                        is_static: false,
                    };

                    if func_instance.def.requires_caller_location(tcx) {
                        let location = caller_location_operand(
                            terminator.source_info,
                            tcx,
                            instance,
                            mir,
                            data_types,
                            &mut instructions,
                            &format!("{label}_caller_location"),
                        );
                        let location_ty = location
                            .get_type()
                            .expect("caller location operand must have a JVM type");
                        method_signature
                            .params
                            .push((oomir::CALLER_LOCATION_PARAM_NAME.to_string(), location_ty));
                        oomir_operands.push(location);
                    }

                    let jvm_import =
                        super::naming::jvm_static_import_from_instance(tcx, func_instance)
                            .unwrap_or_else(|message| {
                                tcx.dcx().span_fatal(terminator.source_info.span, message)
                            });
                    let assoc_item = tcx.opt_associated_item(func_instance.def_id());

                    if let Some(jvm_import) = jvm_import {
                        method_signature.is_static = true;
                        let rust_descriptor =
                            method_signature.to_jvm_descriptor_with_explicit_params();
                        if rust_descriptor != jvm_import.descriptor {
                            tcx.dcx().span_fatal(
                                terminator.source_info.span,
                                format!(
                                    "JVM import descriptor `{}` does not match the lowered Rust signature `{rust_descriptor}`",
                                    jvm_import.descriptor
                                ),
                            );
                        }
                        instructions.push(oomir::Instruction::InvokeStatic {
                            class_name: jvm_import.class_name,
                            method_name: jvm_import.method_name,
                            method_ty: method_signature,
                            args: oomir_operands,
                            dest: effective_dest,
                        });
                    } else if let InstanceKind::Shim(ShimKind::DropGlue(_, drop_ty)) =
                        func_instance.def
                    {
                        if let Some(drop_ty) = drop_ty
                            && drop_ty.needs_drop(tcx, TypingEnv::fully_monomorphized())
                        {
                            let drop_oomir_ty =
                                super::types::ty_to_oomir_type(drop_ty, tcx, data_types, instance);
                            let pointer = oomir_operands[0].clone();
                            let value = if matches!(drop_ty.kind(), TyKind::Slice(_)) {
                                match pointer {
                                    oomir::Operand::Variable { name, .. } => {
                                        oomir::Operand::Variable {
                                            name,
                                            ty: drop_oomir_ty.clone(),
                                        }
                                    }
                                    other => other,
                                }
                            } else if matches!(pointer.get_type(), Some(oomir::Type::Pointer(_))) {
                                let value_name = format!("{label}_drop_glue_value");
                                super::place::emit_pointer_read(
                                    pointer,
                                    &drop_oomir_ty,
                                    &value_name,
                                    &mut instructions,
                                )
                            } else {
                                pointer
                            };
                            emit_rust_drop_value(
                                drop_ty,
                                value,
                                &format!("{label}_drop_glue"),
                                tcx,
                                instance,
                                data_types,
                                &mut instructions,
                            );
                        }
                    } else if matches!(tcx.def_kind(func_instance.def_id()), DefKind::Ctor(..)) {
                        let TyKind::Adt(adt_def, _) = fn_output.kind() else {
                            panic!("constructor returned non-ADT type {fn_output:?}")
                        };
                        if let Some(dest) = effective_dest {
                            let base_class = oomir_output_type
                                .get_class_name()
                                .expect("constructor result has a JVM class");
                            let class_name = if adt_def.is_enum() {
                                let variant_def_id = tcx.parent(func_instance.def_id());
                                format!(
                                    "{}${}",
                                    base_class,
                                    jvm_names::member_name(tcx.item_name(variant_def_id).as_str())
                                )
                            } else {
                                base_class.to_string()
                            };
                            let constructor_args = oomir_operands
                                .into_iter()
                                .filter_map(|operand| {
                                    let operand_ty = operand.get_type()?;
                                    operand_ty.has_jvm_value().then_some((operand, operand_ty))
                                })
                                .collect();
                            instructions.push(oomir::Instruction::ConstructObject {
                                dest,
                                class_name,
                                args: constructor_args,
                            });
                        }
                    } else if let Some(item) = assoc_item {
                        if item.is_method() {
                            let receiver_mir_ty = args[0].node.ty(mir, tcx);
                            let receiver_operand = oomir_operands[0].clone();

                            // Keep the self parameter in the signature. Signature::to_string()
                            // is responsible for omitting the implicit JVM receiver.
                            method_signature.is_static = false;

                            // Separate args for InvokeInterface/InvokeVirtual (receiver handled via 'operand')
                            let method_args = oomir_operands[1..].to_vec();
                            let explicit_method_args =
                                if method_signature.params.last().is_some_and(|(name, _)| {
                                    name == oomir::CALLER_LOCATION_PARAM_NAME
                                }) {
                                    &method_args[..method_args.len() - 1]
                                } else {
                                    &method_args[..]
                                };
                            let method_name = super::naming::associated_method_name_from_instance(
                                tcx,
                                func_instance,
                                &method_signature,
                            );
                            let declared_method_name = item.name().as_str().to_string();

                            if let Some(callable_abi) = super::types::callable_trait_object_abi(
                                receiver_mir_ty,
                                tcx,
                                data_types,
                                instance,
                            ) {
                                let mut flattened_args = Vec::new();
                                if !callable_abi.signature.params.is_empty() {
                                    let tuple_operand = method_args
                                        .first()
                                        .cloned()
                                        .expect("Fn trait calls carry an argument tuple");
                                    let tuple_oomir_ty = tuple_operand
                                        .get_type()
                                        .expect("Fn trait argument tuple is typed");
                                    let tuple_class = tuple_oomir_ty
                                        .get_class_name()
                                        .expect("non-unit Fn argument tuple is a JVM class")
                                        .to_string();
                                    let fields = match data_types.get(&tuple_class) {
                                        Some(oomir::DataType::Class { fields, .. }) => {
                                            fields.clone()
                                        }
                                        _ => panic!(
                                            "Fn argument tuple class {tuple_class} was not defined"
                                        ),
                                    };
                                    for (field_index, (field_name, field_ty)) in
                                        fields.into_iter().enumerate()
                                    {
                                        if !field_ty.has_jvm_value() {
                                            continue;
                                        }
                                        let field_dest =
                                            format!("{label}_callable_arg_{field_index}");
                                        instructions.push(oomir::Instruction::GetField {
                                            dest: field_dest.clone(),
                                            object: tuple_operand.clone(),
                                            field_name,
                                            field_ty: field_ty.clone(),
                                            owner_class: tuple_class.clone(),
                                        });
                                        flattened_args.push(oomir::Operand::Variable {
                                            name: field_dest,
                                            ty: field_ty,
                                        });
                                    }
                                }
                                instructions.push(oomir::Instruction::InvokeInterface {
                                    class_name: callable_abi.interface_name,
                                    method_name: "call".to_string(),
                                    // This descriptor contains only the flattened Java
                                    // arguments; the receiver is carried separately.
                                    method_ty: callable_abi.signature,
                                    args: flattened_args,
                                    dest: effective_dest,
                                    operand: receiver_operand,
                                });
                            } else if let rustc_middle::ty::TyKind::Dynamic(preds, ..) =
                                receiver_mir_ty.kind()
                            {
                                let principal = preds.principal().unwrap().skip_binder();
                                let interface_name = match receiver_operand.get_type() {
                                    Some(oomir::Type::Interface(interface_name)) => interface_name,
                                    _ => jvm_names::class_for_def_id(tcx, principal.def_id),
                                };
                                instructions.push(oomir::Instruction::InvokeInterface {
                                    class_name: interface_name,
                                    method_name: declared_method_name,
                                    method_ty: method_signature,
                                    args: method_args,
                                    dest: effective_dest,
                                    operand: receiver_operand,
                                });
                            } else {
                                // Check if this method is declared in a trait (interface)
                                let trait_container = item.trait_container(tcx);

                                // Check if the receiver operand is an interface type (after any casts)
                                let receiver_oomir_ty = receiver_operand.get_type();
                                let has_concrete_receiver_method = match &receiver_oomir_ty {
                                    Some(oomir::Type::Class(class_name)) => matches!(
                                        data_types.get(class_name),
                                        Some(oomir::DataType::Class { methods, .. })
                                            if methods.contains_key(&method_name)
                                    ),
                                    _ => false,
                                };
                                let uses_concrete_trait_default =
                                    trait_container.is_some_and(|trait_def_id| {
                                        tcx.provided_trait_methods(trait_def_id).any(|provided| {
                                            provided.def_id == item.def_id
                                                || item.trait_item_def_id() == Some(provided.def_id)
                                        })
                                    }) && !has_concrete_receiver_method;

                                // Use InvokeInterface if:
                                // 1. The receiver type is explicitly an Interface type, OR
                                // 2. The method is declared in a trait (which maps to an interface)
                                let use_interface =
                                    if let Some(oomir::Type::Interface(interface_name)) =
                                        &receiver_oomir_ty
                                    {
                                        Some(interface_name.clone())
                                    } else if let Some(trait_def_id) = trait_container
                                        && item.impl_container(tcx).is_none()
                                        && !has_concrete_receiver_method
                                    {
                                        // Get the trait name and convert to interface name
                                        let interface_name =
                                            jvm_names::class_for_def_id(tcx, trait_def_id);
                                        matches!(
                                            data_types.get(&interface_name),
                                            Some(oomir::DataType::Interface { methods })
                                                if methods.contains_key(&declared_method_name)
                                        )
                                        .then_some(interface_name)
                                    } else {
                                        None
                                    };
                                let dispatch_receiver_ty = super::types::ty_to_oomir_type(
                                    receiver_mir_ty,
                                    tcx,
                                    data_types,
                                    instance,
                                );
                                let resolved_receiver_mir_ty =
                                    EarlyBinder::bind(tcx, receiver_mir_ty)
                                        .instantiate(tcx, instance.args)
                                        .skip_norm_wip();
                                let receiver_self_mir_ty = match resolved_receiver_mir_ty.kind() {
                                    TyKind::Ref(_, pointee, _) => *pointee,
                                    _ => resolved_receiver_mir_ty,
                                };
                                let impl_container_mir_ty =
                                    item.impl_container(tcx).map(|impl_def_id| {
                                        tcx.type_of(impl_def_id)
                                            .instantiate(tcx, func_instance.args)
                                            .skip_norm_wip()
                                    });
                                let inherent_container_mir_ty = item
                                    .impl_container(tcx)
                                    .filter(|impl_def_id| {
                                        tcx.impl_opt_trait_ref(*impl_def_id).is_none()
                                    })
                                    .and(impl_container_mir_ty);
                                let trait_impl_self_requires_static_dispatch =
                                    item.impl_container(tcx).is_some_and(|impl_def_id| {
                                        tcx.impl_opt_trait_ref(impl_def_id).is_some()
                                    }) && !has_concrete_receiver_method;
                                let has_arbitrary_self_receiver = inherent_container_mir_ty
                                    .is_some_and(|container_ty| {
                                        container_ty != receiver_self_mir_ty
                                    });
                                let mut receiver_value_mir_ty = receiver_self_mir_ty;
                                while let TyKind::Ref(_, pointee, _) = receiver_value_mir_ty.kind()
                                {
                                    receiver_value_mir_ty = *pointee;
                                }
                                let has_enum_reference_receiver = matches!(
                                    resolved_receiver_mir_ty.kind(),
                                    TyKind::Ref(_, pointee, _)
                                        if matches!(pointee.kind(), TyKind::Adt(adt_def, _)
                                            if adt_def.is_enum())
                                );
                                let method_has_own_generic_params = !tcx
                                    .generics_of(func_instance.def_id())
                                    .own_params
                                    .is_empty();
                                // The class behind a reference implements
                                // methods for its pointee, not blanket Rust
                                // impls whose `Self` is the reference itself
                                // when that pointee has no generated instance
                                // methods (notably closures and function items).
                                let receiver_self_requires_static_dispatch =
                                    inherent_container_mir_ty.is_some()
                                        || has_enum_reference_receiver
                                        || has_arbitrary_self_receiver
                                        || trait_impl_self_requires_static_dispatch
                                        || matches!(
                                            receiver_value_mir_ty.kind(),
                                            TyKind::Closure(..)
                                                | TyKind::FnDef(..)
                                                | TyKind::FnPtr(..)
                                        )
                                        || matches!(
                                            receiver_self_mir_ty.kind(),
                                            TyKind::Ref(..) if method_has_own_generic_params
                                        )
                                        || matches!(
                                            receiver_self_mir_ty.kind(),
                                            TyKind::RawPtr(..) | TyKind::FnPtr(..)
                                        );
                                let pointer_api_receiver = {
                                    let receiver_value_ty = match resolved_receiver_mir_ty.kind() {
                                        TyKind::Ref(_, pointee, _) => *pointee,
                                        _ => resolved_receiver_mir_ty,
                                    };
                                    matches!(receiver_value_ty.kind(), TyKind::RawPtr(..))
                                        || matches!(
                                            receiver_value_ty.kind(),
                                            TyKind::Adt(adt_def, _)
                                                if tcx.is_diagnostic_item(sym::NonNull, adt_def.did())
                                        )
                                };
                                // Upstream core passes the zero-sized `Clone::clone` function
                                // item through `FnOnce` in helpers such as `Option::cloned`.
                                // Lower its single tuple argument directly because a JVM
                                // function-item carrier has no captured receiver state.
                                let direct_fn_def_clone = match resolved_receiver_mir_ty.kind() {
                                    TyKind::FnDef(def_id, _) => tcx
                                        .opt_item_name(*def_id)
                                        .is_some_and(|name| name == sym::clone),
                                    _ => false,
                                } && matches!(
                                    declared_method_name.as_str(),
                                    "call" | "call_mut" | "call_once"
                                ) && explicit_method_args.len() == 1;
                                let is_pointer_cast_method = [
                                    sym::const_ptr_cast,
                                    sym::ptr_cast,
                                    sym::ptr_cast_const,
                                    sym::ptr_cast_mut,
                                ]
                                .into_iter()
                                .any(|diagnostic| {
                                    tcx.is_diagnostic_item(diagnostic, func_instance.def_id())
                                });
                                let is_pointer_null_method = tcx.is_diagnostic_item(
                                    sym::ptr_const_is_null,
                                    func_instance.def_id(),
                                ) || tcx
                                    .is_diagnostic_item(sym::ptr_is_null, func_instance.def_id());
                                let comparison_value_ty = comparison_value_type(
                                    dispatch_receiver_ty.clone(),
                                    resolved_receiver_mir_ty,
                                );
                                let comparison_rhs_ty = oomir_operands
                                    .get(1)
                                    .and_then(oomir::Operand::get_type)
                                    .zip(fn_inputs.get(1).copied())
                                    .map(|(ty, mir_ty)| comparison_value_type(ty, mir_ty));
                                let direct_equality =
                                    matches!(declared_method_name.as_str(), "eq" | "ne")
                                        && supports_direct_equality(&comparison_value_ty)
                                        && comparison_rhs_ty.as_ref() == Some(&comparison_value_ty)
                                        && oomir_operands.len() == 2;
                                let direct_ordering =
                                    matches!(
                                        declared_method_name.as_str(),
                                        "lt" | "le" | "gt" | "ge"
                                    ) && supports_direct_ordering(&comparison_value_ty)
                                        && comparison_rhs_ty.as_ref() == Some(&comparison_value_ty)
                                        && oomir_operands.len() == 2;

                                let direct_wrapping_integer_op = matches!(
                                    declared_method_name.as_str(),
                                    "wrapping_add" | "wrapping_sub" | "wrapping_mul"
                                ) && (matches!(
                                    &dispatch_receiver_ty,
                                    oomir::Type::I8
                                        | oomir::Type::U8
                                        | oomir::Type::I16
                                        | oomir::Type::U16
                                        | oomir::Type::I32
                                        | oomir::Type::U32
                                        | oomir::Type::I64
                                        | oomir::Type::U64
                                ) || matches!(
                                    &dispatch_receiver_ty,
                                    oomir::Type::Class(class_name)
                                        if class_name == crate::lower2::I128_CLASS
                                            || class_name == crate::lower2::U128_CLASS
                                )) && oomir_operands.len() == 2;

                                let direct_overflowing_integer_op = matches!(
                                    declared_method_name.as_str(),
                                    "overflowing_add" | "overflowing_sub" | "overflowing_mul"
                                ) && (matches!(
                                    &dispatch_receiver_ty,
                                    oomir::Type::I8
                                        | oomir::Type::U8
                                        | oomir::Type::I16
                                        | oomir::Type::U16
                                        | oomir::Type::I32
                                        | oomir::Type::U32
                                        | oomir::Type::I64
                                        | oomir::Type::U64
                                ) || matches!(
                                    &dispatch_receiver_ty,
                                    oomir::Type::Class(class_name)
                                        if class_name == crate::lower2::I128_CLASS
                                            || class_name == crate::lower2::U128_CLASS
                                )) && oomir_operands.len() == 2;

                                let direct_f16_to_bits = dispatch_receiver_ty == oomir::Type::F16
                                    && declared_method_name == "to_bits"
                                    && oomir_operands.len() == 1;

                                if direct_f16_to_bits {
                                    if let Some(dest) = effective_dest {
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            dest: Some(dest),
                                            class_name: "org/rustlang/runtime/Numbers".to_string(),
                                            method_name: "f16ToBits".to_string(),
                                            method_ty: oomir::Signature {
                                                params: vec![(
                                                    "value".to_string(),
                                                    oomir::Type::F16,
                                                )],
                                                ret: Box::new(oomir::Type::U16),
                                                is_static: true,
                                            },
                                            args: vec![oomir_operands[0].clone()],
                                        });
                                    }
                                } else if direct_overflowing_integer_op {
                                    if let Some(dest) = effective_dest {
                                        let tuple_class = oomir_output_type
                                            .get_class_name()
                                            .expect(
                                                "overflowing integer result must be a tuple class",
                                            )
                                            .to_string();
                                        let operation = declared_method_name
                                            .strip_prefix("overflowing_")
                                            .expect("overflowing operation prefix");
                                        let (generated, pair, _, _) =
                                            checked_ops::emit_checked_arithmetic_oomir_instructions(
                                                &dest,
                                                &oomir_operands[0],
                                                &oomir_operands[1],
                                                &dispatch_receiver_ty,
                                                operation,
                                                instructions.len(),
                                                &tuple_class,
                                            );
                                        instructions.extend(generated);
                                        instructions.push(oomir::Instruction::Move {
                                            dest,
                                            src: oomir::Operand::Variable {
                                                name: pair,
                                                ty: oomir::Type::Class(tuple_class),
                                            },
                                        });
                                    }
                                } else if direct_wrapping_integer_op {
                                    if let Some(dest) = effective_dest {
                                        let op1 = oomir_operands[0].clone();
                                        let op2 = oomir_operands[1].clone();
                                        let instruction = match declared_method_name.as_str() {
                                            "wrapping_add" => {
                                                oomir::Instruction::Add { dest, op1, op2 }
                                            }
                                            "wrapping_sub" => {
                                                oomir::Instruction::Sub { dest, op1, op2 }
                                            }
                                            "wrapping_mul" => {
                                                oomir::Instruction::Mul { dest, op1, op2 }
                                            }
                                            _ => unreachable!(),
                                        };
                                        instructions.push(instruction);
                                    }
                                } else if direct_equality || direct_ordering {
                                    if let Some(dest) = effective_dest {
                                        let left = emit_comparison_value(
                                            oomir_operands[0].clone(),
                                            fn_inputs[0],
                                            &format!("{label}_comparison_left"),
                                            &mut instructions,
                                        );
                                        let right = emit_comparison_value(
                                            oomir_operands[1].clone(),
                                            fn_inputs[1],
                                            &format!("{label}_comparison_right"),
                                            &mut instructions,
                                        );
                                        if matches!(left.get_type(), Some(oomir::Type::Pointer(_)))
                                        {
                                            let pointer_result = if declared_method_name == "ne" {
                                                format!("{dest}_pointer_eq")
                                            } else {
                                                dest.clone()
                                            };
                                            let method_name = match declared_method_name.as_str() {
                                                "eq" | "ne" => "sameAddress",
                                                "lt" => "lessThan",
                                                "le" => "lessOrEqual",
                                                "gt" => "greaterThan",
                                                "ge" => "greaterOrEqual",
                                                _ => unreachable!(),
                                            };
                                            instructions.push(oomir::Instruction::InvokeVirtual {
                                                dest: Some(pointer_result.clone()),
                                                class_name: oomir::POINTER_CLASS.to_string(),
                                                method_name: method_name.to_string(),
                                                method_ty: oomir::Signature {
                                                    params: vec![
                                                        (
                                                            "self".to_string(),
                                                            left.get_type().unwrap(),
                                                        ),
                                                        (
                                                            "other".to_string(),
                                                            right.get_type().unwrap(),
                                                        ),
                                                    ],
                                                    ret: Box::new(oomir::Type::Boolean),
                                                    is_static: false,
                                                },
                                                args: vec![right],
                                                operand: left,
                                            });
                                            if declared_method_name == "ne" {
                                                instructions.push(oomir::Instruction::Not {
                                                    dest,
                                                    src: oomir::Operand::Variable {
                                                        name: pointer_result,
                                                        ty: oomir::Type::Boolean,
                                                    },
                                                });
                                            }
                                        } else {
                                            let instruction = match declared_method_name.as_str() {
                                                "eq" => oomir::Instruction::Eq {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                "ne" => oomir::Instruction::Ne {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                "lt" => oomir::Instruction::Lt {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                "le" => oomir::Instruction::Le {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                "gt" => oomir::Instruction::Gt {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                "ge" => oomir::Instruction::Ge {
                                                    dest,
                                                    op1: left,
                                                    op2: right,
                                                },
                                                _ => unreachable!(),
                                            };
                                            instructions.push(instruction);
                                        }
                                    }
                                } else if direct_fn_def_clone {
                                    if let Some(dest) = effective_dest {
                                        let tuple_operand = explicit_method_args[0].clone();
                                        let tuple_class = tuple_operand
                                            .get_type()
                                            .and_then(|ty| ty.get_class_name().map(str::to_string))
                                            .expect("FnOnce argument tuple must be a JVM class");
                                        let tuple_mir_ty = EarlyBinder::bind(tcx, fn_inputs[1])
                                            .instantiate(tcx, instance.args)
                                            .skip_norm_wip();
                                        let TyKind::Tuple(tuple_fields) = tuple_mir_ty.kind()
                                        else {
                                            panic!(
                                                "FnOnce argument is not a Rust tuple: {tuple_mir_ty:?}"
                                            )
                                        };
                                        let field_mir_ty = tuple_fields[0];
                                        let field_ty = super::types::ty_to_oomir_type(
                                            field_mir_ty,
                                            tcx,
                                            data_types,
                                            instance,
                                        );
                                        let field_name = format!("{label}_fn_def_clone_arg");
                                        instructions.push(oomir::Instruction::GetField {
                                            dest: field_name.clone(),
                                            object: tuple_operand,
                                            field_name: "field0".to_string(),
                                            field_ty: field_ty.clone(),
                                            owner_class: tuple_class,
                                        });
                                        let field = oomir::Operand::Variable {
                                            name: field_name,
                                            ty: field_ty.clone(),
                                        };
                                        if let oomir::Type::Pointer(inner) = field_ty {
                                            super::place::emit_pointer_read(
                                                field,
                                                &inner,
                                                &dest,
                                                &mut instructions,
                                            );
                                        } else {
                                            instructions.push(oomir::Instruction::Move {
                                                dest,
                                                src: field,
                                            });
                                        }
                                    }
                                } else if matches!(
                                    &dispatch_receiver_ty,
                                    oomir::Type::Slice(_) | oomir::Type::Str
                                ) && is_pointer_cast_method
                                    && matches!(oomir_output_type, oomir::Type::Pointer(_))
                                {
                                    let source_pointee = match receiver_mir_ty.kind() {
                                        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                                            if pointee.is_slice() {
                                                pointee.sequence_element_type(tcx)
                                            } else {
                                                tcx.types.u8
                                            }
                                        }
                                        other => panic!(
                                            "fat pointer cast receiver has unexpected type {other:?}"
                                        ),
                                    };
                                    let target_pointee = match fn_output.kind() {
                                        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                                            *pointee
                                        }
                                        other => panic!(
                                            "fat pointer cast returns unexpected type {other:?}"
                                        ),
                                    };
                                    let source_element_size =
                                        super::types::layout_size_bytes(tcx, source_pointee)
                                            .expect("fat pointer element has a concrete layout");
                                    let data_pointer = format!("{label}_fat_cast_data");
                                    instructions.push(oomir::Instruction::InvokeStatic {
                                        dest: Some(data_pointer.clone()),
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "fromSlice".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![
                                                (
                                                    "slice".to_string(),
                                                    oomir::Type::Class(
                                                        "java/lang/Object".to_string(),
                                                    ),
                                                ),
                                                ("element_size".to_string(), oomir::Type::U64),
                                                ("codec".to_string(), oomir::Type::java_string()),
                                            ],
                                            ret: Box::new(oomir_output_type.clone()),
                                            is_static: true,
                                        },
                                        args: vec![
                                            receiver_operand,
                                            oomir::Operand::Constant(oomir::Constant::U64(
                                                u64::try_from(source_element_size).expect(
                                                    "Rust slice element layout exceeds u64",
                                                ),
                                            )),
                                            super::types::pointer_view_codec_operand(
                                                source_pointee,
                                                tcx,
                                                data_types,
                                                instance,
                                            ),
                                        ],
                                    });
                                    instructions.push(oomir::Instruction::InvokeVirtual {
                                        dest: effective_dest,
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "retype".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![
                                                (
                                                    "self".to_string(),
                                                    oomir_output_type.clone(),
                                                ),
                                                ("view_size".to_string(), oomir::Type::U64),
                                                ("view_codec".to_string(), oomir::Type::java_string()),
                                            ],
                                            ret: Box::new(oomir_output_type.clone()),
                                            is_static: false,
                                        },
                                        args: vec![
                                            oomir::Operand::Constant(oomir::Constant::U64(
                                                u64::try_from(
                                                    super::types::layout_size_bytes(
                                                        tcx,
                                                        target_pointee,
                                                    )
                                                    .expect(
                                                        "fat pointer cast target has a concrete layout",
                                                    ),
                                                )
                                                .expect("Rust pointer target layout exceeds u64"),
                                            )),
                                            super::types::pointer_view_codec_operand(
                                                target_pointee,
                                                tcx,
                                                data_types,
                                                instance,
                                            ),
                                        ],
                                        operand: oomir::Operand::Variable {
                                            name: data_pointer,
                                            ty: oomir_output_type.clone(),
                                        },
                                    });
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && matches!(declared_method_name.as_str(), "as_ref" | "as_mut")
                                    && matches!(
                                        fn_output.kind(),
                                        TyKind::Adt(adt_def, _)
                                            if tcx.is_lang_item(
                                                adt_def.did(),
                                                rustc_hir::LangItem::Option,
                                            )
                                    )
                                {
                                    if let Some(dest) = effective_dest {
                                        let option_class = oomir_output_type
                                            .get_class_name()
                                            .expect("pointer as_ref/as_mut returns Option")
                                            .to_string();
                                        let option_object =
                                            format!("{label}_pointer_option_object");
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name: oomir::POINTER_CLASS.to_string(),
                                            method_name: "asRefOption".to_string(),
                                            method_ty: oomir::Signature {
                                                params: vec![
                                                    (
                                                        "pointer".to_string(),
                                                        dispatch_receiver_ty.clone(),
                                                    ),
                                                    (
                                                        "option_class".to_string(),
                                                        oomir::Type::java_string(),
                                                    ),
                                                ],
                                                ret: Box::new(oomir::Type::Class(
                                                    "java/lang/Object".to_string(),
                                                )),
                                                is_static: true,
                                            },
                                            args: vec![
                                                receiver_operand,
                                                oomir::Operand::Constant(oomir::Constant::String(
                                                    option_class,
                                                )),
                                            ],
                                            dest: Some(option_object.clone()),
                                        });
                                        instructions.push(oomir::Instruction::Cast {
                                            op: oomir::Operand::Variable {
                                                name: option_object,
                                                ty: oomir::Type::Class(
                                                    "java/lang/Object".to_string(),
                                                ),
                                            },
                                            ty: oomir_output_type.clone(),
                                            dest,
                                        });
                                    }
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && matches!(declared_method_name.as_str(), "as_ref" | "as_mut")
                                {
                                    // NonNull::as_ref/as_mut return a reference directly, unlike
                                    // raw-pointer methods with the same names which return Option.
                                    if let Some(dest) = effective_dest {
                                        if let oomir::Type::Pointer(non_null_ty) =
                                            &dispatch_receiver_ty
                                            && let oomir::Type::Class(owner_class) =
                                                non_null_ty.as_ref()
                                        {
                                            // NonNull is a transparent wrapper around its pointer
                                            // field. Returning the wrapper's storage pointer would
                                            // make a later dereference observe a NonNull object
                                            // instead of the pointee, so decode the wrapper and
                                            // return the actual field for both sized and DST values.
                                            let wrapper = super::place::emit_pointer_read(
                                                receiver_operand,
                                                non_null_ty,
                                                &format!("{label}_non_null_wrapper"),
                                                &mut instructions,
                                            );
                                            let field_ty = match data_types.get(owner_class) {
                                                Some(oomir::DataType::Class { fields, .. }) => {
                                                    fields
                                                        .iter()
                                                        .find(|(name, _)| name == "pointer")
                                                        .map(|(_, ty)| ty.clone())
                                                        .expect(
                                                            "NonNull carrier has a pointer field",
                                                        )
                                                }
                                                _ => panic!(
                                                    "NonNull carrier class {owner_class} was not generated"
                                                ),
                                            };
                                            let field_dest = if field_ty == oomir_output_type {
                                                dest.clone()
                                            } else {
                                                format!("{label}_non_null_pointer")
                                            };
                                            instructions.push(oomir::Instruction::GetField {
                                                dest: field_dest.clone(),
                                                object: wrapper,
                                                field_name: "pointer".to_string(),
                                                field_ty: field_ty.clone(),
                                                owner_class: owner_class.clone(),
                                            });
                                            if field_ty != oomir_output_type {
                                                let adapted =
                                                    super::value_repr::adapt_operand_to_rust_type(
                                                        oomir::Operand::Variable {
                                                            name: field_dest,
                                                            ty: field_ty,
                                                        },
                                                        fn_output,
                                                        &format!("{label}_non_null_reference"),
                                                        tcx,
                                                        instance,
                                                        data_types,
                                                        &mut instructions,
                                                    );
                                                instructions.push(oomir::Instruction::Move {
                                                    dest,
                                                    src: adapted,
                                                });
                                            }
                                        } else {
                                            instructions.push(oomir::Instruction::Move {
                                                dest,
                                                src: receiver_operand,
                                            });
                                        }
                                    }
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && matches!(
                                        declared_method_name.as_str(),
                                        "as_ref_unchecked" | "as_mut_unchecked"
                                    )
                                {
                                    if let Some(dest) = effective_dest {
                                        instructions.push(oomir::Instruction::Move {
                                            dest,
                                            src: receiver_operand,
                                        });
                                    }
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && declared_method_name == "is_aligned"
                                {
                                    let receiver_ty = EarlyBinder::bind(tcx, receiver_mir_ty)
                                        .instantiate(tcx, instance.args)
                                        .skip_norm_wip();
                                    let pointee = match receiver_ty.kind() {
                                        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                                            *pointee
                                        }
                                        other => panic!(
                                            "pointer is_aligned receiver has non-pointer type {other:?}"
                                        ),
                                    };
                                    let alignment = super::types::layout_align_bytes(tcx, pointee)
                                        .unwrap_or_else(|error| {
                                            panic!(
                                                "could not determine pointer target alignment: {error}"
                                            )
                                        });
                                    instructions.push(oomir::Instruction::InvokeStatic {
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "is_aligned_to".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![
                                                (
                                                    "pointer".to_string(),
                                                    dispatch_receiver_ty.clone(),
                                                ),
                                                ("alignment".to_string(), oomir::Type::U64),
                                            ],
                                            ret: Box::new(oomir::Type::Boolean),
                                            is_static: true,
                                        },
                                        args: vec![
                                            receiver_operand,
                                            oomir::Operand::Constant(oomir::Constant::U64(
                                                u64::try_from(alignment)
                                                    .expect("Rust pointer alignment exceeds u64"),
                                            )),
                                        ],
                                        dest: effective_dest,
                                    });
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && declared_method_name == "to_raw_parts"
                                {
                                    if let Some(dest) = effective_dest {
                                        let tuple_class = oomir_output_type
                                            .get_class_name()
                                            .expect("pointer to_raw_parts returns a tuple")
                                            .to_string();
                                        let tuple_fields = match data_types.get(&tuple_class) {
                                            Some(oomir::DataType::Class { fields, .. }) => {
                                                fields.clone()
                                            }
                                            other => panic!(
                                                "to_raw_parts tuple class is unavailable: {other:?}"
                                            ),
                                        };
                                        let data_pointer_ty = tuple_fields
                                            .first()
                                            .map(|(_, ty)| ty.clone())
                                            .expect("to_raw_parts tuple has a data pointer");
                                        let data_pointer_name = format!("{label}_raw_parts_data");
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name: oomir::POINTER_CLASS.to_string(),
                                            method_name: "retype".to_string(),
                                            method_ty: oomir::Signature {
                                                params: vec![
                                                    (
                                                        "pointer".to_string(),
                                                        dispatch_receiver_ty.clone(),
                                                    ),
                                                    ("view_size".to_string(), oomir::Type::U64),
                                                    (
                                                        "view_codec".to_string(),
                                                        oomir::Type::java_string(),
                                                    ),
                                                ],
                                                ret: Box::new(data_pointer_ty.clone()),
                                                is_static: true,
                                            },
                                            args: vec![
                                                receiver_operand,
                                                oomir::Operand::Constant(oomir::Constant::U64(0)),
                                                oomir::Operand::Constant(oomir::Constant::Null(
                                                    oomir::Type::java_string(),
                                                )),
                                            ],
                                            dest: Some(data_pointer_name.clone()),
                                        });
                                        let mut tuple_args = vec![(
                                            oomir::Operand::Variable {
                                                name: data_pointer_name,
                                                ty: data_pointer_ty.clone(),
                                            },
                                            data_pointer_ty,
                                        )];
                                        for (_, metadata_ty) in tuple_fields.into_iter().skip(1) {
                                            tuple_args.push((
                                                oomir::Operand::Constant(oomir::Constant::Null(
                                                    metadata_ty.clone(),
                                                )),
                                                metadata_ty,
                                            ));
                                        }
                                        instructions.push(oomir::Instruction::ConstructObject {
                                            dest,
                                            class_name: tuple_class,
                                            args: tuple_args,
                                        });
                                    }
                                } else if let oomir::Type::Pointer(pointee_ty) =
                                    &dispatch_receiver_ty
                                    && pointer_api_receiver
                                    && matches!(
                                        declared_method_name.as_str(),
                                        "read" | "read_unaligned" | "read_volatile"
                                    )
                                {
                                    if let Some(dest) = effective_dest {
                                        if declared_method_name == "read_volatile" {
                                            instructions.push(oomir::Instruction::InvokeStatic {
                                                class_name: oomir::POINTER_CLASS.to_string(),
                                                method_name: "volatileFence".to_string(),
                                                method_ty: oomir::Signature {
                                                    params: Vec::new(),
                                                    ret: Box::new(oomir::Type::Void),
                                                    is_static: true,
                                                },
                                                args: Vec::new(),
                                                dest: None,
                                            });
                                        }
                                        super::place::emit_pointer_read_copy(
                                            receiver_operand,
                                            pointee_ty,
                                            &dest,
                                            &mut instructions,
                                        );
                                    }
                                } else if let oomir::Type::Pointer(pointee_ty) =
                                    &dispatch_receiver_ty
                                    && pointer_api_receiver
                                    && matches!(
                                        declared_method_name.as_str(),
                                        "write" | "write_unaligned" | "write_volatile"
                                    )
                                    && explicit_method_args.len() == 1
                                {
                                    super::place::emit_pointer_write(
                                        receiver_operand,
                                        pointee_ty,
                                        explicit_method_args[0].clone(),
                                        &mut instructions,
                                    );
                                    if declared_method_name == "write_volatile" {
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name: oomir::POINTER_CLASS.to_string(),
                                            method_name: "volatileFence".to_string(),
                                            method_ty: oomir::Signature {
                                                params: Vec::new(),
                                                ret: Box::new(oomir::Type::Void),
                                                is_static: true,
                                            },
                                            args: Vec::new(),
                                            dest: None,
                                        });
                                    }
                                } else if let oomir::Type::Pointer(pointee_ty) =
                                    &dispatch_receiver_ty
                                    && pointer_api_receiver
                                    && declared_method_name == "replace"
                                    && explicit_method_args.len() == 1
                                {
                                    if let Some(dest) = effective_dest {
                                        super::place::emit_pointer_read(
                                            receiver_operand.clone(),
                                            pointee_ty,
                                            &dest,
                                            &mut instructions,
                                        );
                                    }
                                    super::place::emit_pointer_write(
                                        receiver_operand,
                                        pointee_ty,
                                        explicit_method_args[0].clone(),
                                        &mut instructions,
                                    );
                                } else if let oomir::Type::Pointer(pointee_ty) =
                                    &dispatch_receiver_ty
                                    && pointer_api_receiver
                                    && declared_method_name == "swap"
                                    && explicit_method_args.len() == 1
                                {
                                    let left_name = format!("{label}_method_swap_left");
                                    let right_name = format!("{label}_method_swap_right");
                                    let left = super::place::emit_pointer_read(
                                        receiver_operand.clone(),
                                        pointee_ty,
                                        &left_name,
                                        &mut instructions,
                                    );
                                    let right = super::place::emit_pointer_read(
                                        explicit_method_args[0].clone(),
                                        pointee_ty,
                                        &right_name,
                                        &mut instructions,
                                    );
                                    super::place::emit_pointer_write(
                                        receiver_operand,
                                        pointee_ty,
                                        right,
                                        &mut instructions,
                                    );
                                    super::place::emit_pointer_write(
                                        explicit_method_args[0].clone(),
                                        pointee_ty,
                                        left,
                                        &mut instructions,
                                    );
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && matches!(
                                        declared_method_name.as_str(),
                                        "copy_to"
                                            | "copy_to_nonoverlapping"
                                            | "copy_from"
                                            | "copy_from_nonoverlapping"
                                            | "write_bytes"
                                    )
                                    && explicit_method_args.len() == 2
                                {
                                    let receiver_ty = EarlyBinder::bind(tcx, receiver_mir_ty)
                                        .instantiate(tcx, instance.args)
                                        .skip_norm_wip();
                                    let pointee = match receiver_ty.kind() {
                                        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                                            *pointee
                                        }
                                        other => panic!(
                                            "pointer memory method receiver has non-pointer type {other:?}"
                                        ),
                                    };
                                    let element_size = super::types::layout_size_bytes(tcx, pointee)
                                        .unwrap_or_else(|error| {
                                            panic!(
                                                "could not determine pointer memory method element size: {error}"
                                            )
                                        });
                                    let byte_count_name =
                                        format!("{label}_{declared_method_name}_byte_count");
                                    instructions.push(oomir::Instruction::Mul {
                                        dest: byte_count_name.clone(),
                                        op1: explicit_method_args[1].clone(),
                                        op2: oomir::Operand::Constant(oomir::Constant::U64(
                                            element_size as u64,
                                        )),
                                    });
                                    let byte_count = oomir::Operand::Variable {
                                        name: byte_count_name,
                                        ty: oomir::Type::U64,
                                    };
                                    if declared_method_name == "write_bytes" {
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name: oomir::POINTER_CLASS.to_string(),
                                            method_name: "writeBytes".to_string(),
                                            method_ty: oomir::Signature {
                                                params: vec![
                                                    (
                                                        "destination".to_string(),
                                                        dispatch_receiver_ty.clone(),
                                                    ),
                                                    ("value".to_string(), oomir::Type::I32),
                                                    ("byte_count".to_string(), oomir::Type::U64),
                                                ],
                                                ret: Box::new(oomir::Type::Void),
                                                is_static: true,
                                            },
                                            args: vec![
                                                receiver_operand,
                                                explicit_method_args[0].clone(),
                                                byte_count,
                                            ],
                                            dest: None,
                                        });
                                    } else {
                                        let from_receiver =
                                            declared_method_name.starts_with("copy_to");
                                        let source = if from_receiver {
                                            receiver_operand.clone()
                                        } else {
                                            explicit_method_args[0].clone()
                                        };
                                        let destination = if from_receiver {
                                            explicit_method_args[0].clone()
                                        } else {
                                            receiver_operand
                                        };
                                        let source_ty = source
                                            .get_type()
                                            .expect("pointer copy source is typed");
                                        let destination_ty = destination
                                            .get_type()
                                            .expect("pointer copy destination is typed");
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name: oomir::POINTER_CLASS.to_string(),
                                            method_name: if declared_method_name
                                                .ends_with("nonoverlapping")
                                            {
                                                "copyNonOverlapping".to_string()
                                            } else {
                                                "copy".to_string()
                                            },
                                            method_ty: oomir::Signature {
                                                params: vec![
                                                    ("source".to_string(), source_ty),
                                                    ("destination".to_string(), destination_ty),
                                                    ("byte_count".to_string(), oomir::Type::U64),
                                                ],
                                                ret: Box::new(oomir::Type::Void),
                                                is_static: true,
                                            },
                                            args: vec![source, destination, byte_count],
                                            dest: None,
                                        });
                                    }
                                } else if matches!(&dispatch_receiver_ty, oomir::Type::Pointer(_))
                                    && pointer_api_receiver
                                    && declared_method_name == "map_addr"
                                    && explicit_method_args.len() == 1
                                {
                                    // `map_addr` is generic over a Rust `FnOnce`. A closure value is
                                    // only its captured environment on the JVM; its body is emitted as
                                    // a static method on the crate module class. Invoke that body here
                                    // and then apply the returned address while retaining provenance.
                                    let mapper_ty =
                                        EarlyBinder::bind(tcx, args[1].node.ty(mir, tcx))
                                            .instantiate(tcx, instance.args)
                                            .skip_norm_wip();
                                    let address_dest = format!("{label}_map_addr_input");
                                    instructions.push(oomir::Instruction::InvokeStatic {
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "addr".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![(
                                                "pointer".to_string(),
                                                dispatch_receiver_ty.clone(),
                                            )],
                                            ret: Box::new(oomir::Type::U64),
                                            is_static: true,
                                        },
                                        args: vec![receiver_operand.clone()],
                                        dest: Some(address_dest.clone()),
                                    });
                                    let mapped_address_dest = format!("{label}_map_addr_output");
                                    let address_operand = oomir::Operand::Variable {
                                        name: address_dest,
                                        ty: oomir::Type::U64,
                                    };
                                    match mapper_ty.kind() {
                                        TyKind::Closure(closure_def_id, closure_args) => {
                                            let closure_instance =
                                                Instance::new_raw(*closure_def_id, *closure_args);
                                            let closure_signature = closure_args.as_closure().sig();
                                            let closure_inputs =
                                                closure_signature.inputs().skip_binder().to_vec();
                                            let closure_arg_ty = *closure_inputs.first().expect(
                                                "FnOnce closure signature has an argument tuple",
                                            );
                                            let closure_arg_oomir_ty =
                                                super::types::ty_to_oomir_type(
                                                    closure_arg_ty,
                                                    tcx,
                                                    data_types,
                                                    instance,
                                                );
                                            let closure_tuple_dest =
                                                format!("{label}_map_addr_closure_tuple");
                                            let closure_tuple_class = closure_arg_oomir_ty
                                                .get_class_name()
                                                .expect(
                                                    "FnOnce closure argument tuple is a JVM class",
                                                )
                                                .to_string();
                                            instructions.push(
                                                oomir::Instruction::ConstructObject {
                                                    dest: closure_tuple_dest.clone(),
                                                    class_name: closure_tuple_class,
                                                    args: vec![(
                                                        address_operand.clone(),
                                                        oomir::Type::U64,
                                                    )],
                                                },
                                            );

                                            let captures = closure_args
                                                .as_closure()
                                                .upvar_tys()
                                                .iter()
                                                .next()
                                                .is_some();
                                            let mut closure_params = closure_inputs
                                                .iter()
                                                .enumerate()
                                                .map(|(index, input)| {
                                                    (
                                                        format!("arg{index}"),
                                                        super::types::ty_to_oomir_type(
                                                            *input, tcx, data_types, instance,
                                                        ),
                                                    )
                                                })
                                                .collect::<Vec<_>>();
                                            let mut closure_call_args =
                                                vec![oomir::Operand::Variable {
                                                    name: closure_tuple_dest,
                                                    ty: closure_arg_oomir_ty,
                                                }];
                                            if captures {
                                                let environment_ty = explicit_method_args[0]
                                                    .get_type()
                                                    .expect(
                                                        "capturing map_addr closure has an environment",
                                                    );
                                                closure_params.insert(
                                                    0,
                                                    ("closure_env".to_string(), environment_ty),
                                                );
                                                closure_call_args
                                                    .insert(0, explicit_method_args[0].clone());
                                            }
                                            instructions.push(oomir::Instruction::InvokeStatic {
                                                class_name: super::naming::mono_owner_class(
                                                    tcx,
                                                    closure_instance,
                                                ),
                                                method_name: super::generate_closure_function_name(
                                                    tcx,
                                                    closure_instance,
                                                ),
                                                method_ty: oomir::Signature {
                                                    params: closure_params,
                                                    ret: Box::new(oomir::Type::U64),
                                                    is_static: true,
                                                },
                                                args: closure_call_args,
                                                dest: Some(mapped_address_dest.clone()),
                                            });
                                        }
                                        TyKind::FnDef(def_id, generic_args) => {
                                            let mapper_instance = Instance::resolve_for_fn_ptr(
                                                tcx,
                                                typing_env,
                                                *def_id,
                                                generic_args.no_bound_vars().unwrap(),
                                            )
                                            .expect("map_addr function item resolves");
                                            let target = super::naming::mono_fn_name_from_instance(
                                                tcx,
                                                mapper_instance,
                                            );
                                            instructions.push(oomir::Instruction::InvokeStatic {
                                                class_name: target.class_to_call_on.expect(
                                                    "map_addr function item has a JVM owner",
                                                ),
                                                method_name: target.method_name,
                                                method_ty: oomir::Signature {
                                                    params: vec![(
                                                        "address".to_string(),
                                                        oomir::Type::U64,
                                                    )],
                                                    ret: Box::new(oomir::Type::U64),
                                                    is_static: true,
                                                },
                                                args: vec![address_operand],
                                                dest: Some(mapped_address_dest.clone()),
                                            });
                                        }
                                        TyKind::FnPtr(_, _) => {
                                            let signature = super::types::fn_ptr_signature_from_ty(
                                                mapper_ty, tcx, data_types, instance,
                                            );
                                            super::types::ensure_fn_ptr_interface(
                                                &signature, data_types, tcx, instance,
                                            );
                                            instructions.push(oomir::Instruction::CallIndirect {
                                                dest: Some(mapped_address_dest.clone()),
                                                function_ptr: Box::new(
                                                    explicit_method_args[0].clone(),
                                                ),
                                                args: vec![address_operand],
                                                signature,
                                            });
                                        }
                                        other => panic!(
                                            "raw pointer map_addr requires a monomorphized callable, got {other:?}"
                                        ),
                                    }
                                    instructions.push(oomir::Instruction::InvokeStatic {
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "with_addr".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![
                                                (
                                                    "pointer".to_string(),
                                                    dispatch_receiver_ty.clone(),
                                                ),
                                                ("address".to_string(), oomir::Type::U64),
                                            ],
                                            ret: Box::new(dispatch_receiver_ty.clone()),
                                            is_static: true,
                                        },
                                        args: vec![
                                            receiver_operand,
                                            oomir::Operand::Variable {
                                                name: mapped_address_dest,
                                                ty: oomir::Type::U64,
                                            },
                                        ],
                                        dest: effective_dest,
                                    });
                                } else if let Some(interface_name) = use_interface {
                                    let concrete_provided_trait_method = uses_concrete_trait_default
                                        && !matches!(
                                            dispatch_receiver_ty,
                                            oomir::Type::Interface(_)
                                        );
                                    if concrete_provided_trait_method
                                        || receiver_self_requires_static_dispatch
                                        || requires_compiled_static_dispatch(&dispatch_receiver_ty)
                                    {
                                        let target = super::naming::mono_fn_name_from_instance(
                                            tcx,
                                            func_instance,
                                        );
                                        let mut static_signature = method_signature;
                                        static_signature.is_static = true;
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name: target
                                                .class_to_call_on
                                                .expect("monomorphized functions have JVM owners"),
                                            method_name: target.method_name,
                                            method_ty: static_signature,
                                            args: oomir_operands.clone(),
                                            dest: effective_dest,
                                        });
                                    } else {
                                        // The method is from an interface - use InvokeInterface
                                        instructions.push(oomir::Instruction::InvokeInterface {
                                            class_name: interface_name,
                                            method_name: declared_method_name,
                                            method_ty: method_signature,
                                            args: method_args,
                                            dest: effective_dest,
                                            operand: receiver_operand,
                                        });
                                    }
                                } else {
                                    // The receiver is a concrete class - use InvokeVirtual
                                    let class_type = super::types::ty_to_oomir_type(
                                        receiver_mir_ty,
                                        tcx,
                                        data_types,
                                        instance,
                                    );
                                    let runtime_static_target = match &class_type {
                                        oomir::Type::Str if declared_method_name == "as_bytes" => {
                                            Some((
                                                oomir::UTF8_VIEW_CLASS.to_string(),
                                                "asSlice".to_string(),
                                            ))
                                        }
                                        oomir::Type::Str
                                            if declared_method_name == "starts_with"
                                                && oomir_operands.get(1).is_some_and(
                                                    |operand| {
                                                        matches!(
                                                            operand.get_type(),
                                                            Some(oomir::Type::I32)
                                                        )
                                                    },
                                                ) =>
                                        {
                                            Some((
                                                oomir::UTF8_VIEW_CLASS.to_string(),
                                                "startsWithChar".to_string(),
                                            ))
                                        }
                                        oomir::Type::Str
                                            if declared_method_name == "starts_with" =>
                                        {
                                            Some((
                                                oomir::UTF8_VIEW_CLASS.to_string(),
                                                "startsWith".to_string(),
                                            ))
                                        }
                                        oomir::Type::Str if declared_method_name == "eq" => Some((
                                            oomir::UTF8_VIEW_CLASS.to_string(),
                                            "equals".to_string(),
                                        )),
                                        oomir::Type::Str if declared_method_name == "len" => Some(
                                            (oomir::UTF8_VIEW_CLASS.to_string(), "len".to_string()),
                                        ),
                                        oomir::Type::Slice(element)
                                            if declared_method_name == "starts_with"
                                                && matches!(
                                                    element.as_ref(),
                                                    oomir::Type::I8 | oomir::Type::U8
                                                ) =>
                                        {
                                            Some((
                                                oomir::SLICE_VIEW_CLASS.to_string(),
                                                "startsWithI8".to_string(),
                                            ))
                                        }
                                        oomir::Type::Slice(_)
                                            if declared_method_name == "starts_with" =>
                                        {
                                            Some((
                                                oomir::SLICE_VIEW_CLASS.to_string(),
                                                "startsWith".to_string(),
                                            ))
                                        }
                                        oomir::Type::Slice(_)
                                            if matches!(
                                                declared_method_name.as_str(),
                                                "as_ptr" | "as_mut_ptr"
                                            ) =>
                                        {
                                            Some((
                                                oomir::POINTER_CLASS.to_string(),
                                                "fromSlice".to_string(),
                                            ))
                                        }
                                        oomir::Type::Pointer(_)
                                            if pointer_api_receiver
                                                && (is_pointer_cast_method
                                                    || is_pointer_null_method
                                                    || matches!(
                                                        declared_method_name.as_str(),
                                                        "add"
                                                            | "sub"
                                                            | "offset"
                                                            | "offset_from"
                                                            | "offsetFrom"
                                                            | "offset_from_unsigned"
                                                            | "byte_offset_from"
                                                            | "byte_offset_from_unsigned"
                                                            | "wrapping_add"
                                                            | "wrapping_sub"
                                                            | "wrapping_offset"
                                                            | "byte_add"
                                                            | "byte_sub"
                                                            | "byte_offset"
                                                            | "wrapping_byte_add"
                                                            | "wrapping_byte_sub"
                                                            | "wrapping_byte_offset"
                                                            | "align_offset"
                                                            | "is_aligned_to"
                                                            | "addr"
                                                            | "expose_provenance"
                                                            | "with_addr"
                                                            | "map_addr"
                                                            | "with_metadata_of"
                                                    )) =>
                                        {
                                            Some((
                                                oomir::POINTER_CLASS.to_string(),
                                                if is_pointer_cast_method
                                                    || declared_method_name == "with_metadata_of"
                                                {
                                                    "retype".to_string()
                                                } else {
                                                    declared_method_name.clone()
                                                },
                                            ))
                                        }
                                        oomir::Type::Class(class_name)
                                            if class_name == crate::lower2::F128_CLASS
                                                && item.name().as_str() == "to_bits" =>
                                        {
                                            Some((
                                                crate::lower2::F128_CLASS.to_string(),
                                                "to_bits".to_string(),
                                            ))
                                        }
                                        _ => None,
                                    };
                                    let static_target = runtime_static_target.or_else(|| {
                                        (uses_concrete_trait_default
                                            || receiver_self_requires_static_dispatch
                                            || requires_compiled_static_dispatch(&class_type))
                                        .then(|| {
                                            let target = super::naming::mono_fn_name_from_instance(
                                                tcx,
                                                func_instance,
                                            );
                                            (
                                                target.class_to_call_on.expect(
                                                    "monomorphized functions have JVM owners",
                                                ),
                                                target.method_name,
                                            )
                                        })
                                    });

                                    if let Some((class_name, static_method_name)) = static_target {
                                        let mut static_signature = method_signature;
                                        static_signature.is_static = true;
                                        let mut static_args = oomir_operands.clone();
                                        if class_name == oomir::POINTER_CLASS
                                            && static_method_name == "fromSlice"
                                        {
                                            static_signature.params = vec![
                                                (
                                                    "slice".to_string(),
                                                    oomir::Type::Class(
                                                        "java/lang/Object".to_string(),
                                                    ),
                                                ),
                                                ("element_size".to_string(), oomir::Type::U64),
                                                ("codec".to_string(), oomir::Type::java_string()),
                                            ];
                                            let receiver_value_ty = match receiver_mir_ty.kind() {
                                                TyKind::Ref(_, pointee, _)
                                                | TyKind::RawPtr(pointee, _) => *pointee,
                                                _ => receiver_mir_ty,
                                            };
                                            let element_ty =
                                                receiver_value_ty.sequence_element_type(tcx);
                                            let element_ty = EarlyBinder::bind(tcx, element_ty)
                                                .instantiate(tcx, instance.args)
                                                .skip_norm_wip();
                                            let element_size = super::types::layout_size_bytes(
                                                tcx,
                                                element_ty,
                                            )
                                            .unwrap_or_else(|error| {
                                                panic!(
                                                    "could not determine slice pointer element size: {error}"
                                                )
                                            });
                                            static_args.push(oomir::Operand::Constant(
                                                oomir::Constant::U64(
                                                    u64::try_from(element_size).expect(
                                                        "Rust slice element layout exceeds u64",
                                                    ),
                                                ),
                                            ));
                                            static_args.push(
                                                super::types::pointer_view_codec_operand(
                                                    element_ty, tcx, data_types, instance,
                                                ),
                                            );
                                        } else if class_name == oomir::POINTER_CLASS
                                            && static_method_name == "retype"
                                        {
                                            if declared_method_name == "with_metadata_of" {
                                                static_signature.params.truncate(1);
                                                static_args.truncate(1);
                                            }
                                            static_signature
                                                .params
                                                .push(("view_size".to_string(), oomir::Type::U64));
                                            static_signature.params.push((
                                                "view_codec".to_string(),
                                                oomir::Type::java_string(),
                                            ));
                                            let target_pointee = match fn_output.kind() {
                                                TyKind::RawPtr(pointee, _)
                                                | TyKind::Ref(_, pointee, _) => *pointee,
                                                other => panic!(
                                                    "pointer cast returned non-pointer type {other:?}"
                                                ),
                                            };
                                            let target_pointee =
                                                EarlyBinder::bind(tcx, target_pointee)
                                                    .instantiate(tcx, instance.args)
                                                    .skip_norm_wip();
                                            let target_size = super::types::layout_size_bytes(
                                                tcx,
                                                target_pointee,
                                            )
                                            .unwrap_or_else(|error| {
                                                panic!(
                                                    "could not determine pointer cast target size: {error}"
                                                )
                                            });
                                            static_args.push(oomir::Operand::Constant(
                                                oomir::Constant::U64(
                                                    u64::try_from(target_size).expect(
                                                        "Rust pointer target layout exceeds u64",
                                                    ),
                                                ),
                                            ));
                                            static_args.push(
                                                super::types::pointer_view_codec_operand(
                                                    target_pointee,
                                                    tcx,
                                                    data_types,
                                                    instance,
                                                ),
                                            );
                                        } else if class_name == oomir::POINTER_CLASS
                                            && static_method_name == "map_addr"
                                            && static_signature.params.len() >= 2
                                        {
                                            static_signature.params[1].1 =
                                                oomir::Type::Class("java/lang/Object".to_string());
                                        }
                                        if class_name.starts_with("org/rustlang/runtime/")
                                            && static_signature.params.last().is_some_and(
                                                |(name, _)| {
                                                    name == oomir::CALLER_LOCATION_PARAM_NAME
                                                },
                                            )
                                        {
                                            static_signature.params.pop();
                                            static_args.pop();
                                        }
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            class_name,
                                            method_name: static_method_name,
                                            method_ty: static_signature,
                                            args: static_args,
                                            dest: effective_dest,
                                        });
                                    } else {
                                        let class_name = class_type
                                            .get_class_name()
                                            .unwrap_or_else(|| {
                                                panic!(
                                                    "no JVM class is available for virtual method `{declared_method_name}` on {class_type:?}"
                                                )
                                            })
                                            .to_string();

                                        instructions.push(oomir::Instruction::InvokeVirtual {
                                            class_name,
                                            method_name,
                                            method_ty: method_signature,
                                            args: method_args,
                                            dest: effective_dest,
                                            operand: receiver_operand,
                                        });
                                    }
                                }
                            }
                        } else {
                            method_signature.is_static = true;
                            let method_name = super::naming::associated_method_name_from_instance(
                                tcx,
                                func_instance,
                                &method_signature,
                            );

                            let self_ty_opt = if let Some(impl_def_id) = item.impl_container(tcx) {
                                Some(
                                    tcx.type_of(impl_def_id)
                                        .instantiate(tcx, func_instance.args)
                                        .skip_norm_wip(),
                                )
                            } else {
                                func_instance.args.types().next()
                            };

                            let mut generated = false;
                            if item.trait_container(tcx).is_none()
                                && item.trait_item_def_id().is_none()
                                && let Some(self_ty) = self_ty_opt
                            {
                                let class_type = super::types::ty_to_oomir_type(
                                    self_ty,
                                    tcx,
                                    data_types,
                                    func_instance,
                                );

                                if class_type == oomir::Type::F16
                                    && method_name == "from_bits"
                                    && oomir_operands.len() == 1
                                {
                                    if let Some(dest) = effective_dest.clone() {
                                        instructions.push(oomir::Instruction::InvokeStatic {
                                            dest: Some(dest),
                                            class_name: "org/rustlang/runtime/Numbers".to_string(),
                                            method_name: "f16FromBits".to_string(),
                                            method_ty: oomir::Signature {
                                                params: vec![(
                                                    "bits".to_string(),
                                                    oomir::Type::U16,
                                                )],
                                                ret: Box::new(oomir::Type::F16),
                                                is_static: true,
                                            },
                                            args: vec![oomir_operands[0].clone()],
                                        });
                                    }
                                    generated = true;
                                }

                                if !generated
                                    && let Some(class_name) = class_type.get_class_name()
                                    && matches!(
                                        data_types.get(class_name),
                                        Some(oomir::DataType::Class { methods, .. })
                                            if methods.contains_key(&method_name)
                                    )
                                {
                                    instructions.push(oomir::Instruction::InvokeStatic {
                                        class_name: class_name.to_string(),
                                        method_name: method_name.clone(),
                                        method_ty: method_signature.clone(),
                                        args: oomir_operands.clone(),
                                        dest: effective_dest.clone(), // use effective_dest
                                    });
                                    generated = true;
                                }
                            }

                            if !generated {
                                let fn_name_data =
                                    super::naming::mono_fn_name_from_instance(tcx, func_instance);
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    class_name: fn_name_data
                                        .class_to_call_on
                                        .expect("monomorphized functions have JVM owners"),
                                    method_name: fn_name_data.method_name,
                                    method_ty: method_signature.clone(),
                                    args: oomir_operands.clone(),
                                    dest: effective_dest, // use effective_dest
                                });
                            }
                        }
                    } else {
                        let called_def_id = func_instance.def_id();
                        let intrinsic_name = tcx
                            .opt_item_name(called_def_id)
                            .map(|name| name.as_str().to_string())
                            .unwrap_or_default();
                        let is_compiler_intrinsic = matches!(
                            func_instance.def,
                            InstanceKind::Intrinsic(_) | InstanceKind::LlvmIntrinsic(_)
                        ) || (!intrinsic_name.is_empty()
                            && tcx.is_intrinsic(called_def_id, Symbol::intern(&intrinsic_name)));
                        let is_diagnostic_item =
                            |diagnostic| tcx.is_diagnostic_item(diagnostic, called_def_id);
                        let has_diagnostic_item = |diagnostic: &str| {
                            tcx.is_diagnostic_item(Symbol::intern(diagnostic), called_def_id)
                        };
                        let is_external_intrinsic = !called_def_id.is_local();
                        let is_core_crate_item =
                            tcx.crate_name(called_def_id.krate).as_str() == "core";
                        let has_pointer_like_operand =
                            oomir_operands.first().is_some_and(|operand| {
                                matches!(
                                    operand.get_type(),
                                    Some(
                                        oomir::Type::Pointer(_)
                                            | oomir::Type::Slice(_)
                                            | oomir::Type::Str
                                    )
                                )
                            });
                        let is_core_ptr = is_core_crate_item && has_pointer_like_operand;
                        let is_size_of = (is_compiler_intrinsic
                            && intrinsic_name.as_str() == "size_of")
                            || has_diagnostic_item("mem_size_of");
                        let is_align_of = (is_compiler_intrinsic
                            && intrinsic_name.as_str() == "align_of")
                            || has_diagnostic_item("mem_align_of");
                        let is_size_of_val = (is_compiler_intrinsic
                            && intrinsic_name.as_str() == "size_of_val")
                            || has_diagnostic_item("mem_size_of_val");
                        let is_align_of_val =
                            is_compiler_intrinsic && intrinsic_name.as_str() == "align_of_val";
                        // The public wrapper has no diagnostic/lang item of its own,
                        // so identify it by its defining module rather than its bare name.
                        let is_ptr_metadata = (is_compiler_intrinsic
                            && intrinsic_name.as_str() == "ptr_metadata")
                            || is_core_ptr_metadata_api(tcx, called_def_id);
                        if is_compiler_intrinsic
                            && matches!(
                                intrinsic_name.as_str(),
                                "assert_inhabited"
                                    | "assert_zero_valid"
                                    | "assert_mem_uninitialized_valid"
                            )
                        {
                            // These are compile-time UB guards. rustc permits them to either
                            // panic or do nothing, and reaching the invalid case would make the
                            // following unsafe operation UB. Valid monomorphizations therefore
                            // require no JVM instruction.
                        } else if is_compiler_intrinsic
                            && intrinsic_name == "black_box"
                            && oomir_operands.len() == 1
                        {
                            if let Some(dest) = effective_dest.clone() {
                                instructions.push(oomir::Instruction::Move {
                                    dest,
                                    src: oomir_operands[0].clone(),
                                });
                            }
                        } else if is_compiler_intrinsic
                            && intrinsic_name == "disjoint_bitor"
                            && oomir_operands.len() == 2
                            && let Some(dest) = effective_dest.clone()
                        {
                            instructions.push(oomir::Instruction::BitOr {
                                dest,
                                op1: oomir_operands[0].clone(),
                                op2: oomir_operands[1].clone(),
                            });
                        } else if is_compiler_intrinsic
                            && matches!(intrinsic_name.as_str(), "bswap" | "fabs")
                            && oomir_operands.len() == 1
                        {
                            instructions.push(oomir::Instruction::InvokeStatic {
                                class_name: "org/rustlang/runtime/Intrinsics".to_string(),
                                method_name: if intrinsic_name == "bswap" {
                                    "byteSwap".to_string()
                                } else {
                                    "floatAbs".to_string()
                                },
                                method_ty: oomir::Signature {
                                    params: vec![(
                                        "value".to_string(),
                                        oomir_operands[0]
                                            .get_type()
                                            .expect("unary intrinsic operand is typed"),
                                    )],
                                    ret: Box::new(oomir_output_type.clone()),
                                    is_static: true,
                                },
                                args: oomir_operands.clone(),
                                dest: effective_dest.clone(),
                            });
                        } else if is_compiler_intrinsic
                            && matches!(
                                intrinsic_name.as_str(),
                                "unchecked_funnel_shl" | "unchecked_funnel_shr"
                            )
                            && oomir_operands.len() == 3
                        {
                            instructions.push(oomir::Instruction::InvokeStatic {
                                class_name: "org/rustlang/runtime/Intrinsics".to_string(),
                                method_name: if intrinsic_name == "unchecked_funnel_shl" {
                                    "funnelShiftLeft".to_string()
                                } else {
                                    "funnelShiftRight".to_string()
                                },
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("high".to_string(), oomir_output_type.clone()),
                                        ("low".to_string(), oomir_output_type.clone()),
                                        ("shift".to_string(), oomir::Type::U32),
                                    ],
                                    ret: Box::new(oomir_output_type.clone()),
                                    is_static: true,
                                },
                                args: oomir_operands.clone(),
                                dest: effective_dest.clone(),
                            });
                        } else if is_compiler_intrinsic
                            && intrinsic_name == "carryless_mul"
                            && oomir_operands.len() == 2
                        {
                            instructions.push(oomir::Instruction::InvokeStatic {
                                class_name: "org/rustlang/runtime/Intrinsics".to_string(),
                                method_name: "carrylessMultiply".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("left".to_string(), oomir_output_type.clone()),
                                        ("right".to_string(), oomir_output_type.clone()),
                                    ],
                                    ret: Box::new(oomir_output_type.clone()),
                                    is_static: true,
                                },
                                args: oomir_operands.clone(),
                                dest: effective_dest.clone(),
                            });
                        } else if is_compiler_intrinsic
                            && intrinsic_name == "catch_unwind"
                            && oomir_operands.len() == 3
                        {
                            let data_ty = oomir_operands[1]
                                .get_type()
                                .expect("catch_unwind data pointer is typed");
                            instructions.push(oomir::Instruction::InvokeStatic {
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "catchUnwind".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        (
                                            "try_function".to_string(),
                                            oomir::Type::Class("java/lang/Object".to_string()),
                                        ),
                                        ("data".to_string(), data_ty),
                                        (
                                            "catch_function".to_string(),
                                            oomir::Type::Class("java/lang/Object".to_string()),
                                        ),
                                    ],
                                    ret: Box::new(oomir::Type::Boolean),
                                    is_static: true,
                                },
                                args: oomir_operands.clone(),
                                dest: effective_dest.clone(),
                            });
                        } else if is_compiler_intrinsic
                            && emit_atomic_intrinsic(
                                &intrinsic_name,
                                func_instance,
                                instance,
                                &oomir_operands,
                                &oomir_output_type,
                                effective_dest.clone(),
                                &label,
                                tcx,
                                data_types,
                                &mut instructions,
                            )
                        {
                        } else if is_compiler_intrinsic
                            && intrinsic_name == "const_allocate"
                            && oomir_operands.len() == 2
                        {
                            let pointer_size = match fn_output.kind() {
                                TyKind::RawPtr(pointee, _) => {
                                    super::types::layout_size_bytes(tcx, *pointee).unwrap_or(1)
                                }
                                _ => 1,
                            };
                            instructions.push(oomir::Instruction::InvokeStatic {
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "nullPointer".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![("view_size".to_string(), oomir::Type::U64)],
                                    ret: Box::new(oomir_output_type.clone()),
                                    is_static: true,
                                },
                                args: vec![oomir::Operand::Constant(oomir::Constant::U64(
                                    u64::try_from(pointer_size)
                                        .expect("Rust allocation pointer layout exceeds u64"),
                                ))],
                                dest: effective_dest,
                            });
                        } else if is_compiler_intrinsic && intrinsic_name == "const_deallocate" {
                            // Rust's const allocator performs no allocation at
                            // runtime, so its matching deallocator is a no-op.
                        } else if is_compiler_intrinsic && intrinsic_name == "cold_path" {
                            // The JVM has no equivalent branch-prediction hint.
                        } else if is_compiler_intrinsic
                            && intrinsic_name == "select_unpredictable"
                            && oomir_operands.len() == 3
                            && let Some(dest) = effective_dest.clone()
                        {
                            let condition = oomir_operands[0].clone();
                            let true_value = oomir_operands[1].clone();
                            let false_value = oomir_operands[2].clone();
                            if oomir_output_type.is_jvm_reference_type() {
                                let selected_object = format!("{dest}_selected_object");
                                let object_ty = oomir::Type::Class("java/lang/Object".to_string());
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    class_name: "org/rustlang/runtime/Intrinsics".to_string(),
                                    method_name: "selectUnpredictable".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            ("condition".to_string(), oomir::Type::Boolean),
                                            ("true_value".to_string(), object_ty.clone()),
                                            ("false_value".to_string(), object_ty.clone()),
                                        ],
                                        ret: Box::new(object_ty.clone()),
                                        is_static: true,
                                    },
                                    args: vec![condition, true_value, false_value],
                                    dest: Some(selected_object.clone()),
                                });
                                instructions.push(oomir::Instruction::Cast {
                                    op: oomir::Operand::Variable {
                                        name: selected_object,
                                        ty: object_ty,
                                    },
                                    ty: oomir_output_type.clone(),
                                    dest,
                                });
                            } else {
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    class_name: "org/rustlang/runtime/Intrinsics".to_string(),
                                    method_name: "selectUnpredictable".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            ("condition".to_string(), oomir::Type::Boolean),
                                            ("true_value".to_string(), oomir_output_type.clone()),
                                            ("false_value".to_string(), oomir_output_type.clone()),
                                        ],
                                        ret: Box::new(oomir_output_type.clone()),
                                        is_static: true,
                                    },
                                    args: vec![condition, true_value, false_value],
                                    dest: Some(dest),
                                });
                            }
                        } else if is_compiler_intrinsic
                            && intrinsic_name == "ctpop"
                            && let Some(dest) = effective_dest.clone()
                        {
                            let operand = oomir_operands
                                .first()
                                .cloned()
                                .expect("ctpop has an integer operand");
                            let operand_ty = operand
                                .get_type()
                                .expect("ctpop integer operand has a JVM type");
                            let (method_name, parameter_ty, call_args) = match operand_ty {
                                oomir::Type::I8 | oomir::Type::U8 => (
                                    "bitCount32",
                                    oomir::Type::I32,
                                    vec![
                                        operand,
                                        oomir::Operand::Constant(oomir::Constant::I32(8)),
                                    ],
                                ),
                                oomir::Type::I16 | oomir::Type::U16 => (
                                    "bitCount32",
                                    oomir::Type::I32,
                                    vec![
                                        operand,
                                        oomir::Operand::Constant(oomir::Constant::I32(16)),
                                    ],
                                ),
                                oomir::Type::I32 | oomir::Type::U32 => (
                                    "bitCount32",
                                    oomir::Type::I32,
                                    vec![
                                        operand,
                                        oomir::Operand::Constant(oomir::Constant::I32(32)),
                                    ],
                                ),
                                oomir::Type::I64 | oomir::Type::U64 => {
                                    ("bitCount64", oomir::Type::I64, vec![operand])
                                }
                                oomir::Type::Class(ref class_name)
                                    if class_name == crate::lower2::I128_CLASS =>
                                {
                                    ("bitCountI128", operand_ty.clone(), vec![operand])
                                }
                                oomir::Type::Class(ref class_name)
                                    if class_name == crate::lower2::U128_CLASS =>
                                {
                                    ("bitCountU128", operand_ty.clone(), vec![operand])
                                }
                                other => panic!("unsupported ctpop integer carrier {other:?}"),
                            };
                            let mut params = vec![("value".to_string(), parameter_ty)];
                            if call_args.len() == 2 {
                                params.push(("bit_width".to_string(), oomir::Type::I32));
                            }
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(dest),
                                class_name: "org/rustlang/runtime/Numbers".to_string(),
                                method_name: method_name.to_string(),
                                method_ty: oomir::Signature {
                                    params,
                                    ret: Box::new(oomir::Type::U32),
                                    is_static: true,
                                },
                                args: call_args,
                            });
                        } else if is_compiler_intrinsic
                            && matches!(
                                intrinsic_name.as_str(),
                                "ctlz" | "ctlz_nonzero" | "cttz" | "cttz_nonzero"
                            )
                            && let Some(dest) = effective_dest.clone()
                        {
                            let operand = oomir_operands
                                .first()
                                .cloned()
                                .expect("zero-count intrinsic has an integer operand");
                            let operand_ty = operand
                                .get_type()
                                .expect("zero-count integer operand has a JVM type");
                            let leading = intrinsic_name.starts_with("ctlz");
                            let operation = if leading {
                                "leadingZeros"
                            } else {
                                "trailingZeros"
                            };
                            let (method_name, parameter_ty, call_args) = match operand_ty {
                                oomir::Type::I8 | oomir::Type::U8 => (
                                    format!("{operation}32"),
                                    oomir::Type::I32,
                                    vec![
                                        operand,
                                        oomir::Operand::Constant(oomir::Constant::I32(8)),
                                    ],
                                ),
                                oomir::Type::I16 | oomir::Type::U16 => (
                                    format!("{operation}32"),
                                    oomir::Type::I32,
                                    vec![
                                        operand,
                                        oomir::Operand::Constant(oomir::Constant::I32(16)),
                                    ],
                                ),
                                oomir::Type::I32 | oomir::Type::U32 => (
                                    format!("{operation}32"),
                                    oomir::Type::I32,
                                    vec![
                                        operand,
                                        oomir::Operand::Constant(oomir::Constant::I32(32)),
                                    ],
                                ),
                                oomir::Type::I64 | oomir::Type::U64 => {
                                    (format!("{operation}64"), oomir::Type::I64, vec![operand])
                                }
                                oomir::Type::Class(ref class_name)
                                    if class_name == crate::lower2::I128_CLASS =>
                                {
                                    (
                                        format!("{operation}I128"),
                                        operand_ty.clone(),
                                        vec![operand],
                                    )
                                }
                                oomir::Type::Class(ref class_name)
                                    if class_name == crate::lower2::U128_CLASS =>
                                {
                                    (
                                        format!("{operation}U128"),
                                        operand_ty.clone(),
                                        vec![operand],
                                    )
                                }
                                other => {
                                    panic!("unsupported zero-count integer carrier {other:?}")
                                }
                            };
                            let mut params = vec![("value".to_string(), parameter_ty)];
                            if call_args.len() == 2 {
                                params.push(("bit_width".to_string(), oomir::Type::I32));
                            }
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(dest),
                                class_name: "org/rustlang/runtime/Numbers".to_string(),
                                method_name,
                                method_ty: oomir::Signature {
                                    params,
                                    ret: Box::new(oomir::Type::U32),
                                    is_static: true,
                                },
                                args: call_args,
                            });
                        } else if is_compiler_intrinsic
                            && intrinsic_name.as_str() == "is_val_statically_known"
                            && let Some(dest) = effective_dest.clone()
                        {
                            // This intrinsic is explicitly nondeterministic: callers must be
                            // correct for either result. Returning false is its conservative
                            // runtime implementation when this backend has not proved the
                            // operand to be a compile-time constant.
                            instructions.push(oomir::Instruction::Move {
                                dest,
                                src: oomir::Operand::Constant(oomir::Constant::Boolean(false)),
                            });
                        } else if is_compiler_intrinsic
                            && intrinsic_name == "arith_offset"
                            && oomir_operands.len() == 2
                        {
                            let pointer_ty = oomir_operands[0]
                                .get_type()
                                .expect("arith_offset pointer operand is typed");
                            if !matches!(pointer_ty, oomir::Type::Pointer(_)) {
                                panic!(
                                    "arith_offset requires a pointer operand, found {pointer_ty:?}"
                                );
                            }
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: effective_dest.clone(),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "offset".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("pointer".to_string(), pointer_ty.clone()),
                                        ("count".to_string(), oomir::Type::I64),
                                    ],
                                    ret: Box::new(oomir_output_type.clone()),
                                    is_static: true,
                                },
                                args: vec![oomir_operands[0].clone(), oomir_operands[1].clone()],
                            });
                        } else if is_compiler_intrinsic
                            && matches!(
                                intrinsic_name.as_str(),
                                "saturating_add" | "saturating_sub"
                            )
                            && let Some(dest) = effective_dest.clone()
                        {
                            let operation = if intrinsic_name == "saturating_add" {
                                "Add"
                            } else {
                                "Sub"
                            };
                            let suffix = match &oomir_output_type {
                                oomir::Type::I8 => "I8",
                                oomir::Type::U8 => "U8",
                                oomir::Type::I16 => "I16",
                                oomir::Type::U16 => "U16",
                                oomir::Type::I32 => "I32",
                                oomir::Type::U32 => "U32",
                                oomir::Type::I64 => "I64",
                                oomir::Type::U64 => "U64",
                                oomir::Type::Class(class_name)
                                    if class_name == crate::lower2::I128_CLASS =>
                                {
                                    "I128"
                                }
                                oomir::Type::Class(class_name)
                                    if class_name == crate::lower2::U128_CLASS =>
                                {
                                    "U128"
                                }
                                ref other => {
                                    panic!("unsupported saturating intrinsic carrier {other:?}")
                                }
                            };
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: Some(dest),
                                class_name: "org/rustlang/runtime/Numbers".to_string(),
                                method_name: format!("saturating{operation}{suffix}"),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("left".to_string(), oomir_output_type.clone()),
                                        ("right".to_string(), oomir_output_type.clone()),
                                    ],
                                    ret: Box::new(oomir_output_type.clone()),
                                    is_static: true,
                                },
                                args: oomir_operands.clone(),
                            });
                        } else if is_compiler_intrinsic
                            && matches!(
                                intrinsic_name.as_str(),
                                "exact_div" | "unchecked_div" | "unchecked_rem"
                            )
                            && let Some(dest) = effective_dest.clone()
                        {
                            let [left, right] = oomir_operands.as_slice() else {
                                panic!("{intrinsic_name} requires two operands")
                            };
                            if intrinsic_name == "unchecked_rem" {
                                instructions.push(oomir::Instruction::Rem {
                                    dest,
                                    op1: left.clone(),
                                    op2: right.clone(),
                                });
                            } else {
                                instructions.push(oomir::Instruction::Div {
                                    dest,
                                    op1: left.clone(),
                                    op2: right.clone(),
                                });
                            }
                        } else if is_compiler_intrinsic
                            && matches!(
                                intrinsic_name.as_str(),
                                "ptr_offset_from" | "ptr_offset_from_unsigned"
                            )
                            && oomir_operands.len() == 2
                        {
                            let left_ty = oomir_operands[0]
                                .get_type()
                                .expect("ptr_offset_from left operand is typed");
                            let right_ty = oomir_operands[1]
                                .get_type()
                                .expect("ptr_offset_from right operand is typed");
                            if !matches!(left_ty, oomir::Type::Pointer(_))
                                || !matches!(right_ty, oomir::Type::Pointer(_))
                            {
                                panic!(
                                    "ptr_offset_from requires pointer operands, found {left_ty:?} and {right_ty:?}"
                                );
                            }
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: effective_dest.clone(),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "offset_from".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("left".to_string(), left_ty),
                                        ("right".to_string(), right_ty),
                                    ],
                                    ret: Box::new(oomir_output_type.clone()),
                                    is_static: true,
                                },
                                args: vec![oomir_operands[0].clone(), oomir_operands[1].clone()],
                            });
                        } else if is_compiler_intrinsic
                            && intrinsic_name == "compare_bytes"
                            && oomir_operands.len() == 3
                        {
                            let left_ty = oomir_operands[0]
                                .get_type()
                                .expect("compare_bytes left operand is typed");
                            let right_ty = oomir_operands[1]
                                .get_type()
                                .expect("compare_bytes right operand is typed");
                            if !matches!(left_ty, oomir::Type::Pointer(_))
                                || !matches!(right_ty, oomir::Type::Pointer(_))
                            {
                                panic!(
                                    "compare_bytes requires pointer operands, found {left_ty:?} and {right_ty:?}"
                                );
                            }
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: effective_dest.clone(),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "compareBytes".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("left".to_string(), left_ty),
                                        ("right".to_string(), right_ty),
                                        ("length".to_string(), oomir::Type::U64),
                                    ],
                                    ret: Box::new(oomir::Type::I32),
                                    is_static: true,
                                },
                                args: vec![
                                    oomir_operands[0].clone(),
                                    oomir_operands[1].clone(),
                                    oomir_operands[2].clone(),
                                ],
                            });
                        } else if intrinsic_name == "raw_eq"
                            && is_compiler_intrinsic
                            && let Some(dest) = effective_dest.clone()
                        {
                            let compared_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("raw_eq has a compared type argument");
                            let byte_count = super::types::layout_size_bytes(tcx, compared_ty)
                                .expect("raw_eq type has a concrete layout");
                            if byte_count == 0 {
                                instructions.push(oomir::Instruction::Move {
                                    dest,
                                    src: oomir::Operand::Constant(oomir::Constant::Boolean(true)),
                                });
                            } else {
                                let left = emit_raw_eq_pointer(
                                    oomir_operands[0].clone(),
                                    compared_ty,
                                    &format!("{label}_raw_eq_left"),
                                    tcx,
                                    instance,
                                    data_types,
                                    &mut instructions,
                                );
                                let right = emit_raw_eq_pointer(
                                    oomir_operands[1].clone(),
                                    compared_ty,
                                    &format!("{label}_raw_eq_right"),
                                    tcx,
                                    instance,
                                    data_types,
                                    &mut instructions,
                                );
                                let comparison = format!("{label}_raw_eq_comparison");
                                let pointer_ty =
                                    left.get_type().expect("raw_eq pointer operand is typed");
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    dest: Some(comparison.clone()),
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "compareBytes".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            ("left".to_string(), pointer_ty.clone()),
                                            ("right".to_string(), pointer_ty),
                                            ("length".to_string(), oomir::Type::U64),
                                        ],
                                        ret: Box::new(oomir::Type::I32),
                                        is_static: true,
                                    },
                                    args: vec![
                                        left,
                                        right,
                                        oomir::Operand::Constant(oomir::Constant::U64(
                                            byte_count as u64,
                                        )),
                                    ],
                                });
                                instructions.push(oomir::Instruction::Eq {
                                    dest,
                                    op1: oomir::Operand::Variable {
                                        name: comparison,
                                        ty: oomir::Type::I32,
                                    },
                                    op2: oomir::Operand::Constant(oomir::Constant::I32(0)),
                                });
                            }
                        } else if intrinsic_name.as_str() == "caller_location"
                            && is_compiler_intrinsic
                            && let Some(dest) = effective_dest.clone()
                        {
                            let location = caller_location_operand(
                                terminator.source_info,
                                tcx,
                                instance,
                                mir,
                                data_types,
                                &mut instructions,
                                &format!("{label}_caller_location_intrinsic"),
                            );
                            instructions.push(oomir::Instruction::Move {
                                dest,
                                src: location,
                            });
                        } else if (is_size_of || is_align_of)
                            && let Some(dest) = effective_dest.clone()
                        {
                            let measured_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("size/alignment intrinsic has a type argument");
                            let layout = tcx
                                .layout_of(
                                    TypingEnv::fully_monomorphized()
                                        .as_query_input(measured_ty),
                                )
                                .unwrap_or_else(|error| {
                                    panic!(
                                        "could not determine layout for {intrinsic_name}::<{measured_ty:?}>: {error:?}"
                                    )
                                });
                            let value = if is_size_of {
                                layout.size.bytes()
                            } else {
                                layout.align.abi.bytes()
                            };
                            instructions.push(oomir::Instruction::Move {
                                dest,
                                src: oomir::Operand::Constant(oomir::Constant::U64(value)),
                            });
                        } else if is_size_of_val && let Some(dest) = effective_dest.clone() {
                            let measured_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("size_of_val intrinsic has a type argument");
                            let tail = tcx.struct_tail_for_codegen(
                                measured_ty,
                                TypingEnv::fully_monomorphized(),
                            );
                            let tail_element = match tail.kind() {
                                TyKind::Slice(element_ty) => Some(*element_ty),
                                TyKind::Str => Some(tcx.types.u8),
                                _ => None,
                            };
                            if let Some(element_ty) = tail_element {
                                let layout = tcx
                                    .layout_of(
                                        TypingEnv::fully_monomorphized()
                                            .as_query_input(measured_ty),
                                    )
                                    .unwrap_or_else(|error| {
                                        panic!(
                                            "could not determine size_of_val DST layout for {measured_ty:?}: {error:?}"
                                        )
                                    });
                                let element_size =
                                    super::types::layout_size_bytes(tcx, element_ty)
                                        .unwrap_or_else(|error| {
                                            panic!(
                                                "could not determine size_of_val tail element layout: {error}"
                                            )
                                        });
                                let alignment = layout.align.abi.bytes().max(
                                    super::types::layout_align_bytes(tcx, element_ty)
                                        .expect("DST tail element has an alignment")
                                        as u64,
                                );
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    dest: Some(dest),
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "sizeOfSliceTailed".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            (
                                                "value".to_string(),
                                                oomir::Type::Class("java/lang/Object".to_string()),
                                            ),
                                            ("prefix_size".to_string(), oomir::Type::U64),
                                            ("element_size".to_string(), oomir::Type::U64),
                                            ("alignment".to_string(), oomir::Type::U64),
                                        ],
                                        ret: Box::new(oomir_output_type.clone()),
                                        is_static: true,
                                    },
                                    args: vec![
                                        oomir_operands[0].clone(),
                                        oomir::Operand::Constant(oomir::Constant::U64(
                                            layout.size.bytes(),
                                        )),
                                        oomir::Operand::Constant(oomir::Constant::U64(
                                            element_size as u64,
                                        )),
                                        oomir::Operand::Constant(oomir::Constant::U64(alignment)),
                                    ],
                                });
                            } else {
                                let size = super::types::layout_size_bytes(tcx, measured_ty)
                                    .unwrap_or_else(|error| {
                                        panic!("could not determine size_of_val layout: {error}")
                                    });
                                instructions.push(oomir::Instruction::Move {
                                    dest,
                                    src: oomir::Operand::Constant(oomir::Constant::U64(
                                        size as u64,
                                    )),
                                });
                            }
                        } else if is_align_of_val && let Some(dest) = effective_dest.clone() {
                            let measured_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("align_of_val intrinsic has a type argument");
                            let alignment = match measured_ty.kind() {
                                TyKind::Slice(element_ty) => {
                                    super::types::layout_align_bytes(tcx, *element_ty)
                                }
                                TyKind::Str => Ok(1),
                                _ => super::types::layout_align_bytes(tcx, measured_ty),
                            }
                            .unwrap_or_else(|error| {
                                panic!(
                                    "could not determine align_of_val layout for {measured_ty:?}: {error}"
                                )
                            });
                            instructions.push(oomir::Instruction::Move {
                                dest,
                                src: oomir::Operand::Constant(oomir::Constant::U64(
                                    alignment as u64,
                                )),
                            });
                        } else if intrinsic_name.as_str() == "type_id_eq"
                            && is_compiler_intrinsic
                            && oomir_operands.len() == 2
                            && let Some(dest) = effective_dest.clone()
                        {
                            let type_id_ty = oomir_operands[0]
                                .get_type()
                                .expect("TypeId equality operands are typed");
                            let oomir::Type::Class(type_id_class) = &type_id_ty else {
                                panic!("type_id_eq received non-class operand {type_id_ty:?}");
                            };
                            let data_ty = match data_types.get(type_id_class) {
                                Some(oomir::DataType::Class { fields, .. }) => fields
                                    .iter()
                                    .find(|(name, _)| name == "data")
                                    .map(|(_, ty)| ty.clone())
                                    .expect("TypeId class has a data field"),
                                _ => panic!("TypeId class {type_id_class} is not defined"),
                            };
                            let oomir::Type::Array(pointer_ty) = &data_ty else {
                                panic!("TypeId data field is not an array: {data_ty:?}");
                            };
                            let left_data = format!("{label}_type_id_left_data");
                            let right_data = format!("{label}_type_id_right_data");
                            instructions.push(oomir::Instruction::GetField {
                                dest: left_data.clone(),
                                object: oomir_operands[0].clone(),
                                field_name: "data".to_string(),
                                field_ty: data_ty.clone(),
                                owner_class: type_id_class.clone(),
                            });
                            instructions.push(oomir::Instruction::GetField {
                                dest: right_data.clone(),
                                object: oomir_operands[1].clone(),
                                field_name: "data".to_string(),
                                field_ty: data_ty.clone(),
                                owner_class: type_id_class.clone(),
                            });

                            let limb_count = 16usize
                                / usize::try_from(tcx.data_layout.pointer_size().bytes())
                                    .expect("pointer size fits usize");
                            let mut equality = None;
                            for limb in 0..limb_count {
                                let left_limb = format!("{label}_type_id_left_{limb}");
                                let right_limb = format!("{label}_type_id_right_{limb}");
                                let limb_equal = format!("{label}_type_id_equal_{limb}");
                                let index =
                                    oomir::Operand::Constant(oomir::Constant::I32(limb as i32));
                                instructions.push(oomir::Instruction::ArrayGet {
                                    dest: left_limb.clone(),
                                    array: oomir::Operand::Variable {
                                        name: left_data.clone(),
                                        ty: data_ty.clone(),
                                    },
                                    index: index.clone(),
                                });
                                instructions.push(oomir::Instruction::ArrayGet {
                                    dest: right_limb.clone(),
                                    array: oomir::Operand::Variable {
                                        name: right_data.clone(),
                                        ty: data_ty.clone(),
                                    },
                                    index,
                                });
                                instructions.push(oomir::Instruction::InvokeVirtual {
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "sameAddress".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            ("self".to_string(), pointer_ty.as_ref().clone()),
                                            ("other".to_string(), pointer_ty.as_ref().clone()),
                                        ],
                                        ret: Box::new(oomir::Type::Boolean),
                                        is_static: false,
                                    },
                                    args: vec![oomir::Operand::Variable {
                                        name: right_limb,
                                        ty: pointer_ty.as_ref().clone(),
                                    }],
                                    dest: Some(limb_equal.clone()),
                                    operand: oomir::Operand::Variable {
                                        name: left_limb,
                                        ty: pointer_ty.as_ref().clone(),
                                    },
                                });
                                let limb_equal = oomir::Operand::Variable {
                                    name: limb_equal,
                                    ty: oomir::Type::Boolean,
                                };
                                equality = Some(if let Some(previous) = equality {
                                    let combined = format!("{label}_type_id_combined_{limb}");
                                    instructions.push(oomir::Instruction::BitAnd {
                                        dest: combined.clone(),
                                        op1: previous,
                                        op2: limb_equal,
                                    });
                                    oomir::Operand::Variable {
                                        name: combined,
                                        ty: oomir::Type::Boolean,
                                    }
                                } else {
                                    limb_equal
                                });
                            }
                            instructions.push(oomir::Instruction::Move {
                                dest,
                                src: equality.unwrap_or(oomir::Operand::Constant(
                                    oomir::Constant::Boolean(true),
                                )),
                            });
                        } else if intrinsic_name.as_str() == "forget" && is_external_intrinsic {
                            // `mem::forget` intentionally consumes its argument without
                            // running drop glue. The value is already represented by the
                            // evaluated call operand, so there is no JVM instruction to emit.
                        } else if has_diagnostic_item("ptr_drop_in_place") && is_core_ptr {
                            let pointee_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("drop_in_place has a pointee type argument");
                            if pointee_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                                let pointee_oomir_ty = super::types::ty_to_oomir_type(
                                    pointee_ty, tcx, data_types, instance,
                                );
                                let pointer = oomir_operands[0].clone();
                                let value = if !matches!(pointee_ty.kind(), TyKind::Slice(_))
                                    && matches!(pointer.get_type(), Some(oomir::Type::Pointer(_)))
                                {
                                    let value_name = format!("{label}_drop_in_place_value");
                                    super::place::emit_pointer_read(
                                        pointer,
                                        &pointee_oomir_ty,
                                        &value_name,
                                        &mut instructions,
                                    )
                                } else {
                                    // Fat slice references are already carried
                                    // as SliceView values rather than Pointer.
                                    match pointer {
                                        oomir::Operand::Variable { name, .. } => {
                                            oomir::Operand::Variable {
                                                name,
                                                ty: pointee_oomir_ty.clone(),
                                            }
                                        }
                                        other => other,
                                    }
                                };
                                emit_rust_drop_value(
                                    pointee_ty,
                                    value,
                                    &format!("{label}_drop_in_place"),
                                    tcx,
                                    instance,
                                    data_types,
                                    &mut instructions,
                                );
                            }
                        } else if (is_diagnostic_item(sym::ptr_from_ref)
                            || intrinsic_name.as_str() == "from_mut")
                            && is_core_ptr
                        {
                            if let Some(dest) = effective_dest.clone() {
                                instructions.push(oomir::Instruction::Move {
                                    dest,
                                    src: oomir_operands[0].clone(),
                                });
                            }
                        } else if (has_diagnostic_item("ptr_eq")
                            || intrinsic_name.as_str() == "addr_eq")
                            && is_core_ptr
                            && oomir_operands.len() == 2
                        {
                            let first_ty = oomir_operands[0]
                                .get_type()
                                .expect("pointer equality operand is typed");
                            let second_ty = oomir_operands[1]
                                .get_type()
                                .expect("pointer equality operand is typed");
                            if matches!(first_ty, oomir::Type::Slice(_) | oomir::Type::Str) {
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: if intrinsic_name.as_str() == "addr_eq" {
                                        "fatPointerSameAddress".to_string()
                                    } else {
                                        "fatPointerEquals".to_string()
                                    },
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            (
                                                "left".to_string(),
                                                oomir::Type::Class("java/lang/Object".to_string()),
                                            ),
                                            (
                                                "right".to_string(),
                                                oomir::Type::Class("java/lang/Object".to_string()),
                                            ),
                                        ],
                                        ret: Box::new(oomir::Type::Boolean),
                                        is_static: true,
                                    },
                                    args: oomir_operands[..2].to_vec(),
                                    dest: effective_dest.clone(),
                                });
                            } else {
                                instructions.push(oomir::Instruction::InvokeVirtual {
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "sameAddress".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            ("self".to_string(), first_ty),
                                            ("other".to_string(), second_ty),
                                        ],
                                        ret: Box::new(oomir::Type::Boolean),
                                        is_static: false,
                                    },
                                    args: vec![oomir_operands[1].clone()],
                                    dest: effective_dest.clone(),
                                    operand: oomir_operands[0].clone(),
                                });
                            }
                        } else if matches!(intrinsic_name.as_str(), "dangling" | "dangling_mut")
                            && is_external_intrinsic
                            && matches!(fn_output.kind(), TyKind::RawPtr(_, _))
                        {
                            let TyKind::RawPtr(pointee, _) = fn_output.kind() else {
                                unreachable!()
                            };
                            let pointee_size = super::types::layout_size_bytes(tcx, *pointee)
                                .unwrap_or_else(|error| {
                                    panic!("could not determine dangling pointer size: {error}")
                                });
                            let pointee_alignment = super::types::layout_align_bytes(tcx, *pointee)
                                .unwrap_or_else(|error| {
                                    panic!(
                                        "could not determine dangling pointer alignment: {error}"
                                    )
                                });
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: effective_dest.clone(),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "withoutProvenance".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("address".to_string(), oomir::Type::U64),
                                        ("view_size".to_string(), oomir::Type::U64),
                                    ],
                                    ret: method_signature.ret.clone(),
                                    is_static: true,
                                },
                                args: vec![
                                    oomir::Operand::Constant(oomir::Constant::U64(
                                        pointee_alignment as u64,
                                    )),
                                    oomir::Operand::Constant(oomir::Constant::U64(
                                        u64::try_from(pointee_size)
                                            .expect("Rust pointee layout exceeds u64"),
                                    )),
                                ],
                            });
                        } else if (has_diagnostic_item("ptr_slice_from_raw_parts")
                            || has_diagnostic_item("ptr_slice_from_raw_parts_mut")
                            || matches!(
                                intrinsic_name.as_str(),
                                "from_raw_parts" | "from_raw_parts_mut"
                            ))
                            && is_core_ptr
                            && matches!(oomir_output_type, oomir::Type::Slice(_) | oomir::Type::Str)
                        {
                            let data = oomir_operands[0].clone();
                            if let Some(dest) = effective_dest.clone() {
                                let is_str = matches!(oomir_output_type, oomir::Type::Str);
                                let pointee_ty = match fn_output.kind() {
                                    TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => {
                                        *pointee
                                    }
                                    other => panic!(
                                        "raw slice constructor returned unexpected type {other:?}"
                                    ),
                                };
                                let element_ty = match pointee_ty.kind() {
                                    TyKind::Slice(element_ty) => *element_ty,
                                    TyKind::Str => tcx.types.u8,
                                    other => panic!(
                                        "raw slice constructor returned unexpected pointee {other:?}"
                                    ),
                                };
                                let element_size = super::types::layout_size_bytes(tcx, element_ty)
                                    .unwrap_or_else(|error| {
                                        panic!(
                                            "could not determine raw slice element layout: {error}"
                                        )
                                    });
                                let data = super::place::emit_retyped_slice_data_pointer(
                                    data,
                                    oomir::Operand::Constant(oomir::Constant::U64(
                                        u64::try_from(element_size)
                                            .expect("Rust slice element layout exceeds u64"),
                                    )),
                                    super::types::pointer_view_codec_operand(
                                        element_ty, tcx, data_types, instance,
                                    ),
                                    &format!("{label}_raw_slice"),
                                    &mut instructions,
                                );
                                let view_class = if is_str {
                                    oomir::UTF8_VIEW_CLASS
                                } else {
                                    oomir::SLICE_VIEW_CLASS
                                };
                                let (backing, offset) = super::place::emit_pointer_slice_parts(
                                    data,
                                    &format!("{label}_raw_slice"),
                                    &mut instructions,
                                );
                                let slice_object = format!("{label}_raw_slice_object");
                                instructions.push(oomir::Instruction::ConstructObject {
                                    dest: slice_object.clone(),
                                    class_name: view_class.to_string(),
                                    args: vec![
                                        (
                                            backing,
                                            oomir::Type::Class("java/lang/Object".to_string()),
                                        ),
                                        (offset, oomir::Type::I32),
                                        (oomir_operands[1].clone(), oomir::Type::I32),
                                    ],
                                });
                                instructions.push(oomir::Instruction::Cast {
                                    op: oomir::Operand::Variable {
                                        name: slice_object,
                                        ty: oomir::Type::Class(view_class.to_string()),
                                    },
                                    ty: oomir_output_type.clone(),
                                    dest,
                                });
                            }
                        } else if is_ptr_metadata
                            && is_core_ptr
                            && (matches!(
                                oomir_operands[0].get_type(),
                                Some(oomir::Type::Slice(_) | oomir::Type::Str)
                            ) || fn_inputs.first().is_some_and(|input| {
                                matches!(
                                    input.kind(),
                                    TyKind::RawPtr(pointee, _)
                                        if pointee.is_slice() || pointee.is_str()
                                )
                            }))
                        {
                            if let Some(dest) = effective_dest.clone() {
                                let length_dest = if oomir_output_type == oomir::Type::I32 {
                                    dest.clone()
                                } else {
                                    format!("{dest}_slice_metadata_i32")
                                };
                                instructions.push(oomir::Instruction::GetField {
                                    dest: length_dest.clone(),
                                    object: oomir_operands[0].clone(),
                                    field_name: "length".to_string(),
                                    field_ty: oomir::Type::I32,
                                    owner_class: oomir::SLICE_VIEW_CLASS.to_string(),
                                });
                                if oomir_output_type != oomir::Type::I32 {
                                    instructions.push(oomir::Instruction::Cast {
                                        op: oomir::Operand::Variable {
                                            name: length_dest,
                                            ty: oomir::Type::I32,
                                        },
                                        ty: oomir_output_type.clone(),
                                        dest,
                                    });
                                }
                            }
                        } else if is_ptr_metadata
                            && is_core_ptr
                            && matches!(oomir_operands[0].get_type(), Some(oomir::Type::Pointer(_)))
                        {
                            if let Some(dest) = effective_dest.clone() {
                                let pointee = fn_inputs.first().and_then(|input| {
                                    let (TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _)) =
                                        input.kind()
                                    else {
                                        return None;
                                    };
                                    Some(*pointee)
                                });
                                let slice_tailed_pointee = pointee.and_then(|pointee| {
                                    let tail = tcx.struct_tail_for_codegen(
                                        pointee,
                                        TypingEnv::fully_monomorphized(),
                                    );
                                    (tail.is_slice() || tail.is_str()).then_some(pointee)
                                });
                                if slice_tailed_pointee.is_some() {
                                    let pointer_ty = oomir_operands[0]
                                        .get_type()
                                        .expect("DST metadata operand must be typed");
                                    instructions.push(oomir::Instruction::InvokeVirtual {
                                        dest: Some(dest),
                                        class_name: oomir::POINTER_CLASS.to_string(),
                                        method_name: "metadata".to_string(),
                                        method_ty: oomir::Signature {
                                            params: vec![("self".to_string(), pointer_ty)],
                                            ret: Box::new(oomir_output_type.clone()),
                                            is_static: false,
                                        },
                                        args: Vec::new(),
                                        operand: oomir_operands[0].clone(),
                                    });
                                } else if pointee.is_some_and(|pointee| {
                                    matches!(pointee.kind(), TyKind::Dynamic(..))
                                }) {
                                    emit_trait_object_metadata(
                                        oomir_operands[0].clone(),
                                        &oomir_output_type,
                                        dest,
                                        &format!("{label}_trait_metadata"),
                                        data_types,
                                        &mut instructions,
                                    );
                                } else {
                                    instructions.push(oomir::Instruction::Move {
                                        dest,
                                        src: oomir::Operand::Constant(oomir::Constant::Null(
                                            oomir_output_type.clone(),
                                        )),
                                    });
                                }
                            }
                        } else if matches!(
                            intrinsic_name.as_str(),
                            "from_raw_parts" | "from_raw_parts_mut"
                        ) && is_core_ptr
                            && matches!(oomir_output_type, oomir::Type::Pointer(_))
                        {
                            let pointee = match fn_output.kind() {
                                TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
                                other => {
                                    panic!("from_raw_parts returned non-pointer type {other:?}")
                                }
                            };
                            let source_ty = oomir_operands[0]
                                .get_type()
                                .expect("from_raw_parts data pointer is typed");
                            if matches!(pointee.kind(), TyKind::Dynamic(_, _)) {
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    dest: effective_dest.clone(),
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "restoreErasedView".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![("pointer".to_string(), source_ty)],
                                        ret: method_signature.ret.clone(),
                                        is_static: true,
                                    },
                                    args: vec![oomir_operands[0].clone()],
                                });
                            } else {
                                let tail = tcx.struct_tail_for_codegen(
                                    pointee,
                                    TypingEnv::fully_monomorphized(),
                                );
                                let carries_slice_metadata = tail.is_slice() || tail.is_str();
                                let pointee_size = super::types::layout_size_bytes(tcx, pointee)
                                    .unwrap_or_else(|error| {
                                        panic!(
                                            "could not determine from_raw_parts pointee size: {error}"
                                        )
                                    });
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    dest: effective_dest.clone(),
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: if carries_slice_metadata {
                                        "retypeWithMetadata".to_string()
                                    } else {
                                        "retype".to_string()
                                    },
                                    method_ty: oomir::Signature {
                                        params: {
                                            let mut params = vec![
                                                ("pointer".to_string(), source_ty),
                                                ("view_size".to_string(), oomir::Type::U64),
                                                (
                                                    "view_codec".to_string(),
                                                    oomir::Type::java_string(),
                                                ),
                                            ];
                                            if carries_slice_metadata {
                                                params.push((
                                                    "metadata".to_string(),
                                                    oomir::Type::U64,
                                                ));
                                            }
                                            params
                                        },
                                        ret: method_signature.ret.clone(),
                                        is_static: true,
                                    },
                                    args: {
                                        let mut args = vec![
                                            oomir_operands[0].clone(),
                                            oomir::Operand::Constant(oomir::Constant::U64(
                                                u64::try_from(pointee_size).expect(
                                                    "Rust pointer target layout exceeds u64",
                                                ),
                                            )),
                                            super::types::pointer_view_codec_operand(
                                                pointee, tcx, data_types, instance,
                                            ),
                                        ];
                                        if carries_slice_metadata {
                                            args.push(oomir_operands[1].clone());
                                        }
                                        args
                                    },
                                });
                            }
                        } else if (is_diagnostic_item(sym::ptr_null)
                            || is_diagnostic_item(sym::ptr_null_mut))
                            && (is_external_intrinsic || is_core_crate_item)
                            && matches!(fn_output.kind(), TyKind::RawPtr(_, _))
                        {
                            let TyKind::RawPtr(pointee, _) = fn_output.kind() else {
                                unreachable!()
                            };
                            let pointee_size = super::types::layout_size_bytes(tcx, *pointee)
                                .unwrap_or_else(|error| {
                                    panic!("could not determine null pointer target size: {error}")
                                });
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: effective_dest.clone(),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "nullPointer".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![("view_size".to_string(), oomir::Type::U64)],
                                    ret: method_signature.ret.clone(),
                                    is_static: true,
                                },
                                args: vec![oomir::Operand::Constant(oomir::Constant::U64(
                                    u64::try_from(pointee_size)
                                        .expect("Rust null pointer target layout exceeds u64"),
                                ))],
                            });
                        } else if (has_diagnostic_item("ptr_without_provenance")
                            || has_diagnostic_item("ptr_without_provenance_mut")
                            || matches!(
                                intrinsic_name.as_str(),
                                "with_exposed_provenance" | "with_exposed_provenance_mut"
                            ))
                            && is_external_intrinsic
                            && matches!(fn_output.kind(), TyKind::RawPtr(_, _))
                        {
                            let TyKind::RawPtr(pointee, _) = fn_output.kind() else {
                                unreachable!()
                            };
                            let pointee_size = super::types::layout_size_bytes(tcx, *pointee)
                                .unwrap_or_else(|error| {
                                    panic!(
                                        "could not determine provenance pointer target size: {error}"
                                    )
                                });
                            instructions.push(oomir::Instruction::InvokeStatic {
                                dest: effective_dest.clone(),
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: if intrinsic_name.starts_with("without_") {
                                    "withoutProvenance".to_string()
                                } else {
                                    "fromAddress".to_string()
                                },
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("address".to_string(), oomir::Type::U64),
                                        ("view_size".to_string(), oomir::Type::U64),
                                        ("view_codec".to_string(), oomir::Type::java_string()),
                                    ],
                                    ret: method_signature.ret.clone(),
                                    is_static: true,
                                },
                                args: vec![
                                    oomir_operands[0].clone(),
                                    oomir::Operand::Constant(oomir::Constant::U64(
                                        u64::try_from(pointee_size)
                                            .expect("Rust pointee layout exceeds u64"),
                                    )),
                                    super::types::pointer_view_codec_operand(
                                        *pointee, tcx, data_types, instance,
                                    ),
                                ],
                            });
                        } else if (is_diagnostic_item(sym::ptr_read)
                            || is_diagnostic_item(sym::ptr_read_unaligned)
                            || has_diagnostic_item("ptr_read_volatile")
                            || (is_compiler_intrinsic
                                && matches!(
                                    intrinsic_name.as_str(),
                                    "volatile_load" | "unaligned_volatile_load"
                                )))
                            && is_core_ptr
                        {
                            if has_diagnostic_item("ptr_read_volatile")
                                || (is_compiler_intrinsic
                                    && matches!(
                                        intrinsic_name.as_str(),
                                        "volatile_load" | "unaligned_volatile_load"
                                    ))
                            {
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "volatileFence".to_string(),
                                    method_ty: oomir::Signature {
                                        params: Vec::new(),
                                        ret: Box::new(oomir::Type::Void),
                                        is_static: true,
                                    },
                                    args: Vec::new(),
                                    dest: None,
                                });
                            }
                            if let Some(dest) = effective_dest.clone()
                                && let Some(oomir::Type::Pointer(pointee)) =
                                    oomir_operands[0].get_type()
                            {
                                super::place::emit_pointer_read_copy(
                                    oomir_operands[0].clone(),
                                    pointee.as_ref(),
                                    &dest,
                                    &mut instructions,
                                );
                            }
                        } else if (is_diagnostic_item(sym::ptr_write)
                            || is_diagnostic_item(sym::ptr_write_unaligned)
                            || is_diagnostic_item(sym::ptr_write_volatile)
                            || (is_compiler_intrinsic
                                && matches!(
                                    intrinsic_name.as_str(),
                                    "volatile_store" | "unaligned_volatile_store"
                                )))
                            && is_core_ptr
                        {
                            if let Some(oomir::Type::Pointer(pointee)) =
                                oomir_operands[0].get_type()
                            {
                                super::place::emit_pointer_write(
                                    oomir_operands[0].clone(),
                                    pointee.as_ref(),
                                    oomir_operands[1].clone(),
                                    &mut instructions,
                                );
                            }
                            if is_diagnostic_item(sym::ptr_write_volatile)
                                || (is_compiler_intrinsic
                                    && matches!(
                                        intrinsic_name.as_str(),
                                        "volatile_store" | "unaligned_volatile_store"
                                    ))
                            {
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "volatileFence".to_string(),
                                    method_ty: oomir::Signature {
                                        params: Vec::new(),
                                        ret: Box::new(oomir::Type::Void),
                                        is_static: true,
                                    },
                                    args: Vec::new(),
                                    dest: None,
                                });
                            }
                        } else if is_diagnostic_item(sym::ptr_replace) && is_core_ptr {
                            if let Some(oomir::Type::Pointer(pointee)) =
                                oomir_operands[0].get_type()
                            {
                                if let Some(dest) = effective_dest.clone() {
                                    super::place::emit_pointer_read(
                                        oomir_operands[0].clone(),
                                        pointee.as_ref(),
                                        &dest,
                                        &mut instructions,
                                    );
                                }
                                super::place::emit_pointer_write(
                                    oomir_operands[0].clone(),
                                    pointee.as_ref(),
                                    oomir_operands[1].clone(),
                                    &mut instructions,
                                );
                            }
                        // This private `core` helper has no diagnostic item. It is reached by
                        // optimized `ptr::swap_nonoverlapping` for dynamically sized byte counts.
                        } else if intrinsic_name.as_str() == "swap_nonoverlapping_bytes"
                            && is_core_ptr
                            && oomir_operands.len() == 3
                        {
                            instructions.push(oomir::Instruction::InvokeStatic {
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name: "swapNonOverlappingNonZero".to_string(),
                                method_ty: oomir::Signature {
                                    params: vec![
                                        (
                                            "left".to_string(),
                                            oomir_operands[0]
                                                .get_type()
                                                .expect("swap left pointer is typed"),
                                        ),
                                        (
                                            "right".to_string(),
                                            oomir_operands[1]
                                                .get_type()
                                                .expect("swap right pointer is typed"),
                                        ),
                                        (
                                            "byte_count".to_string(),
                                            oomir::Type::Class("java/lang/Object".to_string()),
                                        ),
                                    ],
                                    ret: Box::new(oomir::Type::Void),
                                    is_static: true,
                                },
                                args: oomir_operands[..3].to_vec(),
                                dest: None,
                            });
                        } else if (is_diagnostic_item(sym::ptr_swap)
                            || (is_compiler_intrinsic
                                && intrinsic_name.as_str() == "typed_swap_nonoverlapping"))
                            && is_core_ptr
                        {
                            if let Some(oomir::Type::Pointer(pointee)) =
                                oomir_operands[0].get_type()
                            {
                                let left_name = format!("{}_swap_left", label);
                                let right_name = format!("{}_swap_right", label);
                                let left = super::place::emit_pointer_read(
                                    oomir_operands[0].clone(),
                                    pointee.as_ref(),
                                    &left_name,
                                    &mut instructions,
                                );
                                let right = super::place::emit_pointer_read(
                                    oomir_operands[1].clone(),
                                    pointee.as_ref(),
                                    &right_name,
                                    &mut instructions,
                                );
                                super::place::emit_pointer_write(
                                    oomir_operands[0].clone(),
                                    pointee.as_ref(),
                                    right,
                                    &mut instructions,
                                );
                                super::place::emit_pointer_write(
                                    oomir_operands[1].clone(),
                                    pointee.as_ref(),
                                    left,
                                    &mut instructions,
                                );
                            }
                        } else if is_diagnostic_item(sym::ptr_swap_nonoverlapping) && is_core_ptr {
                            let element_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("swap_nonoverlapping has a type argument");
                            let (method_name, count) = if let Ok(element_size) =
                                super::types::layout_size_bytes(tcx, element_ty)
                            {
                                let byte_count_name =
                                    format!("{label}_swap_nonoverlapping_byte_count");
                                instructions.push(oomir::Instruction::Mul {
                                    dest: byte_count_name.clone(),
                                    op1: oomir_operands[2].clone(),
                                    op2: oomir::Operand::Constant(oomir::Constant::U64(
                                        element_size as u64,
                                    )),
                                });
                                (
                                    "swapNonOverlapping".to_string(),
                                    oomir::Operand::Variable {
                                        name: byte_count_name,
                                        ty: oomir::Type::U64,
                                    },
                                )
                            } else {
                                (
                                    "swapNonOverlappingElements".to_string(),
                                    oomir_operands[2].clone(),
                                )
                            };
                            let left_ty = oomir_operands[0]
                                .get_type()
                                .expect("swap_nonoverlapping left pointer is typed");
                            let right_ty = oomir_operands[1]
                                .get_type()
                                .expect("swap_nonoverlapping right pointer is typed");
                            instructions.push(oomir::Instruction::InvokeStatic {
                                class_name: oomir::POINTER_CLASS.to_string(),
                                method_name,
                                method_ty: oomir::Signature {
                                    params: vec![
                                        ("left".to_string(), left_ty),
                                        ("right".to_string(), right_ty),
                                        ("byte_count".to_string(), oomir::Type::U64),
                                    ],
                                    ret: Box::new(oomir::Type::Void),
                                    is_static: true,
                                },
                                args: vec![
                                    oomir_operands[0].clone(),
                                    oomir_operands[1].clone(),
                                    count,
                                ],
                                dest: None,
                            });
                        } else if (is_diagnostic_item(sym::ptr_copy)
                            || is_diagnostic_item(sym::ptr_copy_nonoverlapping)
                            || is_diagnostic_item(sym::ptr_write_bytes)
                            || (is_compiler_intrinsic
                                && matches!(
                                    intrinsic_name.as_str(),
                                    "copy" | "copy_nonoverlapping" | "write_bytes"
                                )))
                            && is_core_ptr
                        {
                            let writes_bytes = is_diagnostic_item(sym::ptr_write_bytes)
                                || (is_compiler_intrinsic
                                    && intrinsic_name.as_str() == "write_bytes");
                            let nonoverlapping = is_diagnostic_item(sym::ptr_copy_nonoverlapping)
                                || (is_compiler_intrinsic
                                    && intrinsic_name.as_str() == "copy_nonoverlapping");
                            let element_ty = func_instance
                                .args
                                .types()
                                .next()
                                .expect("pointer memory intrinsic has a type argument");
                            let count = oomir_operands
                                .last()
                                .cloned()
                                .expect("pointer memory intrinsic has a count argument");
                            let (uses_runtime_element_size, count) = if let Ok(element_size) =
                                super::types::layout_size_bytes(tcx, element_ty)
                            {
                                let byte_count_name = format!("{label}_pointer_memory_byte_count");
                                instructions.push(oomir::Instruction::Mul {
                                    dest: byte_count_name.clone(),
                                    op1: count,
                                    op2: oomir::Operand::Constant(oomir::Constant::U64(
                                        element_size as u64,
                                    )),
                                });
                                (
                                    false,
                                    oomir::Operand::Variable {
                                        name: byte_count_name,
                                        ty: oomir::Type::U64,
                                    },
                                )
                            } else {
                                (true, count)
                            };
                            let first_pointer_ty = oomir_operands
                                .first()
                                .and_then(oomir::Operand::get_type)
                                .expect(
                                    "pointer memory intrinsic has a destination/source pointer",
                                );
                            if writes_bytes {
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: if uses_runtime_element_size {
                                        "writeElements".to_string()
                                    } else {
                                        "writeBytes".to_string()
                                    },
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            ("destination".to_string(), first_pointer_ty),
                                            ("value".to_string(), oomir::Type::I32),
                                            ("byte_count".to_string(), oomir::Type::U64),
                                        ],
                                        ret: Box::new(oomir::Type::Void),
                                        is_static: true,
                                    },
                                    args: vec![
                                        oomir_operands[0].clone(),
                                        oomir_operands[1].clone(),
                                        count,
                                    ],
                                    dest: None,
                                });
                            } else {
                                let second_pointer_ty = oomir_operands[1]
                                    .get_type()
                                    .expect("copy intrinsic has a destination pointer");
                                instructions.push(oomir::Instruction::InvokeStatic {
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: if nonoverlapping && uses_runtime_element_size {
                                        "copyNonOverlappingElements".to_string()
                                    } else if nonoverlapping {
                                        "copyNonOverlapping".to_string()
                                    } else if uses_runtime_element_size {
                                        "copyElements".to_string()
                                    } else {
                                        "copy".to_string()
                                    },
                                    method_ty: oomir::Signature {
                                        params: vec![
                                            ("source".to_string(), first_pointer_ty),
                                            ("destination".to_string(), second_pointer_ty),
                                            ("byte_count".to_string(), oomir::Type::U64),
                                        ],
                                        ret: Box::new(oomir::Type::Void),
                                        is_static: true,
                                    },
                                    args: vec![
                                        oomir_operands[0].clone(),
                                        oomir_operands[1].clone(),
                                        count,
                                    ],
                                    dest: None,
                                });
                            }
                        } else {
                            let is_closure_call = matches!(instance_ty.kind(), TyKind::Closure(..));
                            let closure_has_captures = matches!(
                                instance_ty.kind(),
                                TyKind::Closure(_, args)
                                    if args.as_closure().upvar_tys().iter().next().is_some()
                            );
                            let (class_name, function) = if is_closure_call {
                                (
                                    super::naming::mono_owner_class(tcx, func_instance),
                                    super::generate_closure_function_name(tcx, func_instance),
                                )
                            } else {
                                let fn_name_data =
                                    super::naming::mono_fn_name_from_instance(tcx, func_instance);
                                (
                                    fn_name_data
                                        .class_to_call_on
                                        .expect("monomorphized functions have JVM owners"),
                                    fn_name_data.method_name,
                                )
                            };
                            if super::naming::is_global_link_symbol_class(&class_name) {
                                for (index, input_ty) in fn_inputs.iter().enumerate() {
                                    let TyKind::Adt(adt_def, _) = input_ty.kind() else {
                                        continue;
                                    };
                                    if !tcx.is_diagnostic_item(sym::NonNull, adt_def.did()) {
                                        continue;
                                    }
                                    let Some(operand) = oomir_operands.get(index).cloned() else {
                                        continue;
                                    };
                                    let Some(oomir::Type::Class(wrapper_class)) =
                                        operand.get_type()
                                    else {
                                        continue;
                                    };
                                    let pointer_ty = match data_types.get(&wrapper_class) {
                                        Some(oomir::DataType::Class { fields, .. }) => fields
                                            .iter()
                                            .find(|(field_name, _)| field_name == "pointer")
                                            .map(|(_, field_ty)| field_ty.clone())
                                            .expect("NonNull carrier has a pointer field"),
                                        _ => panic!(
                                            "NonNull carrier class {wrapper_class} was not generated"
                                        ),
                                    };
                                    let unwrapped = format!("{label}_global_abi_arg_{index}");
                                    instructions.push(oomir::Instruction::GetField {
                                        dest: unwrapped.clone(),
                                        object: operand,
                                        field_name: "pointer".to_string(),
                                        field_ty: pointer_ty.clone(),
                                        owner_class: wrapper_class,
                                    });
                                    oomir_operands[index] = oomir::Operand::Variable {
                                        name: unwrapped,
                                        ty: pointer_ty.clone(),
                                    };
                                    method_signature.params[index].1 = pointer_ty;
                                }
                            }
                            let call_args = if is_closure_call && !oomir_operands.is_empty() {
                                if closure_has_captures {
                                    oomir_operands.clone()
                                } else {
                                    oomir_operands[1..].to_vec()
                                }
                            } else {
                                oomir_operands.clone()
                            };
                            if closure_has_captures {
                                let closure_env_ty = oomir_operands
                                    .first()
                                    .and_then(oomir::Operand::get_type)
                                    .expect("capturing closure calls have an environment operand");
                                method_signature
                                    .params
                                    .insert(0, ("closure_env".to_string(), closure_env_ty));
                            }
                            method_signature.is_static = true;

                            instructions.push(oomir::Instruction::InvokeStatic {
                                class_name,
                                method_name: function,
                                method_ty: method_signature,
                                args: call_args,
                                dest: effective_dest, // use effective_dest
                            });
                        }
                    }
                } else {
                    let indirect_sig = func_ty.fn_sig(tcx).skip_binder();
                    for (index, target_ty) in indirect_sig.inputs().iter().enumerate() {
                        let Some(source) = oomir_operands.get(index).cloned() else {
                            break;
                        };
                        oomir_operands[index] = super::value_repr::adapt_operand_to_rust_type(
                            source,
                            *target_ty,
                            &format!("{}_indirect_arg_{}", label, index),
                            tcx,
                            instance,
                            data_types,
                            &mut instructions,
                        );
                    }
                    let func_oomir_operand =
                        convert_operand(&func, tcx, instance, mir, data_types, &mut instructions);

                    let oomir_sig =
                        super::types::fn_ptr_signature_from_ty(func_ty, tcx, data_types, instance);
                    super::types::ensure_fn_ptr_interface(&oomir_sig, data_types, tcx, instance);

                    let effective_dest = if !oomir_sig.ret.has_jvm_value() {
                        None
                    } else {
                        dest_var_name.clone()
                    };

                    instructions.push(oomir::Instruction::CallIndirect {
                        dest: effective_dest,
                        function_ptr: Box::new(func_oomir_operand),
                        args: oomir_operands.clone(),
                        signature: oomir_sig,
                    });
                }

                if deferred_destination_store && let Some(result_name) = dest_var_name {
                    instructions.extend(emit_instructions_to_set_value(
                        destination,
                        oomir::Operand::Variable {
                            name: result_name,
                            ty: destination_oomir_type,
                        },
                        tcx,
                        instance,
                        mir,
                        data_types,
                    ));
                }

                let destination_ty = destination.ty(&mir.local_decls, tcx).ty;
                if destination.projection.is_empty()
                    && matches!(
                        destination_ty.kind(),
                        TyKind::Ref(_, _, mutability) if mutability.is_mut()
                    )
                {
                    let aliases = args
                        .iter()
                        .filter_map(|arg| match &arg.node {
                            MirOperand::Move(place) | MirOperand::Copy(place)
                                if place.projection.is_empty() =>
                            {
                                mutable_borrow_arrays.get(&place.local).cloned()
                            }
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    // A returned `&mut U` is only a plain reborrow of an input
                    // `&mut T` when U and T are the same representation. Methods
                    // such as `ManuallyDrop::deref_mut` return a pointer derived
                    // from a field of `T`; that Pointer already carries the field
                    // address and must not be copied back as though it were T.
                    if let [alias] = aliases.as_slice()
                        && destination_pointer_pointee.as_ref() == Some(&alias.pointee_type)
                    {
                        mutable_borrow_arrays.insert(
                            destination.local,
                            PointerOrigin {
                                original_place: alias.original_place.clone(),
                                carrier_name: super::place::place_to_string(destination, tcx),
                                pointee_type: alias.pointee_type.clone(),
                                writable: alias.writable,
                            },
                        );
                        initialized_borrows.insert(destination.local);
                    }
                }

                let mut writeback_locals = HashSet::new();
                for arg in args {
                    if let MirOperand::Move(place) | MirOperand::Copy(place) = &arg.node
                        && place.projection.is_empty()
                        && mutable_borrow_arrays.contains_key(&place.local)
                    {
                        writeback_locals.insert(place.local);
                    }
                }
                let mut writeback_locals = writeback_locals.into_iter().collect::<Vec<_>>();
                writeback_locals.sort_by_key(|local| local.index());
                instructions.extend(emit_selected_mutable_borrow_writebacks(
                    writeback_locals,
                    mutable_borrow_arrays,
                    tcx,
                    instance,
                    mir,
                    data_types,
                ));

                if let Some(target_bb) = target {
                    let target_label = format!("bb{}", target_bb.index());
                    instructions.push(oomir::Instruction::Jump {
                        target: target_label,
                    });
                } else {
                    instructions.push(oomir::Instruction::ThrowNewWithMessage {
                        exception_class: "java/lang/AssertionError".to_string(),
                        message: "Diverging Rust call returned unexpectedly".to_string(),
                    });
                }
            }
            TerminatorKind::Assert {
                target,
                cond,
                expected,
                msg,
                unwind: _,
            } => {
                let condition_operand: oomir::Operand;

                // Check if the condition operand is a direct use of a place (Copy or Move)
                let condition_place_opt = match cond {
                    MirOperand::Copy(place) | MirOperand::Move(place) => Some(place),
                    _ => None, // If it's a constant, handle directly
                };

                if let Some(place) = condition_place_opt {
                    // Now, check if this place has a field projection
                    let (temp_dest, instrs, field_oomir_type) =
                        emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
                    instructions.extend(instrs);
                    // Use the temporary variable as the condition operand
                    condition_operand = oomir::Operand::Variable {
                        name: temp_dest.clone(),
                        ty: field_oomir_type,
                    };
                } else {
                    breadcrumbs::log!(
                        breadcrumbs::LogLevel::Info,
                        "mir-lowering",
                        format!("Info: Assert condition uses constant operand {:?}", cond)
                    );
                    // Condition is likely a constant itself
                    condition_operand =
                        convert_operand(cond, tcx, instance, mir, data_types, &mut instructions);
                }

                // The MIR assert checks `!cond == expected`. Rust asserts check `cond == expected`.
                // Standard Rust `assert!(expr)` lowers to MIR `assert(expr, expected: true, ...)`
                // Standard Rust `assert_eq!(a,b)` might lower differently, but `assert!(a==b)` lowers like above.
                // The `checked_add` MIR uses `assert(!move (_7.1: bool), expected: true, ...)` effectively meaning "panic if _7.1 is true".
                // So, we need to check if `condition_operand == *expected`.

                // Generate a comparison instruction to check if the *actual condition value*
                // matches the expected boolean value.
                let comparison_dest = format!("assert_cmp_{}", bb.index()); // e.g., assert_cmp_3

                // Handle potential negation: MIR `assert(!cond)` means panic if `cond` is true.
                // MIR `assert(cond)` means panic if `cond` is false.
                // The `expected` field tells us what the non-panic value should be.
                // We want to branch to the failure block if `condition_operand != expected`.

                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!(
                        "Info: Generating Assert comparison: '{}' = ({:?}) == {:?}",
                        comparison_dest, condition_operand, *expected
                    )
                );

                instructions.push(oomir::Instruction::Eq {
                    dest: comparison_dest.clone(),
                    op1: condition_operand, // Use the potentially GetField'd value
                    op2: oomir::Operand::Constant(oomir::Constant::Boolean(*expected)),
                });

                // Generate a branch based on the comparison result
                let success_block = format!("bb{}", target.index()); // Success path
                let failure_block = format!("assert_fail_{}", bb.index()); // Failure path label

                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!(
                        "Info: Generating Assert branch: if '{}' == true goto {} else goto {}",
                        comparison_dest, success_block, failure_block
                    )
                );

                instructions.push(oomir::Instruction::Branch {
                    condition: oomir::Operand::Variable {
                        name: comparison_dest, // Use the result of the Eq comparison
                        ty: oomir::Type::Boolean,
                    },
                    true_block: success_block, // Jump here if condition == expected (assertion holds)
                    false_block: failure_block.clone(), // Jump here if assertion fails
                });

                // Extract the message. msg is an AssertMessage.
                // We need to handle different kinds of AssertMessage.
                let panic_message = match &**msg {
                    rustc_middle::mir::AssertKind::BoundsCheck { len, index } => {
                        // TODO: More sophisticated message generation using len/index operands later
                        format!("BoundsCheck failed (len: {:?}, index: {:?})", len, index)
                    }
                    rustc_middle::mir::AssertKind::Overflow(op, l, r) => {
                        // TODO: Convert l and r operands to strings if possible later
                        format!("Overflow({:?}, {:?}, {:?})", op, l, r)
                    }
                    rustc_middle::mir::AssertKind::OverflowNeg(op) => {
                        format!("OverflowNeg({:?})", op)
                    }
                    rustc_middle::mir::AssertKind::DivisionByZero(op) => {
                        format!("DivisionByZero({:?})", op)
                    }
                    rustc_middle::mir::AssertKind::RemainderByZero(op) => {
                        format!("RemainderByZero({:?})", op)
                    }
                    rustc_middle::mir::AssertKind::ResumedAfterReturn(_) => {
                        "ResumedAfterReturn".to_string()
                    }
                    rustc_middle::mir::AssertKind::ResumedAfterPanic(_) => {
                        "ResumedAfterPanic".to_string()
                    }
                    rustc_middle::mir::AssertKind::MisalignedPointerDereference {
                        required,
                        found,
                    } => {
                        format!(
                            "MisalignedPointerDereference (required: {:?}, found: {:?})",
                            required, found
                        )
                    }
                    rustc_middle::mir::AssertKind::NullPointerDereference => {
                        "NullPointerDereference".to_string()
                    }
                    rustc_middle::mir::AssertKind::NullReferenceConstructed => {
                        "NullReferenceConstructed".to_string()
                    }
                    rustc_middle::mir::AssertKind::ResumedAfterDrop(_) => {
                        "ResumedAfterDrop".to_string()
                    }
                    rustc_middle::mir::AssertKind::InvalidEnumConstruction(_) => {
                        "InvalidEnumConstruction".to_string()
                    }
                };

                let mut fail_instructions = Vec::new();
                if let Some(location) =
                    super::source_location(tcx, mir.span, terminator.source_info.span)
                {
                    fail_instructions.push(oomir::Instruction::SourceLocation(location));
                }
                fail_instructions.push(super::local_variable_scope(
                    debug_scope_cache,
                    terminator.source_info.scope,
                    &debug_local_collector.locals,
                    debug_variables,
                    debug_variable_scopes,
                ));
                fail_instructions.push(oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/RuntimeException".to_string(), // Or ArithmeticException for overflows?
                    message: panic_message,
                });
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "mir-lowering",
                    format!("Info: Creating failure block '{}'", failure_block)
                );
                basic_blocks.insert(
                    // Ensure 'basic_blocks' map is mutable and passed in
                    failure_block.clone(),
                    oomir::BasicBlock {
                        label: failure_block,
                        instructions: fail_instructions,
                    },
                );
            }
            TerminatorKind::Drop {
                place,
                target,
                unwind: _,
                replace: _,
                drop: _,
            } => {
                let rust_ty = EarlyBinder::bind(tcx, place.ty(&mir.local_decls, tcx).ty)
                    .instantiate(tcx, instance.args)
                    .skip_norm_wip();
                if rust_ty.needs_drop(tcx, TypingEnv::fully_monomorphized()) {
                    let (value_name, value_instructions, value_ty) =
                        emit_instructions_to_get_on_own(place, tcx, instance, mir, data_types);
                    instructions.extend(value_instructions);
                    emit_rust_drop_value(
                        rust_ty,
                        oomir::Operand::Variable {
                            name: value_name,
                            ty: value_ty,
                        },
                        &format!("{label}_drop"),
                        tcx,
                        instance,
                        data_types,
                        &mut instructions,
                    );
                }

                let target_label = format!("bb{}", target.index());
                instructions.push(oomir::Instruction::Jump {
                    target: target_label,
                });
            }
            TerminatorKind::Unreachable => {
                instructions.push(oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/RuntimeException".to_string(),
                    message: "Unreachable code reached".to_string(),
                });
            }
            TerminatorKind::UnwindResume => {
                instructions.push(oomir::Instruction::Rethrow);
            }
            TerminatorKind::UnwindTerminate(_) => {
                instructions.push(oomir::Instruction::InvokeStatic {
                    dest: None,
                    class_name: "org/rustlang/runtime/PanicSupport".to_string(),
                    method_name: "abort".to_string(),
                    method_ty: oomir::Signature {
                        params: vec![(
                            "failure".to_string(),
                            oomir::Type::Class("java/lang/Throwable".to_string()),
                        )],
                        ret: Box::new(oomir::Type::Void),
                        is_static: true,
                    },
                    args: vec![oomir::Operand::Variable {
                        name: "__rust_unwind_exception".to_string(),
                        ty: oomir::Type::Class("java/lang/Throwable".to_string()),
                    }],
                });
                instructions.push(oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/AssertionError".to_string(),
                    message: "Rust abort unexpectedly returned".to_string(),
                });
            }
            // Other terminator kinds will be added as needed.
            _ => {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Warn,
                    "mir-lowering",
                    format!("Warning: Unhandled terminator {:?}", terminator.kind)
                );
            }
        }
        if unwind_target.is_some() {
            instructions.push(oomir::Instruction::UnwindEnd);
        }
        if instructions.len() > instruction_start {
            let mut metadata = Vec::new();
            if let Some(location) =
                super::source_location(tcx, mir.span, terminator.source_info.span)
            {
                metadata.push(oomir::Instruction::SourceLocation(location));
            }
            metadata.push(super::local_variable_scope(
                debug_scope_cache,
                terminator.source_info.scope,
                &debug_local_collector.locals,
                debug_variables,
                debug_variable_scopes,
            ));
            instructions.splice(instruction_start..instruction_start, metadata);
        }
    }

    oomir::BasicBlock {
        label,
        instructions,
    }
}
