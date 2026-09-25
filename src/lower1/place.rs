use super::{
    jvm_names,
    operand::convert_operand,
    types::{
        adapt_simple_enum_operand, enum_variant_field_name, force_define_named_adt,
        generate_adt_jvm_class_name, get_field_name_from_index, jvm_subtype_payload_ty,
        pointer_view_codec_operand, ty_to_oomir_type, union_getter_method_name,
        union_setter_method_name,
    },
};
use crate::lower1::context::Definitions;
use crate::oomir::{self, Instruction, Operand};
use rustc_hash::FxHashMap as HashMap;
use rustc_middle::{
    mir::{Body, Local, Operand as MirOperand, Place, ProjectionElem},
    ty::{AdtDef, EarlyBinder, GenericArgsRef, Instance, Ty, TyCtxt, TyKind, TypingEnv},
};

pub(crate) fn has_slice_or_str_struct_tail<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> bool {
    if !matches!(ty.kind(), TyKind::Adt(adt_def, _) if adt_def.is_struct()) {
        return false;
    }
    tcx.try_normalize_erasing_regions(
        TypingEnv::fully_monomorphized(),
        rustc_middle::ty::Unnormalized::new_wip(ty),
    )
    .is_ok_and(|normalized| {
        let tail = tcx.struct_tail_for_codegen(normalized, TypingEnv::fully_monomorphized());
        tail.is_slice() || tail.is_str()
    })
}

pub fn place_to_string<'tcx>(place: &Place<'tcx>, _tcx: TyCtxt<'tcx>) -> String {
    // Base variable name (e.g., "_1")
    format!("_{}", place.local.index()) // Start with base local "_N"
}

pub(crate) fn local_cell_name(local: Local) -> String {
    format!("_cell_{}", local.index())
}

fn union_parts_from_ty<'tcx>(ty: Ty<'tcx>) -> Option<(AdtDef<'tcx>, GenericArgsRef<'tcx>)> {
    match ty.kind() {
        TyKind::Adt(adt_def, substs) if adt_def.is_union() => Some((*adt_def, substs)),
        TyKind::Ref(_, inner_ty, _) => match inner_ty.kind() {
            TyKind::Adt(adt_def, substs) if adt_def.is_union() => Some((*adt_def, substs)),
            _ => None,
        },
        _ => None,
    }
}

fn projection_prefix_place<'tcx>(
    place: &Place<'tcx>,
    proj_index: usize,
    tcx: TyCtxt<'tcx>,
) -> Place<'tcx> {
    Place {
        local: place.local,
        projection: tcx.mk_place_elems(&place.projection[..proj_index]),
    }
}

fn union_field_name<'tcx>(adt_def: AdtDef<'tcx>, field_index: usize, tcx: TyCtxt<'tcx>) -> String {
    adt_def
        .variant(0usize.into())
        .fields
        .get(rustc_abi::FieldIdx::from_usize(field_index))
        .unwrap_or_else(|| {
            panic!(
                "Union field index {} out of bounds for {:?}",
                field_index, adt_def
            )
        })
        .ident(tcx)
        .to_string()
}

struct UnionWriteback {
    class_name: String,
    field_name: String,
    receiver: Operand,
    value_name: String,
    value_ty: oomir::Type,
}

fn collect_union_writebacks<'tcx>(
    base_place: &Place<'tcx>,
    get_instructions: &[Instruction],
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
) -> Vec<UnionWriteback> {
    let mut writebacks = Vec::new();
    let mut instruction_cursor = 0;

    for (projection_index, projection) in base_place.projection.iter().enumerate() {
        let ProjectionElem::Field(field_index, _) = projection else {
            continue;
        };
        let union_base_place = projection_prefix_place(base_place, projection_index, tcx);
        let union_base_ty = EarlyBinder::bind(tcx, union_base_place.ty(&mir.local_decls, tcx).ty)
            .instantiate(tcx, instance.args)
            .skip_norm_wip();
        let Some((union_def, _)) = union_parts_from_ty(union_base_ty) else {
            continue;
        };
        let class_name = match ty_to_oomir_type(union_base_ty, tcx, data_types, instance) {
            oomir::Type::Class(name) => name,
            oomir::Type::Reference(inner) => match inner.as_ref() {
                oomir::Type::Class(name) => name.clone(),
                other => panic!("Union field access through non-class reference: {other:?}"),
            },
            other => panic!("Union field access on non-class type: {other:?}"),
        };
        let field_name = union_field_name(union_def, field_index.index(), tcx);
        let getter_name = union_getter_method_name(&field_name);

        let Some((relative_index, getter)) = get_instructions[instruction_cursor..]
            .iter()
            .enumerate()
            .find(|(_, instruction)| {
                matches!(
                    instruction,
                    Instruction::InvokeVirtual {
                        dest: Some(_),
                        class_name: owner,
                        method_name,
                        ..
                    } if owner == &class_name && method_name == &getter_name
                )
            })
        else {
            panic!(
                "Could not locate generated getter for nested union field {}.{}",
                class_name, field_name
            );
        };
        instruction_cursor += relative_index + 1;
        let Instruction::InvokeVirtual {
            dest: Some(value_name),
            method_ty,
            operand,
            ..
        } = getter
        else {
            unreachable!();
        };
        writebacks.push(UnionWriteback {
            class_name,
            field_name,
            receiver: operand.clone(),
            value_name: value_name.clone(),
            value_ty: method_ty.ret.as_ref().clone(),
        });
    }

    writebacks
}

fn emit_union_writebacks(writebacks: &[UnionWriteback], instructions: &mut Vec<Instruction>) {
    for writeback in writebacks.iter().rev() {
        instructions.push(Instruction::InvokeVirtual {
            dest: None,
            class_name: writeback.class_name.clone(),
            method_name: union_setter_method_name(&writeback.field_name),
            method_ty: oomir::Signature {
                params: vec![
                    (
                        "self".to_string(),
                        oomir::Type::Class(writeback.class_name.clone()),
                    ),
                    ("value".to_string(), writeback.value_ty.clone()),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: false,
            },
            args: vec![Operand::Variable {
                name: writeback.value_name.clone(),
                ty: writeback.value_ty.clone(),
            }],
            operand: writeback.receiver.clone(),
        });
    }
}

fn collect_memory_view_writebacks(get_instructions: &[Instruction]) -> Vec<Operand> {
    get_instructions
        .iter()
        .filter_map(|instruction| match instruction {
            Instruction::InvokeVirtual {
                class_name,
                method_name,
                operand,
                ..
            } if class_name == oomir::POINTER_CLASS
                && matches!(method_name.as_str(), "getObject" | "getObjectAs")
                && matches!(operand.get_type(), Some(oomir::Type::Pointer(_))) =>
            {
                Some(operand.clone())
            }
            _ => None,
        })
        .collect()
}

fn emit_memory_view_writebacks(writebacks: &[Operand], instructions: &mut Vec<Instruction>) {
    for pointer in writebacks.iter().rev() {
        let pointer_ty = pointer
            .get_type()
            .expect("a memory-view writeback requires a typed pointer");
        instructions.push(Instruction::InvokeVirtual {
            dest: None,
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "commitMemoryView".to_string(),
            method_ty: oomir::Signature {
                params: vec![("self".to_string(), pointer_ty)],
                ret: Box::new(oomir::Type::Void),
                is_static: false,
            },
            args: vec![],
            operand: pointer.clone(),
        });
    }
}

fn field_name_from_rust_ty<'tcx>(
    ty: Ty<'tcx>,
    field_index: usize,
    tcx: TyCtxt<'tcx>,
) -> Option<String> {
    match ty.kind() {
        TyKind::Ref(_, inner_ty, _) => field_name_from_rust_ty(*inner_ty, field_index, tcx),
        TyKind::Tuple(_) => Some(format!("field{}", field_index)),
        TyKind::Adt(adt_def, _) if adt_def.is_struct() => adt_def
            .variant(0usize.into())
            .fields
            .get(rustc_abi::FieldIdx::from_usize(field_index))
            .map(|field| field.ident(tcx).to_string()),
        TyKind::Adt(adt_def, _) if adt_def.is_enum() => None,
        TyKind::Coroutine(_, _) => Some(format!("arg{}", field_index)),
        _ => None,
    }
}

pub(super) fn coroutine_saved_field_name<'tcx>(
    ty: Ty<'tcx>,
    variant_index: rustc_abi::VariantIdx,
    field_index: usize,
    tcx: TyCtxt<'tcx>,
) -> Option<String> {
    let ty = match ty.kind() {
        TyKind::Ref(_, inner, _) => *inner,
        _ => ty,
    };
    let TyKind::Coroutine(def_id, args) = ty.kind() else {
        return None;
    };
    let layout = tcx.coroutine_layout(*def_id, args).ok()?;
    let saved_local = *layout
        .variant_fields
        .get(variant_index)?
        .get(rustc_abi::FieldIdx::from_usize(field_index))?;
    Some(format!("state{}", saved_local.as_usize()))
}

pub(super) fn field_name_for_projection<'tcx>(
    owner_class_name: &str,
    field_index: usize,
    base_rust_ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &HashMap<String, oomir::DataType>,
) -> Result<String, String> {
    let direct_rust_ty = match base_rust_ty.kind() {
        TyKind::Ref(_, inner_ty, _) => *inner_ty,
        _ => base_rust_ty,
    };
    if let TyKind::Adt(adt_def, _) = direct_rust_ty.kind()
        && adt_def.is_enum()
        && let Some(variant) = adt_def.variants().iter().find(|variant| {
            owner_class_name.ends_with(&format!(
                "${}",
                jvm_names::member_name(&variant.name.to_string())
            ))
        })
    {
        return Ok(enum_variant_field_name(variant, field_index, tcx));
    }

    let named_source_field = match base_rust_ty.kind() {
        TyKind::Ref(_, inner_ty, _) => match inner_ty.kind() {
            TyKind::Tuple(_) => field_name_from_rust_ty(*inner_ty, field_index, tcx),
            TyKind::Adt(adt_def, _) if adt_def.is_struct() => {
                field_name_from_rust_ty(*inner_ty, field_index, tcx)
            }
            _ => None,
        },
        TyKind::Tuple(_) => field_name_from_rust_ty(base_rust_ty, field_index, tcx),
        TyKind::Adt(adt_def, _) if adt_def.is_struct() => {
            field_name_from_rust_ty(base_rust_ty, field_index, tcx)
        }
        _ => None,
    };
    if named_source_field.is_some() {
        return Ok(named_source_field.expect("checked above"));
    }
    get_field_name_from_index(owner_class_name, field_index, data_types).or_else(|original_error| {
        field_name_from_rust_ty(base_rust_ty, field_index, tcx).ok_or(original_error)
    })
}

/// Resolves a nested place projection, returning its variable name, emitted
/// instructions, and final OOMIR type.
pub fn get_place_type<'tcx>(
    place: &Place<'tcx>,
    mir: &Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
) -> oomir::Type {
    if let Some(ProjectionElem::Downcast(_, variant_idx)) = place.projection.last() {
        let prefix = &place.projection[..place.projection.len() - 1];
        let base_place = Place {
            local: place.local,
            projection: tcx.mk_place_elems(prefix),
        };
        let base_ty = EarlyBinder::bind(tcx, base_place.ty(&mir.local_decls, tcx).ty)
            .instantiate(tcx, instance.args)
            .skip_norm_wip();
        if matches!(base_ty.kind(), TyKind::Coroutine(..)) {
            return ty_to_oomir_type(base_ty, tcx, data_types, instance);
        }
        let (adt_def, substs) = match base_ty.kind() {
            TyKind::Adt(adt_def, substs) => (*adt_def, *substs),
            TyKind::Ref(_, inner, _) => match inner.kind() {
                TyKind::Adt(adt_def, substs) => (*adt_def, *substs),
                _ => return ty_to_oomir_type(base_ty, tcx, data_types, instance),
            },
            _ => return ty_to_oomir_type(base_ty, tcx, data_types, instance),
        };
        if adt_def.is_enum() {
            let base_class =
                generate_adt_jvm_class_name(&adt_def, substs, tcx, data_types, instance);
            let variant = adt_def.variant(*variant_idx);
            if let Some(payload_ty) = jvm_subtype_payload_ty(&adt_def, variant, substs, tcx) {
                return ty_to_oomir_type(payload_ty, tcx, data_types, instance);
            }
            return oomir::Type::Class(format!(
                "{}${}",
                base_class,
                jvm_names::member_name(&variant.name.to_string())
            ));
        }
    }

    let place_ty = place.ty(&mir.local_decls, tcx);
    // Instantiate the type with the instance's generic arguments to get concrete types
    let instantiated_ty = rustc_middle::ty::EarlyBinder::bind(tcx, place_ty.ty)
        .instantiate(tcx, instance.args)
        .skip_norm_wip();
    ty_to_oomir_type(instantiated_ty, tcx, data_types, instance)
}

/// Generates OOMIR instructions to "get" the value from a Place.
/// This function now supports nested projections by calling
/// `emit_instructions_to_get_recursive`.
mod views;
pub(crate) use views::*;

mod read;
pub(crate) use read::*;

mod write;
pub(crate) use write::*;
