use super::jvm_names;
use crate::oomir::{self, DataType, DataTypeMethod};

use rustc_abi::{FieldIdx, TagEncoding, VariantIdx, Variants};
use rustc_middle::ty::layout::TyAndLayout;
use rustc_middle::ty::{
    AdtDef, ExistentialPredicate, FloatTy, GenericArgsRef, IntTy, Ty, TyCtxt, TyKind,
    TypeVisitableExt, TypingEnv, UintTy,
};
use rustc_span::def_id::DefId;
use sha2::Digest;
use std::collections::HashMap;

pub const UNION_BYTES_FIELD: &str = "__bytes";
pub const UNION_OBJECTS_FIELD: &str = "__objects";
pub(super) const ENUM_UNION_DISCRIMINANT_METHOD: &str = "__unionDiscriminant";
const ENUM_FROM_UNION_DISCRIMINANT_METHOD: &str = "__fromUnionDiscriminant";
const ENUM_WRITE_UNION_STORAGE_METHOD: &str = "__writeUnionStorage";
const ENUM_READ_UNION_STORAGE_METHOD: &str = "__readUnionStorage";

pub fn union_from_method_name(field_name: &str) -> String {
    format!("from_{}", jvm_names::member_name(field_name))
}

pub fn union_getter_method_name(field_name: &str) -> String {
    format!("get_{}", jvm_names::member_name(field_name))
}

pub fn union_setter_method_name(field_name: &str) -> String {
    format!("set_{}", jvm_names::member_name(field_name))
}

pub fn fn_ptr_signature_from_ty<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Signature {
    let sig = ty.fn_sig(tcx).skip_binder();
    let params = sig
        .inputs()
        .iter()
        .enumerate()
        .map(|(i, ty)| {
            (
                format!("arg{}", i),
                ty_to_oomir_type(*ty, tcx, data_types, instance_context),
            )
        })
        .collect();
    let ret = ty_to_oomir_type(sig.output(), tcx, data_types, instance_context);

    oomir::Signature {
        params,
        ret: Box::new(ret),
        is_static: true,
    }
}

pub fn ensure_fn_ptr_interface<'tcx>(
    signature: &oomir::Signature,
    data_types: &mut HashMap<String, oomir::DataType>,
    tcx: TyCtxt<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    let interface_name = jvm_names::synthetic_class_for_instance(
        tcx,
        instance_context,
        signature.fn_ptr_interface_name(),
    );
    let method_signature = signature.fn_ptr_interface_method_signature();

    match data_types.get_mut(&interface_name) {
        Some(oomir::DataType::Interface { methods }) => {
            methods
                .entry("call".to_string())
                .or_insert(method_signature);
        }
        Some(oomir::DataType::Class { .. }) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!(
                    "Function pointer interface name '{}' already exists as a class",
                    interface_name
                )
            );
        }
        None => {
            data_types.insert(
                interface_name.clone(),
                oomir::DataType::Interface {
                    methods: HashMap::from([("call".to_string(), method_signature)]),
                },
            );
        }
    }

    interface_name
}

fn byte_array_type() -> oomir::Type {
    oomir::Type::Array(Box::new(oomir::Type::I8))
}

fn object_array_type() -> oomir::Type {
    oomir::Type::Array(Box::new(oomir::Type::Class("java/lang/Object".to_string())))
}

fn next_union_temp(prefix: &str, counter: &mut usize) -> String {
    let temp = format!("{}_{}", prefix, *counter);
    *counter += 1;
    temp
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
        instructions.push(oomir::Instruction::Add {
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

fn layout_size_bytes<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Result<usize, String> {
    let ty = normalize_union_ty(tcx, ty)?;
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .map_err(|err| format!("could not get layout for {:?}: {:?}", ty, err))?;
    Ok(layout.size.bytes_usize())
}

fn normalize_union_ty<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Result<Ty<'tcx>, String> {
    tcx.try_normalize_erasing_regions(
        TypingEnv::fully_monomorphized(),
        rustc_middle::ty::Unnormalized::new_wip(ty),
    )
    .map_err(|err| format!("could not normalize union storage type {ty:?}: {err:?}"))
}

fn resolve_union_ty<'tcx>(
    tcx: TyCtxt<'tcx>,
    ty: Ty<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<Ty<'tcx>, String> {
    let instantiated = rustc_middle::ty::EarlyBinder::bind(tcx, ty)
        .instantiate(tcx, instance_context.args)
        .skip_norm_wip();
    Ok(normalize_union_ty(tcx, instantiated).unwrap_or(instantiated))
}

pub(super) fn simple_enum_union_size<'tcx>(
    adt_def: &AdtDef<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> Result<usize, String> {
    if !adt_def.is_enum() || adt_def.variants().is_empty() {
        return Err("only non-empty enums are supported in unions".to_string());
    }
    if adt_def
        .variants()
        .iter()
        .any(|variant| !variant.fields.is_empty())
    {
        return Err("only fieldless enums are supported in unions".to_string());
    }

    if !enum_union_discriminant_supported(adt_def, tcx) {
        return Err(
            "enum discriminant does not fit the JVM union integer representation".to_string(),
        );
    }

    let enum_ty = tcx
        .type_of(adt_def.did())
        .instantiate_identity()
        .skip_norm_wip();
    let size = layout_size_bytes(tcx, enum_ty)?;
    if !matches!(size, 1 | 2 | 4 | 8) {
        return Err(format!(
            "unsupported enum discriminant layout size: {} bytes",
            size
        ));
    }
    Ok(size)
}

pub(super) fn enum_union_discriminant_supported<'tcx>(
    adt_def: &AdtDef<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> bool {
    let Some((_, discriminant)) = adt_def.discriminants(tcx).next() else {
        return false;
    };
    let discriminant_ty = discriminant.ty;
    match discriminant_ty.kind() {
        TyKind::Int(IntTy::I128) | TyKind::Uint(UintTy::U64 | UintTy::U128) => false,
        TyKind::Int(_) | TyKind::Uint(_) => true,
        _ => false,
    }
}

fn masked_enum_discriminant(value: u128, size: usize) -> i64 {
    if size == 8 {
        value as i64
    } else {
        let mask = (1u128 << (size * 8)) - 1;
        (value & mask) as i64
    }
}

fn scalar_bits_type(rust_size: usize, oomir_ty: &oomir::Type) -> oomir::Type {
    if matches!(oomir_ty, oomir::Type::I64 | oomir::Type::F64) || rust_size > 4 {
        oomir::Type::I64
    } else {
        oomir::Type::I32
    }
}

fn int_constant_for_type(value: i64, ty: &oomir::Type) -> oomir::Constant {
    if matches!(ty, oomir::Type::I64) {
        oomir::Constant::I64(value)
    } else {
        oomir::Constant::I32(value as i32)
    }
}

pub fn should_define_named_data_type<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> bool {
    def_id.is_local() || tcx.crate_name(def_id.krate).as_str() == "core"
}

fn add_enum_helper_methods(
    methods: &mut HashMap<String, DataTypeMethod>,
    variants_info: Vec<(String, Vec<oomir::Type>)>,
) {
    methods
        .entry("getVariantIdx".to_string())
        .or_insert(DataTypeMethod::SimpleConstantReturn(oomir::Type::I32, None));
    methods
        .entry("eq".to_string())
        .or_insert(DataTypeMethod::AdtHelperMethod {
            kind: oomir::AdtHelperKind::PartialEqEnum {
                variants: variants_info.clone(),
            },
        });

    if variants_info.len() == 2 {
        let mut none_variant_idx = 1u32;
        let mut some_variant_idx = 0u32;
        for (idx, (name, _)) in variants_info.iter().enumerate() {
            if name == "None" {
                none_variant_idx = idx as u32;
            } else if name == "Some" {
                some_variant_idx = idx as u32;
            }
        }

        methods
            .entry("is_none".to_string())
            .or_insert(DataTypeMethod::AdtHelperMethod {
                kind: oomir::AdtHelperKind::IsVariant {
                    variant_idx: none_variant_idx,
                },
            });
        methods
            .entry("is_some".to_string())
            .or_insert(DataTypeMethod::AdtHelperMethod {
                kind: oomir::AdtHelperKind::IsVariant {
                    variant_idx: some_variant_idx,
                },
            });
    }
}

fn enum_from_union_discriminant_function<'tcx>(
    adt_def: &AdtDef<'tcx>,
    union_size: usize,
    base_enum_name: &str,
    tcx: TyCtxt<'tcx>,
) -> oomir::Function {
    let mut basic_blocks = HashMap::new();
    let mut targets = Vec::new();

    for (variant_idx, discriminant) in adt_def.discriminants(tcx) {
        let variant = adt_def.variant(variant_idx);
        let variant_class_name = format!(
            "{}${}",
            base_enum_name,
            jvm_names::member_name(&variant.name.to_string())
        );
        let block_name = format!("variant_{}", variant_idx.as_u32());
        let result_name = format!("_variant_{}", variant_idx.as_u32());
        let masked_discriminant = masked_enum_discriminant(discriminant.val, union_size);
        targets.push((
            oomir::Constant::I64(masked_discriminant),
            block_name.clone(),
        ));
        let shift = 64 - union_size * 8;
        let signed_discriminant = (masked_discriminant << shift) >> shift;
        if signed_discriminant != masked_discriminant {
            targets.push((
                oomir::Constant::I64(signed_discriminant),
                block_name.clone(),
            ));
        }
        basic_blocks.insert(
            block_name.clone(),
            oomir::BasicBlock {
                label: block_name,
                instructions: vec![
                    oomir::Instruction::ConstructObject {
                        dest: result_name.clone(),
                        class_name: variant_class_name.clone(),
                        args: vec![],
                    },
                    oomir::Instruction::Return {
                        operand: Some(operand_var(
                            result_name,
                            oomir::Type::Class(variant_class_name),
                        )),
                    },
                ],
            },
        );
    }

    basic_blocks.insert(
        "entry".to_string(),
        oomir::BasicBlock {
            label: "entry".to_string(),
            instructions: vec![oomir::Instruction::Switch {
                discr: operand_var("_1", oomir::Type::I64),
                targets,
                otherwise: "invalid".to_string(),
            }],
        },
    );
    basic_blocks.insert(
        "invalid".to_string(),
        oomir::BasicBlock {
            label: "invalid".to_string(),
            instructions: vec![oomir::Instruction::ThrowNewWithMessage {
                exception_class: "java/lang/IllegalArgumentException".to_string(),
                message: format!(
                    "invalid discriminant while reading enum {} from union storage",
                    base_enum_name
                ),
            }],
        },
    );

    oomir::Function {
        name: ENUM_FROM_UNION_DISCRIMINANT_METHOD.to_string(),
        owner_class: None,
        signature: oomir::Signature {
            params: vec![("discriminant".to_string(), oomir::Type::I64)],
            ret: Box::new(oomir::Type::Class(base_enum_name.to_string())),
            is_static: true,
        },
        body: oomir::CodeBlock {
            entry: "entry".to_string(),
            basic_blocks,
        },
    }
}

fn ensure_enum_data_types<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    base_enum_name: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) {
    let mut created_placeholders = Vec::new();

    // 1. Insert a placeholder for the base enum class
    if !data_types.contains_key(base_enum_name) {
        data_types.insert(
            base_enum_name.to_string(),
            oomir::DataType::Class {
                fields: vec![],
                is_abstract: true,
                methods: HashMap::new(),
                super_class: None,
                interfaces: vec![],
            },
        );
        created_placeholders.push(base_enum_name.to_string());
    }

    // 2. Insert placeholders for all individual variant subclasses
    for variant in adt_def.variants().iter() {
        let variant_class_name = format!(
            "{}${}",
            base_enum_name,
            jvm_names::member_name(&variant.name.to_string())
        );
        if !data_types.contains_key(&variant_class_name) {
            data_types.insert(
                variant_class_name.clone(),
                oomir::DataType::Class {
                    fields: vec![],
                    is_abstract: false,
                    methods: HashMap::new(),
                    super_class: Some(base_enum_name.to_string()),
                    interfaces: vec![],
                },
            );
            created_placeholders.push(variant_class_name);
        }
    }

    let union_size = simple_enum_union_size(adt_def, tcx).ok();
    let has_numeric_discriminant = enum_union_discriminant_supported(adt_def, tcx);
    let union_factory = union_size
        .map(|size| enum_from_union_discriminant_function(adt_def, size, base_enum_name, tcx));
    let variants_info: Vec<_> = adt_def
        .variants()
        .iter()
        .map(|variant| {
            let variant_name = jvm_names::member_name(&variant.name.to_string());
            let fields = variant
                .fields
                .iter()
                .filter_map(|field| {
                    let field_ty = ty_to_oomir_type(
                        field.ty(tcx, substs).skip_norm_wip(),
                        tcx,
                        data_types,
                        instance_context,
                    );
                    field_ty.has_jvm_value().then_some(field_ty)
                })
                .collect();
            (variant_name, fields)
        })
        .collect();

    if let Some(oomir::DataType::Class { methods, .. }) = data_types.get_mut(base_enum_name) {
        add_enum_helper_methods(methods, variants_info.clone());
        if has_numeric_discriminant {
            methods
                .entry(ENUM_UNION_DISCRIMINANT_METHOD.to_string())
                .or_insert(DataTypeMethod::SimpleConstantReturn(oomir::Type::I64, None));
        }
        if let Some(factory) = union_factory.clone() {
            methods
                .entry(ENUM_FROM_UNION_DISCRIMINANT_METHOD.to_string())
                .or_insert(DataTypeMethod::Function(factory));
        }
    }

    let discriminants: HashMap<_, _> = adt_def.discriminants(tcx).collect();
    for (variant_idx, variant) in adt_def.variants().iter().enumerate() {
        let variant_class_name = format!(
            "{}${}",
            base_enum_name,
            jvm_names::member_name(&variant.name.to_string())
        );
        if !created_placeholders.contains(&variant_class_name) {
            if let Some(oomir::DataType::Class { methods, .. }) =
                data_types.get_mut(&variant_class_name)
            {
                if has_numeric_discriminant {
                    let discriminant = discriminants[&variant_idx.into()];
                    methods
                        .entry(ENUM_UNION_DISCRIMINANT_METHOD.to_string())
                        .or_insert(DataTypeMethod::SimpleConstantReturn(
                            oomir::Type::I64,
                            Some(oomir::Constant::I64(discriminant.val as i64)),
                        ));
                }
                continue;
            }
        }

        let fields = variant
            .fields
            .iter()
            .filter_map(|field| {
                let field_ty = ty_to_oomir_type(
                    field.ty(tcx, substs).skip_norm_wip(),
                    tcx,
                    data_types,
                    instance_context,
                );
                field_ty.has_jvm_value().then_some(field_ty)
            })
            .enumerate()
            .map(|(field_idx, field_ty)| (format!("field{}", field_idx), field_ty))
            .collect();
        let mut methods = HashMap::new();
        methods.insert(
            "getVariantIdx".to_string(),
            DataTypeMethod::SimpleConstantReturn(
                oomir::Type::I32,
                Some(oomir::Constant::I32(variant_idx as i32)),
            ),
        );
        if has_numeric_discriminant {
            let discriminant = discriminants[&variant_idx.into()];
            methods.insert(
                ENUM_UNION_DISCRIMINANT_METHOD.to_string(),
                DataTypeMethod::SimpleConstantReturn(
                    oomir::Type::I64,
                    Some(oomir::Constant::I64(discriminant.val as i64)),
                ),
            );
        }

        data_types.insert(
            variant_class_name,
            oomir::DataType::Class {
                fields,
                is_abstract: false,
                methods,
                super_class: Some(base_enum_name.to_string()),
                interfaces: vec![],
            },
        );
    }
}

fn operand_var(name: impl Into<String>, ty: oomir::Type) -> oomir::Operand {
    oomir::Operand::Variable {
        name: name.into(),
        ty,
    }
}

pub(super) fn adapt_simple_enum_operand(
    source: oomir::Operand,
    target_ty: &oomir::Type,
    temp_prefix: &str,
    data_types: &HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    let oomir::Type::Class(enum_class) = target_ty else {
        return source;
    };
    if source
        .get_type()
        .is_some_and(|source_ty| source_ty.is_jvm_reference_type())
    {
        return source;
    }
    let has_factory = matches!(
        data_types.get(enum_class),
        Some(oomir::DataType::Class { methods, .. })
            if methods.contains_key(ENUM_FROM_UNION_DISCRIMINANT_METHOD)
    );
    if !has_factory {
        return source;
    }

    let bits_dest = format!("{}_enum_discriminant", temp_prefix);
    instructions.push(oomir::Instruction::Cast {
        op: source,
        ty: oomir::Type::I64,
        dest: bits_dest.clone(),
    });
    let enum_dest = format!("{}_enum_value", temp_prefix);
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(enum_dest.clone()),
        class_name: enum_class.clone(),
        method_name: ENUM_FROM_UNION_DISCRIMINANT_METHOD.to_string(),
        method_ty: oomir::Signature {
            params: vec![("discriminant".to_string(), oomir::Type::I64)],
            ret: Box::new(target_ty.clone()),
            is_static: true,
        },
        args: vec![operand_var(bits_dest, oomir::Type::I64)],
    });
    operand_var(enum_dest, target_ty.clone())
}

fn is_direct_union_scalar<'tcx>(ty: Ty<'tcx>) -> bool {
    matches!(
        ty.kind(),
        TyKind::Bool
            | TyKind::Char
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(FloatTy::F32 | FloatTy::F64)
    )
}

fn scalar_bit_operand_for_union<'tcx>(
    ty: Ty<'tcx>,
    source: oomir::Operand,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<(oomir::Operand, oomir::Type, usize), String> {
    let rust_size = layout_size_bytes(tcx, ty)?;
    let oomir_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);

    match ty.kind() {
        TyKind::Float(FloatTy::F32) => {
            let dest = next_union_temp("union_f32_bits", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.clone()),
                class_name: "java/lang/Float".to_string(),
                method_name: "floatToRawIntBits".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("value".to_string(), oomir::Type::F32)],
                    ret: Box::new(oomir::Type::I32),
                    is_static: true,
                },
                args: vec![source],
            });
            Ok((
                operand_var(dest, oomir::Type::I32),
                oomir::Type::I32,
                rust_size,
            ))
        }
        TyKind::Float(FloatTy::F64) => {
            let dest = next_union_temp("union_f64_bits", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.clone()),
                class_name: "java/lang/Double".to_string(),
                method_name: "doubleToRawLongBits".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("value".to_string(), oomir::Type::F64)],
                    ret: Box::new(oomir::Type::I64),
                    is_static: true,
                },
                args: vec![source],
            });
            Ok((
                operand_var(dest, oomir::Type::I64),
                oomir::Type::I64,
                rust_size,
            ))
        }
        TyKind::Float(_) => Err(format!("unsupported float width in union field: {:?}", ty)),
        TyKind::Int(IntTy::I128) | TyKind::Uint(UintTy::U64 | UintTy::U128) => {
            Err(format!("unsupported wide integer union field: {:?}", ty))
        }
        TyKind::Bool | TyKind::Char | TyKind::Int(_) | TyKind::Uint(_) => {
            let bits_ty = scalar_bits_type(rust_size, &oomir_ty);
            if source.get_type().as_ref() == Some(&bits_ty) {
                Ok((source, bits_ty, rust_size))
            } else {
                let dest = next_union_temp("union_bits", temp_counter);
                instructions.push(oomir::Instruction::Cast {
                    op: source,
                    ty: bits_ty.clone(),
                    dest: dest.clone(),
                });
                Ok((operand_var(dest, bits_ty.clone()), bits_ty, rust_size))
            }
        }
        _ => Err(format!("unsupported scalar union field: {:?}", ty)),
    }
}

fn emit_bits_to_union_bytes(
    bits_operand: oomir::Operand,
    bits_ty: oomir::Type,
    rust_size: usize,
    storage: &JvmUnionStorage,
    base_offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<(), String> {
    for byte_idx in 0..rust_size {
        let shifted = if byte_idx == 0 {
            bits_operand.clone()
        } else {
            let shift_dest = next_union_temp("union_shift", temp_counter);
            instructions.push(oomir::Instruction::Shr {
                dest: shift_dest.clone(),
                op1: bits_operand.clone(),
                op2: oomir::Operand::Constant(oomir::Constant::I32((byte_idx * 8) as i32)),
            });
            operand_var(shift_dest, bits_ty.clone())
        };

        let masked_dest = next_union_temp("union_mask", temp_counter);
        instructions.push(oomir::Instruction::BitAnd {
            dest: masked_dest.clone(),
            op1: shifted,
            op2: oomir::Operand::Constant(int_constant_for_type(0xff, &bits_ty)),
        });

        let byte_dest = next_union_temp("union_byte", temp_counter);
        instructions.push(oomir::Instruction::Cast {
            op: operand_var(masked_dest, bits_ty.clone()),
            ty: oomir::Type::I8,
            dest: byte_dest.clone(),
        });

        let index = storage.byte_index(base_offset + byte_idx, instructions, temp_counter);
        instructions.push(oomir::Instruction::ArrayStore {
            array: storage.bytes_var.clone(),
            index,
            value: operand_var(byte_dest, oomir::Type::I8),
        });
    }

    Ok(())
}

fn emit_scalar_to_union_bytes<'tcx>(
    ty: Ty<'tcx>,
    source: oomir::Operand,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<(), String> {
    let (bits_operand, bits_ty, rust_size) = scalar_bit_operand_for_union(
        ty,
        source,
        tcx,
        data_types,
        instance_context,
        instructions,
        temp_counter,
    )?;
    emit_bits_to_union_bytes(
        bits_operand,
        bits_ty,
        rust_size,
        storage,
        base_offset,
        instructions,
        temp_counter,
    )
}

fn union_aggregate_layout<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<Option<UnionAggregateLayout<'tcx>>, String> {
    let ty = resolve_union_ty(tcx, ty, instance_context)?;
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .map_err(|err| format!("could not get layout for {:?}: {:?}", ty, err))?;

    let (class_name, raw_fields): (String, Vec<(Ty<'tcx>, String, usize)>) = match ty.kind() {
        TyKind::Tuple(elements) if !elements.is_empty() => {
            let element_tys: Vec<_> = elements.iter().collect();
            let class_name =
                generate_tuple_jvm_class_name(&element_tys, tcx, data_types, instance_context);
            let fields = element_tys
                .into_iter()
                .enumerate()
                .map(|(index, field_ty)| {
                    (
                        field_ty,
                        format!("field{index}"),
                        layout
                            .fields
                            .offset(FieldIdx::from_usize(index).into())
                            .bytes_usize(),
                    )
                })
                .collect();
            (class_name, fields)
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_struct() => {
            let class_name =
                generate_adt_jvm_class_name(adt_def, substs, tcx, data_types, instance_context);
            let fields = adt_def
                .variant(VariantIdx::from_usize(0))
                .fields
                .iter()
                .enumerate()
                .map(|(index, field)| {
                    Ok((
                        resolve_union_ty(
                            tcx,
                            field.ty(tcx, substs).skip_norm_wip(),
                            instance_context,
                        )?,
                        field.ident(tcx).to_string(),
                        layout
                            .fields
                            .offset(FieldIdx::from_usize(index).into())
                            .bytes_usize(),
                    ))
                })
                .collect::<Result<Vec<_>, String>>()?;
            (class_name, fields)
        }
        _ => return Ok(None),
    };

    let mut fields = Vec::new();
    for (rust_ty, jvm_name, offset) in raw_fields {
        if layout_size_bytes(tcx, rust_ty)? == 0 {
            continue;
        }
        fields.push(UnionAggregateField {
            rust_ty,
            jvm_ty: ty_to_oomir_type(rust_ty, tcx, data_types, instance_context),
            jvm_name,
            offset,
        });
    }
    Ok(Some(UnionAggregateLayout { class_name, fields }))
}

fn emit_aggregate_to_union_bytes<'tcx>(
    aggregate: &UnionAggregateLayout<'tcx>,
    source: oomir::Operand,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<(), String> {
    for field in &aggregate.fields {
        let field_dest = next_union_temp("union_aggregate_field", temp_counter);
        instructions.push(oomir::Instruction::GetField {
            dest: field_dest.clone(),
            object: source.clone(),
            field_name: field.jvm_name.clone(),
            field_ty: field.jvm_ty.clone(),
            owner_class: aggregate.class_name.clone(),
        });
        emit_ty_to_union_bytes(
            field.rust_ty,
            operand_var(field_dest, field.jvm_ty.clone()),
            storage,
            base_offset + field.offset,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        )?;
    }
    Ok(())
}

fn emit_aggregate_from_union_bytes<'tcx>(
    aggregate: &UnionAggregateLayout<'tcx>,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<oomir::Operand, String> {
    let mut constructor_args = Vec::new();
    for field in &aggregate.fields {
        let value = emit_ty_from_union_bytes(
            field.rust_ty,
            storage,
            base_offset + field.offset,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        )?;
        constructor_args.push((value, field.jvm_ty.clone()));
    }

    let dest = next_union_temp("union_aggregate_value", temp_counter);
    instructions.push(oomir::Instruction::ConstructObject {
        dest: dest.clone(),
        class_name: aggregate.class_name.clone(),
        args: constructor_args,
    });
    Ok(operand_var(
        dest,
        oomir::Type::Class(aggregate.class_name.clone()),
    ))
}

fn union_enum_tag<'tcx>(
    layout: &TyAndLayout<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> Result<UnionEnumTag, String> {
    match &layout.variants {
        Variants::Single { index } => Ok(UnionEnumTag::Single { variant: *index }),
        Variants::Multiple {
            tag,
            tag_encoding,
            tag_field,
            ..
        } => {
            let size = tag.size(&tcx.data_layout).bytes_usize();
            if size == 0 || size > 8 {
                return Err(format!("unsupported enum tag size: {size} bytes"));
            }
            let offset = layout.fields.offset((*tag_field).into()).bytes_usize();
            match tag_encoding {
                TagEncoding::Direct => Ok(UnionEnumTag::Direct { offset, size }),
                TagEncoding::Niche {
                    untagged_variant,
                    niche_variants,
                    niche_start,
                } => Ok(UnionEnumTag::Niche {
                    offset,
                    size,
                    untagged_variant: *untagged_variant,
                    niche_start_variant: niche_variants.start,
                    niche_end_variant: niche_variants.last,
                    niche_start: *niche_start,
                }),
            }
        }
        Variants::Empty => Err("uninhabited enums cannot be stored in unions".to_string()),
    }
}

fn union_enum_variant_layout<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    enum_class: &str,
    layout: &TyAndLayout<'tcx>,
    variant_idx: VariantIdx,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<UnionAggregateLayout<'tcx>, String> {
    let variant = adt_def.variant(variant_idx);
    let variant_class = format!(
        "{}${}",
        enum_class,
        jvm_names::member_name(&variant.name.to_string())
    );
    let mut fields = Vec::new();
    let mut jvm_field_index = 0;
    for (field_index, field) in variant.fields.iter().enumerate() {
        let rust_ty =
            resolve_union_ty(tcx, field.ty(tcx, substs).skip_norm_wip(), instance_context)?;
        if layout_size_bytes(tcx, rust_ty)? == 0 {
            continue;
        }
        let offset = match &layout.variants {
            Variants::Single { .. } => layout.fields.offset(field_index).bytes_usize(),
            Variants::Multiple { variants, .. } => {
                variants[variant_idx].field_offsets[FieldIdx::from_usize(field_index)].bytes_usize()
            }
            Variants::Empty => unreachable!(),
        };
        fields.push(UnionAggregateField {
            rust_ty,
            jvm_ty: ty_to_oomir_type(rust_ty, tcx, data_types, instance_context),
            jvm_name: format!("field{jvm_field_index}"),
            offset,
        });
        jvm_field_index += 1;
    }
    Ok(UnionAggregateLayout {
        class_name: variant_class,
        fields,
    })
}

fn enum_union_write_signature(receiver_class: &str) -> oomir::Signature {
    oomir::Signature {
        params: vec![
            (
                "self".to_string(),
                oomir::Type::Class(receiver_class.to_string()),
            ),
            ("bytes".to_string(), byte_array_type()),
            ("objects".to_string(), object_array_type()),
            ("offset".to_string(), oomir::Type::I32),
        ],
        ret: Box::new(oomir::Type::Void),
        is_static: false,
    }
}

fn enum_union_read_signature(enum_class: &str) -> oomir::Signature {
    oomir::Signature {
        params: vec![
            ("bytes".to_string(), byte_array_type()),
            ("objects".to_string(), object_array_type()),
            ("offset".to_string(), oomir::Type::I32),
        ],
        ret: Box::new(oomir::Type::Class(enum_class.to_string())),
        is_static: true,
    }
}

fn enum_variant_union_writer<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    enum_class: &str,
    layout: &TyAndLayout<'tcx>,
    tag: &UnionEnumTag,
    variant_idx: VariantIdx,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<(String, oomir::Function), String> {
    let variant_layout = union_enum_variant_layout(
        adt_def,
        substs,
        enum_class,
        layout,
        variant_idx,
        tcx,
        data_types,
        instance_context,
    )?;
    let storage = JvmUnionStorage::at_offset("_2", "_3", operand_var("_4", oomir::Type::I32));
    let mut instructions = Vec::new();
    let mut temp_counter = 0;

    emit_aggregate_to_union_bytes(
        &variant_layout,
        operand_var("_1", oomir::Type::Class(variant_layout.class_name.clone())),
        &storage,
        0,
        tcx,
        data_types,
        instance_context,
        &mut instructions,
        &mut temp_counter,
    )?;

    let tag_value = match tag {
        UnionEnumTag::Single { .. } => None,
        UnionEnumTag::Direct { offset, size } => {
            let discriminant = adt_def
                .discriminants(tcx)
                .find(|(index, _)| *index == variant_idx)
                .ok_or_else(|| format!("missing discriminant for variant {variant_idx:?}"))?
                .1;
            Some((
                *offset,
                *size,
                masked_enum_discriminant(discriminant.val, *size),
            ))
        }
        UnionEnumTag::Niche {
            offset,
            size,
            untagged_variant,
            niche_start_variant,
            niche_end_variant,
            niche_start,
        } if variant_idx != *untagged_variant => {
            if variant_idx < *niche_start_variant || variant_idx > *niche_end_variant {
                return Err(format!(
                    "variant {variant_idx:?} is neither the untagged nor a niche variant"
                ));
            }
            let relative = variant_idx.as_u32() - niche_start_variant.as_u32();
            Some((
                *offset,
                *size,
                masked_enum_discriminant(niche_start.wrapping_add(relative.into()), *size),
            ))
        }
        UnionEnumTag::Niche { .. } => None,
    };
    if let Some((offset, size, value)) = tag_value {
        emit_bits_to_union_bytes(
            oomir::Operand::Constant(oomir::Constant::I64(value)),
            oomir::Type::I64,
            size,
            &storage,
            offset,
            &mut instructions,
            &mut temp_counter,
        )?;
    }
    instructions.push(oomir::Instruction::Return { operand: None });

    Ok((
        variant_layout.class_name.clone(),
        oomir::Function {
            name: ENUM_WRITE_UNION_STORAGE_METHOD.to_string(),
            owner_class: None,
            signature: enum_union_write_signature(&variant_layout.class_name),
            body: simple_body(instructions),
        },
    ))
}

fn enum_union_reader<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    enum_class: &str,
    layout: &TyAndLayout<'tcx>,
    tag: &UnionEnumTag,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<oomir::Function, String> {
    let storage = JvmUnionStorage::at_offset("_1", "_2", operand_var("_3", oomir::Type::I32));
    let mut basic_blocks = HashMap::new();

    for variant_index in 0..adt_def.variants().len() {
        let variant_idx = VariantIdx::from_usize(variant_index);
        let variant_layout = union_enum_variant_layout(
            adt_def,
            substs,
            enum_class,
            layout,
            variant_idx,
            tcx,
            data_types,
            instance_context,
        )?;
        let mut instructions = Vec::new();
        let mut temp_counter = variant_idx.as_usize() * 10_000;
        let value = emit_aggregate_from_union_bytes(
            &variant_layout,
            &storage,
            0,
            tcx,
            data_types,
            instance_context,
            &mut instructions,
            &mut temp_counter,
        )?;
        instructions.push(oomir::Instruction::Return {
            operand: Some(value),
        });
        let block_name = format!("variant_{}", variant_idx.as_u32());
        basic_blocks.insert(
            block_name.clone(),
            oomir::BasicBlock {
                label: block_name,
                instructions,
            },
        );
    }

    let entry = match tag {
        UnionEnumTag::Single { variant } => format!("variant_{}", variant.as_u32()),
        UnionEnumTag::Direct { offset, size } => {
            let mut instructions = Vec::new();
            let mut temp_counter = 0;
            let discriminant = emit_bits_from_union_bytes(
                oomir::Type::I64,
                *size,
                &storage,
                *offset,
                &mut instructions,
                &mut temp_counter,
            );
            let targets = adt_def
                .discriminants(tcx)
                .map(|(variant, discriminant)| {
                    (
                        oomir::Constant::I64(masked_enum_discriminant(discriminant.val, *size)),
                        format!("variant_{}", variant.as_u32()),
                    )
                })
                .collect();
            instructions.push(oomir::Instruction::Switch {
                discr: discriminant,
                targets,
                otherwise: "invalid".to_string(),
            });
            basic_blocks.insert(
                "entry".to_string(),
                oomir::BasicBlock {
                    label: "entry".to_string(),
                    instructions,
                },
            );
            basic_blocks.insert(
                "invalid".to_string(),
                oomir::BasicBlock {
                    label: "invalid".to_string(),
                    instructions: vec![oomir::Instruction::ThrowNewWithMessage {
                        exception_class: "java/lang/IllegalArgumentException".to_string(),
                        message: format!(
                            "invalid discriminant while reading enum {enum_class} from union storage"
                        ),
                    }],
                },
            );
            "entry".to_string()
        }
        UnionEnumTag::Niche {
            offset,
            size,
            untagged_variant,
            niche_start_variant,
            niche_end_variant,
            niche_start,
        } => {
            let mut instructions = Vec::new();
            let mut temp_counter = 0;
            let niche = emit_bits_from_union_bytes(
                oomir::Type::I64,
                *size,
                &storage,
                *offset,
                &mut instructions,
                &mut temp_counter,
            );
            let targets = (niche_start_variant.as_u32()..=niche_end_variant.as_u32())
                .map(|variant| {
                    let relative = variant - niche_start_variant.as_u32();
                    (
                        oomir::Constant::I64(masked_enum_discriminant(
                            niche_start.wrapping_add(relative.into()),
                            *size,
                        )),
                        format!("variant_{variant}"),
                    )
                })
                .collect();
            instructions.push(oomir::Instruction::Switch {
                discr: niche,
                targets,
                otherwise: format!("variant_{}", untagged_variant.as_u32()),
            });
            basic_blocks.insert(
                "entry".to_string(),
                oomir::BasicBlock {
                    label: "entry".to_string(),
                    instructions,
                },
            );
            "entry".to_string()
        }
    };

    Ok(oomir::Function {
        name: ENUM_READ_UNION_STORAGE_METHOD.to_string(),
        owner_class: None,
        signature: enum_union_read_signature(enum_class),
        body: oomir::CodeBlock {
            entry,
            basic_blocks,
        },
    })
}

fn ensure_enum_union_codec<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    enum_ty: Ty<'tcx>,
    enum_class: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<(), String> {
    let already_defined = matches!(
        data_types.get(enum_class),
        Some(DataType::Class { methods, .. })
            if methods.contains_key(ENUM_READ_UNION_STORAGE_METHOD)
    );
    if already_defined {
        return Ok(());
    }

    let base_writer = oomir::Function {
        name: ENUM_WRITE_UNION_STORAGE_METHOD.to_string(),
        owner_class: None,
        signature: enum_union_write_signature(enum_class),
        body: unsupported_union_body(format!(
            "enum base class {enum_class} has no concrete union representation"
        )),
    };
    if let Some(DataType::Class { methods, .. }) = data_types.get_mut(enum_class) {
        methods.insert(
            ENUM_WRITE_UNION_STORAGE_METHOD.to_string(),
            DataTypeMethod::Function(base_writer),
        );
    } else {
        return Err(format!("enum JVM class {enum_class} was not defined"));
    }

    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(enum_ty))
        .map_err(|err| format!("could not get layout for {enum_ty:?}: {err:?}"))?;
    let tag = union_enum_tag(&layout, tcx)?;
    let mut writers = Vec::new();
    for variant_index in 0..adt_def.variants().len() {
        let variant_idx = VariantIdx::from_usize(variant_index);
        writers.push(enum_variant_union_writer(
            adt_def,
            substs,
            enum_class,
            &layout,
            &tag,
            variant_idx,
            tcx,
            data_types,
            instance_context,
        )?);
    }
    let reader = enum_union_reader(
        adt_def,
        substs,
        enum_class,
        &layout,
        &tag,
        tcx,
        data_types,
        instance_context,
    )?;

    for (variant_class, writer) in writers {
        let Some(DataType::Class { methods, .. }) = data_types.get_mut(&variant_class) else {
            return Err(format!(
                "enum variant JVM class {variant_class} was not defined"
            ));
        };
        methods.insert(
            ENUM_WRITE_UNION_STORAGE_METHOD.to_string(),
            DataTypeMethod::Function(writer),
        );
    }
    let Some(DataType::Class { methods, .. }) = data_types.get_mut(enum_class) else {
        return Err(format!("enum JVM class {enum_class} was not defined"));
    };
    methods.insert(
        ENUM_READ_UNION_STORAGE_METHOD.to_string(),
        DataTypeMethod::Function(reader),
    );
    Ok(())
}

fn emit_object_to_union_storage(
    source: oomir::Operand,
    storage: &JvmUnionStorage,
    base_offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) {
    let index = storage.byte_index(base_offset, instructions, temp_counter);
    instructions.push(oomir::Instruction::ArrayStore {
        array: storage.objects_var.clone(),
        index,
        value: source,
    });
}

fn emit_object_from_union_storage(
    target_ty: oomir::Type,
    storage: &JvmUnionStorage,
    base_offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> oomir::Operand {
    let index = storage.byte_index(base_offset, instructions, temp_counter);
    let object_dest = next_union_temp("union_object", temp_counter);
    instructions.push(oomir::Instruction::ArrayGet {
        dest: object_dest.clone(),
        array: operand_var(storage.objects_var.clone(), object_array_type()),
        index,
    });
    if target_ty == oomir::Type::Class("java/lang/Object".to_string()) {
        return operand_var(object_dest, target_ty);
    }

    let typed_dest = next_union_temp("union_typed_object", temp_counter);
    instructions.push(oomir::Instruction::Cast {
        op: operand_var(
            object_dest,
            oomir::Type::Class("java/lang/Object".to_string()),
        ),
        ty: target_ty.clone(),
        dest: typed_dest.clone(),
    });
    operand_var(typed_dest, target_ty)
}

fn emit_ty_to_union_bytes<'tcx>(
    ty: Ty<'tcx>,
    source: oomir::Operand,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<(), String> {
    let ty = resolve_union_ty(tcx, ty, instance_context)?;
    if is_direct_union_scalar(ty) {
        return emit_scalar_to_union_bytes(
            ty,
            source,
            storage,
            base_offset,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        );
    }

    match ty.kind() {
        TyKind::Pat(inner, _) => emit_ty_to_union_bytes(
            *inner,
            source,
            storage,
            base_offset,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        ),
        TyKind::Float(FloatTy::F128) => {
            for (method_name, offset) in [("lowBits", 0), ("highBits", 8)] {
                let bits_dest = next_union_temp("union_f128_bits", temp_counter);
                instructions.push(oomir::Instruction::InvokeVirtual {
                    dest: Some(bits_dest.clone()),
                    class_name: crate::lower2::F128_CLASS.to_string(),
                    method_name: method_name.to_string(),
                    method_ty: oomir::Signature {
                        params: Vec::new(),
                        ret: Box::new(oomir::Type::I64),
                        is_static: false,
                    },
                    args: Vec::new(),
                    operand: source.clone(),
                });
                emit_bits_to_union_bytes(
                    operand_var(bits_dest, oomir::Type::I64),
                    oomir::Type::I64,
                    8,
                    storage,
                    base_offset + offset,
                    instructions,
                    temp_counter,
                )?;
            }
            Ok(())
        }
        TyKind::Tuple(elements) if elements.is_empty() => Ok(()),
        TyKind::Array(element_ty, length) => {
            let length = length
                .try_to_target_usize(tcx)
                .ok_or_else(|| format!("array length is not concrete for {:?}", ty))?
                as usize;
            let element_size = layout_size_bytes(tcx, *element_ty)?;
            if element_size == 0 {
                return Err(format!(
                    "zero-sized array elements are unsupported in union field {:?}",
                    ty
                ));
            }
            let element_oomir_ty = ty_to_oomir_type(*element_ty, tcx, data_types, instance_context);

            for index in 0..length {
                let element_dest = next_union_temp("union_array_element", temp_counter);
                instructions.push(oomir::Instruction::ArrayGet {
                    dest: element_dest.clone(),
                    array: source.clone(),
                    index: oomir::Operand::Constant(oomir::Constant::I32(index as i32)),
                });
                emit_ty_to_union_bytes(
                    *element_ty,
                    operand_var(element_dest, element_oomir_ty.clone()),
                    storage,
                    base_offset + index * element_size,
                    tcx,
                    data_types,
                    instance_context,
                    instructions,
                    temp_counter,
                )?;
            }
            Ok(())
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_enum() => {
            let enum_oomir_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            let oomir::Type::Class(enum_class) = &enum_oomir_ty else {
                return Err(format!("enum {ty:?} did not map to a JVM class"));
            };
            ensure_enum_union_codec(
                adt_def,
                substs,
                ty,
                enum_class,
                tcx,
                data_types,
                instance_context,
            )?;
            let offset = storage.byte_index(base_offset, instructions, temp_counter);
            instructions.push(oomir::Instruction::InvokeVirtual {
                dest: None,
                class_name: enum_class.clone(),
                method_name: ENUM_WRITE_UNION_STORAGE_METHOD.to_string(),
                method_ty: enum_union_write_signature(enum_class),
                args: vec![
                    operand_var(storage.bytes_var.clone(), byte_array_type()),
                    operand_var(storage.objects_var.clone(), object_array_type()),
                    offset,
                ],
                operand: source,
            });
            Ok(())
        }
        TyKind::Tuple(elements) if !elements.is_empty() => {
            let aggregate = union_aggregate_layout(ty, tcx, data_types, instance_context)?
                .expect("non-empty tuples have aggregate layouts");
            emit_aggregate_to_union_bytes(
                &aggregate,
                source,
                storage,
                base_offset,
                tcx,
                data_types,
                instance_context,
                instructions,
                temp_counter,
            )
        }
        TyKind::Adt(adt_def, _) if adt_def.is_struct() => {
            match union_aggregate_layout(ty, tcx, data_types, instance_context) {
                Ok(Some(aggregate)) => emit_aggregate_to_union_bytes(
                    &aggregate,
                    source,
                    storage,
                    base_offset,
                    tcx,
                    data_types,
                    instance_context,
                    instructions,
                    temp_counter,
                ),
                Err(_) => {
                    let jvm_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
                    if !jvm_ty.is_jvm_reference_type() {
                        return Err(format!("cannot store unresolved struct {ty:?} in a union"));
                    }
                    emit_object_to_union_storage(
                        source,
                        storage,
                        base_offset,
                        instructions,
                        temp_counter,
                    );
                    Ok(())
                }
                Ok(None) => unreachable!(),
            }
        }
        _ => {
            let jvm_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            if jvm_ty.is_jvm_reference_type() {
                emit_object_to_union_storage(
                    source,
                    storage,
                    base_offset,
                    instructions,
                    temp_counter,
                );
                Ok(())
            } else {
                Err(format!(
                    "unsupported union field type {ty:?}: JVM representation {jvm_ty:?} is neither byte-addressable nor a reference"
                ))
            }
        }
    }
}

fn emit_bits_from_union_bytes(
    bits_ty: oomir::Type,
    rust_size: usize,
    storage: &JvmUnionStorage,
    base_offset: usize,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> oomir::Operand {
    let mut accum: Option<oomir::Operand> = None;
    for byte_idx in 0..rust_size {
        let raw_dest = next_union_temp("union_raw_byte", temp_counter);
        let index = storage.byte_index(base_offset + byte_idx, instructions, temp_counter);
        instructions.push(oomir::Instruction::ArrayGet {
            dest: raw_dest.clone(),
            array: operand_var(storage.bytes_var.clone(), byte_array_type()),
            index,
        });

        let widened_dest = next_union_temp("union_wide_byte", temp_counter);
        instructions.push(oomir::Instruction::Cast {
            op: operand_var(raw_dest, oomir::Type::I8),
            ty: bits_ty.clone(),
            dest: widened_dest.clone(),
        });

        let masked_dest = next_union_temp("union_masked_byte", temp_counter);
        instructions.push(oomir::Instruction::BitAnd {
            dest: masked_dest.clone(),
            op1: operand_var(widened_dest, bits_ty.clone()),
            op2: oomir::Operand::Constant(int_constant_for_type(0xff, &bits_ty)),
        });

        let byte_value = if byte_idx == 0 {
            operand_var(masked_dest, bits_ty.clone())
        } else {
            let shifted_dest = next_union_temp("union_decode_shift", temp_counter);
            instructions.push(oomir::Instruction::Shl {
                dest: shifted_dest.clone(),
                op1: operand_var(masked_dest, bits_ty.clone()),
                op2: oomir::Operand::Constant(oomir::Constant::I32((byte_idx * 8) as i32)),
            });
            operand_var(shifted_dest, bits_ty.clone())
        };

        accum = Some(if let Some(prev) = accum {
            let combined_dest = next_union_temp("union_decode_or", temp_counter);
            instructions.push(oomir::Instruction::BitOr {
                dest: combined_dest.clone(),
                op1: prev,
                op2: byte_value,
            });
            operand_var(combined_dest, bits_ty.clone())
        } else {
            byte_value
        });
    }

    accum.unwrap_or_else(|| {
        oomir::Operand::Constant(if matches!(bits_ty, oomir::Type::I64) {
            oomir::Constant::I64(0)
        } else {
            oomir::Constant::I32(0)
        })
    })
}

fn emit_scalar_from_union_bytes<'tcx>(
    ty: Ty<'tcx>,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<oomir::Operand, String> {
    let rust_size = layout_size_bytes(tcx, ty)?;
    let oomir_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
    let bits_ty = match ty.kind() {
        TyKind::Float(FloatTy::F32) => oomir::Type::I32,
        TyKind::Float(FloatTy::F64) => oomir::Type::I64,
        TyKind::Float(_) => {
            return Err(format!("unsupported float width in union field: {:?}", ty));
        }
        TyKind::Int(IntTy::I128) | TyKind::Uint(UintTy::U64 | UintTy::U128) => {
            return Err(format!("unsupported wide integer union field: {:?}", ty));
        }
        TyKind::Bool | TyKind::Char | TyKind::Int(_) | TyKind::Uint(_) => {
            scalar_bits_type(rust_size, &oomir_ty)
        }
        _ => return Err(format!("unsupported scalar union field: {:?}", ty)),
    };
    let bits_operand = emit_bits_from_union_bytes(
        bits_ty.clone(),
        rust_size,
        storage,
        base_offset,
        instructions,
        temp_counter,
    );

    match ty.kind() {
        TyKind::Float(FloatTy::F32) => {
            let dest = next_union_temp("union_f32_value", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.clone()),
                class_name: "java/lang/Float".to_string(),
                method_name: "intBitsToFloat".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("bits".to_string(), oomir::Type::I32)],
                    ret: Box::new(oomir::Type::F32),
                    is_static: true,
                },
                args: vec![bits_operand],
            });
            Ok(operand_var(dest, oomir::Type::F32))
        }
        TyKind::Float(FloatTy::F64) => {
            let dest = next_union_temp("union_f64_value", temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(dest.clone()),
                class_name: "java/lang/Double".to_string(),
                method_name: "longBitsToDouble".to_string(),
                method_ty: oomir::Signature {
                    params: vec![("bits".to_string(), oomir::Type::I64)],
                    ret: Box::new(oomir::Type::F64),
                    is_static: true,
                },
                args: vec![bits_operand],
            });
            Ok(operand_var(dest, oomir::Type::F64))
        }
        _ if oomir_ty == bits_ty => Ok(bits_operand),
        _ => {
            let dest = next_union_temp("union_scalar_value", temp_counter);
            instructions.push(oomir::Instruction::Cast {
                op: bits_operand,
                ty: oomir_ty.clone(),
                dest: dest.clone(),
            });
            Ok(operand_var(dest, oomir_ty))
        }
    }
}

fn emit_ty_from_union_bytes<'tcx>(
    ty: Ty<'tcx>,
    storage: &JvmUnionStorage,
    base_offset: usize,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_counter: &mut usize,
) -> Result<oomir::Operand, String> {
    let ty = resolve_union_ty(tcx, ty, instance_context)?;
    if is_direct_union_scalar(ty) {
        return emit_scalar_from_union_bytes(
            ty,
            storage,
            base_offset,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        );
    }

    match ty.kind() {
        TyKind::Pat(inner, _) => emit_ty_from_union_bytes(
            *inner,
            storage,
            base_offset,
            tcx,
            data_types,
            instance_context,
            instructions,
            temp_counter,
        ),
        TyKind::Float(FloatTy::F128) => {
            let low = emit_bits_from_union_bytes(
                oomir::Type::I64,
                8,
                storage,
                base_offset,
                instructions,
                temp_counter,
            );
            let high = emit_bits_from_union_bytes(
                oomir::Type::I64,
                8,
                storage,
                base_offset + 8,
                instructions,
                temp_counter,
            );
            let dest = next_union_temp("union_f128_value", temp_counter);
            instructions.push(oomir::Instruction::ConstructObject {
                dest: dest.clone(),
                class_name: crate::lower2::F128_CLASS.to_string(),
                args: vec![(high, oomir::Type::I64), (low, oomir::Type::I64)],
            });
            Ok(operand_var(
                dest,
                oomir::Type::Class(crate::lower2::F128_CLASS.to_string()),
            ))
        }
        TyKind::Array(element_ty, length) => {
            let length = length
                .try_to_target_usize(tcx)
                .ok_or_else(|| format!("array length is not concrete for {:?}", ty))?
                as usize;
            let element_size = layout_size_bytes(tcx, *element_ty)?;
            if element_size == 0 {
                return Err(format!(
                    "zero-sized array elements are unsupported in union field {:?}",
                    ty
                ));
            }
            let element_oomir_ty = ty_to_oomir_type(*element_ty, tcx, data_types, instance_context);
            let array_ty = oomir::Type::Array(Box::new(element_oomir_ty.clone()));
            let array_dest = next_union_temp("union_array_value", temp_counter);
            instructions.push(oomir::Instruction::NewArray {
                dest: array_dest.clone(),
                element_type: element_oomir_ty,
                size: oomir::Operand::Constant(oomir::Constant::I32(length as i32)),
            });

            for index in 0..length {
                let element_value = emit_ty_from_union_bytes(
                    *element_ty,
                    storage,
                    base_offset + index * element_size,
                    tcx,
                    data_types,
                    instance_context,
                    instructions,
                    temp_counter,
                )?;
                instructions.push(oomir::Instruction::ArrayStore {
                    array: array_dest.clone(),
                    index: oomir::Operand::Constant(oomir::Constant::I32(index as i32)),
                    value: element_value,
                });
            }
            Ok(operand_var(array_dest, array_ty))
        }
        TyKind::Adt(adt_def, substs) if adt_def.is_enum() => {
            let enum_oomir_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            let oomir::Type::Class(enum_class) = &enum_oomir_ty else {
                return Err(format!("enum {ty:?} did not map to a JVM class"));
            };
            ensure_enum_union_codec(
                adt_def,
                substs,
                ty,
                enum_class,
                tcx,
                data_types,
                instance_context,
            )?;
            let enum_dest = next_union_temp("union_enum_value", temp_counter);
            let offset = storage.byte_index(base_offset, instructions, temp_counter);
            instructions.push(oomir::Instruction::InvokeStatic {
                dest: Some(enum_dest.clone()),
                class_name: enum_class.clone(),
                method_name: ENUM_READ_UNION_STORAGE_METHOD.to_string(),
                method_ty: enum_union_read_signature(enum_class),
                args: vec![
                    operand_var(storage.bytes_var.clone(), byte_array_type()),
                    operand_var(storage.objects_var.clone(), object_array_type()),
                    offset,
                ],
            });
            Ok(operand_var(enum_dest, enum_oomir_ty))
        }
        TyKind::Tuple(elements) if elements.is_empty() => {
            Ok(oomir::Operand::Constant(oomir::Constant::Unit))
        }
        TyKind::Tuple(elements) if !elements.is_empty() => {
            let aggregate = union_aggregate_layout(ty, tcx, data_types, instance_context)?
                .expect("non-empty tuples have aggregate layouts");
            emit_aggregate_from_union_bytes(
                &aggregate,
                storage,
                base_offset,
                tcx,
                data_types,
                instance_context,
                instructions,
                temp_counter,
            )
        }
        TyKind::Adt(adt_def, _) if adt_def.is_struct() => {
            match union_aggregate_layout(ty, tcx, data_types, instance_context) {
                Ok(Some(aggregate)) => emit_aggregate_from_union_bytes(
                    &aggregate,
                    storage,
                    base_offset,
                    tcx,
                    data_types,
                    instance_context,
                    instructions,
                    temp_counter,
                ),
                Err(_) => {
                    let jvm_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
                    if !jvm_ty.is_jvm_reference_type() {
                        return Err(format!("cannot load unresolved struct {ty:?} from a union"));
                    }
                    Ok(emit_object_from_union_storage(
                        jvm_ty,
                        storage,
                        base_offset,
                        instructions,
                        temp_counter,
                    ))
                }
                Ok(None) => unreachable!(),
            }
        }
        _ => {
            let jvm_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            if jvm_ty.is_jvm_reference_type() {
                Ok(emit_object_from_union_storage(
                    jvm_ty,
                    storage,
                    base_offset,
                    instructions,
                    temp_counter,
                ))
            } else {
                Err(format!(
                    "unsupported union field type {ty:?}: JVM representation {jvm_ty:?} is neither byte-addressable nor a reference"
                ))
            }
        }
    }
}

fn unsupported_union_body(message: String) -> oomir::CodeBlock {
    oomir::CodeBlock {
        entry: "bb0".to_string(),
        basic_blocks: HashMap::from([(
            "bb0".to_string(),
            oomir::BasicBlock {
                label: "bb0".to_string(),
                instructions: vec![oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/UnsupportedOperationException".to_string(),
                    message,
                }],
            },
        )]),
    }
}

fn simple_body(instructions: Vec<oomir::Instruction>) -> oomir::CodeBlock {
    oomir::CodeBlock {
        entry: "bb0".to_string(),
        basic_blocks: HashMap::from([(
            "bb0".to_string(),
            oomir::BasicBlock {
                label: "bb0".to_string(),
                instructions,
            },
        )]),
    }
}

fn union_from_function<'tcx>(
    union_class: &str,
    union_size: usize,
    field_name: &str,
    field_ty: Ty<'tcx>,
    field_oomir_ty: oomir::Type,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Function {
    let is_unit = !field_oomir_ty.has_jvm_value();
    let mut instructions = vec![
        oomir::Instruction::NewArray {
            dest: "_bytes".to_string(),
            element_type: oomir::Type::I8,
            size: oomir::Operand::Constant(oomir::Constant::I32(union_size as i32)),
        },
        oomir::Instruction::NewArray {
            dest: "_objects".to_string(),
            element_type: oomir::Type::Class("java/lang/Object".to_string()),
            size: oomir::Operand::Constant(oomir::Constant::I32(union_size.max(1) as i32)),
        },
    ];
    let mut temp_counter = 0;
    let storage = JvmUnionStorage::at_start("_bytes", "_objects");
    let body = match emit_ty_to_union_bytes(
        field_ty,
        operand_var("_1", field_oomir_ty.clone()),
        &storage,
        0,
        tcx,
        data_types,
        instance_context,
        &mut instructions,
        &mut temp_counter,
    ) {
        Ok(()) => {
            instructions.push(oomir::Instruction::ConstructObject {
                dest: "_ret".to_string(),
                class_name: union_class.to_string(),
                args: vec![
                    (operand_var("_bytes", byte_array_type()), byte_array_type()),
                    (
                        operand_var("_objects", object_array_type()),
                        object_array_type(),
                    ),
                ],
            });
            instructions.push(oomir::Instruction::Return {
                operand: Some(operand_var(
                    "_ret",
                    oomir::Type::Class(union_class.to_string()),
                )),
            });
            simple_body(instructions)
        }
        Err(err) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!(
                    "Union constructor helper for {}.{} is unsupported: {}",
                    union_class, field_name, err
                )
            );
            unsupported_union_body(err)
        }
    };

    oomir::Function {
        name: union_from_method_name(field_name),
        owner_class: None,
        signature: oomir::Signature {
            params: if is_unit {
                vec![]
            } else {
                vec![("value".to_string(), field_oomir_ty)]
            },
            ret: Box::new(oomir::Type::Class(union_class.to_string())),
            is_static: true,
        },
        body,
    }
}

fn union_getter_function<'tcx>(
    union_class: &str,
    field_name: &str,
    field_ty: Ty<'tcx>,
    field_oomir_ty: oomir::Type,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Function {
    if !field_oomir_ty.has_jvm_value() {
        return oomir::Function {
            name: union_getter_method_name(field_name),
            owner_class: None,
            signature: oomir::Signature {
                params: vec![(
                    "self".to_string(),
                    oomir::Type::Class(union_class.to_string()),
                )],
                ret: Box::new(oomir::Type::Void),
                is_static: false,
            },
            body: simple_body(vec![oomir::Instruction::Return { operand: None }]),
        };
    }

    let mut instructions = vec![
        oomir::Instruction::GetField {
            dest: "_bytes".to_string(),
            object: operand_var("_1", oomir::Type::Class(union_class.to_string())),
            field_name: UNION_BYTES_FIELD.to_string(),
            field_ty: byte_array_type(),
            owner_class: union_class.to_string(),
        },
        oomir::Instruction::GetField {
            dest: "_objects".to_string(),
            object: operand_var("_1", oomir::Type::Class(union_class.to_string())),
            field_name: UNION_OBJECTS_FIELD.to_string(),
            field_ty: object_array_type(),
            owner_class: union_class.to_string(),
        },
    ];
    let mut temp_counter = 0;
    let storage = JvmUnionStorage::at_start("_bytes", "_objects");
    let body = match emit_ty_from_union_bytes(
        field_ty,
        &storage,
        0,
        tcx,
        data_types,
        instance_context,
        &mut instructions,
        &mut temp_counter,
    ) {
        Ok(value) => {
            instructions.push(oomir::Instruction::Return {
                operand: Some(value),
            });
            simple_body(instructions)
        }
        Err(err) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!(
                    "Union getter helper for {}.{} is unsupported: {}",
                    union_class, field_name, err
                )
            );
            unsupported_union_body(err)
        }
    };

    oomir::Function {
        name: union_getter_method_name(field_name),
        owner_class: None,
        signature: oomir::Signature {
            params: vec![(
                "self".to_string(),
                oomir::Type::Class(union_class.to_string()),
            )],
            ret: Box::new(field_oomir_ty),
            is_static: false,
        },
        body,
    }
}

fn union_setter_function<'tcx>(
    union_class: &str,
    field_name: &str,
    field_ty: Ty<'tcx>,
    field_oomir_ty: oomir::Type,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Function {
    let is_unit = !field_oomir_ty.has_jvm_value();
    let mut instructions = vec![
        oomir::Instruction::GetField {
            dest: "_bytes".to_string(),
            object: operand_var("_1", oomir::Type::Class(union_class.to_string())),
            field_name: UNION_BYTES_FIELD.to_string(),
            field_ty: byte_array_type(),
            owner_class: union_class.to_string(),
        },
        oomir::Instruction::GetField {
            dest: "_objects".to_string(),
            object: operand_var("_1", oomir::Type::Class(union_class.to_string())),
            field_name: UNION_OBJECTS_FIELD.to_string(),
            field_ty: object_array_type(),
            owner_class: union_class.to_string(),
        },
    ];
    let mut temp_counter = 0;
    let storage = JvmUnionStorage::at_start("_bytes", "_objects");
    let body = match emit_ty_to_union_bytes(
        field_ty,
        operand_var("_2", field_oomir_ty.clone()),
        &storage,
        0,
        tcx,
        data_types,
        instance_context,
        &mut instructions,
        &mut temp_counter,
    ) {
        Ok(()) => {
            instructions.push(oomir::Instruction::Return { operand: None });
            simple_body(instructions)
        }
        Err(err) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!(
                    "Union setter helper for {}.{} is unsupported: {}",
                    union_class, field_name, err
                )
            );
            unsupported_union_body(err)
        }
    };

    oomir::Function {
        name: union_setter_method_name(field_name),
        owner_class: None,
        signature: oomir::Signature {
            params: vec![(
                "self".to_string(),
                oomir::Type::Class(union_class.to_string()),
            )]
            .into_iter()
            .chain((!is_unit).then_some(("value".to_string(), field_oomir_ty)))
            .collect(),
            ret: Box::new(oomir::Type::Void),
            is_static: false,
        },
        body,
    }
}

pub fn ensure_union_data_type<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    let union_class =
        generate_adt_jvm_class_name(adt_def, substs, tcx, data_types, instance_context);

    // Pre-populate with placeholder class to break recursive resolution loops
    if !data_types.contains_key(&union_class) {
        data_types.insert(
            union_class.clone(),
            oomir::DataType::Class {
                fields: vec![
                    (UNION_BYTES_FIELD.to_string(), byte_array_type()),
                    (UNION_OBJECTS_FIELD.to_string(), object_array_type()),
                ],
                is_abstract: false,
                methods: HashMap::new(),
                super_class: Some("java/lang/Object".to_string()),
                interfaces: vec![],
            },
        );
    }

    let union_ty = tcx
        .type_of(adt_def.did())
        .instantiate(tcx, substs)
        .skip_norm_wip();
    let union_ty = resolve_union_ty(tcx, union_ty, instance_context).unwrap_or(union_ty);
    // Unresolved generic unions use one byte plus the object slot at offset zero.
    // Concrete monomorphizations replace this with their exact rustc layout.
    let union_size = layout_size_bytes(tcx, union_ty).unwrap_or(1);

    let variant = adt_def.variant(0usize.into());
    let mut methods = HashMap::new();
    for field_def in variant.fields.iter() {
        let field_name = field_def.ident(tcx).to_string();
        let raw_field_ty = field_def.ty(tcx, substs).skip_norm_wip();
        let field_ty =
            resolve_union_ty(tcx, raw_field_ty, instance_context).unwrap_or(raw_field_ty);
        let field_oomir_ty = ty_to_oomir_type(field_ty, tcx, data_types, instance_context);

        methods.insert(
            union_from_method_name(&field_name),
            DataTypeMethod::Function(union_from_function(
                &union_class,
                union_size,
                &field_name,
                field_ty,
                field_oomir_ty.clone(),
                tcx,
                data_types,
                instance_context,
            )),
        );
        methods.insert(
            union_getter_method_name(&field_name),
            DataTypeMethod::Function(union_getter_function(
                &union_class,
                &field_name,
                field_ty,
                field_oomir_ty.clone(),
                tcx,
                data_types,
                instance_context,
            )),
        );
        methods.insert(
            union_setter_method_name(&field_name),
            DataTypeMethod::Function(union_setter_function(
                &union_class,
                &field_name,
                field_ty,
                field_oomir_ty,
                tcx,
                data_types,
                instance_context,
            )),
        );
    }

    let union_fields = vec![
        (UNION_BYTES_FIELD.to_string(), byte_array_type()),
        (UNION_OBJECTS_FIELD.to_string(), object_array_type()),
    ];
    match data_types.get_mut(&union_class) {
        Some(oomir::DataType::Class {
            fields,
            methods: existing_methods,
            ..
        }) => {
            if !fields.iter().any(|(name, _)| name == UNION_BYTES_FIELD) {
                fields.insert(0, (UNION_BYTES_FIELD.to_string(), byte_array_type()));
            }
            if !fields.iter().any(|(name, _)| name == UNION_OBJECTS_FIELD) {
                fields.push((UNION_OBJECTS_FIELD.to_string(), object_array_type()));
            }
            existing_methods.extend(methods);
        }
        Some(oomir::DataType::Interface { .. }) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!(
                    "Union class name '{}' already exists as an interface",
                    union_class
                )
            );
        }
        None => {
            data_types.insert(
                union_class.clone(),
                oomir::DataType::Class {
                    fields: union_fields,
                    is_abstract: false,
                    methods,
                    super_class: Some("java/lang/Object".to_string()),
                    interfaces: vec![],
                },
            );
        }
    }

    union_class
}

/// Converts a Rust MIR type (`Ty`) to an OOMIR type (`oomir::Type`).
pub fn ty_to_oomir_type<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Type {
    // Check if the instance args contain generic parameters.
    let has_params = instance_context.args.has_param();

    let resolved_ty = if has_params {
        rustc_middle::ty::EarlyBinder::bind(tcx, ty)
            .instantiate(tcx, instance_context.args)
            .skip_norm_wip()
    } else {
        let instantiated =
            rustc_middle::ty::EarlyBinder::bind(tcx, ty).instantiate(tcx, instance_context.args);
        match tcx.try_normalize_erasing_regions(TypingEnv::fully_monomorphized(), instantiated) {
            Ok(normalized) => normalized,
            Err(_) => instantiated.skip_norm_wip(),
        }
    };
    match resolved_ty.kind() {
        rustc_middle::ty::TyKind::Bool => oomir::Type::Boolean,
        // Rust `char` is a 32-bit Unicode scalar value, unlike the JVM's
        // 16-bit UTF-16 `char` primitive. Keep it as an int so supplementary
        // characters are not truncated.
        rustc_middle::ty::TyKind::Char => oomir::Type::I32,
        rustc_middle::ty::TyKind::Int(int_ty) => match int_ty {
            IntTy::I8 => oomir::Type::I8,
            IntTy::I16 => oomir::Type::I16,
            IntTy::I32 => oomir::Type::I32,
            IntTy::I64 => oomir::Type::I64,
            IntTy::Isize => oomir::Type::I32,
            IntTy::I128 => oomir::Type::Class("java/math/BigInteger".to_string()), // doesn't fit in a primitive
        },
        rustc_middle::ty::TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => oomir::Type::I16, // make it the next size up to capture full range
            UintTy::U16 => oomir::Type::I32,
            UintTy::U32 => oomir::Type::I64,
            UintTy::Usize => oomir::Type::I32, // java uses an i32 for most "usize" i.e. array indexes etc.
            UintTy::U64 => oomir::Type::Class("java/math/BigInteger".to_string()),
            UintTy::U128 => oomir::Type::Class("java/math/BigInteger".to_string()),
        },
        rustc_middle::ty::TyKind::Float(float_ty) => match float_ty {
            FloatTy::F32 => oomir::Type::F32,
            FloatTy::F64 => oomir::Type::F64,
            FloatTy::F16 => oomir::Type::F32,
            FloatTy::F128 => oomir::Type::Class(crate::lower2::F128_CLASS.to_string()),
        },
        rustc_middle::ty::TyKind::Adt(adt_def, substs) => {
            // Get the full path string for the ADT
            let full_path_str = tcx.def_path_str(adt_def.did());

            if full_path_str == "String" || full_path_str == "std::string::String" {
                oomir::Type::String
            } else {
                let jvm_name_full = generate_adt_jvm_class_name(
                    &adt_def,
                    substs,
                    tcx,
                    data_types,
                    instance_context,
                );

                if !should_define_named_data_type(tcx, adt_def.did()) {
                    return oomir::Type::Class(jvm_name_full);
                }

                if adt_def.is_struct() {
                    let variant = adt_def.variant(0usize.into());
                    if !data_types.contains_key(&jvm_name_full) {
                        // Pre-populate with a placeholder class to break recursive resolution loops
                        data_types.insert(
                            jvm_name_full.clone(),
                            oomir::DataType::Class {
                                fields: vec![],
                                is_abstract: false,
                                methods: HashMap::new(),
                                super_class: None,
                                interfaces: vec![],
                            },
                        );

                        let oomir_fields = variant
                            .fields
                            .iter()
                            .filter_map(|field_def| {
                                let field_name = field_def.ident(tcx).to_string();
                                let field_ty = field_def.ty(tcx, substs);
                                let field_mir_ty =
                                    if field_ty.has_param() || field_ty.has_escaping_bound_vars() {
                                        field_ty.skip_norm_wip()
                                    } else {
                                        tcx.try_normalize_erasing_regions(
                                            TypingEnv::fully_monomorphized(),
                                            field_ty,
                                        )
                                        .unwrap_or_else(|_| field_ty.skip_norm_wip())
                                    };
                                let field_oomir_type = ty_to_oomir_type(
                                    field_mir_ty,
                                    tcx,
                                    data_types,
                                    instance_context,
                                );
                                field_oomir_type
                                    .has_jvm_value()
                                    .then_some((field_name, field_oomir_type))
                            })
                            .collect::<Vec<_>>();
                        let mut methods = HashMap::new();
                        methods.insert(
                            "eq".to_string(),
                            DataTypeMethod::AdtHelperMethod {
                                kind: oomir::AdtHelperKind::PartialEqClass {
                                    fields: oomir_fields.clone(),
                                },
                            },
                        );
                        if let Some(oomir::DataType::Class {
                            fields,
                            methods: existing_methods,
                            ..
                        }) = data_types.get_mut(&jvm_name_full)
                        {
                            *fields = oomir_fields;
                            *existing_methods = methods;
                        }
                    } else if let Some(oomir::DataType::Class {
                        fields, methods, ..
                    }) = data_types.get_mut(&jvm_name_full)
                    {
                        methods.entry("eq".to_string()).or_insert_with(|| {
                            DataTypeMethod::AdtHelperMethod {
                                kind: oomir::AdtHelperKind::PartialEqClass {
                                    fields: fields.clone(),
                                },
                            }
                        });
                    }
                } else if adt_def.is_enum() {
                    ensure_enum_data_types(
                        &adt_def,
                        substs,
                        &jvm_name_full,
                        tcx,
                        data_types,
                        instance_context,
                    );
                } else if adt_def.is_union() {
                    ensure_union_data_type(&adt_def, substs, tcx, data_types, instance_context);
                }
                oomir::Type::Class(jvm_name_full)
            }
        }
        rustc_middle::ty::TyKind::Str => oomir::Type::Str,
        rustc_middle::ty::TyKind::Pat(inner_ty, _) => {
            ty_to_oomir_type(*inner_ty, tcx, data_types, instance_context)
        }
        rustc_middle::ty::TyKind::Ref(_, inner_ty, mutability) => {
            let pointee_oomir_type = ty_to_oomir_type(*inner_ty, tcx, data_types, instance_context);
            // For trait objects (&dyn Trait, &mut dyn Trait), represent as direct Interface
            // rather than using the array wrapper, since we call virtual methods on the object
            if matches!(
                inner_ty.kind(),
                rustc_middle::ty::TyKind::Dynamic(_, _)
                    | rustc_middle::ty::TyKind::Slice(_)
                    | rustc_middle::ty::TyKind::Str
            ) {
                pointee_oomir_type
            } else if let rustc_middle::ty::TyKind::Array(element_ty, _) = inner_ty.kind() {
                oomir::Type::Slice(Box::new(ty_to_oomir_type(
                    *element_ty,
                    tcx,
                    data_types,
                    instance_context,
                )))
            } else if mutability.is_mut() {
                oomir::Type::MutableReference(Box::new(pointee_oomir_type))
            } else {
                pointee_oomir_type
            }
        }
        rustc_middle::ty::TyKind::RawPtr(ty, _mutability) => {
            if ty.is_str() {
                // A raw pointer to a string slice (*const str) is semantically a reference
                // to string data. Its OOMIR representation should be consistent with &str.
                oomir::Type::Str
            } else if ty.is_slice() {
                // Preserve the pointer metadata as a slice view.
                let component_ty = ty.sequence_element_type(tcx);
                let oomir_component_type =
                    ty_to_oomir_type(component_ty, tcx, data_types, instance_context);
                oomir::Type::Slice(Box::new(oomir_component_type))
            } else {
                // For a pointer to a sized type (*const T), use the mutable reference
                // "array hack" to represent it as a reference that can be written back to.
                let oomir_pointee_type = ty_to_oomir_type(*ty, tcx, data_types, instance_context);
                if matches!(oomir_pointee_type, oomir::Type::Void) {
                    oomir::Type::Class("java/lang/Object".to_string())
                } else {
                    oomir::Type::MutableReference(Box::new(oomir_pointee_type))
                }
            }
        }
        rustc_middle::ty::TyKind::Array(component_ty, _) => {
            // Special case for arrays of string references
            if let TyKind::Ref(_, inner_ty, _) = component_ty.kind() {
                if inner_ty.is_str() {
                    return oomir::Type::Array(Box::new(oomir::Type::Str));
                }
            }
            // Default array handling
            oomir::Type::Array(Box::new(ty_to_oomir_type(
                *component_ty,
                tcx,
                data_types,
                instance_context,
            )))
        }
        rustc_middle::ty::TyKind::Tuple(tuple_elements) => {
            // Unit is an inhabited Rust value, but occupies no JVM stack or local slot.
            if tuple_elements.is_empty() {
                return oomir::Type::Unit;
            }

            // Handle non-empty tuples -> generate a class
            let element_mir_tys: Vec<Ty<'tcx>> = tuple_elements.iter().collect(); // Collect MIR types

            // Generate the JVM class name for this specific tuple type
            let tuple_class_name =
                generate_tuple_jvm_class_name(&element_mir_tys, tcx, data_types, instance_context);

            // Check if we've already created the DataType for this tuple signature
            if !data_types.contains_key(&tuple_class_name) {
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "type-mapping",
                    format!(
                        "Info: Defining new tuple type class: {} for MIR type {:?}",
                        tuple_class_name, ty
                    )
                );
                // Create the fields ("field0", "field1", ...) and their OOMIR types
                let oomir_fields = element_mir_tys
                    .iter()
                    .enumerate()
                    .map(|(i, &elem_ty)| {
                        let field_name = format!("field{}", i);
                        // Recursively convert element type to OOMIR type
                        let field_oomir_type =
                            ty_to_oomir_type(elem_ty, tcx, data_types, instance_context);
                        (field_name, field_oomir_type)
                    })
                    .collect::<Vec<_>>();

                let mut methods = HashMap::new();
                methods.insert(
                    "eq".to_string(),
                    DataTypeMethod::AdtHelperMethod {
                        kind: oomir::AdtHelperKind::PartialEqClass {
                            fields: oomir_fields.clone(),
                        },
                    },
                );

                // Create and insert the DataType definition
                let tuple_data_type = oomir::DataType::Class {
                    fields: oomir_fields,
                    is_abstract: false,
                    methods,
                    super_class: None,
                    interfaces: vec![],
                };
                data_types.insert(tuple_class_name.clone(), tuple_data_type);
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "type-mapping",
                    format!("   -> Added DataType: {:?}", data_types[&tuple_class_name])
                );
            } else {
                if let Some(oomir::DataType::Class {
                    fields, methods, ..
                }) = data_types.get_mut(&tuple_class_name)
                {
                    methods.entry("eq".to_string()).or_insert_with(|| {
                        DataTypeMethod::AdtHelperMethod {
                            kind: oomir::AdtHelperKind::PartialEqClass {
                                fields: fields.clone(),
                            },
                        }
                    });
                }
                breadcrumbs::log!(
                    breadcrumbs::LogLevel::Info,
                    "type-mapping",
                    format!(
                        "Info: Reusing existing tuple type class: {}",
                        tuple_class_name
                    )
                );
            }

            // Return the OOMIR type as a Class reference
            oomir::Type::Class(tuple_class_name)
        }
        rustc_middle::ty::TyKind::Slice(component_ty) => {
            // Special case for slices of string references
            if let TyKind::Ref(_, inner_ty, _) = component_ty.kind() {
                if inner_ty.is_str() {
                    return oomir::Type::Slice(Box::new(oomir::Type::Str));
                }
            }
            // Default slice handling
            oomir::Type::Slice(Box::new(ty_to_oomir_type(
                *component_ty,
                tcx,
                data_types,
                instance_context,
            )))
        }
        rustc_middle::ty::TyKind::Never => {
            // Handle the never type
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "type-mapping",
                "Info: Mapping Never type to OOMIR Void"
            );
            oomir::Type::Void
        }
        rustc_middle::ty::TyKind::Dynamic(bound_preds, _region) => {
            // bound_preds is a collection of `Binder<ExistentialPredicate<'tcx>>` entries.
            // Iterate and resolve trait predicates into OOMIR interface types.
            let mut resolved_types: Vec<oomir::Type> = Vec::new();
            for binder in bound_preds.iter() {
                match binder.skip_binder() {
                    ExistentialPredicate::Trait(trait_ref) => {
                        let safe_name = jvm_names::class_for_def_id(tcx, trait_ref.def_id);
                        if should_define_named_data_type(tcx, trait_ref.def_id) {
                            data_types.entry(safe_name.clone()).or_insert_with(|| {
                                oomir::DataType::Interface {
                                    methods: HashMap::new(),
                                }
                            });
                        }
                        resolved_types.push(oomir::Type::Interface(safe_name));
                    }
                    ExistentialPredicate::AutoTrait(def_id) => {
                        // Auto traits like Send/Sync — treat as interfaces as well.
                        let safe_name = jvm_names::class_for_def_id(tcx, def_id);
                        if should_define_named_data_type(tcx, def_id) {
                            data_types.entry(safe_name.clone()).or_insert_with(|| {
                                oomir::DataType::Interface {
                                    methods: HashMap::new(),
                                }
                            });
                        }
                        resolved_types.push(oomir::Type::Interface(safe_name));
                    }
                    ExistentialPredicate::Projection(_) => {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "type-mapping",
                            format!(
                                "Warning: Unhandled dynamic projection predicate {:?}",
                                binder
                            )
                        );
                        resolved_types.push(oomir::Type::Class("java/lang/Object".to_string()));
                    }
                }
            }
            // Return the first resolved bound, or fall back to Object.
            resolved_types
                .get(0)
                .cloned()
                .unwrap_or(oomir::Type::Class("java/lang/Object".to_string()))
        }
        rustc_middle::ty::TyKind::Param(param_ty) => {
            oomir::Type::Class(jvm_names::member_name(param_ty.name.as_str()))
        }
        rustc_middle::ty::TyKind::Closure(def_id, args) => {
            let safe_name = jvm_names::closure_class_for_args(tcx, *def_id, args);

            // Define the closure class struct if not already present
            if should_define_named_data_type(tcx, *def_id) && !data_types.contains_key(&safe_name) {
                let closure_args = args.as_closure();
                let upvar_tys = closure_args.upvar_tys();

                let mut fields = Vec::new();
                for (i, upvar_ty) in upvar_tys.iter().enumerate() {
                    let field_name = format!("arg{}", i);
                    // Recursively resolve capture types
                    let field_oomir_ty =
                        ty_to_oomir_type(upvar_ty, tcx, data_types, instance_context);
                    fields.push((field_name, field_oomir_ty));
                }

                data_types.insert(
                    safe_name.clone(),
                    oomir::DataType::Class {
                        fields,
                        is_abstract: false,
                        methods: HashMap::new(), // 'call' is handled via MIR lowering logic
                        super_class: Some("java/lang/Object".to_string()),
                        interfaces: vec![],
                    },
                );
            }
            oomir::Type::Class(safe_name)
        }
        rustc_middle::ty::TyKind::FnPtr(_, _) => {
            let signature =
                fn_ptr_signature_from_ty(resolved_ty, tcx, data_types, instance_context);
            let interface_name =
                ensure_fn_ptr_interface(&signature, data_types, tcx, instance_context);
            oomir::Type::Interface(interface_name)
        }
        rustc_middle::ty::TyKind::FnDef(def_id, _args) => {
            // Named functions are Zero-Sized Types (ZSTs).
            // We generate a singleton class so generics like Map<Iter, MyFunc>
            // produce unique JVM class names.
            let safe_name = jvm_names::class_for_def_id(tcx, *def_id);

            if should_define_named_data_type(tcx, *def_id) && !data_types.contains_key(&safe_name) {
                data_types.insert(
                    safe_name.clone(),
                    oomir::DataType::Class {
                        fields: vec![], // No state
                        is_abstract: false,
                        methods: HashMap::new(),
                        super_class: Some("java/lang/Object".to_string()),
                        interfaces: vec![],
                    },
                );
            }
            oomir::Type::Class(safe_name)
        }
        rustc_middle::ty::TyKind::Alias(_, _alias_ty) => {
            // This handles associated types and projections (like Self::Output)
            // that couldn't be normalized because we are in a generic context.
            // We map them to java/lang/Object, effectively erasing them.
            oomir::Type::Class("java/lang/Object".to_string())
        }
        _ => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!("Warning: Unhandled type {:?}", ty)
            );
            oomir::Type::Class("java/lang/Object".to_string())
        }
    }
}

/// Generates a short hash of the input string.
/// The hash is truncated to the specified length to ensure it fits within JVM class name constraints.
pub fn short_hash(input: &str, length: usize) -> String {
    let mut hasher = sha2::Sha256::new();
    hasher.update(input);
    let full_hash = format!("{:x}", hasher.finalize());
    full_hash[..length].to_string()
}

// Maximum length for a readable tuple name (including the "Tuple_" prefix).
// If the human-readable name exceeds this, we fall back to the hashed name.
const MAX_TUPLE_NAME_LEN: usize = 64;

// Produce a compact, human readable token for an OOMIR type to use in tuple class names.
pub fn readable_oomir_type_name(t: &oomir::Type) -> String {
    use oomir::Type;
    match t {
        Type::Boolean => "bool".to_string(),
        Type::Char => "char".to_string(),
        Type::I8 => "i8".to_string(),
        Type::I16 => "i16".to_string(),
        Type::I32 => "i32".to_string(),
        Type::I64 => "i64".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::Str => "Str".to_string(),
        Type::String => "String".to_string(),
        Type::Void => "Void".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Class(name) => {
            // take last path segment for readability (e.g. java/lang/String -> String)
            name.rsplit('/').next().unwrap_or(name).to_string()
        }
        Type::Array(inner) => format!("{}Array", readable_oomir_type_name(inner)),
        Type::Slice(inner) => format!("{}Slice", readable_oomir_type_name(inner)),
        Type::MutableReference(inner) => format!("Ref{}", readable_oomir_type_name(inner)),
        Type::Interface(name) => {
            // prefix interfaces with I to avoid conflicts with classes
            let seg = name.rsplit('/').next().unwrap_or(name);
            format!("I{}", seg)
        }
        // Fallback for any other variants
        _ => "Unsupported".to_string(),
    }
}

// Sanitize token so it contains only ASCII alphanumeric characters and underscores.
pub fn sanitize_name_token(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
}

fn adt_base_jvm_name<'tcx>(adt_def: &AdtDef<'tcx>, tcx: TyCtxt<'tcx>) -> String {
    jvm_names::class_for_def_id(tcx, adt_def.did())
}

/// Generate a JVM-safe class name for an ADT (struct/enum) including readable generic
/// substitution tokens. Falls back to a hashed last segment when the full name is too long.
pub fn generate_adt_jvm_class_name<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    let base_jvm = adt_base_jvm_name(adt_def, tcx);

    // Build readable generic tokens from substitutions (if any)
    let mut generic_tokens: Vec<String> = Vec::new();
    for arg in substs.iter() {
        if let Some(arg_ty) = arg.as_type() {
            let oomir_ty = ty_to_oomir_type(arg_ty, tcx, data_types, instance_context);
            let token = readable_oomir_type_name(&oomir_ty);
            generic_tokens.push(sanitize_name_token(&token));
        } else {
            generic_tokens.push("_".to_string());
        }
    }

    // Attach generics to the last path segment for readability
    let (prefix, last_segment) = match base_jvm.rsplit_once('/') {
        Some((p, l)) => (p.to_string(), l.to_string()),
        None => ("".to_string(), base_jvm.clone()),
    };

    let mut last_with_gens = last_segment.clone();
    if !generic_tokens.is_empty() {
        last_with_gens = format!("{}_{}", last_segment, generic_tokens.join("_"));
    }

    let mut jvm_name_full = if prefix.is_empty() {
        last_with_gens.clone()
    } else {
        format!("{}/{}", prefix, last_with_gens)
    };

    // If the name is too long, fall back to hashed form
    if jvm_name_full.len() > MAX_TUPLE_NAME_LEN {
        let mut name_parts = String::new();
        name_parts.push_str(&base_jvm);
        name_parts.push_str("_");
        for arg in substs.iter() {
            if let Some(arg_ty) = arg.as_type() {
                let oomir_ty = ty_to_oomir_type(arg_ty, tcx, data_types, instance_context);
                name_parts.push_str(&oomir_ty.to_jvm_descriptor());
                name_parts.push_str("_");
            }
        }
        let hash = short_hash(&name_parts, 10);
        let hashed_last = format!("{}_{}", last_segment, hash);
        jvm_name_full = if prefix.is_empty() {
            hashed_last
        } else {
            format!("{}/{}", prefix, hashed_last)
        };
    }

    jvm_name_full
}

/// Generates a JVM class name for a tuple type based on its element types.
/// The name is derived from the hash of the element types to ensure uniqueness.
/// The hash is truncated to a specified length to avoid excessively long names.
pub fn generate_tuple_jvm_class_name<'tcx>(
    element_tys: &[Ty<'tcx>],
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>, // Needed for recursive calls
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> String {
    // First attempt: build a human-readable name like `Tuple_i32_String`
    let mut tokens: Vec<String> = Vec::new();
    for ty in element_tys {
        let oomir_ty = ty_to_oomir_type(*ty, tcx, data_types, instance_context);
        let token = readable_oomir_type_name(&oomir_ty);
        tokens.push(sanitize_name_token(&token));
    }

    let readable_name = jvm_names::synthetic_class_for_instance(
        tcx,
        instance_context,
        format!("Tuple_{}", tokens.join("_")),
    );

    // If the readable name is within bounds, use it. Otherwise fall back to a hash
    if readable_name.len() <= MAX_TUPLE_NAME_LEN {
        readable_name
    } else {
        // build hashed name using JVM descriptors (previous behaviour)
        let mut name_parts = String::new();
        for ty in element_tys {
            // convert to oomir type
            let ty = ty_to_oomir_type(*ty, tcx, data_types, instance_context);
            name_parts.push_str(&ty.to_jvm_descriptor());
            name_parts.push_str("_");
        }
        // with a length of 10, the chance of a collision is tiny
        let hash = short_hash(&name_parts, 10);
        jvm_names::synthetic_class_for_instance(tcx, instance_context, format!("Tuple_{}", hash))
    }
}

// A helper function to convert MIR integer values to OOMIR Constants, respecting type
pub fn mir_int_to_oomir_const<'tcx>(
    value: u128,
    ty: Ty<'tcx>,
    _tcx: TyCtxt<'tcx>,
) -> oomir::Constant {
    match ty.kind() {
        TyKind::Int(int_ty) => match int_ty {
            // Cast u128 carefully to avoid panic/wrap-around if value is out of range
            IntTy::I8 => oomir::Constant::I8(value as i8),
            IntTy::I16 => oomir::Constant::I16(value as i16),
            IntTy::I32 => oomir::Constant::I32(value as i32),
            IntTy::I64 => oomir::Constant::I64(value as i64),
            IntTy::Isize => oomir::Constant::I32(value as i32), // JVM uses i32 for most "usize" tasks
            IntTy::I128 => oomir::Constant::Instance {
                class_name: "java/math/BigInteger".to_string(),
                params: vec![oomir::Constant::String(value.to_string())],
                fields: HashMap::new(),
            }, // Handle large integers
        },
        TyKind::Uint(uint_ty) => match uint_ty {
            // JVM uses signed types, treat appropriately
            // maps to the next size up to capture full range
            UintTy::U8 => oomir::Constant::I16(value as i16),
            UintTy::U16 => oomir::Constant::I32(value as i32),
            UintTy::U32 => oomir::Constant::I64(value as i64),
            UintTy::Usize => oomir::Constant::I32(value as i32), // java uses an i32 for most "usize" tasks i.e. array indexes etc.
            UintTy::U64 | UintTy::U128 => oomir::Constant::Instance {
                class_name: "java/math/BigInteger".to_string(),
                params: vec![oomir::Constant::String(value.to_string())],
                fields: HashMap::new(),
            },
        },
        TyKind::Bool => oomir::Constant::Boolean(value != 0), // 0 is false, non-zero is true
        TyKind::Char => oomir::Constant::I32(value as i32),
        _ => {
            // This case should ideally not happen if MIR is well-typed
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Warn,
                "type-mapping",
                format!(
                    "Warning: Cannot convert MIR integer value {} to OOMIR constant for non-integer type {:?}",
                    value, ty
                )
            );
            oomir::Constant::I32(0) // Default fallback
        }
    }
}

// Helper to get field name from index using DataType info
pub fn get_field_name_from_index(
    owner_class_name: &str,
    index: usize,
    data_types: &HashMap<String, oomir::DataType>,
) -> Result<String, String> {
    // Return Result for error handling
    data_types
        .get(owner_class_name)
        .ok_or_else(|| format!("DataType not found for class '{}'", owner_class_name))
        .and_then(|data_type| match data_type {
            DataType::Class { fields, .. } => fields
                .get(index)
                .ok_or_else(|| {
                    format!(
                        "Field index {} out of bounds for class '{}' (has {} fields)",
                        index,
                        owner_class_name,
                        fields.len()
                    )
                })
                .map(|(name, _)| name.clone()),
            DataType::Interface { .. } => Err(format!(
                "Expected class, found interface {}",
                owner_class_name
            )),
        })
}
