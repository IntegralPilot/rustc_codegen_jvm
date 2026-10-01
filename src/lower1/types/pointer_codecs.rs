use super::*;
use crate::lower1::context::Definitions;

const ZERO_SIZED_CODEC_PREFIX: &str = "@zero-sized:";

pub(super) fn pointer_codec_class_name<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    local_name: &str,
) -> String {
    let root = match ty.kind() {
        TyKind::Tuple(_) | TyKind::Array(_, _) => "org/rustlang/core".to_string(),
        TyKind::Adt(def, _) => jvm_names::crate_root(tcx, def.did().krate),
        TyKind::Closure(def, _) | TyKind::Coroutine(def, _) => {
            jvm_names::crate_root(tcx, def.krate)
        }
        _ => unreachable!("codec owner requires an aggregate"),
    };
    format!("{root}/{}", jvm_names::path_segment(local_name))
}

pub(super) fn default_operand_for_codec(ty: &oomir::Type) -> oomir::Operand {
    let constant = match ty {
        oomir::Type::Boolean => oomir::Constant::Boolean(false),
        oomir::Type::Char => oomir::Constant::Char('\0'),
        oomir::Type::I8 => oomir::Constant::I8(0),
        oomir::Type::U8 => oomir::Constant::U8(0),
        oomir::Type::I16 => oomir::Constant::I16(0),
        oomir::Type::U16 => oomir::Constant::U16(0),
        oomir::Type::F16 => oomir::Constant::F16(0),
        oomir::Type::I32 => oomir::Constant::I32(0),
        oomir::Type::U32 => oomir::Constant::U32(0),
        oomir::Type::I64 => oomir::Constant::I64(0),
        oomir::Type::U64 => oomir::Constant::U64(0),
        oomir::Type::F32 => oomir::Constant::F32(0.0),
        oomir::Type::F64 => oomir::Constant::F64(0.0),
        ty if ty.is_jvm_reference_type() => oomir::Constant::Null(ty.clone()),
        other => panic!("no JVM default value for coroutine field {other:?}"),
    };
    oomir::Operand::Constant(constant)
}

/// Generates an erased runtime codec for aggregate values placed in pointer
/// storage. The codec deliberately reuses union/transmute's rustc-layout
/// encoder so raw byte aliases and ordinary field access observe one value.
pub(crate) fn ensure_pointer_memory_codec<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<Option<PointerMemoryCodec>, String> {
    let ty = resolve_union_ty(tcx, ty, instance_context)?;
    if value_scalar_ty(ty, tcx).is_some() || transparent_payload(ty, tcx).is_some() {
        return Ok(None);
    }
    if let Some(cached) = data_types.completed_codec(ty) {
        // The first requesting shard owns the body. Other shards require only its declarations.
        ty_to_oomir_type(ty, tcx, data_types, instance_context);
        return Ok(Some(cached));
    }
    if let Some(codec) = data_types.building_codecs.get(&ty) {
        return Ok(Some(codec.clone()));
    }
    let result = build_pointer_memory_codec(ty, tcx, data_types, instance_context);
    data_types.building_codecs.remove(&ty);
    if let Ok(Some(codec)) = &result {
        data_types.complete_codec(ty, codec.clone());
    }
    result
}

/// Codec recipes share holders. Method enumeration would also resolve unrelated codec types.
fn register_codec(
    data_types: &mut Definitions<'_>,
    codec: &PointerMemoryCodec,
    methods: HashMap<String, DataTypeMethod>,
) {
    let entry = data_types
        .entry(codec.owner().to_owned())
        .or_insert_with(|| oomir::DataType::Class {
            fields: Vec::new(),
            kind: oomir::ClassKind::Static,
            is_abstract: false,
            methods: HashMap::default(),
            super_class: Some("java/lang/Object".to_owned()),
            interfaces: Vec::new(),
        });
    let oomir::DataType::Class {
        methods: existing, ..
    } = entry
    else {
        unreachable!("codec holder cannot be an interface");
    };
    for (name, method) in methods {
        let DataTypeMethod::Function(mut function) = method else {
            unreachable!("codec operation must have a body");
        };
        function.name = codec.method(&name);
        existing.insert(function.name.clone(), DataTypeMethod::Function(function));
    }
}

fn build_pointer_memory_codec<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<Option<PointerMemoryCodec>, String> {
    if !matches!(
        ty.kind(),
        TyKind::Tuple(_)
            | TyKind::Array(_, _)
            | TyKind::Adt(_, _)
            | TyKind::Closure(_, _)
            | TyKind::Coroutine(_, _)
    ) {
        return Ok(None);
    }
    let size = layout_size_bytes(tcx, ty)?;
    let value_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
    if !value_ty.has_jvm_value() {
        return Ok(None);
    }
    // Primitive array codecs share runtime methods. Each recipe supplies its length and element
    // type.
    if let TyKind::Array(element, _) = ty.kind()
        && let oomir::Type::Array(value) = &value_ty
        && let Some(width) = oomir::Type::pointer((**value).clone()).scalar_address_size()
        && layout_size_bytes(tcx, *element)? == width as usize
    {
        return Ok(Some(PointerMemoryCodec {
            class_name: format!(
                "org/rustlang/runtime/ArrayMemoryCodec#array#{}#{size}",
                value_ty.to_jvm_descriptor()
            ),
        }));
    }
    // These carriers have a public no-argument constructor and no state to
    // encode or bind. Retain the concrete class identity in the codec recipe;
    // no generated codec class or computational helper bodies are needed.
    let fieldless = match ty.kind() {
        TyKind::Closure(_, args) => args.as_closure().upvar_tys().is_empty(),
        TyKind::Adt(def, _) => def.is_struct() && def.non_enum_variant().fields.is_empty(),
        _ => false,
    };
    if size == 0
        && fieldless
        && let oomir::Type::Class(class_name) = &value_ty
    {
        return Ok(Some(PointerMemoryCodec {
            class_name: format!("{ZERO_SIZED_CODEC_PREFIX}{class_name}"),
        }));
    }
    // A pretty-printed `Ty` can use a different crate alias in an upstream
    // crate and a downstream monomorphization (for example `alloc::borrow`
    // versus `std::borrow`). Pointer views of the same Rust allocation must
    // nevertheless select the same codec: codec equality is what lets a
    // retyped view mutate the original JVM carrier instead of a decoded copy.
    // Use rustc's crate-alias-independent type hash, while retaining the JVM
    // descriptor so genuinely different carrier ABIs remain disjoint.
    let identity = format!(
        "{}:{}:{size}",
        stable_type_identity(tcx, ty),
        value_ty.to_jvm_descriptor()
    );
    let key = crate::stable_hash::short_hash(&identity, 16);
    let owner = pointer_codec_class_name(ty, tcx, &format!("Codecs_{}", &key[..2]));
    let codec = PointerMemoryCodec {
        class_name: format!("{owner}#{key}#{}#{size}", value_ty.to_jvm_descriptor()),
    };
    if data_types.has_upstream_codec(&codec.class_name) {
        return Ok(Some(codec));
    }
    exact_bytes_supported(ty, tcx, instance_context, data_types)?;
    data_types.building_codecs.insert(ty, codec.clone());

    if matches!(ty.kind(), TyKind::Coroutine(_, _)) {
        let generated = coroutine_pointer_codec_methods(
            ty,
            value_ty.clone(),
            tcx,
            data_types,
            instance_context,
        );
        register_codec(data_types, &codec, generated?);
        return Ok(Some(codec));
    }

    let bytes_ty = byte_array_type();
    let object_storage_size =
        union_object_storage_size(ty, size, tcx, instance_context, data_types);

    let encode_at_offset = size > 0 && object_storage_size == 0 && range_encoder::supports(ty, tcx);
    let mut encode_instructions = vec![allocate_union_object_storage(
        "_objects",
        object_storage_size,
    )];
    if !encode_at_offset {
        encode_instructions.insert(
            0,
            oomir::Instruction::NewArray {
                dest: "_bytes".to_string(),
                element_type: oomir::Type::I8,
                size: oomir::Operand::Constant(oomir::Constant::I32(size as i32)),
            },
        );
    }
    let encode_storage = if encode_at_offset {
        JvmUnionStorage::at_offset("_2", "_objects", operand_var("_3", oomir::Type::I32))
    } else {
        JvmUnionStorage::at_start("_bytes", "_objects")
    };
    let mut encode_counter = 0;
    let source = if value_ty.has_jvm_value() {
        operand_var("_1", value_ty.clone())
    } else {
        oomir::Operand::Constant(oomir::Constant::Unit)
    };
    if encode_at_offset {
        range_encoder::emit(
            ty,
            source,
            &encode_storage,
            tcx,
            data_types,
            instance_context,
            &mut encode_instructions,
            &mut encode_counter,
        )?;
    } else {
        emit_ty_to_union_bytes(
            ty,
            source,
            &encode_storage,
            0,
            tcx,
            data_types,
            instance_context,
            &mut encode_instructions,
            &mut encode_counter,
        )?;
    }
    encode_instructions.push(oomir::Instruction::Return {
        operand: (!encode_at_offset).then(|| operand_var("_bytes", bytes_ty.clone())),
    });
    let encode = oomir::Function {
        name: if encode_at_offset {
            "encodeAt"
        } else {
            "encode"
        }
        .to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: if encode_at_offset {
                vec![
                    ("value".to_string(), value_ty.clone()),
                    ("bytes".to_string(), bytes_ty.clone()),
                    ("offset".to_string(), oomir::Type::I32),
                ]
            } else {
                vec![("value".to_string(), value_ty.clone())]
            },
            ret: Box::new(if encode_at_offset {
                oomir::Type::Void
            } else {
                bytes_ty.clone()
            }),
            is_static: true,
        },
        body: simple_body(encode_instructions).into(),
    };

    let mut decode_instructions = vec![allocate_union_object_storage(
        "_objects",
        object_storage_size,
    )];
    // Plain values can be decoded directly from a window of their allocation.
    // Keep pointer-bearing codecs on independent images: their object sidecar
    // and provenance handling still require the existing snapshot path.
    let decode_at_offset = object_storage_size == 0;
    let decode_storage = if decode_at_offset {
        JvmUnionStorage::at_offset("_1", "_objects", operand_var("_2", oomir::Type::I32))
    } else {
        JvmUnionStorage::at_start("_1", "_objects")
    };
    let mut decode_counter = 0;
    let decoded = match emit_ty_from_union_bytes(
        ty,
        &decode_storage,
        0,
        tcx,
        data_types,
        instance_context,
        &mut decode_instructions,
        &mut decode_counter,
    ) {
        Ok(decoded) => decoded,
        Err(error) => {
            return Err(error);
        }
    };
    decode_instructions.push(oomir::Instruction::Return {
        operand: value_ty.has_jvm_value().then_some(decoded),
    });
    let decode = oomir::Function {
        name: if decode_at_offset {
            "decodeAt"
        } else {
            "decode"
        }
        .to_string(),
        owner_class: None,
        debug_variables: Vec::new(),
        signature: oomir::Signature {
            params: if decode_at_offset {
                vec![
                    ("bytes".to_string(), bytes_ty.clone()),
                    ("offset".to_string(), oomir::Type::I32),
                ]
            } else {
                vec![("bytes".to_string(), bytes_ty.clone())]
            },
            ret: Box::new(value_ty.clone()),
            is_static: true,
        },
        body: simple_body(decode_instructions).into(),
    };

    let pointer_ty = oomir::Type::pointer(value_ty.clone());
    let mut bind_instructions = Vec::new();
    let mut bind_counter = 0;
    if let Err(error) = emit_memory_view_bindings(
        ty,
        operand_var("_1", pointer_ty.clone()),
        operand_var("_2", value_ty.clone()),
        tcx,
        data_types,
        instance_context,
        &mut bind_instructions,
        &mut bind_counter,
    ) {
        return Err(error);
    }
    let mut methods = HashMap::from_iter([
        (encode.name.clone(), DataTypeMethod::Function(encode)),
        (decode.name.clone(), DataTypeMethod::Function(decode)),
    ]);
    // The runtime supplies whole-buffer adapters and treats absent binders as no-ops.
    if !bind_instructions.is_empty() {
        bind_instructions.push(oomir::Instruction::Return { operand: None });
        let bind = oomir::Function {
            name: "bind".to_string(),
            owner_class: None,
            debug_variables: Vec::new(),
            signature: oomir::Signature {
                params: vec![
                    ("pointer".to_string(), pointer_ty),
                    ("value".to_string(), value_ty),
                ],
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            body: simple_body(bind_instructions).into(),
        };
        methods.insert("bind".to_string(), DataTypeMethod::Function(bind));
    }
    if let TyKind::Array(element_ty, _) = ty.kind() {
        let element_size = layout_size_bytes(tcx, *element_ty)?;
        let element_codec =
            pointer_view_codec_operand(*element_ty, tcx, data_types, instance_context);
        methods.insert(
            "_rustArrayElementSize".to_string(),
            DataTypeMethod::Function(oomir::Function {
                name: "_rustArrayElementSize".to_string(),
                owner_class: None,
                debug_variables: Vec::new(),
                signature: oomir::Signature {
                    params: Vec::new(),
                    ret: Box::new(oomir::Type::I32),
                    is_static: true,
                },
                body: simple_body(vec![oomir::Instruction::Return {
                    operand: Some(oomir::Operand::Constant(oomir::Constant::I32(
                        i32::try_from(element_size)
                            .map_err(|_| "array element layout exceeds JVM address space")?,
                    ))),
                }])
                .into(),
            }),
        );
        methods.insert(
            "_rustArrayElementCodec".to_string(),
            DataTypeMethod::Function(oomir::Function {
                name: "_rustArrayElementCodec".to_string(),
                owner_class: None,
                debug_variables: Vec::new(),
                signature: oomir::Signature {
                    params: Vec::new(),
                    ret: Box::new(oomir::Type::java_string()),
                    is_static: true,
                },
                body: simple_body(vec![oomir::Instruction::Return {
                    operand: Some(element_codec),
                }])
                .into(),
            }),
        );
    }
    register_codec(data_types, &codec, methods);
    Ok(Some(codec))
}

pub(crate) fn pointer_memory_codec_operand<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Operand {
    if let Some(payload) = transparent_payload(data_types.normalize(tcx, ty, instance_context), tcx)
    {
        return pointer_memory_codec_operand(payload.ty, tcx, data_types, instance_context);
    }
    if let Some(codec) = fat_pointer_codec_operand(ty, tcx, data_types, instance_context) {
        return codec;
    }
    if let Some(codec) = pointer_builtin_codec_operand(ty, tcx, data_types, instance_context) {
        return codec;
    }
    match ensure_pointer_memory_codec(ty, tcx, data_types, instance_context) {
        Ok(Some(codec)) => oomir::Operand::Constant(oomir::Constant::String(codec.class_name)),
        Ok(None) => oomir::Operand::Constant(oomir::Constant::Null(oomir::Type::java_string())),
        Err(error) => {
            breadcrumbs::log!(
                breadcrumbs::LogLevel::Info,
                "type-mapping",
                format!("Pointer memory codec is unavailable for {ty:?}: {error}")
            );
            oomir::Operand::Constant(oomir::Constant::Null(oomir::Type::java_string()))
        }
    }
}

/// Codec used when a pointer is a subview/cast into an existing allocation.
/// Aggregate pointees use generated exact-layout codecs. Other JVM reference
/// carriers (fat strings/slices, function values, managed numeric classes,
/// etc.) use the runtime's stable managed-object address representation.
pub(crate) fn pointer_view_codec_operand<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> oomir::Operand {
    if let Some(payload) = transparent_payload(data_types.normalize(tcx, ty, instance_context), tcx)
    {
        return pointer_view_codec_operand(payload.ty, tcx, data_types, instance_context);
    }
    if let Some(codec) = fat_pointer_codec_operand(ty, tcx, data_types, instance_context) {
        return codec;
    }
    if let Some(codec) = pointer_builtin_codec_operand(ty, tcx, data_types, instance_context) {
        return codec;
    }
    match ensure_pointer_memory_codec(ty, tcx, data_types, instance_context) {
        Ok(Some(codec)) => oomir::Operand::Constant(oomir::Constant::String(codec.class_name)),
        Ok(None) => {
            let jvm_ty = ty_to_oomir_type(ty, tcx, data_types, instance_context);
            if matches!(
                ty.kind(),
                TyKind::Tuple(_) | TyKind::Array(_, _) | TyKind::Adt(_, _)
            ) {
                // Aggregate allocations store their JVM carrier directly. If an exact byte
                // codec is unavailable, retaining the allocation's direct view still permits
                // ordinary aligned dereferences. `@managed-object` instead describes an
                // object reference stored *inside* memory and would try to decode address
                // bytes from the aggregate object itself.
                oomir::Operand::Constant(oomir::Constant::Null(oomir::Type::java_string()))
            } else if jvm_ty.is_jvm_reference_type() {
                oomir::Operand::Constant(oomir::Constant::String(
                    MANAGED_OBJECT_POINTER_VIEW_CODEC.to_string(),
                ))
            } else {
                oomir::Operand::Constant(oomir::Constant::Null(oomir::Type::java_string()))
            }
        }
        Err(_) => {
            let resolved = resolve_union_ty(tcx, ty, instance_context).unwrap_or(ty);
            let jvm_ty = ty_to_oomir_type(resolved, tcx, data_types, instance_context);
            if matches!(
                resolved.kind(),
                TyKind::Tuple(_) | TyKind::Array(_, _) | TyKind::Adt(_, _)
            ) {
                oomir::Operand::Constant(oomir::Constant::Null(oomir::Type::java_string()))
            } else if jvm_ty.is_jvm_reference_type() {
                oomir::Operand::Constant(oomir::Constant::String(
                    MANAGED_OBJECT_POINTER_VIEW_CODEC.to_string(),
                ))
            } else {
                oomir::Operand::Constant(oomir::Constant::Null(oomir::Type::java_string()))
            }
        }
    }
}

/// Describes Rust fat pointers that use JVM reference carriers at ordinary
/// call boundaries but still need their native two-word representation when
/// stored in or reinterpreted as byte-addressable memory.
pub(super) fn fat_pointer_codec_operand<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Option<oomir::Operand> {
    let ty = resolve_union_ty(tcx, ty, instance_context).ok()?;
    let pointee = match ty.kind() {
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
        _ => return None,
    };

    let descriptor = if pointee.is_slice() {
        let element = pointee.sequence_element_type(tcx);
        let element_size = layout_size_bytes(tcx, element).ok()?;
        let element_codec = pointer_view_codec_operand(element, tcx, data_types, instance_context);
        let element_codec = match element_codec {
            oomir::Operand::Constant(oomir::Constant::String(codec)) => codec,
            oomir::Operand::Constant(oomir::Constant::Null(_)) => String::new(),
            other => panic!("fat-pointer element codec must be constant, found {other:?}"),
        };
        format!(
            "{SLICE_POINTER_VIEW_CODEC_PREFIX}{}\n{element_size}\n{element_codec}",
            oomir::SLICE_VIEW_CLASS
        )
    } else if pointee.is_str() {
        format!(
            "{SLICE_POINTER_VIEW_CODEC_PREFIX}{}\n1\n",
            oomir::UTF8_VIEW_CLASS
        )
    } else if matches!(ty.kind(), TyKind::RawPtr(_, _))
        && matches!(pointee.kind(), TyKind::Dynamic(..))
    {
        let interface = ty_to_oomir_type(pointee, tcx, data_types, instance_context)
            .get_class_name()
            .expect("trait-object pointee must map to a JVM interface")
            .to_string();
        format!("{TRAIT_POINTER_VIEW_CODEC_PREFIX}{interface}")
    } else {
        if !matches!(pointee.kind(), TyKind::Adt(..)) {
            return None;
        }
        let tail = tcx.struct_tail_for_codegen(pointee, TypingEnv::fully_monomorphized());
        let target_class = ty_to_oomir_type(pointee, tcx, data_types, instance_context)
            .get_class_name()?
            .to_string();
        let prefix_size = layout_size_bytes(tcx, pointee).ok()?;
        if matches!(tail.kind(), TyKind::Dynamic(..)) {
            let interface = ty_to_oomir_type(tail, tcx, data_types, instance_context)
                .get_class_name()?
                .to_string();
            let prefix_codec =
                ensure_pointer_memory_codec(pointee, tcx, data_types, instance_context)
                    .ok()??
                    .class_name;
            return Some(oomir::Operand::Constant(oomir::Constant::String(format!(
                "{STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX}{target_class}\n{prefix_size}\n@trait:{interface}\n0\n{prefix_codec}"
            ))));
        }
        let (carrier_class, element) = if tail.is_slice() {
            (oomir::SLICE_VIEW_CLASS, tail.sequence_element_type(tcx))
        } else if tail.is_str() {
            (oomir::UTF8_VIEW_CLASS, tcx.types.u8)
        } else {
            return None;
        };
        let element_size = layout_size_bytes(tcx, element).ok()?;
        let element_codec = pointer_view_codec_operand(element, tcx, data_types, instance_context);
        let element_codec = match element_codec {
            oomir::Operand::Constant(oomir::Constant::String(codec)) => codec,
            oomir::Operand::Constant(oomir::Constant::Null(_)) => String::new(),
            other => panic!("struct-tail element codec must be constant, found {other:?}"),
        };
        format!(
            "{STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX}{target_class}\n{prefix_size}\n{carrier_class}\n{element_size}\n{element_codec}"
        )
    };

    Some(oomir::Operand::Constant(oomir::Constant::String(
        descriptor,
    )))
}

pub(super) fn pointer_builtin_codec_operand<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Option<oomir::Operand> {
    let ty = resolve_union_ty(tcx, ty, instance_context).ok()?;
    let mut sized_pointer_codec = |pointee: Ty<'tcx>| {
        let pointee_size = layout_size_bytes(tcx, pointee).ok()?;
        let pointee_class = match pointee.kind() {
            TyKind::Tuple(_) | TyKind::Adt(_, _) | TyKind::Closure(_, _) => {
                ty_to_oomir_type(pointee, tcx, data_types, instance_context)
                    .get_class_name()
                    .unwrap_or_default()
                    .to_string()
            }
            _ => String::new(),
        };
        let pointee_codec = pointer_view_codec_operand(pointee, tcx, data_types, instance_context);
        let pointee_codec = match pointee_codec {
            oomir::Operand::Constant(oomir::Constant::String(codec)) => codec,
            oomir::Operand::Constant(oomir::Constant::Null(_)) => String::new(),
            other => panic!("raw-pointer pointee codec must be constant, found {other:?}"),
        };
        Some(format!(
            "{RAW_POINTER_VIEW_CODEC}\n{pointee_size}\n{pointee_class}\n{pointee_codec}"
        ))
    };
    let codec = match ty.kind() {
        TyKind::Ref(_, pointee, _) if let TyKind::Array(element, length) = pointee.kind() => {
            let length = length.try_to_target_usize(tcx)?;
            let element_size = layout_size_bytes(tcx, *element).ok()?;
            let element_codec =
                pointer_view_codec_operand(*element, tcx, data_types, instance_context);
            let element_codec = match element_codec {
                oomir::Operand::Constant(oomir::Constant::String(codec)) => codec,
                oomir::Operand::Constant(oomir::Constant::Null(_)) => String::new(),
                other => panic!("array-reference element codec must be constant, found {other:?}"),
            };
            format!("{ARRAY_REFERENCE_VIEW_CODEC_PREFIX}{length}\n{element_size}\n{element_codec}")
        }
        TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _)
            if is_codegen_sized(*pointee, tcx) =>
        {
            sized_pointer_codec(*pointee)?
        }
        TyKind::Adt(adt_def, args) if crate::lower1::is_non_null_lang_item(tcx, adt_def.did()) => {
            let pointee = args.iter().find_map(|arg| arg.as_type())?;
            if !is_codegen_sized(pointee, tcx) {
                return None;
            }
            sized_pointer_codec(pointee)?
        }
        TyKind::Int(IntTy::I128) => "@signed-big-integer".to_string(),
        TyKind::Uint(UintTy::U128) => "@unsigned-big-integer".to_string(),
        TyKind::Float(FloatTy::F128) => "@f128".to_string(),
        _ => return None,
    };
    Some(oomir::Operand::Constant(oomir::Constant::String(codec)))
}
