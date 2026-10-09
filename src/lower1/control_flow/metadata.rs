//! Pointer metadata has the same semantics before and after MIR inlining.
use super::*;

pub(super) fn emit_pointer_metadata<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    pointer_ty: Ty<'tcx>,
    pointer: oomir::Operand,
    output_type: oomir::Type,
    dest: String,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    // Inlined intrinsics can contain generic parameters. Metadata classification requires the
    // instantiated pointee.
    let pointer_ty = data_types.normalize(tcx, pointer_ty, instance);
    let (TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _)) = pointer_ty.kind() else {
        panic!("metadata source must be a pointer: {pointer_ty:?}");
    };
    if !output_type.has_jvm_value() {
        return oomir::Operand::Constant(oomir::Constant::Unit);
    }
    let tail = tcx.struct_tail_for_codegen(*pointee, TypingEnv::fully_monomorphized());
    if tail.is_slice() || tail.is_str() {
        match pointer.get_type().expect("metadata source is typed") {
            oomir::Type::Slice(_) | oomir::Type::Str => {
                instructions.push(oomir::Instruction::GetField {
                    dest: dest.clone(),
                    object: pointer,
                    field_name: "rustLength".into(),
                    field_ty: oomir::Type::U64,
                    owner_class: oomir::SLICE_VIEW_CLASS.into(),
                });
            }
            ty @ oomir::Type::Pointer(_) => {
                instructions.push(oomir::Instruction::InvokeVirtual {
                    dest: Some(dest.clone()),
                    class_name: oomir::POINTER_CLASS.into(),
                    method_name: "metadata".into(),
                    method_ty: oomir::Signature {
                        params: vec![("self".into(), ty)],
                        ret: Box::new(oomir::Type::U64),
                        is_static: false,
                    },
                    args: vec![],
                    operand: pointer,
                });
            }
            other => panic!("unsupported slice metadata carrier: {other:?}"),
        }
    } else if matches!(tail.kind(), TyKind::Dynamic(..)) {
        emit_trait_object_metadata(
            pointer,
            crate::lower1::types::stable_type_identity(tcx, tail),
            &output_type,
            dest.clone(),
            &dest,
            data_types,
            instructions,
        );
    } else {
        panic!("non-unit metadata for {pointer_ty:?}: {output_type:?}");
    }
    oomir::Operand::Variable {
        name: dest,
        ty: output_type,
    }
}

pub(super) fn emit_trait_object_metadata(
    pointer: oomir::Operand,
    identity: String,
    output_type: &oomir::Type,
    dest: String,
    temp_prefix: &str,
    data_types: &crate::lower1::context::Definitions<'_>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    // FakeForPtrMetadata can expose an interface carrier. The runtime marker API requires a raw
    // pointer.
    let pointer = if matches!(pointer.get_type(), Some(oomir::Type::Pointer(_))) {
        pointer
    } else {
        let pointer_ty = oomir::Type::pointer(pointer.get_type().expect("trait source is typed"));
        crate::lower1::value_repr::emit_trait_object_reference_pointer(
            pointer,
            &pointer_ty,
            temp_prefix,
            instructions,
        )
    };
    let mut wrappers = Vec::new();
    let mut marker_type = output_type.clone();
    while let oomir::Type::Class(class) = &marker_type {
        assert!(wrappers.len() < 16, "recursive trait metadata carrier");
        let fields = match data_types.get(class) {
            Some(oomir::DataType::Class { fields, .. }) => fields,
            other => panic!("trait metadata class is unavailable: {other:?}"),
        };
        let [(_, inner)] = fields.as_slice() else {
            panic!("trait metadata wrapper must have one payload")
        };
        wrappers.push((class.clone(), inner.clone()));
        marker_type = inner.clone();
    }
    assert!(
        matches!(marker_type, oomir::Type::Pointer(_)),
        "trait metadata needs an address"
    );
    let marker_name = format!("{temp_prefix}_vtable_marker");
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: Some(marker_name.clone()),
        class_name: oomir::POINTER_CLASS.to_string(),
        method_name: "traitMetadataMarker".to_string(),
        method_ty: oomir::Signature {
            params: vec![
                (
                    "pointer".to_string(),
                    pointer.get_type().expect("trait source is typed"),
                ),
                ("identity".to_string(), oomir::Type::java_string()),
            ],
            ret: Box::new(marker_type.clone()),
            is_static: true,
        },
        args: vec![
            pointer,
            oomir::Operand::Constant(oomir::Constant::String(identity)),
        ],
    });
    let mut value = oomir::Operand::Variable {
        name: marker_name,
        ty: marker_type,
    };
    for (index, (class_name, field_ty)) in wrappers.into_iter().rev().enumerate() {
        let name = format!("{temp_prefix}_metadata_{index}");
        instructions.push(oomir::Instruction::ConstructObject {
            dest: name.clone(),
            class_name: class_name.clone(),
            args: vec![(value, field_ty)],
        });
        value = oomir::Operand::Variable {
            name,
            ty: oomir::Type::Class(class_name),
        };
    }
    instructions.push(oomir::Instruction::Move { dest, src: value });
}

/// Reconstruct both pointer words; the data address alone cannot recover a vtable.
pub(super) fn emit_trait_pointer_from_parts(
    data: oomir::Operand,
    metadata: oomir::Operand,
    pointer_type: oomir::Type,
    dest: Option<String>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    instructions.push(oomir::Instruction::InvokeStatic {
        dest,
        class_name: oomir::POINTER_CLASS.into(),
        method_name: "fromRawTraitParts".into(),
        method_ty: oomir::Signature {
            params: vec![
                (
                    "pointer".into(),
                    data.get_type().expect("raw pointer data is typed"),
                ),
                (
                    "metadata".into(),
                    oomir::Type::Class("java/lang/Object".into()),
                ),
            ],
            ret: Box::new(pointer_type),
            is_static: true,
        },
        args: vec![data, metadata],
    });
}

/// RawPtr MIR and uninlined from_raw_parts calls share reconstruction rules.
pub(super) fn emit_raw_pointer_from_parts<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    data_types: &mut Definitions<'tcx>,
    pointee: Ty<'tcx>,
    pointer_type: oomir::Type,
    data: oomir::Operand,
    metadata: oomir::Operand,
    dest: Option<String>,
    instructions: &mut Vec<oomir::Instruction>,
) {
    let pointee = data_types.normalize(tcx, pointee, instance);
    if matches!(pointee.kind(), TyKind::Dynamic(..)) {
        emit_trait_pointer_from_parts(data, metadata, pointer_type, dest, instructions);
        return;
    }
    let tail = tcx.struct_tail_for_codegen(pointee, TypingEnv::fully_monomorphized());
    let size = oomir::Operand::Constant(oomir::Constant::U64(
        crate::lower1::types::layout_size_bytes(tcx, pointee)
            .expect("raw pointer pointee has a static prefix layout") as u64,
    ));
    let codec =
        crate::lower1::types::pointer_view_codec_operand(pointee, tcx, data_types, instance);
    let (method, metadata_type) = if tail.is_slice() || tail.is_str() {
        ("retypeWithMetadata", oomir::Type::U64)
    } else if matches!(tail.kind(), TyKind::Dynamic(..)) {
        (
            "fromRawStructTraitParts",
            oomir::Type::Class("java/lang/Object".into()),
        )
    } else {
        instructions.push(oomir::Instruction::AddressRetype {
            dest,
            source: data,
            layout: Box::new(oomir::AddressLayout {
                pointer_type,
                size,
                codec,
            }),
        });
        return;
    };
    instructions.push(oomir::Instruction::InvokeStatic {
        dest,
        class_name: oomir::POINTER_CLASS.into(),
        method_name: method.into(),
        method_ty: oomir::Signature {
            params: vec![
                (
                    "pointer".into(),
                    data.get_type().expect("raw pointer data is typed"),
                ),
                ("prefix_size".into(), oomir::Type::U64),
                ("codec".into(), oomir::Type::java_string()),
                ("metadata".into(), metadata_type),
            ],
            ret: Box::new(pointer_type),
            is_static: true,
        },
        args: vec![data, size, codec, metadata],
    });
}
