//! Exact Rust enum layouts belong to codecs, not to shareable variant storage.
use super::*;
use crate::lower1::context::Definitions;

pub(super) struct EnumCodec {
    pub owner: String,
    pub writer: String,
    pub reader: String,
}

pub(super) fn ensure_enum_union_codec<'tcx>(
    adt_def: &AdtDef<'tcx>,
    substs: GenericArgsRef<'tcx>,
    enum_ty: Ty<'tcx>,
    enum_class: &str,
    tcx: TyCtxt<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instance_context: rustc_middle::ty::Instance<'tcx>,
) -> Result<EnumCodec, String> {
    let key = crate::stable_hash::short_hash(enum_class, 16);
    let codec = EnumCodec {
        owner: pointer_codec_class_name(enum_ty, tcx, &format!("Codecs_{}", &key[..2])),
        writer: format!("ew${key}"),
        reader: format!("er${key}"),
    };
    if !data_types.contains_key(enum_class) {
        ensure_enum_data_types(
            adt_def,
            substs,
            enum_class,
            tcx,
            data_types,
            instance_context,
        );
    }
    if !data_types.contains_key(&codec.owner) {
        data_types.insert(
            codec.owner.clone(),
            DataType::Class {
                fields: vec![],
                kind: oomir::ClassKind::Static,
                is_abstract: false,
                methods: HashMap::default(),
                super_class: None,
                interfaces: vec![],
            },
        );
    }
    let Some(DataType::Class { methods, .. }) = data_types.get_mut(&codec.owner) else {
        return Err(format!("enum codec holder {} is not a class", codec.owner));
    };
    // The placeholder prevents recursion through payload codecs. Emission requires the complete
    // body.
    if methods.contains_key(&codec.writer) {
        return Ok(codec);
    }
    methods.insert(
        codec.writer.clone(),
        DataTypeMethod::Abstract(enum_union_write_signature(enum_class)),
    );
    let generated = (|| {
        let layout = tcx
            .layout_of(TypingEnv::fully_monomorphized().as_query_input(enum_ty))
            .map_err(|err| format!("could not get layout for {enum_ty:?}: {err:?}"))?;
        let tag = union_enum_tag(&layout, tcx)?;
        let mut writers = Vec::new();
        for index in 0..adt_def.variants().len() {
            let (receiver, function) = enum_variant_union_writer(
                format!("ev${key}${index}"),
                adt_def,
                substs,
                enum_class,
                &layout,
                &tag,
                VariantIdx::from_usize(index),
                tcx,
                data_types,
                instance_context,
            )?;
            writers.push((receiver, function));
        }
        let reader = enum_union_reader(
            codec.reader.clone(),
            adt_def,
            substs,
            enum_class,
            &layout,
            &tag,
            tcx,
            data_types,
            instance_context,
        )?;
        let tagged = matches!(data_types.get(enum_class), Some(DataType::Interface { methods, .. }) if methods.contains_key(oomir::ENUM_TAG_METHOD));
        let dispatcher = dispatcher(
            &codec,
            enum_class,
            &writers,
            tagged.then(|| {
                adt_def
                    .discriminants(tcx)
                    .map(|(_, d)| d.val as i64)
                    .collect::<Vec<_>>()
            }),
        );
        Ok::<_, String>((writers, reader, dispatcher))
    })();
    let Some(DataType::Class { methods, .. }) = data_types.get_mut(&codec.owner) else {
        unreachable!()
    };
    match generated {
        Ok((writers, reader, dispatcher)) => {
            for function in writers
                .into_iter()
                .map(|(_, f)| f)
                .chain([reader, dispatcher])
            {
                methods.insert(function.name.clone(), DataTypeMethod::Function(function));
            }
            Ok(codec)
        }
        Err(error) => {
            methods.remove(&codec.writer);
            Err(error)
        }
    }
}

fn dispatcher(
    codec: &EnumCodec,
    enum_class: &str,
    writers: &[(String, oomir::Function)],
    tags: Option<Vec<i64>>,
) -> oomir::Function {
    use oomir::{Constant as C, Instruction as I, Type as T};
    let receiver = operand_var("_1", T::Class(enum_class.into()));
    let mut blocks = HashMap::default();
    let (query, discriminator) = if tags.is_some() {
        (
            I::InvokeVirtual {
                dest: Some("_tag".into()),
                class_name: enum_class.into(),
                method_name: oomir::ENUM_TAG_METHOD.into(),
                method_ty: oomir::Signature {
                    params: vec![("self".into(), T::Class(enum_class.into()))],
                    ret: Box::new(T::I64),
                    is_static: false,
                },
                args: vec![],
                operand: receiver.clone(),
            },
            T::I64,
        )
    } else {
        (
            I::InvokeStatic {
                dest: Some("_tag".into()),
                class_name: enum_class.into(),
                method_name: "variantIndex".into(),
                method_ty: oomir::Signature {
                    params: vec![("value".into(), T::Class(enum_class.into()))],
                    ret: Box::new(T::I32),
                    is_static: true,
                },
                args: vec![receiver.clone()],
            },
            T::I32,
        )
    };
    let mut targets = Vec::new();
    for (i, (class, writer)) in writers.iter().enumerate() {
        let label = format!("variant_{i}");
        targets.push((
            tags.as_ref().map_or(C::I32(i as i32), |t| C::I64(t[i])),
            label.clone(),
        ));
        blocks.insert(
            label.clone(),
            oomir::BasicBlock {
                label,
                instructions: vec![
                    I::Cast {
                        dest: "_variant".into(),
                        op: receiver.clone(),
                        ty: T::Class(class.clone()),
                    },
                    I::InvokeStatic {
                        dest: None,
                        class_name: codec.owner.clone(),
                        method_name: writer.name.clone(),
                        method_ty: writer.signature.clone(),
                        args: vec![
                            operand_var("_variant", T::Class(class.clone())),
                            operand_var("_2", byte_array_type()),
                            operand_var("_3", object_array_type()),
                            operand_var("_4", T::I32),
                        ],
                    },
                    I::Return { operand: None },
                ],
            },
        );
    }
    blocks.insert(
        "entry".into(),
        oomir::BasicBlock {
            label: "entry".into(),
            instructions: vec![
                query,
                I::Switch {
                    discr: operand_var("_tag", discriminator),
                    targets,
                    otherwise: "invalid".into(),
                },
            ],
        },
    );
    blocks.insert(
        "invalid".into(),
        oomir::BasicBlock {
            label: "invalid".into(),
            instructions: vec![I::ThrowNewWithMessage {
                exception_class: "java/lang/IllegalArgumentException".into(),
                message: "invalid enum variant while writing union storage".into(),
            }],
        },
    );
    oomir::Function {
        name: codec.writer.clone(),
        owner_class: None,
        debug_variables: vec![],
        signature: enum_union_write_signature(enum_class),
        body: oomir::CodeBlock {
            entry: "entry".into(),
            basic_blocks: blocks,
        }
        .into(),
    }
}
