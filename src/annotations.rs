//! Java declaration annotations carried by Rust struct definitions.
use jvm_compiler_core::classfile::constant_pool::InternedConstantPool;
use ristretto_classfile::attributes::{
    Annotation, AnnotationElement, AnnotationValuePair, Attribute,
};
use rustc_hash::FxHashSet;
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::DefId;
use serde::Deserialize;
use std::sync::Arc;

pub type ClassAnnotations = Option<Arc<Vec<ClassAnnotation>>>;

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
enum Retention {
    Runtime,
    Class,
    Source,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Hash)]
pub struct ClassAnnotation {
    retention: Retention,
    annotation: AnnotationData,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Hash)]
struct AnnotationData {
    class: String,
    elements: Vec<(String, Value)>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Hash)]
#[serde(tag = "kind", content = "value")]
enum Value {
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(u32),
    Double(u64),
    Boolean(bool),
    Char(u16),
    String(String),
    Class(String),
    Enum { class: String, constant: String },
    Annotation(AnnotationData),
    Array(Vec<Value>),
}

pub(crate) fn from_def(tcx: TyCtxt<'_>, def_id: DefId) -> ClassAnnotations {
    let mut annotations = Vec::new();
    let mut types = FxHashSet::default();
    #[allow(deprecated)]
    for attr in tcx.get_all_attrs(def_id) {
        let path = attr.path();
        if path.len() != 2 || path[0].as_str() != "jvm_codegen" || path[1].as_str() != "annotation"
        {
            continue;
        }
        let annotation: ClassAnnotation = attr
            .value_str()
            .and_then(|value| serde_json::from_str(value.as_str()).ok())
            .unwrap_or_else(|| {
                tcx.dcx()
                    .span_fatal(tcx.def_span(def_id), "invalid JVM annotation metadata")
            });
        if annotation.retention == Retention::Source {
            continue;
        }
        if !types.insert(annotation.annotation.class.clone()) {
            tcx.dcx().span_fatal(
                tcx.def_span(def_id),
                "duplicate Java annotation type; use its repeatable container annotation",
            );
        }
        annotations.push(annotation);
    }
    (!annotations.is_empty()).then(|| Arc::new(annotations))
}

pub(crate) fn attributes(
    annotations: &[ClassAnnotation],
    cp: &mut InternedConstantPool,
) -> jvm_compiler_core::classfile::Result<Vec<Attribute>> {
    let mut visible = Vec::new();
    let mut invisible = Vec::new();
    for item in annotations {
        match item.retention {
            Retention::Runtime => visible.push(item.annotation.encode(cp)?),
            Retention::Class => invisible.push(item.annotation.encode(cp)?),
            Retention::Source => {}
        }
    }
    let mut attributes = Vec::new();
    if !visible.is_empty() {
        attributes.push(Attribute::RuntimeVisibleAnnotations {
            name_index: cp.add_utf8("RuntimeVisibleAnnotations")?,
            annotations: visible,
        });
    }
    if !invisible.is_empty() {
        attributes.push(Attribute::RuntimeInvisibleAnnotations {
            name_index: cp.add_utf8("RuntimeInvisibleAnnotations")?,
            annotations: invisible,
        });
    }
    Ok(attributes)
}

impl AnnotationData {
    fn encode(
        &self,
        cp: &mut InternedConstantPool,
    ) -> jvm_compiler_core::classfile::Result<Annotation> {
        Ok(Annotation {
            type_index: cp.add_utf8(&self.class)?,
            elements: self
                .elements
                .iter()
                .map(|(name, value)| {
                    Ok(AnnotationValuePair {
                        name_index: cp.add_utf8(name)?,
                        value: value.encode(cp)?,
                    })
                })
                .collect::<jvm_compiler_core::classfile::Result<_>>()?,
        })
    }
}

impl Value {
    fn encode(
        &self,
        cp: &mut InternedConstantPool,
    ) -> jvm_compiler_core::classfile::Result<AnnotationElement> {
        use AnnotationElement as E;
        Ok(match self {
            Self::Byte(v) => E::Byte {
                const_value_index: cp.add_integer((*v).into())?,
            },
            Self::Short(v) => E::Short {
                const_value_index: cp.add_integer((*v).into())?,
            },
            Self::Int(v) => E::Int {
                const_value_index: cp.add_integer(*v)?,
            },
            Self::Long(v) => E::Long {
                const_value_index: cp.add_long(*v)?,
            },
            Self::Float(v) => E::Float {
                const_value_index: cp.add_float(f32::from_bits(*v))?,
            },
            Self::Double(v) => E::Double {
                const_value_index: cp.add_double(f64::from_bits(*v))?,
            },
            Self::Boolean(v) => E::Boolean {
                const_value_index: cp.add_integer((*v).into())?,
            },
            Self::Char(v) => E::Char {
                const_value_index: cp.add_integer((*v).into())?,
            },
            Self::String(v) => E::String {
                const_value_index: cp.add_utf8(v)?,
            },
            Self::Class(v) => E::Class {
                class_info_index: cp.add_utf8(v)?,
            },
            Self::Enum { class, constant } => E::Enum {
                type_name_index: cp.add_utf8(class)?,
                const_name_index: cp.add_utf8(constant)?,
            },
            Self::Annotation(v) => E::Annotation {
                annotation: v.encode(cp)?,
            },
            Self::Array(values) => E::Array {
                values: values
                    .iter()
                    .map(|v| v.encode(cp))
                    .collect::<jvm_compiler_core::classfile::Result<_>>()?,
            },
        })
    }
}
