use super::*;
use ristretto_classfile::attributes::AnnotationValuePair;

fn fragment(padding: bool, value: Option<&str>) -> Vec<u8> {
    let mut class = ClassFile::default();
    let cp = &mut class.constant_pool;
    if padding {
        cp.add_utf8("unrelated constant").unwrap();
    }
    class.this_class = cp.add_class("example/Item").unwrap();
    class.super_class = cp.add_class("java/lang/Object").unwrap();
    if let Some(value) = value {
        let annotation = Annotation {
            type_index: cp.add_utf8("Lexample/Tag;").unwrap(),
            elements: vec![AnnotationValuePair {
                name_index: cp.add_utf8("value").unwrap(),
                value: AnnotationElement::Array {
                    values: vec![AnnotationElement::Annotation {
                        annotation: Annotation {
                            type_index: cp.add_utf8("Lexample/Nested;").unwrap(),
                            elements: vec![AnnotationValuePair {
                                name_index: cp.add_utf8("text").unwrap(),
                                value: AnnotationElement::String {
                                    const_value_index: cp.add_utf8(value).unwrap(),
                                },
                            }],
                        },
                    }],
                },
            }],
        };
        class.attributes.push(Attribute::RuntimeVisibleAnnotations {
            name_index: cp.add_utf8("RuntimeVisibleAnnotations").unwrap(),
            annotations: vec![annotation],
        });
        class
            .attributes
            .push(Attribute::RuntimeInvisibleAnnotations {
                name_index: cp.add_utf8("RuntimeInvisibleAnnotations").unwrap(),
                annotations: vec![Annotation {
                    type_index: cp.add_utf8("Lexample/Hidden;").unwrap(),
                    elements: vec![],
                }],
            });
    }
    serialize_class_file(&class).unwrap()
}

#[test]
fn preserves_metadata_only_fragments_and_deduplicates_remapped_annotations() {
    let first = fragment(false, None);
    let second = fragment(true, Some("λ\0😀"));
    let merged = merge_class_data(&first, &second).unwrap();
    let merged = merge_class_data(&merged, &fragment(false, Some("λ\0😀"))).unwrap();
    let class = class_file_from_data(&merged).unwrap();
    let annotations = class
        .attributes
        .iter()
        .filter_map(entries)
        .collect::<Vec<_>>();
    assert_eq!(annotations.len(), 2);
    assert_eq!(annotations[0].1.len(), 1);
    let nested = &annotations[0].1[0].elements[0].value;
    let AnnotationElement::Array { values } = nested else {
        panic!()
    };
    let AnnotationElement::Annotation { annotation } = &values[0] else {
        panic!()
    };
    let AnnotationElement::String { const_value_index } = annotation.elements[0].value else {
        panic!()
    };
    assert_eq!(
        class.constant_pool.try_get_utf8(const_value_index).unwrap(),
        "λ\0😀"
    );
    assert!(merge_class_data(&merged, &fragment(false, Some("conflict"))).is_err());
}

#[test]
fn namespace_relocation_preserves_annotation_literals_that_share_name_constants() {
    use jvm_compiler_core::classfile::{constant_pool::InternedConstantPool, names::NAME_STRING};
    let owner = "app$crate3333333333333333$/Node";
    let descriptor = format!("L{owner};");
    let tagged = format!("{NAME_STRING}{owner}");
    let mut cp = InternedConstantPool::default();
    let this_class = cp.add_class(owner).unwrap();
    let super_class = cp.add_class("java/lang/Object").unwrap();
    let reflection = cp.add_name_string(owner).unwrap();
    let descriptor_index = cp.add_utf8(&descriptor).unwrap();
    let attribute = Attribute::RuntimeVisibleAnnotations {
        name_index: cp.add_utf8("RuntimeVisibleAnnotations").unwrap(),
        annotations: vec![Annotation {
            type_index: cp.add_utf8("Lexample/Tag;").unwrap(),
            elements: vec![
                AnnotationValuePair {
                    name_index: cp.add_utf8("type").unwrap(),
                    value: AnnotationElement::Class {
                        class_info_index: descriptor_index,
                    },
                },
                AnnotationValuePair {
                    name_index: cp.add_utf8("text").unwrap(),
                    value: AnnotationElement::String {
                        const_value_index: descriptor_index,
                    },
                },
                AnnotationValuePair {
                    name_index: cp.add_utf8("tagged").unwrap(),
                    value: AnnotationElement::String {
                        const_value_index: cp.add_utf8(&tagged).unwrap(),
                    },
                },
            ],
        }],
    };
    let class = ClassFile {
        constant_pool: cp.into_inner(),
        this_class,
        super_class,
        attributes: vec![attribute],
        ..Default::default()
    };
    let relocated = namespaces::Namespaces::collect([owner].into_iter())
        .class(ClassInfo {
            jar_entry_name: format!("{owner}.class"),
            data: serialize_class_file(&class).unwrap(),
        })
        .unwrap();
    let class = class_file_from_data(&relocated.data).unwrap();
    let pairs = &entries(&class.attributes[0]).unwrap().1[0].elements;
    for (pair, expected) in pairs.iter().zip(["Lapp/Node;", &descriptor, &tagged]) {
        let index = match pair.value {
            AnnotationElement::Class { class_info_index } => class_info_index,
            AnnotationElement::String { const_value_index } => const_value_index,
            _ => panic!(),
        };
        assert_eq!(class.constant_pool.try_get_utf8(index).unwrap(), expected);
    }
    assert_eq!(
        class.constant_pool.try_get_string(reflection).unwrap(),
        "app/Node"
    );
}
