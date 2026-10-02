use super::*;
use crate::test_support::{body, holder, run_jar, write_classes};
use jvm_compiler_core::classfile::{names, summary};

fn summary(name: &str, private: bool) -> summary::Summary {
    summary::Summary {
        name: name.into(),
        has_main: false,
        private,
        opaque_reflection: false,
        method_demands: false,
        carrier: None,
    }
}

#[test]
fn roots_follow_descriptors_recipes_and_cycles_without_retaining_dead_cycles() {
    let mut graph = reachability::Graph::default();
    let mut edges = Vec::new();
    graph.references(b"(IJLtest/Value;[Ltest/Array;)Ltest/Return;", &mut edges);
    graph.references(
        format!(
            "{}test/Codecs#0123456789abcdef#Ltest/Decoded;",
            names::NAME_STRING
        )
        .as_bytes(),
        &mut edges,
    );
    graph.references(b"test.BinaryName", &mut edges);
    graph.record(&summary("test/Main", false), edges, false);
    let live_ids = [
        "test/Value",
        "test/Array",
        "test/Return",
        "test/Codecs",
        "test/Decoded",
        "test/BinaryName",
    ]
    .map(|name| {
        let mut edges = Vec::new();
        graph.references(b"Ltest/Cycle;", &mut edges);
        graph.record(&summary(name, true), edges, false)
    });
    let mut cycle = Vec::new();
    graph.references(b"Ltest/Value;", &mut cycle);
    let cycle = graph.record(&summary("test/Cycle", true), cycle, false);
    let mut dead = Vec::new();
    graph.references(b"Ltest/DeadB;", &mut dead);
    let dead_a = graph.record(&summary("test/DeadA", true), dead, false);
    let mut dead = Vec::new();
    graph.references(b"Ltest/DeadA;", &mut dead);
    let dead_b = graph.record(&summary("test/DeadB", true), dead, false);
    let live = graph.live(true);
    assert!(
        live_ids
            .into_iter()
            .chain([cycle])
            .all(|id| live[id as usize])
    );
    assert!(!live[dead_a as usize] && !live[dead_b as usize]);
}

#[test]
fn libraries_unknown_reflection_and_public_fragments_remain_conservative() {
    for scenario in 0..4 {
        let mut graph = reachability::Graph::default();
        let id = graph.record(&summary("test/Private", true), Vec::new(), scenario == 0);
        match scenario {
            1 => {
                let mut reflection = summary("user/Reflection", false);
                reflection.opaque_reflection = true;
                graph.record(&reflection, Vec::new(), false);
            }
            2 => {
                graph.record(&summary("test/Private", false), Vec::new(), false);
            }
            _ => {}
        }
        let demands = graph.finish(scenario != 3);
        assert!(demands.classes[id as usize]);
        assert!(demands.private_classes.is_empty());
    }
}

#[test]
fn compact_storage_names_cannot_collide_after_crate_namespace_relocation() {
    let mut graph = reachability::Graph::default();
    graph.record(
        &summary("org$crate0123456789abcdef$/rustlang/shape/S0", false),
        Vec::new(),
        false,
    );
    let demands = graph.finish(true);
    assert!(demands.share_carriers);
    assert!(!demands.compact_carriers);
}

#[test]
fn private_marker_and_reflection_are_read_from_class_metadata() {
    let mut class = ClassFile::default();
    class.this_class = class.constant_pool.add_class("test/Internal").unwrap();
    class.super_class = class.constant_pool.add_class("java/lang/Object").unwrap();
    let name_index = class
        .constant_pool
        .add_utf8(summary::PRIVATE_ATTRIBUTE)
        .unwrap();
    class.attributes.push(Attribute::Unknown {
        name_index,
        info: Vec::new(),
    });
    let owner = class.constant_pool.add_class("java/lang/Class").unwrap();
    class
        .constant_pool
        .add_method_ref(owner, "forName", "(Ljava/lang/String;)Ljava/lang/Class;")
        .unwrap();
    let bytes = serialize_class_file(&class).unwrap();
    let summary = summary::read(&bytes).unwrap();
    assert!(summary.private && summary.opaque_reflection);
}

#[test]
fn unused_static_cycles_and_their_types_disappear_but_handles_execute() {
    let mut main = holder("test/Main", false);
    let mut helpers = holder("test/mono/Mono_helpers", true);
    let mut unused = holder("test/Unused", true);
    let helper_owner = main
        .constant_pool
        .add_class("test/mono/Mono_helpers")
        .unwrap();
    let call = main
        .constant_pool
        .add_method_ref(helper_owner, "live", "()V")
        .unwrap();
    body(
        &mut main,
        "main",
        "([Ljava/lang/String;)V",
        vec![Instruction::Invokestatic(call), Instruction::Return],
    );
    let handled = helpers
        .constant_pool
        .add_method_ref(helpers.this_class, "handled", "()V")
        .unwrap();
    let handle = helpers
        .constant_pool
        .add_method_handle(ristretto_classfile::ReferenceKind::InvokeStatic, handled)
        .unwrap();
    let mh = helpers
        .constant_pool
        .add_class("java/lang/invoke/MethodHandle")
        .unwrap();
    let invoke = helpers
        .constant_pool
        .add_method_ref(mh, "invokeExact", "()V")
        .unwrap();
    body(
        &mut helpers,
        "live",
        "()V",
        vec![
            Instruction::Ldc_w(handle),
            Instruction::Invokevirtual(invoke),
            Instruction::Return,
        ],
    );
    body(&mut helpers, "handled", "()V", vec![Instruction::Return]);
    body(
        &mut helpers,
        "drop_glue$runtime",
        "()V",
        vec![Instruction::Return],
    );
    // Runtime drop lookup uses a method name without a MethodRef.
    main.constant_pool.add_string("drop_glue$runtime").unwrap();
    let a = helpers
        .constant_pool
        .add_method_ref(helpers.this_class, "deadA", "()V")
        .unwrap();
    let b = helpers
        .constant_pool
        .add_method_ref(helpers.this_class, "deadB", "()V")
        .unwrap();
    let dead_class = helpers.constant_pool.add_class("test/Unused").unwrap();
    body(
        &mut helpers,
        "deadA",
        "()V",
        vec![
            Instruction::Ldc_w(dead_class),
            Instruction::Pop,
            Instruction::Invokestatic(b),
            Instruction::Return,
        ],
    );
    body(
        &mut helpers,
        "deadB",
        "()V",
        vec![Instruction::Invokestatic(a), Instruction::Return],
    );
    body(&mut unused, "never", "()V", vec![Instruction::Return]);
    let temp = tempfile::tempdir().unwrap();
    let paths = write_classes(temp.path(), [main, helpers, unused]);
    let output = temp.path().join("output.jar");
    pipeline::link(&paths, &[], &[], &[], output.to_str().unwrap()).unwrap();
    run_jar(&output);
    let mut jar = ZipArchive::new(fs::File::open(&output).unwrap()).unwrap();
    assert!(jar.by_name("test/Unused.class").is_err());
    let mut bytes = Vec::new();
    let helper = jar
        .file_names()
        .find(|n| n.contains("/mono/Mono_"))
        .unwrap()
        .to_owned();
    jar.by_name(&helper)
        .unwrap()
        .read_to_end(&mut bytes)
        .unwrap();
    let class = class_file_from_data(&bytes).unwrap();
    let names = (0..class.methods.len())
        .map(|i| method_identity(&class, i).unwrap().0.to_string())
        .collect::<HashSet<_>>();
    assert_eq!(
        names,
        [
            "live".to_owned(),
            "handled".to_owned(),
            "drop_glue$runtime".to_owned()
        ]
        .into_iter()
        .collect()
    );
    assert!(
        !bytes
            .windows(b"test/Unused".len())
            .any(|w| w == b"test/Unused")
    );
}

#[test]
fn public_fragment_retains_all_methods_of_the_combined_owner() {
    let mut private = holder("test/mono/Mono_mixed", true);
    body(&mut private, "internal", "()V", vec![Instruction::Return]);
    let public = holder("test/mono/Mono_mixed", false);
    let mut graph = reachability::Graph::default();
    graph
        .scan(&serialize_class_file(&private).unwrap(), false)
        .unwrap();
    graph
        .scan(&serialize_class_file(&public).unwrap(), false)
        .unwrap();
    let demands = graph.finish(true);
    assert!(demands.dead_methods.is_empty());
    assert!(!demands.private_classes.contains("test/mono/Mono_mixed"));
}

#[test]
fn codec_recipe_retains_its_exact_method_family_without_other_recipes() {
    let mut main = holder("test/Main", false);
    let mut codecs = holder("test/Codecs_ab", true);
    main.constant_pool
        .add_string("test/Codecs_ab#abcdef0123456789#Ltest/Value;")
        .unwrap();
    // Recipe text inside a user literal must not retain helpers.
    main.constant_pool
        .add_string(format!(
            "{}example\ntest/Codecs_ab#ab0123456789cdef#Ltest/Value;",
            jvm_compiler_core::classfile::names::LITERAL_STRING
        ))
        .unwrap();
    for key in ["abcdef0123456789", "ab0123456789cdef"] {
        for prefix in ["e$", "d$", "a$", "w$", "b$", "s$", "c$"] {
            body(
                &mut codecs,
                &format!("{prefix}{key}"),
                "()V",
                vec![Instruction::Return],
            );
        }
    }
    let mut graph = reachability::Graph::default();
    graph
        .scan(&serialize_class_file(&main).unwrap(), false)
        .unwrap();
    let (metadata, _) = graph
        .scan(&serialize_class_file(&codecs).unwrap(), false)
        .unwrap();
    assert!(metadata.method_demands);
    let demands = graph.finish(true);
    let dead = &demands.dead_methods["test/Codecs_ab"];
    assert_eq!(dead.len(), 7);
    assert!(
        dead.iter()
            .all(|(name, _)| name.to_string().ends_with("ab0123456789cdef"))
    );
}

#[test]
fn binary_constant_resources_follow_live_code_and_dynamic_access_keeps_them() {
    use jvm_compiler_core::classfile::resources::Resource;
    let used = Resource::new(vec![1, 2, 3]);
    let unused = Resource::new(vec![4, 5, 6]);
    for dynamic in [false, true] {
        let mut graph = reachability::Graph::default();
        let a = graph.resource(&used.name);
        let b = graph.resource(&unused.name);
        let mut main = holder("test/Main", false);
        main.constant_pool
            .add_string(format!(
                "{}/{}",
                jvm_compiler_core::classfile::names::NAME_STRING,
                used.name
            ))
            .unwrap();
        main.constant_pool
            .add_string(format!(
                "{}{}",
                jvm_compiler_core::classfile::names::LITERAL_STRING,
                unused.name
            ))
            .unwrap();
        if dynamic {
            let loader = main.constant_pool.add_class("test/CustomLoader").unwrap();
            main.constant_pool
                .add_method_ref(
                    loader,
                    "getResourceAsStream",
                    "(Ljava/lang/String;)Ljava/io/InputStream;",
                )
                .unwrap();
        }
        graph
            .scan(&serialize_class_file(&main).unwrap(), false)
            .unwrap();
        let live = graph.finish(true);
        assert!(live.classes[a as usize]);
        assert_eq!(live.classes[b as usize], dynamic);
    }
}

#[test]
fn reflective_helpers_require_a_live_owner_and_follow_newly_discovered_owners() {
    for reverse in [false, true] {
        let mut main = holder("test/Main", false);
        main.constant_pool.add_class("test/mono/Mono_a").unwrap();
        let mut runtime = holder("org/rustlang/runtime/Protocol", false);
        runtime.constant_pool.add_string("hook").unwrap();
        let mut classes = vec![main, runtime];
        for (name, dependency) in [("a", Some("b")), ("b", Some("payload")), ("unused", None)] {
            let mut class = holder(&format!("test/mono/Mono_{name}"), true);
            let mut code = Vec::new();
            if let Some(dependency) = dependency {
                code.extend([
                    Instruction::Ldc_w(
                        class
                            .constant_pool
                            .add_class(format!("test/mono/Mono_{dependency}"))
                            .unwrap(),
                    ),
                    Instruction::Pop,
                ]);
            }
            code.push(Instruction::Return);
            body(&mut class, "hook", "()V", code);
            classes.push(class);
        }
        classes.push(holder("test/mono/Mono_payload", true));
        if reverse {
            classes.reverse();
        }
        let mut graph = reachability::Graph::default();
        let mut ids = HashMap::default();
        for class in classes {
            let (summary, id) = graph
                .scan(&serialize_class_file(&class).unwrap(), false)
                .unwrap();
            ids.insert(summary.name, id);
        }
        let demands = graph.finish(true);
        for name in ["a", "b", "payload"] {
            let owner = format!("test/mono/Mono_{name}");
            assert!(demands.classes[ids[&owner] as usize], "{owner}");
            assert!(!demands.dead_methods.contains_key(&owner));
        }
        assert!(!demands.classes[ids["test/mono/Mono_unused"] as usize]);
    }
}

#[test]
fn private_holders_pack_after_pruning_and_calls_handles_and_names_follow() {
    let mut main = holder("test/Main", false);
    let mut a = holder("test/mono/Mono_a", true);
    let mut b = holder("test/mono/Mono_b", true);
    let mut c = holder("test/mono/Mono_c", true);
    let a_class = main.constant_pool.add_class("test/mono/Mono_a").unwrap();
    let call = main
        .constant_pool
        .add_method_ref(a_class, "first", "()V")
        .unwrap();
    body(
        &mut main,
        "main",
        "([Ljava/lang/String;)V",
        vec![Instruction::Invokestatic(call), Instruction::Return],
    );
    let b_class = a.constant_pool.add_class("test/mono/Mono_b").unwrap();
    let call = a
        .constant_pool
        .add_method_ref(b_class, "second", "()V")
        .unwrap();
    body(
        &mut a,
        "first",
        "()V",
        vec![Instruction::Invokestatic(call), Instruction::Return],
    );
    let c_class = b.constant_pool.add_class("test/mono/Mono_c").unwrap();
    let call = b
        .constant_pool
        .add_method_ref(c_class, "third", "()V")
        .unwrap();
    let handle = b
        .constant_pool
        .add_method_handle(ristretto_classfile::ReferenceKind::InvokeStatic, call)
        .unwrap();
    let mh = b
        .constant_pool
        .add_class("java/lang/invoke/MethodHandle")
        .unwrap();
    let invoke = b
        .constant_pool
        .add_method_ref(mh, "invokeExact", "()V")
        .unwrap();
    body(
        &mut b,
        "second",
        "()V",
        vec![
            Instruction::Ldc_w(handle),
            Instruction::Invokevirtual(invoke),
            Instruction::Return,
        ],
    );
    body(&mut c, "third", "()V", vec![Instruction::Return]);
    body(&mut c, "unused", "()V", vec![Instruction::Return]);
    // Dead bodies and duplicate fragments affect input memory but not packing size.
    for class in [&mut a, &mut b, &mut c] {
        let mut dead = vec![Instruction::Nop; 40_000];
        dead.push(Instruction::Return);
        body(class, "large_dead", "()V", dead);
    }
    for name in ["test/mono/Mono_a", "test.mono.Mono_b"] {
        main.constant_pool
            .add_string(format!("{}{name}", names::NAME_STRING))
            .unwrap();
        main.constant_pool
            .add_string(format!("{}{name}", names::LITERAL_STRING))
            .unwrap();
    }
    // Runtime-generated names without crate markers are relocated as well.
    main.constant_pool.add_string("test/mono/Mono_c").unwrap();
    let temp = tempfile::tempdir().unwrap();
    let paths = write_classes(
        temp.path(),
        [main, a.clone(), a, b.clone(), b, c.clone(), c],
    );
    let output = temp.path().join("packed.jar");
    pipeline::link(&paths, &[], &[], &[], output.to_str().unwrap()).unwrap();
    run_jar(&output);
    let mut jar = ZipArchive::new(fs::File::open(&output).unwrap()).unwrap();
    let names = jar
        .file_names()
        .filter(|n| n.ends_with(".class"))
        .map(str::to_owned)
        .collect::<Vec<_>>();
    assert_eq!(names.len(), 2, "{names:?}");
    let packed = names.iter().find(|n| n.contains("/mono/Mono_")).unwrap();
    let mut bytes = Vec::new();
    jar.by_name(packed)
        .unwrap()
        .read_to_end(&mut bytes)
        .unwrap();
    let class = class_file_from_data(&bytes).unwrap();
    assert_eq!(
        class.methods.len(),
        2,
        "the first method only forwards to the second"
    );
    bytes.clear();
    jar.by_name("test/Main.class")
        .unwrap()
        .read_to_end(&mut bytes)
        .unwrap();
    let class = class_file_from_data(&bytes).unwrap();
    let strings = class
        .constant_pool
        .iter()
        .filter_map(|c| match c {
            Constant::String(i) => Some(class.constant_pool.try_get_utf8(*i).unwrap().to_string()),
            _ => None,
        })
        .collect::<Vec<_>>();
    let target = packed.trim_end_matches(".class");
    assert!(strings.contains(&target.to_owned()));
    assert!(strings.contains(&target.replace('/', ".")));
    assert!(strings.contains(&"test/mono/Mono_a".to_owned()));
    assert!(strings.contains(&"test.mono.Mono_b".to_owned()));
}

#[test]
fn external_library_references_pin_private_method_owners() {
    let mut helpers = holder("test/mono/Mono_a", true);
    body(&mut helpers, "called", "()V", vec![Instruction::Return]);
    let mut library = holder("javaapi/Entry", false);
    let owner = library.constant_pool.add_class("test/mono/Mono_a").unwrap();
    library
        .constant_pool
        .add_method_ref(owner, "called", "()V")
        .unwrap();
    let mut graph = reachability::Graph::default();
    graph
        .scan(&serialize_class_file(&helpers).unwrap(), false)
        .unwrap();
    graph
        .scan(&serialize_class_file(&library).unwrap(), true)
        .unwrap();
    let demands = graph.finish(true);
    assert!(demands.packable.is_empty());
    assert!(!demands.private_classes.contains("test/mono/Mono_a"));
}

#[test]
fn enum_static_helpers_are_demanded_without_erasing_interface_identity() {
    let mut main = holder("test/Main", false);
    let mut interface = holder("test/Choice", true);
    interface.access_flags =
        ClassAccessFlags::PUBLIC | ClassAccessFlags::INTERFACE | ClassAccessFlags::ABSTRACT;
    interface.attributes.push(Attribute::InnerClasses {
        name_index: interface.constant_pool.add_utf8("InnerClasses").unwrap(),
        classes: vec![ristretto_classfile::attributes::InnerClass {
            class_info_index: interface
                .constant_pool
                .add_class("test/ChoiceValue")
                .unwrap(),
            outer_class_info_index: interface.this_class,
            name_index: interface.constant_pool.add_utf8("ChoiceValue").unwrap(),
            access_flags: ristretto_classfile::attributes::NestedClassAccessFlags::PUBLIC
                | ristretto_classfile::attributes::NestedClassAccessFlags::STATIC,
        }],
    });
    let mut implementation = holder("test/ChoiceValue", true);
    let iface = implementation
        .constant_pool
        .add_class("test/Choice")
        .unwrap();
    implementation.interfaces.push(iface);
    let object = implementation
        .constant_pool
        .add_class("java/lang/Object")
        .unwrap();
    let init = implementation
        .constant_pool
        .add_method_ref(object, "<init>", "()V")
        .unwrap();
    body(
        &mut implementation,
        "<init>",
        "()V",
        vec![
            Instruction::Aload_0,
            Instruction::Invokespecial(init),
            Instruction::Return,
        ],
    );
    implementation.methods[0]
        .access_flags
        .remove(ristretto_classfile::MethodAccessFlags::STATIC);
    body(
        &mut interface,
        "variantIndex",
        "(Ltest/Choice;)I",
        vec![Instruction::Iconst_0, Instruction::Ireturn],
    );
    let dead = interface
        .constant_pool
        .add_class("test/DeadVariant")
        .unwrap();
    body(
        &mut interface,
        "eq",
        "()V",
        vec![
            Instruction::Ldc_w(dead),
            Instruction::Pop,
            Instruction::Return,
        ],
    );
    let implementation_class = main.constant_pool.add_class("test/ChoiceValue").unwrap();
    let init = main
        .constant_pool
        .add_method_ref(implementation_class, "<init>", "()V")
        .unwrap();
    let iface = main.constant_pool.add_class("test/Choice").unwrap();
    let call = main
        .constant_pool
        .add_interface_method_ref(iface, "variantIndex", "(Ltest/Choice;)I")
        .unwrap();
    body(
        &mut main,
        "main",
        "([Ljava/lang/String;)V",
        vec![
            Instruction::New(implementation_class),
            Instruction::Dup,
            Instruction::Invokespecial(init),
            Instruction::Invokestatic(call),
            Instruction::Pop,
            Instruction::Return,
        ],
    );
    let temp = tempfile::tempdir().unwrap();
    let paths = write_classes(
        temp.path(),
        [
            main,
            interface,
            implementation,
            holder("test/DeadVariant", true),
        ],
    );
    let output = temp.path().join("enums.jar");
    pipeline::link(&paths, &[], &[], &[], output.to_str().unwrap()).unwrap();
    run_jar(&output);
    let mut jar = ZipArchive::new(fs::File::open(&output).unwrap()).unwrap();
    assert!(jar.by_name("test/DeadVariant.class").is_err());
    let mut bytes = Vec::new();
    jar.by_name("test/Main.class")
        .unwrap()
        .read_to_end(&mut bytes)
        .unwrap();
    let main = class_file_from_data(&bytes).unwrap();
    let interface = main
        .constant_pool
        .iter()
        .find_map(|c| match c {
            Constant::InterfaceMethodRef { class_index, .. } => Some(
                main.constant_pool
                    .try_get_class(*class_index)
                    .unwrap()
                    .to_string(),
            ),
            _ => None,
        })
        .unwrap();
    let implementation = main.methods[0]
        .attributes
        .iter()
        .find_map(|a| match a {
            Attribute::Code { code, .. } => code.iter().find_map(|i| match i {
                Instruction::New(i) => {
                    Some(main.constant_pool.try_get_class(*i).unwrap().to_string())
                }
                _ => None,
            }),
            _ => None,
        })
        .unwrap();
    assert_ne!(interface, implementation);
    bytes.clear();
    jar.by_name(&format!("{interface}.class"))
        .unwrap()
        .read_to_end(&mut bytes)
        .unwrap();
    let class = class_file_from_data(&bytes).unwrap();
    assert!(class.access_flags.contains(ClassAccessFlags::INTERFACE));
    assert_eq!(class.methods.len(), 1);
    let nested = class
        .attributes
        .iter()
        .find_map(|a| match a {
            Attribute::InnerClasses { classes, .. } => Some(classes),
            _ => None,
        })
        .unwrap();
    assert_eq!(nested.len(), 1);
    assert_eq!(
        class
            .constant_pool
            .try_get_class(nested[0].class_info_index)
            .unwrap(),
        implementation.as_str()
    );
    assert_eq!(
        class
            .constant_pool
            .try_get_class(nested[0].outer_class_info_index)
            .unwrap(),
        interface.as_str()
    );
}

#[test]
fn internal_static_roots_are_pruned_without_changing_live_initialization_cycles() {
    use ristretto_classfile::{BaseType, Field, FieldAccessFlags, FieldType};
    fn static_owner(name: &str, private: bool, field: &str) -> ClassFile<'static> {
        let mut c = holder(name, private);
        c.fields.push(Field {
            access_flags: FieldAccessFlags::PUBLIC | FieldAccessFlags::STATIC,
            name_index: c.constant_pool.add_utf8(field).unwrap(),
            descriptor_index: c.constant_pool.add_utf8("I").unwrap(),
            field_type: FieldType::Base(BaseType::Int),
            attributes: vec![],
        });
        c
    }
    let mut a = static_owner("test/A$Static", true, "value");
    let own = a
        .constant_pool
        .add_field_ref(a.this_class, "value", "I")
        .unwrap();
    let other = a.constant_pool.add_class("test/B$Static").unwrap();
    let other = a.constant_pool.add_field_ref(other, "value", "I").unwrap();
    body(
        &mut a,
        "<clinit>",
        "()V",
        vec![
            Instruction::Iconst_1,
            Instruction::Putstatic(own),
            Instruction::Getstatic(other),
            Instruction::Getstatic(own),
            Instruction::Iadd,
            Instruction::Putstatic(own),
            Instruction::Return,
        ],
    );
    let mut b = static_owner("test/B$Static", true, "value");
    let own = b
        .constant_pool
        .add_field_ref(b.this_class, "value", "I")
        .unwrap();
    let other = b.constant_pool.add_class("test/A$Static").unwrap();
    let other = b.constant_pool.add_field_ref(other, "value", "I").unwrap();
    body(
        &mut b,
        "<clinit>",
        "()V",
        vec![
            Instruction::Getstatic(other),
            Instruction::Iconst_1,
            Instruction::Iadd,
            Instruction::Putstatic(own),
            Instruction::Return,
        ],
    );
    let mut dead = static_owner("test/Dead$Static", true, "dead_initializer_marker");
    let unused = dead.constant_pool.add_class("test/UnusedPayload").unwrap();
    body(
        &mut dead,
        "<clinit>",
        "()V",
        vec![
            Instruction::Ldc_w(unused),
            Instruction::Pop,
            Instruction::Return,
        ],
    );
    let mut payload = holder("test/UnusedPayload", true);
    body(
        &mut payload,
        "unused_payload_marker",
        "()V",
        vec![Instruction::Return],
    );
    let public = static_owner("test/Exported$Static", false, "exported_value");
    let mut main = holder("test/Main", false);
    let system = main.constant_pool.add_class("java/lang/System").unwrap();
    let out = main
        .constant_pool
        .add_field_ref(system, "out", "Ljava/io/PrintStream;")
        .unwrap();
    let stream = main.constant_pool.add_class("java/io/PrintStream").unwrap();
    let print = main
        .constant_pool
        .add_method_ref(stream, "println", "(I)V")
        .unwrap();
    let mut code = Vec::new();
    for owner in ["test/A$Static", "test/B$Static"] {
        let owner = main.constant_pool.add_class(owner).unwrap();
        let field = main
            .constant_pool
            .add_field_ref(owner, "value", "I")
            .unwrap();
        code.extend([
            Instruction::Getstatic(out),
            Instruction::Getstatic(field),
            Instruction::Invokevirtual(print),
        ]);
    }
    code.push(Instruction::Return);
    body(&mut main, "main", "([Ljava/lang/String;)V", code);
    let temp = tempfile::tempdir().unwrap();
    let paths = write_classes(temp.path(), [main, a, b, dead, payload, public]);
    let jar = temp.path().join("statics.jar");
    pipeline::link(&paths, &[], &[], &[], jar.to_str().unwrap()).unwrap();
    let mut archive = ZipArchive::new(fs::File::open(&jar).unwrap()).unwrap();
    assert!(
        archive
            .file_names()
            .any(|name| name == "test/Exported$Static.class")
    );
    for index in 0..archive.len() {
        let mut bytes = Vec::new();
        archive
            .by_index(index)
            .unwrap()
            .read_to_end(&mut bytes)
            .unwrap();
        for marker in [
            b"dead_initializer_marker".as_slice(),
            b"unused_payload_marker".as_slice(),
        ] {
            assert!(!bytes.windows(marker.len()).any(|window| window == marker));
        }
    }
    let run = run_jar(&jar);
    assert_eq!(
        String::from_utf8(run.stdout).unwrap().replace("\r\n", "\n"),
        "3\n2\n"
    );
}
