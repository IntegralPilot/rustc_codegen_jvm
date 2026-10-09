use crate::test_support::{body, holder, run_jar, write_classes};
use crate::*;
use jvm_compiler_core::classfile::{names, summary};

#[test]
fn shared_callable_interfaces_keep_the_runtime_protocol_name() {
    let mut main = holder("test/Main", false);
    let mut classes = Vec::new();
    let mut code = Vec::new();
    for suffix in ["first", "second"] {
        let name = format!("org/rustlang/runtime/FnPtr_{suffix}");
        let mut class = holder(&name, true);
        class.access_flags =
            ClassAccessFlags::PUBLIC | ClassAccessFlags::INTERFACE | ClassAccessFlags::ABSTRACT;
        class.attributes.push(Attribute::Unknown {
            name_index: class
                .constant_pool
                .add_utf8(summary::CARRIER_ATTRIBUTE)
                .unwrap(),
            info: b"carrier-v2;function-v1;param;0:I;return;0:I;".to_vec(),
        });
        class.methods.push(ristretto_classfile::Method {
            access_flags: ristretto_classfile::MethodAccessFlags::PUBLIC
                | ristretto_classfile::MethodAccessFlags::ABSTRACT,
            name_index: class.constant_pool.add_utf8("call").unwrap(),
            descriptor_index: class.constant_pool.add_utf8("(I)I").unwrap(),
            attributes: Vec::new(),
        });
        code.extend([
            Instruction::Ldc_w(main.constant_pool.add_class(&name).unwrap()),
            Instruction::Pop,
        ]);
        classes.push(class);
    }
    code.push(Instruction::Return);
    body(&mut main, "main", "([Ljava/lang/String;)V", code);
    classes.push(main);
    let temp = tempfile::tempdir().unwrap();
    let paths = write_classes(temp.path(), classes);
    let output = temp.path().join("shared.jar");
    pipeline::link(&paths, &[], &[], &[], output.to_str().unwrap()).unwrap();
    run_jar(&output);
    let mut jar = ZipArchive::new(fs::File::open(&output).unwrap()).unwrap();
    assert_eq!(
        jar.file_names()
            .filter(|name| name.starts_with("org/rustlang/runtime/FnPtr_"))
            .count(),
        1
    );
    let mut bytes = Vec::new();
    jar.by_name("test/Main.class")
        .unwrap()
        .read_to_end(&mut bytes)
        .unwrap();
    let main = class_file_from_data(&bytes).unwrap();
    let references = main
        .constant_pool
        .iter()
        .filter_map(|constant| {
            if let Constant::Class(index) = constant {
                let name = main.constant_pool.try_get_utf8(*index).unwrap().to_string();
                name.starts_with("org/rustlang/runtime/FnPtr_")
                    .then_some(name)
            } else {
                None
            }
        })
        .collect::<HashSet<_>>();
    assert_eq!(references.len(), 1);
}

#[test]
fn matching_private_carriers_share_identity_and_relocate_only_symbolic_names() {
    for barrier in [
        "none",
        "dead-eq",
        "fragment",
        "library",
        "reflection",
        "collision",
    ] {
        let mut main = holder("test/Main", false);
        let mut a = holder("test/CarrierA", true);
        let mut b = holder("test/CarrierB", true);
        for carrier in [&mut a, &mut b] {
            carrier.attributes.push(Attribute::Unknown {
                name_index: carrier
                    .constant_pool
                    .add_utf8(summary::CARRIER_ATTRIBUTE)
                    .unwrap(),
                info: b"test-empty-carrier-v1".to_vec(),
            });
            carrier.fields.push(ristretto_classfile::Field {
                access_flags: ristretto_classfile::FieldAccessFlags::PUBLIC,
                name_index: carrier.constant_pool.add_utf8("payload").unwrap(),
                descriptor_index: carrier.constant_pool.add_utf8("Ltest/FieldType;").unwrap(),
                field_type: ristretto_classfile::FieldType::Object("test/FieldType".into()),
                attributes: Vec::new(),
            });
            let init = carrier
                .constant_pool
                .add_method_ref(carrier.super_class, "<init>", "()V")
                .unwrap();
            body(
                carrier,
                "<init>",
                "()V",
                vec![
                    Instruction::Aload_0,
                    Instruction::Invokespecial(init),
                    Instruction::Return,
                ],
            );
            carrier.methods[0].access_flags = ristretto_classfile::MethodAccessFlags::PUBLIC;
            let name = carrier
                .constant_pool
                .try_get_class(carrier.this_class)
                .unwrap()
                .to_string();
            body(
                carrier,
                "eq",
                &format!("(L{name};)Z"),
                vec![Instruction::Iconst_1, Instruction::Ireturn],
            );
            carrier.methods[1].access_flags = ristretto_classfile::MethodAccessFlags::PUBLIC
                | ristretto_classfile::MethodAccessFlags::FINAL;
            if let Attribute::Code { max_locals, .. } = &mut carrier.methods[1].attributes[0] {
                *max_locals = 2;
            }
        }
        let mut code = Vec::new();
        for name in ["test/CarrierA", "test/CarrierB"] {
            let class = main.constant_pool.add_class(name).unwrap();
            let init = main
                .constant_pool
                .add_method_ref(class, "<init>", "()V")
                .unwrap();
            code.extend([
                Instruction::New(class),
                Instruction::Dup,
                Instruction::Invokespecial(init),
            ]);
            if barrier == "none" && name == "test/CarrierB" {
                let eq = main
                    .constant_pool
                    .add_method_ref(class, "eq", "(Ltest/CarrierB;)Z")
                    .unwrap();
                code.extend([Instruction::Dup, Instruction::Invokevirtual(eq)]);
            }
            code.push(Instruction::Pop);
        }
        main.constant_pool.add_string("test/CarrierB").unwrap();
        main.constant_pool
            .add_string(format!("{}@zero-sized:test/CarrierB", names::NAME_STRING))
            .unwrap();
        if barrier == "reflection" {
            let class = main.constant_pool.add_class("java/lang/Class").unwrap();
            main.constant_pool
                .add_method_ref(class, "forName", "(Ljava/lang/String;)Ljava/lang/Class;")
                .unwrap();
        }
        code.push(Instruction::Return);
        body(&mut main, "main", "([Ljava/lang/String;)V", code);
        let mut classes = vec![main, a, b, holder("test/FieldType", true)];
        if barrier == "fragment" {
            classes.push(holder("test/CarrierB", true));
        }
        let temp = tempfile::tempdir().unwrap();
        let paths = write_classes(temp.path(), classes);
        let mut libraries = Vec::new();
        if matches!(barrier, "library" | "collision") {
            let mut external = holder(
                if barrier == "collision" {
                    "org/rustlang/shape/S0"
                } else {
                    "external/Api"
                },
                false,
            );
            // A descriptor-only reference must pin the carrier too.
            body(
                &mut external,
                "accept",
                if barrier == "collision" {
                    "()V"
                } else {
                    "(Ltest/CarrierB;)V"
                },
                vec![Instruction::Return],
            );
            let path = temp.path().join("api.jar");
            let mut jar = zip::ZipWriter::new(fs::File::create(&path).unwrap());
            jar.start_file(
                format!("{}.class", external.class_name().unwrap()),
                zip::write::SimpleFileOptions::default(),
            )
            .unwrap();
            use std::io::Write;
            jar.write_all(&serialize_class_file(&external).unwrap())
                .unwrap();
            jar.finish().unwrap();
            libraries.push(path.to_string_lossy().into_owned());
        }
        let output = temp.path().join("shared.jar");
        pipeline::link(&paths, &[], &[], &libraries, output.to_str().unwrap()).unwrap();
        run_jar(&output);
        let mut jar = ZipArchive::new(fs::File::open(&output).unwrap()).unwrap();
        let mut bytes = Vec::new();
        jar.by_name("test/Main.class")
            .unwrap()
            .read_to_end(&mut bytes)
            .unwrap();
        let class = class_file_from_data(&bytes).unwrap();
        let allocations = class.methods[0]
            .attributes
            .iter()
            .find_map(|a| match a {
                Attribute::Code { code, .. } => Some(
                    code.iter()
                        .filter_map(|i| match i {
                            Instruction::New(i) => {
                                Some(class.constant_pool.try_get_class(*i).unwrap().to_string())
                            }
                            _ => None,
                        })
                        .collect::<Vec<_>>(),
                ),
                _ => None,
            })
            .unwrap();
        assert_eq!(allocations.len(), 2);
        assert_eq!(
            allocations[0] == allocations[1],
            matches!(barrier, "none" | "dead-eq" | "collision"),
            "{barrier}"
        );
        if barrier == "library" {
            assert_eq!(
                allocations[1], "test/CarrierB",
                "external descriptor must pin identity"
            );
        }
        if barrier == "reflection" {
            assert_eq!(allocations, ["test/CarrierA", "test/CarrierB"]);
        }
        let mut carrier_bytes = Vec::new();
        jar.by_name(&format!("{}.class", allocations[0]))
            .unwrap()
            .read_to_end(&mut carrier_bytes)
            .unwrap();
        let carrier = class_file_from_data(&carrier_bytes).unwrap();
        let dependency = carrier
            .constant_pool
            .try_get_utf8(carrier.fields[0].descriptor_index)
            .unwrap()
            .to_string();
        let dependency = dependency
            .strip_prefix('L')
            .unwrap()
            .strip_suffix(';')
            .unwrap();
        assert!(
            jar.by_name(&format!("{dependency}.class")).is_ok(),
            "field descriptor lost its dependency"
        );
        assert_eq!(
            carrier.methods.iter().any(|m| carrier
                .constant_pool
                .try_get_utf8(m.name_index)
                .unwrap()
                == "eq"),
            matches!(barrier, "none" | "reflection"),
            "{barrier}"
        );
        let strings = class
            .constant_pool
            .iter()
            .filter_map(|constant| match constant {
                Constant::String(index) => Some(
                    class
                        .constant_pool
                        .try_get_utf8(*index)
                        .unwrap()
                        .to_string(),
                ),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert!(strings.contains(&"test/CarrierB".to_owned()));
        let expected = format!("@zero-sized:{}", allocations[1]);
        assert!(strings.contains(&expected), "{strings:?}");
    }
}
