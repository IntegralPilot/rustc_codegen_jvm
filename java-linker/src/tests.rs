use super::{
    CLASS_BUNDLE_MAGIC, ClassInfo, LinkerMetrics, class_file_from_data, instruction_byte_offsets,
    interface_name, jar_output_path, merge_class_data, merge_duplicate_classes, merge_input_jars,
    method_identity, msvc_output_path, parse_response_lines, remap_local_variable_ranges,
    write_final_jar,
};
use ristretto_classfile::attributes::{
    Attribute, BootstrapMethod, Instruction, LocalVariableTable,
};
use ristretto_classfile::{
    ClassAccessFlags, ClassFile, Constant, ConstantPool, Method, MethodAccessFlags, ReferenceKind,
    Version,
};
use std::{collections::HashSet, fs::File, io::Read, io::Write};
use tempfile::tempdir;
use zip::{ZipArchive, write::SimpleFileOptions, write::ZipWriter};

#[test]
fn linker_metrics_measure_fragment_amplification() {
    let classes = vec![
        ClassInfo {
            jar_entry_name: "A.class".to_string(),
            data: vec![1, 2],
        },
        ClassInfo {
            jar_entry_name: "A.class".to_string(),
            data: vec![3, 4, 5],
        },
        ClassInfo {
            jar_entry_name: "B.class".to_string(),
            data: vec![6],
        },
    ];
    let mut metrics = LinkerMetrics::from_inputs(&classes, &[]);
    metrics.record_fragment_groups(&[
        vec![
            ClassInfo {
                jar_entry_name: "A.class".to_string(),
                data: vec![1, 2],
            },
            ClassInfo {
                jar_entry_name: "A.class".to_string(),
                data: vec![3, 4, 5],
            },
        ],
        vec![ClassInfo {
            jar_entry_name: "B.class".to_string(),
            data: vec![6],
        }],
    ]);
    assert_eq!(metrics.input_fragments, 3);
    assert_eq!(metrics.input_fragment_bytes, 6);
    assert_eq!(metrics.unique_class_names, 2);
    assert_eq!(metrics.duplicate_class_names, 1);
    assert_eq!(metrics.duplicate_fragments, 1);
    assert_eq!(metrics.top_duplicate_classes[0].class, "A.class");
    assert_eq!(metrics.top_duplicate_classes[0].fragments, 2);
}

fn abstract_class_with_method(method_name: &str) -> Vec<u8> {
    abstract_class_with_method_and_interface(method_name, None)
}

fn interface_with_method(method_name: &str) -> Vec<u8> {
    let bytes = abstract_class_with_method(method_name);
    let mut class_file = class_file_from_data(&bytes).unwrap();
    class_file.access_flags =
        ClassAccessFlags::PUBLIC | ClassAccessFlags::ABSTRACT | ClassAccessFlags::INTERFACE;
    let mut bytes = Vec::new();
    class_file.to_bytes(&mut bytes).unwrap();
    bytes
}

fn holder_class_with_method_and_constructor(method_name: &str) -> Vec<u8> {
    let bytes = abstract_class_with_method(method_name);
    let mut class_file = class_file_from_data(&bytes).unwrap();
    let name_index = class_file.constant_pool.add_utf8("<init>").unwrap();
    let descriptor_index = class_file.constant_pool.add_utf8("()V").unwrap();
    class_file.methods.push(Method {
        access_flags: MethodAccessFlags::PUBLIC,
        name_index,
        descriptor_index,
        attributes: Vec::new(),
    });
    let mut bytes = Vec::new();
    class_file.to_bytes(&mut bytes).unwrap();
    bytes
}

fn abstract_class_with_method_and_interface(
    method_name: &str,
    interface_name: Option<&str>,
) -> Vec<u8> {
    let mut constant_pool = ConstantPool::default();
    let this_class = constant_pool.add_class("test/Generic").unwrap();
    let super_class = constant_pool.add_class("java/lang/Object").unwrap();
    let interfaces = interface_name
        .map(|name| vec![constant_pool.add_class(name).unwrap()])
        .unwrap_or_default();
    let name_index = constant_pool.add_utf8(method_name).unwrap();
    let descriptor_index = constant_pool.add_utf8("()V").unwrap();
    let class_file = ClassFile {
        version: Version::Java8 { minor: 0 },
        constant_pool,
        access_flags: ClassAccessFlags::PUBLIC
            | ClassAccessFlags::ABSTRACT
            | ClassAccessFlags::SUPER,
        this_class,
        super_class,
        interfaces,
        methods: vec![Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::ABSTRACT,
            name_index,
            descriptor_index,
            attributes: Vec::new(),
        }],
        ..Default::default()
    };
    let mut bytes = Vec::new();
    class_file.to_bytes(&mut bytes).unwrap();
    bytes
}

fn abstract_class_with_lambda_bootstrap(method_name: &str) -> Vec<u8> {
    let mut constant_pool = ConstantPool::default();
    let this_class = constant_pool.add_class("test/Generic").unwrap();
    let super_class = constant_pool.add_class("java/lang/Object").unwrap();
    let name_index = constant_pool.add_utf8(method_name).unwrap();
    let descriptor_index = constant_pool.add_utf8("()V").unwrap();
    let metafactory_class = constant_pool
        .add_class("java/lang/invoke/LambdaMetafactory")
        .unwrap();
    let metafactory = constant_pool
            .add_method_ref(
                metafactory_class,
                "metafactory",
                "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;",
            )
            .unwrap();
    let metafactory_handle = constant_pool
        .add_method_handle(ReferenceKind::InvokeStatic, metafactory)
        .unwrap();
    let sam_type = constant_pool.add_method_type("()V").unwrap();
    let target_class = constant_pool
        .add_class(format!("test/Target_{method_name}"))
        .unwrap();
    let target_method = constant_pool
        .add_method_ref(target_class, "run", "()V")
        .unwrap();
    let target_handle = constant_pool
        .add_method_handle(ReferenceKind::InvokeStatic, target_method)
        .unwrap();
    constant_pool
        .add_invoke_dynamic(0, "run", "()Ljava/lang/Runnable;")
        .unwrap();
    let bootstrap_name = constant_pool.add_utf8("BootstrapMethods").unwrap();
    let class_file = ClassFile {
        version: Version::Java8 { minor: 0 },
        constant_pool,
        access_flags: ClassAccessFlags::PUBLIC
            | ClassAccessFlags::ABSTRACT
            | ClassAccessFlags::SUPER,
        this_class,
        super_class,
        methods: vec![Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::ABSTRACT,
            name_index,
            descriptor_index,
            attributes: Vec::new(),
        }],
        attributes: vec![Attribute::BootstrapMethods {
            name_index: bootstrap_name,
            methods: vec![BootstrapMethod {
                bootstrap_method_ref: metafactory_handle,
                arguments: vec![sam_type, target_handle, sam_type],
            }],
        }],
        ..Default::default()
    };
    let mut bytes = Vec::new();
    class_file.to_bytes(&mut bytes).unwrap();
    bytes
}

fn class_with_near_limit_ldc_method() -> Vec<u8> {
    let mut constant_pool = ConstantPool::default();
    let this_class = constant_pool.add_class("test/Generic").unwrap();
    let super_class = constant_pool.add_class("java/lang/Object").unwrap();
    let name_index = constant_pool.add_utf8("large").unwrap();
    let descriptor_index = constant_pool.add_utf8("()V").unwrap();
    let code_name_index = constant_pool.add_utf8("Code").unwrap();
    let value_index = constant_pool.add_integer(123_456).unwrap();
    let value_index = u8::try_from(value_index).unwrap();
    let mut code = Vec::with_capacity(43_601);
    for _ in 0..21_800 {
        code.extend([Instruction::Ldc(value_index), Instruction::Pop]);
    }
    code.push(Instruction::Return);
    let class_file = ClassFile {
        version: Version::Java8 { minor: 0 },
        constant_pool,
        access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
        this_class,
        super_class,
        methods: vec![Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index,
            descriptor_index,
            attributes: vec![Attribute::Code {
                name_index: code_name_index,
                max_stack: 1,
                max_locals: 0,
                code,
                exception_table: Vec::new(),
                attributes: Vec::new(),
            }],
        }],
        ..Default::default()
    };
    let mut bytes = Vec::new();
    class_file.to_bytes(&mut bytes).unwrap();
    bytes
}

fn class_with_large_constant_pool() -> Vec<u8> {
    let bytes = abstract_class_with_method("small");
    let mut class_file = class_file_from_data(&bytes).unwrap();
    for value in 0..260 {
        class_file.constant_pool.add_integer(value).unwrap();
    }
    let mut bytes = Vec::new();
    class_file.to_bytes(&mut bytes).unwrap();
    bytes
}

fn write_test_jar(path: &std::path::Path, name: &str, contents: &[u8]) {
    let mut writer = ZipWriter::new(File::create(path).unwrap());
    writer
        .start_file(name, SimpleFileOptions::default())
        .unwrap();
    writer.write_all(contents).unwrap();
    writer.finish().unwrap();
}

#[test]
fn recognizes_msvc_output_argument_case_insensitively() {
    assert_eq!(
        msvc_output_path("/OUT:C:\\tmp\\app.exe"),
        Some("C:\\tmp\\app.exe")
    );
    assert_eq!(
        msvc_output_path("/out:C:\\tmp\\app.exe"),
        Some("C:\\tmp\\app.exe")
    );
    assert_eq!(msvc_output_path("/DEBUG"), None);
}

#[test]
fn remaps_local_variable_ranges_when_ldc_widens() {
    let old_offsets = instruction_byte_offsets(&[Instruction::Ldc(1), Instruction::Return])
        .expect("old bytecode offsets");
    let new_offsets = instruction_byte_offsets(&[Instruction::Ldc_w(256), Instruction::Return])
        .expect("new bytecode offsets");
    let mut attributes = vec![Attribute::LocalVariableTable {
        name_index: 1,
        variables: vec![LocalVariableTable {
            start_pc: 2,
            length: 1,
            name_index: 2,
            descriptor_index: 3,
            index: 0,
        }],
    }];

    remap_local_variable_ranges(&mut attributes, &old_offsets, &new_offsets)
        .expect("remapped local variable range");

    let Attribute::LocalVariableTable { variables, .. } = &attributes[0] else {
        panic!("expected local variable table")
    };
    assert_eq!(variables[0].start_pc, 3);
    assert_eq!(variables[0].length, 1);
}

#[test]
fn converts_native_output_names_to_jar_names() {
    assert_eq!(jar_output_path("target/app".into()), "target/app.jar");
    assert_eq!(jar_output_path("target/app.exe".into()), "target/app.jar");
    assert_eq!(jar_output_path("target/app.EXE".into()), "target/app.jar");
    assert_eq!(jar_output_path("target/app.jar".into()), "target/app.jar");
}

#[test]
fn parses_rustc_msvc_response_file_lines() {
    let content = concat!(
        "\"C:\\project with spaces\\Main.class\"\n",
        "\"/OUT:C:\\project with spaces\\app.exe\"\n",
        "\"/PDBALTPATH:%_PDB%\"\n",
    );
    assert_eq!(
        parse_response_lines(content, true),
        [
            "C:\\project with spaces\\Main.class",
            "/OUT:C:\\project with spaces\\app.exe",
            "/PDBALTPATH:%_PDB%",
        ]
    );
}

#[test]
fn parses_rustc_gnu_response_file_lines() {
    assert_eq!(
        parse_response_lines("path\\ with\\ spaces/Main.class\n-o\noutput\n", false),
        ["path with spaces/Main.class", "-o", "output"]
    );
}

#[test]
fn reads_compiler_class_bundles() {
    let class = abstract_class_with_method("bundled");
    let name = b"test/Generic";
    let mut bundle = CLASS_BUNDLE_MAGIC.to_vec();
    bundle.extend_from_slice(&(name.len() as u32).to_le_bytes());
    bundle.extend_from_slice(&(class.len() as u64).to_le_bytes());
    bundle.extend_from_slice(name);
    bundle.extend_from_slice(&class);

    let directory = tempdir().unwrap();
    let path = directory.path().join("test.jvmbundle");
    std::fs::write(&path, bundle).unwrap();
    let index =
        crate::inputs::Index::collect(&[], &[path.to_string_lossy().into_owned()], &[]).unwrap();
    let classes = crate::inputs::Readers::default()
        .load(&index.paths, &index.groups[0])
        .unwrap();
    assert_eq!(classes[0].jar_entry_name, "test/Generic.class");
    assert_eq!(classes[0].data, class);
}

#[test]
fn merges_complementary_generic_class_methods() {
    let first = abstract_class_with_method("first");
    let second = abstract_class_with_method("second");
    let first_constant_count = class_file_from_data(&first).unwrap().constant_pool.len();
    let merged = merge_class_data(&first, &second).unwrap();
    let class_file = class_file_from_data(&merged).unwrap();
    let methods: Vec<_> = (0..class_file.methods.len())
        .map(|index| method_identity(&class_file, index).unwrap())
        .collect();

    assert_eq!(
        methods,
        [
            ("first".into(), "()V".into()),
            ("second".into(), "()V".into())
        ]
    );
    assert_eq!(
        class_file.constant_pool.len(),
        first_constant_count + 1,
        "merging should reuse every shared class, descriptor, and attribute constant"
    );
}

#[test]
fn merge_keeps_near_limit_method_constant_indexes_compact() {
    let small = class_with_large_constant_pool();
    let large = class_with_near_limit_ldc_method();
    assert!(merge_class_data(&small, &large).is_err());

    let classes = vec![
        ClassInfo {
            jar_entry_name: "test/Generic.class".to_string(),
            data: small,
        },
        ClassInfo {
            jar_entry_name: "test/Generic.class".to_string(),
            data: large,
        },
    ];
    let merged = merge_duplicate_classes(classes).unwrap();
    assert_eq!(merged.len(), 1);
    assert_eq!(
        class_file_from_data(&merged[0].data).unwrap().methods.len(),
        2
    );
}

#[test]
fn merges_complementary_implemented_interfaces() {
    let first = abstract_class_with_method_and_interface("same", Some("test/First"));
    let second = abstract_class_with_method_and_interface("same", Some("test/Second"));
    let merged = merge_class_data(&first, &second).unwrap();
    let class_file = class_file_from_data(&merged).unwrap();
    let interfaces: Vec<_> = (0..class_file.interfaces.len())
        .map(|index| interface_name(&class_file, index).unwrap())
        .collect();

    assert_eq!(interfaces, ["test/First", "test/Second"]);
    assert_eq!(class_file.methods.len(), 1);
}

#[test]
fn merging_trait_helpers_preserves_interface_and_drops_holder_constructor() {
    let holder = holder_class_with_method_and_constructor("helper");
    let interface = interface_with_method("next");

    for (base, incoming) in [(&holder, &interface), (&interface, &holder)] {
        let merged = merge_class_data(base, incoming).unwrap();
        let class_file = class_file_from_data(&merged).unwrap();
        let mut methods: Vec<_> = (0..class_file.methods.len())
            .map(|index| method_identity(&class_file, index).unwrap().0)
            .collect();
        methods.sort();

        assert!(
            class_file
                .access_flags
                .contains(ClassAccessFlags::INTERFACE)
        );
        assert_eq!(methods, ["helper", "next"]);
    }
}

#[test]
fn merging_matching_trait_fragment_still_removes_holder_constructor() {
    let holder = holder_class_with_method_and_constructor("same");
    let interface = interface_with_method("same");
    let merged = merge_class_data(&holder, &interface).unwrap();
    let class_file = class_file_from_data(&merged).unwrap();
    let methods: Vec<_> = (0..class_file.methods.len())
        .map(|index| method_identity(&class_file, index).unwrap().0)
        .collect();

    assert!(
        class_file
            .access_flags
            .contains(ClassAccessFlags::INTERFACE)
    );
    assert_eq!(methods, ["same"]);
}

#[test]
fn merges_and_reindexes_lambda_bootstrap_methods() {
    let first = abstract_class_with_lambda_bootstrap("first");
    let second = abstract_class_with_lambda_bootstrap("second");
    let merged = merge_class_data(&first, &second).unwrap();
    let class_file = class_file_from_data(&merged).unwrap();
    let bootstrap_methods = class_file
        .attributes
        .iter()
        .find_map(|attribute| match attribute {
            Attribute::BootstrapMethods { methods, .. } => Some(methods),
            _ => None,
        })
        .expect("merged BootstrapMethods attribute");
    assert_eq!(bootstrap_methods.len(), 2);

    let mut bootstrap_indexes = HashSet::default();
    for raw_index in 1..=class_file.constant_pool.len() {
        let Ok(index) = u16::try_from(raw_index) else {
            continue;
        };
        if let Ok(Constant::InvokeDynamic {
            bootstrap_method_attr_index,
            ..
        }) = class_file.constant_pool.try_get(index)
        {
            bootstrap_indexes.insert(*bootstrap_method_attr_index);
        }
    }
    assert_eq!(bootstrap_indexes, HashSet::from([0, 1]));
}

#[test]
fn compiled_classes_take_precedence_over_runtime_jar_entries() {
    let directory = tempdir().unwrap();
    let app = directory.path().join("app.jar");
    let runtime = directory.path().join("runtime.jar");
    let output = directory.path().join("output.jar");
    let entry = "example/Owner.class";
    write_test_jar(&app, entry, b"compiled");
    write_test_jar(&runtime, entry, b"runtime");

    merge_input_jars(Some(&app), &[runtime], &output).unwrap();

    let mut archive = ZipArchive::new(File::open(output).unwrap()).unwrap();
    let mut contents = Vec::new();
    archive
        .by_name(entry)
        .unwrap()
        .read_to_end(&mut contents)
        .unwrap();
    assert_eq!(contents, b"compiled");
}

#[test]
fn final_jar_is_written_directly_with_manifest_and_app_precedence() {
    let directory = tempdir().unwrap();
    let runtime = directory.path().join("runtime.jar");
    let output = directory.path().join("output.jar");
    let entry = "test/Generic.class";
    let class = abstract_class_with_method("compiled");
    write_test_jar(&runtime, entry, b"runtime");

    write_final_jar(
        &[ClassInfo {
            jar_entry_name: entry.to_string(),
            data: class.clone(),
        }],
        &[runtime],
        &output,
        Some("test.Generic"),
    )
    .unwrap();

    let mut archive = ZipArchive::new(File::open(output).unwrap()).unwrap();
    let mut contents = Vec::new();
    archive
        .by_name(entry)
        .unwrap()
        .read_to_end(&mut contents)
        .unwrap();
    assert_eq!(contents, class);
    contents.clear();
    archive
        .by_name("META-INF/MANIFEST.MF")
        .unwrap()
        .read_to_end(&mut contents)
        .unwrap();
    assert!(
        String::from_utf8(contents)
            .unwrap()
            .contains("Main-Class: test.Generic\r\n")
    );
}

fn bundle_fragment(class: &[u8]) -> Vec<u8> {
    let name = jvm_compiler_core::classfile::summary::read(class)
        .unwrap()
        .name;
    let mut out = CLASS_BUNDLE_MAGIC.to_vec();
    out.extend((name.len() as u32).to_le_bytes());
    out.extend((class.len() as u64).to_le_bytes());
    out.extend(name.as_bytes());
    out.extend(class);
    out
}

fn ar_member(out: &mut Vec<u8>, name: &str, bytes: &[u8]) {
    out.extend(
        format!(
            "{name:<16}{:<12}{:<6}{:<6}{:<8}{:<10}`\n",
            0,
            0,
            0,
            0,
            bytes.len()
        )
        .bytes(),
    );
    out.extend(bytes);
    if out.len() % 2 != 0 {
        out.push(b'\n');
    }
}

#[test]
fn indexes_gnu_and_bsd_archives_and_skips_unrelated_payloads() {
    let directory = tempdir().unwrap();
    let first = abstract_class_with_method("gnu");
    let second = abstract_class_with_method("bsd");
    let mut archive = b"!<arch>\n".to_vec();
    ar_member(&mut archive, "/", b"symbol table");
    ar_member(&mut archive, "//", b"very-long-member-name.jvmbundle/\n");
    ar_member(&mut archive, "/0", &bundle_fragment(&first));
    let name = b"another-long-member-name.class";
    ar_member(
        &mut archive,
        &format!("#1/{}", name.len()),
        &[name.as_slice(), &second].concat(),
    );
    ar_member(&mut archive, "lib.rmeta/", &vec![0; 128 * 1024]);
    let path = directory.path().join("mixed.rlib");
    std::fs::write(&path, &archive).unwrap();
    let index =
        crate::inputs::Index::collect(&[], &[], &[path.to_string_lossy().into_owned()]).unwrap();
    assert_eq!(index.groups.len(), 1);
    assert_eq!(index.groups[0].fragments.len(), 2);
    assert_eq!(index.groups[0].bytes, first.len() + second.len());
    let classes = crate::inputs::Readers::default()
        .load(&index.paths, &index.groups[0])
        .unwrap();
    assert_eq!(classes[0].data, first);
    assert_eq!(classes[1].data, second);
    // A length that extends beyond its containing member must fail before
    // allocating the claimed class payload.
    let mut broken = bundle_fragment(&first);
    broken[12..20].copy_from_slice(&u64::MAX.to_le_bytes());
    let path = directory.path().join("bad.jvmbundle");
    std::fs::write(&path, broken).unwrap();
    assert!(
        crate::inputs::Index::collect(&[], &[path.to_string_lossy().into_owned()], &[]).is_err()
    );
    std::fs::write(&path, &archive[..archive.len() - 1]).unwrap();
    assert!(
        crate::inputs::Index::collect(&[], &[], &[path.to_string_lossy().into_owned()]).is_err()
    );
}

#[test]
fn borrowed_metadata_matches_full_reader_and_rejects_truncation() {
    use jvm_compiler_core::classfile::summary;
    let mut class = class_file_from_data(&abstract_class_with_method("ordinary")).unwrap();
    let name = "test/Unicode_🦀";
    class.this_class = class.constant_pool.add_class(name).unwrap();
    // Exercise both wide constant slots while reading a later UTF8 name.
    class.constant_pool.add_long(i64::MIN).unwrap();
    class.constant_pool.add_double(-0.0).unwrap();
    class.methods[0].name_index = class.constant_pool.add_utf8("main").unwrap();
    class.methods[0].descriptor_index = class
        .constant_pool
        .add_utf8("([Ljava/lang/String;)V")
        .unwrap();
    class.methods[0].access_flags = MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC;
    let mut bytes = Vec::new();
    class.to_bytes(&mut bytes).unwrap();
    assert_eq!(
        summary::read(&bytes).unwrap(),
        summary::Summary {
            name: name.into(),
            has_main: true
        }
    );
    for length in 0..bytes.len() {
        assert!(summary::read(&bytes[..length]).is_err(), "length {length}");
    }
    for (flags, descriptor) in [
        (MethodAccessFlags::PUBLIC, "([Ljava/lang/String;)V"),
        (MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC, "()V"),
    ] {
        class.methods[0].access_flags = flags;
        class.methods[0].descriptor_index = class.constant_pool.add_utf8(descriptor).unwrap();
        bytes.clear();
        class.to_bytes(&mut bytes).unwrap();
        assert!(!summary::read(&bytes).unwrap().has_main);
    }
}

#[test]
fn indexed_pipeline_merges_fragments_and_preserves_output_on_failure() {
    let directory = tempdir().unwrap();
    let first = directory.path().join("first.jvmbundle");
    let second = directory.path().join("second.jvmbundle");
    std::fs::write(
        &first,
        bundle_fragment(&abstract_class_with_method("first")),
    )
    .unwrap();
    std::fs::write(
        &second,
        bundle_fragment(&abstract_class_with_method("second")),
    )
    .unwrap();
    let output = directory.path().join("out.jar");
    let paths = [
        first.to_string_lossy().into_owned(),
        second.to_string_lossy().into_owned(),
    ];
    crate::pipeline::link(&[], &paths, &[], &[], output.to_str().unwrap()).unwrap();
    let mut archive = ZipArchive::new(File::open(&output).unwrap()).unwrap();
    let mut class = Vec::new();
    archive
        .by_name("test/Generic.class")
        .unwrap()
        .read_to_end(&mut class)
        .unwrap();
    assert_eq!(class_file_from_data(&class).unwrap().methods.len(), 2);
    let previous = std::fs::read(&output).unwrap();
    // Fail after opening the staged JAR, during library merging.
    assert!(
        crate::pipeline::link(
            &[],
            &paths,
            &[],
            &[directory
                .path()
                .join("missing.jar")
                .to_string_lossy()
                .into_owned()],
            output.to_str().unwrap()
        )
        .is_err()
    );
    assert_eq!(std::fs::read(output).unwrap(), previous);
}
