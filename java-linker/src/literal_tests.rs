use crate::test_support::{body, holder, run_jar, write_classes};
use crate::*;
use jvm_compiler_core::classfile::constant_pool::InternedConstantPool;

#[test]
fn literal_text_does_not_retain_methods_classes_or_resources() {
    let mut main = holder("test/Main", false);
    let mut pool = InternedConstantPool::default();
    main.this_class = pool.add_class("test/Main").unwrap();
    main.super_class = pool.add_class("java/lang/Object").unwrap();
    main.attributes.clear();
    let system = pool.add_class("java/lang/System").unwrap();
    let stdout = pool
        .add_field_ref(system, "out", "Ljava/io/PrintStream;")
        .unwrap();
    let stream = pool.add_class("java/io/PrintStream").unwrap();
    let println = pool
        .add_method_ref(stream, "println", "(Ljava/lang/String;)V")
        .unwrap();
    let mut code = Vec::new();
    for literal in [
        "eq",
        "test.Ghost",
        "META-INF/rust-data/0123456789abcdef.bin",
    ] {
        code.extend([
            Instruction::Getstatic(stdout),
            Instruction::Ldc_w(pool.add_string(literal).unwrap()),
            Instruction::Invokevirtual(println),
        ]);
    }
    // Actual compiler reflection names still retain their target method.
    let kept = pool.add_name_string("kept").unwrap();
    code.extend([
        Instruction::Ldc_w(pool.add_name_string("test/mono/Mono_needed").unwrap()),
        Instruction::Pop,
        Instruction::Ldc_w(kept),
        Instruction::Pop,
        Instruction::Return,
    ]);
    main.constant_pool = pool.into_inner();
    body(&mut main, "main", "([Ljava/lang/String;)V", code);
    let mut unused = holder("test/mono/Mono_unused", true);
    body(&mut unused, "eq", "()V", vec![Instruction::Return]);
    let mut needed = holder("test/mono/Mono_needed", true);
    body(&mut needed, "kept", "()V", vec![Instruction::Return]);
    let temp = tempfile::tempdir().unwrap();
    let paths = write_classes(
        temp.path(),
        [main, unused, needed, holder("test/Ghost", true)],
    );
    let output = temp.path().join("literals.jar");
    pipeline::link(&paths, &[], &[], &[], output.to_str().unwrap()).unwrap();
    let result = run_jar(&output);
    assert_eq!(
        String::from_utf8(result.stdout)
            .unwrap()
            .replace("\r\n", "\n"),
        "eq\ntest.Ghost\nMETA-INF/rust-data/0123456789abcdef.bin\n"
    );
    let mut jar = ZipArchive::new(fs::File::open(output).unwrap()).unwrap();
    let names = jar
        .file_names()
        .filter(|name| name.ends_with(".class"))
        .map(str::to_owned)
        .collect::<Vec<_>>();
    assert_eq!(
        names.len(),
        2,
        "only main and the reflection target are live"
    );
    for name in names {
        let mut bytes = Vec::new();
        jar.by_name(&name).unwrap().read_to_end(&mut bytes).unwrap();
        let class = class_file_from_data(&bytes).unwrap();
        assert!(
            class.methods.iter().all(|method| class
                .constant_pool
                .try_get_utf8(method.name_index)
                .unwrap()
                != "eq")
        );
    }
}
