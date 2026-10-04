use crate::test_support::{body, holder, run_jar, write_classes};
use crate::*;
use jvm_compiler_core::classfile::names;

#[test]
fn compact_names_avoid_external_names_and_preserve_reflective_codecs() {
    let temp = tempfile::tempdir().unwrap();
    let source = temp.path().join("Lookup.java");
    fs::write(
        &source,
        r##"
package org.rustlang.runtime;
public class Lookup {
    public static void checkBody(Class<?> carrier) throws Exception {
        Class.forName(carrier.getName() + "$Body");
    }
    public static void check(String recipe, int expected) throws Exception {
        String[] parts = recipe.split("#");
        Object value = Class.forName(parts[0].replace('/', '.'))
            .getMethod("e$" + parts[1]).invoke(null);
        if (!Integer.valueOf(expected).equals(value)) throw new AssertionError(value);
    }
}
"##,
    )
    .unwrap();
    assert!(
        std::process::Command::new("javac")
            .arg("-d")
            .arg(temp.path())
            .arg(&source)
            .status()
            .unwrap()
            .success()
    );
    let mut paths = vec![
        temp.path()
            .join("org/rustlang/runtime/Lookup.class")
            .to_string_lossy()
            .into_owned(),
    ];
    let mut main = holder("test/Main", false);
    let lookup = main
        .constant_pool
        .add_class("org/rustlang/runtime/Lookup")
        .unwrap();
    let check = main
        .constant_pool
        .add_method_ref(lookup, "check", "(Ljava/lang/String;I)V")
        .unwrap();
    let mut code = Vec::new();
    let mut classes = Vec::new();
    for (owner, key, value) in [
        (
            "test/Codecs_first",
            "0123456789abcdef",
            Instruction::Iconst_1,
        ),
        (
            "test/Codecs_second",
            "fedcba9876543210",
            Instruction::Iconst_2,
        ),
    ] {
        let mut codec = holder(owner, true);
        body(
            &mut codec,
            &format!("e${key}"),
            "()I",
            vec![value.clone(), Instruction::Ireturn],
        );
        classes.push(codec);
        let recipe = main
            .constant_pool
            .add_string(format!("{}{owner}#{key}#I", names::NAME_STRING))
            .unwrap();
        code.extend([
            Instruction::Ldc_w(recipe),
            value,
            Instruction::Invokestatic(check),
        ]);
    }
    // Occupied roots include identities whose crate marker will be removed.
    classes.push(holder("j$crate0123456789abcdef$/Api", false));
    classes.push(holder("j1/Api", false));
    classes.push(holder("test/Future", true));
    classes.push(holder("test/Future$Body", false));
    let future = main.constant_pool.add_class("test/Future").unwrap();
    let check_body = main
        .constant_pool
        .add_method_ref(lookup, "checkBody", "(Ljava/lang/Class;)V")
        .unwrap();
    code.extend([
        Instruction::Ldc_w(future),
        Instruction::Invokestatic(check_body),
    ]);
    let callable = "org/rustlang/runtime/FnPtr_test";
    classes.push(holder(callable, true));
    let callable_class = main.constant_pool.add_class(callable).unwrap();
    code.extend([
        Instruction::Ldc_w(callable_class),
        Instruction::Pop,
        Instruction::Return,
    ]);
    // The same UTF8 entry is both a symbolic Class and an ordinary String.
    main.constant_pool.add_class("test/Codecs_first").unwrap();
    main.constant_pool.add_string("test/Codecs_first").unwrap();
    body(&mut main, "main", "([Ljava/lang/String;)V", code);
    classes.push(main);
    paths.extend(write_classes(temp.path(), classes));
    let output = temp.path().join("compact.jar");
    pipeline::link(&paths, &[], &[], &[], output.to_str().unwrap()).unwrap();
    run_jar(&output);
    let mut jar = ZipArchive::new(fs::File::open(output).unwrap()).unwrap();
    for name in [
        "j/Api.class",
        "j1/Api.class",
        "org/rustlang/runtime/FnPtr_test.class",
        "test/Main.class",
        "test/Future.class",
        "test/Future$Body.class",
    ] {
        assert!(jar.by_name(name).is_ok(), "stable identity lost: {name}");
    }
    let codecs = jar
        .file_names()
        .filter(|name| name.contains("Codecs_"))
        .collect::<Vec<_>>();
    assert_eq!(codecs.len(), 1, "codec owners must pack");
    assert!(codecs[0].starts_with("j2/Codecs_"), "{codecs:?}");
    let mut bytes = Vec::new();
    jar.by_name("test/Main.class")
        .unwrap()
        .read_to_end(&mut bytes)
        .unwrap();
    let main = class_file_from_data(&bytes).unwrap();
    assert!(
        main.constant_pool.iter().any(|c| match c {
            Constant::String(i) =>
                main.constant_pool.try_get_utf8(*i).unwrap() == "test/Codecs_first",
            _ => false,
        }),
        "ordinary literal must not follow class relocation"
    );
}
