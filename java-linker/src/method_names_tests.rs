use crate::test_support::{body, holder, run_jar, write_classes};
use crate::*;
use jvm_compiler_core::classfile::names;

#[test]
fn shortened_methods_preserve_handles_overloads_literals_and_nominal_names() {
    let temp = tempfile::tempdir().unwrap();
    let value = "value$0123456789abcdef";
    let reflected = "reflected$0123456789abcdef";
    let identity = "identity$0123456789abcdef";
    let field = "field$0123456789abcdef";
    let published = "published$0123456789abcdef";
    let owner = "test/mono/Mono_names";
    fs::write(temp.path().join("NameLookup.java"), format!(r#"
package org.rustlang.runtime;
public class NameLookup {{
    public static int {field};
    public static void {published}() {{}}
    public static void check(Class<?> owner, String name) throws Exception {{
        if (!Integer.valueOf(3).equals(owner.getMethod(name).invoke(null))) throw new AssertionError(name);
    }}
    public static void identity(String name) throws Exception {{
        String[] parts = name.split("::", 2);
        check(Class.forName(parts[0].replace('/', '.')), parts[1].split(":", 2)[0]);
    }}
}}
"#)).unwrap();
    let javac = std::process::Command::new("javac")
        .args(["--release", "8", "-d", ".", "NameLookup.java"])
        .current_dir(temp.path())
        .output()
        .unwrap();
    assert!(
        javac.status.success(),
        "{}",
        String::from_utf8_lossy(&javac.stderr)
    );
    let mut helper = holder(owner, true);
    for name in [value, reflected, identity, field, published] {
        body(
            &mut helper,
            name,
            "()I",
            vec![Instruction::Iconst_3, Instruction::Ireturn],
        );
    }
    body(
        &mut helper,
        value,
        "(I)I",
        vec![Instruction::Iload_0, Instruction::Ireturn],
    );
    // Reserve the default short-name prefix, including a method that dies.
    body(&mut helper, "$m0", "()V", vec![Instruction::Return]);
    let pool = &mut helper.constant_pool;
    let system = pool.add_class("java/lang/System").unwrap();
    let out = pool
        .add_field_ref(system, "out", "Ljava/io/PrintStream;")
        .unwrap();
    let stream = pool.add_class("java/io/PrintStream").unwrap();
    let print = pool.add_method_ref(stream, "println", "(I)V").unwrap();
    let print_string = pool
        .add_method_ref(stream, "println", "(Ljava/lang/String;)V")
        .unwrap();
    let mut code = Vec::new();
    for name in [value, field, published] {
        let call = pool.add_method_ref(helper.this_class, name, "()I").unwrap();
        code.extend([
            Instruction::Getstatic(out),
            Instruction::Invokestatic(call),
            Instruction::Invokevirtual(print),
        ]);
    }
    let target = pool
        .add_method_ref(helper.this_class, value, "(I)I")
        .unwrap();
    let handle = pool
        .add_method_handle(ristretto_classfile::ReferenceKind::InvokeStatic, target)
        .unwrap();
    let mh = pool.add_class("java/lang/invoke/MethodHandle").unwrap();
    let invoke = pool.add_method_ref(mh, "invokeExact", "(I)I").unwrap();
    code.extend([
        Instruction::Getstatic(out),
        Instruction::Ldc_w(handle),
        Instruction::Iconst_5,
        Instruction::Invokevirtual(invoke),
        Instruction::Invokevirtual(print),
    ]);
    let literal = pool
        .add_string(format!("{}{value}", names::LITERAL_STRING))
        .unwrap();
    code.extend([
        Instruction::Getstatic(out),
        Instruction::Ldc_w(literal),
        Instruction::Invokevirtual(print_string),
    ]);
    let lookup = pool.add_class("org/rustlang/runtime/NameLookup").unwrap();
    let check = pool
        .add_method_ref(lookup, "check", "(Ljava/lang/Class;Ljava/lang/String;)V")
        .unwrap();
    let reflected_string = pool.add_string(reflected).unwrap();
    code.extend([
        Instruction::Ldc_w(helper.this_class),
        Instruction::Ldc_w(reflected_string),
        Instruction::Invokestatic(check),
    ]);
    let check_identity = pool
        .add_method_ref(lookup, "identity", "(Ljava/lang/String;)V")
        .unwrap();
    let identity_string = pool.add_string(format!("{owner}::{identity}:()I")).unwrap();
    code.extend([
        Instruction::Ldc_w(identity_string),
        Instruction::Invokestatic(check_identity),
        Instruction::Return,
    ]);
    body(&mut helper, "run", "()V", code);
    if let Attribute::Code { max_stack, .. } = helper
        .methods
        .last_mut()
        .unwrap()
        .attributes
        .last_mut()
        .unwrap()
    {
        *max_stack = 3;
    }
    let mut main = holder("test/Main", false);
    let class = main.constant_pool.add_class(owner).unwrap();
    let run = main
        .constant_pool
        .add_method_ref(class, "run", "()V")
        .unwrap();
    body(
        &mut main,
        "main",
        "([Ljava/lang/String;)V",
        vec![Instruction::Invokestatic(run), Instruction::Return],
    );
    let mut paths = vec![
        temp.path()
            .join("org/rustlang/runtime/NameLookup.class")
            .to_string_lossy()
            .into_owned(),
    ];
    paths.extend(write_classes(temp.path(), [main, helper]));
    let output = temp.path().join("names.jar");
    pipeline::link(&paths, &[], &[], &[], output.to_str().unwrap()).unwrap();
    let mut jar = ZipArchive::new(fs::File::open(&output).unwrap()).unwrap();
    let class_name = jar
        .file_names()
        .find(|name| name.contains("/mono/"))
        .unwrap()
        .to_owned();
    let mut bytes = Vec::new();
    jar.by_name(&class_name)
        .unwrap()
        .read_to_end(&mut bytes)
        .unwrap();
    let linked = class_file_from_data(&bytes).unwrap();
    let methods = linked
        .methods
        .iter()
        .map(|m| {
            linked
                .constant_pool
                .try_get_utf8(m.name_index)
                .unwrap()
                .to_string()
        })
        .collect::<Vec<_>>();
    assert!(!methods.iter().any(|m| m == value), "{methods:?}");
    assert_eq!(
        methods.iter().filter(|m| m.starts_with("value$m$")).count(),
        2
    );
    for name in [reflected, identity, field, published] {
        assert!(methods.iter().any(|m| m == name), "{name}: {methods:?}");
    }
    let run = run_jar(&output);
    assert_eq!(
        String::from_utf8(run.stdout).unwrap().replace("\r\n", "\n"),
        format!("3\n3\n3\n5\n{value}\n")
    );
}
