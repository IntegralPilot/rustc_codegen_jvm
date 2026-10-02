use crate::test_support::{body, holder, write_classes};
use crate::*;

fn forward(class: &mut ClassFile<'static>, name: &str, owner: &str, target: &str) {
    let owner = class.constant_pool.add_class(owner).unwrap();
    let target = class
        .constant_pool
        .add_method_ref(owner, target, "(I)I")
        .unwrap();
    body(
        class,
        name,
        "(I)I",
        vec![
            Instruction::Iload_0,
            Instruction::Invokestatic(target),
            Instruction::Istore_0,
            Instruction::Iload_0,
            Instruction::Ireturn,
        ],
    );
}

#[test]
fn forwarder_proof_keeps_reflection_private_targets_initializers_cycles_and_missing_bodies() {
    for mode in [
        "plain",
        "reflection",
        "private",
        "initializer",
        "cycle",
        "missing",
    ] {
        let mut main = holder("test/Main", false);
        let mut a = holder("test/mono/Mono_a", true);
        let mut target = holder("test/mono/Mono_target", true);
        let owner = main.constant_pool.add_class("test/mono/Mono_a").unwrap();
        main.constant_pool
            .add_method_ref(owner, "forward", "(I)I")
            .unwrap();
        let (owner, method) = if mode == "cycle" {
            ("test/mono/Mono_a", "forward")
        } else {
            ("test/mono/Mono_target", "identity")
        };
        forward(&mut a, "forward", owner, method);
        body(
            &mut target,
            "identity",
            "(I)I",
            vec![Instruction::Iload_0, Instruction::Ireturn],
        );
        if mode == "reflection" {
            main.constant_pool.add_string("forward").unwrap();
        }
        if mode == "private" {
            target.methods[0]
                .access_flags
                .remove(ristretto_classfile::MethodAccessFlags::PUBLIC);
        }
        if mode == "initializer" {
            body(&mut a, "<clinit>", "()V", vec![Instruction::Return]);
        }
        let mut graph = reachability::Graph::default();
        for class in [main, a] {
            graph
                .scan(&serialize_class_file(&class).unwrap(), false)
                .unwrap();
        }
        if mode != "missing" {
            graph
                .scan(&serialize_class_file(&target).unwrap(), false)
                .unwrap();
        }
        let demands = graph.finish(true);
        assert_eq!(
            demands.aliases.len(),
            usize::from(mode == "plain"),
            "{mode}"
        );
    }
}

#[test]
fn forwarders_redirect_calls_and_handles_without_an_additional_jar_pass() {
    let mut main = holder("test/Main", false);
    let mut a = holder("test/mono/Mono_a", true);
    let mut b = holder("test/mono/Mono_b", true);
    let mut target = holder("test/mono/Mono_target", true);
    forward(&mut a, "forward", "test/mono/Mono_b", "forward_again");
    forward(&mut b, "forward_again", "test/mono/Mono_target", "identity");
    body(
        &mut target,
        "identity",
        "(I)I",
        vec![Instruction::Iload_0, Instruction::Ireturn],
    );
    let owner = main.constant_pool.add_class("test/mono/Mono_a").unwrap();
    let call = main
        .constant_pool
        .add_method_ref(owner, "forward", "(I)I")
        .unwrap();
    let handle = main
        .constant_pool
        .add_method_handle(ristretto_classfile::ReferenceKind::InvokeStatic, call)
        .unwrap();
    let handle_type = main
        .constant_pool
        .add_class("java/lang/invoke/MethodHandle")
        .unwrap();
    let invoke = main
        .constant_pool
        .add_method_ref(handle_type, "invokeExact", "(I)I")
        .unwrap();
    let system = main.constant_pool.add_class("java/lang/System").unwrap();
    let out = main
        .constant_pool
        .add_field_ref(system, "out", "Ljava/io/PrintStream;")
        .unwrap();
    let print = main.constant_pool.add_class("java/io/PrintStream").unwrap();
    let print = main
        .constant_pool
        .add_method_ref(print, "println", "(I)V")
        .unwrap();
    body(
        &mut main,
        "main",
        "([Ljava/lang/String;)V",
        vec![
            Instruction::Iconst_5,
            Instruction::Invokestatic(call),
            Instruction::Istore_0,
            Instruction::Getstatic(out),
            Instruction::Iload_0,
            Instruction::Invokevirtual(print),
            Instruction::Ldc_w(handle),
            Instruction::Bipush(7),
            Instruction::Invokevirtual(invoke),
            Instruction::Istore_0,
            Instruction::Getstatic(out),
            Instruction::Iload_0,
            Instruction::Invokevirtual(print),
            Instruction::Return,
        ],
    );
    let temp = tempfile::tempdir().unwrap();
    let paths = write_classes(temp.path(), [main, a, b, target]);
    let output = temp.path().join("test.jar");
    pipeline::link(&paths, &[], &[], &[], output.to_str().unwrap()).unwrap();
    let execution = std::process::Command::new("java")
        .args(["-Xverify:all", "-cp", output.to_str().unwrap(), "test.Main"])
        .output()
        .unwrap();
    assert!(
        execution.status.success(),
        "{}",
        String::from_utf8_lossy(&execution.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&execution.stdout), "5\n7\n");
    let mut jar = ZipArchive::new(fs::File::open(output).unwrap()).unwrap();
    let mut methods = 0;
    for i in 0..jar.len() {
        let mut entry = jar.by_index(i).unwrap();
        if !entry.name().ends_with(".class") {
            continue;
        }
        let mut bytes = Vec::new();
        entry.read_to_end(&mut bytes).unwrap();
        methods += class_file_from_data(&bytes).unwrap().methods.len();
    }
    assert_eq!(methods, 2, "only main and the actual implementation remain");
}

#[test]
fn forwarder_proof_tracks_argument_order_wide_slots_and_fallthrough() {
    for (name, descriptor, mut loads, ret, accepted) in [
        (
            "ordered",
            "(II)I",
            vec![Instruction::Iload_0, Instruction::Iload_1],
            Instruction::Ireturn,
            true,
        ),
        (
            "reordered",
            "(II)I",
            vec![Instruction::Iload_1, Instruction::Iload_0],
            Instruction::Ireturn,
            false,
        ),
        (
            "wide",
            "(JD)J",
            vec![Instruction::Lload_0, Instruction::Dload_2],
            Instruction::Lreturn,
            true,
        ),
        (
            "duplicate",
            "(II)I",
            vec![Instruction::Iload_0, Instruction::Iload_0],
            Instruction::Ireturn,
            false,
        ),
        (
            "backward",
            "(II)I",
            vec![Instruction::Iload_0, Instruction::Iload_1],
            Instruction::Ireturn,
            false,
        ),
    ] {
        let mut main = holder("test/Main", false);
        let mut source = holder("test/mono/Mono_source", true);
        let mut target = holder("test/mono/Mono_target", true);
        let owner = main
            .constant_pool
            .add_class("test/mono/Mono_source")
            .unwrap();
        main.constant_pool
            .add_method_ref(owner, name, descriptor)
            .unwrap();
        let owner = source
            .constant_pool
            .add_class("test/mono/Mono_target")
            .unwrap();
        let call = source
            .constant_pool
            .add_method_ref(owner, "target", descriptor)
            .unwrap();
        loads.extend([
            Instruction::Invokestatic(call),
            Instruction::Goto(if name == "backward" { 0 } else { 4 }),
            ret.clone(),
        ]);
        body(&mut source, name, descriptor, loads);
        body(
            &mut target,
            "target",
            descriptor,
            vec![
                if descriptor == "(JD)J" {
                    Instruction::Lload_0
                } else {
                    Instruction::Iload_0
                },
                ret,
            ],
        );
        for class in [&mut source, &mut target] {
            for method in &mut class.methods {
                for attribute in &mut method.attributes {
                    if let Attribute::Code {
                        max_stack,
                        max_locals,
                        ..
                    } = attribute
                    {
                        *max_stack = 4;
                        *max_locals = 4;
                    }
                }
            }
        }
        let mut graph = reachability::Graph::default();
        for class in [main, source, target] {
            graph
                .scan(&serialize_class_file(&class).unwrap(), false)
                .unwrap();
        }
        assert_eq!(
            graph.finish(true).aliases.len(),
            usize::from(accepted),
            "{name}"
        );
    }
}
