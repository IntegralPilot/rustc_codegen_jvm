use crate::test_support::run_jar;
use crate::*;
use ristretto_classfile::{Method, MethodAccessFlags, ReferenceKind, Version};

fn fragment(owner: &str, start: i32) -> ClassInfo {
    let mut pool = ConstantPool::default();
    let this_class = pool.add_class(owner).unwrap();
    let super_class = pool.add_class("java/lang/Object").unwrap();
    let descriptor_index = pool.add_utf8("()V").unwrap();
    let code_name = pool.add_utf8("Code").unwrap();
    let mut methods = Vec::new();
    for m in 0..100 {
        let name_index = pool.add_utf8(format!("f{}", start + m)).unwrap();
        let mut code = Vec::new();
        for n in 0..400 {
            code.push(Instruction::Ldc_w(
                pool.add_integer(start * 400 + m * 400 + n).unwrap(),
            ));
            code.push(Instruction::Pop);
        }
        code.push(Instruction::Return);
        methods.push(Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index,
            descriptor_index,
            attributes: vec![Attribute::Code {
                name_index: code_name,
                max_stack: 1,
                max_locals: 0,
                code,
                exception_table: Vec::new(),
                attributes: Vec::new(),
            }],
        });
    }
    let class = ClassFile {
        version: Version::Java8 { minor: 0 },
        constant_pool: pool,
        access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
        this_class,
        super_class,
        methods,
        ..Default::default()
    };
    ClassInfo {
        jar_entry_name: format!("{owner}.class"),
        data: serialize_class_file(&class).unwrap(),
    }
}

#[test]
fn oversized_holder_is_split_and_calls_and_handles_follow() {
    let owner = "test/mono/Mono_example_00";
    let fragments = vec![fragment(owner, 0), fragment(owner, 100)];
    assert!(merge_class_data(&fragments[0].data, &fragments[1].data).is_err());
    let temp = tempfile::tempdir().unwrap();
    let mut paths = Vec::new();
    for (i, class) in fragments.iter().enumerate() {
        let path = temp.path().join(format!("{i}.class"));
        fs::write(&path, &class.data).unwrap();
        paths.push(path.to_string_lossy().into_owned());
    }
    let mut caller = class_file_from_data(&fragments[0].data).unwrap();
    caller.methods.clear();
    caller.this_class = caller.constant_pool.add_class("test/Caller").unwrap();
    let reference = caller
        .constant_pool
        .add_method_ref(fragments_class(&fragments[0]), "f100", "()V")
        .unwrap();
    let handle = caller
        .constant_pool
        .add_method_handle(ReferenceKind::InvokeStatic, reference)
        .unwrap();
    let handles = caller
        .constant_pool
        .add_class("java/lang/invoke/MethodHandle")
        .unwrap();
    let invoke = caller
        .constant_pool
        .add_method_ref(handles, "invokeExact", "()V")
        .unwrap();
    caller.methods.push(Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: caller.constant_pool.add_utf8("main").unwrap(),
        descriptor_index: caller
            .constant_pool
            .add_utf8("([Ljava/lang/String;)V")
            .unwrap(),
        attributes: vec![Attribute::Code {
            name_index: caller.constant_pool.add_utf8("Code").unwrap(),
            max_stack: 1,
            max_locals: 1,
            code: vec![
                Instruction::Invokestatic(reference),
                Instruction::Ldc_w(handle),
                Instruction::Invokevirtual(invoke),
                Instruction::Return,
            ],
            exception_table: Vec::new(),
            attributes: Vec::new(),
        }],
    });
    let path = temp.path().join("caller.class");
    fs::write(&path, serialize_class_file(&caller).unwrap()).unwrap();
    paths.push(path.to_string_lossy().into_owned());
    let output = temp.path().join("output.jar");
    pipeline::link(&paths, &[], &[], &[], output.to_str().unwrap()).unwrap();
    run_jar(&output);
    let mut jar = ZipArchive::new(fs::File::open(output).unwrap()).unwrap();
    let mut names = HashSet::default();
    let mut target = None;
    for i in 0..jar.len() {
        let mut entry = jar.by_index(i).unwrap();
        if !entry.name().ends_with(".class") {
            continue;
        }
        let mut bytes = Vec::new();
        entry.read_to_end(&mut bytes).unwrap();
        let class = class_file_from_data(&bytes).unwrap();
        assert!(class.constant_pool.len() < 65535);
        for m in 0..class.methods.len() {
            let key = method_identity(&class, m).unwrap();
            if key.0 == "f100" {
                target = Some(class.class_name().unwrap().to_owned());
            }
            assert!(names.insert(key));
        }
    }
    assert_eq!(names.len(), 201);
    let mut bytes = Vec::new();
    jar.by_name("test/Caller.class")
        .unwrap()
        .read_to_end(&mut bytes)
        .unwrap();
    let caller = class_file_from_data(&bytes).unwrap();
    let (owner_index, _) = caller.constant_pool.try_get_method_ref(reference).unwrap();
    assert_eq!(
        caller
            .constant_pool
            .try_get_class(*owner_index)
            .unwrap()
            .to_owned(),
        target.unwrap()
    );
    assert!(
        matches!(caller.constant_pool.get(handle), Some(Constant::MethodHandle { reference_index, .. }) if *reference_index == reference)
    );
}

fn fragments_class(fragment: &ClassInfo) -> u16 {
    class_file_from_data(&fragment.data).unwrap().this_class
}

#[test]
fn overflow_does_not_move_public_or_stateful_classes() {
    let fragments = vec![fragment("test/Public", 0), fragment("test/Public", 100)];
    let moves = std::sync::Mutex::new(split::Relocations::default());
    assert!(merge_group_with_relocations(fragments, Some(&moves)).is_err());
    assert!(moves.into_inner().unwrap().is_empty());
    let mut class = class_file_from_data(&fragment("test/mono/Mono_example_00", 0).data).unwrap();
    class.methods[0].name_index = class.constant_pool.add_utf8("<clinit>").unwrap();
    assert!(!split::eligible(&class));
}

#[test]
fn split_codecs_keep_reflective_lookup_and_all_argument_widths() {
    let owner = "test/Codecs_overflow";
    let mut first = class_file_from_data(&fragment(owner, 0).data).unwrap();
    first.methods.push(Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: first.constant_pool.add_utf8("mixed").unwrap(),
        descriptor_index: first
            .constant_pool
            .add_utf8("(IJFDLjava/lang/Object;[I)D")
            .unwrap(),
        attributes: vec![Attribute::Code {
            name_index: first.constant_pool.add_utf8("Code").unwrap(),
            max_stack: 2,
            max_locals: 8,
            code: vec![Instruction::Dload(4), Instruction::Dreturn],
            exception_table: Vec::new(),
            attributes: Vec::new(),
        }],
    });
    let temp = tempfile::tempdir().unwrap();
    let fragments = [
        serialize_class_file(&first).unwrap(),
        fragment(owner, 100).data,
    ];
    let paths = fragments
        .iter()
        .enumerate()
        .map(|(i, bytes)| {
            let path = temp.path().join(format!("{i}.class"));
            fs::write(&path, bytes).unwrap();
            path.to_string_lossy().into_owned()
        })
        .collect::<Vec<_>>();
    let output = temp.path().join("codecs.jar");
    pipeline::link(&paths, &[], &[], &[], output.to_str().unwrap()).unwrap();
    let check = temp.path().join("Check.java");
    fs::write(
        &check,
        r#"
public class Check {
    public static void main(String[] args) throws Exception {
        Class<?> codec = Class.forName("test.Codecs_overflow");
        codec.getMethod("f0").invoke(null);
        codec.getMethod("f150").invoke(null);
        Object result = codec.getMethod("mixed", int.class, long.class, float.class,
                double.class, Object.class, int[].class)
            .invoke(null, 1, 2L, 3.0f, 7.25, new Object(), new int[1]);
        if (!result.equals(7.25)) throw new AssertionError(result);
    }
}
"#,
    )
    .unwrap();
    let compile = std::process::Command::new("javac")
        .arg(&check)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let classpath = std::env::join_paths([temp.path(), output.as_path()]).unwrap();
    let execution = std::process::Command::new("java")
        .args(["-Xverify:all", "-cp"])
        .arg(classpath)
        .arg("Check")
        .output()
        .unwrap();
    assert!(
        execution.status.success(),
        "{}",
        String::from_utf8_lossy(&execution.stderr)
    );
}
