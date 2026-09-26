use super::*;
use std::{fs, process::Command};

#[test]
fn large_enum_equality_verifies_and_executes() {
    let name = "LargeEnum";
    let mut module = oomir::Module {
        name: name.into(),
        source_file: None,
        functions: HashMap::default(),
        data_types: HashMap::default(),
        suppressed_data_types: HashSet::default(),
        shared_data_types: None,
        relative_static_methods: Default::default(),
        external_interfaces: HashSet::default(),
        statics: HashMap::default(),
    };
    module.data_types.insert(
        name.into(),
        oomir::DataType::Interface {
            methods: HashMap::default(),
            interfaces: vec![],
            is_enum: true,
        },
    );
    // A single equality method for this shape exceeds both the short-branch
    // range and the JVM's 65535-byte method limit.
    let variants: Vec<_> = (0..180)
        .map(|index| oomir::EnumVariantShape {
            runtime_type: format!("Variant{index}"),
            fields: (0..30)
                .map(|field| (format!("f{field}"), Type::I32))
                .collect(),
            transparent: false,
        })
        .collect();
    let mut cp = InternedConstantPool::default();
    let this_class = cp.add_class(name).unwrap();
    let super_class = cp.add_class("java/lang/Object").unwrap();
    let methods =
        create_enum_equality_methods(&mut cp, &module, name, "eq", name, &variants).unwrap();
    let class = ClassFile {
        code_source_url: None,
        version: Version::Java8 { minor: 0 },
        constant_pool: cp.into_inner(),
        access_flags: ClassAccessFlags::PUBLIC
            | ClassAccessFlags::INTERFACE
            | ClassAccessFlags::ABSTRACT,
        this_class,
        super_class,
        interfaces: vec![],
        fields: vec![],
        methods,
        attributes: vec![],
    };
    let mut bytes = Vec::new();
    jvm::encode::class_file(&class, &mut bytes).unwrap();
    let directory = std::env::temp_dir().join(format!("rcj-enum-eq-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();
    fs::write(directory.join("LargeEnum.class"), bytes).unwrap();
    let mut source = String::from(
        r#"
public class Run {
    public static void main(String[] args) throws Exception {
        if (!LargeEnum.eq(null, null)) throw new AssertionError("null identity");
        for (int index = 0; index < 180; index++) {
            Class<?> type = Class.forName("Variant" + index);
            LargeEnum a = (LargeEnum) type.getDeclaredConstructor().newInstance();
            LargeEnum b = (LargeEnum) type.getDeclaredConstructor().newInstance();
            if (!LargeEnum.eq(a, a) || !LargeEnum.eq(a, b)) throw new AssertionError("equal " + index);
            if (LargeEnum.eq(a, null) || LargeEnum.eq(null, a)) throw new AssertionError("null " + index);
            type.getField("f29").setInt(b, 7);
            if (LargeEnum.eq(a, b)) throw new AssertionError("field " + index);
            if (index != 0 && LargeEnum.eq(a, new Variant0())) throw new AssertionError("variant " + index);
        }
    }
}
"#,
    );
    for variant in &variants {
        source.push_str(&format!(
            "class {} implements LargeEnum {{\n",
            variant.runtime_type
        ));
        for (field, _) in &variant.fields {
            source.push_str(&format!("public int {field};\n"));
        }
        source.push_str("}\n");
    }
    fs::write(directory.join("Run.java"), source).unwrap();
    for (program, args) in [
        ("javac", vec!["-cp", ".", "Run.java"]),
        ("java", vec!["-Xverify:all", "-cp", ".", "Run"]),
    ] {
        let result = Command::new(program)
            .args(args)
            .current_dir(&directory)
            .output()
            .unwrap();
        assert!(
            result.status.success(),
            "{program}: {}\n{}",
            String::from_utf8_lossy(&result.stdout),
            String::from_utf8_lossy(&result.stderr)
        );
    }
    fs::remove_dir_all(directory).unwrap();
}
