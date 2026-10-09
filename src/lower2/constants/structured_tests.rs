use super::*;
use std::{fs, process::Command};

fn row(index: usize) -> C {
    C::Instance {
        class_name: "Row".into(),
        param_types: vec![
            T::Array(Box::new(T::U8)),
            T::I16,
            T::U16,
            T::I32,
            T::U64,
            T::F32,
            T::F64,
            T::Boolean,
        ],
        params: vec![
            C::Array(
                Box::new(T::U8),
                (0..index % 17).map(|j| C::U8((index + j) as u8)).collect(),
            ),
            C::I16(-12345),
            C::U16(0xfedc),
            C::I32(index as i32),
            C::U64(u64::MAX),
            C::F32(-0.0),
            C::F64(f64::from_bits(0x7ff8_1234_5678_9abc)),
            C::Boolean(true),
        ],
    }
}

#[test]
fn structured_tables_use_bounded_resources_and_fresh_storage() {
    let mut cp = InternedConstantPool::default();
    let this_class = cp.add_class("Data").unwrap();
    cp.set_resource_anchor(this_class);
    let super_class = cp.add_class("java/lang/Object").unwrap();
    let mut methods = Vec::new();
    let rows = (0..10000).map(row).collect::<Vec<_>>();
    let value = factory(
        &mut cp,
        "Data",
        &T::Class("Row".into()),
        &rows,
        &mut methods,
        &mut 0,
        None,
    )
    .unwrap()
    .unwrap();
    assert_eq!(
        methods.len(),
        2,
        "table length must not multiply constructor methods"
    );
    let mut code = Vec::new();
    load_constant(&mut code, &mut cp, &value).unwrap();
    code.push(Instruction::Areturn);
    factories::add_constant_helper_method(&mut cp, &mut methods, "rows", "()[LRow;", 0, code)
        .unwrap();
    methods.last_mut().unwrap().access_flags =
        MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC;
    let resources = cp.take_resources();
    assert!(resources.len() > 1);
    assert!(
        resources
            .iter()
            .all(|r| r.bytes.len() <= jvm::resources::BLOCK_BYTES)
    );
    let class = jvm::ClassFile {
        code_source_url: None,
        version: jvm::Version::Java8 { minor: 0 },
        constant_pool: cp.into_inner(),
        access_flags: jvm::ClassAccessFlags::PUBLIC | jvm::ClassAccessFlags::SUPER,
        this_class,
        super_class,
        interfaces: vec![],
        fields: vec![],
        methods,
        attributes: vec![],
    };
    let mut bytes = Vec::new();
    jvm::encode::class_file(&class, &mut bytes).unwrap();
    assert!(
        bytes.len() < 4096,
        "constant data expanded into bytecode: {}",
        bytes.len()
    );
    let directory =
        std::env::temp_dir().join(format!("rcj-structured-data-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();
    let jar = super::super::test_support::link(&directory, &class, resources);
    fs::write(
        directory.join("Row.java"),
        r#"
public class Row {
    final byte[] bytes;
    final int index;
    public Row(byte[] bytes, short s, char c, int index, long l, float f, double d, boolean b) {
        if (s != -12345 || c != 0xfedc || l != -1 || Float.floatToRawIntBits(f) != 0x80000000
                || Double.doubleToRawLongBits(d) != 0x7ff8123456789abcL || !b)
            throw new AssertionError("scalar decoding");
        this.bytes = bytes; this.index = index;
    }
}
"#,
    )
    .unwrap();
    let result = Command::new("javac")
        .arg(directory.join("Row.java"))
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    fs::write(
        directory.join("Run.java"),
        r#"
class Run {
    public static void main(String[] args) {
        Row[] a = Data.rows(), b = Data.rows();
        if (a.length != 10000) throw new AssertionError("length");
        for (int i = 0; i < a.length; i++) {
            if (a[i].index != i || a[i].bytes.length != i % 17) throw new AssertionError("row " + i);
            for (int j = 0; j < a[i].bytes.length; j++) {
                if (a[i].bytes[j] != (byte)(i + j)) throw new AssertionError("byte " + i);
            }
            if (a[i] == b[i] || a[i].bytes == b[i].bytes) throw new AssertionError("shared row");
        }
        a[1].bytes[0] = 99;
        if (b[1].bytes[0] != 1 || a[257].bytes[0] != 1) throw new AssertionError("aliased bytes");
    }
}
"#,
    )
    .unwrap();
    let result = Command::new("javac")
        .arg("-cp")
        .arg(std::env::join_paths([&jar, &directory]).unwrap())
        .arg(directory.join("Run.java"))
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    let runtime = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("runtime/build/libs/runtime-0.1.0.jar");
    let result = Command::new("java")
        .args(["-Xverify:all", "--class-path"])
        .arg(std::env::join_paths([&jar, &directory, &runtime]).unwrap())
        .arg("Run")
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "{}\n{}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr)
    );
    fs::remove_dir_all(directory).unwrap();
}

#[test]
fn incompatible_rows_do_not_partially_emit_a_resource() {
    let mut cp = InternedConstantPool::default();
    let owner = cp.add_class("Data").unwrap();
    cp.set_resource_anchor(owner);
    let mut rows = (0..100).map(row).collect::<Vec<_>>();
    let C::Instance { class_name, .. } = rows.last_mut().unwrap() else {
        unreachable!()
    };
    *class_name = "Other".into();
    let before = cp.len();
    let mut methods = vec![];
    assert!(
        factory(
            &mut cp,
            "Data",
            &T::Class("Row".into()),
            &rows,
            &mut methods,
            &mut 0,
            None
        )
        .unwrap()
        .is_none()
    );
    assert!(methods.is_empty() && cp.take_resources().is_empty());
    assert_eq!(cp.len(), before);
}
