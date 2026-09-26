use super::*;
use std::{fs, process::Command};

#[test]
fn packed_byte_constants_preserve_every_byte_and_array_ownership() {
    let length = oomir::PACKED_BYTE_CHUNK * 3 + 257;
    let bytes: Vec<_> = (0..length).map(|i| i as u8).collect();
    let values: Vec<_> = bytes.iter().map(|b| oomir::Constant::U8(*b)).collect();
    let signed: Vec<_> = bytes
        .iter()
        .map(|b| oomir::Constant::I8(*b as i8))
        .collect();
    let mut cp = InternedConstantPool::default();
    let this_class = cp.add_class("PackedConstants").unwrap();
    let super_class = cp.add_class("java/lang/Object").unwrap();
    let mut methods = Vec::new();
    let factory = factories::create_shared_array_factory(
        &mut cp,
        "PackedConstants",
        &oomir::Type::U8,
        &values,
        &mut methods,
        &mut 0,
    )
    .unwrap();
    for name in ["array", "signed", "bytes", "shared"] {
        let mut code = Vec::new();
        match name {
            "array" => arrays::load_array(&mut code, &mut cp, &oomir::Type::U8, &values),
            "signed" => arrays::load_array(&mut code, &mut cp, &oomir::Type::I8, &signed),
            "bytes" => arrays::load_bytes(&mut code, &mut cp, &bytes),
            _ => load_constant(&mut code, &mut cp, &factory),
        }
        .unwrap();
        code.push(Instruction::Areturn);
        let max_stack = code.max_stack(&cp).unwrap();
        methods.push(jvm::Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index: cp.add_utf8(name).unwrap(),
            descriptor_index: cp.add_utf8("()[B").unwrap(),
            attributes: vec![Attribute::Code {
                name_index: cp.add_utf8("Code").unwrap(),
                max_stack,
                max_locals: 0,
                code,
                exception_table: vec![],
                attributes: vec![],
            }],
        });
    }
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
    let mut encoded = Vec::new();
    jvm::encode::class_file(&class, &mut encoded).unwrap();
    let directory = std::env::temp_dir().join(format!("rcj-packed-bytes-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();
    fs::write(directory.join("PackedConstants.class"), encoded).unwrap();
    fs::write(directory.join("Run.java"), format!(r#"
public class Run {{
    public static void main(String[] args) {{
        byte[][] arrays = {{ PackedConstants.array(), PackedConstants.signed(), PackedConstants.bytes(), PackedConstants.shared() }};
        for (byte[] bytes : arrays) {{
            if (bytes.length != {length}) throw new AssertionError("length");
            for (int index = 0; index < bytes.length; index++)
                if (bytes[index] != (byte) index) throw new AssertionError("byte " + index);
        }}
        arrays[0][0] = 99;
        if (PackedConstants.array()[0] != 0) throw new AssertionError("array ownership");
        if (PackedConstants.shared() != arrays[3]) throw new AssertionError("shared identity");
    }}
}}
"#)).unwrap();
    let runtime = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("runtime/build/libs/runtime-0.1.0.jar");
    let result = Command::new("java")
        .arg("-Xverify:all")
        .arg("--class-path")
        .arg(std::env::join_paths([&directory, &runtime]).unwrap())
        .arg(directory.join("Run.java"))
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
