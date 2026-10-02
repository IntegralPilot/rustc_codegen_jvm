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
    cp.set_resource_anchor(this_class);
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
    let nested = factories::create_constant_factory(
        &mut cp,
        "PackedConstants",
        &oomir::Constant::SliceRef {
            backing: Box::new(oomir::Constant::InternedPointer {
                identity: "PackedConstants::nested".into(),
                value: Box::new(oomir::Constant::Array(
                    Box::new(oomir::Type::U8),
                    values.clone(),
                )),
                array_backed: true,
                allocation_size: length as u64,
                offset: 0,
                view_size: 1,
                alignment: 1,
                view_codec: Box::new(oomir::Constant::Null(oomir::Type::java_string())),
                pointee: Box::new(oomir::Type::U8),
            }),
            element_type: Box::new(oomir::Type::U8),
            offset: 1,
            length: (length - 1) as u64,
        },
        &mut methods,
        &mut 0,
    )
    .unwrap();
    let pointer = factories::create_shared_pointer_factory(
        &mut cp,
        "PackedConstants",
        &oomir::Constant::InternedPointer {
            identity: "PackedConstants::pointer".into(),
            value: Box::new(oomir::Constant::Array(
                Box::new(oomir::Type::U8),
                values.clone(),
            )),
            array_backed: false,
            allocation_size: length as u64,
            offset: 0,
            view_size: length as u64,
            alignment: 1,
            view_codec: Box::new(oomir::Constant::Null(oomir::Type::java_string())),
            pointee: Box::new(oomir::Type::Array(Box::new(oomir::Type::U8))),
        },
        &mut methods,
        &mut 10,
    )
    .unwrap();
    // Separate fragments start numbering at zero before they are merged into
    // one owner. Unrelated factories must remain distinct, including factories
    // whose return descriptor is the same Pointer type.
    let scalar = factories::create_shared_pointer_factory(
        &mut cp,
        "PackedConstants",
        &oomir::Constant::InternedPointer {
            identity: "PackedConstants::scalar".into(),
            value: Box::new(oomir::Constant::U64(17)),
            array_backed: false,
            allocation_size: 8,
            offset: 0,
            view_size: 8,
            alignment: 8,
            view_codec: Box::new(oomir::Constant::Null(oomir::Type::java_string())),
            pointee: Box::new(oomir::Type::U64),
        },
        &mut methods,
        &mut 0,
    )
    .unwrap();
    let another = factories::create_shared_pointer_factory(
        &mut cp,
        "PackedConstants",
        &oomir::Constant::InternedPointer {
            identity: "PackedConstants::another".into(),
            // Nested long arguments exercise category-2 stack accounting:
            // fromAddress(JJ) followed by constantCell(Object,J,String,J).
            value: Box::new(oomir::Constant::PointerAddress {
                address: 2,
                view_size: 0,
                pointee: Box::new(oomir::Type::Unit),
            }),
            array_backed: false,
            allocation_size: 8,
            offset: 0,
            view_size: 8,
            alignment: 8,
            view_codec: Box::new(oomir::Constant::Null(oomir::Type::java_string())),
            pointee: Box::new(oomir::Type::U64),
        },
        &mut methods,
        &mut 0,
    )
    .unwrap();
    for name in [
        "array", "signed", "bytes", "packed", "shared", "pointer", "scalar", "another", "nested",
    ] {
        let mut code = Vec::new();
        match name {
            "array" => arrays::load_array(&mut code, &mut cp, &oomir::Type::U8, &values),
            "signed" => arrays::load_array(&mut code, &mut cp, &oomir::Type::I8, &signed),
            "bytes" => arrays::load_bytes(&mut code, &mut cp, &bytes),
            "packed" => {
                let size = oomir::PACKED_BYTE_CHUNK * 2 + 1;
                arrays::append_empty_array(&mut code, &mut cp, &oomir::Type::U8, size).unwrap();
                arrays::fill_packed_bytes(&mut code, &mut cp, 0, size, |_| 255)
            }
            "pointer" => load_constant(&mut code, &mut cp, &pointer),
            "scalar" => load_constant(&mut code, &mut cp, &scalar),
            "another" => load_constant(&mut code, &mut cp, &another),
            "nested" => load_constant(&mut code, &mut cp, &nested),
            _ => load_constant(&mut code, &mut cp, &factory),
        }
        .unwrap();
        code.push(Instruction::Areturn);
        let max_stack = code.max_stack(&cp).unwrap();
        methods.push(jvm::Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index: cp.add_utf8(name).unwrap(),
            descriptor_index: cp
                .add_utf8(if matches!(name, "pointer" | "scalar" | "another") {
                    "()Lorg/rustlang/runtime/Pointer;"
                } else if name == "nested" {
                    "()Lorg/rustlang/runtime/SliceView;"
                } else {
                    "()[B"
                })
                .unwrap(),
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
    for (name, value) in [
        ("shortText", "\0😃é".repeat(100)),
        ("longText", "\0😃é".repeat(15000)),
    ] {
        let mut code = Vec::new();
        load_constant(&mut code, &mut cp, &oomir::Constant::Str(value)).unwrap();
        code.push(Instruction::Areturn);
        factories::add_constant_helper_method(
            &mut cp,
            &mut methods,
            name,
            "()Lorg/rustlang/runtime/Utf8View;",
            0,
            code,
        )
        .unwrap();
        methods.last_mut().unwrap().access_flags =
            MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC;
    }
    let resources = cp.take_resources();
    assert!(!resources.is_empty());
    assert!(
        cp.len() < 256,
        "binary payload leaked into the constant pool"
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
    let directory = std::env::temp_dir().join(format!("rcj-packed-bytes-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();
    let jar = super::test_support::link(&directory, &class, resources);
    fs::write(directory.join("Run.java"), format!(r#"
public class Run {{
    private static volatile Object sink;
    public static void main(String[] args) {{
        String piece = "\0\uD83D\uDE03é";
        if (!org.rustlang.runtime.Utf8View.toJavaString(PackedConstants.shortText()).equals(piece.repeat(100)))
            throw new AssertionError("short text");
        if (!org.rustlang.runtime.Utf8View.toJavaString(PackedConstants.longText()).equals(piece.repeat(15000)))
            throw new AssertionError("long text");
        if (PackedConstants.longText().array != PackedConstants.longText().array)
            throw new AssertionError("long text identity");
        byte[] packed = PackedConstants.packed();
        if (packed.length != {packed_length}) throw new AssertionError("packed length");
        for (byte b : packed) if (b != -1) throw new AssertionError("packed byte");
        byte[][] arrays = {{ PackedConstants.array(), PackedConstants.signed(), PackedConstants.bytes(), PackedConstants.shared() }};
        for (byte[] bytes : arrays) {{
            if (bytes.length != {length}) throw new AssertionError("length");
            for (int index = 0; index < bytes.length; index++)
                if (bytes[index] != (byte) index) throw new AssertionError("byte " + index);
        }}
        arrays[0][0] = 99;
        if (PackedConstants.array()[0] != 0) throw new AssertionError("array ownership");
        if (PackedConstants.shared() != arrays[3]) throw new AssertionError("shared identity");
        org.rustlang.runtime.Pointer pointer = PackedConstants.pointer();
        byte[] backing = (byte[]) pointer.backingArray();
        if (!java.util.Arrays.equals(backing, arrays[3])) throw new AssertionError("pointer contents");
        org.rustlang.runtime.SliceView nested = PackedConstants.nested();
        if (nested.offset != 1 || nested.length != {length} - 1
                || !java.util.Arrays.equals((byte[]) ((org.rustlang.runtime.Pointer) nested.array).backingArray(), arrays[3]))
            throw new AssertionError("nested pointer contents");
        if (PackedConstants.nested().array != nested.array) throw new AssertionError("nested pointer identity");
        java.lang.management.ThreadMXBean bean = java.lang.management.ManagementFactory.getThreadMXBean();
        if (bean instanceof com.sun.management.ThreadMXBean) {{
            com.sun.management.ThreadMXBean allocations = (com.sun.management.ThreadMXBean) bean;
            if (allocations.isThreadAllocatedMemorySupported()) {{
                allocations.setThreadAllocatedMemoryEnabled(true);
                for (int i = 0; i < 32; i++) {{
                    sink = PackedConstants.pointer();
                    sink = PackedConstants.nested();
                }}
                long thread = Thread.currentThread().getId();
                long before = allocations.getThreadAllocatedBytes(thread);
                for (int i = 0; i < 128; i++) {{
                    sink = PackedConstants.pointer();
                    sink = PackedConstants.nested();
                }}
                long allocated = allocations.getThreadAllocatedBytes(thread) - before;
                if (allocated > 1024 * 1024) throw new AssertionError("reconstructed cached pointer: " + allocated);
            }}
        }}
        if (PackedConstants.pointer() != pointer) throw new AssertionError("pointer identity");
        if (PackedConstants.scalar().getI64() != 17) throw new AssertionError("ambiguous factory");
        if (((org.rustlang.runtime.Pointer) PackedConstants.another().getObject()).addr() != 2)
            throw new AssertionError("colliding factory");
    }}
}}
"#, packed_length = oomir::PACKED_BYTE_CHUNK * 2 + 1)).unwrap();
    let runtime = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("runtime/build/libs/runtime-0.1.0.jar");
    let result = Command::new("java")
        .arg("-Xverify:all")
        .arg("--class-path")
        .arg(std::env::join_paths([&jar, &directory, &runtime]).unwrap())
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
