use super::*;
use crate::classfile::{ClassAccessFlags, ClassFile, Method, MethodAccessFlags, Version};
use jvm::attributes::Attribute;
use std::{fs, process::Command};

#[test]
fn jvm_preserves_throwable_identity_through_catch_and_rethrow() {
    let mut types = Types::default();
    let symbol = types.symbol("java/lang/Throwable");
    let throwable = types.intern(Type::Class(symbol));
    let int = types.scalar(ScalarType::I32);
    let mut cp = InternedConstantPool::default();
    let this_class = cp.add_class("UnwindSsa").unwrap();
    let super_class = cp.add_class("java/lang/Object").unwrap();
    let mut methods = Vec::new();
    for rethrow in [false, true] {
        let mut b = Builder::new(&types, throwable);
        let original = b.parameter(b.current(), throwable);
        let handler = b.create_block();
        let before = b.current();
        // Protected scalar work must not manufacture empty exception ranges.
        let one = b.constant(int, Scalar::integer(ScalarType::I32, 1).unwrap());
        b.invoke(Op::Cast(one), Some(int), handler);
        assert_eq!(b.current(), before);
        let unwind = b.edge(handler, Vec::new());
        b.terminate(Terminator::Throw {
            value: original,
            unwind: Some(unwind),
        });
        b.switch_to(handler);
        let caught = b.emit(Op::Exception, Some(throwable)).unwrap();
        b.terminate(if rethrow {
            Terminator::Throw {
                value: caught,
                unwind: None,
            }
        } else {
            Terminator::Return(Some(caught))
        });
        let body = b.finish().unwrap();
        let code = compile(&body, &types, &mut cp).unwrap();
        assert_eq!(code.exceptions.len(), 1);
        assert!(code.exceptions[0].range_pc.start < code.exceptions[0].range_pc.end);
        methods.push(Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index: cp
                .add_utf8(if rethrow { "rethrow" } else { "capture" })
                .unwrap(),
            descriptor_index: cp
                .add_utf8("(Ljava/lang/Throwable;)Ljava/lang/Throwable;")
                .unwrap(),
            attributes: vec![Attribute::Code {
                name_index: cp.add_utf8("Code").unwrap(),
                max_stack: code.max_stack,
                max_locals: code.max_locals,
                code: code.instructions,
                exception_table: code.exceptions,
                attributes: code.attributes,
            }],
        });
    }
    let class = ClassFile {
        code_source_url: None,
        version: Version::Java8 { minor: 0 },
        constant_pool: cp.into_inner(),
        access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
        this_class,
        super_class,
        interfaces: vec![],
        fields: vec![],
        methods,
        attributes: vec![],
    };
    let mut bytes = Vec::new();
    class.to_bytes(&mut bytes).unwrap();
    let directory = std::env::temp_dir().join(format!("rcj-ssa-unwind-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();
    fs::write(directory.join("UnwindSsa.class"), bytes).unwrap();
    fs::write(
        directory.join("UnwindRun.java"),
        r#"
public class UnwindRun {
    public static void main(String[] args) throws Throwable {
        Throwable original = new IllegalArgumentException("identity");
        if (UnwindSsa.capture(original) != original) throw new AssertionError("catch");
        try { UnwindSsa.rethrow(original); throw new AssertionError("returned"); }
        catch (Throwable caught) { if (caught != original) throw new AssertionError("rethrow"); }
    }
}"#,
    )
    .unwrap();
    for (program, args) in [
        ("javac", vec!["-cp", ".", "UnwindRun.java"]),
        ("java", vec!["-Xverify:all", "-cp", ".", "UnwindRun"]),
    ] {
        let output = Command::new(program)
            .args(args)
            .current_dir(&directory)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    fs::remove_dir_all(directory).unwrap();
}
