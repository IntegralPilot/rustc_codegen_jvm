use super::*;
use crate::classfile::{ClassAccessFlags, ClassFile, Method, MethodAccessFlags, Version};
use jvm::attributes::Attribute;
use std::{fs, process::Command};

#[test]
fn executes_full_width_view_metadata_and_typed_data_extraction() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I64);
    let usize_ty = types.scalar(ScalarType::U64);
    let byte = types.scalar(ScalarType::U8);
    let pointer = types.intern(Type::Pointer(int));
    let byte_pointer = types.intern(Type::Pointer(byte));
    let slice = types.intern(Type::Slice(int));
    let string = types.intern(Type::Str);
    let mut cp = InternedConstantPool::default();
    let this_class = cp.add_class("ViewSsa").unwrap();
    let super_class = cp.add_class("java/lang/Object").unwrap();
    let mut methods = Vec::new();
    for (name, pointer, view, size) in [
        ("slice", pointer, slice, 8),
        ("string", byte_pointer, string, 1),
    ] {
        let mut b = Builder::new(&types, usize_ty);
        let data = b.parameter(b.current(), pointer);
        let length = b.parameter(b.current(), usize_ty);
        let view_ty = view;
        let view = b.emit(Op::View { data, length }, Some(view)).unwrap();
        b.emit(
            Op::ViewData {
                view,
                size,
                codec: None,
            },
            Some(pointer),
        );
        let result = b.emit(Op::Length(view), Some(usize_ty)).unwrap();
        b.terminate(Terminator::Return(Some(result)));
        let body = b.finish().unwrap();
        let mut invalid = body.clone();
        invalid.instructions[1].op = Op::ViewData {
            view,
            size: u32::MAX,
            codec: None,
        };
        assert!(verify(&invalid, &types).is_err());
        let mut invalid = body.clone();
        invalid.instructions[1].op = Op::Length(data);
        assert!(verify(&invalid, &types).is_err());
        let code = compile(&body, &types, &mut cp).unwrap();
        methods.push(Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index: cp.add_utf8(name).unwrap(),
            descriptor_index: cp.add_utf8("(Lorg/rustlang/runtime/Pointer;J)J").unwrap(),
            attributes: vec![Attribute::Code {
                name_index: cp.add_utf8("Code").unwrap(),
                max_stack: code.max_stack,
                max_locals: code.max_locals,
                code: code.instructions,
                exception_table: code.exceptions,
                attributes: code.attributes,
            }],
        });
        let mut b = Builder::new(&types, usize_ty);
        let input = b.parameter(b.current(), view_ty);
        let result = b.emit(Op::Length(input), Some(usize_ty)).unwrap();
        b.terminate(Terminator::Return(Some(result)));
        let body = b.finish().unwrap();
        let code = compile(&body, &types, &mut cp).unwrap();
        let mut signature = String::from("(");
        representation::descriptor(&types, view_ty, &mut signature).unwrap();
        signature.push_str(")J");
        methods.push(Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index: cp.add_utf8(format!("read_{name}")).unwrap(),
            descriptor_index: cp.add_utf8(signature).unwrap(),
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
    let directory = std::env::temp_dir().join(format!("rcj-ssa-views-{}", std::process::id()));
    let runtime = directory.join("org/rustlang/runtime");
    fs::create_dir_all(&runtime).unwrap();
    fs::write(directory.join("ViewSsa.class"), bytes).unwrap();
    // Minimal ABI carriers isolate instruction selection. The backend's Rust
    // fixture exercises the actual runtime's offset/allocation/storage behavior.
    fs::write(
        runtime.join("SliceView.java"),
        r#"
package org.rustlang.runtime;
public class SliceView {
    public final Object array; public final int offset; public final long rustLength;
    public SliceView(Object array, int offset, long length) {
        this.array = array; this.offset = offset; this.rustLength = length;
    }
}"#,
    )
    .unwrap();
    fs::write(
        runtime.join("Utf8View.java"),
        r#"
package org.rustlang.runtime;
public class Utf8View extends SliceView {
    public Utf8View(Object data, int offset, long length) { super(data, offset, length); }
}"#,
    )
    .unwrap();
    fs::write(
        runtime.join("Pointer.java"),
        r#"
package org.rustlang.runtime;
public class Pointer {
    public static Pointer extracted;
    public static Pointer fromSlice(Object value, int size, String codec) {
        SliceView view = (SliceView)value;
        if (view.offset != 0 || codec != null || size != (view instanceof Utf8View ? 1 : 8))
            throw new AssertionError("wrong extraction arguments");
        return extracted = (Pointer)view.array;
    }
}"#,
    )
    .unwrap();
    fs::write(directory.join("ViewRun.java"), r#"
import org.rustlang.runtime.*;
public class ViewRun {
    public static void main(String[] args) {
        Pointer pointer = new Pointer();
        for (long length : new long[] { 0, 1, Integer.MAX_VALUE, (1L << 40) + 23, Long.MAX_VALUE }) {
            if (ViewSsa.slice(pointer, length) != length || Pointer.extracted != pointer)
                throw new AssertionError("slice " + length);
            if (ViewSsa.string(pointer, length) != length || Pointer.extracted != pointer)
                throw new AssertionError("string " + length);
            if (ViewSsa.read_slice(new SliceView(pointer, 3, length)) != length
                || ViewSsa.read_string(new Utf8View(pointer, 2, length)) != length)
                throw new AssertionError("metadata " + length);
        }
    }
}"#).unwrap();
    let compiled = Command::new("javac")
        .args(["-cp", ".", "ViewRun.java"])
        .current_dir(&directory)
        .output()
        .unwrap();
    assert!(
        compiled.status.success(),
        "{}",
        String::from_utf8_lossy(&compiled.stderr)
    );
    let run = Command::new("java")
        .args(["-Xverify:all", "-cp", ".", "ViewRun"])
        .current_dir(&directory)
        .output()
        .unwrap();
    assert!(
        run.status.success(),
        "{}",
        String::from_utf8_lossy(&run.stderr)
    );
    fs::remove_dir_all(directory).unwrap();
}

#[test]
fn length_of_new_view_needs_no_carrier_allocation() {
    let mut types = Types::default();
    let byte = types.scalar(ScalarType::U8);
    let usize_ty = types.scalar(ScalarType::U64);
    let pointer = types.intern(Type::Pointer(byte));
    let slice = types.intern(Type::Slice(byte));
    let mut b = Builder::new(&types, usize_ty);
    let data = b.parameter(b.current(), pointer);
    let length = b.parameter(b.current(), usize_ty);
    let view = b.emit(Op::View { data, length }, Some(slice)).unwrap();
    let result = b.emit(Op::Length(view), Some(usize_ty)).unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let body = b.finish().unwrap();
    let code = compile(&body, &types, &mut InternedConstantPool::default()).unwrap();
    assert!(!code.instructions.iter().any(|inst| matches!(
        inst,
        Instruction::New(_) | Instruction::Invokestatic(_) | Instruction::Invokevirtual(_)
    )));
}
