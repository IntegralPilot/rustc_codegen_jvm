use super::*;
use crate::classfile::{ClassAccessFlags, ClassFile, Method, MethodAccessFlags, Version};
use jvm::attributes::Attribute;
use std::{fs, process::Command, sync::Arc};

#[test]
fn shared_type_vocabulary_preserves_ids_and_structural_identity() {
    let mut base = Types::default();
    let int = base.scalar(ScalarType::I32);
    let symbol = base.symbol("java/lang/Object");
    let object = base.intern(Type::Class(symbol));
    let mut extended = Types::with_base(Arc::new(base.clone()));
    assert_eq!(extended.scalar(ScalarType::I32), int);
    assert_eq!(extended.symbol("java/lang/Object"), symbol);
    assert_eq!(extended.get(object), Some(Type::Class(symbol)));
    let pointer = extended.intern(Type::Pointer(object));
    assert_eq!(base.find(Type::Pointer(object)), None);
    assert_eq!(base.intern(Type::Pointer(object)), pointer);
    assert_eq!(base, extended);
    use std::hash::{Hash, Hasher};
    let mut a = std::collections::hash_map::DefaultHasher::new();
    let mut b = a.clone();
    base.hash(&mut a);
    extended.hash(&mut b);
    assert_eq!(a.finish(), b.finish());
}

#[test]
fn jvm_executes_constructors_fields_and_virtual_interface_calls() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let long = types.scalar(ScalarType::I64);
    let unit = types.intern(Type::Unit);
    let pair_symbol = types.symbol("AggregateRun$Pair");
    let pair = types.intern(Type::Class(pair_symbol));
    let supplier_symbol = types.symbol("java/util/function/LongSupplier");
    let supplier = types.intern(Type::Interface(supplier_symbol));
    let mut b = Builder::new(&types, long);
    let input = b.parameter(b.current(), int);
    let big = b.constant(long, Scalar::integer(ScalarType::I64, 1 << 40).unwrap());
    let constructor = b.method(MethodRef {
        owner: "AggregateRun$Pair".into(),
        name: "<init>".into(),
        params: vec![int, long],
        returns: unit,
        interface: false,
    });
    let args = b.args([input, big]);
    let instance = b
        .emit(
            Op::Call {
                method: constructor,
                kind: CallKind::Constructor,
                args,
            },
            Some(pair),
        )
        .unwrap();
    let field = b.field(FieldRef {
        owner: pair,
        name: "narrow".into(),
        ty: int,
        is_static: false,
        relative_pointer: false,
    });
    let old = b
        .emit(
            Op::GetField {
                object: instance,
                field,
            },
            Some(int),
        )
        .unwrap();
    let one = b.constant(int, Scalar::integer(ScalarType::I32, 1).unwrap());
    let next = b
        .emit(
            Op::Binary {
                op: BinaryOp::Add,
                left: old,
                right: one,
            },
            Some(int),
        )
        .unwrap();
    b.emit(
        Op::SetField {
            object: instance,
            field,
            value: next,
        },
        None,
    );
    let virtual_method = b.method(MethodRef {
        owner: "AggregateRun$Pair".into(),
        name: "getAsLong".into(),
        params: vec![],
        returns: long,
        interface: false,
    });
    let args = b.args([instance]);
    let first = b
        .emit(
            Op::Call {
                method: virtual_method,
                kind: CallKind::Virtual,
                args,
            },
            Some(long),
        )
        .unwrap();
    let interface = b.emit(Op::Cast(instance), Some(supplier)).unwrap();
    let interface_method = b.method(MethodRef {
        owner: "java/util/function/LongSupplier".into(),
        name: "getAsLong".into(),
        params: vec![],
        returns: long,
        interface: true,
    });
    let args = b.args([interface]);
    let second = b
        .emit(
            Op::Call {
                method: interface_method,
                kind: CallKind::Interface,
                args,
            },
            Some(long),
        )
        .unwrap();
    let result = b
        .emit(
            Op::Binary {
                op: BinaryOp::Add,
                left: first,
                right: second,
            },
            Some(long),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    let body = b.finish().unwrap();
    let mut invalid = body.clone();
    invalid.fields[0].owner = supplier;
    assert!(verify(&invalid, &types).is_err());
    let mut cp = InternedConstantPool::default();
    let this_class = cp.add_class("AggregateSsa").unwrap();
    let super_class = cp.add_class("java/lang/Object").unwrap();
    let code = compile(&body, &types, &mut cp).unwrap();
    let method = Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("run").unwrap(),
        descriptor_index: cp.add_utf8("(I)J").unwrap(),
        attributes: vec![Attribute::Code {
            name_index: cp.add_utf8("Code").unwrap(),
            max_stack: code.max_stack,
            max_locals: code.max_locals,
            code: code.instructions,
            exception_table: code.exceptions,
            attributes: code.attributes,
        }],
    };
    let class = ClassFile {
        code_source_url: None,
        version: Version::Java8 { minor: 0 },
        constant_pool: cp.into_inner(),
        access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
        this_class,
        super_class,
        interfaces: vec![],
        fields: vec![],
        methods: vec![method],
        attributes: vec![],
    };
    let mut bytes = Vec::new();
    class.to_bytes(&mut bytes).unwrap();
    let directory = std::env::temp_dir().join(format!("rcj-ssa-objects-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();
    fs::write(directory.join("AggregateSsa.class"), bytes).unwrap();
    fs::write(
        directory.join("AggregateRun.java"),
        r#"
public class AggregateRun {
    public static class Pair implements java.util.function.LongSupplier {
        public int narrow;
        public long wide;
        public Pair(int narrow, long wide) { this.narrow = narrow; this.wide = wide; }
        public long getAsLong() { return narrow + wide; }
    }
    public static void main(String[] args) {
        for (int input : new int[] { 0, -1, 17, Integer.MIN_VALUE, Integer.MAX_VALUE }) {
            long expected = 2 * ((long)(input + 1) + (1L << 40));
            if (AggregateSsa.run(input) != expected) throw new AssertionError(input);
        }
    }
}
"#,
    )
    .unwrap();
    let compile = Command::new("javac")
        .args(["-cp", ".", "AggregateRun.java"])
        .current_dir(&directory)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new("java")
        .args(["-Xverify:all", "-cp", ".", "AggregateRun"])
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
fn field_projection_verifies_both_ends_of_the_pointer_view() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let long = types.scalar(ScalarType::I64);
    let symbol = types.symbol("Pair");
    let pair = types.intern(Type::Class(symbol));
    let pair_pointer = types.intern(Type::Pointer(pair));
    let int_pointer = types.intern(Type::Pointer(int));
    let long_pointer = types.intern(Type::Pointer(long));
    let mut b = Builder::new(&types, int_pointer);
    let base = b.parameter(b.current(), pair_pointer);
    let field = b.field(FieldRef {
        owner: pair,
        name: "value".into(),
        ty: int,
        is_static: false,
        relative_pointer: false,
    });
    let projection = b.projection(PointerProjection {
        field,
        offset: 8,
        size: 4,
        codec: None,
    });
    let pointer = b
        .emit(Op::Project { base, projection }, Some(int_pointer))
        .unwrap();
    b.terminate(Terminator::Return(Some(pointer)));
    let mut body = b.finish().unwrap();
    // These mistakes would otherwise become incorrect runtime memory views.
    body.fields[field.index()].ty = long;
    assert!(
        verify(&body, &types)
            .unwrap_err()
            .0
            .contains("projection result")
    );
    body.values[pointer.index()].ty = long_pointer;
    body.return_type = long_pointer;
    verify(&body, &types).unwrap();
    body.fields[field.index()].is_static = true;
    assert!(
        verify(&body, &types)
            .unwrap_err()
            .0
            .contains("projection owner")
    );
    body.fields[field.index()].is_static = false;
    body.projections[projection.index()].offset = u64::MAX;
    assert!(
        verify(&body, &types)
            .unwrap_err()
            .0
            .contains("address space")
    );
}
