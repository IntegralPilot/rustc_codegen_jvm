use super::*;
use jvm_compiler_core::{classfile::*, scalar::BinaryOp};
use std::{fs, process::Command};

fn padding(b: &mut Builder<'_>, mut value: ValueId, int: TypeId) -> ValueId {
    for _ in 0..MAX_WORK * 2 + 10 {
        value = b.emit(Op::Opaque(value), Some(int)).unwrap();
    }
    value
}
fn function(name: &str, ir: Body, types: &Arc<Types>, debug: Option<DebugInfo>) -> Function {
    Function {
        name: name.into(),
        owner_class: None,
        signature: super::super::Signature {
            params: ir.blocks[ir.entry.index()]
                .params
                .iter()
                .enumerate()
                .map(|(i, &v)| {
                    (
                        format!("arg{i}"),
                        super::super::construct::source_type(types, ir.value_type(v)),
                    )
                })
                .collect(),
            ret: Box::new(super::super::construct::source_type(types, ir.return_type)),
            is_static: true,
        },
        body: Arc::new(SsaBody {
            ir,
            types: Arc::clone(types),
            lines: None,
            source_file: None,
            constants: vec![],
            debug,
        }),
        debug_variables: vec![],
    }
}

#[test]
fn outlined_loops_exceptions_and_zero_sized_storage_execute() {
    let mut types = Types::default();
    let unit = types.intern(Type::Unit);
    let int = types.scalar(ScalarType::I32);
    let boolean = types.scalar(ScalarType::Bool);
    let object = types.symbol("java/lang/Object");
    let object = types.intern(Type::Class(object));
    let throwable = types.symbol("java/lang/Throwable");
    let throwable = types.intern(Type::Class(throwable));
    types.intern(Type::Pointer(object));
    let types = Arc::new(types);
    let mut functions = Vec::new();

    let mut b = Builder::new(&types, int);
    let limit = b.parameter(b.current(), int);
    let header = b.create_block();
    let body = b.create_block();
    let done = b.create_block();
    let count = b.parameter(header, int);
    let zero = integer(&mut b, int, 0);
    b.jump(header, vec![zero]);
    b.switch_to(header);
    let condition = b
        .emit(
            Op::Binary {
                op: BinaryOp::Lt,
                left: count,
                right: limit,
            },
            Some(boolean),
        )
        .unwrap();
    b.branch(condition, body, done);
    b.switch_to(body);
    let next = padding(&mut b, count, int);
    let one = integer(&mut b, int, 1);
    let next = b
        .emit(
            Op::Binary {
                op: BinaryOp::Add,
                left: next,
                right: one,
            },
            Some(int),
        )
        .unwrap();
    b.jump(header, vec![next]);
    b.switch_to(done);
    b.terminate(Terminator::Return(Some(count)));
    let debug = DebugInfo {
        locals: vec![DebugLocal::Value(unit)],
        variables: vec![DebugVariable {
            name: "unit".into(),
            local: 0,
        }],
        ..Default::default()
    };
    functions.push(function("cycle", b.finish().unwrap(), &types, Some(debug)));

    let mut b = Builder::new(&types, unit);
    let exception = b.parameter(b.current(), throwable);
    let handler = b.create_block();
    let edge = b.edge(handler, vec![]);
    b.terminate(Terminator::Throw {
        value: exception,
        unwind: Some(edge),
    });
    b.switch_to(handler);
    let one = integer(&mut b, int, 1);
    padding(&mut b, one, int);
    b.terminate(Terminator::Rethrow);
    functions.push(function("rethrow", b.finish().unwrap(), &types, None));

    let mut b = Builder::new(&types, object);
    let value = b.parameter(b.current(), object);
    let slot = SlotId::new(0);
    b.body.slots.push(StorageSlot {
        ty: object,
        size: 0,
        alignment: 1,
        codec: None,
    });
    b.emit(Op::StoreSlot { slot, value }, None);
    let one = integer(&mut b, int, 1);
    padding(&mut b, one, int);
    let value = b.emit(Op::LoadSlot(slot), Some(object)).unwrap();
    b.terminate(Terminator::Return(Some(value)));
    let debug = DebugInfo {
        locals: vec![DebugLocal::Storage(slot)],
        variables: vec![DebugVariable {
            name: "cell".into(),
            local: 0,
        }],
        ..Default::default()
    };
    functions.push(function(
        "storage",
        b.finish().unwrap(),
        &types,
        Some(debug),
    ));

    let mut cp = constant_pool::InternedConstantPool::default();
    let this_class = cp.add_class("Outlined").unwrap();
    let super_class = cp.add_class("java/lang/Object").unwrap();
    let mut methods = Vec::new();
    for function in functions {
        assert!(needed(&function));
        let parts = split(function, "Outlined", false).unwrap();
        assert!(parts.len() >= 3);
        for function in parts {
            let code = crate::lower2::select::compile(
                &function.body,
                &mut cp,
                &mut vec![],
                crate::lower2::DebugInfoOptions {
                    line_numbers: true,
                    local_variables: true,
                },
                false,
            )
            .unwrap();
            methods.push(Method {
                access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
                name_index: cp.add_utf8(&function.name).unwrap(),
                descriptor_index: cp.add_utf8(function.signature.to_string()).unwrap(),
                attributes: vec![attributes::Attribute::Code {
                    name_index: cp.add_utf8("Code").unwrap(),
                    max_stack: code.max_stack,
                    max_locals: code.max_locals,
                    code: code.instructions,
                    exception_table: code.exceptions,
                    attributes: code.attributes,
                }],
            });
        }
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
    let directory = std::env::temp_dir().join(format!("rcj-outline-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();
    fs::write(directory.join("Outlined.class"), bytes).unwrap();
    fs::write(directory.join("Run.java"), r#"
public class Run {
    public static void main(String[] args) {
        if (Outlined.cycle(2000) != 2000 || Outlined.cycle(0) != 0) throw new AssertionError("loop");
        Throwable expected = new IllegalStateException("identity");
        try { Outlined.rethrow(expected); throw new AssertionError("no exception"); }
        catch (Throwable actual) { if (actual != expected) throw new AssertionError("exception identity", actual); }
        Object cell = new Object();
        if (Outlined.storage(cell) != cell) throw new AssertionError("zero-sized cell");
    }
}
"#).unwrap();
    let runtime = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("runtime/build/libs/runtime-0.1.0.jar");
    assert!(
        runtime.is_file(),
        "build the JVM runtime before execution tests: {}",
        runtime.display()
    );
    let result = Command::new("java")
        .args(["-Xverify:all", "-Xss256k", "--class-path"])
        .arg(format!("{}:{}", directory.display(), runtime.display()))
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
