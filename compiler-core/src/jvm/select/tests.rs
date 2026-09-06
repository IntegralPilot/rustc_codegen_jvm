use super::*;
use crate::classfile::attributes::Attribute;
use crate::classfile::{ClassAccessFlags, ClassFile, Method, MethodAccessFlags, Version};
use std::{fs, process::Command};

fn integer(b: &mut Builder<'_>, ty: TypeId, scalar_ty: ScalarType, n: i128) -> ValueId {
    b.constant(ty, Scalar::integer(scalar_ty, n as u128).unwrap())
}

fn binary_body(types: &Types, ty: TypeId, ret: TypeId, op: BinaryOp) -> Body {
    let mut b = Builder::new(types, ret);
    let a = b.parameter(b.current(), ty);
    let c = b.parameter(b.current(), ty);
    let result = b
        .emit(
            Op::Binary {
                op,
                left: a,
                right: c,
            },
            Some(ret),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    b.finish().unwrap()
}

fn swap_loop(
    types: &Types,
    ty: TypeId,
    scalar_ty: ScalarType,
    i32_ty: TypeId,
    boolean: TypeId,
) -> Body {
    let mut b = Builder::new(types, ty);
    let a = b.parameter(b.current(), ty);
    let c = b.parameter(b.current(), ty);
    let count = b.parameter(b.current(), i32_ty);
    let av = b.variable(ty);
    let cv = b.variable(ty);
    let nv = b.variable(i32_ty);
    b.define(av, a);
    b.define(cv, c);
    let zero = integer(&mut b, i32_ty, ScalarType::I32, 0);
    let one = integer(&mut b, i32_ty, ScalarType::I32, 1);
    b.define(nv, zero);
    let header = b.create_block();
    let update = b.create_block();
    let exit = b.create_block();
    b.jump(header, vec![]);
    b.switch_to(header);
    let n = b.read(nv);
    let cond = b
        .emit(
            Op::Binary {
                op: BinaryOp::Lt,
                left: n,
                right: count,
            },
            Some(boolean),
        )
        .unwrap();
    b.branch(cond, update, exit);
    b.switch_to(update);
    let old_a = b.read(av);
    let old_c = b.read(cv);
    let n = b.read(nv);
    b.define(av, old_c);
    b.define(cv, old_a);
    let next = b
        .emit(
            Op::Binary {
                op: BinaryOp::Add,
                left: n,
                right: one,
            },
            Some(i32_ty),
        )
        .unwrap();
    b.define(nv, next);
    b.jump(header, vec![]);
    b.switch_to(exit);
    let result = b.read(av);
    // Keep both loop-carried values observable so both copy cycles survive DCE.
    let other = b.read(cv);
    let multiplier = integer(&mut b, ty, scalar_ty, 100);
    let first = b
        .emit(
            Op::Binary {
                op: BinaryOp::Mul,
                left: result,
                right: multiplier,
            },
            Some(ty),
        )
        .unwrap();
    let result = b
        .emit(
            Op::Binary {
                op: BinaryOp::Add,
                left: first,
                right: other,
            },
            Some(ty),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    b.finish().unwrap()
}

fn throw_points(types: &Types, ty: TypeId) -> Body {
    let mut b = Builder::new(types, ty);
    let d1 = b.parameter(b.current(), ty);
    let d2 = b.parameter(b.current(), ty);
    let x = b.variable(ty);
    let seven = integer(&mut b, ty, ScalarType::I32, 7);
    b.define(x, seven);
    let handler = b.create_block();
    b.invoke(
        Op::Binary {
            op: BinaryOp::Div,
            left: seven,
            right: d1,
        },
        Some(ty),
        handler,
    );
    let thirteen = integer(&mut b, ty, ScalarType::I32, 13);
    b.define(x, thirteen);
    let result = b
        .invoke(
            Op::Binary {
                op: BinaryOp::Div,
                left: thirteen,
                right: d2,
            },
            Some(ty),
            handler,
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    b.switch_to(handler);
    let old = b.read(x);
    b.terminate(Terminator::Return(Some(old)));
    b.finish().unwrap()
}

fn overflow_body(types: &Types, ty: TypeId, boolean: TypeId, op: BinaryOp) -> Body {
    let mut b = Builder::new(types, boolean);
    let left = b.parameter(b.current(), ty);
    let right = b.parameter(b.current(), ty);
    let wrapped = b.emit(Op::Binary { op, left, right }, Some(ty)).unwrap();
    let args = b.args([left, right, wrapped]);
    let flag = b.emit(Op::Overflow { op, args }, Some(boolean)).unwrap();
    b.terminate(Terminator::Return(Some(flag)));
    b.finish().unwrap()
}

fn bit_body(types: &Types, ty: TypeId, result: TypeId, op: crate::scalar::BitOp) -> Body {
    let mut b = Builder::new(types, result);
    let value = b.parameter(b.current(), ty);
    let result = b.emit(Op::Bit { op, value }, Some(result)).unwrap();
    b.terminate(Terminator::Return(Some(result)));
    b.finish().unwrap()
}

fn byte_call(types: &Types, byte: TypeId, int: TypeId) -> Body {
    let mut b = Builder::new(types, int);
    let a = b.parameter(b.current(), byte);
    let c = b.parameter(b.current(), byte);
    let method = b.method(MethodRef {
        owner: "java/lang/Byte".into(),
        name: "compare".into(),
        params: vec![byte, byte],
        returns: int,
        interface: false,
    });
    let args = b.args([a, c]);
    let result = b
        .emit(
            Op::Call {
                method,
                kind: CallKind::JvmStatic,
                args,
            },
            Some(int),
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    b.finish().unwrap()
}

fn call_with_handler(types: &Types, int: TypeId) -> Body {
    let mut b = Builder::new(types, int);
    let value = b.parameter(b.current(), int);
    let divisor = b.parameter(b.current(), int);
    let handler = b.create_block();
    let method = b.method(MethodRef {
        owner: "java/lang/Math".into(),
        name: "floorDiv".into(),
        params: vec![int, int],
        returns: int,
        interface: false,
    });
    let args = b.args([value, divisor]);
    let result = b
        .invoke(
            Op::Call {
                method,
                kind: CallKind::JvmStatic,
                args,
            },
            Some(int),
            handler,
        )
        .unwrap();
    b.terminate(Terminator::Return(Some(result)));
    b.switch_to(handler);
    b.terminate(Terminator::Return(Some(value)));
    b.finish().unwrap()
}

fn switch_body(types: &Types, ty: TypeId, scalar: ScalarType, keys: &[i128]) -> Body {
    let mut b = Builder::new(types, ty);
    let value = b.parameter(b.current(), ty);
    let branches: Vec<_> = keys
        .iter()
        .map(|&key| {
            (
                Scalar::integer(scalar, key as u128).unwrap(),
                b.create_block(),
            )
        })
        .collect();
    let default = b.create_block();
    let join = b.create_block();
    let result = b.variable(ty);
    b.switch(value, branches.iter().copied(), default);
    for (index, (_, target)) in branches.into_iter().enumerate() {
        b.switch_to(target);
        let n = integer(&mut b, ty, scalar, (index + 1) as i128);
        b.define(result, n);
        b.jump(join, vec![]);
    }
    b.switch_to(default);
    let n = integer(&mut b, ty, scalar, -1);
    b.define(result, n);
    b.jump(join, vec![]);
    b.switch_to(join);
    let result = b.read(result);
    b.terminate(Terminator::Return(Some(result)));
    b.finish().unwrap()
}

#[test]
fn jvm_verifies_and_executes_ssa_loops_parallel_copies_scalars_and_throw_points() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let long = types.scalar(ScalarType::I64);
    let boolean = types.scalar(ScalarType::Bool);
    let float = types.scalar(ScalarType::F32);
    let double = types.scalar(ScalarType::F64);
    let uint = types.scalar(ScalarType::U32);
    let ulong = types.scalar(ScalarType::U64);
    let byte = types.scalar(ScalarType::U8);
    let short = types.scalar(ScalarType::I16);
    let signed_byte = types.scalar(ScalarType::I8);
    use crate::scalar::BitOp;
    let methods = [
        (
            "popByte",
            "(B)I",
            bit_body(&types, signed_byte, uint, BitOp::Count),
        ),
        (
            "leadingByte",
            "(B)I",
            bit_body(&types, signed_byte, uint, BitOp::LeadingZeros),
        ),
        (
            "trailingByte",
            "(B)I",
            bit_body(&types, signed_byte, uint, BitOp::TrailingZeros),
        ),
        (
            "reverseByte",
            "(B)B",
            bit_body(&types, signed_byte, signed_byte, BitOp::Reverse),
        ),
        (
            "swapByte",
            "(B)B",
            bit_body(&types, signed_byte, signed_byte, BitOp::SwapBytes),
        ),
        (
            "swapShort",
            "(S)S",
            bit_body(&types, short, short, BitOp::SwapBytes),
        ),
        (
            "reverseLong",
            "(J)J",
            bit_body(&types, long, long, BitOp::Reverse),
        ),
        ("byteCall", "(BB)I", byte_call(&types, byte, int)),
        (
            "overflowAddByte",
            "(BB)Z",
            overflow_body(&types, signed_byte, boolean, BinaryOp::Add),
        ),
        (
            "overflowSubByte",
            "(BB)Z",
            overflow_body(&types, signed_byte, boolean, BinaryOp::Sub),
        ),
        (
            "overflowMulByte",
            "(BB)Z",
            overflow_body(&types, signed_byte, boolean, BinaryOp::Mul),
        ),
        (
            "overflowAddUbyte",
            "(BB)Z",
            overflow_body(&types, byte, boolean, BinaryOp::Add),
        ),
        (
            "overflowSubUbyte",
            "(BB)Z",
            overflow_body(&types, byte, boolean, BinaryOp::Sub),
        ),
        (
            "overflowMulUbyte",
            "(BB)Z",
            overflow_body(&types, byte, boolean, BinaryOp::Mul),
        ),
        (
            "overflowAddLong",
            "(JJ)Z",
            overflow_body(&types, long, boolean, BinaryOp::Add),
        ),
        (
            "overflowSubLong",
            "(JJ)Z",
            overflow_body(&types, long, boolean, BinaryOp::Sub),
        ),
        (
            "overflowMulLong",
            "(JJ)Z",
            overflow_body(&types, long, boolean, BinaryOp::Mul),
        ),
        (
            "overflowMulUlong",
            "(JJ)Z",
            overflow_body(&types, ulong, boolean, BinaryOp::Mul),
        ),
        ("callHandler", "(II)I", call_with_handler(&types, int)),
        (
            "tableSwitch",
            "(I)I",
            switch_body(&types, int, ScalarType::I32, &[-2, -1, 0, 1, 2]),
        ),
        (
            "lookupSwitch",
            "(I)I",
            switch_body(
                &types,
                int,
                ScalarType::I32,
                &[i32::MIN as i128, 0, i32::MAX as i128],
            ),
        ),
        (
            "wideSwitch",
            "(J)J",
            switch_body(
                &types,
                long,
                ScalarType::I64,
                &[i64::MIN as i128, 0, i64::MAX as i128],
            ),
        ),
        (
            "swap",
            "(III)I",
            swap_loop(&types, int, ScalarType::I32, int, boolean),
        ),
        (
            "swapWide",
            "(JJI)J",
            swap_loop(&types, long, ScalarType::I64, int, boolean),
        ),
        ("throwPoints", "(II)I", throw_points(&types, int)),
        (
            "shiftByte",
            "(BB)B",
            binary_body(&types, byte, byte, BinaryOp::Shl),
        ),
        (
            "shiftShort",
            "(SS)S",
            binary_body(&types, short, short, BinaryOp::Shr),
        ),
        (
            "addByte",
            "(BB)B",
            binary_body(&types, byte, byte, BinaryOp::Add),
        ),
        (
            "divUnsigned",
            "(II)I",
            binary_body(&types, uint, uint, BinaryOp::Div),
        ),
        (
            "ltUnsigned",
            "(JJ)Z",
            binary_body(&types, ulong, boolean, BinaryOp::Lt),
        ),
        (
            "ltFloat",
            "(FF)Z",
            binary_body(&types, float, boolean, BinaryOp::Lt),
        ),
        (
            "geFloat",
            "(FF)Z",
            binary_body(&types, float, boolean, BinaryOp::Ge),
        ),
        (
            "eqDouble",
            "(DD)Z",
            binary_body(&types, double, boolean, BinaryOp::Eq),
        ),
        (
            "divDouble",
            "(DD)D",
            binary_body(&types, double, double, BinaryOp::Div),
        ),
    ];
    let mut cp = InternedConstantPool::default();
    let this_class = cp.add_class("SsaFixture").unwrap();
    let super_class = cp.add_class("java/lang/Object").unwrap();
    let methods = methods
        .into_iter()
        .map(|(name, descriptor, body)| {
            let code = compile(&body, &types, &mut cp).unwrap();
            Method {
                access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
                name_index: cp.add_utf8(name).unwrap(),
                descriptor_index: cp.add_utf8(descriptor).unwrap(),
                attributes: vec![Attribute::Code {
                    name_index: cp.add_utf8("Code").unwrap(),
                    max_stack: code.max_stack,
                    max_locals: code.max_locals,
                    code: code.instructions,
                    exception_table: code.exceptions,
                    attributes: code.attributes,
                }],
            }
        })
        .collect();
    let class = ClassFile {
        code_source_url: None,
        version: Version::Java8 { minor: 0 },
        constant_pool: cp.into_inner(),
        access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
        this_class,
        super_class,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods,
        attributes: Vec::new(),
    };
    let mut bytes = Vec::new();
    class.to_bytes(&mut bytes).unwrap();
    let directory = std::env::temp_dir().join(format!("rcj-ssa-fixture-{}", std::process::id()));
    fs::create_dir_all(&directory).unwrap();
    fs::write(directory.join("SsaFixture.class"), bytes).unwrap();
    fs::write(
        directory.join("Run.java"),
        r#"
public class Run {
    static void check(boolean b) { if (!b) throw new AssertionError(); }
    public static void main(String[] args) {
        for (int a=-128; a<128; a++) {
            check(SsaFixture.popByte((byte)a)==Integer.bitCount(a&255));
            check(SsaFixture.leadingByte((byte)a)==Integer.numberOfLeadingZeros(a&255)-24);
            check(SsaFixture.trailingByte((byte)a)==Math.min(8,Integer.numberOfTrailingZeros(a&255)));
            check(SsaFixture.reverseByte((byte)a)==(byte)(Integer.reverse(a&255)>>>24));
            check(SsaFixture.swapByte((byte)a)==(byte)a);
            check(SsaFixture.swapShort((short)(a*127))==Short.reverseBytes((short)(a*127)));
            check(SsaFixture.reverseLong(a)==Long.reverse(a));
        }
        for (int a=-128; a<128; a++) for (int b=-128; b<128; b++) {
            check(SsaFixture.byteCall((byte)a,(byte)b)==Byte.compare((byte)a,(byte)b));
            check(SsaFixture.overflowAddByte((byte)a,(byte)b)==(a+b < -128 || a+b > 127));
            check(SsaFixture.overflowSubByte((byte)a,(byte)b)==(a-b < -128 || a-b > 127));
            check(SsaFixture.overflowMulByte((byte)a,(byte)b)==(a*b < -128 || a*b > 127));
            check(SsaFixture.overflowAddUbyte((byte)a,(byte)b)==((a&255)+(b&255)>255));
            check(SsaFixture.overflowSubUbyte((byte)a,(byte)b)==((a&255)<(b&255)));
            check(SsaFixture.overflowMulUbyte((byte)a,(byte)b)==((a&255)*(b&255)>255));
        }
        long[] boundaries = {Long.MIN_VALUE, Long.MIN_VALUE+1, -3037000500L, -1, 0, 1, 3037000500L, Long.MAX_VALUE-1, Long.MAX_VALUE};
        for (long a : boundaries) for (long b : boundaries) {
            boolean add=false, sub=false, mul=false;
            try { Math.addExact(a,b); } catch (ArithmeticException ex) { add=true; }
            try { Math.subtractExact(a,b); } catch (ArithmeticException ex) { sub=true; }
            try { Math.multiplyExact(a,b); } catch (ArithmeticException ex) { mul=true; }
            check(SsaFixture.overflowAddLong(a,b)==add);
            check(SsaFixture.overflowSubLong(a,b)==sub);
            check(SsaFixture.overflowMulLong(a,b)==mul);
            java.math.BigInteger unsigned = new java.math.BigInteger(Long.toUnsignedString(a)).multiply(new java.math.BigInteger(Long.toUnsignedString(b)));
            check(SsaFixture.overflowMulUlong(a,b)==(unsigned.bitLength()>64));
        }
        for (int n=0; n<100; n++) {
            check(SsaFixture.swap(2,7,n) == (n%2==0 ? 207 : 702));
            check(SsaFixture.swapWide(20000000000L,70000000000L,n) ==
                (n%2==0 ? 2070000000000L : 7020000000000L));
        }
        check(SsaFixture.callHandler(-7, 2)==-4);
        check(SsaFixture.callHandler(123, 0)==123);
        for (int n=-100; n<100; n++) check(SsaFixture.tableSwitch(n)==(n>=-2 && n<=2 ? n+3 : -1));
        check(SsaFixture.lookupSwitch(Integer.MIN_VALUE)==1);
        check(SsaFixture.lookupSwitch(0)==2); check(SsaFixture.lookupSwitch(Integer.MAX_VALUE)==3);
        check(SsaFixture.lookupSwitch(1)==-1);
        check(SsaFixture.wideSwitch(Long.MIN_VALUE)==1);
        check(SsaFixture.wideSwitch(0)==2); check(SsaFixture.wideSwitch(Long.MAX_VALUE)==3);
        check(SsaFixture.wideSwitch(1L << 32)==-1);
        check(SsaFixture.throwPoints(0, 0)==7);
        check(SsaFixture.throwPoints(1, 0)==13);
        check(SsaFixture.throwPoints(1, 2)==6);
        for (int a=0; a<256; a++) for (int b=0; b<256; b++)
            check(SsaFixture.addByte((byte)a,(byte)b)==(byte)(a+b));
        for (int a=0; a<256; a++) for (int b=0; b<256; b++) {
            check(SsaFixture.shiftByte((byte)a,(byte)b)==(byte)(a << (b & 7)));
            check(SsaFixture.shiftShort((short)(a * -129),(short)b)==(short)((short)(a * -129) >> (b & 15)));
        }
        check(SsaFixture.divUnsigned(-1, 2)==Integer.divideUnsigned(-1,2));
        check(!SsaFixture.ltUnsigned(-1, 0)); check(SsaFixture.ltUnsigned(0,-1));
        check(!SsaFixture.ltFloat(Float.NaN, 1)); check(!SsaFixture.ltFloat(1,Float.NaN));
        check(!SsaFixture.geFloat(Float.NaN, 1)); check(SsaFixture.geFloat(-0.0f,0.0f));
        check(SsaFixture.eqDouble(-0.0,0.0)); check(!SsaFixture.eqDouble(Double.NaN,Double.NaN));
        check(Double.doubleToRawLongBits(SsaFixture.divDouble(-0.0,1.0))==Long.MIN_VALUE);
        check(SsaFixture.divDouble(1.0,0.0)==Double.POSITIVE_INFINITY);
        System.out.println("SSA JVM execution passed");
    }
}
"#,
    )
    .unwrap();
    let result = Command::new("java")
        .args(["-Xverify:all", "--class-path"])
        .arg(&directory)
        .arg(directory.join("Run.java"))
        .output()
        .expect("Java is required for SSA execution validation");
    assert!(
        result.status.success(),
        "{}\n{}\nclassfiles: {}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr),
        directory.display()
    );
    fs::remove_dir_all(directory).unwrap();
}

#[test]
fn source_line_tables_follow_selected_instructions_and_terminators() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let body = binary_body(&types, int, int, BinaryOp::Add);
    let lines = SourceLines {
        instructions: vec![Some(7); body.instructions.len()],
        terminators: vec![Some(9); body.blocks.len()],
    };
    let mut cp = InternedConstantPool::default();
    let code = compile_with_options(
        &body,
        &types,
        &mut cp,
        Options {
            lines: Some(&lines),
            ..Default::default()
        },
    )
    .unwrap();
    let table = code
        .attributes
        .iter()
        .find_map(|attribute| match attribute {
            Attribute::LineNumberTable { line_numbers, .. } => Some(line_numbers),
            _ => None,
        })
        .unwrap();
    assert_eq!(
        table
            .iter()
            .map(|line| line.line_number)
            .collect::<Vec<_>>(),
        vec![7, 9]
    );
    assert_eq!(table[0].start_pc, 0);
    assert!(matches!(
        code.instructions[usize::from(table[1].start_pc)],
        Instruction::Ireturn
    ));
    assert!(
        compile_with_options(
            &body,
            &types,
            &mut cp,
            Options {
                lines: Some(&SourceLines::default()),
                ..Default::default()
            }
        )
        .is_err()
    );
}

#[test]
fn typed_calls_reject_bad_signatures_and_preserve_void_effects() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let unit = types.intern(Type::Unit);
    let mut b = Builder::new(&types, unit);
    let method = b.method(MethodRef {
        owner: "java/lang/System".into(),
        name: "gc".into(),
        params: vec![],
        returns: unit,
        interface: false,
    });
    let args = b.args([]);
    b.emit(
        Op::Call {
            method,
            kind: CallKind::JvmStatic,
            args,
        },
        None,
    );
    b.terminate(Terminator::Return(None));
    let mut body = b.finish().unwrap();
    let code = compile(&body, &types, &mut Default::default()).unwrap();
    assert!(
        code.instructions
            .iter()
            .any(|i| matches!(i, Instruction::Invokestatic(_)))
    );
    body.methods[0].params.push(int);
    assert!(
        verify(&body, &types)
            .unwrap_err()
            .0
            .contains("argument count")
    );
    body.methods[0].params.clear();
    body.methods[0].returns = int;
    assert!(verify(&body, &types).unwrap_err().0.contains("return type"));
}

#[test]
fn representation_constants_and_opaque_values_retain_ordered_effects() {
    struct Pool;
    impl Constants for Pool {
        fn emit(
            &self,
            index: u32,
            code: &mut Vec<Instruction>,
            _: &mut InternedConstantPool,
        ) -> jvm::Result<()> {
            assert_eq!(index, 7);
            code.push(Instruction::Iconst_1);
            Ok(())
        }
    }
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let unit = types.intern(Type::Unit);
    let mut b = Builder::new(&types, unit);
    b.body
        .constants
        .push(Constant::External { index: 7, ty: int });
    let value = b.emit(Op::Constant(ConstId::new(0)), Some(int)).unwrap();
    b.emit(Op::Opaque(value), Some(int));
    b.terminate(Terminator::Return(None));
    let body = b.finish().unwrap();
    let live = crate::opt::live(&body, &types);
    assert!(live.instructions.iter().all(|&needed| needed));
    assert!(compile(&body, &types, &mut Default::default()).is_err());
    let code = compile_with_options(
        &body,
        &types,
        &mut Default::default(),
        Options {
            constants: Some(&Pool),
            ..Default::default()
        },
    )
    .unwrap();
    assert!(code.instructions.contains(&Instruction::Pop));
}
