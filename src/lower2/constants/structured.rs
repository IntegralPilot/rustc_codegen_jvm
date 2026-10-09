//! Repeated constant shapes share a constructor loop. Each row and nested array receives separate
//! storage.
use super::*;
use oomir::{Constant as C, Type as T};

#[cfg(test)]
#[path = "structured_tests.rs"]
mod tests;

enum Shape<'a> {
    Scalar(&'a T, usize),
    Array(&'a T, usize),
    Object(&'a str, &'a [T], Vec<Shape<'a>>),
    Unit,
}

impl<'a> Shape<'a> {
    fn new(ty: &'a T, value: &'a C, budget: &mut usize) -> Option<Self> {
        *budget = budget.checked_sub(1)?;
        if let Some((_, size)) = resources::element(ty, value) {
            return Some(Self::Scalar(ty, size));
        }
        match value {
            C::Unit => Some(Self::Unit),
            C::Array(element, _) => {
                let size = match element.as_ref() {
                    T::Boolean | T::I8 | T::U8 => 1,
                    T::I16 | T::U16 | T::Char | T::F16 => 2,
                    T::I32 | T::U32 | T::F32 => 4,
                    T::I64 | T::U64 | T::F64 => 8,
                    _ => return None,
                };
                Some(Self::Array(element, size))
            }
            C::Instance {
                class_name,
                params,
                param_types,
            } if params.len() == param_types.len() => {
                let fields = params
                    .iter()
                    .zip(param_types)
                    .map(|(value, ty)| Self::new(ty, value, budget))
                    .collect::<Option<Vec<_>>>()?;
                Some(Self::Object(class_name, param_types, fields))
            }
            _ => None,
        }
    }

    fn encode(&self, value: &C, bytes: &mut Vec<u8>) -> bool {
        match (self, value) {
            (Self::Scalar(ty, size), value) => {
                let Some((bits, _)) = resources::element(ty, value) else {
                    return false;
                };
                bytes.extend_from_slice(&bits.to_le_bytes()[..*size]);
                true
            }
            (Self::Array(ty, size), C::Array(element, values)) if *ty == element.as_ref() => {
                let length = values.len().saturating_mul(*size).saturating_add(4);
                if bytes.len().saturating_add(length) > jvm::resources::BLOCK_BYTES {
                    return false;
                }
                bytes.extend_from_slice(&(values.len() as u32).to_le_bytes());
                values
                    .iter()
                    .all(|value| Self::Scalar(ty, *size).encode(value, bytes))
            }
            (
                Self::Object(name, types, fields),
                C::Instance {
                    class_name,
                    param_types,
                    params,
                },
            ) if *name == class_name && *types == param_types && fields.len() == params.len() => {
                fields
                    .iter()
                    .zip(params)
                    .all(|(field, value)| field.encode(value, bytes))
            }
            (Self::Unit, C::Unit) => true,
            _ => false,
        }
    }

    fn read(&self, code: &mut Vec<Instruction>, cp: &mut InternedConstantPool) -> jvm::Result<()> {
        match self {
            Self::Unit => {}
            Self::Scalar(ty, _) => {
                let (name, desc) = match ty {
                    T::Boolean | T::I8 | T::U8 => ("get", "()B"),
                    T::I16 | T::F16 => ("getShort", "()S"),
                    T::Char | T::U16 => ("getChar", "()C"),
                    T::I32 | T::U32 => ("getInt", "()I"),
                    T::I64 | T::U64 => ("getLong", "()J"),
                    T::F32 => ("getFloat", "()F"),
                    T::F64 => ("getDouble", "()D"),
                    _ => unreachable!("validated scalar resource"),
                };
                let owner = cp.add_class("java/nio/ByteBuffer")?;
                code.extend([
                    Instruction::Aload_3,
                    Instruction::Invokevirtual(cp.add_method_ref(owner, name, desc)?),
                ]);
            }
            Self::Array(ty, _) => {
                let buffer = cp.add_class("java/nio/ByteBuffer")?;
                code.extend([
                    Instruction::Aload_3,
                    Instruction::Invokevirtual(cp.add_method_ref(buffer, "getInt", "()I")?),
                    Instruction::Newarray(ArrayType::from_bytes(&mut jvm::ByteReader::new(&[ty
                        .to_jvm_primitive_array_type_code()
                        .expect("primitive constant array")]))?),
                ]);
                code.extend([Instruction::Dup, Instruction::Aload_3]);
                let runtime = cp.add_class("org/rustlang/runtime/ConstantData")?;
                code.push(Instruction::Invokestatic(cp.add_method_ref(
                    runtime,
                    "readArray",
                    "(Ljava/lang/Object;Ljava/nio/ByteBuffer;)V",
                )?));
            }
            Self::Object(name, types, fields) => {
                let owner = cp.add_class(name)?;
                code.extend([Instruction::New(owner), Instruction::Dup]);
                for field in fields {
                    field.read(code, cp)?;
                }
                let descriptor = format!(
                    "({})V",
                    types
                        .iter()
                        .filter(|ty| ty.has_jvm_value())
                        .map(T::to_jvm_descriptor)
                        .collect::<String>()
                );
                code.push(Instruction::Invokespecial(
                    cp.add_method_ref(owner, "<init>", descriptor)?,
                ));
            }
        }
        Ok(())
    }
}

pub(super) fn factory(
    cp: &mut InternedConstantPool,
    owner: &str,
    element: &T,
    values: &[C],
    methods: &mut Vec<jvm::Method>,
    next: &mut usize,
    storage: Option<u16>,
) -> jvm::Result<Option<C>> {
    let Some(anchor) = cp.resource_anchor() else {
        return Ok(None);
    };
    let Some(first) = values.first() else {
        return Ok(None);
    };
    let Some(shape) = Shape::new(element, first, &mut 64) else {
        return Ok(None);
    };
    // Validation precedes pool and method changes. Only block boundaries remain in memory.
    let mut bytes = Vec::new();
    let mut total = 0usize;
    let mut block_bytes = 0usize;
    let mut start = 0;
    let mut blocks = Vec::new();
    for (index, value) in values.iter().enumerate() {
        bytes.clear();
        if !shape.encode(value, &mut bytes) || bytes.len() > jvm::resources::BLOCK_BYTES {
            return Ok(None);
        }
        total = total.saturating_add(bytes.len());
        if block_bytes + bytes.len() > jvm::resources::BLOCK_BYTES {
            blocks.push(start..index);
            start = index;
            block_bytes = 0;
        }
        block_bytes += bytes.len();
    }
    if total < 1024 {
        return Ok(None);
    }
    blocks.push(start..values.len());
    let identity = crate::stable_hash::short_hash_value(&(element, values, storage), 16);
    let fill_name = factories::helper_name('r', owner, &identity, *next);
    let factory_name = factories::helper_name('d', owner, &identity, *next);
    *next += 1;
    let array = T::Array(Box::new(element.clone()));
    let descriptor = format!("({}IILjava/nio/ByteBuffer;)V", array.to_jvm_descriptor());
    let mut fill = vec![
        Instruction::Iload_1,
        Instruction::Iload_2,
        Instruction::If_icmpge(0),
        Instruction::Aload_0,
        Instruction::Iload_1,
    ];
    shape.read(&mut fill, cp)?;
    fill.extend([
        element
            .get_jvm_array_store_instruction()
            .expect("resource array"),
        Instruction::Iinc(1, 1),
        Instruction::Goto(0),
    ]);
    fill[2] = Instruction::If_icmpge(u16::try_from(fill.len())?);
    fill.push(Instruction::Return);
    factories::add_constant_helper_method(cp, methods, &fill_name, &descriptor, 4, fill)?;
    let owner = cp.add_class(owner)?;
    let fill = cp.add_method_ref(owner, &fill_name, &descriptor)?;
    let runtime = cp.add_class("org/rustlang/runtime/ConstantData")?;
    let buffer = cp.add_method_ref(
        runtime,
        "buffer",
        "(Ljava/lang/String;Ljava/lang/Class;I)Ljava/nio/ByteBuffer;",
    )?;
    let mut code = Vec::new();
    if let Some(storage) = storage {
        code.push(Instruction::Getstatic(storage));
    } else {
        append_empty_array(&mut code, cp, element, values.len())?;
    }
    for range in blocks {
        let mut bytes = Vec::new();
        for value in &values[range.clone()] {
            assert!(shape.encode(value, &mut bytes));
        }
        let length = bytes.len();
        let resource = cp.add_resource(bytes)?;
        code.extend([
            Instruction::Dup,
            get_int_const_instr(cp, i32::try_from(range.start)?),
            get_int_const_instr(cp, i32::try_from(range.end)?),
            Instruction::Ldc_w(resource),
            Instruction::Ldc_w(anchor),
            get_int_const_instr(cp, i32::try_from(length)?),
            Instruction::Invokestatic(buffer),
            Instruction::Invokestatic(fill),
        ]);
    }
    code.push(Instruction::Areturn);
    factories::add_constant_helper_method(
        cp,
        methods,
        &factory_name,
        &format!("(){}", array.to_jvm_descriptor()),
        0,
        code,
    )?;
    Ok(Some(C::FactoryCall {
        owner_class: cp.try_get_class(owner)?.to_string(),
        method_name: factory_name,
        ty: array,
    }))
}
