use crate::lower2::jvm::{self, attributes::Instruction};
use crate::lower2::{
    I128_CLASS, U128_CLASS, constant_pool::InternedConstantPool, helpers::get_cast_instructions,
};
use crate::oomir::{self, Type};

pub use jvm_compiler_core::jvm::constants::{
    append_unpooled_int_const, get_double_const_instr, get_float_const_instr, get_int_const_instr,
    get_long_const_instr,
};

/// Appends JVM instructions for loading a constant onto the stack.
pub fn load_constant(
    instructions: &mut Vec<Instruction>,
    cp: &mut InternedConstantPool,
    constant: &oomir::Constant,
) -> Result<(), jvm::Error> {
    use jvm::attributes::Instruction as JI;
    use oomir::Constant as OC;

    match constant {
        OC::Unit => {}
        OC::StaticRef {
            owner_class,
            field_name,
            ty,
        } => {
            let owner = cp.add_class(owner_class)?;
            let field = cp.add_field_ref(owner, field_name, &ty.to_jvm_descriptor())?;
            instructions.push(JI::Getstatic(field));
        }
        OC::FunctionPointer { adapter_class, .. } => {
            let class_index = cp.add_class(adapter_class)?;
            let constructor = cp.add_method_ref(class_index, "<init>", "()V")?;
            instructions.push(JI::New(class_index));
            instructions.push(JI::Dup);
            instructions.push(JI::Invokespecial(constructor));
        }
        OC::FactoryCall {
            owner_class,
            method_name,
            ty,
        } => {
            let owner = cp.add_class(owner_class)?;
            let method =
                cp.add_method_ref(owner, method_name, &format!("(){}", ty.to_jvm_descriptor()))?;
            instructions.push(JI::Invokestatic(method));
        }
        OC::StaticCall {
            owner_class,
            method_name,
            args,
            param_types,
            ty,
        } => {
            for arg in args {
                load_constant(instructions, cp, arg)?;
            }
            let owner = cp.add_class(owner_class)?;
            let params = args
                .iter()
                .enumerate()
                .map(|(index, arg)| {
                    param_types
                        .get(index)
                        .cloned()
                        .unwrap_or_else(|| oomir::Type::from_constant(arg))
                        .to_jvm_descriptor()
                })
                .collect::<String>();
            let method = cp.add_method_ref(
                owner,
                method_name,
                &format!("({params}){}", ty.to_jvm_descriptor()),
            )?;
            instructions.push(JI::Invokestatic(method));
        }
        OC::PointerAddress {
            address, view_size, ..
        } => {
            let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
            let from_address = cp.add_method_ref(
                pointer_class,
                "fromAddress",
                &format!("(JJ)L{};", oomir::POINTER_CLASS),
            )?;
            instructions.push(get_long_const_instr(cp, *address as i64));
            instructions.push(get_long_const_instr(cp, *view_size as i64));
            instructions.push(JI::Invokestatic(from_address));
        }
        OC::RepeatedBytePointer {
            identity,
            byte,
            length,
            offset,
            view_size,
            alignment,
            view_codec,
            ..
        } => {
            let identity_index = cp.add_string(identity)?;
            instructions.push(if let Ok(index) = u8::try_from(identity_index) {
                JI::Ldc(index)
            } else {
                JI::Ldc_w(identity_index)
            });
            instructions.push(get_int_const_instr(cp, i32::from(*byte as i8)));
            instructions.push(get_long_const_instr(cp, *length as i64));
            instructions.push(get_long_const_instr(cp, *offset as i64));
            instructions.push(get_long_const_instr(cp, *view_size as i64));
            instructions.push(get_long_const_instr(cp, *alignment as i64));
            if let Some(codec) = view_codec {
                let codec_index = cp.add_string(codec)?;
                instructions.push(if let Ok(index) = u8::try_from(codec_index) {
                    JI::Ldc(index)
                } else {
                    JI::Ldc_w(codec_index)
                });
            } else {
                instructions.push(JI::Aconst_null);
            }
            let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
            let factory = cp.add_method_ref(
                pointer_class,
                "constantRepeatedByte",
                &format!(
                    "(Ljava/lang/String;IJJJJLjava/lang/String;)L{};",
                    oomir::POINTER_CLASS
                ),
            )?;
            instructions.push(JI::Invokestatic(factory));
        }
        OC::ByteArrayPointer {
            identity,
            bytes,
            offset,
            view_size,
            alignment,
            view_codec,
            ..
        } => {
            let identity_index = cp.add_string(identity)?;
            instructions.push(if let Ok(index) = u8::try_from(identity_index) {
                JI::Ldc(index)
            } else {
                JI::Ldc_w(identity_index)
            });
            super::arrays::load_bytes(instructions, cp, bytes)?;
            instructions.push(get_long_const_instr(cp, *offset as i64));
            instructions.push(get_long_const_instr(cp, *view_size as i64));
            instructions.push(get_long_const_instr(cp, *alignment as i64));
            if let Some(codec) = view_codec {
                let codec_index = cp.add_string(codec)?;
                instructions.push(if let Ok(index) = u8::try_from(codec_index) {
                    JI::Ldc(index)
                } else {
                    JI::Ldc_w(codec_index)
                });
            } else {
                instructions.push(JI::Aconst_null);
            }
            let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
            let factory = cp.add_method_ref(
                pointer_class,
                "constantBytes",
                &format!(
                    "(Ljava/lang/String;[BJJJLjava/lang/String;)L{};",
                    oomir::POINTER_CLASS
                ),
            )?;
            instructions.push(JI::Invokestatic(factory));
        }
        OC::InternedPointer {
            identity,
            value,
            array_backed,
            allocation_size,
            offset,
            view_size,
            alignment,
            view_codec,
            ..
        } => {
            let identity_index = cp.add_string(identity)?;
            instructions.push(if let Ok(index) = u8::try_from(identity_index) {
                JI::Ldc(index)
            } else {
                JI::Ldc_w(identity_index)
            });
            load_constant(instructions, cp, value)?;
            let value_ty = Type::from_constant(value);
            if !value_ty.has_jvm_value() {
                instructions.push(JI::Aconst_null);
            } else if value_ty != Type::Class("java/lang/Object".to_string()) {
                instructions.extend(get_cast_instructions(
                    "interned constant allocation",
                    &value_ty,
                    &Type::Class("java/lang/Object".to_string()),
                    cp,
                )?);
            }
            let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
            let factory = if *array_backed {
                instructions.push(get_long_const_instr(cp, *view_size as i64));
                load_constant(instructions, cp, view_codec)?;
                instructions.push(get_long_const_instr(cp, *alignment as i64));
                cp.add_method_ref(
                    pointer_class,
                    "constantArray",
                    &format!(
                        "(Ljava/lang/String;Ljava/lang/Object;JLjava/lang/String;J)L{};",
                        oomir::POINTER_CLASS
                    ),
                )?
            } else if *offset == 0 && *allocation_size == *view_size {
                instructions.push(get_long_const_instr(cp, *view_size as i64));
                load_constant(instructions, cp, view_codec)?;
                instructions.push(get_long_const_instr(cp, *alignment as i64));
                cp.add_method_ref(
                    pointer_class,
                    "constantCell",
                    &format!(
                        "(Ljava/lang/String;Ljava/lang/Object;JLjava/lang/String;J)L{};",
                        oomir::POINTER_CLASS
                    ),
                )?
            } else {
                instructions.push(get_long_const_instr(cp, *allocation_size as i64));
                instructions.push(get_long_const_instr(cp, *offset as i64));
                instructions.push(get_long_const_instr(cp, *view_size as i64));
                load_constant(instructions, cp, view_codec)?;
                instructions.push(get_long_const_instr(cp, *alignment as i64));
                cp.add_method_ref(
                    pointer_class,
                    "constantCellAt",
                    &format!(
                        "(Ljava/lang/String;Ljava/lang/Object;JJJLjava/lang/String;J)L{};",
                        oomir::POINTER_CLASS
                    ),
                )?
            };
            instructions.push(JI::Invokestatic(factory));
        }
        OC::I8(v) => instructions.push(get_int_const_instr(cp, *v as i32)),
        OC::U8(v) => instructions.push(get_int_const_instr(cp, i32::from(*v as i8))),
        OC::I16(v) => instructions.push(get_int_const_instr(cp, *v as i32)),
        OC::U16(v) => instructions.push(get_int_const_instr(cp, i32::from(*v))),
        OC::I32(v) => instructions.push(get_int_const_instr(cp, *v)),
        OC::U32(v) => instructions.push(get_int_const_instr(cp, *v as i32)),
        OC::I64(v) => instructions.push(get_long_const_instr(cp, *v)),
        OC::U64(v) => instructions.push(get_long_const_instr(cp, *v as i64)),
        OC::F16(bits) => instructions.push(get_int_const_instr(cp, i32::from(*bits as i16))),
        OC::F32(v) => instructions.push(get_float_const_instr(cp, *v)),
        OC::F64(v) => instructions.push(get_double_const_instr(cp, *v)),
        OC::Boolean(v) => instructions.push(if *v { JI::Iconst_1 } else { JI::Iconst_0 }),
        OC::Char(v) => instructions.push(get_int_const_instr(cp, *v as i32)),
        OC::Str(s) => {
            let index = cp.add_string(s)?;
            instructions.push(if let Ok(idx8) = u8::try_from(index) {
                JI::Ldc(idx8)
            } else {
                JI::Ldc_w(index)
            });
            let view_class = cp.add_class(oomir::UTF8_VIEW_CLASS)?;
            let descriptor = format!("(Ljava/lang/String;)L{};", oomir::UTF8_VIEW_CLASS);
            let from_java = cp.add_method_ref(view_class, "fromJavaString", descriptor)?;
            instructions.push(JI::Invokestatic(from_java));
        }
        OC::String(s) => {
            let index = cp.add_string(s)?;
            instructions.push(if let Ok(idx8) = u8::try_from(index) {
                JI::Ldc(idx8)
            } else {
                JI::Ldc_w(index)
            });
        }
        OC::Null(_) => {
            instructions.push(JI::Aconst_null);
        }
        OC::Slice(element_type, elements) => {
            let class_index = cp.add_class(oomir::SLICE_VIEW_CLASS)?;
            let constructor =
                cp.add_method_ref(class_index, "<init>", "(Ljava/lang/Object;II)V")?;
            instructions.push(JI::New(class_index));
            instructions.push(JI::Dup);
            super::arrays::load_array(instructions, cp, element_type, elements)?;
            instructions.push(JI::Iconst_0);
            instructions.push(get_int_const_instr(cp, elements.len() as i32));
            instructions.push(JI::Invokespecial(constructor));
        }
        OC::SliceRef {
            backing,
            offset,
            length,
            ..
        } => {
            let class_index = cp.add_class(oomir::SLICE_VIEW_CLASS)?;
            let constructor =
                cp.add_method_ref(class_index, "<init>", "(Ljava/lang/Object;IJ)V")?;
            instructions.push(JI::New(class_index));
            instructions.push(JI::Dup);
            load_constant(instructions, cp, backing)?;
            instructions.push(get_int_const_instr(
                cp,
                i32::try_from(*offset).map_err(|_| jvm::Error::VerificationError {
                    context: format!("Attempting to load constant {constant:?}"),
                    message: "Constant slice offset exceeds the JVM address space".to_string(),
                })?,
            ));
            instructions.push(get_long_const_instr(cp, *length as i64));
            instructions.push(JI::Invokespecial(constructor));
        }
        OC::Array(element_type, elements) => {
            super::arrays::load_array(instructions, cp, element_type, elements)?
        }
        OC::Instance {
            class_name,
            fields,
            params,
            param_types,
        } => {
            // 1. Add Class reference to constant pool
            let class_index = cp.add_class(class_name)?;

            // i128/u128 constants are represented in OOMIR as decimal strings so
            // that the interpreter can manipulate them without losing width. At
            // bytecode generation time, materialise their two primitive limbs
            // directly. This avoids string allocation every time a wide
            // constant is loaded at runtime.
            if (class_name == I128_CLASS || class_name == U128_CLASS)
                && let [OC::String(value)] = params.as_slice()
            {
                let (high, low) =
                    if class_name == I128_CLASS {
                        let value = value.parse::<i128>().map_err(|error| {
                            jvm::Error::VerificationError {
                                context: format!("Attempting to load constant {constant:?}"),
                                message: format!("Invalid i128 constant '{value}': {error}"),
                            }
                        })?;
                        ((value >> 64) as i64, value as i64)
                    } else {
                        let value = value.parse::<u128>().map_err(|error| {
                            jvm::Error::VerificationError {
                                context: format!("Attempting to load constant {constant:?}"),
                                message: format!("Invalid u128 constant '{value}': {error}"),
                            }
                        })?;
                        ((value >> 64) as i64, value as i64)
                    };
                let constructor = cp.add_method_ref(class_index, "<init>", "(JJ)V")?;
                instructions.push(JI::New(class_index));
                instructions.push(JI::Dup);
                instructions.push(get_long_const_instr(cp, high));
                instructions.push(get_long_const_instr(cp, low));
                instructions.push(JI::Invokespecial(constructor));
                return Ok(());
            }

            if class_name == oomir::POINTER_CLASS
                && let [value, OC::I32(size), codec] = params.as_slice()
            {
                let constructor = cp.add_method_ref(
                    class_index,
                    "<init>",
                    "(Ljava/lang/Object;ILjava/lang/String;)V",
                )?;
                instructions.push(JI::New(class_index));
                instructions.push(JI::Dup);
                load_constant(instructions, cp, value)?;
                let value_ty = Type::from_constant(value);
                if !value_ty.has_jvm_value() {
                    instructions.push(JI::Aconst_null);
                } else if value_ty != Type::Class("java/lang/Object".to_string()) {
                    instructions.extend(get_cast_instructions(
                        "constant Pointer cell",
                        &value_ty,
                        &Type::Class("java/lang/Object".to_string()),
                        cp,
                    )?);
                }
                instructions.push(get_int_const_instr(cp, *size));
                load_constant(instructions, cp, codec)?;
                instructions.push(JI::Invokespecial(constructor));
                return Ok(());
            }

            if params.is_empty() && !fields.is_empty() {
                return Err(jvm::Error::VerificationError {
                    context: format!("Attempting to load constant {:?}", constant),
                    message: format!(
                        "Constant::Instance for fielded class '{}' has no constructor parameters",
                        class_name
                    ),
                });
            }
            let constructor_params = params
                .iter()
                .enumerate()
                .filter_map(|(index, param)| {
                    let ty = Type::from_constant(param);
                    if !ty.has_jvm_value() {
                        return None;
                    }
                    let declared_ty = param_types.get(index).unwrap_or(&ty).clone();
                    Some((param, declared_ty))
                })
                .collect::<Vec<_>>();

            // 2. Determine constructor signature descriptor.
            let constructor_descriptor = format!(
                "({})V",
                constructor_params
                    .iter()
                    .map(|(_, ty)| ty.to_jvm_descriptor())
                    .collect::<String>()
            );

            // 3. Add Method reference for the constructor "<init>" with the determined signature
            let constructor_ref_index = cp.add_method_ref(
                class_index,
                "<init>",                // Standard name for constructors
                &constructor_descriptor, // Use the calculated descriptor
            )?;
            // 4. Generate instructions to create the object and set its fields

            // a. Emit 'new' instruction: Create uninitialized object
            instructions.push(JI::New(class_index)); // Stack: [uninitialized_ref]

            // b. Emit 'dup' instruction: Duplicate ref (one for invokespecial, one for result/fields)
            instructions.push(JI::Dup); // Stack: [uninitialized_ref, uninitialized_ref]

            // c. Load constructor parameters onto the stack IN ORDER
            for (param_const, _) in constructor_params {
                // Recursively load the constant value for the parameter.
                load_constant(instructions, cp, param_const)?;
                // Stack: [uninitialized_ref, uninitialized_ref, param1, ..., param_i]
            }

            // d. Emit 'invokespecial' to call the constructor
            // Consumes the top ref and all params, initializes the object pointed to by the second ref.
            instructions.push(JI::Invokespecial(constructor_ref_index)); // Stack: [initialized_ref]

            // The generated constructor initializes all fields from `params`.
            // `fields` carries named OOMIR metadata and must not be written a
            // second time here (doing so duplicates referenced object graphs).
            let _ = fields;
        }
    };

    Ok(())
}
