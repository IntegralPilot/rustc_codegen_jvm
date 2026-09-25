//! Native JVM slices emission.
use super::*;

/// Builds the backend-owned runtime representation used for Rust slices.
pub(in crate::lower2) fn create_slice_view_classfile() -> jvm::Result<Vec<u8>> {
    let mut cp = InternedConstantPool::default();
    let this_class = cp.add_class(oomir::SLICE_VIEW_CLASS)?;
    let object_class = cp.add_class("java/lang/Object")?;

    let array_field = cp.add_field_ref(this_class, "array", "Ljava/lang/Object;")?;
    let offset_field = cp.add_field_ref(this_class, "offset", "I")?;
    let length_field = cp.add_field_ref(this_class, "length", "I")?;
    let rust_length_field = cp.add_field_ref(this_class, "rustLength", "J")?;
    let object_init = cp.add_method_ref(object_class, "<init>", "()V")?;

    let constructor_descriptor = "(Ljava/lang/Object;II)V";
    let constructor = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC,
        name_index: cp.add_utf8("<init>")?,
        descriptor_index: cp.add_utf8(constructor_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            4,
            vec![
                Instruction::Aload_0,
                Instruction::Invokespecial(object_init),
                Instruction::Aload_0,
                Instruction::Aload_1,
                Instruction::Putfield(array_field),
                Instruction::Aload_0,
                Instruction::Iload_2,
                Instruction::Putfield(offset_field),
                Instruction::Aload_0,
                Instruction::Iload_3,
                Instruction::Putfield(length_field),
                Instruction::Aload_0,
                Instruction::Iload_3,
                Instruction::I2l,
                Instruction::Putfield(rust_length_field),
                Instruction::Return,
            ],
            constructor_descriptor,
            false,
            Some(oomir::SLICE_VIEW_CLASS),
            "<init>",
        )?],
    };

    let get_class = cp.add_method_ref(object_class, "getClass", "()Ljava/lang/Class;")?;
    let class_class = cp.add_class("java/lang/Class")?;
    let get_component_type =
        cp.add_method_ref(class_class, "getComponentType", "()Ljava/lang/Class;")?;
    let reflect_array_class = cp.add_class("java/lang/reflect/Array")?;
    let new_array = cp.add_method_ref(
        reflect_array_class,
        "newInstance",
        "(Ljava/lang/Class;I)Ljava/lang/Object;",
    )?;
    let system_class = cp.add_class("java/lang/System")?;
    let array_copy = cp.add_method_ref(
        system_class,
        "arraycopy",
        "(Ljava/lang/Object;ILjava/lang/Object;II)V",
    )?;
    let to_array_descriptor = "()Ljava/lang/Object;";
    let to_array = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::FINAL,
        name_index: cp.add_utf8("toArray")?,
        descriptor_index: cp.add_utf8(to_array_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            2,
            vec![
                Instruction::Aload_0,
                Instruction::Getfield(array_field),
                Instruction::Invokevirtual(get_class),
                Instruction::Invokevirtual(get_component_type),
                Instruction::Aload_0,
                Instruction::Getfield(length_field),
                Instruction::Invokestatic(new_array),
                Instruction::Astore_1,
                Instruction::Aload_0,
                Instruction::Getfield(array_field),
                Instruction::Aload_0,
                Instruction::Getfield(offset_field),
                Instruction::Aload_1,
                Instruction::Iconst_0,
                Instruction::Aload_0,
                Instruction::Getfield(length_field),
                Instruction::Invokestatic(array_copy),
                Instruction::Aload_1,
                Instruction::Areturn,
            ],
            to_array_descriptor,
            false,
            Some(oomir::SLICE_VIEW_CLASS),
            "toArray",
        )?],
    };

    let long_constructor_descriptor = "(Ljava/lang/Object;IJ)V";
    let long_constructor = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC,
        name_index: cp.add_utf8("<init>")?,
        descriptor_index: cp.add_utf8(long_constructor_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            5,
            vec![
                Instruction::Aload_0,
                Instruction::Invokespecial(object_init),
                Instruction::Aload_0,
                Instruction::Aload_1,
                Instruction::Putfield(array_field),
                Instruction::Aload_0,
                Instruction::Iload_2,
                Instruction::Putfield(offset_field),
                Instruction::Aload_0,
                Instruction::Lload_3,
                Instruction::L2i,
                Instruction::Putfield(length_field),
                Instruction::Aload_0,
                Instruction::Lload_3,
                Instruction::Putfield(rust_length_field),
                Instruction::Return,
            ],
            long_constructor_descriptor,
            false,
            Some(oomir::SLICE_VIEW_CLASS),
            "<init>",
        )?],
    };

    let standard_charsets = cp.add_class("java/nio/charset/StandardCharsets")?;
    let utf8 = cp.add_field_ref(standard_charsets, "UTF_8", "Ljava/nio/charset/Charset;")?;
    let string_class = cp.add_class("java/lang/String")?;
    let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
    let string_view = cp.add_method_ref(
        pointer_class,
        "stringView",
        "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/Object;",
    )?;
    let slice_view_name = cp.add_string(oomir::SLICE_VIEW_CLASS)?;
    let from_string_descriptor = format!("(Ljava/lang/String;)L{};", oomir::SLICE_VIEW_CLASS);
    let from_string = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("fromString")?,
        descriptor_index: cp.add_utf8(&from_string_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            1,
            vec![
                Instruction::Aload_0,
                Instruction::Ldc_w(slice_view_name),
                Instruction::Invokestatic(string_view),
                Instruction::Checkcast(this_class),
                Instruction::Areturn,
            ],
            &from_string_descriptor,
            true,
            Some(oomir::SLICE_VIEW_CLASS),
            "fromString",
        )?],
    };

    let slice_to_byte_array = cp.add_method_ref(
        pointer_class,
        "sliceToByteArray",
        "(Ljava/lang/Object;II)[B",
    )?;
    let string_from_bytes =
        cp.add_method_ref(string_class, "<init>", "([BLjava/nio/charset/Charset;)V")?;
    let to_utf8_string_descriptor = format!("(L{};)Ljava/lang/String;", oomir::SLICE_VIEW_CLASS);
    let to_utf8_string = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("toUtf8String")?,
        descriptor_index: cp.add_utf8(&to_utf8_string_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            2,
            vec![
                Instruction::Aload_0,
                Instruction::Getfield(array_field),
                Instruction::Aload_0,
                Instruction::Getfield(offset_field),
                Instruction::Aload_0,
                Instruction::Getfield(length_field),
                Instruction::Invokestatic(slice_to_byte_array),
                Instruction::Astore_1,
                Instruction::New(string_class),
                Instruction::Dup,
                Instruction::Aload_1,
                Instruction::Getstatic(utf8),
                Instruction::Invokespecial(string_from_bytes),
                Instruction::Areturn,
            ],
            &to_utf8_string_descriptor,
            true,
            Some(oomir::SLICE_VIEW_CLASS),
            "toUtf8String",
        )?],
    };

    let character_class = cp.add_class("java/lang/Character")?;
    let to_chars = cp.add_method_ref(character_class, "toChars", "(I)[C")?;
    let string_value_of = cp.add_method_ref(string_class, "valueOf", "([C)Ljava/lang/String;")?;
    let from_string_ref = cp.add_method_ref(this_class, "fromString", &from_string_descriptor)?;
    let utf8_class = cp.add_class(oomir::UTF8_VIEW_CLASS)?;
    let utf8_constructor = cp.add_method_ref(utf8_class, "<init>", "(Ljava/lang/Object;II)V")?;
    let encode_utf8_descriptor = format!(
        "(IL{};)L{};",
        oomir::SLICE_VIEW_CLASS,
        oomir::UTF8_VIEW_CLASS
    );
    let encode_utf8 = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("encodeUtf8")?,
        descriptor_index: cp.add_utf8(&encode_utf8_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            4,
            vec![
                Instruction::Iload_0,
                Instruction::Invokestatic(to_chars),
                Instruction::Invokestatic(string_value_of),
                Instruction::Astore_2,
                Instruction::Aload_2,
                Instruction::Invokestatic(from_string_ref),
                Instruction::Astore_3,
                Instruction::Aload_3,
                Instruction::Getfield(array_field),
                Instruction::Aload_3,
                Instruction::Getfield(offset_field),
                Instruction::Aload_1,
                Instruction::Getfield(array_field),
                Instruction::Aload_1,
                Instruction::Getfield(offset_field),
                Instruction::Aload_3,
                Instruction::Getfield(length_field),
                Instruction::Invokestatic(array_copy),
                Instruction::New(utf8_class),
                Instruction::Dup,
                Instruction::Aload_1,
                Instruction::Getfield(array_field),
                Instruction::Aload_1,
                Instruction::Getfield(offset_field),
                Instruction::Aload_3,
                Instruction::Getfield(length_field),
                Instruction::Invokespecial(utf8_constructor),
                Instruction::Areturn,
            ],
            &encode_utf8_descriptor,
            true,
            Some(oomir::SLICE_VIEW_CLASS),
            "encodeUtf8",
        )?],
    };

    let slice_get_object = cp.add_method_ref(
        pointer_class,
        "sliceGetObject",
        "(Ljava/lang/Object;I)Ljava/lang/Object;",
    )?;
    let objects_class = cp.add_class("java/util/Objects")?;
    let objects_equals = cp.add_method_ref(
        objects_class,
        "equals",
        "(Ljava/lang/Object;Ljava/lang/Object;)Z",
    )?;
    let starts_with_descriptor = format!(
        "(L{};L{};)Z",
        oomir::SLICE_VIEW_CLASS,
        oomir::SLICE_VIEW_CLASS
    );
    let starts_with = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("startsWith")?,
        descriptor_index: cp.add_utf8(&starts_with_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            3,
            vec![
                Instruction::Aload_1,
                Instruction::Getfield(length_field),
                Instruction::Aload_0,
                Instruction::Getfield(length_field),
                Instruction::If_icmpgt(31),
                Instruction::Iconst_0,
                Instruction::Istore_2,
                Instruction::Iload_2,
                Instruction::Aload_1,
                Instruction::Getfield(length_field),
                Instruction::If_icmpge(29),
                Instruction::Aload_0,
                Instruction::Getfield(array_field),
                Instruction::Aload_0,
                Instruction::Getfield(offset_field),
                Instruction::Iload_2,
                Instruction::Iadd,
                Instruction::Invokestatic(slice_get_object),
                Instruction::Aload_1,
                Instruction::Getfield(array_field),
                Instruction::Aload_1,
                Instruction::Getfield(offset_field),
                Instruction::Iload_2,
                Instruction::Iadd,
                Instruction::Invokestatic(slice_get_object),
                Instruction::Invokestatic(objects_equals),
                Instruction::Ifeq(31),
                Instruction::Iinc(2, 1),
                Instruction::Goto(7),
                Instruction::Iconst_1,
                Instruction::Ireturn,
                Instruction::Iconst_0,
                Instruction::Ireturn,
            ],
            &starts_with_descriptor,
            true,
            Some(oomir::SLICE_VIEW_CLASS),
            "startsWith",
        )?],
    };

    // Byte slices may be backed either directly by a JVM byte array or by a
    // runtime Pointer carrying a codec (notably `[MaybeUninit<u8>]` during
    // optimised UTF-8 construction). Pointer.sliceGetI8 handles both forms.
    let slice_get_i8 = cp.add_method_ref(pointer_class, "sliceGetI8", "(Ljava/lang/Object;I)B")?;
    let starts_with_i8 = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("startsWithI8")?,
        descriptor_index: cp.add_utf8(&starts_with_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            3,
            vec![
                Instruction::Aload_1,
                Instruction::Getfield(length_field),
                Instruction::Aload_0,
                Instruction::Getfield(length_field),
                Instruction::If_icmpgt(30),
                Instruction::Iconst_0,
                Instruction::Istore_2,
                Instruction::Iload_2,
                Instruction::Aload_1,
                Instruction::Getfield(length_field),
                Instruction::If_icmpge(28),
                Instruction::Aload_0,
                Instruction::Getfield(array_field),
                Instruction::Aload_0,
                Instruction::Getfield(offset_field),
                Instruction::Iload_2,
                Instruction::Iadd,
                Instruction::Invokestatic(slice_get_i8),
                Instruction::Aload_1,
                Instruction::Getfield(array_field),
                Instruction::Aload_1,
                Instruction::Getfield(offset_field),
                Instruction::Iload_2,
                Instruction::Iadd,
                Instruction::Invokestatic(slice_get_i8),
                Instruction::If_icmpne(30),
                Instruction::Iinc(2, 1),
                Instruction::Goto(7),
                Instruction::Iconst_1,
                Instruction::Ireturn,
                Instruction::Iconst_0,
                Instruction::Ireturn,
            ],
            &starts_with_descriptor,
            true,
            Some(oomir::SLICE_VIEW_CLASS),
            "startsWithI8",
        )?],
    };

    // Integer slice views can be backed by either a JVM primitive array or
    // encoded Rust allocation storage. Use the typed accessor so equality is
    // independent of that physical representation.
    let slice_get_i32 =
        cp.add_method_ref(pointer_class, "sliceGetI32", "(Ljava/lang/Object;I)I")?;
    let starts_with_i32 = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("startsWithI32")?,
        descriptor_index: cp.add_utf8(&starts_with_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            3,
            vec![
                Instruction::Aload_1,
                Instruction::Getfield(length_field),
                Instruction::Aload_0,
                Instruction::Getfield(length_field),
                Instruction::If_icmpgt(30),
                Instruction::Iconst_0,
                Instruction::Istore_2,
                Instruction::Iload_2,
                Instruction::Aload_1,
                Instruction::Getfield(length_field),
                Instruction::If_icmpge(28),
                Instruction::Aload_0,
                Instruction::Getfield(array_field),
                Instruction::Aload_0,
                Instruction::Getfield(offset_field),
                Instruction::Iload_2,
                Instruction::Iadd,
                Instruction::Invokestatic(slice_get_i32),
                Instruction::Aload_1,
                Instruction::Getfield(array_field),
                Instruction::Aload_1,
                Instruction::Getfield(offset_field),
                Instruction::Iload_2,
                Instruction::Iadd,
                Instruction::Invokestatic(slice_get_i32),
                Instruction::If_icmpne(30),
                Instruction::Iinc(2, 1),
                Instruction::Goto(7),
                Instruction::Iconst_1,
                Instruction::Ireturn,
                Instruction::Iconst_0,
                Instruction::Ireturn,
            ],
            &starts_with_descriptor,
            true,
            Some(oomir::SLICE_VIEW_CLASS),
            "startsWithI32",
        )?],
    };

    let fields = vec![
        jvm::Field {
            access_flags: FieldAccessFlags::PUBLIC | FieldAccessFlags::FINAL,
            name_index: cp.add_utf8("array")?,
            descriptor_index: cp.add_utf8("Ljava/lang/Object;")?,
            field_type: jvm::FieldType::Object("java/lang/Object".into()),
            attributes: Vec::new(),
        },
        jvm::Field {
            access_flags: FieldAccessFlags::PUBLIC | FieldAccessFlags::FINAL,
            name_index: cp.add_utf8("offset")?,
            descriptor_index: cp.add_utf8("I")?,
            field_type: jvm::FieldType::Base(BaseType::Int),
            attributes: Vec::new(),
        },
        jvm::Field {
            access_flags: FieldAccessFlags::PUBLIC | FieldAccessFlags::FINAL,
            name_index: cp.add_utf8("rustLength")?,
            descriptor_index: cp.add_utf8("J")?,
            field_type: jvm::FieldType::Base(BaseType::Long),
            attributes: Vec::new(),
        },
        jvm::Field {
            access_flags: FieldAccessFlags::PUBLIC | FieldAccessFlags::FINAL,
            name_index: cp.add_utf8("length")?,
            descriptor_index: cp.add_utf8("I")?,
            field_type: jvm::FieldType::Base(BaseType::Int),
            attributes: Vec::new(),
        },
    ];

    let class_file = ClassFile {
        code_source_url: None,
        version: Version::Java8 { minor: 0 },
        constant_pool: cp.into_inner(),
        access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
        this_class,
        super_class: object_class,
        interfaces: Vec::new(),
        fields,
        methods: vec![
            constructor,
            long_constructor,
            to_array,
            from_string,
            to_utf8_string,
            encode_utf8,
            starts_with,
            starts_with_i8,
            starts_with_i32,
        ],
        attributes: Vec::new(),
    };
    verify_no_duplicate_constants(&class_file)?;

    let mut bytes = Vec::new();
    class_file.to_bytes(&mut bytes)?;
    Ok(bytes)
}
