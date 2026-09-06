//! Native JVM strings emission.
use super::*;

/// Builds the UTF-8-valid specialization used for Rust `str` values.
pub(in crate::lower2) fn create_utf8_view_classfile() -> jvm::Result<Vec<u8>> {
    let mut cp = InternedConstantPool::default();
    let this_class = cp.add_class(oomir::UTF8_VIEW_CLASS)?;
    let slice_class = cp.add_class(oomir::SLICE_VIEW_CLASS)?;
    let constructor_descriptor = "(Ljava/lang/Object;II)V";
    let slice_constructor = cp.add_method_ref(slice_class, "<init>", constructor_descriptor)?;
    let long_constructor_descriptor = "(Ljava/lang/Object;IJ)V";
    let long_slice_constructor =
        cp.add_method_ref(slice_class, "<init>", long_constructor_descriptor)?;
    let array_field = cp.add_field_ref(slice_class, "array", "Ljava/lang/Object;")?;
    let offset_field = cp.add_field_ref(slice_class, "offset", "I")?;
    let length_field = cp.add_field_ref(slice_class, "length", "I")?;
    let utf8_constructor = cp.add_method_ref(this_class, "<init>", constructor_descriptor)?;

    let constructor = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC,
        name_index: cp.add_utf8("<init>")?,
        descriptor_index: cp.add_utf8(constructor_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            4,
            vec![
                Instruction::Aload_0,
                Instruction::Aload_1,
                Instruction::Iload_2,
                Instruction::Iload_3,
                Instruction::Invokespecial(slice_constructor),
                Instruction::Return,
            ],
            constructor_descriptor,
            false,
            Some(oomir::UTF8_VIEW_CLASS),
            "<init>",
        )?],
    };

    let long_constructor = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC,
        name_index: cp.add_utf8("<init>")?,
        descriptor_index: cp.add_utf8(long_constructor_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            5,
            vec![
                Instruction::Aload_0,
                Instruction::Aload_1,
                Instruction::Iload_2,
                Instruction::Lload_3,
                Instruction::Invokespecial(long_slice_constructor),
                Instruction::Return,
            ],
            long_constructor_descriptor,
            false,
            Some(oomir::UTF8_VIEW_CLASS),
            "<init>",
        )?],
    };

    let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
    let string_view = cp.add_method_ref(
        pointer_class,
        "stringView",
        "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/Object;",
    )?;
    let utf8_view_name = cp.add_string(oomir::UTF8_VIEW_CLASS)?;
    let from_java_descriptor = format!("(Ljava/lang/String;)L{};", oomir::UTF8_VIEW_CLASS);
    let from_java = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("fromJavaString")?,
        descriptor_index: cp.add_utf8(&from_java_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            1,
            vec![
                Instruction::Aload_0,
                Instruction::Ldc_w(utf8_view_name),
                Instruction::Invokestatic(string_view),
                Instruction::Checkcast(this_class),
                Instruction::Areturn,
            ],
            &from_java_descriptor,
            true,
            Some(oomir::UTF8_VIEW_CLASS),
            "fromJavaString",
        )?],
    };

    let slice_to_string_descriptor = format!("(L{};)Ljava/lang/String;", oomir::SLICE_VIEW_CLASS);
    let slice_to_string =
        cp.add_method_ref(slice_class, "toUtf8String", &slice_to_string_descriptor)?;
    let to_java_descriptor = format!("(L{};)Ljava/lang/String;", oomir::UTF8_VIEW_CLASS);
    let to_java = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("toJavaString")?,
        descriptor_index: cp.add_utf8(&to_java_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            1,
            vec![
                Instruction::Aload_0,
                Instruction::Invokestatic(slice_to_string),
                Instruction::Areturn,
            ],
            &to_java_descriptor,
            true,
            Some(oomir::UTF8_VIEW_CLASS),
            "toJavaString",
        )?],
    };

    let as_slice_descriptor = format!(
        "(L{};)L{};",
        oomir::UTF8_VIEW_CLASS,
        oomir::SLICE_VIEW_CLASS
    );
    let as_slice = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("asSlice")?,
        descriptor_index: cp.add_utf8(&as_slice_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            1,
            vec![Instruction::Aload_0, Instruction::Areturn],
            &as_slice_descriptor,
            true,
            Some(oomir::UTF8_VIEW_CLASS),
            "asSlice",
        )?],
    };

    let from_slice_descriptor = format!(
        "(L{};)L{};",
        oomir::SLICE_VIEW_CLASS,
        oomir::UTF8_VIEW_CLASS
    );
    let from_slice = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("fromSlice")?,
        descriptor_index: cp.add_utf8(&from_slice_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            1,
            vec![
                Instruction::New(this_class),
                Instruction::Dup,
                Instruction::Aload_0,
                Instruction::Getfield(array_field),
                Instruction::Aload_0,
                Instruction::Getfield(offset_field),
                Instruction::Aload_0,
                Instruction::Getfield(length_field),
                Instruction::Invokespecial(utf8_constructor),
                Instruction::Areturn,
            ],
            &from_slice_descriptor,
            true,
            Some(oomir::UTF8_VIEW_CLASS),
            "fromSlice",
        )?],
    };

    let len_descriptor = format!("(L{};)J", oomir::UTF8_VIEW_CLASS);
    let len = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("len")?,
        descriptor_index: cp.add_utf8(&len_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            1,
            vec![
                Instruction::Aload_0,
                Instruction::Getfield(length_field),
                Instruction::I2l,
                Instruction::Lreturn,
            ],
            &len_descriptor,
            true,
            Some(oomir::UTF8_VIEW_CLASS),
            "len",
        )?],
    };

    let pointer_class = cp.add_class(oomir::POINTER_CLASS)?;
    let slice_get_i8 = cp.add_method_ref(pointer_class, "sliceGetI8", "(Ljava/lang/Object;I)B")?;
    let starts_with_descriptor = format!(
        "(L{};L{};)Z",
        oomir::UTF8_VIEW_CLASS,
        oomir::UTF8_VIEW_CLASS
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
            Some(oomir::UTF8_VIEW_CLASS),
            "startsWith",
        )?],
    };

    let equals_descriptor = starts_with_descriptor.clone();
    let starts_with_ref = cp.add_method_ref(this_class, "startsWith", &starts_with_descriptor)?;
    let equals = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("equals")?,
        descriptor_index: cp.add_utf8(&equals_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            2,
            vec![
                Instruction::Aload_0,
                Instruction::Getfield(length_field),
                Instruction::Aload_1,
                Instruction::Getfield(length_field),
                Instruction::If_icmpne(9),
                Instruction::Aload_0,
                Instruction::Aload_1,
                Instruction::Invokestatic(starts_with_ref),
                Instruction::Ireturn,
                Instruction::Iconst_0,
                Instruction::Ireturn,
            ],
            &equals_descriptor,
            true,
            Some(oomir::UTF8_VIEW_CLASS),
            "equals",
        )?],
    };

    let to_java_ref = cp.add_method_ref(this_class, "toJavaString", &to_java_descriptor)?;
    let character_class = cp.add_class("java/lang/Character")?;
    let to_chars = cp.add_method_ref(character_class, "toChars", "(I)[C")?;
    let string_class = cp.add_class("java/lang/String")?;
    let string_value_of = cp.add_method_ref(string_class, "valueOf", "([C)Ljava/lang/String;")?;
    let java_starts_with =
        cp.add_method_ref(string_class, "startsWith", "(Ljava/lang/String;)Z")?;
    let starts_with_char_descriptor = format!("(L{};I)Z", oomir::UTF8_VIEW_CLASS);
    let starts_with_char = jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index: cp.add_utf8("startsWithChar")?,
        descriptor_index: cp.add_utf8(&starts_with_char_descriptor)?,
        attributes: vec![code_attribute_for_descriptor(
            &mut cp,
            2,
            vec![
                Instruction::Aload_0,
                Instruction::Invokestatic(to_java_ref),
                Instruction::Iload_1,
                Instruction::Invokestatic(to_chars),
                Instruction::Invokestatic(string_value_of),
                Instruction::Invokevirtual(java_starts_with),
                Instruction::Ireturn,
            ],
            &starts_with_char_descriptor,
            true,
            Some(oomir::UTF8_VIEW_CLASS),
            "startsWithChar",
        )?],
    };

    let class_file = ClassFile {
        code_source_url: None,
        version: Version::Java8 { minor: 0 },
        constant_pool: cp.into_inner(),
        access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::FINAL | ClassAccessFlags::SUPER,
        this_class,
        super_class: slice_class,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: vec![
            constructor,
            long_constructor,
            from_java,
            to_java,
            as_slice,
            from_slice,
            len,
            starts_with,
            equals,
            starts_with_char,
        ],
        attributes: Vec::new(),
    };
    verify_no_duplicate_constants(&class_file)?;

    let mut bytes = Vec::new();
    class_file.to_bytes(&mut bytes)?;
    Ok(bytes)
}
