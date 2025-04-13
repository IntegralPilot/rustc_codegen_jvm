// src/lower2/jvm_gen.rs

use crate::oomir;
use ristretto_classfile::{
    self as jvm, BaseType, ClassAccessFlags, ClassFile, ConstantPool, FieldAccessFlags,
    MethodAccessFlags, Version,
    attributes::{Attribute, Instruction},
};

/// Creates a default constructor `<init>()V` that just calls `super()`.
pub(super) fn create_default_constructor(
    // pub(super) or pub(crate)
    cp: &mut ConstantPool,
    super_class_index: u16,
) -> jvm::Result<jvm::Method> {
    let code_attr_name_index = cp.add_utf8("Code")?;
    let init_name_index = cp.add_utf8("<init>")?;
    let init_desc_index = cp.add_utf8("()V")?;

    // Add reference to super.<init>()V
    let super_init_ref_index = cp.add_method_ref(super_class_index, "<init>", "()V")?;

    let instructions = vec![
        Instruction::Aload_0,
        Instruction::Invokespecial(super_init_ref_index),
        Instruction::Return,
    ];

    let max_stack = 1;
    let max_locals = 1;

    let code_attribute = Attribute::Code {
        name_index: code_attr_name_index,
        max_stack,
        max_locals,
        code: instructions,
        exception_table: Vec::new(),
        attributes: Vec::new(),
    };

    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC,
        name_index: init_name_index,
        descriptor_index: init_desc_index,
        attributes: vec![code_attribute],
    })
}

/// Converts an OOMIR Type to a Ristretto FieldType for class field definitions.
pub(super) fn oomir_type_to_ristretto_field_type(
    // pub(super) or pub(crate)
    type2: &oomir::Type,
) -> jvm::FieldType {
    match type2 {
        oomir::Type::I8 => jvm::FieldType::Base(BaseType::Byte),
        oomir::Type::I16 => jvm::FieldType::Base(BaseType::Short),
        oomir::Type::I32 => jvm::FieldType::Base(BaseType::Int),
        oomir::Type::I64 => jvm::FieldType::Base(BaseType::Long),
        oomir::Type::F32 => jvm::FieldType::Base(BaseType::Float),
        oomir::Type::F64 => jvm::FieldType::Base(BaseType::Double),
        oomir::Type::Boolean => jvm::FieldType::Base(BaseType::Boolean),
        oomir::Type::Char => jvm::FieldType::Base(BaseType::Char),
        oomir::Type::String => jvm::FieldType::Object("java/lang/String".to_string()),
        oomir::Type::Reference(ref2) => {
            let inner_ty = ref2.as_ref();
            oomir_type_to_ristretto_field_type(inner_ty)
        }
        oomir::Type::Array(inner_ty) => {
            let inner_field_type = oomir_type_to_ristretto_field_type(inner_ty);
            jvm::FieldType::Array(Box::new(inner_field_type))
        }
        oomir::Type::Class(name) => jvm::FieldType::Object(name.clone()),
        oomir::Type::Void => {
            panic!("Void type cannot be used as a field type");
        }
    }
}

/// Creates a ClassFile (as bytes) for a given OOMIR DataType.
pub(super) fn create_data_type_classfile(
    // pub(super) or pub(crate)
    class_name_jvm: &str,
    data_type: &oomir::DataType,
    super_class_name_jvm: &str,
) -> jvm::Result<Vec<u8>> {
    let mut cp = ConstantPool::default();

    let this_class_index = cp.add_class(class_name_jvm)?;
    let super_class_index = cp.add_class(super_class_name_jvm)?;

    // --- Create Fields ---
    let mut fields: Vec<jvm::Field> = Vec::new();
    for (field_name, field_ty) in &data_type.fields {
        let name_index = cp.add_utf8(field_name)?;
        let descriptor = field_ty.to_jvm_descriptor(); // Ensure this method exists on oomir::Type
        let descriptor_index = cp.add_utf8(&descriptor)?;

        let field = jvm::Field {
            access_flags: FieldAccessFlags::PUBLIC,
            name_index,
            descriptor_index,
            field_type: oomir_type_to_ristretto_field_type(field_ty), // Use helper
            attributes: Vec::new(),
        };
        fields.push(field);
        println!("  - Added field: {} {}", field_name, descriptor);
    }

    // --- Create Default Constructor ---
    let constructor = create_default_constructor(&mut cp, super_class_index)?; // Use helper
    let methods = vec![constructor];

    // --- Assemble ClassFile ---
    let mut class_file = ClassFile {
        version: Version::Java8 { minor: 0 },
        constant_pool: cp,
        access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
        this_class: this_class_index,
        super_class: super_class_index,
        interfaces: Vec::new(),
        fields,
        methods,
        attributes: Vec::new(),
    };

    // --- Add SourceFile Attribute ---
    let simple_name = class_name_jvm.split('/').last().unwrap_or(class_name_jvm);
    let source_file_name = format!("{}.rs", simple_name);
    let source_file_utf8_index = class_file.constant_pool.add_utf8(&source_file_name)?;
    let source_file_attr_name_index = class_file.constant_pool.add_utf8("SourceFile")?;
    class_file.attributes.push(Attribute::SourceFile {
        name_index: source_file_attr_name_index,
        source_file_index: source_file_utf8_index,
    });

    // --- Serialize ---
    let mut byte_vector = Vec::new();
    class_file.to_bytes(&mut byte_vector)?;

    Ok(byte_vector)
}
