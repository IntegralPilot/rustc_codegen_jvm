// src/lower2/jvm_gen.rs

use super::consts::load_constant;
use crate::oomir::{self, Type};

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
        oomir::Type::Array(inner_ty) | oomir::Type::MutableReference(inner_ty) => {
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
    let constructor = create_default_constructor(&mut cp, super_class_index)?;
    let methods = vec![constructor];

    let is_abstract = data_type.is_abstract;

    // --- Assemble ClassFile ---
    let mut class_file = ClassFile {
        version: Version::Java8 { minor: 0 },
        constant_pool: cp,
        access_flags: ClassAccessFlags::PUBLIC
            | ClassAccessFlags::SUPER
            | if is_abstract {
                ClassAccessFlags::ABSTRACT
            } else {
                ClassAccessFlags::FINAL
            },
        this_class: this_class_index,
        super_class: super_class_index,
        interfaces: Vec::new(),
        fields,
        methods,
        attributes: Vec::new(),
    };

    // Check for methods
    for (method_name, tuple) in data_type.methods.iter() {
        let return_type = &tuple.0;
        let return_const = &tuple.1;
        let method_desc = format!("(){}", return_type.to_jvm_descriptor());

        // Add the method to the class file
        let name_index = class_file.constant_pool.add_utf8(&method_name)?;
        let descriptor_index = class_file.constant_pool.add_utf8(method_desc)?;

        let mut attributes = vec![];
        let mut is_abstract = false;

        match return_const {
            Some(rc) => attributes.push(create_code_from_method_name_and_constant_return(
                &rc,
                &mut class_file.constant_pool,
            )?),
            None => {
                is_abstract = true;
            }
        }

        let jvm_method = jvm::Method {
            access_flags: MethodAccessFlags::PUBLIC
                | if is_abstract {
                    MethodAccessFlags::ABSTRACT
                } else {
                    MethodAccessFlags::FINAL
                },
            name_index,
            descriptor_index,
            attributes,
        };

        class_file.methods.push(jvm_method);
    }

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

/// Creates a code attribute for a method that returns a constant value.
fn create_code_from_method_name_and_constant_return(
    return_const: &oomir::Constant,
    cp: &mut ConstantPool,
) -> jvm::Result<Attribute> {
    let code_attr_name_index = cp.add_utf8("Code")?;
    let return_ty = Type::from_constant(return_const);

    // Create the instructions based on the constant type
    let mut instructions = Vec::new();

    load_constant(&mut instructions, cp, return_const)?;

    // add an instruction to return the value that we just loaded onto the stack
    let return_instr = match return_ty {
        oomir::Type::I8
        | oomir::Type::I16
        | oomir::Type::I32
        | oomir::Type::Boolean
        | oomir::Type::Char => Instruction::Ireturn,
        oomir::Type::I64 => Instruction::Lreturn,
        oomir::Type::F32 => Instruction::Freturn,
        oomir::Type::F64 => Instruction::Dreturn,
        oomir::Type::Reference(_)
        | oomir::Type::MutableReference(_)
        | oomir::Type::Array(_)
        | oomir::Type::String
        | oomir::Type::Class(_) => Instruction::Areturn,
        oomir::Type::Void => Instruction::Return, // Should not happen with Some(op)
    };

    instructions.push(return_instr);

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

    Ok(code_attribute)
}
