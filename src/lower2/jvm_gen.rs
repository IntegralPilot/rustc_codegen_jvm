// src/lower2/jvm_gen.rs

use super::{
    FunctionTranslator,
    constant_pool::{InternedConstantPool, verify_no_duplicate_constants},
    consts::{get_int_const_instr, load_constant},
    helpers::{get_load_instruction, get_type_size, oomir_function_stack_floor},
    optimise2, stackmaps,
};
use crate::oomir::{self, AdtHelperKind, DataTypeMethod, Signature, Type};

use ristretto_classfile::{
    self as jvm, BaseType, ClassAccessFlags, ClassFile, FieldAccessFlags, MethodAccessFlags,
    Version,
    attributes::{Attribute, InnerClass, Instruction, MaxStack, NestedClassAccessFlags},
};
use std::collections::{HashMap, HashSet};

fn code_attribute_with_stack_maps(
    cp: &mut InternedConstantPool,
    max_stack: u16,
    max_locals: u16,
    code: Vec<Instruction>,
    initial_locals: Vec<stackmaps::FrameValue>,
    context: &str,
) -> jvm::Result<Attribute> {
    let fixed_prefix_slots = initial_locals.len() as u16;
    let optimised = optimise2::optimise(code, max_locals, fixed_prefix_slots)?;
    let code = optimised.instructions;
    let max_locals = optimised.max_locals;
    let name_index = cp.add_utf8("Code")?;
    let attributes = stackmaps::build_stack_map_attributes(
        &code,
        &initial_locals,
        &[],
        max_locals,
        cp,
        context,
    )?;
    Ok(Attribute::Code {
        name_index,
        max_stack,
        max_locals,
        code,
        exception_table: Vec::new(),
        attributes,
    })
}

fn code_attribute_for_descriptor(
    cp: &mut InternedConstantPool,
    max_stack: u16,
    max_locals: u16,
    code: Vec<Instruction>,
    descriptor: &str,
    is_static: bool,
    this_class_name: Option<&str>,
    method_name: &str,
) -> jvm::Result<Attribute> {
    let initial_locals = stackmaps::initial_locals_for_descriptor(
        descriptor,
        is_static,
        this_class_name,
        method_name == "<init>",
    )?;
    code_attribute_with_stack_maps(cp, max_stack, max_locals, code, initial_locals, method_name)
}

/// Creates a default constructor `<init>()V` that just calls `super()`.
pub(super) fn create_default_constructor(
    // pub(super) or pub(crate)
    cp: &mut InternedConstantPool,
    super_class_index: u16,
) -> jvm::Result<jvm::Method> {
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

    let code_attribute = code_attribute_for_descriptor(
        cp,
        max_stack,
        max_locals,
        instructions,
        "()V",
        false,
        None,
        "<init>",
    )?;

    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC,
        name_index: init_name_index,
        descriptor_index: init_desc_index,
        attributes: vec![code_attribute],
    })
}

fn create_field_constructor(
    cp: &mut InternedConstantPool,
    this_class_index: u16,
    super_class_index: u16,
    fields: &[(String, Type)],
) -> jvm::Result<jvm::Method> {
    let init_name_index = cp.add_utf8("<init>")?;
    let descriptor = format!(
        "({})V",
        fields
            .iter()
            .map(|(_, ty)| ty.to_jvm_descriptor())
            .collect::<String>()
    );
    let init_desc_index = cp.add_utf8(&descriptor)?;
    let super_init_ref_index = cp.add_method_ref(super_class_index, "<init>", "()V")?;

    let mut instructions = vec![
        Instruction::Aload_0,
        Instruction::Invokespecial(super_init_ref_index),
    ];
    let mut next_local = 1;
    let mut max_field_stack = 1;

    for (field_name, field_ty) in fields {
        let field_ref =
            cp.add_field_ref(this_class_index, field_name, &field_ty.to_jvm_descriptor())?;
        instructions.push(Instruction::Aload_0);
        instructions.push(get_load_instruction(field_ty, next_local)?);
        instructions.push(Instruction::Putfield(field_ref));

        let field_size = get_type_size(field_ty);
        max_field_stack = max_field_stack.max(1 + field_size);
        next_local += field_size;
    }

    instructions.push(Instruction::Return);

    let mut parameters = Vec::new();
    for (field_name, _) in fields {
        let name_index = cp.add_utf8(field_name)?;
        parameters.push(jvm::attributes::MethodParameter {
            name_index,
            access_flags: MethodAccessFlags::empty(),
        });
    }
    let method_parameters_attribute_name_index = cp.add_utf8("MethodParameters")?;

    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC,
        name_index: init_name_index,
        descriptor_index: init_desc_index,
        attributes: vec![
            code_attribute_for_descriptor(
                cp,
                max_field_stack,
                next_local,
                instructions,
                &descriptor,
                false,
                None,
                "<init>",
            )?,
            Attribute::MethodParameters {
                name_index: method_parameters_attribute_name_index,
                parameters,
            },
        ],
    })
}

fn return_instruction_for_type(ty: &Type) -> Instruction {
    match ty {
        Type::I8 | Type::I16 | Type::I32 | Type::Boolean | Type::Char => Instruction::Ireturn,
        Type::I64 => Instruction::Lreturn,
        Type::F32 => Instruction::Freturn,
        Type::F64 => Instruction::Dreturn,
        Type::Void => Instruction::Return,
        Type::Reference(_)
        | Type::MutableReference(_)
        | Type::Array(_)
        | Type::String
        | Type::Class(_)
        | Type::Interface(_) => Instruction::Areturn,
    }
}

fn create_static_instance_bridge(
    cp: &mut InternedConstantPool,
    class_name_jvm: &str,
    method_name: &str,
    function: &oomir::Function,
) -> jvm::Result<jvm::Method> {
    debug_assert!(
        !function.signature.is_static && !function.signature.params.is_empty(),
        "static receiver bridges require the first signature parameter to be self"
    );
    let name_index = cp.add_utf8(method_name)?;

    // OOMIR instance method signatures retain self as params[0], while the real
    // JVM instance method descriptor omits it. The static bridge makes that
    // receiver explicit again and delegates to the instance method.
    let mut bridge_params = function.signature.params.clone();
    if let Some((_, receiver_ty)) = bridge_params.first_mut() {
        *receiver_ty = Type::Class(class_name_jvm.to_string());
    }

    let bridge_signature = Signature {
        params: bridge_params,
        ret: function.signature.ret.clone(),
        is_static: true,
    };
    let bridge_descriptor = bridge_signature.to_string();
    let descriptor_index = cp.add_utf8(&bridge_descriptor)?;

    let class_index = cp.add_class(class_name_jvm)?;
    let instance_method_ref =
        cp.add_method_ref(class_index, method_name, &function.signature.to_string())?;

    let mut instructions = Vec::new();
    let mut next_local = 0;
    let mut max_stack = 0;
    for (_, param_ty) in &bridge_signature.params {
        instructions.push(get_load_instruction(param_ty, next_local)?);
        let size = get_type_size(param_ty);
        max_stack += size;
        next_local += size;
    }
    instructions.push(Instruction::Invokevirtual(instance_method_ref));
    instructions.push(return_instruction_for_type(bridge_signature.ret.as_ref()));

    let mut parameters = Vec::new();
    for (param_name, _) in &bridge_signature.params {
        let name_index = cp.add_utf8(param_name)?;
        parameters.push(jvm::attributes::MethodParameter {
            name_index,
            access_flags: MethodAccessFlags::empty(),
        });
    }
    let method_parameters_attribute_name_index = cp.add_utf8("MethodParameters")?;

    let return_stack = if *bridge_signature.ret == Type::Void {
        0
    } else {
        get_type_size(bridge_signature.ret.as_ref())
    };

    Ok(jvm::Method {
        access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
        name_index,
        descriptor_index,
        attributes: vec![
            code_attribute_for_descriptor(
                cp,
                max_stack.max(return_stack),
                next_local,
                instructions,
                &bridge_descriptor,
                true,
                None,
                method_name,
            )?,
            Attribute::MethodParameters {
                name_index: method_parameters_attribute_name_index,
                parameters,
            },
        ],
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
        oomir::Type::Class(name) | oomir::Type::Interface(name) => {
            jvm::FieldType::Object(name.clone())
        }
        oomir::Type::Void => {
            panic!("Void type cannot be used as a field type");
        }
    }
}

fn patch_branch_target(instructions: &mut [Instruction], branch_index: usize, target: u16) {
    match &mut instructions[branch_index] {
        Instruction::Ifeq(offset)
        | Instruction::Ifne(offset)
        | Instruction::If_icmpne(offset)
        | Instruction::If_acmpne(offset) => *offset = target,
        other => panic!("Cannot patch non-branch instruction: {:?}", other),
    }
}

fn has_generated_eq(module: &oomir::Module, class_name: &str) -> bool {
    match module.data_types.get(class_name) {
        Some(oomir::DataType::Class { methods, .. }) => methods.contains_key("eq"),
        Some(oomir::DataType::Interface { methods }) => methods.contains_key("eq"),
        None => false,
    }
}

fn append_boolean_false_check(instructions: &mut Vec<Instruction>, false_fixups: &mut Vec<usize>) {
    false_fixups.push(instructions.len());
    instructions.push(Instruction::Ifeq(0));
}

fn append_field_equality_check(
    module: &oomir::Module,
    cp: &mut InternedConstantPool,
    instructions: &mut Vec<Instruction>,
    false_fixups: &mut Vec<usize>,
    variant_class_idx: u16,
    field_name: &str,
    field_ty: &Type,
) -> jvm::Result<()> {
    let field_ref =
        cp.add_field_ref(variant_class_idx, field_name, &field_ty.to_jvm_descriptor())?;

    instructions.push(Instruction::Aload_0);
    instructions.push(Instruction::Checkcast(variant_class_idx));
    instructions.push(Instruction::Getfield(field_ref));
    instructions.push(Instruction::Aload_1);
    instructions.push(Instruction::Checkcast(variant_class_idx));
    instructions.push(Instruction::Getfield(field_ref));

    match field_ty {
        Type::I64 => {
            instructions.push(Instruction::Lcmp);
            false_fixups.push(instructions.len());
            instructions.push(Instruction::Ifne(0));
        }
        Type::F32 => {
            instructions.push(Instruction::Fcmpl);
            false_fixups.push(instructions.len());
            instructions.push(Instruction::Ifne(0));
        }
        Type::F64 => {
            instructions.push(Instruction::Dcmpl);
            false_fixups.push(instructions.len());
            instructions.push(Instruction::Ifne(0));
        }
        Type::I8 | Type::I16 | Type::I32 | Type::Boolean | Type::Char => {
            false_fixups.push(instructions.len());
            instructions.push(Instruction::If_icmpne(0));
        }
        Type::String => {
            let string_class_idx = cp.add_class("java/lang/String")?;
            let equals_ref =
                cp.add_method_ref(string_class_idx, "equals", "(Ljava/lang/Object;)Z")?;
            instructions.push(Instruction::Invokevirtual(equals_ref));
            append_boolean_false_check(instructions, false_fixups);
        }
        Type::Class(class_name) if has_generated_eq(module, class_name) => {
            let class_idx = cp.add_class(class_name)?;
            let eq_desc = format!("(L{};)Z", class_name);
            let eq_ref = cp.add_method_ref(class_idx, "eq", &eq_desc)?;
            instructions.push(Instruction::Invokevirtual(eq_ref));
            append_boolean_false_check(instructions, false_fixups);
        }
        Type::Interface(interface_name) if has_generated_eq(module, interface_name) => {
            let interface_idx = cp.add_class(interface_name)?;
            let eq_desc = format!("(L{};)Z", interface_name);
            let eq_ref = cp.add_interface_method_ref(interface_idx, "eq", &eq_desc)?;
            instructions.push(Instruction::Invokeinterface(eq_ref, 2));
            append_boolean_false_check(instructions, false_fixups);
        }
        Type::Class(_) | Type::Interface(_) => {
            let object_class_idx = cp.add_class("java/lang/Object")?;
            let equals_ref =
                cp.add_method_ref(object_class_idx, "equals", "(Ljava/lang/Object;)Z")?;
            instructions.push(Instruction::Invokevirtual(equals_ref));
            append_boolean_false_check(instructions, false_fixups);
        }
        _ => {
            false_fixups.push(instructions.len());
            instructions.push(Instruction::If_acmpne(0));
        }
    }

    Ok(())
}

/// Creates a ClassFile (as bytes) for a given OOMIR DataType that's a class
pub(super) fn create_data_type_classfile_for_class(
    // pub(super) or pub(crate)
    class_name_jvm: &str,
    fields: Vec<(String, Type)>,
    is_abstract: bool,
    methods: HashMap<String, DataTypeMethod>,
    super_class_name_jvm: &str,
    implements_interfaces: Vec<String>,
    module: &oomir::Module,
    subclasses: Vec<String>,
    nest_host: Option<String>,
) -> jvm::Result<Vec<u8>> {
    let mut cp = InternedConstantPool::default();

    let this_class_index = cp.add_class(class_name_jvm)?;

    let super_class_index = cp.add_class(super_class_name_jvm)?;

    let mut seen_interfaces = HashSet::new();
    let mut interface_indices: Vec<u16> = Vec::with_capacity(implements_interfaces.len());
    for interface_name in &implements_interfaces {
        if !seen_interfaces.insert(interface_name.as_str()) {
            continue;
        }
        // Add the interface name to the constant pool as a Class reference
        let interface_index = cp.add_class(interface_name)?;
        interface_indices.push(interface_index);
    }

    let mut jvm_fields: Vec<jvm::Field> = Vec::new();
    for (field_name, field_ty) in &fields {
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
        jvm_fields.push(field);
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!("  - Added field: {} {}", field_name, descriptor)
        );
    }

    // Fielded Rust structs/enums must be initialized with all fields. Only genuinely
    // fieldless classes keep a no-args constructor.
    let constructor = if fields.is_empty() {
        create_default_constructor(&mut cp, super_class_index)?
    } else {
        create_field_constructor(&mut cp, this_class_index, super_class_index, &fields)?
    };
    let mut jvm_methods = vec![constructor];
    let mut class_attributes = Vec::new();

    // Check for jvm_methods
    for (method_name, method) in methods.iter() {
        match method {
            DataTypeMethod::SimpleConstantReturn(return_type, return_const) => {
                let method_desc = format!("(){}", return_type.to_jvm_descriptor());

                // Add the method to the class file
                let name_index = cp.add_utf8(&method_name)?;
                let descriptor_index: u16 = cp.add_utf8(method_desc)?;

                let mut attributes = vec![];
                let mut is_abstract = false;

                match return_const {
                    Some(rc) => attributes.push(create_code_from_method_name_and_constant_return(
                        &rc, &mut cp,
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

                jvm_methods.push(jvm_method);
            }
            DataTypeMethod::Function(function) => {
                let instrumented_fn_name = format!("{class_name_jvm}::{method_name}");
                let _timer =
                    crate::instrumentation::Timer::function("lower2", None, &instrumented_fn_name);

                // Translate the function body using its own constant pool reference
                let owner_class = if !function.signature.is_static {
                    Some(class_name_jvm)
                } else {
                    None
                };
                let translator = FunctionTranslator::new(
                    function,
                    &mut cp,
                    module,
                    function.signature.is_static,
                    owner_class,
                );
                let (jvm_code, max_locals_val, code_attributes) =
                    translator
                        .translate()
                        .map_err(|error| jvm::Error::VerificationError {
                            context: format!("Function {class_name_jvm}::{method_name}"),
                            message: format!("Failed to translate function: {error:?}"),
                        })?;

                let stack_floor = oomir_function_stack_floor(function);
                let max_stack_val = match jvm_code.max_stack(&cp) {
                    Ok(max_stack) => max_stack.saturating_mul(2).max(stack_floor),
                    Err(error) => {
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "bytecode-gen",
                            format!(
                                "Falling back to conservative max_stack for {}::{} after max_stack failed: {:?}",
                                class_name_jvm, method_name, error
                            )
                        );
                        stack_floor.max(1024)
                    }
                };

                let code_attribute = Attribute::Code {
                    name_index: cp.add_utf8("Code")?,
                    max_stack: max_stack_val,
                    max_locals: max_locals_val,
                    code: jvm_code,
                    exception_table: Vec::new(),
                    attributes: code_attributes,
                };

                // Create MethodParameters attribute to preserve parameter names
                // For instance methods where the first param is self, skip it as it's implicit in JVM
                let mut parameters_for_attribute = Vec::new();
                let has_self =
                    !function.signature.is_static && !function.signature.params.is_empty() && {
                        matches!(
                            &function.signature.params[0].1,
                            Type::Class(_) | Type::MutableReference(_)
                        )
                    };
                let params_to_iterate = if has_self {
                    &function.signature.params[1..]
                } else {
                    &function.signature.params[..]
                };
                for (name, _) in params_to_iterate {
                    let name_index = cp.add_utf8(name)?;
                    parameters_for_attribute.push(jvm::attributes::MethodParameter {
                        name_index,
                        access_flags: MethodAccessFlags::empty(), // No special flags
                    });
                }
                let method_parameters_attribute_name_index = cp.add_utf8("MethodParameters")?;
                let method_parameters_attribute = Attribute::MethodParameters {
                    name_index: method_parameters_attribute_name_index,
                    parameters: parameters_for_attribute,
                };

                let name_index = cp.add_utf8(method_name)?;
                let descriptor_index = cp.add_utf8(&function.signature.to_string())?;

                let mut attributes_vec = vec![code_attribute];
                // Skip MethodParameters for constructors and getVariantIdx
                if method_name != "<init>" && method_name != "getVariantIdx" {
                    attributes_vec.push(method_parameters_attribute);
                }

                let mut access_flags = MethodAccessFlags::PUBLIC;
                if function.signature.is_static {
                    access_flags |= MethodAccessFlags::STATIC;
                }
                let jvm_method = jvm::Method {
                    access_flags,
                    name_index,
                    descriptor_index,
                    attributes: attributes_vec,
                };

                jvm_methods.push(jvm_method);
                if !function.signature.is_static && !function.signature.params.is_empty() {
                    jvm_methods.push(create_static_instance_bridge(
                        &mut cp,
                        class_name_jvm,
                        method_name,
                        function,
                    )?);
                }
            }
            DataTypeMethod::AdtHelperMethod { kind } => {
                let jvm_method = match kind {
                    AdtHelperKind::IsVariant { variant_idx } => {
                        // Signature: ()Z - returns boolean
                        let method_desc = "()Z";
                        let name_index = cp.add_utf8(method_name)?;
                        let descriptor_index = cp.add_utf8(method_desc)?;

                        // Get the getVariantIdx method reference on THIS class
                        let this_class_idx = this_class_index;
                        let get_variant_idx_ref =
                            cp.add_method_ref(this_class_idx, "getVariantIdx", "()I")?;

                        let idx = *variant_idx as u16;
                        // Offsets are instruction indices, not byte offsets:
                        // 0: aload_0
                        // 1: invokevirtual - getVariantIdx()
                        // 2: iconst_X     - push variant index to compare
                        // 3: if_icmpne 6  - if not equal, jump to instruction 6 (iconst_0)
                        // 4: iconst_1     - push true
                        // 5: goto 7       - jump to instruction 7 (ireturn)
                        // 6: iconst_0     - push false
                        // 7: ireturn
                        let push_iconst = match idx {
                            0 => Instruction::Iconst_0,
                            1 => Instruction::Iconst_1,
                            2 => Instruction::Iconst_2,
                            3 => Instruction::Iconst_3,
                            4 => Instruction::Iconst_4,
                            5 => Instruction::Iconst_5,
                            _ => Instruction::Bipush(idx as i8),
                        };

                        let instructions = vec![
                            Instruction::Aload_0,
                            Instruction::Invokevirtual(get_variant_idx_ref),
                            push_iconst,
                            Instruction::If_icmpne(6), // Jump to instruction index 6
                            Instruction::Iconst_1,
                            Instruction::Goto(7), // Jump to instruction index 7
                            Instruction::Iconst_0,
                            Instruction::Ireturn,
                        ];

                        let max_stack = 2u16;
                        let max_locals = 1u16;

                        let code_attribute = code_attribute_for_descriptor(
                            &mut cp,
                            max_stack,
                            max_locals,
                            instructions,
                            method_desc,
                            false,
                            Some(class_name_jvm),
                            method_name,
                        )?;

                        jvm::Method {
                            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::FINAL,
                            name_index,
                            descriptor_index,
                            attributes: vec![code_attribute],
                        }
                    }
                    AdtHelperKind::PartialEqEnum { variants } => {
                        // Signature: (LObject;)Z - takes another enum instance, returns boolean
                        let method_desc = format!("(L{};)Z", class_name_jvm);
                        let name_index = cp.add_utf8(method_name)?;
                        let descriptor_index = cp.add_utf8(&method_desc)?;

                        // Get the getVariantIdx method reference on THIS class
                        let this_class_idx = this_class_index;
                        let get_variant_idx_ref =
                            cp.add_method_ref(this_class_idx, "getVariantIdx", "()I")?;

                        let mut instructions = vec![
                            Instruction::Aload_0,
                            Instruction::Invokevirtual(get_variant_idx_ref),
                            Instruction::Aload_1,
                            Instruction::Invokevirtual(get_variant_idx_ref),
                        ];
                        let mut false_fixups = vec![instructions.len()];
                        instructions.push(Instruction::If_icmpne(0));

                        for (variant_idx, (variant_name, fields)) in variants.iter().enumerate() {
                            if fields.is_empty() {
                                continue;
                            }

                            let variant_class_name = format!("{class_name_jvm}${variant_name}");
                            if !module.data_types.contains_key(&variant_class_name) {
                                continue;
                            }

                            instructions.push(Instruction::Aload_0);
                            instructions.push(Instruction::Invokevirtual(get_variant_idx_ref));
                            instructions.push(get_int_const_instr(&mut cp, variant_idx as i32));
                            let next_variant_fixup = instructions.len();
                            instructions.push(Instruction::If_icmpne(0));

                            let variant_class_idx = cp.add_class(&variant_class_name)?;

                            for (field_idx, field_ty) in fields.iter().enumerate() {
                                append_field_equality_check(
                                    module,
                                    &mut cp,
                                    &mut instructions,
                                    &mut false_fixups,
                                    variant_class_idx,
                                    &format!("field{field_idx}"),
                                    field_ty,
                                )?;
                            }

                            instructions.push(Instruction::Iconst_1);
                            instructions.push(Instruction::Ireturn);

                            let next_variant_target = instructions.len() as u16;
                            patch_branch_target(
                                &mut instructions,
                                next_variant_fixup,
                                next_variant_target,
                            );
                        }

                        instructions.push(Instruction::Iconst_1);
                        instructions.push(Instruction::Ireturn);

                        let false_target = instructions.len() as u16;
                        instructions.push(Instruction::Iconst_0);
                        instructions.push(Instruction::Ireturn);

                        for fixup in false_fixups {
                            patch_branch_target(&mut instructions, fixup, false_target);
                        }

                        let max_stack = 4u16;
                        let max_locals = 2u16;

                        let code_attribute = code_attribute_for_descriptor(
                            &mut cp,
                            max_stack,
                            max_locals,
                            instructions,
                            &method_desc,
                            false,
                            Some(class_name_jvm),
                            method_name,
                        )?;

                        jvm::Method {
                            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::FINAL,
                            name_index,
                            descriptor_index,
                            attributes: vec![code_attribute],
                        }
                    }
                    AdtHelperKind::PartialEqClass { fields } => {
                        let method_desc = format!("(L{};)Z", class_name_jvm);
                        let name_index = cp.add_utf8(method_name)?;
                        let descriptor_index = cp.add_utf8(&method_desc)?;

                        let this_class_idx = this_class_index;
                        let mut instructions = Vec::new();
                        let mut false_fixups = Vec::new();

                        for (field_name, field_ty) in fields {
                            append_field_equality_check(
                                module,
                                &mut cp,
                                &mut instructions,
                                &mut false_fixups,
                                this_class_idx,
                                field_name,
                                field_ty,
                            )?;
                        }

                        instructions.push(Instruction::Iconst_1);
                        instructions.push(Instruction::Ireturn);

                        if !false_fixups.is_empty() {
                            let false_target = instructions.len() as u16;
                            instructions.push(Instruction::Iconst_0);
                            instructions.push(Instruction::Ireturn);

                            for fixup in false_fixups {
                                patch_branch_target(&mut instructions, fixup, false_target);
                            }
                        }

                        let code_attribute = code_attribute_for_descriptor(
                            &mut cp,
                            4,
                            2,
                            instructions,
                            &method_desc,
                            false,
                            Some(class_name_jvm),
                            method_name,
                        )?;

                        jvm::Method {
                            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::FINAL,
                            name_index,
                            descriptor_index,
                            attributes: vec![code_attribute],
                        }
                    }
                };
                jvm_methods.push(jvm_method);
            }
        }
    }

    if !subclasses.is_empty() || nest_host.is_some() {
        let mut inner_classes_vec: Vec<InnerClass> = Vec::with_capacity(subclasses.len());

        for subclass_name in &subclasses {
            // Ensure subclass class_info is in the constant pool
            let class_info_index = cp.add_class(subclass_name)?;

            // The outer class is this class
            let outer_class_info_index = this_class_index;

            // Derive simple name: part after last '$'. If there's no '$', treat as unnamed (0).
            let simple_name_part = subclass_name.rsplit('$').next().unwrap_or(subclass_name);

            // If the simple name looks like an anonymous class (all digits), set name_index = 0
            let name_index = if simple_name_part.chars().all(|c| c.is_ascii_digit()) {
                0
            } else if simple_name_part == *subclass_name && !subclass_name.contains('$') {
                // No '$' present -> not an inner/member class; leave name_index = 0
                0
            } else {
                cp.add_utf8(simple_name_part)?
            };

            // Default to PUBLIC | STATIC for generated nested classes. This can be adjusted
            // if more precise access info becomes available.
            let access_flags = NestedClassAccessFlags::PUBLIC | NestedClassAccessFlags::STATIC;

            inner_classes_vec.push(InnerClass {
                class_info_index,
                outer_class_info_index,
                name_index,
                access_flags,
            });
        }

        // If this class has a nest host, add it as well
        // make it like [us]=class Host$[us] of class Host
        if let Some(nest_host_name) = nest_host {
            let class_info_index = cp.add_class(class_name_jvm)?;
            let outer_class_info_index = cp.add_class(&nest_host_name)?;
            let name_index =
                cp.add_utf8(class_name_jvm.rsplit('$').next().unwrap_or(class_name_jvm))?;
            let access_flags = NestedClassAccessFlags::PUBLIC | NestedClassAccessFlags::STATIC;
            inner_classes_vec.push(InnerClass {
                class_info_index,
                outer_class_info_index,
                name_index,
                access_flags,
            });
        }

        let inner_classes_attr_name_index = cp.add_utf8("InnerClasses")?;
        class_attributes.push(Attribute::InnerClasses {
            name_index: inner_classes_attr_name_index,
            classes: inner_classes_vec,
        });
    }

    let simple_name = class_name_jvm.split('/').last().unwrap_or(class_name_jvm);
    let source_file_name = format!("{}.rs", simple_name);
    let source_file_utf8_index = cp.add_utf8(&source_file_name)?;
    let source_file_attr_name_index = cp.add_utf8("SourceFile")?;
    class_attributes.push(Attribute::SourceFile {
        name_index: source_file_attr_name_index,
        source_file_index: source_file_utf8_index,
    });

    let class_file = ClassFile {
        version: Version::Java8 { minor: 0 },
        constant_pool: cp.into_inner(),
        access_flags: ClassAccessFlags::PUBLIC
            | ClassAccessFlags::SUPER
            | if is_abstract {
                ClassAccessFlags::ABSTRACT
            } else {
                ClassAccessFlags::FINAL
            },
        this_class: this_class_index,
        super_class: super_class_index,
        interfaces: interface_indices,
        fields: jvm_fields,
        methods: jvm_methods,
        attributes: class_attributes,
    };
    verify_no_duplicate_constants(&class_file)?;

    let mut byte_vector = Vec::new();
    class_file
        .to_bytes(&mut byte_vector)
        .map_err(|error| jvm::Error::VerificationError {
            context: format!("Class {class_name_jvm}"),
            message: format!("Failed to serialize class file: {error:?}"),
        })?;

    Ok(byte_vector)
}

/// Creates a ClassFile (as bytes) for a given OOMIR DataType that's an interface
pub(super) fn create_data_type_classfile_for_interface(
    interface_name_jvm: &str, // Renamed for clarity
    methods: &HashMap<String, Signature>,
) -> jvm::Result<Vec<u8>> {
    let mut cp = InternedConstantPool::default();

    let this_class_index = cp.add_class(interface_name_jvm)?;

    // Interfaces always implicitly extend Object, and must specify it in the classfile
    let super_class_index = cp.add_class("java/lang/Object")?;

    let mut jvm_methods: Vec<jvm::Method> = Vec::new();
    for (method_name, signature) in methods {
        // Construct the descriptor: (param1_desc param2_desc ...)return_desc
        let mut descriptor = String::from("(");
        for (_param_name, param_type) in &signature.params {
            descriptor.push_str(&param_type.to_jvm_descriptor());
        }
        descriptor.push(')');
        descriptor.push_str(&signature.ret.to_jvm_descriptor());

        let name_index = cp.add_utf8(method_name)?;
        let descriptor_index = cp.add_utf8(&descriptor)?;

        let flags = if signature.is_static {
            MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC | MethodAccessFlags::ABSTRACT
        } else {
            MethodAccessFlags::PUBLIC | MethodAccessFlags::ABSTRACT
        };

        // Interface methods are implicitly public and abstract (unless 'default' or 'static')
        // We assume these are the standard abstract interface methods.
        let jvm_method = jvm::Method {
            access_flags: flags,
            name_index,
            descriptor_index,
            attributes: Vec::new(), // Abstract methods have no Code attribute
        };
        jvm_methods.push(jvm_method);
        // Consider using tracing or logging
        breadcrumbs::log!(
            breadcrumbs::LogLevel::Info,
            "bytecode-gen",
            format!("  - Added interface method: {} {}", method_name, descriptor)
        );
    }

    let mut class_attributes = Vec::new();

    let simple_name = interface_name_jvm
        .split('/')
        .last()
        .unwrap_or(interface_name_jvm);
    let source_file_name = format!("{}.rs", simple_name); // Or .java
    let source_file_utf8_index = cp.add_utf8(&source_file_name)?;
    let source_file_attr_name_index = cp.add_utf8("SourceFile")?;
    class_attributes.push(Attribute::SourceFile {
        name_index: source_file_attr_name_index,
        source_file_index: source_file_utf8_index,
    });

    let class_file = ClassFile {
        version: Version::Java8 { minor: 0 },
        constant_pool: cp.into_inner(),
        access_flags: ClassAccessFlags::PUBLIC
            | ClassAccessFlags::INTERFACE
            | ClassAccessFlags::ABSTRACT,
        this_class: this_class_index,
        super_class: super_class_index,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: jvm_methods,
        attributes: class_attributes,
    };
    verify_no_duplicate_constants(&class_file)?;

    let mut byte_vector = Vec::new();
    class_file
        .to_bytes(&mut byte_vector)
        .map_err(|error| jvm::Error::VerificationError {
            context: format!("Interface {interface_name_jvm}"),
            message: format!("Failed to serialize class file: {error:?}"),
        })?;

    Ok(byte_vector)
}

/// Creates a code attribute for a method that returns a constant value.
fn create_code_from_method_name_and_constant_return(
    return_const: &oomir::Constant,
    cp: &mut InternedConstantPool,
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
        | oomir::Type::Class(_)
        | oomir::Type::Interface(_) => Instruction::Areturn,
        oomir::Type::Void => Instruction::Return, // Should not happen with Some(op)
    };

    instructions.push(return_instr);

    let max_stack = get_type_size(&return_ty).max(1);
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
