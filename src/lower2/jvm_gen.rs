//! Class assembly and shared JVM method recipes.
mod body;
pub(super) use body::{BodyEmitter, BodyOwner};
mod constructors;
pub(super) use constructors::create_default_constructor;
use constructors::{
    create_field_constructor, create_managed_copy_method, create_relative_pointer_field_constructor,
};
mod slices;
pub(super) use slices::create_slice_view_classfile;
mod strings;
pub(super) use strings::create_utf8_view_classfile;
mod bridges;
pub(super) use bridges::create_relative_pointer_bridge;
use bridges::create_static_instance_bridge;
mod enums;
use enums::{append_field_equality_check, create_enum_adt_helper_method, patch_branch_target};
mod classes;
pub(super) use classes::create_data_type_classfile_for_class;
mod interfaces;
pub(super) use interfaces::create_data_type_classfile_for_interface;

mod forward;

use super::{
    DebugInfoOptions,
    constant_pool::{InternedConstantPool, verify_no_duplicate_constants},
    constants::{get_int_const_instr, get_long_const_instr, load_constant},
    helpers::{
        get_cast_instructions, get_load_instruction, get_type_size, return_instruction_for_type,
    },
    stackmaps,
};
use crate::oomir::{self, AdtHelperKind, DataTypeMethod, Signature, Type};

use super::jvm::{
    self, BaseType, ClassAccessFlags, ClassFile, FieldAccessFlags, MethodAccessFlags, Version,
    attributes::{Attribute, BootstrapMethod, InnerClass, Instruction, NestedClassAccessFlags},
};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

fn code_attribute_with_stack_maps(
    cp: &mut InternedConstantPool,
    max_locals: u16,
    mut code: Vec<Instruction>,
    initial_locals: Vec<stackmaps::FrameValue>,
    context: &str,
) -> jvm::Result<Attribute> {
    stackmaps::move_zero_branch_target(&mut code, context)?;
    let analysis = stackmaps::analyze(
        &code,
        &initial_locals,
        &[],
        usize::from(max_locals),
        cp,
        context,
        &[],
    )?;
    let attributes = stackmaps::build_stack_map_attributes_from_analysis(
        &code,
        &initial_locals,
        cp,
        &[],
        &analysis,
    )?;
    Ok(Attribute::Code {
        name_index: cp.add_utf8("Code")?,
        max_stack: analysis.max_stack,
        max_locals,
        code,
        exception_table: Vec::new(),
        attributes,
    })
}

fn code_attribute_for_descriptor(
    cp: &mut InternedConstantPool,
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
    code_attribute_with_stack_maps(cp, max_locals, code, initial_locals, method_name)
}

/// Converts an OOMIR Type to a Ristretto FieldType for class field definitions.
pub(super) fn oomir_type_to_ristretto_field_type(type2: &oomir::Type) -> jvm::FieldType {
    match type2 {
        oomir::Type::I8 | oomir::Type::U8 => jvm::FieldType::Base(BaseType::Byte),
        oomir::Type::I16 | oomir::Type::F16 => jvm::FieldType::Base(BaseType::Short),
        oomir::Type::U16 => jvm::FieldType::Base(BaseType::Char),
        oomir::Type::I32 | oomir::Type::U32 => jvm::FieldType::Base(BaseType::Int),
        oomir::Type::I64 | oomir::Type::U64 => jvm::FieldType::Base(BaseType::Long),
        oomir::Type::F32 => jvm::FieldType::Base(BaseType::Float),
        oomir::Type::F64 => jvm::FieldType::Base(BaseType::Double),
        oomir::Type::Boolean => jvm::FieldType::Base(BaseType::Boolean),
        oomir::Type::Char => jvm::FieldType::Base(BaseType::Char),
        oomir::Type::Str => jvm::FieldType::Object(oomir::UTF8_VIEW_CLASS.into()),
        oomir::Type::Reference(ref2) => {
            let inner_ty = ref2.as_ref();
            oomir_type_to_ristretto_field_type(inner_ty)
        }
        oomir::Type::Pointer(_) => jvm::FieldType::Object(oomir::POINTER_CLASS.into()),
        oomir::Type::Array(inner_ty) => {
            let inner_field_type = if inner_ty.has_jvm_value() {
                oomir_type_to_ristretto_field_type(inner_ty)
            } else {
                jvm::FieldType::Object("java/lang/Object".into())
            };
            jvm::FieldType::Array(Box::new(inner_field_type))
        }
        oomir::Type::Slice(_) => jvm::FieldType::Object(oomir::SLICE_VIEW_CLASS.into()),
        oomir::Type::MutableReference(inner_ty) if !inner_ty.has_jvm_value() => {
            jvm::FieldType::Object("java/lang/Object".into())
        }
        oomir::Type::MutableReference(inner_ty) => {
            let inner_field_type = oomir_type_to_ristretto_field_type(inner_ty);
            jvm::FieldType::Array(Box::new(inner_field_type))
        }
        oomir::Type::Class(name) | oomir::Type::Interface(name) => {
            jvm::FieldType::Object(name.clone().into())
        }
        oomir::Type::Void => {
            panic!("Void type cannot be used as a field type");
        }
        oomir::Type::Unit => {
            panic!("Unit has no JVM field representation");
        }
    }
}

/// Constant methods use the same exact finalization as other native recipes.
fn create_code_from_method_name_and_constant_return(
    value: &oomir::Constant,
    cp: &mut InternedConstantPool,
) -> jvm::Result<Attribute> {
    let mut code = Vec::new();
    load_constant(&mut code, cp, value)?;
    code.push(return_instruction_for_type(&Type::from_constant(value)));
    code_attribute_with_stack_maps(cp, 1, code, Vec::new(), "constant return")
}
