//! JVM names shared by body selection and generated representation schemas.
pub const SLICE_VIEW_CLASS: &str = "org/rustlang/runtime/SliceView";
pub const UTF8_VIEW_CLASS: &str = "org/rustlang/runtime/Utf8View";
pub const TAGGED_LONG_CLASS: &str = "org/rustlang/runtime/TaggedLong";
pub const POINTER_CLASS: &str = "org/rustlang/runtime/Pointer";

/// Reconstruction plans for addresses of stored borrows. These are ABI tags.
/// Ordinary scalar plans retain their 1/2/4/8-byte strides.
pub const STORED_VIEW: u32 = 64;
pub const STORED_ADDRESS: u32 = 128;

pub fn address_plan(types: &crate::ir::Types, pointee: crate::ir::TypeId) -> u32 {
    use crate::ir::{StorageSlot, Type};
    match types.get(pointee) {
        Some(Type::Slice(_) | Type::Str) => STORED_VIEW,
        Some(Type::Pointer(_)) => STORED_ADDRESS,
        _ => StorageSlot::scalar(pointee, types).map_or(0, |slot| slot.size),
    }
}

/// Consume an Object root and long displacement.
/// Construct a boundary carrier only when a consumer needs one reference.
pub fn materialize_address(
    cp: &mut crate::classfile::constant_pool::InternedConstantPool,
    code: &mut Vec<crate::classfile::attributes::Instruction>,
    plan: u32,
) -> crate::classfile::Result<()> {
    use crate::classfile::attributes::Instruction;
    code.push(crate::jvm::constants::get_int_const_instr(cp, plan as i32));
    let owner = cp.add_class(POINTER_CLASS)?;
    code.push(Instruction::Invokestatic(cp.add_method_ref(
        owner,
        "addressFromParts",
        "(Ljava/lang/Object;JI)Lorg/rustlang/runtime/Pointer;",
    )?));
    Ok(())
}

pub fn materialize_typed_address(
    cp: &mut crate::classfile::constant_pool::InternedConstantPool,
    code: &mut Vec<crate::classfile::attributes::Instruction>,
    size: u32,
    codec: Option<&str>,
) -> crate::classfile::Result<()> {
    use crate::classfile::attributes::Instruction;
    code.push(crate::jvm::constants::get_int_const_instr(cp, size as i32));
    code.push(if let Some(codec) = codec {
        Instruction::Ldc_w(cp.add_name_string(codec)?)
    } else {
        Instruction::Aconst_null
    });
    let owner = cp.add_class(POINTER_CLASS)?;
    code.push(Instruction::Invokestatic(cp.add_method_ref(
        owner,
        "fromTypedStorageLocation",
        "(Ljava/lang/Object;JILjava/lang/String;)Lorg/rustlang/runtime/Pointer;",
    )?));
    Ok(())
}

pub const VIEW_PARTS: [(&str, &str); 3] = [
    ("array", "Ljava/lang/Object;"),
    ("offset", "I"),
    ("rustLength", "J"),
];

/// Use null for an absent optional borrow at boundaries.
/// All adapters use the same default component slots for this niche.
pub fn view_part_access(
    cp: &mut crate::classfile::constant_pool::InternedConstantPool,
    index: usize,
) -> crate::classfile::Result<crate::classfile::attributes::Instruction> {
    let (name, descriptor) = VIEW_PARTS[index];
    let owner = cp.add_class(SLICE_VIEW_CLASS)?;
    Ok(crate::classfile::attributes::Instruction::Invokestatic(
        cp.add_method_ref(
            owner,
            format!("$part${name}"),
            format!("(L{SLICE_VIEW_CLASS};){descriptor}"),
        )?,
    ))
}
/// Synthetic displacement field paired with a scalar-address root field.
pub fn tagged_field_names(name: &str) -> [String; 2] {
    [name.into(), format!("$rust$t${name}")]
}
pub fn address_field_name(name: &str, size: u32) -> String {
    format!("$rust${size}${name}")
}
/// Exact layouts are metadata on the carrier schema, not on every borrow.
pub fn typed_address_field_name(name: &str, size: u32) -> String {
    format!("$rust$a{size}${name}")
}
pub fn address_codec_field_name(name: &str) -> String {
    format!("$rust$c${name}")
}
pub fn address_displacement_name(
    types: &crate::ir::Types,
    pointer: crate::ir::TypeId,
    name: &str,
) -> String {
    if let Some((size, _)) = types.address_layout(pointer) {
        typed_address_field_name(name, size)
    } else {
        let Some(crate::ir::Type::Pointer(inner)) = types.get(pointer) else {
            unreachable!()
        };
        address_field_name(name, address_plan(types, inner))
    }
}
pub fn view_field_names(name: &str, utf8: bool) -> [String; 3] {
    [
        name.into(),
        format!("$rust${}${name}", if utf8 { "u" } else { "s" }),
        format!("$rust$l${name}"),
    ]
}
