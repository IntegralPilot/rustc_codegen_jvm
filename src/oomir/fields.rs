//! The physical field ABI for compiler-owned value carriers.
use super::Type;

/// Reserve room for the receiver in the JVM's 255-unit constructor limit.
pub fn split_borrows(fields: &[(String, Type)]) -> bool {
    fields.iter().any(|(_, ty)| ty.component_shape().is_some())
        && fields
            .iter()
            .map(|(_, ty)| {
                if let Some(shape) = ty.component_shape() {
                    shape.slots()
                } else {
                    match ty {
                        Type::I64 | Type::U64 | Type::F64 => 2,
                        Type::Unit | Type::Void => 0,
                        _ => 1,
                    }
                }
            })
            .sum::<usize>()
            <= 254
}

pub fn components(name: &str, ty: &Type) -> Option<Vec<(String, Type)>> {
    let names = if matches!(ty, Type::TaggedI64) {
        jvm_compiler_core::jvm::abi::tagged_field_names(name).to_vec()
    } else if matches!(ty, Type::Pointer(_)) {
        vec![name.into(), displacement_name(name, ty)]
    } else if matches!(ty, Type::Slice(_) | Type::Str) {
        jvm_compiler_core::jvm::abi::view_field_names(name, matches!(ty, Type::Str)).to_vec()
    } else {
        return None;
    };
    Some(names.into_iter().zip(ty.components().unwrap()).collect())
}

pub fn displacement_name(name: &str, ty: &Type) -> String {
    if let Type::Pointer(p) = ty
        && let Some(layout) = &p.layout
    {
        jvm_compiler_core::jvm::abi::typed_address_field_name(name, layout.size)
    } else {
        jvm_compiler_core::jvm::abi::address_field_name(name, ty.address_plan())
    }
}

pub fn physical(fields: &[(String, Type)]) -> Vec<(String, Type)> {
    let split = split_borrows(fields);
    fields
        .iter()
        .flat_map(|(name, ty)| {
            if let Some(parts) = components(name, ty).filter(|_| split) {
                parts
            } else {
                vec![(name.clone(), ty.clone())]
            }
        })
        .collect()
}
