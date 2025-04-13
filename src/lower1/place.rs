use super::types::ty_to_oomir_type;
use crate::oomir;

use regex::Regex;
use rustc_middle::{
    mir::{Body, Place},
    ty::{Ty, TyCtxt},
};
use std::{collections::HashMap, sync::OnceLock};

pub fn place_to_string<'tcx>(place: &Place<'tcx>, _tcx: TyCtxt<'tcx>) -> String {
    // Base variable name (e.g., "_1")
    let mut name = format!("_{}", place.local.index()); // Start with base local "_N"

    // Append projections cleanly
    for proj_elem in place.projection.iter() {
        match proj_elem {
            rustc_middle::mir::ProjectionElem::Field(field, _ty) => {
                // Append ".index" for field access
                name.push_str(&format!(".{}", field.index()));
            }
            // --- Handle other projections as needed, cleanly ---
            rustc_middle::mir::ProjectionElem::Deref => {
                name = format!("deref_{}", name); // Or maybe just skip for simple cases?
            }
            rustc_middle::mir::ProjectionElem::Index(local) => {
                name.push_str(&format!(".idx_{}", local.index())); // Example for index
            }
            // ... etc ...
            _ => {
                name.push_str(".?"); // Placeholder for others
            }
        }
    }
    name // Return the simple, projected name (e.g., "_7.0")
}

// Make a rust idenfier (i.e. function name) JVM safe
// i.e. Arguments::<'_>::new_const::<1> -> arguments_new_const
// and <String as PartialEq<&str>>::eq -> eq
pub fn make_jvm_safe(input: &str) -> String {
    static RE_ANGLES: OnceLock<Regex> = OnceLock::new();
    static RE_COLONS: OnceLock<Regex> = OnceLock::new();
    static RE_TRAILING_UNDERSCORE: OnceLock<Regex> = OnceLock::new();
    // 1. Remove anything inside < > (including the angle brackets themselves),
    // but keep the inner thing if it is a Rust primitive type.
    let re_angles = RE_ANGLES.get_or_init(|| Regex::new(r"<([^>]+)>").expect("Invalid regex"));
    let without_angles = re_angles.replace_all(input, |caps: &regex::Captures| {
        let inner = &caps[1];
        match inner {
            "i8" | "i16" | "i32" | "i64" | "i128" | "u8" | "u16" | "u32" | "u64" | "u128"
            | "f32" | "f64" | "bool" | "char" | "str" => inner.to_string(),
            _ => "".to_string(),
        }
    });

    // 2. Replace occurrences of :: (or more consecutive ':') with a single underscore
    let re_colons = RE_COLONS.get_or_init(|| Regex::new(r":{2,}").expect("Invalid regex"));
    let replaced: std::borrow::Cow<'_, str> = re_colons.replace_all(&without_angles, "_");

    // 3. ensure there are no starting or trailing > or < or _
    let re_trailing_underscore = RE_TRAILING_UNDERSCORE
        .get_or_init(|| Regex::new(r"^[<>_]+|[<>_]+$").expect("Invalid regex"));
    let replaced = re_trailing_underscore.replace_all(&replaced, "");

    // 4. Convert all to lowercase
    let lower = replaced.to_lowercase();

    // 5. if the first or last character is _, rm it
    let trimmed = lower.trim_start_matches('_').trim_end_matches('_');

    trimmed.to_string()
}

// --- Helper to get OOMIR Type for a Place ---
pub fn get_place_type<'tcx>(
    place: &Place<'tcx>,
    mir: &Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::Type {
    let place_ty = place.ty(&mir.local_decls, tcx);
    ty_to_oomir_type(place_ty.ty, tcx, data_types)
}

// Helper to check if a place involves a field projection and extract info
pub fn extract_base_and_field<'tcx>(
    tcx: TyCtxt<'tcx>,
    place: &Place<'tcx>,
) -> Option<(Place<'tcx>, rustc_abi::FieldIdx, Ty<'tcx>)> {
    if let Some(proj_elem) = place.projection.last() {
        if let rustc_middle::mir::ProjectionElem::Field(field_index, field_ty) = proj_elem {
            // Create a new Place representing the base (without the last field projection)
            // This requires creating a new Place which isn't trivial as Place is complex.
            // A simpler approach for now is to assume the base is just the local.
            // WARNING: This simplification currently doesn't handle nested field access like _1.0.1 correctly.
            // For simple cases like _1.0 it works.
            // TODO: Handle nested field access properly.
            if place.projection.len() == 1 {
                // Only handle direct field access on local for now
                let base_place = Place {
                    local: place.local,
                    projection: tcx.mk_place_elems(&[]), // Empty projection for the base
                };
                // We need the actual MIR Ty<'tcx> of the field, which proj_elem gives us.
                return Some((base_place, *field_index, *field_ty));
            }
        }
    }
    None
}
