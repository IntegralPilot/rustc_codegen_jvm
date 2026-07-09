// checked_intrinsic_registry.rs
// Global registry to track which checked arithmetic intrinsics are needed

use once_cell::sync::Lazy;
use std::collections::HashSet;
use std::sync::Mutex;

/// Global registry of needed checked arithmetic intrinsics.
/// Format: (operation, type, result tuple class), e.g. ("add", "i32", "test/Tuple_i32_bool").
static NEEDED_INTRINSICS: Lazy<Mutex<HashSet<(String, String, String)>>> =
    Lazy::new(|| Mutex::new(HashSet::new()));

/// Register that a checked arithmetic intrinsic is needed
pub fn register_intrinsic(operation: &str, ty: &str, result_tuple_class: &str) {
    let mut registry = NEEDED_INTRINSICS.lock().unwrap();
    registry.insert((
        operation.to_string(),
        ty.to_string(),
        result_tuple_class.to_string(),
    ));
}

/// Get all registered intrinsics and clear the registry
pub fn take_needed_intrinsics() -> Vec<(String, String, String)> {
    let mut registry = NEEDED_INTRINSICS.lock().unwrap();
    let intrinsics: Vec<_> = registry.iter().cloned().collect();
    registry.clear();
    intrinsics
}
