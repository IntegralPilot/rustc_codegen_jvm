// checked_intrinsic_registry.rs
// Global registry to track which checked arithmetic intrinsics are needed

use std::collections::HashSet;
use std::sync::Mutex;
use once_cell::sync::Lazy;

/// Global registry of needed checked arithmetic intrinsics
/// Format: (operation, type) e.g., ("add", "i32")
static NEEDED_INTRINSICS: Lazy<Mutex<HashSet<(String, String)>>> = Lazy::new(|| {
    Mutex::new(HashSet::new())
});

/// Register that a checked arithmetic intrinsic is needed
pub fn register_intrinsic(operation: &str, ty: &str) {
    let mut registry = NEEDED_INTRINSICS.lock().unwrap();
    registry.insert((operation.to_string(), ty.to_string()));
}

/// Get all registered intrinsics and clear the registry
pub fn take_needed_intrinsics() -> Vec<(String, String)> {
    let mut registry = NEEDED_INTRINSICS.lock().unwrap();
    let intrinsics: Vec<_> = registry.iter().cloned().collect();
    registry.clear();
    intrinsics
}

/// Check if an intrinsic has been registered
pub fn is_registered(operation: &str, ty: &str) -> bool {
    let registry = NEEDED_INTRINSICS.lock().unwrap();
    registry.contains(&(operation.to_string(), ty.to_string()))
}
