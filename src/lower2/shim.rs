// src/lower2/shim.rs

use serde::Deserialize;
use std::{collections::HashMap, str, sync::OnceLock};

// --- Standard Library Shim Metadata Loader ---

#[derive(Deserialize, Debug, Clone)]
pub(super) struct ShimInfo {
    // pub(super) or pub(crate)
    pub(super) descriptor: String,
    pub(super) is_static: bool,
}

// Key: Simplified function name (output of make_jvm_safe)
pub(super) type ShimMap = HashMap<String, ShimInfo>;

// --- Lazy Static Loader for Shims (Reads JSON File) ---

static SHIM_METADATA: OnceLock<Result<ShimMap, String>> = OnceLock::new();

pub(super) fn get_shim_metadata() -> Result<&'static ShimMap, &'static str> {
    SHIM_METADATA
        .get_or_init(|| {
            const JSON_BYTES: &[u8] = include_bytes!("../../shim-metadata-gen/core.json"); // Adjust path relative to THIS file

            let json_str = str::from_utf8(JSON_BYTES)
                .map_err(|e| format!("Failed to decode embedded JSON bytes as UTF-8: {}", e))?;

            serde_json::from_str(json_str)
                .map_err(|e| format!("Failed to parse embedded JSON string: {}", e))
        })
        .as_ref()
        .map_err(|e| e.as_str())
}
