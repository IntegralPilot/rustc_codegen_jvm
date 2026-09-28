//! Reserved names in compiler artifacts, resolved by the final Java linker.
pub const CRATE_MARKER: &str = "$crate";
pub const CRATE_MARKER_LEN: usize = 6 + 16 + 1;
pub const STRING_TAG: &str = "\u{1}rustc-jvm:";
pub const NAME_STRING: &str = "\u{1}rustc-jvm:name:";
pub const LITERAL_STRING: &str = "\u{1}rustc-jvm:literal:";

pub fn is_crate_marker(bytes: &[u8]) -> bool {
    bytes.len() == CRATE_MARKER_LEN
        && bytes.starts_with(CRATE_MARKER.as_bytes())
        && bytes.ends_with(b"$")
        && bytes[6..22].iter().all(u8::is_ascii_hexdigit)
}

/// Dollar signs inside crate identities do not denote nested JVM classes.
pub fn nesting_separators(name: &str) -> impl Iterator<Item = usize> + '_ {
    let mut skip_until = 0;
    name.match_indices('$').filter_map(move |(index, _)| {
        if index < skip_until {
            return None;
        }
        if name
            .as_bytes()
            .get(index..index + CRATE_MARKER_LEN)
            .is_some_and(is_crate_marker)
        {
            skip_until = index + CRATE_MARKER_LEN;
            None
        } else {
            Some(index)
        }
    })
}

pub fn inner_name(name: &str) -> &str {
    nesting_separators(name)
        .last()
        .map_or(name, |index| &name[index + 1..])
}
