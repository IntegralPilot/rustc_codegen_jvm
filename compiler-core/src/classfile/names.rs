//! Reserved names in compiler artifacts, resolved by the final Java linker.
pub const CRATE_MARKER: &str = "$crate";
pub const CRATE_MARKER_LEN: usize = 6 + 16 + 1;
pub const STRING_TAG: &str = "\u{1}rustc-jvm:";
pub const NAME_STRING: &str = "\u{1}rustc-jvm:name:";
pub const LITERAL_STRING: &str = "\u{1}rustc-jvm:literal:";

/// Include the temporary linker tag in the JVM's modified UTF-8 limit. NUL
/// expands to two bytes and supplementary characters to a surrogate pair.
pub fn literal_fits(value: &str) -> bool {
    let limit = u16::MAX as usize - LITERAL_STRING.len();
    value.len() <= limit / 2
        || value
            .chars()
            .map(|c| match c {
                '\0' => 2,
                c if c as u32 > 0xffff => 6,
                c => c.len_utf8(),
            })
            .sum::<usize>()
            <= limit
}

pub fn codec_owner(name: &str) -> bool {
    name.rsplit('/')
        .next()
        .is_some_and(|name| name.starts_with("Codecs_"))
}

pub fn codec_method_key(name: &str) -> Option<&str> {
    let (operation, key) = name.split_once('$')?;
    (matches!(operation, "e" | "w" | "d" | "a" | "b" | "s" | "c")
        && key.len() == 16
        && key.bytes().all(|byte| byte.is_ascii_hexdigit()))
    .then_some(key)
}

/// Parse packed memory codecs in pointer and view recipes.
/// Declaration, emission and linking share this dependency grammar.
pub fn codec_recipes(value: &str) -> impl Iterator<Item = (&str, &str)> {
    value.lines().filter_map(|line| {
        let line = line.strip_prefix(NAME_STRING).unwrap_or(line);
        let mut parts = line.split('#');
        let (Some(owner), Some(key), Some(descriptor)) = (parts.next(), parts.next(), parts.next())
        else {
            return None;
        };
        let valid_size = parts.next().is_none_or(|size| size.parse::<u32>().is_ok());
        (valid_size
            && parts.next().is_none()
            && codec_owner(owner)
            && key.len() == 16
            && key.bytes().all(|byte| byte.is_ascii_hexdigit())
            && !descriptor.is_empty())
        .then_some((line, key))
    })
}

#[cfg(test)]
mod codec_tests {
    #[test]
    fn nested_recipes_retain_every_exact_identity() {
        let first = "pkg/Codecs_12#1234567890abcdef#Lpkg/Value;";
        let second = "pkg/Codecs_ab#abcdef1234567890#[B#8";
        let nested = format!(
            "{}@slice-pointer\nview\n4\n{first}\n@raw-pointer\n{second}",
            super::NAME_STRING
        );
        assert_eq!(
            super::codec_recipes(&nested)
                .map(|(text, _)| text)
                .collect::<Vec<_>>(),
            [first, second]
        );
        assert!(
            super::codec_recipes("pkg/Value#1234567890abcdef#I\npkg/Codecs_12#oops#I")
                .next()
                .is_none()
        );
    }
}

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
