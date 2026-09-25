use crate::*;

pub(crate) fn msvc_output_path(arg: &str) -> Option<&str> {
    let prefix = arg.get(..5)?;
    prefix.eq_ignore_ascii_case("/out:").then(|| &arg[5..])
}

pub(crate) fn jar_output_path(mut output_name: String) -> String {
    if output_name.to_ascii_lowercase().ends_with(".exe") {
        output_name.truncate(output_name.len() - ".exe".len());
    }
    if !output_name.to_ascii_lowercase().ends_with(".jar") {
        output_name.push_str(".jar");
    }
    output_name
}

pub(crate) fn parse_response_lines(content: &str, msvc_quoting: bool) -> Vec<String> {
    content
        .lines()
        .filter_map(|line| {
            let line = line.trim_end_matches('\r');
            if line.is_empty() {
                return None;
            }

            let line = if msvc_quoting {
                line.strip_prefix('"')
                    .and_then(|line| line.strip_suffix('"'))
                    .unwrap_or(line)
            } else {
                line
            };

            let mut parsed = String::with_capacity(line.len());
            let mut chars = line.chars().peekable();
            while let Some(character) = chars.next() {
                if character == '\\' {
                    if msvc_quoting {
                        if chars.peek() == Some(&'"') {
                            parsed.push(chars.next().unwrap());
                        } else {
                            parsed.push(character);
                        }
                    } else if let Some(escaped) = chars.next() {
                        parsed.push(escaped);
                    } else {
                        parsed.push(character);
                    }
                } else {
                    parsed.push(character);
                }
            }
            Some(parsed)
        })
        .collect()
}

pub(crate) fn read_response_file(path: &Path) -> io::Result<Vec<String>> {
    let bytes = fs::read(path)?;
    if bytes.starts_with(&[0xff, 0xfe]) {
        let body = &bytes[2..];
        if body.len() % 2 != 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "UTF-16 response file has an odd byte length",
            ));
        }
        let units: Vec<u16> = body
            .chunks_exact(2)
            .map(|bytes| u16::from_le_bytes([bytes[0], bytes[1]]))
            .collect();
        let content = String::from_utf16(&units)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;
        Ok(parse_response_lines(&content, true))
    } else {
        let content =
            std::str::from_utf8(bytes.strip_prefix(&[0xef, 0xbb, 0xbf]).unwrap_or(&bytes))
                .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;
        Ok(parse_response_lines(content, false))
    }
}

pub(crate) fn linker_args() -> io::Result<Vec<String>> {
    let mut command_line = env::args();
    let mut expanded = vec![command_line.next().unwrap_or_else(|| "java-linker".into())];
    for arg in command_line {
        if let Some(path) = arg.strip_prefix('@') {
            expanded.extend(read_response_file(Path::new(path))?);
        } else {
            expanded.push(arg);
        }
    }
    Ok(expanded)
}
