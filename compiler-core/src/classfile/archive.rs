//! Read member ranges without loading an archive's object files.
use std::io::{self, Read, Seek, SeekFrom};
fn invalid(message: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message)
}
fn end(start: u64, size: u64, limit: u64) -> io::Result<u64> {
    start
        .checked_add(size)
        .filter(|n| *n <= limit)
        .ok_or_else(|| invalid("archive member exceeds containing file"))
}
pub fn members<R: Read + Seek>(
    reader: &mut R,
    limit: u64,
    mut visit: impl FnMut(&mut R, &str, u64, u64) -> io::Result<()>,
) -> io::Result<()> {
    reader.seek(SeekFrom::Start(0))?;
    let mut magic = [0; 8];
    reader.read_exact(&mut magic)?;
    if &magic != b"!<arch>\n" {
        return Err(invalid("not an ar archive"));
    }
    let mut offset = 8;
    let mut names = Vec::new();
    while offset < limit {
        let mut start = end(offset, 60, limit)?;
        reader.seek(SeekFrom::Start(offset))?;
        let mut header = [0; 60];
        reader.read_exact(&mut header)?;
        if &header[58..] != b"`\n" {
            return Err(invalid("invalid ar member header"));
        }
        let text = |bytes| {
            std::str::from_utf8(bytes)
                .map(str::trim)
                .map_err(|_| invalid("invalid ar header text"))
        };
        let raw = text(&header[..16])?;
        let size = text(&header[48..58])?
            .parse::<u64>()
            .map_err(|_| invalid("invalid ar member size"))?;
        let member_end = end(start, size, limit)?;
        let name = match raw {
            "/" | "/SYM64/" => "__symbols".into(),
            "//" => "__names".into(),
            name if name.starts_with("#1/") => {
                let len = name[3..]
                    .trim()
                    .parse::<u64>()
                    .map_err(|_| invalid("invalid BSD ar name length"))?;
                let data_start = end(start, len, member_end)?;
                let mut bytes =
                    vec![0; usize::try_from(len).map_err(|_| invalid("ar name too large"))?];
                reader.read_exact(&mut bytes)?;
                start = data_start;
                String::from_utf8_lossy(&bytes)
                    .trim_end_matches('\0')
                    .to_owned()
            }
            name if name.starts_with('/') => {
                let index = name[1..]
                    .parse::<usize>()
                    .map_err(|_| invalid("invalid GNU name offset"))?;
                let tail: &[u8] = names
                    .get(index..)
                    .filter(|b| !b.is_empty())
                    .ok_or_else(|| invalid("GNU name offset outside table"))?;
                let n = tail
                    .windows(2)
                    .position(|w| w == b"/\n")
                    .unwrap_or(tail.len());
                String::from_utf8_lossy(&tail[..n]).into_owned()
            }
            name => name.trim_end_matches('/').to_owned(),
        };
        if name == "__names" {
            names.resize(
                usize::try_from(member_end - start).map_err(|_| invalid("name table too large"))?,
                0,
            );
            reader.read_exact(&mut names)?;
        } else if name != "__symbols" {
            visit(reader, &name, start, member_end - start)?;
        }
        offset = member_end
            .checked_add(member_end % 2)
            .filter(|n| *n <= limit)
            .ok_or_else(|| invalid("missing ar member padding"))?;
    }
    Ok(())
}
