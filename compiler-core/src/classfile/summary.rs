//! Read class identity and entry-point metadata without decoding method bodies.
//! Constant strings borrow the input; only the returned class name is allocated.
use super::JavaStr;
use std::io;

#[derive(Debug, PartialEq, Eq)]
pub struct Summary {
    pub name: String,
    pub has_main: bool,
}

#[derive(Clone, Copy)]
enum Constant<'a> {
    Other,
    Utf8(&'a [u8]),
    Class(u16),
}

struct Reader<'a> {
    bytes: &'a [u8],
}

fn invalid() -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, "invalid JVM class metadata")
}

impl<'a> Reader<'a> {
    fn take(&mut self, count: usize) -> io::Result<&'a [u8]> {
        let (head, tail) = self.bytes.split_at_checked(count).ok_or_else(invalid)?;
        self.bytes = tail;
        Ok(head)
    }
    fn u8(&mut self) -> io::Result<u8> {
        Ok(self.take(1)?[0])
    }
    fn u16(&mut self) -> io::Result<u16> {
        Ok(u16::from_be_bytes(self.take(2)?.try_into().unwrap()))
    }
    fn u32(&mut self) -> io::Result<u32> {
        Ok(u32::from_be_bytes(self.take(4)?.try_into().unwrap()))
    }
    fn attributes(&mut self) -> io::Result<()> {
        for _ in 0..self.u16()? {
            self.u16()?;
            let count = usize::try_from(self.u32()?).map_err(|_| invalid())?;
            self.take(count)?;
        }
        Ok(())
    }
}

pub fn read(bytes: &[u8]) -> io::Result<Summary> {
    let mut r = Reader { bytes };
    if r.take(4)? != b"\xca\xfe\xba\xbe" {
        return Err(invalid());
    }
    r.take(4)?; // minor/major version
    let count = usize::from(r.u16()?);
    let mut constants = vec![Constant::Other; count];
    let mut index = 1;
    while index < count {
        constants[index] = match r.u8()? {
            1 => {
                let len = usize::from(r.u16()?);
                Constant::Utf8(r.take(len)?)
            }
            7 => Constant::Class(r.u16()?),
            3 | 4 => {
                r.take(4)?;
                Constant::Other
            }
            5 | 6 => {
                r.take(8)?;
                index += 1;
                if index >= count {
                    return Err(invalid());
                }
                Constant::Other
            }
            8 | 16 | 19 | 20 => {
                r.take(2)?;
                Constant::Other
            }
            9 | 10 | 11 | 12 | 17 | 18 => {
                r.take(4)?;
                Constant::Other
            }
            15 => {
                r.take(3)?;
                Constant::Other
            }
            _ => return Err(invalid()),
        };
        index += 1;
    }
    let utf8 = |index: u16| match constants.get(usize::from(index)) {
        Some(Constant::Utf8(bytes)) => Ok(*bytes),
        _ => Err(invalid()),
    };
    r.u16()?; // access flags
    let Some(Constant::Class(name_index)) = constants.get(usize::from(r.u16()?)) else {
        return Err(invalid());
    };
    let name = JavaStr::from_mutf8(utf8(*name_index)?)
        .map_err(|_| invalid())?
        .to_rust_string();
    r.u16()?; // superclass
    let interface_count = usize::from(r.u16()?);
    r.take(interface_count * 2)?;
    for _ in 0..r.u16()? {
        r.take(6)?; // flags, name, descriptor
        r.attributes()?;
    }
    let mut has_main = false;
    for _ in 0..r.u16()? {
        let flags = r.u16()?;
        let name = utf8(r.u16()?)?;
        let descriptor = utf8(r.u16()?)?;
        has_main |=
            flags & 0x0009 == 0x0009 && name == b"main" && descriptor == b"([Ljava/lang/String;)V";
        r.attributes()?;
    }
    r.attributes()?;
    if !r.bytes.is_empty() {
        return Err(invalid());
    }
    Ok(Summary { name, has_main })
}
