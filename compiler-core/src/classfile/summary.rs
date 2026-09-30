//! Borrowed dependency summaries for private holders and proven carrier helpers.
//! Ordinary classes retain their complete constant-pool dependencies.
use super::JavaStr;
use std::io;
mod code;
mod forwarder;

#[derive(Debug, PartialEq, Eq)]
pub struct Summary {
    pub name: String,
    pub has_main: bool,
    pub private: bool,
    pub opaque_reflection: bool,
    pub method_demands: bool,
    pub carrier: Option<Vec<u8>>,
}

pub const PRIVATE_ATTRIBUTE: &str = "RustJvmPrivate";
pub const CARRIER_ATTRIBUTE: &str = "RustJvmCarrier";

/// These namespaces have no observable Java method ownership.
/// Eligible classes must also have the private marker and no instance state.
pub fn method_owner(name: &str) -> bool {
    name.contains("/mono/Mono_")
        || name
            .rsplit('/')
            .next()
            .is_some_and(|n| n.starts_with("Codecs_"))
}

/// Static enum helpers do not participate in virtual dispatch.
/// Exact calls and runtime reflection names determine which helpers are live.
pub fn enum_helper(name: &str) -> bool {
    matches!(
        name,
        "eq" | "variantIndex"
            | "is_some"
            | "is_none"
            | "_unionDiscriminant"
            | "_fromUnionDiscriminant"
            | "_writeUnionStorage"
            | "_readUnionStorage"
    ) || name.starts_with("_rust_drop_fields$")
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct MethodKey<'a> {
    pub owner: &'a [u8],
    pub name: &'a [u8],
    pub descriptor: &'a [u8],
}

#[derive(Clone, Copy)]
pub enum Dependency<'a> {
    /// Definition metadata is separate from dependency edges.
    Definition {
        public: bool,
        forward: Option<MethodKey<'a>>,
        /// Count definition bytes before constant-pool sharing.
        /// Count each repeated fragment once and exclude unreachable methods.
        bytes: usize,
    },
    Class(&'a [u8]),
    Text(&'a [u8]),
    String(&'a [u8]),
    Method(MethodKey<'a>),
    /// A field or non-private method name is part of a nominal Java surface.
    FixedMemberName(&'a [u8]),
}

#[derive(Clone, Copy)]
enum Constant<'a> {
    Other,
    Utf8(&'a [u8]),
    Class(u16),
    Text(u16),
    String(u16),
    Member(u16, u16, bool),
    NameAndType(u16, u16),
    Handle(u16),
    Dynamic(u16, u16),
}
struct Method<'a> {
    flags: u16,
    name: &'a [u8],
    descriptor: &'a [u8],
    code: Option<&'a [u8]>,
    supported: bool,
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
}
struct Pool<'a> {
    constants: Vec<Constant<'a>>,
    bootstrap: Vec<Vec<u16>>,
}
impl<'a> Pool<'a> {
    fn utf8(&self, index: u16) -> io::Result<&'a [u8]> {
        match self.constants.get(index as usize) {
            Some(Constant::Utf8(bytes)) => Ok(bytes),
            _ => Err(invalid()),
        }
    }
    fn class(&self, index: u16) -> io::Result<&'a [u8]> {
        match self.constants.get(index as usize) {
            Some(Constant::Class(index)) => self.utf8(*index),
            _ => Err(invalid()),
        }
    }
    fn member(&self, owner: u16, index: u16) -> io::Result<MethodKey<'a>> {
        let Some(Constant::NameAndType(name, descriptor)) = self.constants.get(index as usize)
        else {
            return Err(invalid());
        };
        Ok(MethodKey {
            owner: self.class(owner)?,
            name: self.utf8(*name)?,
            descriptor: self.utf8(*descriptor)?,
        })
    }
    fn visit(
        &self,
        index: u16,
        stamp: u32,
        seen: &mut [u32],
        visit: &mut impl FnMut(Dependency<'a>),
    ) -> io::Result<()> {
        let entry = seen.get_mut(index as usize).ok_or_else(invalid)?;
        if *entry == stamp {
            return Ok(());
        }
        *entry = stamp;
        match self.constants.get(index as usize).ok_or_else(invalid)? {
            Constant::Class(index) => visit(Dependency::Class(self.utf8(*index)?)),
            Constant::Utf8(bytes) => visit(Dependency::Text(bytes)),
            Constant::Text(index) => visit(Dependency::Text(self.utf8(*index)?)),
            Constant::String(index) => visit(Dependency::String(self.utf8(*index)?)),
            Constant::Member(owner, member, method) => {
                let key = self.member(*owner, *member)?;
                visit(Dependency::Class(key.owner));
                visit(Dependency::Text(key.descriptor));
                if *method {
                    visit(Dependency::Method(key));
                } else {
                    visit(Dependency::FixedMemberName(key.name));
                }
            }
            Constant::NameAndType(_, descriptor) => {
                visit(Dependency::Text(self.utf8(*descriptor)?))
            }
            Constant::Handle(index) => self.visit(*index, stamp, seen, visit)?,
            Constant::Dynamic(bootstrap, signature) => {
                if let Some(Constant::NameAndType(name, _)) =
                    self.constants.get(*signature as usize)
                {
                    visit(Dependency::FixedMemberName(self.utf8(*name)?));
                }
                self.visit(*signature, stamp, seen, visit)?;
                for &index in self
                    .bootstrap
                    .get(*bootstrap as usize)
                    .ok_or_else(invalid)?
                {
                    self.visit(index, stamp, seen, visit)?;
                }
            }
            Constant::Other => {}
        }
        Ok(())
    }
}

pub fn read(bytes: &[u8]) -> io::Result<Summary> {
    read_dependencies(bytes, |_| {})
}
pub fn read_dependencies(
    bytes: &[u8],
    mut visit: impl FnMut(Dependency<'_>),
) -> io::Result<Summary> {
    read_demands(bytes, |_, dependency| visit(dependency))
}

/// Limit method scopes to private, stateless compiler namespaces.
/// Keep other fragments intact. Borrowed data remains local to this call.
pub fn read_demands<'a>(
    bytes: &'a [u8],
    mut visit: impl FnMut(Option<MethodKey<'a>>, Dependency<'a>),
) -> io::Result<Summary> {
    let mut r = Reader { bytes };
    if r.take(4)? != b"\xca\xfe\xba\xbe" {
        return Err(invalid());
    }
    r.take(4)?;
    let count = r.u16()? as usize;
    let mut pool = Pool {
        constants: vec![Constant::Other; count],
        bootstrap: Vec::new(),
    };
    let mut index = 1;
    while index < count {
        pool.constants[index] = match r.u8()? {
            1 => {
                let len = r.u16()? as usize;
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
            8 => Constant::String(r.u16()?),
            16 => Constant::Text(r.u16()?),
            19 | 20 => {
                r.take(2)?;
                Constant::Other
            }
            tag @ 9..=11 => Constant::Member(r.u16()?, r.u16()?, tag != 9),
            12 => Constant::NameAndType(r.u16()?, r.u16()?),
            17 | 18 => Constant::Dynamic(r.u16()?, r.u16()?),
            15 => {
                r.u8()?;
                Constant::Handle(r.u16()?)
            }
            _ => return Err(invalid()),
        };
        index += 1;
    }
    let flags = r.u16()?;
    let owner = pool.class(r.u16()?)?;
    let name = JavaStr::from_mutf8(owner)
        .map_err(|_| invalid())?
        .to_rust_string();
    let superclass = r.u16()?;
    let interfaces = (0..r.u16()?)
        .map(|_| r.u16())
        .collect::<io::Result<Vec<_>>>()?;
    let fields = r.u16()?;
    let mut field_descriptors = Vec::with_capacity(fields as usize);
    let mut plain_fields = true;
    for _ in 0..fields {
        plain_fields &= r.u16()? & 0x0008 == 0;
        visit(None, Dependency::FixedMemberName(pool.utf8(r.u16()?)?));
        field_descriptors.push(pool.utf8(r.u16()?)?);
        let attributes = r.u16()?;
        plain_fields &= attributes == 0;
        for _ in 0..attributes {
            r.u16()?;
            let length = r.u32()? as usize;
            r.take(length)?;
        }
    }
    let mut methods = Vec::new();
    let mut has_main = false;
    let mut opaque_reflection = false;
    for _ in 0..r.u16()? {
        let flags = r.u16()?;
        opaque_reflection |= flags & 0x0100 != 0;
        let name = pool.utf8(r.u16()?)?;
        let descriptor = pool.utf8(r.u16()?)?;
        has_main |=
            flags & 0x0009 == 0x0009 && name == b"main" && descriptor == b"([Ljava/lang/String;)V";
        let mut method = Method {
            flags,
            name,
            descriptor,
            code: None,
            supported: true,
        };
        for _ in 0..r.u16()? {
            let attribute = pool.utf8(r.u16()?)?;
            let len = r.u32()? as usize;
            let data = r.take(len)?;
            match attribute {
                b"Code" => method.code = Some(data),
                b"MethodParameters" => {}
                _ => method.supported = false,
            }
        }
        methods.push(method);
    }
    let mut private = false;
    let mut carrier = None;
    let mut supported = true;
    let mut metadata_classes = Vec::new();
    for _ in 0..r.u16()? {
        let attribute = pool.utf8(r.u16()?)?;
        let len = r.u32()? as usize;
        let payload = r.take(len)?;
        match attribute {
            b"RustJvmPrivate" if payload.is_empty() => private = true,
            b"RustJvmCarrier" => carrier = Some(payload.to_vec()),
            b"SourceFile" => {}
            b"InnerClasses" => {
                let mut nested = Reader { bytes: payload };
                for _ in 0..nested.u16()? {
                    for _ in 0..2 {
                        let index = nested.u16()?;
                        if index != 0 {
                            metadata_classes.push(pool.class(index)?);
                        }
                    }
                    nested.take(4)?; // simple name and access flags
                }
                if !nested.bytes.is_empty() {
                    return Err(invalid());
                }
            }
            b"BootstrapMethods" => {
                let mut b = Reader { bytes: payload };
                for _ in 0..b.u16()? {
                    let handle = b.u16()?;
                    let mut arguments = vec![handle];
                    for _ in 0..b.u16()? {
                        arguments.push(b.u16()?);
                    }
                    pool.bootstrap.push(arguments);
                }
                if !b.bytes.is_empty() {
                    return Err(invalid());
                }
            }
            _ => supported = false,
        }
    }
    if !r.bytes.is_empty() {
        return Err(invalid());
    }
    for constant in &pool.constants {
        let Constant::Member(owner, member, true) = *constant else {
            continue;
        };
        let key = pool.member(owner, member)?;
        // A symbolic owner may be a ClassLoader subclass, so do not require
        // the exact platform owner for class-loading operations.
        opaque_reflection |= matches!(
            key.name,
            b"forName"
                | b"loadClass"
                | b"findClass"
                | b"defineClass"
                | b"getResource"
                | b"getResources"
                | b"getResourceAsStream"
        ) || (matches!(
            key.owner,
            b"java/lang/Class" | b"java/lang/invoke/MethodHandles$Lookup"
        ) && matches!(
            key.name,
            b"findStatic"
                | b"findVirtual"
                | b"findConstructor"
                | b"findGetter"
                | b"findSetter"
                | b"findStaticGetter"
                | b"findStaticSetter"
                | b"getMethod"
                | b"getDeclaredMethod"
                | b"getField"
                | b"getDeclaredField"
                | b"getConstructor"
                | b"getDeclaredConstructor"
        ));
    }
    let static_owner = method_owner(&name)
        && flags & 0x0200 == 0
        && interfaces.is_empty()
        && methods
            .iter()
            .all(|m| m.flags & 0x0008 != 0 && m.flags & 0x0520 == 0 && m.name != b"<clinit>");
    let private_interface = flags & 0x0200 != 0 && !method_owner(&name);
    let private_carrier = carrier.is_some() && plain_fields;
    let method_demands = private
        && supported
        && (((static_owner || private_interface) && fields == 0) || private_carrier)
        && superclass != 0
        && pool.class(superclass)? == b"java/lang/Object"
        && methods.iter().all(|m| m.supported);
    let mut seen = vec![0; count];
    if !method_demands || !static_owner {
        for method in &methods {
            visit(None, Dependency::FixedMemberName(method.name));
        }
    }
    if method_demands {
        visit(None, Dependency::Class(b"java/lang/Object"));
        for name in metadata_classes {
            visit(None, Dependency::Class(name));
        }
        for &interface in &interfaces {
            visit(None, Dependency::Class(pool.class(interface)?));
        }
        for descriptor in field_descriptors {
            visit(None, Dependency::Text(descriptor));
        }
        for (index, method) in methods.iter().enumerate() {
            // Pure carrier recipes prove this final equality helper.
            // Its virtual calls name the exact owner. Copy and interface dispatch
            // still require the containing class.
            let carrier_eq = private_carrier
                && method.flags & 0x0010 != 0
                && method.name == b"eq"
                && method
                    .descriptor
                    .strip_prefix(b"(L")
                    .and_then(|s| s.strip_suffix(b";)Z"))
                    == Some(owner);
            let scope = ((method.flags & 0x0008 != 0
                && method.flags & 0x0520 == 0
                && method.name != b"<clinit>")
                || carrier_eq)
                .then_some(MethodKey {
                    owner,
                    name: method.name,
                    descriptor: method.descriptor,
                });
            if scope.is_some() {
                let forward = (static_owner && method.flags & 0x0001 != 0)
                    .then(|| {
                        method
                            .code
                            .and_then(|bytes| forwarder::target(bytes, method.descriptor, &pool))
                    })
                    .flatten();
                visit(
                    scope,
                    Dependency::Definition {
                        public: method.flags & 0x0001 != 0,
                        forward,
                        bytes: method.code.map_or(0, <[u8]>::len)
                            + method.name.len()
                            + method.descriptor.len()
                            + 32,
                    },
                );
            }
            visit(scope, Dependency::Text(method.descriptor)); // defines even an empty body
            let mut dependency = |d| visit(scope, d);
            if let Some(bytes) = method.code {
                let stamp = index as u32 + 1;
                code::constants(bytes, |i| pool.visit(i, stamp, &mut seen, &mut dependency))?;
                code::stack_maps(
                    bytes,
                    |i| pool.utf8(i),
                    |i| pool.visit(i, stamp, &mut seen, &mut dependency),
                )?;
            }
        }
    } else {
        for index in 1..count {
            pool.visit(index as u16, 1, &mut seen, &mut |d| visit(None, d))?;
        }
    }
    Ok(Summary {
        name,
        has_main,
        private,
        opaque_reflection,
        method_demands,
        carrier: private.then_some(carrier).flatten(),
    })
}
