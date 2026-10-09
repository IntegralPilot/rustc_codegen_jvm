//! Describe physical components while retaining semantic pointee types.
//! Emit a boundary carrier only when a consumer needs one object.
use super::*;
use crate::scalar::ScalarType;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ComponentShape {
    View,
    TaggedI64,
    Address,
    /// This root retains the allocation's runtime view layout.
    /// A bare primitive array cannot replace it, unlike scalar address roots.
    StorageAddress,
}

impl ComponentShape {
    pub fn view_carrier(types: &Types, ty: TypeId) -> bool {
        match types.get(ty) {
            Some(Type::Slice(_) | Type::Str) => true,
            Some(Type::Class(name)) => matches!(
                types.symbol_name(name),
                Some("org/rustlang/runtime/SliceView" | "org/rustlang/runtime/Utf8View")
            ),
            _ => false,
        }
    }
    /// Runtime helpers can erase the semantic pointee to a carrier class.
    /// This annotation change does not require a carrier allocation.
    pub fn accepts_annotation(self, types: &Types, ty: TypeId) -> bool {
        if Self::of(types, ty) == Some(self) {
            return true;
        }
        let Some(Type::Class(name)) = types.get(ty) else {
            return false;
        };
        match types.symbol_name(name) {
            Some("java/lang/Object") => true,
            Some("org/rustlang/runtime/Pointer") => self.is_address(),
            Some("org/rustlang/runtime/SliceView" | "org/rustlang/runtime/Utf8View") => {
                self == Self::View
            }
            _ => false,
        }
    }
    pub fn len(self) -> usize {
        match self {
            Self::View => 3,
            Self::TaggedI64 => 2,
            Self::Address | Self::StorageAddress => 2,
        }
    }
    pub fn of(types: &Types, ty: TypeId) -> Option<Self> {
        match types.get(ty)? {
            Type::Slice(_) | Type::Str => Some(Self::View),
            Type::TaggedI64 => Some(Self::TaggedI64),
            Type::Pointer(inner) if StorageSlot::scalar(inner, types).is_some() => {
                Some(Self::Address)
            }
            Type::Pointer(_) => Some(Self::StorageAddress),
            _ => None,
        }
    }
    pub fn parts(self, types: &mut Types) -> impl ExactSizeIterator<Item = TypeId> + use<> {
        let object = types.symbol("java/lang/Object");
        let object = types.intern(Type::Class(object));
        let (parts, count) = match self {
            Self::View => (
                [
                    object,
                    types.scalar(ScalarType::I32),
                    types.scalar(ScalarType::U64),
                ],
                3,
            ),
            Self::TaggedI64 => {
                let long = types.scalar(ScalarType::I64);
                ([long, long, object], 2)
            }
            Self::Address | Self::StorageAddress => {
                ([object, types.scalar(ScalarType::I64), object], 2)
            }
        };
        parts.into_iter().take(count)
    }
    pub fn slots(self) -> usize {
        match self {
            Self::View | Self::TaggedI64 => 4,
            Self::Address | Self::StorageAddress => 3,
        }
    }
    pub fn pack(self, parts: List) -> Op {
        match self {
            Self::View => Op::ViewPack(parts),
            Self::TaggedI64 => Op::TaggedPack(parts),
            Self::Address | Self::StorageAddress => Op::AddressPack(parts),
        }
    }
    pub fn part(self, value: ValueId, index: u8) -> Op {
        match self {
            Self::View => Op::ViewPart { view: value, index },
            Self::TaggedI64 => Op::TaggedPart { value, index },
            Self::Address | Self::StorageAddress => Op::AddressPart {
                address: value,
                index,
            },
        }
    }
    pub fn is_borrowed(self) -> bool {
        self != Self::TaggedI64
    }
    pub fn is_address(self) -> bool {
        matches!(self, Self::Address | Self::StorageAddress)
    }
}
