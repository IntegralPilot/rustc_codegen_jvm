use super::{LayoutId, SymbolId, TypeId};
use crate::scalar::ScalarType;
use rustc_hash::FxHashMap;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    /// Body-local identity for an uninspected pointee. Never a JVM value.
    Opaque(u32),
    /// Exact source-language layout of an address pointee. Never a JVM value.
    Layout(LayoutId),
    Scalar(ScalarType),
    Class(SymbolId),
    Interface(SymbolId),
    Pointer(TypeId),
    Array(TypeId),
    Slice(TypeId),
    Str,
    /// A 64-bit payload and independent discriminant.
    TaggedI64,
}

impl Type {
    /// JVM storage category. Reference identities remain distinct IR types.
    pub fn carrier(self) -> u8 {
        use ScalarType::*;
        match self {
            Self::Unit | Self::Opaque(_) | Self::Layout(_) => 0,
            Self::Scalar(Bool | I8 | U8 | I16 | U16 | I32 | U32 | Char | F16) => 1,
            Self::Scalar(I64 | U64) => 2,
            Self::Scalar(F32) => 3,
            Self::Scalar(F64) => 4,
            _ => 5,
        }
    }
}

/// Shared exact pointee layout, kept out of the eight-byte ordinary type entry.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AddressLayout {
    pub value: TypeId,
    pub size: u32,
    pub codec: Option<SymbolId>,
}

#[derive(Default, Debug, Clone)]
pub struct Types {
    base: Option<Arc<Types>>,
    values: Vec<Type>,
    ids: FxHashMap<Type, TypeId>,
    layouts: Vec<AddressLayout>,
    layout_ids: FxHashMap<AddressLayout, LayoutId>,
    symbols: Vec<Arc<str>>,
    symbol_ids: FxHashMap<Arc<str>, SymbolId>,
}

impl Types {
    pub fn layout(&mut self, layout: AddressLayout) -> TypeId {
        let id = if let Some(&id) = self
            .layout_ids
            .get(&layout)
            .or_else(|| self.base.as_ref().and_then(|b| b.layout_ids.get(&layout)))
        {
            id
        } else {
            let id = LayoutId::new(self.base_layouts() + self.layouts.len());
            self.layouts.push(layout);
            self.layout_ids.insert(layout, id);
            id
        };
        self.intern(Type::Layout(id))
    }
    fn base_layouts(&self) -> usize {
        self.base.as_ref().map_or(0, |b| b.layouts.len())
    }
    pub fn get_layout(&self, id: LayoutId) -> AddressLayout {
        let base = self.base_layouts();
        if id.index() < base {
            self.base.as_ref().unwrap().layouts[id.index()]
        } else {
            self.layouts[id.index() - base]
        }
    }
    pub fn pointee(&self, pointer: TypeId) -> Option<TypeId> {
        let Type::Pointer(inner) = self.get(pointer)? else {
            return None;
        };
        Some(match self.get(inner)? {
            Type::Layout(id) => self.get_layout(id).value,
            _ => inner,
        })
    }
    pub fn address_layout(&self, pointer: TypeId) -> Option<(u32, Option<SymbolId>)> {
        let Type::Pointer(inner) = self.get(pointer)? else {
            return None;
        };
        match self.get(inner)? {
            Type::Layout(id) => {
                let layout = self.get_layout(id);
                Some((layout.size, layout.codec))
            }
            _ => None,
        }
    }

    /// Extend an immutable common vocabulary without copying its tables. Only
    /// one shared layer is allowed, so lookup cost cannot grow across bodies.
    pub fn with_base(base: Arc<Types>) -> Self {
        assert!(
            base.base.is_none(),
            "type vocabularies have one shared layer"
        );
        Self {
            base: Some(base),
            ..Default::default()
        }
    }
    pub fn len(&self) -> usize {
        self.base_types() + self.values.len()
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// Whether an extension contains no additional types or symbols.
    pub fn has_additions(&self) -> bool {
        !self.values.is_empty() || !self.symbols.is_empty()
    }
    fn base_types(&self) -> usize {
        self.base.as_ref().map_or(0, |b| b.values.len())
    }
    fn base_symbols(&self) -> usize {
        self.base.as_ref().map_or(0, |b| b.symbols.len())
    }
    pub fn find(&self, ty: Type) -> Option<TypeId> {
        self.ids
            .get(&ty)
            .or_else(|| self.base.as_ref().and_then(|b| b.ids.get(&ty)))
            .copied()
    }
    pub fn intern(&mut self, ty: Type) -> TypeId {
        if let Some(id) = self.find(ty) {
            return id;
        }
        let id = TypeId::new(self.len());
        self.values.push(ty);
        self.ids.insert(ty, id);
        id
    }
    pub fn scalar(&mut self, ty: ScalarType) -> TypeId {
        self.intern(Type::Scalar(ty))
    }
    pub fn get(&self, ty: TypeId) -> Option<Type> {
        let base = self.base_types();
        if ty.index() < base {
            self.base.as_ref()?.values.get(ty.index()).copied()
        } else {
            self.values.get(ty.index() - base).copied()
        }
    }
    pub fn symbol(&mut self, name: &str) -> SymbolId {
        if let Some(&id) = self
            .symbol_ids
            .get(name)
            .or_else(|| self.base.as_ref().and_then(|b| b.symbol_ids.get(name)))
        {
            return id;
        }
        let id = SymbolId::new(self.base_symbols() + self.symbols.len());
        let name: Arc<str> = name.into();
        self.symbols.push(Arc::clone(&name));
        self.symbol_ids.insert(name, id);
        id
    }
    pub fn find_symbol(&self, name: &str) -> Option<SymbolId> {
        self.symbol_ids
            .get(name)
            .or_else(|| self.base.as_ref().and_then(|b| b.symbol_ids.get(name)))
            .copied()
    }
    pub fn symbol_name(&self, symbol: SymbolId) -> Option<&str> {
        let base = self.base_symbols();
        if symbol.index() < base {
            self.base
                .as_ref()?
                .symbols
                .get(symbol.index())
                .map(AsRef::as_ref)
        } else {
            self.symbols.get(symbol.index() - base).map(AsRef::as_ref)
        }
    }
}

impl PartialEq for Types {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len()
            && (0..self.len()).all(|i| {
                match (self.get(TypeId::new(i)), other.get(TypeId::new(i))) {
                    (Some(Type::Layout(a)), Some(Type::Layout(b))) => {
                        self.get_layout(a) == other.get_layout(b)
                    }
                    (a, b) => a == b,
                }
            })
            && self.base_symbols() + self.symbols.len()
                == other.base_symbols() + other.symbols.len()
            && (0..self.base_symbols() + self.symbols.len())
                .all(|i| self.symbol_name(SymbolId::new(i)) == other.symbol_name(SymbolId::new(i)))
    }
}
impl Eq for Types {}
impl std::hash::Hash for Types {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.len().hash(state);
        for i in 0..self.len() {
            let ty = self.get(TypeId::new(i)).unwrap();
            if let Type::Layout(id) = ty {
                std::mem::discriminant(&ty).hash(state);
                self.get_layout(id).hash(state);
            } else {
                ty.hash(state);
            }
        }
        (self.base_symbols() + self.symbols.len()).hash(state);
        for i in 0..self.base_symbols() + self.symbols.len() {
            self.symbol_name(SymbolId::new(i)).unwrap().hash(state);
        }
    }
}

#[cfg(test)]
mod layout_tests {
    use super::*;
    #[test]
    fn layouts_are_interned_across_shared_vocabularies_without_growing_types() {
        assert_eq!(std::mem::size_of::<Type>(), 8);
        let mut base = Types::default();
        let value = base.scalar(ScalarType::I64);
        let first = AddressLayout {
            value,
            size: 8,
            codec: None,
        };
        let ty = base.layout(first);
        let mut child = Types::with_base(Arc::new(base));
        assert_eq!(child.layout(first), ty);
        assert!(!child.has_additions());
        let different = child.layout(AddressLayout { size: 16, ..first });
        assert_ne!(different, ty);
        let pointer = child.intern(Type::Pointer(different));
        assert_eq!(child.pointee(pointer), Some(value));
        assert_eq!(child.address_layout(pointer), Some((16, None)));
    }
}
