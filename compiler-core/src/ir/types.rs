use super::{SymbolId, TypeId};
use crate::scalar::ScalarType;
use rustc_hash::FxHashMap;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    /// Body-local identity for an uninspected pointee. Never a JVM value.
    Opaque(u32),
    Scalar(ScalarType),
    Class(SymbolId),
    Interface(SymbolId),
    Pointer(TypeId),
    Array(TypeId),
    Slice(TypeId),
    Str,
}

impl Type {
    /// JVM storage category. Reference identities remain distinct IR types.
    pub fn carrier(self) -> u8 {
        use ScalarType::*;
        match self {
            Self::Unit | Self::Opaque(_) => 0,
            Self::Scalar(Bool | I8 | U8 | I16 | U16 | I32 | U32 | Char | F16) => 1,
            Self::Scalar(I64 | U64) => 2,
            Self::Scalar(F32) => 3,
            Self::Scalar(F64) => 4,
            _ => 5,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Types {
    base: Option<Arc<Types>>,
    values: Vec<Type>,
    ids: FxHashMap<Type, TypeId>,
    symbols: Vec<Arc<str>>,
    symbol_ids: FxHashMap<Arc<str>, SymbolId>,
}

impl Types {
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
            && (0..self.len()).all(|i| self.get(TypeId::new(i)) == other.get(TypeId::new(i)))
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
            self.get(TypeId::new(i)).unwrap().hash(state);
        }
        (self.base_symbols() + self.symbols.len()).hash(state);
        for i in 0..self.base_symbols() + self.symbols.len() {
            self.symbol_name(SymbolId::new(i)).unwrap().hash(state);
        }
    }
}
