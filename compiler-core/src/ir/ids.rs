use std::num::NonZeroU32;

macro_rules! ids {
    ($($name:ident),* $(,)?) => {$(
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[repr(transparent)]
        pub struct $name(NonZeroU32);
        impl $name {
            pub fn new(index: usize) -> Self {
                let index = u32::try_from(index).ok().and_then(|i| i.checked_add(1))
                    .and_then(NonZeroU32::new).expect("IR table exceeds 32-bit identifier capacity");
                Self(index)
            }
            pub fn index(self) -> usize { (self.0.get() - 1) as usize }
        }
    )*}
}

ids!(
    ValueId,
    InstId,
    BlockId,
    EdgeId,
    VariableId,
    TypeId,
    SymbolId,
    ConstId,
    SlotId,
    MemberId,
    MethodId,
    ProjectionId
);

/// Contiguous storage for uncommon variable-length payloads.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct List {
    pub start: u32,
    pub len: u32,
}

impl List {
    pub fn append<T>(pool: &mut Vec<T>, values: impl IntoIterator<Item = T>) -> Self {
        let start = u32::try_from(pool.len()).expect("IR payload pool exceeds 32-bit capacity");
        pool.extend(values);
        let end = u32::try_from(pool.len()).expect("IR payload pool exceeds 32-bit capacity");
        Self {
            start,
            len: end - start,
        }
    }
    pub fn range(self) -> std::ops::Range<usize> {
        self.start as usize..self.start as usize + self.len as usize
    }
}
