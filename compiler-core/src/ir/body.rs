use super::*;
use crate::scalar::{BinaryOp, Scalar};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ValueDef {
    Inst(InstId),
    Param(BlockId),
    Alias(ValueId),
    Unreachable,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Value {
    pub ty: TypeId,
    pub def: ValueDef,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CallKind {
    Constructor,
    RustStatic,
    JvmStatic,
    Virtual,
    Interface,
    Indirect,
}

/// Storage operations for the JVM platform allocator.
/// Custom Rust allocators retain their calls and effects.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum HeapOp {
    Allocate,
    Reallocate,
    Deallocate,
}

/// A body-local call target. Signatures describe value-bearing JVM arguments;
/// source-language unit arguments never enter the operand pool.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MethodRef {
    pub owner: String,
    pub name: String,
    pub params: Vec<TypeId>,
    pub returns: TypeId,
    pub interface: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldRef {
    pub owner: TypeId,
    pub name: String,
    pub ty: TypeId,
    pub is_static: bool,
}

/// A field view preserves Rust byte layout and the runtime allocation identity.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PointerProjection {
    pub field: MemberId,
    pub offset: u64,
    pub size: u64,
    pub codec: Option<String>,
}

/// Addressable local storage retains its Rust allocation layout. Ordinary SSA
/// values have no storage record. Codec names use the body's shared vocabulary.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StorageSlot {
    pub ty: TypeId,
    pub size: u32,
    pub alignment: u32,
    pub codec: Option<SymbolId>,
}

impl StorageSlot {
    pub fn scalar(ty: TypeId, types: &Types) -> Option<Self> {
        use crate::scalar::ScalarType::*;
        let Type::Scalar(scalar) = types.get(ty)? else {
            return None;
        };
        let size = match scalar {
            Bool | I8 | U8 => 1,
            I16 | U16 | F16 => 2,
            I32 | U32 | F32 => 4,
            I64 | U64 | F64 => 8,
            _ => return None,
        };
        Some(Self {
            ty,
            size,
            alignment: size,
            codec: None,
        })
    }
}

/// Operands are SSA handles. Type/member/pointer semantics survive until selection.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Op {
    /// Removed effect; keeps source positions stable during promotion.
    Nop,
    Constant(ConstId),
    /// Exception delivered by the current unwind edge.
    Exception,
    /// Number of elements in the physical JVM carrier (an int).
    ArrayLength(ValueId),
    Binary {
        op: BinaryOp,
        left: ValueId,
        right: ValueId,
    },
    /// Overflow flag for [left, right, wrapped result]. The uncommon payload
    /// stays in the operand pool instead of increasing every instruction.
    Overflow {
        op: BinaryOp,
        args: List,
    },
    Not(ValueId),
    Bit {
        op: crate::scalar::BitOp,
        value: ValueId,
    },
    Opaque(ValueId),
    Neg(ValueId),
    Cast(ValueId),
    /// Refine a reference type after representation analysis proves the cast.
    /// Emit the JVM cast only if a consumer needs the result.
    Refine(ValueId),
    /// Representation adaptation at a JVM ABI boundary (boxing, views, casts).
    Adapt(ValueId),
    /// Same physical JVM carrier with a different semantic type annotation.
    Reinterpret(ValueId),
    NewArray(ValueId),
    /// Primitive repeat initialization, retaining the scalar value unboxed.
    ArrayFill {
        array: ValueId,
        value: ValueId,
    },
    /// Fresh, naturally aligned scalar storage with exact initial contents.
    /// Lower to a primitive array only if its address survives SSA promotion.
    ScalarCell(ValueId),
    /// Allocate [size, alignment], reallocate [root, offset, old size,
    /// alignment, new size], or release [root, offset]. Return storage roots.
    /// Keep address metadata separate.
    Heap {
        operation: HeapOp,
        args: List,
    },
    /// Select typed storage for a scalar field.
    /// Use a boundary carrier if the root requires general memory access.
    ProjectRoot {
        address: List,
        projection: ProjectionId,
    },
    ProjectOffset {
        root: ValueId,
        base: ValueId,
        offset: ValueId,
    },
    FunctionPointer {
        signature: MethodId,
        target: MethodId,
    },
    LoadSlot(SlotId),
    StoreSlot {
        slot: SlotId,
        value: ValueId,
    },
    AddressOfSlot(SlotId),
    Load(ValueId),
    /// An owned aggregate snapshot, distinct from a live decoded memory view.
    LoadCopy(ValueId),
    /// An owned Rust value snapshot, independent of subsequent source writes.
    CopyValue(ValueId),
    /// Complete mutations to a live decoded view using the original load owner.
    Commit(ValueId),
    Store {
        pointer: ValueId,
        value: ValueId,
    },
    Project {
        base: ValueId,
        projection: ProjectionId,
    },
    /// Access a scalar field without allocating its intermediate pointer view.
    LoadField {
        base: ValueId,
        projection: ProjectionId,
    },
    /// Copy an owned field without constructing an intermediate address.
    /// Keep the read and snapshot together, including inside an Invoke.
    LoadFieldCopy {
        base: ValueId,
        projection: ProjectionId,
    },
    StoreField {
        base: ValueId,
        projection: ProjectionId,
        value: ValueId,
    },
    LoadFieldPart {
        base: ValueId,
        projection: ProjectionId,
        index: u8,
    },
    /// Read a field through a storage root and displacement.
    /// The index selects one physical component of a borrowed field.
    LoadStorageField {
        address: List,
        projection: ProjectionId,
        index: Option<u8>,
    },
    LoadStorageFieldCopy {
        address: List,
        projection: ProjectionId,
    },
    /// [root, displacement, value/components] for a projected field write.
    StoreStorageField {
        args: List,
        projection: ProjectionId,
        split: bool,
    },
    StoreFieldParts {
        base: ValueId,
        projection: ProjectionId,
        parts: List,
    },
    Offset {
        pointer: ValueId,
        offset: ValueId,
        bytes: bool,
        wrapping: bool,
    },
    Call {
        method: MethodId,
        kind: CallKind,
        args: List,
    },
    GetField {
        object: ValueId,
        field: MemberId,
    },
    SetField {
        object: ValueId,
        field: MemberId,
        value: ValueId,
    },
    GetStatic(MemberId),
    SetStatic {
        field: MemberId,
        value: ValueId,
    },
    ArrayGet {
        array: ValueId,
        index: ValueId,
        /// Compiler-owned ABI and outline scratch, inaccessible to Rust code.
        native: bool,
    },
    ArraySet {
        array: ValueId,
        index: ValueId,
        value: ValueId,
        /// Same unaliased scratch-storage contract as ArrayGet::native.
        native: bool,
    },
    /// Slice access through [backing, start, index], without a view carrier.
    ViewGet(List),
    ViewSet {
        parts: List,
        value: ValueId,
    },
    /// Logical Rust length, including zero-sized slices larger than JVM arrays.
    Length(ValueId),
    /// Address with physical components [storage root, byte offset].
    AddressPack(List),
    /// Scalar [payload, discriminant], boxed only at an opaque boundary.
    TaggedPack(List),
    TaggedPart {
        value: ValueId,
        index: u8,
    },
    /// Exact Rust view layout belongs to the location, not its JVM carrier.
    RetypeAddress {
        pointer: ValueId,
        size: u32,
        codec: Option<SymbolId>,
    },
    /// A component location whose pointee layout is exact at this use.
    TypedAddressPack {
        parts: List,
        size: u32,
        codec: Option<SymbolId>,
    },
    /// An owned aggregate load with a statically known view layout.
    LoadTypedCopy {
        parts: List,
        size: u32,
        codec: Option<SymbolId>,
    },
    /// Parts are [root, displacement, optional JVM class name].
    /// Class and interface results request their type when no name is supplied.
    /// Object and array results use getObject's null target.
    /// No later consumer can commit the borrowed view's binding.
    LoadTyped {
        parts: List,
        size: u32,
        codec: Option<SymbolId>,
    },
    /// [root, displacement, value] with an exact source-language layout.
    StoreTyped {
        parts: List,
        size: u32,
        codec: Option<SymbolId>,
    },
    AddressEqual {
        left: ValueId,
        right: ValueId,
    },
    LocationEqual(List),
    /// Compare unsigned data addresses and return a signed comparison code.
    AddressCompare {
        left: ValueId,
        right: ValueId,
    },
    LocationCompare(List),
    /// Discriminant of a valid null-niche pointer enum (None = 0, Some = 1).
    AddressTag(ValueId),
    LocationTag(List),
    AddressPart {
        address: ValueId,
        index: u8,
    },
    LoadAddress(List),
    LoadAddressCopy(List),
    /// [source root, source offset, destination root, destination offset, bytes].
    CopyStorage {
        parts: List,
        // Intern exact layouts instead of enlarging every instruction.
        layouts: [TypeId; 2],
        nonoverlapping: bool,
    },
    StoreAddress {
        parts: List,
        value: ValueId,
    },
    /// The authoritative primitive array backing an addressable scalar local.
    SlotRoot(SlotId),
    View {
        data: ValueId,
        length: ValueId,
    },
    /// Boundary materialization from [backing, start, logical length].
    ViewPack(List),
    /// Read one physical component without constructing an address.
    ViewPart {
        view: ValueId,
        index: u8,
    },
    ViewData {
        view: ValueId,
        size: u32,
        codec: Option<SymbolId>,
    },
    /// Extract an address from [backing, start, logical length].
    /// No view object is needed, including at memory boundaries.
    ViewAddress {
        parts: List,
        size: u32,
        codec: Option<SymbolId>,
    },
    /// Normalize a nonzero-sized slice backing to retain its exact element layout.
    /// Keep the element displacement separate.
    ViewRoot {
        backing: ValueId,
        size: u32,
        codec: Option<SymbolId>,
    },
    /// Extract a view backing or element start from a scalar location.
    /// Do not allocate a thin-pointer carrier.
    AddressViewPart {
        address: ValueId,
        index: u8,
    },
    TypedAddressViewPart {
        parts: List,
        size: u32,
        codec: Option<SymbolId>,
        index: u8,
    },
}

impl Op {
    /// Language-level traps and representation operations need unwind edges;
    /// scalar computations and literal loads do not.
    pub fn may_throw(self, body: &Body, types: &Types) -> bool {
        match self {
            Self::Constant(id) => matches!(
                body.constants[id.index()],
                Constant::External { pure: false, .. }
            ),
            // Native scratch is initialized and accessed within its declared bounds.
            // Unused reads have no Rust-visible effect.
            Self::ArrayGet { native: true, .. } => false,
            Self::Nop
            | Self::Exception
            | Self::AddressTag(_)
            | Self::LocationTag(_)
            | Self::Reinterpret(_)
            | Self::TaggedPack(_)
            | Self::TaggedPart { .. }
            | Self::Refine(_)
            | Self::Not(_)
            | Self::Neg(_)
            | Self::Bit { .. }
            | Self::Overflow { .. } => false,
            // A typed field address has no source-language effects until used.
            Self::Project { .. }
            | Self::ProjectRoot { .. }
            | Self::ProjectOffset { .. }
            | Self::RetypeAddress { size: 1.., .. }
            | Self::ViewRoot { .. }
            | Self::TypedAddressPack { .. } => false,
            Self::Binary {
                op: BinaryOp::Div | BinaryOp::Rem,
                left,
                ..
            } => {
                matches!(types.get(body.value_type(left)), Some(Type::Scalar(ty)) if ty.integer().is_some())
            }
            Self::Binary { .. } => false,
            Self::Cast(value) => {
                !matches!(types.get(body.value_type(value)), Some(Type::Scalar(_)))
            }
            _ => true,
        }
    }
    pub fn visit_uses(self, args: &[ValueId], mut visit: impl FnMut(ValueId)) {
        match self {
            Self::ProjectRoot { address, .. } => {
                for &value in &args[address.range()] {
                    visit(value);
                }
            }
            Self::ProjectOffset { root, base, offset } => {
                visit(root);
                visit(base);
                visit(offset);
            }
            Self::AddressViewPart { address, .. } => visit(address),
            Self::ViewRoot { backing, .. } => visit(backing),
            Self::RetypeAddress { pointer, .. } => visit(pointer),
            Self::Binary { left, right, .. }
            | Self::AddressEqual { left, right }
            | Self::AddressCompare { left, right } => {
                visit(left);
                visit(right);
            }
            Self::TaggedPart { value: v, .. }
            | Self::AddressTag(v)
            | Self::Not(v)
            | Self::Opaque(v)
            | Self::Bit { value: v, .. }
            | Self::Neg(v)
            | Self::Cast(v)
            | Self::Refine(v)
            | Self::Adapt(v)
            | Self::Reinterpret(v)
            | Self::NewArray(v)
            | Self::ScalarCell(v)
            | Self::ArrayLength(v)
            | Self::Load(v)
            | Self::CopyValue(v)
            | Self::LoadCopy(v)
            | Self::Commit(v)
            | Self::Length(v) => visit(v),
            Self::StoreSlot { value, .. } | Self::SetStatic { value, .. } => visit(value),
            Self::Store { pointer, value }
            | Self::ArrayFill {
                array: pointer,
                value,
            } => {
                visit(pointer);
                visit(value);
            }
            Self::Project { base, .. }
            | Self::LoadField { base, .. }
            | Self::LoadFieldCopy { base, .. }
            | Self::LoadFieldPart { base, .. } => visit(base),
            Self::StoreFieldParts { base, parts, .. } => {
                visit(base);
                for &value in &args[parts.range()] {
                    visit(value);
                }
            }
            Self::StoreField { base, value, .. } => {
                visit(base);
                visit(value);
            }
            Self::ViewData { view, .. }
            | Self::ViewPart { view, .. }
            | Self::AddressPart { address: view, .. } => visit(view),
            Self::View { data, length } => {
                visit(data);
                visit(length);
            }
            Self::Offset {
                pointer, offset, ..
            } => {
                visit(pointer);
                visit(offset);
            }
            Self::Call { args: list, .. }
            | Self::Heap { args: list, .. }
            | Self::StoreStorageField { args: list, .. }
            | Self::LoadStorageField { address: list, .. }
            | Self::LoadStorageFieldCopy { address: list, .. }
            | Self::Overflow { args: list, .. }
            | Self::TaggedPack(list)
            | Self::ViewPack(list)
            | Self::ViewGet(list)
            | Self::AddressPack(list)
            | Self::LoadAddress(list)
            | Self::LoadAddressCopy(list)
            | Self::CopyStorage { parts: list, .. }
            | Self::LoadTypedCopy { parts: list, .. }
            | Self::LoadTyped { parts: list, .. }
            | Self::StoreTyped { parts: list, .. }
            | Self::TypedAddressPack { parts: list, .. }
            | Self::TypedAddressViewPart { parts: list, .. }
            | Self::LocationTag(list)
            | Self::LocationEqual(list)
            | Self::LocationCompare(list)
            | Self::ViewAddress { parts: list, .. } => {
                for &arg in &args[list.range()] {
                    visit(arg);
                }
            }
            Self::StoreAddress { parts, value } | Self::ViewSet { parts, value } => {
                for &part in &args[parts.range()] {
                    visit(part);
                }
                visit(value);
            }
            Self::GetField { object, .. } => visit(object),
            Self::SetField { object, value, .. } => {
                visit(object);
                visit(value);
            }
            Self::ArrayGet { array, index, .. } => {
                visit(array);
                visit(index);
            }
            Self::ArraySet {
                array,
                index,
                value,
                ..
            } => {
                visit(array);
                visit(index);
                visit(value);
            }
            Self::Nop
            | Self::Constant(_)
            | Self::Exception
            | Self::LoadSlot(_)
            | Self::AddressOfSlot(_)
            | Self::SlotRoot(_)
            | Self::GetStatic(_)
            | Self::FunctionPointer { .. } => {}
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Inst {
    pub op: Op,
    pub result: Option<ValueId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Constant {
    Scalar(Scalar),
    Unit,
    Null(TypeId),
    /// Initial contents of an uninitialized source local. Valid Rust control
    /// flow must overwrite this before observation; edge copies may carry it.
    Uninit(TypeId),
    /// Handle into the immutable representation-constant pool supplied by the
    /// embedding compiler. Its emitter must produce one typed stack value.
    External {
        index: u32,
        ty: TypeId,
        /// A literal with no initialization or runtime helper effects.
        pure: bool,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Edge {
    pub target: BlockId,
    pub args: Vec<ValueId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Terminator {
    Jump(EdgeId),
    Branch {
        condition: ValueId,
        yes: EdgeId,
        no: EdgeId,
    },
    Switch {
        value: ValueId,
        cases: List,
        otherwise: EdgeId,
    },
    Return(Option<ValueId>),
    Throw {
        value: ValueId,
        unwind: Option<EdgeId>,
    },
    Rethrow,
    Unreachable,
    /// The normal successor is a dedicated single-predecessor continuation.
    /// The instruction result is defined there, never on the exceptional edge.
    Invoke {
        inst: InstId,
        normal: EdgeId,
        unwind: EdgeId,
    },
}

impl Terminator {
    pub fn visit_uses(self, mut visit: impl FnMut(ValueId)) {
        match self {
            Self::Branch {
                condition: value, ..
            }
            | Self::Switch { value, .. }
            | Self::Return(Some(value))
            | Self::Throw { value, .. } => visit(value),
            _ => {}
        }
    }
    pub fn visit_edges(self, cases: &[(Scalar, EdgeId)], mut visit: impl FnMut(EdgeId)) {
        match self {
            Self::Jump(edge) => visit(edge),
            Self::Branch { yes, no, .. } => {
                visit(yes);
                visit(no);
            }
            Self::Invoke { normal, unwind, .. } => {
                visit(normal);
                visit(unwind);
            }
            Self::Switch {
                cases: list,
                otherwise,
                ..
            } => {
                for &(_, edge) in &cases[list.range()] {
                    visit(edge);
                }
                visit(otherwise);
            }
            Self::Throw { unwind, .. } => {
                if let Some(edge) = unwind {
                    visit(edge);
                }
            }
            Self::Return(_) | Self::Rethrow | Self::Unreachable => {}
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Block {
    pub params: Vec<ValueId>,
    pub instructions: Vec<InstId>,
    pub terminator: Option<Terminator>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Body {
    pub entry: BlockId,
    pub return_type: TypeId,
    pub blocks: Vec<Block>,
    pub values: Vec<Value>,
    pub instructions: Vec<Inst>,
    pub constants: Vec<Constant>,
    pub args: Vec<ValueId>,
    pub edges: Vec<Edge>,
    pub cases: Vec<(Scalar, EdgeId)>,
    pub slots: Vec<StorageSlot>,
    pub methods: Vec<MethodRef>,
    pub fields: Vec<FieldRef>,
    pub projections: Vec<PointerProjection>,
}

impl Body {
    pub fn new(return_type: TypeId) -> Self {
        Self {
            entry: BlockId::new(0),
            return_type,
            blocks: vec![Block::default()],
            values: Vec::new(),
            instructions: Vec::new(),
            constants: Vec::new(),
            args: Vec::new(),
            edges: Vec::new(),
            cases: Vec::new(),
            slots: Vec::new(),
            methods: Vec::new(),
            fields: Vec::new(),
            projections: Vec::new(),
        }
    }
    pub fn resolve(&self, mut value: ValueId) -> ValueId {
        while let ValueDef::Alias(next) = self.values[value.index()].def {
            value = next;
        }
        value
    }
    pub(crate) fn resolve_mut(&mut self, mut value: ValueId) -> ValueId {
        let root = self.resolve(value);
        while let ValueDef::Alias(next) = self.values[value.index()].def {
            self.values[value.index()].def = ValueDef::Alias(root);
            value = next;
        }
        root
    }
    pub fn value_type(&self, value: ValueId) -> TypeId {
        self.values[value.index()].ty
    }
    pub fn predecessors(&self) -> Vec<Vec<(BlockId, EdgeId)>> {
        let mut result = vec![Vec::new(); self.blocks.len()];
        for (index, block) in self.blocks.iter().enumerate() {
            if let Some(term) = block.terminator {
                term.visit_edges(&self.cases, |edge| {
                    result[self.edges[edge.index()].target.index()]
                        .push((BlockId::new(index), edge))
                });
            }
        }
        result
    }
    pub fn reachable(&self) -> Vec<bool> {
        let mut visited = vec![false; self.blocks.len()];
        let mut pending = vec![self.entry];
        while let Some(block) = pending.pop() {
            if std::mem::replace(&mut visited[block.index()], true) {
                continue;
            }
            if let Some(term) = self.blocks[block.index()].terminator {
                term.visit_edges(&self.cases, |edge| {
                    pending.push(self.edges[edge.index()].target)
                });
            }
        }
        visited
    }
    /// Place normal continuations together and cold unwind paths afterwards.
    /// The iterative reverse postorder also handles loops without recursion.
    pub fn layout(&self) -> Vec<BlockId> {
        let mut seen = vec![false; self.blocks.len()];
        let mut pending = vec![(self.entry, false)];
        let mut order = Vec::new();
        while let Some((block, exiting)) = pending.pop() {
            if exiting {
                order.push(block);
                continue;
            }
            if std::mem::replace(&mut seen[block.index()], true) {
                continue;
            }
            pending.push((block, true));
            if let Some(term) = self.blocks[block.index()].terminator {
                term.visit_edges(&self.cases, |edge| {
                    pending.push((self.edges[edge.index()].target, false))
                });
            }
        }
        order.reverse();
        order
    }
}
