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
    /// Generated Rust pointer fields store a base plus two displacement fields.
    pub relative_pointer: bool,
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
            I16 | U16 => 2,
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
    /// Representation adaptation at a JVM ABI boundary (boxing, views, casts).
    Adapt(ValueId),
    /// Same physical JVM carrier with a different semantic type annotation.
    Reinterpret(ValueId),
    NewArray(ValueId),
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
    Store {
        pointer: ValueId,
        value: ValueId,
    },
    Project {
        base: ValueId,
        projection: ProjectionId,
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
    },
    ArraySet {
        array: ValueId,
        index: ValueId,
        value: ValueId,
    },
    /// Logical Rust length, including zero-sized slices larger than JVM arrays.
    Length(ValueId),
    /// Immutable fat-pointer carrier; data retains allocation identity and view.
    View {
        data: ValueId,
        length: ValueId,
    },
    ViewData {
        view: ValueId,
        size: u32,
        codec: Option<SymbolId>,
    },
}

impl Op {
    /// Language-level traps and representation operations need unwind edges;
    /// scalar computations and literal loads do not.
    pub fn may_throw(self, body: &Body, types: &Types) -> bool {
        match self {
            Self::Constant(id) => matches!(body.constants[id.index()], Constant::External { .. }),
            Self::Exception
            | Self::Reinterpret(_)
            | Self::Not(_)
            | Self::Neg(_)
            | Self::Bit { .. }
            | Self::Overflow { .. } => false,
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
            Self::Binary { left, right, .. } => {
                visit(left);
                visit(right);
            }
            Self::Not(v)
            | Self::Opaque(v)
            | Self::Bit { value: v, .. }
            | Self::Neg(v)
            | Self::Cast(v)
            | Self::Adapt(v)
            | Self::Reinterpret(v)
            | Self::NewArray(v)
            | Self::ArrayLength(v)
            | Self::Load(v)
            | Self::Length(v) => visit(v),
            Self::StoreSlot { value, .. } | Self::SetStatic { value, .. } => visit(value),
            Self::Store { pointer, value } => {
                visit(pointer);
                visit(value);
            }
            Self::Project { base, .. } => visit(base),
            Self::ViewData { view, .. } => visit(view),
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
            Self::Call { args: list, .. } | Self::Overflow { args: list, .. } => {
                for &arg in &args[list.range()] {
                    visit(arg);
                }
            }
            Self::GetField { object, .. } => visit(object),
            Self::SetField { object, value, .. } => {
                visit(object);
                visit(value);
            }
            Self::ArrayGet { array, index } => {
                visit(array);
                visit(index);
            }
            Self::ArraySet {
                array,
                index,
                value,
            } => {
                visit(array);
                visit(index);
                visit(value);
            }
            Self::Constant(_)
            | Self::Exception
            | Self::LoadSlot(_)
            | Self::AddressOfSlot(_)
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
    pub(super) fn resolve_mut(&mut self, mut value: ValueId) -> ValueId {
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
