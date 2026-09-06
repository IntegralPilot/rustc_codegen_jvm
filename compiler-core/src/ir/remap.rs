//! Relocate a body region without exposing operand layout to transformations.
use super::*;

pub trait Remap {
    fn value(&mut self, value: ValueId) -> ValueId;
    fn args(&mut self, args: List) -> List;
    fn constant(&mut self, constant: ConstId) -> ConstId;
    fn method(&mut self, method: MethodId) -> MethodId;
    fn field(&mut self, field: MemberId) -> MemberId;
    fn projection(&mut self, projection: ProjectionId) -> ProjectionId;
    fn slot(&mut self, slot: SlotId) -> SlotId;
}
impl Op {
    pub fn remap(self, map: &mut impl Remap) -> Self {
        use Op::*;
        match self {
            Constant(c) => Constant(map.constant(c)),
            Exception => Exception,
            Binary { op, left, right } => Binary {
                op,
                left: map.value(left),
                right: map.value(right),
            },
            Overflow { op, args } => Overflow {
                op,
                args: map.args(args),
            },
            Not(v) => Not(map.value(v)),
            Neg(v) => Neg(map.value(v)),
            Bit { op, value } => Bit {
                op,
                value: map.value(value),
            },
            Opaque(v) => Opaque(map.value(v)),
            Cast(v) => Cast(map.value(v)),
            Adapt(v) => Adapt(map.value(v)),
            Reinterpret(v) => Reinterpret(map.value(v)),
            NewArray(v) => NewArray(map.value(v)),
            ArrayLength(v) => ArrayLength(map.value(v)),
            Length(v) => Length(map.value(v)),
            Load(v) => Load(map.value(v)),
            FunctionPointer { signature, target } => FunctionPointer {
                signature: map.method(signature),
                target: map.method(target),
            },
            LoadSlot(slot) => LoadSlot(map.slot(slot)),
            AddressOfSlot(slot) => AddressOfSlot(map.slot(slot)),
            StoreSlot { slot, value } => StoreSlot {
                slot: map.slot(slot),
                value: map.value(value),
            },
            Store { pointer, value } => Store {
                pointer: map.value(pointer),
                value: map.value(value),
            },
            Project { base, projection } => Project {
                base: map.value(base),
                projection: map.projection(projection),
            },
            Offset {
                pointer,
                offset,
                bytes,
                wrapping,
            } => Offset {
                pointer: map.value(pointer),
                offset: map.value(offset),
                bytes,
                wrapping,
            },
            Call { method, kind, args } => Call {
                method: map.method(method),
                kind,
                args: map.args(args),
            },
            GetField { object, field } => GetField {
                object: map.value(object),
                field: map.field(field),
            },
            SetField {
                object,
                field,
                value,
            } => SetField {
                object: map.value(object),
                field: map.field(field),
                value: map.value(value),
            },
            GetStatic(field) => GetStatic(map.field(field)),
            SetStatic { field, value } => SetStatic {
                field: map.field(field),
                value: map.value(value),
            },
            ArrayGet { array, index } => ArrayGet {
                array: map.value(array),
                index: map.value(index),
            },
            ArraySet {
                array,
                index,
                value,
            } => ArraySet {
                array: map.value(array),
                index: map.value(index),
                value: map.value(value),
            },
            View { data, length } => View {
                data: map.value(data),
                length: map.value(length),
            },
            ViewData { view, size, codec } => ViewData {
                view: map.value(view),
                size,
                codec,
            },
        }
    }
}
