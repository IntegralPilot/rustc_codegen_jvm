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
            AddressViewPart { address, index } => AddressViewPart {
                address: map.value(address),
                index,
            },
            RetypeAddress {
                pointer,
                size,
                codec,
            } => RetypeAddress {
                pointer: map.value(pointer),
                size,
                codec,
            },
            TypedAddressPack { parts, size, codec } => TypedAddressPack {
                parts: map.args(parts),
                size,
                codec,
            },
            LoadTypedCopy { parts, size, codec } => LoadTypedCopy {
                parts: map.args(parts),
                size,
                codec,
            },
            LoadTyped { parts, size, codec } => LoadTyped {
                parts: map.args(parts),
                size,
                codec,
            },
            StoreTyped { parts, size, codec } => StoreTyped {
                parts: map.args(parts),
                size,
                codec,
            },
            Nop => Nop,
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
            Refine(v) => Refine(map.value(v)),
            Adapt(v) => Adapt(map.value(v)),
            Reinterpret(v) => Reinterpret(map.value(v)),
            NewArray(v) => NewArray(map.value(v)),
            ScalarCell(v) => ScalarCell(map.value(v)),
            Heap { operation, args } => Heap {
                operation,
                args: map.args(args),
            },
            ProjectRoot {
                address,
                projection,
            } => ProjectRoot {
                address: map.args(address),
                projection: map.projection(projection),
            },
            ProjectOffset { root, base, offset } => ProjectOffset {
                root: map.value(root),
                base: map.value(base),
                offset: map.value(offset),
            },
            ArrayLength(v) => ArrayLength(map.value(v)),
            Length(v) => Length(map.value(v)),
            Load(v) => Load(map.value(v)),
            LoadCopy(v) => LoadCopy(map.value(v)),
            CopyValue(v) => CopyValue(map.value(v)),
            Commit(v) => Commit(map.value(v)),
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
            LoadField { base, projection } => LoadField {
                base: map.value(base),
                projection: map.projection(projection),
            },
            LoadFieldCopy { base, projection } => LoadFieldCopy {
                base: map.value(base),
                projection: map.projection(projection),
            },
            LoadStorageFieldCopy {
                address,
                projection,
            } => LoadStorageFieldCopy {
                address: map.args(address),
                projection: map.projection(projection),
            },
            LoadStorageField {
                address,
                projection,
                index,
            } => LoadStorageField {
                address: map.args(address),
                projection: map.projection(projection),
                index,
            },
            StoreStorageField {
                args,
                projection,
                split,
            } => StoreStorageField {
                args: map.args(args),
                projection: map.projection(projection),
                split,
            },
            LoadFieldPart {
                base,
                projection,
                index,
            } => LoadFieldPart {
                base: map.value(base),
                projection: map.projection(projection),
                index,
            },
            StoreFieldParts {
                base,
                projection,
                parts,
            } => StoreFieldParts {
                base: map.value(base),
                projection: map.projection(projection),
                parts: map.args(parts),
            },
            StoreField {
                base,
                projection,
                value,
            } => StoreField {
                base: map.value(base),
                projection: map.projection(projection),
                value: map.value(value),
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
            ArrayGet {
                array,
                index,
                native,
            } => ArrayGet {
                native,
                array: map.value(array),
                index: map.value(index),
            },
            ArrayGetCopy { array, index } => ArrayGetCopy {
                array: map.value(array),
                index: map.value(index),
            },
            ArraySet {
                array,
                index,
                value,
                native,
            } => ArraySet {
                native,
                array: map.value(array),
                index: map.value(index),
                value: map.value(value),
            },
            View { data, length } => View {
                data: map.value(data),
                length: map.value(length),
            },
            ViewPack(parts) => ViewPack(map.args(parts)),
            TaggedPack(parts) => TaggedPack(map.args(parts)),
            TaggedPart { value, index } => TaggedPart {
                value: map.value(value),
                index,
            },
            ArrayFill { array, value } => ArrayFill {
                array: map.value(array),
                value: map.value(value),
            },
            ViewGet(parts) => ViewGet(map.args(parts)),
            ViewGetCopy(parts) => ViewGetCopy(map.args(parts)),
            ViewSet { parts, value } => ViewSet {
                parts: map.args(parts),
                value: map.value(value),
            },
            AddressPack(parts) => AddressPack(map.args(parts)),
            AddressEqual { left, right } => AddressEqual {
                left: map.value(left),
                right: map.value(right),
            },
            AddressCompare { left, right } => AddressCompare {
                left: map.value(left),
                right: map.value(right),
            },
            AddressTag(value) => AddressTag(map.value(value)),
            LocationTag(parts) => LocationTag(map.args(parts)),
            LocationEqual(parts) => LocationEqual(map.args(parts)),
            LocationCompare(parts) => LocationCompare(map.args(parts)),
            LoadAddress(parts) => LoadAddress(map.args(parts)),
            LoadAddressCopy(parts) => LoadAddressCopy(map.args(parts)),
            CopyStorage {
                parts,
                layouts,
                nonoverlapping,
            } => CopyStorage {
                parts: map.args(parts),
                layouts,
                nonoverlapping,
            },
            StoreAddress { parts, value } => StoreAddress {
                parts: map.args(parts),
                value: map.value(value),
            },
            AddressPart { address, index } => AddressPart {
                address: map.value(address),
                index,
            },
            SlotRoot(slot) => SlotRoot(map.slot(slot)),
            ViewPart { view, index } => ViewPart {
                view: map.value(view),
                index,
            },
            ViewData { view, size, codec } => ViewData {
                view: map.value(view),
                size,
                codec,
            },
            TypedAddressViewPart {
                parts,
                size,
                codec,
                index,
            } => TypedAddressViewPart {
                parts: map.args(parts),
                size,
                codec,
                index,
            },
            ViewAddress { parts, size, codec } => ViewAddress {
                parts: map.args(parts),
                size,
                codec,
            },
            ViewRoot {
                backing,
                size,
                codec,
            } => ViewRoot {
                backing: map.value(backing),
                size,
                codec,
            },
        }
    }
}
