import java.util.Arrays;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import org.rustlang.runtime.OwnedReads;
import org.rustlang.runtime.MemoryBytes;
import org.rustlang.runtime.Pointer;

public final class MemoryViews {
    public static final class Pair {
        public int first;
        public int second;

        public Pair() { }

        public Pair(int first, int second) {
            this.first = first;
            this.second = second;
        }
    }

    public static final class PairCodec {
        static int encodes;
        static int rangeEncodes;
        static byte[] decodedStorage;

        public static byte[] e$pair(Pair value) {
            encodes++;
            byte[] bytes = new byte[8];
            MemoryBytes.write(bytes, 0, 4, value.first);
            MemoryBytes.write(bytes, 4, 4, value.second);
            return bytes;
        }

        public static Pair d$pair(byte[] bytes) {
            return a$pair(bytes, 0);
        }

        public static Pair a$pair(byte[] bytes, int offset) {
            decodedStorage = bytes;
            Pair value = new Pair();
            value.first = (int) MemoryBytes.read(bytes, offset, 4);
            value.second = (int) MemoryBytes.read(bytes, offset + 4, 4);
            return value;
        }

        public static void w$pair(Pair value, byte[] bytes, int offset) {
            rangeEncodes++;
            MemoryBytes.write(bytes, offset, 4, value.first);
            MemoryBytes.write(bytes, offset + 4, 4, value.second);
        }
    }

    private static final String PAIR_CODEC =
            "MemoryViews$PairCodec#pair#LMemoryViews$Pair;";

    private static Object ownedElement(MethodHandle read, Object backing, int index, String target) {
        if (read == null) return Pointer.sliceGetObjectCopy(backing, index, target);
        try {
            return read.invoke(backing, index);
        } catch (RuntimeException | Error error) {
            throw error;
        } catch (Throwable error) {
            throw new AssertionError(error);
        }
    }

    private static void checkOwnedSliceElements() {
        MethodHandle read = OwnedReads.bootstrap(MethodHandles.lookup(), "read",
                MethodType.methodType(Pair.class, Object.class, int.class)).dynamicInvoker();
        checkOwnedSliceElements(null);
        checkOwnedSliceElements(read);
        byte[] layouts = new byte[32];
        MemoryBytes.write(layouts, 8, 4, 41);
        MemoryBytes.write(layouts, 16, 4, 43);
        Pointer narrow = Pointer.array(layouts, 0, 1).retype(8, PAIR_CODEC);
        Pointer wide = narrow.retype(16, PAIR_CODEC);
        if (((Pair) ownedElement(read, narrow, 1, Pair.class.getName())).first != 41
                || ((Pair) ownedElement(read, wide, 1, Pair.class.getName())).first != 43
                || ((Pair) ownedElement(read, narrow, 1, Pair.class.getName())).first != 41) {
            throw new AssertionError("linked read reused a different element stride");
        }
        byte[][] arrays = {new byte[] {3, 5}};
        MethodHandle arrayRead = OwnedReads.bootstrap(MethodHandles.lookup(), "read",
                MethodType.methodType(byte[].class, Object.class, int.class)).dynamicInvoker();
        byte[] copy = (byte[]) ownedElement(arrayRead, arrays, 0, "[B");
        copy[0] = 7;
        if (arrays[0][0] != 3) throw new AssertionError("linked array copy aliases its source");
        Object empty = ownedElement(read, new Pair[] {null}, 0, Pair.class.getName());
        if (empty != null) throw new AssertionError("linked read did not preserve a null element");
        checkOwnedSliceElements(read);
    }

    private static void checkOwnedSliceElements(MethodHandle read) {
        byte[] bytes = new byte[24];
        MemoryBytes.write(bytes, 12, 4, 17);
        Pointer slice = Pointer.array(bytes, 0, 1).byte_offset(4).retype(8, PAIR_CODEC);
        Pair snapshot = (Pair) ownedElement(read, slice, 1, Pair.class.getName());
        if (snapshot.first != 17 || PairCodec.decodedStorage != bytes) {
            throw new AssertionError("owned slice read used the wrong range or copied storage");
        }
        snapshot.first = 23;
        if (slice.add(1).retype(4, null).getI32() != 17) {
            throw new AssertionError("owned slice read created a live view");
        }
        Pair live = (Pair) Pointer.sliceGetObject(slice, 1);
        live.first = 29;
        Pair next = (Pair) ownedElement(read, slice, 1, Pair.class.getName());
        if (next.first != 29 || snapshot.first != 23) {
            throw new AssertionError("owned slice read lost a pending write or changed a snapshot");
        }
        Pointer.withMetadata(slice, 19);
        slice.addr();
        Pair exposed = (Pair) ownedElement(read, slice, 1, Pair.class.getName());
        exposed.first = 31;
        if (next.first != 29 || slice.metadata() != 19 || MemoryBytes.read(bytes, 12, 4) != 29)
            throw new AssertionError("owned slice read changed exposed storage or metadata");
        byte[][] arrays = {new byte[] {3, 5}};
        byte[] copied = (byte[]) Pointer.sliceGetObjectCopy(arrays, 0, "[B");
        copied[0] = 7;
        if (arrays[0][0] != 3) throw new AssertionError("owned array element still aliases its source");
        try {
            ownedElement(read, slice, 2, Pair.class.getName());
            throw new AssertionError("owned slice read accepted an invalid byte window");
        } catch (IndexOutOfBoundsException expected) { }
    }

    public static void main(String[] args) {
        check();
    }

    private static Pair view(Pointer bytes) {
        return (Pair) bytes.retype(8, PAIR_CODEC).getObject();
    }

    public static void check() {
        Object nullableOwner = Pointer.storageAligned(new Pair(3, 5), 8, PAIR_CODEC, 8);
        if (Pointer.nullableLocationTag(nullableOwner, 0) != 1
                || Pointer.nullableLocationTag(new int[] {7}, 0) != 1
                || Pointer.nullableLocationTag(null, 0) != 0
                || Pointer.nullableLocationTag(null, 8) != 1
                || Pointer.nullableTag(new Pointer(0L, 4)) != 0
                || Pointer.nullableTag(new Pointer(4L, 4)) != 1
                || Pointer.nullableLocationTag(new Pointer(4L, 4), -4) != 0) {
            throw new AssertionError("invalid nullable pointer tag");
        }
        Pair erasedOwner = new Pair(3, 5);
        Pointer.field(erasedOwner, "erasedMarker", 0, null);
        Pointer erasedRoot = Pointer.cell(erasedOwner, 8, PAIR_CODEC);
        Pointer erased = erasedRoot.projectStructField(Pair.class.getName(), "erasedMarker", 8, 0, null);
        if (!Pointer.sameLocation(erased, 0, erasedRoot, 8)) {
            throw new AssertionError("erased field lost its containing allocation");
        }
        try {
            Pointer.field(erasedOwner, "missingValue", 4, null);
            throw new AssertionError("a missing non-ZST field was accepted");
        } catch (IllegalArgumentException expected) { }
        checkTypedStorage();
        checkOwnedRanges();
        checkTypedCopies();
        checkPointerTypedStores();
        checkSliceComponents();
        checkOwnedSliceElements();
        byte[] delayedBytes = new byte[8];
        Pointer delayed = Pointer.array(delayedBytes, 0, 1).retype(8, PAIR_CODEC);
        Pointer commit = Pointer.fromStorageLocation(delayed, 0);
        Pair delayedView = (Pair) Pointer.loadStorageLocation(delayed, 0, Pair.class.getName());
        delayedView.first = 139;
        commit.commitMemoryView();
        if (MemoryBytes.read(delayedBytes, 0, 4) != 139) {
            throw new AssertionError("component boundary detached a later view binding");
        }
        Pair firstPair = new Pair();
        Pair secondPair = new Pair();
        Pointer pairArray = Pointer.array(new Pair[] { firstPair, secondPair }, 0, 8, PAIR_CODEC);
        if (Pointer.locationStride(pairArray) != 8
                || Pointer.directStorageAggregate(pairArray, 8, Pair.class) != secondPair
                || Pointer.loadStorageLocation(pairArray, 8, Pair.class.getName()) != secondPair) {
            throw new AssertionError("aggregate component address lost its element layout");
        }
        Pointer pairCell = Pointer.cell(firstPair, 8, PAIR_CODEC);
        Pointer.storeStorageLocation(pairCell, 0, secondPair);
        if (Pointer.directStorageAggregate(pairCell, 0, Pair.class) != secondPair) {
            throw new AssertionError("aggregate component address retained a replaced object");
        }
        byte[] storage = new byte[16];
        Pointer bytes = Pointer.array(storage, 0, 1);
        Pair original = view(bytes);
        original.first = 17;
        original.second = 23;
        int before = PairCodec.encodes;
        bytes.retype(8, null).set(0x0000003300000022L);
        if (PairCodec.encodes != before) {
            throw new AssertionError("complete overwrite serialized the discarded view");
        }
        Pair replaced = view(bytes);
        if (replaced.first != 0x22 || replaced.second != 0x33) {
            throw new AssertionError("complete overwrite left a stale view");
        }

        replaced.first = 41;
        replaced.second = 43;
        bytes.retype(4, null).set(47);
        Pair partial = view(bytes);
        if (partial.first != 47 || partial.second != 43) {
            throw new AssertionError("partial overwrite lost the untouched field");
        }

        partial.first = 51;
        partial.second = 53;
        Pair following = view(bytes.byte_offset(8));
        following.first = 57;
        following.second = 59;
        bytes.byte_offset(4).retype(8, null).set(0x0000006500000063L);
        Pair left = view(bytes);
        Pair right = view(bytes.byte_offset(8));
        if (left.first != 51 || left.second != 99 || right.first != 101 || right.second != 59) {
            throw new AssertionError("overlapping write lost adjacent live fields");
        }

        left.first = 71;
        Pointer copiedPointer = bytes.retype(8, PAIR_CODEC);
        Pair snapshot = (Pair) copiedPointer.getObjectCopyAs(Pair.class.getName());
        if (PairCodec.decodedStorage != storage) {
            throw new AssertionError("plain aggregate read copied its source buffer");
        }
        if (snapshot.first != 71 || snapshot.second != 99) {
            throw new AssertionError("value copy ignored pending mutations");
        }
        snapshot.first = 73;
        if (view(bytes).first != 71) {
            throw new AssertionError("value copy retained a live alias");
        }
        Pair offsetCopy = (Pair) bytes.byte_offset(8).retype(8, PAIR_CODEC)
                .getObjectCopyAs(Pair.class.getName());
        if (offsetCopy.first != 101 || offsetCopy.second != 59
                || PairCodec.decodedStorage != storage) {
            throw new AssertionError("plain aggregate read used the wrong source window");
        }

        byte[] plain = new byte[24];
        if (Pointer.directStorageAggregate(plain, 8, Pair.class) != null) {
            throw new AssertionError("byte storage unexpectedly supplied a managed aggregate");
        }
        // Byte-backed fields need no Java member. Test both root representations.
        for (Object root : new Object[] {plain, Pointer.array(plain, 0, 1)}) {
            Pointer.storeScalarField(root, 8, "absent.Owner", "absentField", 4, 97, 4);
            if (Pointer.loadScalarField(root, 8, "absent.Owner", "absentField", 4, 4) != 97
                    || MemoryBytes.read(plain, 8, 4) != 0 || MemoryBytes.read(plain, 16, 4) != 0) {
                throw new AssertionError("scalar field fallback lost its exact byte window");
            }
        }
        Pointer.storeTypedStorage(plain, 8, 8, PAIR_CODEC, new Pair(107, 109));
        if (MemoryBytes.read(plain, 8, 4) != 107 || MemoryBytes.read(plain, 12, 4) != 109
                || MemoryBytes.read(plain, 4, 4) != 0 || MemoryBytes.read(plain, 16, 4) != 0) {
            throw new AssertionError("typed store changed the wrong byte window");
        }
        // Existing decoded views and aliases require the general write protocol.
        Pair live = view(bytes);
        live.first = 103;
        Pointer.storeTypedStorage(storage, 8, 8, PAIR_CODEC, new Pair(113, 127));
        if (view(bytes).first != 103 || view(bytes.byte_offset(8)).first != 113
                || view(bytes.byte_offset(8)).second != 127) {
            throw new AssertionError("typed store bypassed a pending memory view");
        }
        // A scalar field write must preserve pending view writes and the adjacent field.
        Pair fieldPending = view(bytes);
        fieldPending.first = 79;
        fieldPending.second = 81;
        Pointer projectedSecond = bytes.retype(8, PAIR_CODEC)
                .projectStructField(Pair.class.getName(), "second", 4, 4, null);
        if (Pointer.loadLocationBits(projectedSecond, 0, 4) != 81) {
            throw new AssertionError("scalar field read ignored a pending view");
        }
        if (Pointer.loadScalarField(bytes, 0, Pair.class.getName(), "second", 4, 4) != 81) {
            throw new AssertionError("scalar field fallback ignored a pending view");
        }
        Pointer.storeScalarField(bytes, 0, Pair.class.getName(), "second", 4, 82, 4);
        if (view(bytes).first != 79 || view(bytes).second != 82) {
            throw new AssertionError("scalar field write lost its sibling or view coherence");
        }
        Pair pending = view(bytes);
        pending.second = 83;
        if (Pointer.loadLocationBits(storage, 4, 4) != 83) {
            throw new AssertionError("component address ignored a pending typed view");
        }
        view(bytes).second = 87;
        Pointer.storeLocationBits(storage, 0, 89, 4);
        if (view(bytes).first != 89 || view(bytes).second != 87) {
            throw new AssertionError("component address lost an adjacent pending field");
        }
        view(bytes).second = 83;
        byte[] image = new byte[storage.length];
        Pointer.encodeArrayMemory(storage, image, 0, 1, null);
        if (MemoryBytes.read(image, 4, 4) != 83) {
            throw new AssertionError("byte array encoding ignored a live aggregate view");
        }

        byte[] encoded = {9, 9, 9, 9, 9, 9};
        Pointer.encodeArrayMemory(new byte[] {1, 2, 3, 4}, encoded, 1, 1, null);
        if (!Arrays.equals(encoded, new byte[] {9, 1, 2, 3, 4, 9})) {
            throw new AssertionError("byte array encoding changed surrounding bytes");
        }
        byte[] decoded = new byte[4];
        Pointer.decodeArrayMemory(encoded, 1, decoded, 1, null);
        if (!Arrays.equals(decoded, new byte[] {1, 2, 3, 4})) {
            throw new AssertionError("byte array decoding read the wrong range");
        }

        Pair fields = new Pair();
        fields.first = 17;
        fields.second = 23;
        Pointer root = Pointer.cell(fields, 8, PAIR_CODEC);
        Pointer first = root.projectStructField(Pair.class.getName(), "first", 0, 4, null);
        if (Pointer.loadLocationBits(first, 4, 4) != 23) {
            throw new AssertionError("component address escaped the wrong field owner");
        }
        if (first.add(0).offset_from(first) != 0 || first.add(1).getI32() != 23) {
            throw new AssertionError("pointer arithmetic lost the containing allocation");
        }
        Pointer next = Pointer.fromLocation(first, 4, 4);
        if (next.getI32() != 23 || next.offset_from(root.retype(4)) != 1) {
            throw new AssertionError("materialized component address lost its field origin");
        }
        Pointer back = Pointer.fromLocation(next, -4, 4);
        if (back.getI32() != 17 || back.offset_from(first) != 0) {
            throw new AssertionError("materialized component address lost backward provenance");
        }
        if (!Pointer.sameLocation(first, 4, next, 0)
                || !Pointer.sameLocation(next, -4, first, 0)
                || Pointer.sameLocation(first, 0, next, 0)
                || !Pointer.sameLocation(null, 73, Pointer.fromAddress(73, 4), 0)) {
            throw new AssertionError("component equality lost an address or containing-field origin");
        }
        int[] cells = {4, 8};
        Object scalarRoot = Pointer.scalarSliceRoot(cells, 4);
        if (scalarRoot != cells || Pointer.loadLocationBits(scalarRoot, 4, 4) != 8) {
            throw new AssertionError("scalar slice lost direct array storage");
        }
        Pointer.storeLocationBits(scalarRoot, 4, 12, 4);
        if (cells[1] != 12) {
            throw new AssertionError("scalar slice write detached its allocation");
        }
        if (Pointer.locationSliceBacking(cells, 4, 4) != cells
                || Pointer.locationSliceOffset(cells, 4, 4) != 1
                || Pointer.locationSliceBacking(Pointer.array(cells, 1, 4), -4, 4) != cells
                || Pointer.locationSliceOffset(Pointer.array(cells, 1, 4), -4, 4) != 0
                || Pointer.fromLocation(cells, 4, 4).getI32() != 12) {
            throw new AssertionError("address/view round trip lost direct array storage");
        }
        try {
            Pointer.locationSliceOffset(cells, 1, 4);
            throw new AssertionError("unaligned slice conversion succeeded");
        } catch (IllegalStateException expected) { }
        Object fieldView = Pointer.locationSliceBacking(first, 4, 4);
        int fieldStart = Pointer.locationSliceOffset(first, 4, 4);
        if (Pointer.sliceGetI32(fieldView, fieldStart) != 23) {
            throw new AssertionError("address/view conversion lost a projected field origin");
        }
        if (!Pointer.sameLocation(cells, 4, Pointer.array(cells, 0, 4), 4)
                || Pointer.sameLocation(cells, 0, cells, 4)) {
            throw new AssertionError("component equality lost an array address");
        }
    }

    private static void checkTypedStorage() {
        Pair initial = new Pair();
        initial.first = 17;
        Object owner = Pointer.storageAligned(initial, 8, PAIR_CODEC, 8);
        if (owner instanceof Pointer || Pointer.locationStride(owner) != 8
                || Pointer.loadStorageLocation(owner, 0, Pair.class.getName()) != initial) {
            throw new AssertionError("typed storage eagerly materialized an address");
        }
        Pair replacement = new Pair();
        replacement.first = 23;
        replacement.second = 29;
        Pointer.storeStorageLocation(owner, 0, replacement);
        Pair copy = (Pair) Pointer.loadStorageCopy(owner, 0, Pair.class.getName());
        copy.first = 101;
        if (replacement.first != 23) throw new AssertionError("owned storage copy aliased its source");
        Pair direct = (Pair) Pointer.directStorageAggregate(owner, 0, Pair.class);
        if (direct != replacement) throw new AssertionError("typed root retained its old value");
        direct.second = 31;
        Pointer.commitStorageLocation(owner, 0);
        try {
            java.lang.reflect.Field boundary = owner.getClass().getDeclaredField("boundary");
            boundary.setAccessible(true);
            if (boundary.get(owner) != null) {
                throw new AssertionError("typed loads and stores allocated a Pointer");
            }
        } catch (ReflectiveOperationException error) {
            throw new AssertionError(error);
        }
        Pointer escaped = Pointer.fromStorageLocation(owner, 0);
        if (escaped != Pointer.fromStorageLocation(owner, 0)
                || !Pointer.sameLocation(owner, 0, escaped, 0)) {
            throw new AssertionError("materialization changed typed allocation identity");
        }
        Pointer first = escaped.projectStructField(Pair.class.getName(), "first", 0, 4, null);
        Pair next = new Pair();
        next.first = 37;
        next.second = 41;
        Pointer.storeStorageLocation(owner, 0, next);
        if (first.getI32() != 37) throw new AssertionError("field alias lost owner replacement");
        first.set(43);
        if (((Pair) Pointer.loadStorageLocation(owner, 0, Pair.class.getName())).first != 43) {
            throw new AssertionError("typed load lost an escaped field write");
        }
        Pointer.storeLocationBits(owner, 4, 47, 4);
        if (((Pair) Pointer.loadStorageLocation(owner, 0, Pair.class.getName())).second != 47
                || Pointer.loadLocationBits(owner, 0, 4) != 43) {
            throw new AssertionError("typed and byte storage disagree");
        }
    }

    private static void checkSliceComponents() {
        Pair[] values = {new Pair(3, 5), new Pair(7, 11), new Pair(13, 17)};
        Pointer array = Pointer.array(values, 1, 8, PAIR_CODEC);
        Object root = Pointer.sliceAddressRoot(array, 8, PAIR_CODEC);
        if (root != array || Pointer.directStorageAggregate(root, 8, Pair.class) != values[2]) {
            throw new AssertionError("normalized aggregate slice lost its root or counted the start twice");
        }
        Object subview = Pointer.typedLocationSliceBacking(root, 8, 8, PAIR_CODEC);
        int start = Pointer.typedLocationSliceOffset(root, 8, 8, PAIR_CODEC);
        if (subview != root || start != 1 || Pointer.sliceGetObject(subview, start) != values[2]) {
            throw new AssertionError("aggregate subview reconstructed or displaced its owner");
        }
        values[2] = new Pair(19, 23);
        if (Pointer.directStorageAggregate(root, 8, Pair.class) != values[2]) {
            throw new AssertionError("component slice retained a replaced array element");
        }
        byte[] bytes = new byte[24];
        Pointer base = Pointer.array(bytes, 0, 1).retype(8, PAIR_CODEC);
        root = Pointer.sliceAddressRoot(base, 8, PAIR_CODEC);
        if (root != base) throw new AssertionError("normalized byte slice allocated a new root");
        if (Pointer.typedLocationSliceBacking(root, 8, 8, PAIR_CODEC) != root
                || Pointer.typedLocationSliceOffset(root, 8, 8, PAIR_CODEC) != 1) {
            throw new AssertionError("byte subview reconstructed its root");
        }
        Pair live = (Pair) base.byte_offset(8).getObject();
        live.second = 29;
        Object field = Pointer.storageFieldRoot(root, 8, Pair.class.getName(), "second", 4, 4, null);
        long offset = Pointer.storageFieldOffset(field, root, 12);
        if (field != root || offset != 12 || Pointer.loadLocationBits(field, offset, 4) != 29) {
            throw new AssertionError("scalar field components lost a pending decoded view");
        }
        Pointer.storeLocationBits(field, offset, 31, 4);
        Pair copy = (Pair) Pointer.loadTypedStorageCopy(root, 8, 8, PAIR_CODEC, Pair.class.getName());
        if (copy.second != 31 || MemoryBytes.read(bytes, 4, 4) != 0) {
            throw new AssertionError("component write changed a neighboring element");
        }
        Object owner = Pointer.storageAligned(new Pair(37, 41), 8, PAIR_CODEC, 8);
        Object projected = Pointer.storageFieldRoot(owner, 0, Pair.class.getName(), "second", 4, 4, null);
        long displacement = Pointer.storageFieldOffset(projected, owner, 4);
        Pointer.storeStorageLocation(owner, 0, new Pair(43, 47));
        if (Pointer.loadLocationBits(projected, displacement, 4) != 47) {
            throw new AssertionError("component field detached from a replaceable owner");
        }
    }

    private static void checkPointerTypedStores() {
        byte[] bytes = new byte[24];
        Arrays.fill(bytes, (byte) 11);
        Pointer base = Pointer.array(bytes, 4, 1);
        int before = PairCodec.rangeEncodes;
        Pointer.storeTypedStorage(base, 4, 8, PAIR_CODEC, new Pair(17, 19));
        if (PairCodec.rangeEncodes != before + 1
                || MemoryBytes.read(bytes, 8, 4) != 17 || MemoryBytes.read(bytes, 12, 4) != 19
                || bytes[7] != 11 || bytes[16] != 11) {
            throw new AssertionError("typed pointer store missed its destination window");
        }
        Pointer start = base.byte_offset(-4);
        Pair left = view(start);
        Pair right = view(start.byte_offset(8));
        left.first = 23;
        left.second = 29;
        right.first = 31;
        right.second = 37;
        Pointer.storeTypedStorage(base, 0, 8, PAIR_CODEC, new Pair(41, 43));
        if (view(start).first != 23 || view(start).second != 41
                || view(start.byte_offset(8)).first != 43
                || view(start.byte_offset(8)).second != 37) {
            throw new AssertionError("typed pointer store lost adjacent pending view fields");
        }
        Pair source = view(start);
        source.first = 47;
        source.second = 53;
        Pointer.storeTypedStorage(base, -4, 8, PAIR_CODEC, source);
        if (view(start).first != 47 || view(start).second != 53) {
            throw new AssertionError("same-origin typed store discarded the source view");
        }
        source = view(start);
        source.second = 59;
        Pointer.storeTypedStorage(base, 12, 8, PAIR_CODEC, source);
        if (view(start.byte_offset(16)).first != 47 || view(start.byte_offset(16)).second != 59
                || view(start).second != 59) {
            throw new AssertionError("typed pointer store copied a stale live source");
        }
        byte[] snapshot = bytes.clone();
        try {
            Pointer.storeTypedStorage(base, 16, 8, PAIR_CODEC, new Pair(61, 67));
            throw new AssertionError("out-of-bounds typed pointer store succeeded");
        } catch (IndexOutOfBoundsException expected) { }
        if (!Arrays.equals(bytes, snapshot)) {
            throw new AssertionError("invalid typed pointer store changed the allocation");
        }
    }

    private static void checkTypedCopies() {
        byte[] bytes = new byte[24];
        MemoryBytes.write(bytes, 8, 4, 31);
        MemoryBytes.write(bytes, 12, 4, 37);
        for (Object root : new Object[] {bytes, Pointer.array(bytes, 0, 1)}) {
            Pair value = (Pair) Pointer.loadTypedStorageCopy(root, 8, 8, PAIR_CODEC, Pair.class.getName());
            if (value.first != 31 || value.second != 37 || PairCodec.decodedStorage != bytes) {
                throw new AssertionError("typed copy ignored its exact view layout or copied bytes");
            }
            value.first = 99;
            if (MemoryBytes.read(bytes, 8, 4) != 31) throw new AssertionError("copy became a live view");
        }
        Pointer pointer = Pointer.array(bytes, 0, 1);
        Pair live = (Pair) pointer.byte_offset(8).retype(8, PAIR_CODEC).getObject();
        live.second = 41;
        Pair value = (Pair) Pointer.loadTypedStorageCopy(bytes, 8, 8, PAIR_CODEC, Pair.class.getName());
        if (value.second != 41) throw new AssertionError("typed copy missed pending mutations");
        Object storage = Pointer.storageAligned(new Pair(43, 47), 8, PAIR_CODEC, 8);
        Pair copy = (Pair) Pointer.loadTypedStorageCopy(storage, 0, 8, PAIR_CODEC, Pair.class.getName());
        Pointer.storeStorageLocation(storage, 0, new Pair(53, 59));
        if (copy.first != 43) throw new AssertionError("owner replacement modified an owned copy");
        try {
            Pointer.loadTypedStorageCopy(bytes, 20, 8, PAIR_CODEC, Pair.class.getName());
            throw new AssertionError("typed copy did not check its byte window");
        } catch (IndexOutOfBoundsException expected) { }
    }

    private static void checkOwnedRanges() {
        byte[] bytes = new byte[24];
        Arrays.fill(bytes, (byte) 11);
        Pointer base = Pointer.array(bytes, 0, 1).retype(8, PAIR_CODEC);
        Pair value = new Pair();
        value.first = 53;
        value.second = 59;
        int before = PairCodec.rangeEncodes;
        Pointer.storeStorageLocation(base, 8, value);
        if (PairCodec.rangeEncodes != before + 1 || bytes[7] != 11 || bytes[16] != 11) {
            throw new AssertionError("range encoder failed to write directly to its destination window");
        }
        Pair copy = (Pair) Pointer.loadStorageCopy(base, 8, Pair.class.getName());
        if (copy.first != 53 || copy.second != 59 || PairCodec.decodedStorage != bytes) {
            throw new AssertionError("owned range load created a byte snapshot or read the wrong window");
        }
        copy.first = 61;
        if (MemoryBytes.read(bytes, 8, 4) != 53) {
            throw new AssertionError("owned range load established a live alias");
        }
        Pair bound = (Pair) base.byte_offset(8).getObjectAs(Pair.class.getName());
        bound.second = 67;
        Pair fresh = (Pair) Pointer.loadStorageCopy(base, 8, Pair.class.getName());
        if (fresh.second != 67) throw new AssertionError("owned range load ignored a live byte view");
        Pointer.storeStorageLocation(base, 8, copy);
        Pair reread = (Pair) base.byte_offset(8).getObjectAs(Pair.class.getName());
        if (reread.first != 61 || reread.second != 59) {
            throw new AssertionError("range store retained a stale decoded view");
        }
        try {
            Pointer.storeStorageLocation(base, 20, copy);
            throw new AssertionError("out-of-bounds range store succeeded");
        } catch (IndexOutOfBoundsException expected) { }
        if (bytes[20] != 11) throw new AssertionError("invalid range store wrote partial bytes");
    }
}
