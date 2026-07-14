package org.rustlang.runtime;

import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicLong;

public final class Pointer {
    private static final String MANAGED_OBJECT_VIEW_CODEC = "@managed-object";
    private static final String RAW_POINTER_VIEW_CODEC = "@raw-pointer";
    private static final String SIGNED_BIG_INTEGER_CODEC = "@signed-big-integer";
    private static final String UNSIGNED_BIG_INTEGER_CODEC = "@unsigned-big-integer";
    private static final String F128_CODEC = "@f128";
    private static final AtomicLong NEXT_ADDRESS = new AtomicLong(0x1_0000_0000L);
    private static final Map<Object, Long> ALLOCATION_BASES = new WeakHashMap<>();
    private static final Map<Object, Integer> ALLOCATION_ELEMENT_SIZES = new WeakHashMap<>();
    private static final Map<Object, Integer> ALLOCATION_ALIGNMENTS = new WeakHashMap<>();
    private static final Map<Object, String> ALLOCATION_CODECS = new WeakHashMap<>();
    private static final Map<Long, ExposedTarget> EXPOSED_ADDRESSES = new HashMap<>();
    private static final Map<String, Method[]> CODEC_METHODS = new HashMap<>();
    private static final Map<Object, Long> MANAGED_OBJECT_ADDRESSES = new IdentityHashMap<>();
    private static final Map<Long, WeakReference<Object>> MANAGED_OBJECTS = new HashMap<>();
    private static final Map<String, Pointer> TRAIT_METADATA_MARKERS = new HashMap<>();

    private static final class Cell {
        private Object value;

        private Cell(Object value) {
            this.value = value;
        }
    }

    private static final class ExposedTarget {
        private final WeakReference<Object> allocation;
        private final int allocationElementSize;
        private final long byteOffset;
        private final String codecClassName;
        private final int viewSize;
        private final String viewCodecClassName;
        private final long metadata;

        private ExposedTarget(
                Object allocation,
                int allocationElementSize,
                long byteOffset,
                String codecClassName,
                int viewSize,
                String viewCodecClassName,
                long metadata) {
            this.allocation = new WeakReference<>(allocation);
            this.allocationElementSize = allocationElementSize;
            this.byteOffset = byteOffset;
            this.codecClassName = codecClassName;
            this.viewSize = viewSize;
            this.viewCodecClassName = viewCodecClassName;
            this.metadata = metadata;
        }
    }

    private final Object allocation;
    private final int allocationElementSize;
    private final long byteOffset;
    private final int viewSize;
    private final String allocationCodecClassName;
    private final String viewCodecClassName;
    private final long exposedAddress;
    private long metadata = -1;
    private int zeroSizedSourceViewSize = -1;
    private String zeroSizedSourceViewCodecClassName;

    private Pointer(Object allocation, int allocationElementSize, int byteOffset, int viewSize) {
        this(allocation, allocationElementSize, byteOffset, viewSize, null, null, -1);
    }

    public Pointer(long exposedAddress, int viewSize) {
        this(null, viewSize, 0, viewSize, null, null, exposedAddress);
    }

    public Pointer(int exposedAddress, int viewSize) {
        this(Integer.toUnsignedLong(exposedAddress), viewSize);
    }

    public Pointer(Object value, int viewSize, String codecClassName) {
        this(new Cell(value), viewSize, 0, viewSize, codecClassName);
    }

    private Pointer(
            Object allocation,
            int allocationElementSize,
            long byteOffset,
            int viewSize,
            String codecClassName) {
        this(
                allocation,
                allocationElementSize,
                byteOffset,
                viewSize,
                codecClassName,
                codecClassName,
                -1);
    }

    private Pointer(
            Object allocation,
            int allocationElementSize,
            long byteOffset,
            int viewSize,
            String allocationCodecClassName,
            String viewCodecClassName,
            long exposedAddress) {
        if (allocationElementSize < 0 || viewSize < 0) {
            throw new IllegalArgumentException("Rust layout sizes cannot be negative");
        }
        this.allocation = allocation;
        this.allocationElementSize = allocationElementSize;
        this.byteOffset = byteOffset;
        this.viewSize = viewSize;
        this.allocationCodecClassName = allocationCodecClassName;
        this.viewCodecClassName = viewCodecClassName;
        this.exposedAddress = exposedAddress;
    }

    public static Pointer cell(Object value, int size, String codecClassName) {
        return cellAligned(value, size, codecClassName, 16);
    }

    public static Pointer cellAligned(
            Object value,
            int size,
            String codecClassName,
            int alignment) {
        if (alignment <= 0 || (alignment & (alignment - 1)) != 0) {
            throw new IllegalArgumentException("Rust allocation alignment must be a power of two");
        }
        Cell cell = new Cell(value);
        synchronized (ALLOCATION_BASES) {
            ALLOCATION_ALIGNMENTS.put(cell, alignment);
        }
        return new Pointer(cell, size, 0, size, codecClassName);
    }

    public static Pointer cell(Object value, int size) {
        return cell(value, size, null);
    }

    public static Pointer cell(Object value) {
        return cell(value, inferredCarrierSize(value));
    }

    public static Pointer array(
            Object array,
            int elementOffset,
            int elementSize,
            String codecClassName) {
        if (array == null || !array.getClass().isArray()) {
            throw new IllegalArgumentException("Rust array pointer requires JVM array storage");
        }
        return new Pointer(
                array,
                elementSize,
                Math.multiplyExact(elementOffset, elementSize),
                elementSize,
                codecClassName);
    }

    public static Pointer array(Object array, int elementOffset, int elementSize) {
        return array(array, elementOffset, elementSize, null);
    }

    public static Pointer array(Object array, int elementOffset) {
        return array(array, elementOffset, inferredArrayElementSize(array));
    }

    public static Pointer fromSlice(
            Object sliceView,
            int elementSize,
            String codecClassName) {
        if (sliceView == null) {
            return nullPointer(elementSize);
        }
        try {
            Field arrayField = sliceView.getClass().getField("array");
            Field offsetField = sliceView.getClass().getField("offset");
            Field lengthField = sliceView.getClass().getField("length");
            Object backing = arrayField.get(sliceView);
            int offset = offsetField.getInt(sliceView);
            int length = lengthField.getInt(sliceView);
            if (backing instanceof Pointer) {
                return ((Pointer) backing)
                        .sliceStorageView()
                        .add(offset)
                        .retype(elementSize, codecClassName)
                        .withMetadata(length);
            }
            return array(
                    backing,
                    offset,
                    elementSize,
                    codecClassName).withMetadata(length);
        } catch (ReflectiveOperationException error) {
            throw new IllegalArgumentException("invalid Rust slice view", error);
        }
    }

    public static Pointer fromSlice(Object sliceView, int elementSize) {
        return fromSlice(sliceView, elementSize, null);
    }

    public static Pointer fromSlice(Object sliceView) {
        if (sliceView == null) {
            return nullPointer();
        }
        try {
            Field arrayField = sliceView.getClass().getField("array");
            Object array = arrayField.get(sliceView);
            return fromSlice(sliceView, inferredArrayElementSize(array));
        } catch (ReflectiveOperationException error) {
            throw new IllegalArgumentException("invalid Rust slice view", error);
        }
    }

    public static Pointer nullPointer(int viewSize) {
        return new Pointer(null, viewSize, 0, viewSize, null, null, 0);
    }

    public static Pointer nullPointer() {
        return nullPointer(1);
    }

    public static Pointer traitMetadataMarker(Pointer pointer, String metadataClassName) {
        Object value = pointer == null ? null : pointer.getObject();
        String concreteClassName = value == null ? "<null>" : value.getClass().getName();
        String key = metadataClassName + '\0' + concreteClassName;
        synchronized (TRAIT_METADATA_MARKERS) {
            return TRAIT_METADATA_MARKERS.computeIfAbsent(
                    key,
                    ignored -> cell(null, 0, null));
        }
    }

    public static Pointer fromAddress(long address, int viewSize) {
        return fromAddress(address, viewSize, null);
    }

    public static Pointer fromAddress(int address, int viewSize) {
        return fromAddress(Integer.toUnsignedLong(address), viewSize);
    }

    public static Pointer fromAddress(long address, int viewSize, String viewCodecClassName) {
        synchronized (ALLOCATION_BASES) {
            ExposedTarget target = EXPOSED_ADDRESSES.get(address);
            if (target != null) {
                Object allocation = target.allocation.get();
                if (allocation != null) {
                    return new Pointer(
                            allocation,
                            target.allocationElementSize,
                            target.byteOffset,
                            viewSize,
                            target.codecClassName,
                            viewCodecClassName,
                            -1).withMetadata(target.metadata);
                }
                EXPOSED_ADDRESSES.remove(address);
            }
            for (Map.Entry<Object, Long> entry : ALLOCATION_BASES.entrySet()) {
                Object allocation = entry.getKey();
                long base = entry.getValue();
                int elementSize = ALLOCATION_ELEMENT_SIZES.getOrDefault(allocation, 1);
                int capacity = allocation.getClass().isArray()
                        ? Math.multiplyExact(Array.getLength(allocation), elementSize)
                        : elementSize;
                long delta = address - base;
                if (delta >= 0 && delta <= capacity) {
                    return new Pointer(
                            allocation,
                            elementSize,
                            delta,
                            viewSize,
                            ALLOCATION_CODECS.get(allocation),
                            viewCodecClassName,
                            -1);
                }
            }
        }
        return new Pointer(
                null, viewSize, 0, viewSize, null, viewCodecClassName, address);
    }

    public static Pointer fromAddress(int address, int viewSize, String viewCodecClassName) {
        return fromAddress(Integer.toUnsignedLong(address), viewSize, viewCodecClassName);
    }

    public static long managedObjectAddress(Object value) {
        if (value == null) {
            return 0;
        }
        synchronized (MANAGED_OBJECT_ADDRESSES) {
            Long existing = MANAGED_OBJECT_ADDRESSES.get(value);
            if (existing != null) {
                return existing.longValue();
            }
            long address = allocateAddress(16, 16);
            MANAGED_OBJECT_ADDRESSES.put(value, address);
            MANAGED_OBJECTS.put(address, new WeakReference<>(value));
            return address;
        }
    }

    public static Object managedObjectFromAddress(long address) {
        if (address == 0) {
            return null;
        }
        synchronized (MANAGED_OBJECT_ADDRESSES) {
            WeakReference<Object> reference = MANAGED_OBJECTS.get(address);
            Object value = reference == null ? null : reference.get();
            if (value == null) {
                MANAGED_OBJECTS.remove(address);
                throw new IllegalStateException(
                        "managed Rust reference address is no longer live: "
                                + Long.toUnsignedString(address));
            }
            return value;
        }
    }

    private static Pointer pointerObjectFromAddress(long address) {
        if (address == 0) {
            return nullPointer();
        }
        synchronized (ALLOCATION_BASES) {
            ExposedTarget target = EXPOSED_ADDRESSES.get(address);
            if (target != null) {
                Object allocation = target.allocation.get();
                if (allocation != null) {
                    return new Pointer(
                            allocation,
                            target.allocationElementSize,
                            target.byteOffset,
                            target.viewSize,
                            target.codecClassName,
                            target.viewCodecClassName,
                            -1).withMetadata(target.metadata);
                }
                EXPOSED_ADDRESSES.remove(address);
            }
        }
        return fromAddress(address, 1);
    }

    public static Pointer fromAddress(long address) {
        return fromAddress(address, 1);
    }

    public static Pointer withoutProvenance(long address, int viewSize) {
        return withoutProvenance(address, viewSize, null);
    }

    public static Pointer withoutProvenance(int address, int viewSize) {
        return withoutProvenance(Integer.toUnsignedLong(address), viewSize);
    }

    public static Pointer withoutProvenance(
            long address, int viewSize, String viewCodecClassName) {
        return new Pointer(
                null, viewSize, 0, viewSize, null, viewCodecClassName, address);
    }

    public static Pointer withoutProvenance(
            int address, int viewSize, String viewCodecClassName) {
        return withoutProvenance(Integer.toUnsignedLong(address), viewSize, viewCodecClassName);
    }

    public Pointer retype(int newViewSize) {
        return retype(newViewSize, null);
    }

    public Pointer retype(int newViewSize, String newViewCodecClassName) {
        Pointer result = new Pointer(
                allocation,
                allocationElementSize,
                byteOffset,
                newViewSize,
                allocationCodecClassName,
                newViewCodecClassName,
                exposedAddress).withMetadata(metadata);
        if (newViewSize == 0 && newViewCodecClassName == null) {
            if (viewSize == 0 && zeroSizedSourceViewSize >= 0) {
                result.zeroSizedSourceViewSize = zeroSizedSourceViewSize;
                result.zeroSizedSourceViewCodecClassName =
                        zeroSizedSourceViewCodecClassName;
            } else {
                result.zeroSizedSourceViewSize = viewSize;
                result.zeroSizedSourceViewCodecClassName = viewCodecClassName;
            }
        }
        return result;
    }

    public static Pointer retype(Pointer pointer, int newViewSize) {
        return pointer.retype(newViewSize);
    }

    public static Pointer retype(
            Pointer pointer, int newViewSize, String newViewCodecClassName) {
        return pointer.retype(newViewSize, newViewCodecClassName);
    }

    public static Pointer restoreAllocationView(Pointer pointer) {
        return new Pointer(
                pointer.allocation,
                pointer.allocationElementSize,
                pointer.byteOffset,
                pointer.allocationElementSize,
                pointer.allocationCodecClassName,
                pointer.allocationCodecClassName,
                pointer.exposedAddress).withMetadata(pointer.metadata);
    }

    public static Pointer restoreErasedView(Pointer pointer) {
        if (pointer.zeroSizedSourceViewSize >= 0) {
            return pointer.retype(
                    pointer.zeroSizedSourceViewSize,
                    pointer.zeroSizedSourceViewCodecClassName);
        }
        return restoreAllocationView(pointer);
    }

    private Pointer withMetadata(long metadata) {
        this.metadata = metadata;
        return this;
    }

    public static Pointer withMetadata(Pointer pointer, long metadata) {
        return pointer.withMetadata(metadata);
    }

    public static Pointer withMetadata(Pointer pointer, int metadata) {
        return pointer.withMetadata(Integer.toUnsignedLong(metadata));
    }

    public long metadata() {
        if (metadata < 0) {
            throw new IllegalStateException("pointer does not carry dynamically sized metadata");
        }
        return metadata;
    }

    public Pointer offset(long elementCount) {
        long delta = Math.multiplyExact(elementCount, (long) viewSize);
        if (allocation == null) {
            return new Pointer(
                    null,
                    allocationElementSize,
                    Math.addExact(byteOffset, delta),
                    viewSize,
                    allocationCodecClassName,
                    viewCodecClassName,
                    Math.addExact(exposedAddress, delta)).withMetadata(metadata);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                Math.addExact(byteOffset, delta),
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata);
    }

    public Pointer offset(int elementCount) { return offset((long) elementCount); }

    public Pointer add(long elementCount) {
        return offset(elementCount);
    }

    public Pointer add(int elementCount) { return add((long) elementCount); }

    public static Pointer add(Pointer pointer, long elementCount) {
        return pointer.add(elementCount);
    }

    public static Pointer add(Pointer pointer, int elementCount) { return pointer.add(elementCount); }

    public Pointer sub(long elementCount) {
        return offset(Math.negateExact(elementCount));
    }

    public Pointer sub(int elementCount) { return sub((long) elementCount); }

    public static Pointer sub(Pointer pointer, long elementCount) {
        return pointer.sub(elementCount);
    }

    public static Pointer sub(Pointer pointer, int elementCount) { return pointer.sub(elementCount); }

    public static Pointer offset(Pointer pointer, long elementCount) {
        return pointer.offset(elementCount);
    }

    public static Pointer offset(Pointer pointer, int elementCount) { return pointer.offset(elementCount); }

    public Pointer byte_offset(long byteCount) {
        if (allocation == null) {
            return new Pointer(
                    null,
                    allocationElementSize,
                    Math.addExact(byteOffset, byteCount),
                    viewSize,
                    allocationCodecClassName,
                    viewCodecClassName,
                    Math.addExact(exposedAddress, byteCount)).withMetadata(metadata);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                Math.addExact(byteOffset, byteCount),
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata);
    }

    public Pointer byte_offset(int byteCount) { return byte_offset((long) byteCount); }

    public static Pointer byte_offset(Pointer pointer, long byteCount) {
        return pointer.byte_offset(byteCount);
    }

    public static Pointer byte_offset(Pointer pointer, int byteCount) { return pointer.byte_offset(byteCount); }

    public static Pointer byte_add(Pointer pointer, long byteCount) {
        return pointer.byte_offset(byteCount);
    }

    public static Pointer byte_add(Pointer pointer, int byteCount) { return pointer.byte_offset(byteCount); }

    public static Pointer byte_sub(Pointer pointer, long byteCount) {
        return pointer.byte_offset(Math.negateExact(byteCount));
    }

    public static Pointer byte_sub(Pointer pointer, int byteCount) { return pointer.byte_offset(-(long) byteCount); }

    public static Pointer wrapping_byte_offset(Pointer pointer, long byteCount) {
        return pointer.wrappingByteOffset(byteCount);
    }

    public static Pointer wrapping_byte_offset(Pointer pointer, int byteCount) { return pointer.wrappingByteOffset(byteCount); }

    public static Pointer wrapping_byte_add(Pointer pointer, long byteCount) {
        return pointer.wrappingByteOffset(byteCount);
    }

    public static Pointer wrapping_byte_add(Pointer pointer, int byteCount) { return pointer.wrappingByteOffset(byteCount); }

    public static Pointer wrapping_byte_sub(Pointer pointer, long byteCount) {
        return pointer.wrappingByteOffset(-byteCount);
    }

    public static Pointer wrapping_byte_sub(Pointer pointer, int byteCount) { return pointer.wrappingByteOffset(-(long) byteCount); }

    private Pointer wrappingByteOffset(long byteCount) {
        if (allocation == null) {
            return new Pointer(
                    null,
                    allocationElementSize,
                    byteOffset + byteCount,
                    viewSize,
                    allocationCodecClassName,
                    viewCodecClassName,
                    exposedAddress + byteCount).withMetadata(metadata);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                byteOffset + byteCount,
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata);
    }

    public long align_offset(long alignment) {
        if (alignment <= 0 || (alignment & (alignment - 1)) != 0) {
            throw new IllegalArgumentException("Rust pointer alignment must be a power of two");
        }
        long current = address();
        long attempts = viewSize == 0 ? 1 : alignment;
        for (long elements = 0; elements < attempts; elements++) {
            if ((current + (long) elements * viewSize) % alignment == 0) {
                return elements;
            }
        }
        return -1;
    }

    public static long align_offset(Pointer pointer, long alignment) {
        return pointer.align_offset(alignment);
    }

    public int align_offset(int alignment) { return Math.toIntExact(align_offset((long) alignment)); }

    public static int align_offset(Pointer pointer, int alignment) {
        return pointer.align_offset(alignment);
    }

    public long addr() {
        return address();
    }

    public static long addr(Pointer pointer) {
        return pointer.addr();
    }

    public long expose_provenance() {
        return address();
    }

    public static long expose_provenance(Pointer pointer) {
        return pointer.expose_provenance();
    }

    public Pointer with_addr(long address) {
        if (allocation == null) {
            return new Pointer(
                    null,
                    allocationElementSize,
                    0,
                    viewSize,
                    allocationCodecClassName,
                    viewCodecClassName,
                    address).withMetadata(metadata);
        }
        long base = address() - byteOffset;
        return new Pointer(
                allocation,
                allocationElementSize,
                address - base,
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata);
    }

    public Pointer with_addr(int address) { return with_addr(Integer.toUnsignedLong(address)); }

    public static Pointer with_addr(Pointer pointer, long address) {
        return pointer.with_addr(address);
    }

    public static Pointer with_addr(Pointer pointer, int address) {
        return pointer.with_addr(Integer.toUnsignedLong(address));
    }

    public static Pointer map_addr(Pointer pointer, Object mapper) {
        try {
            Method call = null;
            for (Method method : mapper.getClass().getMethods()) {
                if (method.getName().equals("call") && method.getParameterTypes().length == 1) {
                    call = method;
                    break;
                }
            }
            if (call == null) {
                throw new NoSuchMethodException("Rust map_addr closure has no call method");
            }
            Object mapped = call.invoke(mapper, Long.valueOf(pointer.address()));
            return pointer.with_addr(((Number) mapped).longValue());
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not invoke Rust map_addr closure", error);
        }
    }

    public static Pointer wrapping_add(Pointer pointer, long elementCount) {
        return pointer.wrappingOffset(elementCount);
    }

    public static Pointer wrapping_add(Pointer pointer, int elementCount) { return pointer.wrappingOffset(elementCount); }

    public static Pointer wrapping_sub(Pointer pointer, long elementCount) {
        return pointer.wrappingOffset(-elementCount);
    }

    public static Pointer wrapping_sub(Pointer pointer, int elementCount) { return pointer.wrappingOffset(-(long) elementCount); }

    public static Pointer wrapping_offset(Pointer pointer, long elementCount) {
        return pointer.wrappingOffset(elementCount);
    }

    public static Pointer wrapping_offset(Pointer pointer, int elementCount) { return pointer.wrappingOffset(elementCount); }

    private Pointer wrappingOffset(long elementCount) {
        long delta = elementCount * viewSize;
        if (allocation == null) {
            return new Pointer(
                    null,
                    allocationElementSize,
                    byteOffset + delta,
                    viewSize,
                    allocationCodecClassName,
                    viewCodecClassName,
                    exposedAddress + delta).withMetadata(metadata);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                byteOffset + delta,
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata);
    }

    public long offsetFrom(Pointer origin) {
        if (origin == null || allocation != origin.allocation) {
            throw new IllegalArgumentException("offset_from requires pointers into one allocation");
        }
        if (viewSize == 0) {
            throw new ArithmeticException("offset_from is undefined for zero-sized pointees");
        }
        long bytes = Math.subtractExact(byteOffset, origin.byteOffset);
        if (bytes % viewSize != 0) {
            throw new ArithmeticException("pointer distance is not a whole number of elements");
        }
        return bytes / viewSize;
    }

    public long offset_from(Pointer origin) {
        return offsetFrom(origin);
    }

    public long offset_from_unsigned(Pointer origin) {
        long distance = offsetFrom(origin);
        if (distance < 0) {
            throw new ArithmeticException("offset_from_unsigned requires self at or after origin");
        }
        return distance;
    }

    public static long offset_from_unsigned(Pointer pointer, Pointer origin) {
        return pointer.offset_from_unsigned(origin);
    }

    public long byte_offset_from(Pointer origin) {
        if (origin == null || allocation != origin.allocation) {
            throw new IllegalArgumentException(
                    "byte_offset_from requires pointers into one allocation");
        }
        return Math.subtractExact(byteOffset, origin.byteOffset);
    }

    public static long byte_offset_from(Pointer pointer, Pointer origin) {
        return pointer.byte_offset_from(origin);
    }

    public long byte_offset_from_unsigned(Pointer origin) {
        long distance = byte_offset_from(origin);
        if (distance < 0) {
            throw new ArithmeticException(
                    "byte_offset_from_unsigned requires self at or after origin");
        }
        return distance;
    }

    public static long byte_offset_from_unsigned(Pointer pointer, Pointer origin) {
        return pointer.byte_offset_from_unsigned(origin);
    }

    public static long offset_from(Pointer pointer, Pointer origin) {
        return pointer.offsetFrom(origin);
    }

    public static long offsetFrom(Pointer pointer, Pointer origin) {
        return pointer.offsetFrom(origin);
    }

    public boolean sameAddress(Pointer other) {
        return other != null && address() == other.address();
    }

    public static boolean arraySameAddresses(Object left, Object right) {
        if (left == right) {
            return true;
        }
        if (left == null || right == null
                || !left.getClass().isArray() || !right.getClass().isArray()) {
            return false;
        }
        int length = Array.getLength(left);
        if (length != Array.getLength(right)) {
            return false;
        }
        for (int index = 0; index < length; index++) {
            Object leftValue = Array.get(left, index);
            Object rightValue = Array.get(right, index);
            if (leftValue == rightValue) {
                continue;
            }
            if (!(leftValue instanceof Pointer) || !(rightValue instanceof Pointer)
                    || !((Pointer) leftValue).sameAddress((Pointer) rightValue)) {
                return false;
            }
        }
        return true;
    }

    public int compareAddress(Pointer other) {
        return Long.compareUnsigned(address(), other.address());
    }

    public boolean lessThan(Pointer other) {
        return compareAddress(other) < 0;
    }

    public boolean lessOrEqual(Pointer other) {
        return compareAddress(other) <= 0;
    }

    public boolean greaterThan(Pointer other) {
        return compareAddress(other) > 0;
    }

    public boolean greaterOrEqual(Pointer other) {
        return compareAddress(other) >= 0;
    }

    public static boolean is_null(Pointer pointer) {
        return pointer == null || pointer.address() == 0;
    }

    public static boolean is_aligned_to(Pointer pointer, long alignment) {
        if (alignment <= 0 || (alignment & (alignment - 1)) != 0) {
            throw new IllegalArgumentException("Rust pointer alignment must be a power of two");
        }
        return (pointer.address() & (alignment - 1)) == 0;
    }

    public static boolean is_aligned_to(Pointer pointer, int alignment) {
        return is_aligned_to(pointer, Integer.toUnsignedLong(alignment));
    }

    public static Object asRefOption(Pointer pointer, String optionClassName) {
        String variantName = optionClassName + (is_null(pointer) ? "$None" : "$Some");
        try {
            Class<?> variant = Class.forName(variantName.replace('/', '.'));
            if (is_null(pointer)) {
                return variant.getConstructor().newInstance();
            }
            for (java.lang.reflect.Constructor<?> constructor : variant.getConstructors()) {
                if (constructor.getParameterTypes().length == 1) {
                    return constructor.newInstance(pointer);
                }
            }
            throw new NoSuchMethodException("Rust Option::Some variant has no value constructor");
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException(
                    "could not construct Rust pointer option " + optionClassName, error);
        }
    }

    public long address() {
        if (allocation == null) {
            return exposedAddress;
        }
        synchronized (ALLOCATION_BASES) {
            Long base = ALLOCATION_BASES.get(allocation);
            if (base == null) {
                int byteCapacity = allocation.getClass().isArray()
                        ? Math.multiplyExact(Array.getLength(allocation), allocationElementSize)
                        : allocationElementSize;
                long requiredSpan = Math.addExact((long) byteCapacity, 1L);
                long span = Math.max(16L, Math.addExact(requiredSpan, 15L) & ~15L);
                int alignment = ALLOCATION_ALIGNMENTS.getOrDefault(allocation, 16);
                base = allocateAddress(span, alignment);
                ALLOCATION_BASES.put(allocation, base);
                ALLOCATION_ELEMENT_SIZES.put(allocation, allocationElementSize);
                ALLOCATION_CODECS.put(allocation, allocationCodecClassName);
            }
            long address = base + byteOffset;
            EXPOSED_ADDRESSES.put(
                    address,
                    new ExposedTarget(
                            allocation,
                            allocationElementSize,
                            byteOffset,
                            allocationCodecClassName,
                            viewSize,
                            viewCodecClassName,
                            metadata));
            return address;
        }
    }

    private static long allocateAddress(long span, int alignment) {
        while (true) {
            long current = NEXT_ADDRESS.get();
            long aligned = Math.addExact(current, alignment - 1L) & -((long) alignment);
            long next = Math.addExact(aligned, span);
            if (NEXT_ADDRESS.compareAndSet(current, next)) {
                return aligned;
            }
        }
    }

    private Object readAlignedElement() {
        if (allocation == null) {
            throw new NullPointerException("attempted to dereference a null Rust pointer");
        }
        if (allocationElementSize == 0) {
            if (allocation instanceof Cell) {
                return ((Cell) allocation).value;
            }
            return allocation.getClass().isArray() && Array.getLength(allocation) != 0
                    ? Array.get(allocation, 0)
                    : null;
        }
        if (byteOffset % allocationElementSize != 0) {
            throw new IllegalStateException("object dereference is not aligned to its allocation element");
        }
        int elementIndex = Math.toIntExact(byteOffset / allocationElementSize);
        if (allocation instanceof Cell) {
            if (elementIndex != 0) {
                throw new IndexOutOfBoundsException("pointer arithmetic escaped scalar storage");
            }
            return ((Cell) allocation).value;
        }
        return Array.get(allocation, elementIndex);
    }

    private long loadUnsigned(int byteCount) {
        if (byteCount < 0 || byteCount > 8) {
            throw new IllegalArgumentException("scalar loads support at most eight bytes");
        }
        long result = 0;
        for (int index = 0; index < byteCount; index++) {
            result |= ((long) loadByte(byteOffset + index)) << (index * 8);
        }
        return result;
    }

    private int loadByte(long absoluteByteOffset) {
        if (allocation == null) {
            throw new NullPointerException("attempted to dereference a null Rust pointer");
        }
        if (allocationElementSize == 0) {
            throw new IndexOutOfBoundsException("zero-sized storage has no addressable bytes");
        }
        int elementIndex = Math.toIntExact(Math.floorDiv(absoluteByteOffset, allocationElementSize));
        int withinElement = (int) Math.floorMod(absoluteByteOffset, allocationElementSize);
        Object value;
        if (allocation instanceof Cell) {
            if (elementIndex != 0) {
                throw new IndexOutOfBoundsException("pointer arithmetic escaped scalar storage");
            }
            value = ((Cell) allocation).value;
        } else {
            value = Array.get(allocation, elementIndex);
        }
        if (value instanceof BigInteger) {
            return bigIntegerByte((BigInteger) value, withinElement) & 0xff;
        }
        if (value instanceof I128) {
            return ((I128) value).byteAt(withinElement) & 0xff;
        }
        if (value instanceof U128) {
            return ((U128) value).byteAt(withinElement) & 0xff;
        }
        if (value instanceof F128) {
            return bigIntegerByte(((F128) value).toBits(), withinElement) & 0xff;
        }
        if (allocationCodecClassName != null
                && !RAW_POINTER_VIEW_CODEC.equals(allocationCodecClassName)) {
            byte[] bytes = encodeAggregate(allocationCodecClassName, value);
            if (withinElement >= bytes.length) {
                throw new IndexOutOfBoundsException("aggregate codec returned a short memory image");
            }
            return bytes[withinElement] & 0xff;
        }
        long bits = valueBits(value, allocationElementSize);
        return (int) ((bits >>> (withinElement * 8)) & 0xffL);
    }

    private void storeBytes(long bits, int byteCount) {
        if (byteCount < 0 || byteCount > 8) {
            throw new IllegalArgumentException("scalar stores support at most eight bytes");
        }
        for (int index = 0; index < byteCount; index++) {
            storeByte(byteOffset + index, (int) ((bits >>> (index * 8)) & 0xffL));
        }
    }

    private void storeByte(long absoluteByteOffset, int value) {
        if (allocation == null) {
            throw new NullPointerException("attempted to write through a null Rust pointer");
        }
        if (allocationElementSize == 0) {
            throw new IndexOutOfBoundsException("zero-sized storage has no addressable bytes");
        }
        int elementIndex = Math.toIntExact(Math.floorDiv(absoluteByteOffset, allocationElementSize));
        int withinElement = (int) Math.floorMod(absoluteByteOffset, allocationElementSize);
        Object current;
        if (allocation instanceof Cell) {
            if (elementIndex != 0) {
                throw new IndexOutOfBoundsException("pointer arithmetic escaped scalar storage");
            }
            current = ((Cell) allocation).value;
        } else {
            current = Array.get(allocation, elementIndex);
        }
        if (current instanceof BigInteger) {
            BigInteger updated = replaceBigIntegerByte(
                    (BigInteger) current,
                    withinElement,
                    value,
                    allocationElementSize,
                    SIGNED_BIG_INTEGER_CODEC.equals(allocationCodecClassName));
            writeElement(elementIndex, updated);
            return;
        }
        if (current instanceof I128) {
            writeElement(elementIndex, ((I128) current).withByte(withinElement, value));
            return;
        }
        if (current instanceof U128) {
            writeElement(elementIndex, ((U128) current).withByte(withinElement, value));
            return;
        }
        if (current instanceof F128) {
            BigInteger updated = replaceBigIntegerByte(
                    ((F128) current).toBits(),
                    withinElement,
                    value,
                    allocationElementSize,
                    false);
            writeElement(elementIndex, F128.fromBits(updated));
            return;
        }
        if (allocationCodecClassName != null
                && !RAW_POINTER_VIEW_CODEC.equals(allocationCodecClassName)) {
            byte[] bytes = encodeAggregate(allocationCodecClassName, current);
            if (withinElement >= bytes.length) {
                throw new IndexOutOfBoundsException("aggregate codec returned a short memory image");
            }
            bytes[withinElement] = (byte) value;
            writeElement(elementIndex, decodeAggregate(allocationCodecClassName, bytes));
            return;
        }
        long bits = valueBits(current, allocationElementSize);
        long mask = 0xffL << (withinElement * 8);
        bits = (bits & ~mask) | (((long) value & 0xffL) << (withinElement * 8));
        writeElement(elementIndex, carrierFromBits(current, bits, allocationElementSize));
    }

    public boolean getBoolean() {
        return loadUnsigned(Math.max(1, viewSize)) != 0;
    }

    public byte getI8() {
        return (byte) loadUnsigned(1);
    }

    public short getI16() {
        long bits = loadUnsigned(Math.max(1, viewSize));
        return viewSize == 1 ? (short) (bits & 0xffL) : (short) bits;
    }

    public int getI32() {
        long bits = loadUnsigned(Math.max(1, viewSize));
        return viewSize < 4 ? (int) bits : (int) (bits & 0xffff_ffffL);
    }

    public long getI64() {
        long bits = loadUnsigned(Math.max(1, viewSize));
        return viewSize < 8 ? bits : (long) bits;
    }

    public float getF32() {
        return viewSize == 2
                ? halfToFloat((int) loadUnsigned(2))
                : Float.intBitsToFloat((int) loadUnsigned(4));
    }

    public double getF64() {
        return Double.longBitsToDouble(loadUnsigned(8));
    }

    public Object getObject() {
        if (viewSize == 0 && zeroSizedSourceViewSize >= 0) {
            return new Pointer(
                            allocation,
                            allocationElementSize,
                            byteOffset,
                            zeroSizedSourceViewSize,
                            allocationCodecClassName,
                            zeroSizedSourceViewCodecClassName,
                            exposedAddress).withMetadata(metadata)
                    .getObject();
        }
        if (MANAGED_OBJECT_VIEW_CODEC.equals(viewCodecClassName)
                && !isDirectAllocationView()) {
            return managedObjectFromAddress(loadUnsigned(Math.min(viewSize, 8)));
        }
        if (RAW_POINTER_VIEW_CODEC.equals(viewCodecClassName)
                && !isDirectAllocationView()) {
            return pointerObjectFromAddress(loadUnsigned(Math.min(viewSize, 8)));
        }
        if (isBigIntegerCodec(viewCodecClassName) && !isDirectAllocationView()) {
            BigInteger bits = bigIntegerFromPointerBytes(viewSize, false);
            return SIGNED_BIG_INTEGER_CODEC.equals(viewCodecClassName)
                    ? I128.fromBigInteger(bits)
                    : U128.fromBigInteger(bits);
        }
        if (F128_CODEC.equals(viewCodecClassName) && !isDirectAllocationView()) {
            return F128.fromBits(bigIntegerFromPointerBytes(viewSize, false));
        }
        if (viewCodecClassName != null && !isDirectAllocationView()) {
            byte[] image = new byte[viewSize];
            for (int index = 0; index < viewSize; index++) {
                image[index] = (byte) loadByte(byteOffset + index);
            }
            return decodeAggregate(viewCodecClassName, image);
        }
        return readAlignedElement();
    }

    private boolean isDirectAllocationView() {
        boolean sameCodec = allocationCodecClassName == null
                ? viewCodecClassName == null
                : allocationCodecClassName.equals(viewCodecClassName);
        return allocationElementSize != 0
                && byteOffset % allocationElementSize == 0
                && viewSize == allocationElementSize
                && sameCodec;
    }

    public Object backingArray() {
        if (allocation == null) {
            return null;
        }
        if (allocation.getClass().isArray()) {
            if (allocationElementSize == 0 || byteOffset == 0) {
                return allocation;
            }
            if (byteOffset % allocationElementSize != 0) {
                throw new IllegalStateException("unaligned pointer cannot be exposed as a JVM array");
            }
            int elementOffset = Math.toIntExact(byteOffset / allocationElementSize);
            int remaining = Array.getLength(allocation) - elementOffset;
            Object tail = Array.newInstance(allocation.getClass().getComponentType(), remaining);
            System.arraycopy(allocation, elementOffset, tail, 0, remaining);
            return tail;
        }
        return readAlignedElement();
    }

    public Object sliceBackingArray() {
        if (allocation == null || !allocation.getClass().isArray()) {
            throw new IllegalStateException("slice data pointer is not backed by a JVM array");
        }
        return allocation;
    }

    public int sliceElementOffset() {
        if (allocationElementSize == 0) {
            return 0;
        }
        if (byteOffset % allocationElementSize != 0) {
            throw new IllegalStateException("slice data pointer is not element-aligned");
        }
        return Math.toIntExact(byteOffset / allocationElementSize);
    }

    public void set(Object value) {
        if (allocation == null) {
            throw new NullPointerException("attempted to write through a null Rust pointer");
        }

        if (allocationElementSize == 0) {
            if (allocation instanceof Cell && byteOffset == 0) {
                ((Cell) allocation).value = value;
                return;
            }
            throw new IndexOutOfBoundsException("pointer arithmetic escaped zero-sized storage");
        }

        if (isDirectAllocationView()) {
            int elementIndex = Math.toIntExact(byteOffset / allocationElementSize);
            Object current = allocation instanceof Cell
                    ? ((Cell) allocation).value
                    : Array.get(allocation, elementIndex);
            writeElement(
                    elementIndex,
                    convertDirectValue(current, value, allocationElementSize));
            return;
        }

        if (viewCodecClassName != null) {
            if (MANAGED_OBJECT_VIEW_CODEC.equals(viewCodecClassName)) {
                long address = managedObjectAddress(value);
                byte[] image = new byte[viewSize];
                for (int index = 0; index < Math.min(viewSize, 8); index++) {
                    image[index] = (byte) (address >>> (index * 8));
                }
                storeRange(image);
                return;
            }
            if (RAW_POINTER_VIEW_CODEC.equals(viewCodecClassName)) {
                long address = value == null
                        ? 0L
                        : ((Pointer) value).address();
                byte[] image = new byte[viewSize];
                for (int index = 0; index < Math.min(viewSize, 8); index++) {
                    image[index] = (byte) (address >>> (index * 8));
                }
                storeRange(image);
                return;
            }
            if (isBigIntegerCodec(viewCodecClassName)) {
                BigInteger bits = value instanceof I128
                        ? ((I128) value).toBigInteger()
                        : ((U128) value).toBigInteger();
                storeBigIntegerBytes(bits, viewSize);
                return;
            }
            if (F128_CODEC.equals(viewCodecClassName)) {
                storeBigIntegerBytes(((F128) value).toBits(), viewSize);
                return;
            }
            storeRange(encodeAggregate(viewCodecClassName, value));
            return;
        }

        storeBytes(incomingBits(value, viewSize), viewSize);
    }

    public static void copy(Pointer source, Pointer destination, int byteCount) {
        byte[] temporary = new byte[byteCount];
        for (int index = 0; index < byteCount; index++) {
            temporary[index] = (byte) source.loadByte(source.byteOffset + index);
        }
        destination.storeRange(temporary);
    }

    public static void copy(Pointer source, Pointer destination, long byteCount) {
        copy(source, destination, checkedArrayLength(byteCount));
    }

    public static void copyElements(
            Pointer source, Pointer destination, long elementCount) {
        copy(source, destination, checkedElementByteCount(source, elementCount));
    }

    public static void copyNonOverlapping(Pointer source, Pointer destination, int byteCount) {
        if (source.allocation == destination.allocation) {
            long sourceEnd = source.byteOffset + byteCount;
            long destinationEnd = destination.byteOffset + byteCount;
            if (source.byteOffset < destinationEnd && destination.byteOffset < sourceEnd) {
                throw new IllegalArgumentException("copy_nonoverlapping regions overlap");
            }
        }
        copy(source, destination, byteCount);
    }

    public static void copyNonOverlapping(
            Pointer source, Pointer destination, long byteCount) {
        copyNonOverlapping(source, destination, checkedArrayLength(byteCount));
    }

    public static void copyNonOverlappingElements(
            Pointer source, Pointer destination, long elementCount) {
        copyNonOverlapping(
                source, destination, checkedElementByteCount(source, elementCount));
    }

    public static void swapNonOverlapping(Pointer left, Pointer right, int byteCount) {
        if (left.allocation == right.allocation) {
            long leftEnd = left.byteOffset + byteCount;
            long rightEnd = right.byteOffset + byteCount;
            if (left.byteOffset < rightEnd && right.byteOffset < leftEnd) {
                throw new IllegalArgumentException("swap_nonoverlapping regions overlap");
            }
        }
        byte[] leftBytes = new byte[byteCount];
        byte[] rightBytes = new byte[byteCount];
        for (int index = 0; index < byteCount; index++) {
            leftBytes[index] = (byte) left.loadByte(left.byteOffset + index);
            rightBytes[index] = (byte) right.loadByte(right.byteOffset + index);
        }
        left.storeRange(rightBytes);
        right.storeRange(leftBytes);
    }

    public static void swapNonOverlapping(Pointer left, Pointer right, long byteCount) {
        swapNonOverlapping(left, right, checkedArrayLength(byteCount));
    }

    public static void swapNonOverlappingElements(
            Pointer left, Pointer right, long elementCount) {
        swapNonOverlapping(left, right, checkedElementByteCount(left, elementCount));
    }

    public static void swapNonOverlappingNonZero(
            Pointer left, Pointer right, Object byteCount) {
        swapNonOverlapping(left, right, rustIntegerCarrierValue(byteCount));
    }

    private static int rustIntegerCarrierValue(Object value) {
        Object current = value;
        while (!(current instanceof Number)) {
            if (current == null) {
                throw new IllegalArgumentException("Rust integer carrier was null");
            }
            Field[] fields = current.getClass().getFields();
            if (fields.length != 1) {
                throw new IllegalArgumentException(
                        "Rust integer carrier does not have one transparent field: "
                                + current.getClass().getName());
            }
            try {
                current = fields[0].get(current);
            } catch (IllegalAccessException error) {
                throw new IllegalArgumentException("could not read Rust integer carrier", error);
            }
        }
        return ((Number) current).intValue();
    }

    public static void writeBytes(Pointer destination, int value, int byteCount) {
        byte[] bytes = new byte[byteCount];
        for (int index = 0; index < byteCount; index++) {
            bytes[index] = (byte) value;
        }
        destination.storeRange(bytes);
    }

    public static void writeBytes(Pointer destination, int value, long byteCount) {
        writeBytes(destination, value, checkedArrayLength(byteCount));
    }

    public static void writeElements(Pointer destination, int value, long elementCount) {
        writeBytes(destination, value, checkedElementByteCount(destination, elementCount));
    }

    private static int checkedElementByteCount(Pointer pointer, long elementCount) {
        return checkedArrayLength(Math.multiplyExact(elementCount, (long) pointer.viewSize));
    }

    private static int checkedArrayLength(long length) {
        if (length < 0 || length > Integer.MAX_VALUE) {
            throw new IllegalArgumentException("Rust memory operation exceeds JVM array limits");
        }
        return (int) length;
    }

    public static synchronized void volatileFence() {
        // TODO
    }

    private static Pointer slicePointer(Object backing, int index) {
        return backing instanceof Pointer
                ? ((Pointer) backing).sliceStorageView().add(index)
                : null;
    }

    private Pointer sliceStorageView() {
        if (viewSize != 0) {
            return this;
        }
        if (zeroSizedSourceViewSize >= 0) {
            return retype(zeroSizedSourceViewSize, zeroSizedSourceViewCodecClassName);
        }
        return restoreAllocationView(this);
    }

    public static boolean sliceGetBoolean(Object backing, int index) {
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getBoolean()
                : ((Boolean) Array.get(backing, index)).booleanValue();
    }

    public static byte sliceGetI8(Object backing, int index) {
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getI8()
                : ((Number) Array.get(backing, index)).byteValue();
    }

    public static short sliceGetI16(Object backing, int index) {
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getI16()
                : ((Number) Array.get(backing, index)).shortValue();
    }

    public static int sliceGetI32(Object backing, int index) {
        Pointer pointer = slicePointer(backing, index);
        Object value = pointer != null ? Integer.valueOf(pointer.getI32()) : Array.get(backing, index);
        return value instanceof Character
                ? ((Character) value).charValue()
                : ((Number) value).intValue();
    }

    public static long sliceGetI64(Object backing, int index) {
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getI64()
                : ((Number) Array.get(backing, index)).longValue();
    }

    public static float sliceGetF32(Object backing, int index) {
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getF32()
                : ((Number) Array.get(backing, index)).floatValue();
    }

    public static double sliceGetF64(Object backing, int index) {
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getF64()
                : ((Number) Array.get(backing, index)).doubleValue();
    }

    public static Object sliceGetObject(Object backing, int index) {
        Pointer pointer = slicePointer(backing, index);
        return pointer != null ? pointer.getObject() : Array.get(backing, index);
    }

    public static void sliceSetBoolean(Object backing, int index, boolean value) {
        sliceSetObject(backing, index, Boolean.valueOf(value));
    }

    public static void sliceSetI8(Object backing, int index, byte value) {
        sliceSetObject(backing, index, Byte.valueOf(value));
    }

    public static void sliceSetI16(Object backing, int index, short value) {
        sliceSetObject(backing, index, Short.valueOf(value));
    }

    public static void sliceSetI32(Object backing, int index, int value) {
        Pointer pointer = slicePointer(backing, index);
        if (pointer != null) {
            pointer.set(Integer.valueOf(value));
            return;
        }
        Class<?> component = backing.getClass().getComponentType();
        Array.set(backing, index, component == char.class
                ? Character.valueOf((char) value)
                : Integer.valueOf(value));
    }

    public static void sliceSetI64(Object backing, int index, long value) {
        sliceSetObject(backing, index, Long.valueOf(value));
    }

    public static void sliceSetF32(Object backing, int index, float value) {
        sliceSetObject(backing, index, Float.valueOf(value));
    }

    public static void sliceSetF64(Object backing, int index, double value) {
        sliceSetObject(backing, index, Double.valueOf(value));
    }

    public static void sliceSetObject(Object backing, int index, Object value) {
        Pointer pointer = slicePointer(backing, index);
        if (pointer != null) {
            pointer.set(value);
        } else {
            Array.set(backing, index, value);
        }
    }

    private void storeRange(byte[] source) {
        if (allocationCodecClassName == null || isBuiltInCodec(allocationCodecClassName)) {
            for (int index = 0; index < source.length; index++) {
                storeByte(byteOffset + index, source[index] & 0xff);
            }
            return;
        }
        int consumed = 0;
        while (consumed < source.length) {
            long absoluteOffset = byteOffset + consumed;
            int elementIndex = Math.toIntExact(Math.floorDiv(absoluteOffset, allocationElementSize));
            int withinElement = (int) Math.floorMod(absoluteOffset, allocationElementSize);
            Object current = allocation instanceof Cell
                    ? ((Cell) allocation).value
                    : Array.get(allocation, elementIndex);
            byte[] image = encodeAggregate(allocationCodecClassName, current);
            int chunk = Math.min(source.length - consumed, image.length - withinElement);
            System.arraycopy(source, consumed, image, withinElement, chunk);
            writeElement(
                    elementIndex,
                    decodeAggregate(allocationCodecClassName, image));
            consumed += chunk;
        }
    }

    private void writeElement(int elementIndex, Object value) {
        if (allocation instanceof Cell) {
            if (elementIndex != 0) {
                throw new IndexOutOfBoundsException("pointer arithmetic escaped scalar storage");
            }
            ((Cell) allocation).value = value;
            return;
        }
        Array.set(allocation, elementIndex, value);
    }

    private static Method[] codecMethods(String codecClassName) {
        if (codecClassName == null) {
            throw new IllegalStateException("pointer view has no aggregate codec");
        }
        synchronized (CODEC_METHODS) {
            Method[] cached = CODEC_METHODS.get(codecClassName);
            if (cached != null) {
                return cached;
            }
            try {
                Class<?> codec = Class.forName(codecClassName.replace('/', '.'));
                Method encode = null;
                Method decode = null;
                for (Method method : codec.getMethods()) {
                    if (method.getName().equals("encode") && method.getParameterTypes().length == 1) {
                        encode = method;
                    } else if (method.getName().equals("decode")
                            && method.getParameterTypes().length == 1) {
                        decode = method;
                    }
                }
                if (encode == null || decode == null) {
                    throw new NoSuchMethodException(
                            "pointer codec must define public static encode/decode methods");
                }
                Method[] methods = new Method[] {encode, decode};
                CODEC_METHODS.put(codecClassName, methods);
                return methods;
            } catch (ReflectiveOperationException error) {
                throw new IllegalStateException("could not load Rust pointer codec " + codecClassName, error);
            }
        }
    }

    private static byte[] encodeAggregate(String codecClassName, Object value) {
        try {
            return (byte[]) codecMethods(codecClassName)[0].invoke(null, value);
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not encode Rust aggregate memory", error);
        }
    }

    private static Object decodeAggregate(String codecClassName, byte[] bytes) {
        try {
            return codecMethods(codecClassName)[1].invoke(null, (Object) bytes);
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not decode Rust aggregate memory", error);
        }
    }

    private static long incomingBits(Object value, int size) {
        if (value instanceof Float) {
            if (size == 2) {
                return floatToHalf(((Float) value).floatValue()) & 0xffffL;
            }
            return ((long) Float.floatToRawIntBits(((Float) value).floatValue())) & 0xffff_ffffL;
        }
        if (value instanceof Double) {
            return Double.doubleToRawLongBits(((Double) value).doubleValue());
        }
        if (value instanceof Boolean) {
            return ((Boolean) value).booleanValue() ? 1L : 0L;
        }
        if (value instanceof Character) {
            return ((Character) value).charValue();
        }
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        if (value instanceof Pointer && size <= 8) {
            return ((Pointer) value).address();
        }
        throw new UnsupportedOperationException(
                "value is not scalar byte-addressable: " + value.getClass().getName());
    }

    public static byte bigIntegerByte(BigInteger value, int byteIndex) {
        return value.shiftRight(byteIndex * 8).byteValue();
    }

    public static byte integer128Byte(I128 value, int byteIndex) {
        return value.byteAt(byteIndex);
    }

    public static byte integer128Byte(U128 value, int byteIndex) {
        return value.byteAt(byteIndex);
    }

    public static BigInteger bigIntegerFromBytes(
            byte[] bytes, int offset, int size, boolean signed) {
        BigInteger value = BigInteger.ZERO;
        for (int index = size - 1; index >= 0; index--) {
            value = value.shiftLeft(8).or(BigInteger.valueOf(bytes[offset + index] & 0xffL));
        }
        if (signed && size > 0 && (bytes[offset + size - 1] & 0x80) != 0) {
            value = value.subtract(BigInteger.ONE.shiftLeft(size * 8));
        }
        return value;
    }

    public static I128 i128FromBytes(byte[] bytes, int offset, int size, boolean signed) {
        return I128.fromBigInteger(bigIntegerFromBytes(bytes, offset, size, false));
    }

    public static U128 u128FromBytes(byte[] bytes, int offset, int size, boolean signed) {
        return U128.fromBigInteger(bigIntegerFromBytes(bytes, offset, size, false));
    }

    private BigInteger bigIntegerFromPointerBytes(int size, boolean signed) {
        byte[] bytes = new byte[size];
        for (int index = 0; index < size; index++) {
            bytes[index] = (byte) loadByte(byteOffset + index);
        }
        return bigIntegerFromBytes(bytes, 0, size, signed);
    }

    private void storeBigIntegerBytes(BigInteger value, int size) {
        byte[] image = new byte[size];
        for (int index = 0; index < size; index++) {
            image[index] = bigIntegerByte(value, index);
        }
        storeRange(image);
    }

    private static BigInteger replaceBigIntegerByte(
            BigInteger current, int byteIndex, int byteValue, int size, boolean signed) {
        int width = size * 8;
        BigInteger modulus = BigInteger.ONE.shiftLeft(width);
        BigInteger widthMask = modulus.subtract(BigInteger.ONE);
        BigInteger byteMask = BigInteger.valueOf(0xffL).shiftLeft(byteIndex * 8);
        BigInteger bits = current.and(widthMask)
                .and(byteMask.not().and(widthMask))
                .or(BigInteger.valueOf(byteValue & 0xffL).shiftLeft(byteIndex * 8));
        if (signed && bits.testBit(width - 1)) {
            bits = bits.subtract(modulus);
        }
        return bits;
    }

    private static boolean isBigIntegerCodec(String codec) {
        return SIGNED_BIG_INTEGER_CODEC.equals(codec)
                || UNSIGNED_BIG_INTEGER_CODEC.equals(codec);
    }

    private static boolean isBuiltInCodec(String codec) {
        return isBigIntegerCodec(codec)
                || F128_CODEC.equals(codec)
                || RAW_POINTER_VIEW_CODEC.equals(codec);
    }

    private static long valueBits(Object value, int size) {
        if (value == null) {
            return 0;
        }
        return incomingBits(value, size);
    }

    private static Object carrierFromBits(Object current, long bits, int size) {
        if (current instanceof Boolean) {
            return Boolean.valueOf(bits != 0);
        }
        if (current instanceof Byte) {
            return Byte.valueOf((byte) bits);
        }
        if (current instanceof Short) {
            return Short.valueOf((short) bits);
        }
        if (current instanceof Integer) {
            return Integer.valueOf((int) bits);
        }
        if (current instanceof Long) {
            return Long.valueOf(bits);
        }
        if (current instanceof Float) {
            return Float.valueOf(
                    size == 2 ? halfToFloat((int) bits) : Float.intBitsToFloat((int) bits));
        }
        if (current instanceof Double) {
            return Double.valueOf(Double.longBitsToDouble(bits));
        }
        if (current instanceof Character) {
            return Character.valueOf((char) bits);
        }
        if (current instanceof Pointer) {
            return pointerObjectFromAddress(bits);
        }
        if (current == null) {
            if (size <= 4) {
                return Integer.valueOf((int) bits);
            }
            return Long.valueOf(bits);
        }
        throw new UnsupportedOperationException(
                "aggregate JVM carrier requires a generated Rust memory codec: "
                        + current.getClass().getName());
    }

    private static Object convertDirectValue(Object current, Object value, int size) {
        if (current instanceof Boolean && value instanceof Number) {
            return Boolean.valueOf(((Number) value).intValue() != 0);
        }
        if (current instanceof Byte && value instanceof Number) {
            return Byte.valueOf(((Number) value).byteValue());
        }
        if (current instanceof Short && value instanceof Number) {
            return Short.valueOf(((Number) value).shortValue());
        }
        if (current instanceof Character && value instanceof Number) {
            return Character.valueOf((char) ((Number) value).intValue());
        }
        if (current instanceof Float && !(value instanceof Float)) {
            return Float.valueOf(
                    size == 2
                            ? halfToFloat(((Number) value).intValue())
                            : Float.intBitsToFloat(((Number) value).intValue()));
        }
        if (current instanceof Double && !(value instanceof Double)) {
            return Double.valueOf(Double.longBitsToDouble(((Number) value).longValue()));
        }
        if (current instanceof Long && value instanceof Float) {
            return Long.valueOf(
                    ((long) Float.floatToRawIntBits(((Float) value).floatValue())) & 0xffff_ffffL);
        }
        if (current instanceof Long && value instanceof Double) {
            return Long.valueOf(Double.doubleToRawLongBits(((Double) value).doubleValue()));
        }
        if (current instanceof Integer && value instanceof Float) {
            return Integer.valueOf(Float.floatToRawIntBits(((Float) value).floatValue()));
        }
        if (current instanceof Integer && value instanceof Number) {
            return Integer.valueOf(((Number) value).intValue());
        }
        if (current instanceof Long && value instanceof Number) {
            return Long.valueOf(((Number) value).longValue());
        }
        return value;
    }

    private static int inferredCarrierSize(Object value) {
        if (value instanceof Boolean || value instanceof Byte) {
            return 1;
        }
        if (value instanceof Short || value instanceof Character) {
            return 2;
        }
        if (value instanceof Integer || value instanceof Float) {
            return 4;
        }
        if (value instanceof Long || value instanceof Double || value instanceof Pointer) {
            return 8;
        }
        return 1;
    }

    private static int inferredArrayElementSize(Object array) {
        Class<?> component = array.getClass().getComponentType();
        if (component == boolean.class || component == byte.class) {
            return 1;
        }
        if (component == short.class || component == char.class) {
            return 2;
        }
        if (component == int.class || component == float.class) {
            return 4;
        }
        if (component == long.class || component == double.class) {
            return 8;
        }
        return 8;
    }

    private static int floatToHalf(float value) {
        int bits = Float.floatToRawIntBits(value);
        int sign = (bits >>> 16) & 0x8000;
        int magnitude = bits & 0x7fff_ffff;
        if (magnitude >= 0x7f80_0000) {
            int payload = (magnitude & 0x007f_ffff) >>> 13;
            return sign | 0x7c00 | (payload == 0 ? 0 : (payload | 1));
        }
        int exponent = ((magnitude >>> 23) & 0xff) - 127 + 15;
        int mantissa = magnitude & 0x007f_ffff;
        if (exponent <= 0) {
            if (exponent < -10) {
                return sign;
            }
            mantissa = (mantissa | 0x0080_0000) >>> (1 - exponent);
            if ((mantissa & 0x0000_1000) != 0) {
                mantissa += 0x0000_2000;
            }
            return sign | (mantissa >>> 13);
        }
        if ((mantissa & 0x0000_1000) != 0) {
            mantissa += 0x0000_2000;
            if ((mantissa & 0x0080_0000) != 0) {
                mantissa = 0;
                exponent++;
            }
        }
        if (exponent >= 31) {
            return sign | 0x7c00;
        }
        return sign | (exponent << 10) | (mantissa >>> 13);
    }

    private static float halfToFloat(int half) {
        int sign = (half & 0x8000) << 16;
        int exponent = (half >>> 10) & 0x1f;
        int mantissa = half & 0x03ff;
        int bits;
        if (exponent == 0) {
            if (mantissa == 0) {
                bits = sign;
            } else {
                int normalizedExponent = -14;
                while ((mantissa & 0x0400) == 0) {
                    mantissa <<= 1;
                    normalizedExponent--;
                }
                mantissa &= 0x03ff;
                bits = sign | ((normalizedExponent + 127) << 23) | (mantissa << 13);
            }
        } else if (exponent == 0x1f) {
            bits = sign | 0x7f80_0000 | (mantissa << 13);
        } else {
            bits = sign | ((exponent - 15 + 127) << 23) | (mantissa << 13);
        }
        return Float.intBitsToFloat(bits);
    }
}
