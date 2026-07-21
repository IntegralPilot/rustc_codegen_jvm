package org.rustlang.runtime;

import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.atomic.AtomicLong;

public final class Pointer {
    public static void dropRustValue(Object value) {
        if (value instanceof RustDrop) {
            ((RustDrop) value).rustDrop();
        }
    }

    public static boolean catchUnwind(Object tryFunction, Pointer data, Object catchFunction) {
        try {
            invokeRustFunction(tryFunction, data);
            return false;
        } catch (PanicSupport.RustPanic failure) {
            Pointer payload = Pointer.cell(failure, 8, MANAGED_OBJECT_VIEW_CODEC);
            invokeRustFunction(catchFunction, data, payload);
            return true;
        }
    }

    static Object invokeRustFunction(Object function, Object... arguments) {
        if (function == null) {
            throw new NullPointerException("Rust function pointer is null");
        }
        Method target = null;
        for (Method method : function.getClass().getMethods()) {
            if (method.getName().equals("call")
                    && method.getParameterTypes().length == arguments.length) {
                target = method;
                break;
            }
        }
        if (target == null) {
            throw new IllegalArgumentException(
                    "Rust function pointer has no compatible call method: "
                            + function.getClass().getName());
        }
        try {
            target.setAccessible(true);
            return target.invoke(function, arguments);
        } catch (InvocationTargetException failure) {
            rethrowUnchecked(failure.getCause());
            return null;
        } catch (IllegalAccessException failure) {
            throw new IllegalStateException("Rust function pointer invocation failed", failure);
        }
    }

    private static void rethrowUnchecked(Throwable failure) {
        if (failure instanceof RuntimeException) {
            throw (RuntimeException) failure;
        }
        if (failure instanceof Error) {
            throw (Error) failure;
        }
        throw new IllegalStateException("Rust unwind handler failed", failure);
    }

    private static final String MANAGED_OBJECT_VIEW_CODEC = "@managed-object";
    private static final String RAW_POINTER_VIEW_CODEC = "@raw-pointer";
    private static final String SLICE_POINTER_VIEW_CODEC_PREFIX = "@slice-pointer\n";
    private static final String STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX =
            "@struct-tail-pointer\n";
    private static final String TRAIT_POINTER_VIEW_CODEC_PREFIX = "@trait-pointer\n";
    private static final String SIGNED_BIG_INTEGER_CODEC = "@signed-big-integer";
    private static final String UNSIGNED_BIG_INTEGER_CODEC = "@unsigned-big-integer";
    private static final String F128_CODEC = "@f128";
    private static final String STRUCTURAL_VIEW_CODEC_PREFIX = "@structural-view:";
    private static final String STRUCT_TAIL_VIEW_CODEC_PREFIX = "@struct-tail-view:";
    private static final String SLICE_VIEW_CLASS_NAME = "org.rustlang.runtime.SliceView";
    private static final AtomicLong NEXT_ADDRESS = new AtomicLong(0x1_0000_0000L);
    private static final Map<Object, Long> ALLOCATION_BASES = new WeakHashMap<>();
    private static final Map<Object, Integer> ALLOCATION_ELEMENT_SIZES = new WeakHashMap<>();
    private static final Map<Object, Integer> ALLOCATION_ALIGNMENTS = new WeakHashMap<>();
    private static final Map<Object, String> ALLOCATION_CODECS = new WeakHashMap<>();
    private static final Map<String, byte[]> CONSTANT_ALLOCATIONS = new HashMap<>();
    private static final Map<Long, ExposedTarget> EXPOSED_ADDRESSES = new HashMap<>();
    private static final Map<String, Method[]> CODEC_METHODS = new HashMap<>();
    private static final Map<Class<?>, Method[]> SCALAR_ENUM_METHODS = new HashMap<>();
    private static final Map<Object, Long> MANAGED_OBJECT_ADDRESSES = new IdentityHashMap<>();
    private static final Map<Long, WeakReference<Object>> MANAGED_OBJECTS = new HashMap<>();
    private static final Map<String, Pointer> TRAIT_METADATA_MARKERS = new HashMap<>();
    private static final Map<Object, Map<Long, StructuralViewState>> STRUCTURAL_VIEWS =
            new WeakHashMap<>();
    private static final Map<Object, Map<Long, MemoryViewState>> MEMORY_VIEWS =
            new WeakHashMap<>();
    private static final Map<Object, MemoryViewOrigin> MEMORY_VIEW_ORIGINS =
            new WeakHashMap<>();
    private static final ThreadLocal<Integer> MEMORY_VIEW_WRITEBACK_DEPTH =
            ThreadLocal.withInitial(() -> 0);
    private static final Map<Object, Map<String, WeakReference<FieldCell>>> FIELD_CELLS =
            new WeakHashMap<>();
    private static final int ATOMIC_STRIPE_COUNT = 64;
    private static final int ATOMIC_RELAXED = 0;
    private static final int ATOMIC_RELEASE = 1;
    private static final int ATOMIC_ACQUIRE = 2;
    private static final int ATOMIC_ACQ_REL = 3;
    private static final int ATOMIC_SEQ_CST = 4;
    private static final Object[] ATOMIC_STRIPES = createAtomicStripes();
    private static final Object ATOMIC_SEQUENCE_LOCK = new Object();

    private static boolean isSliceViewType(Class<?> type) {
        return type != null && SLICE_VIEW_CLASS_NAME.equals(type.getName());
    }

    private static boolean isSliceViewCarrierType(Class<?> type) {
        for (Class<?> current = type; current != null; current = current.getSuperclass()) {
            if (SLICE_VIEW_CLASS_NAME.equals(current.getName())) {
                return true;
            }
        }
        return false;
    }

    /**
     * Keeps the distinct JVM carriers for a Rust struct-tail unsizing coercion
     * coherent. Rust guarantees exclusive access through a mutable borrow, so
     * synchronizing when execution changes carrier is sufficient even though
     * mutations happen directly on the generated public fields.
     */
    private static final class StructuralViewState {
        private final Map<Class<?>, Object> views = new HashMap<>();
        private Object active;

        private StructuralViewState(Object source) {
            views.put(source.getClass(), source);
            active = source;
        }

        private Object activate(Class<?> targetClass) {
            Object target = views.get(targetClass);
            if (target == null) {
                target = constructStructuralView(active, targetClass);
                views.put(targetClass, target);
            } else if (target != active) {
                copyStructuralFields(active, target);
            }
            active = target;
            return target;
        }
    }

    /**
     * A live JVM view of aggregate data stored in byte-addressable Rust memory.
     * Generated code can mutate public fields directly after a pointer load, so
     * the decoded carrier must remain authoritative until the memory is next
     * observed through another view.
     */
    private static final class MemoryViewState {
        private final int size;
        private final String codecClassName;
        private final Object value;
        private byte[] originalImage;

        private MemoryViewState(
                int size, String codecClassName, Object value, byte[] originalImage) {
            this.size = size;
            this.codecClassName = codecClassName;
            this.value = value;
            this.originalImage = originalImage;
        }
    }

    /** Original byte-addressable storage for a decoded aggregate receiver. */
    private static final class MemoryViewOrigin {
        private final WeakReference<Object> allocation;
        private final int allocationElementSize;
        private final long byteOffset;
        private final long viewSize;
        private final String allocationCodecClassName;
        private final long metadata;

        private MemoryViewOrigin(Pointer pointer) {
            allocation = new WeakReference<>(pointer.allocation);
            allocationElementSize = pointer.allocationElementSize;
            byteOffset = pointer.byteOffset;
            viewSize = pointer.viewSize;
            allocationCodecClassName = pointer.allocationCodecClassName;
            metadata = pointer.metadata;
        }
    }

    /**
     * Implements a whole-value assignment through an instance method's
     * {@code &mut self}. The JVM receiver identity cannot be replaced, so copy
     * the generated Rust value fields from the replacement object instead.
     */
    public static void overwriteManagedObject(Object target, Object replacement) {
        if (target == replacement) {
            return;
        }
        if (target == null || replacement == null || target.getClass() != replacement.getClass()) {
            throw new IllegalArgumentException("managed-object overwrite requires matching non-null classes");
        }
        try {
            for (Field field : target.getClass().getFields()) {
                if (!Modifier.isStatic(field.getModifiers())) {
                    field.set(target, field.get(replacement));
                }
            }
        } catch (IllegalAccessException error) {
            throw new IllegalStateException("could not overwrite managed Rust value", error);
        }
    }

    /** Runs generated Rust element drop glue over a dynamically sized slice. */
    public static void dropSlice(Object slice, String ownerClassName, String methodName) {
        if (slice == null) {
            return;
        }
        try {
            Class<?> sliceClass = slice.getClass();
            Object array = instanceField(sliceClass, "array").get(slice);
            int offset = instanceField(sliceClass, "offset").getInt(slice);
            int length = instanceField(sliceClass, "length").getInt(slice);
            Pointer data = array instanceof Pointer
                    ? ((Pointer) array).sliceElementView().add(offset)
                    : null;
            if (data == null && (array == null || !array.getClass().isArray())) {
                throw new IllegalArgumentException("Rust slice drop requires array-backed storage");
            }
            MethodHandle drop = null;
            for (int index = 0; index < length; index++) {
                Pointer element = data == null
                        ? Pointer.cell(Array.get(array, offset + index))
                        : data.add(index);
                Object managed = element.getObject();
                if (managed instanceof RustDrop) {
                    ((RustDrop) managed).rustDrop();
                } else {
                    if (drop == null) {
                        Class<?> owner = Class.forName(ownerClassName.replace('/', '.'));
                        drop = MethodHandles.publicLookup().findStatic(
                                owner,
                                methodName,
                                MethodType.methodType(void.class, Pointer.class));
                    }
                    drop.invokeExact(element);
                }
            }
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not invoke Rust slice element drop glue", error);
        } catch (Throwable error) {
            if (error instanceof RuntimeException) {
                throw (RuntimeException) error;
            }
            if (error instanceof Error) {
                throw (Error) error;
            }
            throw new IllegalStateException("Rust slice element drop failed", error);
        }
    }

    /** Creates an independent JVM carrier for a copied Rust aggregate value. */
    public static Object copyManagedValue(Object value) {
        if (value == null) {
            return null;
        }
        Class<?> valueClass = value.getClass();
        if (valueClass.isArray()) {
            int length = Array.getLength(value);
            Object copy = Array.newInstance(valueClass.getComponentType(), length);
            if (valueClass.getComponentType().isPrimitive()) {
                System.arraycopy(value, 0, copy, 0, length);
            } else {
                for (int index = 0; index < length; index++) {
                    Array.set(copy, index, copyManagedValue(Array.get(value, index)));
                }
            }
            return copy;
        }
        if (valueClass.isPrimitive()
                || value instanceof Number
                || value instanceof Boolean
                || value instanceof Character
                || value instanceof String
                || valueClass.isEnum()
                || valueClass.getName().startsWith("org.rustlang.runtime.")
                || isRustFunctionPointer(valueClass)) {
            return value;
        }

        try {
            Field[] allFields = valueClass.getFields();
            int instanceFieldCount = 0;
            for (Field field : allFields) {
                if (!Modifier.isStatic(field.getModifiers())) {
                    instanceFieldCount++;
                }
            }

            Constructor<?> constructor = null;
            for (Constructor<?> candidate : valueClass.getConstructors()) {
                if (candidate.getParameterTypes().length == instanceFieldCount) {
                    constructor = candidate;
                    break;
                }
            }
            if (constructor == null) {
                throw new IllegalStateException("no value constructor for " + valueClass.getName());
            }
            Class<?>[] parameterTypes = constructor.getParameterTypes();
            Object[] defaults = new Object[parameterTypes.length];
            for (int index = 0; index < parameterTypes.length; index++) {
                defaults[index] = defaultValue(parameterTypes[index]);
            }
            Object copy = constructor.newInstance(defaults);
            for (Field field : allFields) {
                if (!Modifier.isStatic(field.getModifiers())) {
                    field.set(copy, copyManagedValue(field.get(value)));
                }
            }
            return copy;
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not copy managed Rust value", error);
        }
    }

    /**
     * Implements a Rust array-repeat initializer without requiring the
     * compiler to emit one bytecode store for every element.
     */
    public static void fillArray(Object array, Object value, boolean copyValue) {
        int length = Array.getLength(array);
        for (int index = 0; index < length; index++) {
            Array.set(array, index, copyValue ? copyManagedValue(value) : value);
        }
    }

    /** Encodes an array into Rust's contiguous, little-endian memory layout. */
    public static void encodeArrayMemory(
            Object array, byte[] bytes, int offset, int elementSize, String elementCodec) {
        int length = Array.getLength(array);
        // A differently typed pointer into an aggregate array (for example,
        // `&mut T` retyped from `MaybeUninit<T>`) may have a decoded live view
        // whose fields were mutated directly by generated bytecode. Preserve
        // those mutations before a containing aggregate encodes this array.
        new Pointer(
                array,
                elementSize,
                0,
                Math.multiplyExact(length, elementSize),
                elementCodec)
                .flushAllMemoryViews();
        for (int index = 0; index < length; index++) {
            byte[] element = encodeMemoryValue(Array.get(array, index), elementSize, elementCodec);
            System.arraycopy(element, 0, bytes, offset + index * elementSize, elementSize);
        }
    }

    /** Decodes Rust's contiguous, little-endian memory layout into an array. */
    public static void decodeArrayMemory(
            byte[] bytes, int offset, Object array, int elementSize, String elementCodec) {
        int length = Array.getLength(array);
        Class<?> componentType = array.getClass().getComponentType();
        for (int index = 0; index < length; index++) {
            Object element = decodeMemoryValue(
                    bytes, offset + index * elementSize, elementSize, elementCodec, componentType);
            Array.set(array, index, element);
        }
    }

    private static byte[] encodeMemoryValue(Object value, int size, String codec) {
        if (isFatPointerCodec(codec)) {
            return encodeFatPointer(value, size, codec);
        }
        if (codec != null
                && !MANAGED_OBJECT_VIEW_CODEC.equals(codec)
                && !RAW_POINTER_VIEW_CODEC.equals(codec)
                && !isBigIntegerCodec(codec)
                && !F128_CODEC.equals(codec)) {
            byte[] encoded = encodeAggregate(codec, value);
            if (encoded.length != size) {
                throw new IllegalStateException("Rust aggregate codec returned "
                        + encoded.length + " bytes, expected " + size);
            }
            return encoded;
        }

        byte[] encoded = new byte[size];
        if (value == null) {
            return encoded;
        }
        if (isBigIntegerCodec(codec)) {
            BigInteger integer = value instanceof I128
                    ? ((I128) value).toBigInteger()
                    : ((U128) value).toBigInteger();
            for (int index = 0; index < size; index++) {
                encoded[index] = bigIntegerByte(integer, index);
            }
            return encoded;
        }
        if (F128_CODEC.equals(codec)) {
            BigInteger bits = ((F128) value).toBits();
            for (int index = 0; index < size; index++) {
                encoded[index] = bigIntegerByte(bits, index);
            }
            return encoded;
        }

        long bits;
        if (MANAGED_OBJECT_VIEW_CODEC.equals(codec)) {
            bits = managedObjectAddress(value);
        } else if (RAW_POINTER_VIEW_CODEC.equals(codec)) {
            bits = ((Pointer) value).address();
        } else {
            bits = incomingBits(value, size);
        }
        for (int index = 0; index < Math.min(size, 8); index++) {
            encoded[index] = (byte) (bits >>> (index * 8));
        }
        return encoded;
    }

    private static Object decodeMemoryValue(
            byte[] bytes, int offset, int size, String codec, Class<?> componentType) {
        if (isFatPointerCodec(codec)) {
            return decodeFatPointer(bytes, offset, size, codec);
        }
        if (codec != null
                && !MANAGED_OBJECT_VIEW_CODEC.equals(codec)
                && !RAW_POINTER_VIEW_CODEC.equals(codec)
                && !isBigIntegerCodec(codec)
                && !F128_CODEC.equals(codec)) {
            byte[] encoded = new byte[size];
            System.arraycopy(bytes, offset, encoded, 0, size);
            return decodeAggregate(codec, encoded);
        }
        if (isBigIntegerCodec(codec)) {
            BigInteger value = bigIntegerFromBytes(
                    bytes, offset, size, SIGNED_BIG_INTEGER_CODEC.equals(codec));
            return SIGNED_BIG_INTEGER_CODEC.equals(codec)
                    ? I128.fromBigInteger(value)
                    : U128.fromBigInteger(value);
        }
        if (F128_CODEC.equals(codec)) {
            return F128.fromBits(bigIntegerFromBytes(bytes, offset, size, false));
        }

        long bits = 0;
        for (int index = 0; index < Math.min(size, 8); index++) {
            bits |= ((long) bytes[offset + index] & 0xffL) << (index * 8);
        }
        if (MANAGED_OBJECT_VIEW_CODEC.equals(codec)) {
            return managedObjectFromAddress(bits);
        }
        if (RAW_POINTER_VIEW_CODEC.equals(codec)) {
            return pointerObjectFromAddress(bits);
        }
        return carrierFromBits(defaultValue(componentType), bits, size);
    }

    private static boolean isFatPointerCodec(String codec) {
        return codec != null
                && (codec.startsWith(SLICE_POINTER_VIEW_CODEC_PREFIX)
                        || codec.startsWith(STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX)
                        || codec.startsWith(TRAIT_POINTER_VIEW_CODEC_PREFIX));
    }

    private static int fatPointerWordSize(int size) {
        int wordSize = size / 2;
        if (size % 2 != 0 || (wordSize != 4 && wordSize != 8)) {
            throw new IllegalArgumentException(
                    "Rust fat pointer must contain two 32- or 64-bit words, found " + size
                            + " bytes");
        }
        return wordSize;
    }

    private static void writeMemoryWord(byte[] bytes, int offset, int size, long value) {
        for (int index = 0; index < size; index++) {
            bytes[offset + index] = (byte) (value >>> (index * 8));
        }
    }

    private static long readMemoryWord(byte[] bytes, int offset, int size) {
        long value = 0;
        for (int index = 0; index < size; index++) {
            value |= ((long) bytes[offset + index] & 0xffL) << (index * 8);
        }
        return value;
    }

    private static String[] slicePointerDescriptor(String codec) {
        String descriptor = codec.substring(SLICE_POINTER_VIEW_CODEC_PREFIX.length());
        // The element codec may itself be a structured fat-pointer codec, so
        // only the carrier and element-size separators are structural here.
        String[] parts = descriptor.split("\\n", 3);
        if (parts.length != 3) {
            throw new IllegalArgumentException("invalid Rust slice-pointer codec descriptor");
        }
        return parts;
    }

    private static int slicePointerElementSize(String[] descriptor) {
        try {
            int size = Integer.parseInt(descriptor[1]);
            if (size < 0) {
                throw new IllegalArgumentException("negative Rust slice element size");
            }
            return size;
        } catch (NumberFormatException error) {
            throw new IllegalArgumentException("invalid Rust slice element size", error);
        }
    }

    private static String[] structTailPointerDescriptor(String codec) {
        String descriptor = codec.substring(STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX.length());
        String[] parts = descriptor.split("\\n", 5);
        if (parts.length != 5) {
            throw new IllegalArgumentException("invalid Rust struct-tail pointer codec descriptor");
        }
        return parts;
    }

    private static long structTailPointerPrefixSize(String[] descriptor) {
        try {
            long size = Long.parseLong(descriptor[1]);
            if (size < 0) {
                throw new IllegalArgumentException("negative Rust struct-tail prefix size");
            }
            return size;
        } catch (NumberFormatException error) {
            throw new IllegalArgumentException("invalid Rust struct-tail prefix size", error);
        }
    }

    private static int structTailPointerElementSize(String[] descriptor) {
        try {
            int size = Integer.parseInt(descriptor[3]);
            if (size < 0) {
                throw new IllegalArgumentException("negative Rust struct-tail element size");
            }
            return size;
        } catch (NumberFormatException error) {
            throw new IllegalArgumentException("invalid Rust struct-tail element size", error);
        }
    }

    private static byte[] encodeFatPointer(Object value, int size, String codec) {
        int wordSize = fatPointerWordSize(size);
        byte[] image = new byte[size];
        if (value == null) {
            return image;
        }

        long dataAddress;
        long pointerMetadata;
        if (codec.startsWith(SLICE_POINTER_VIEW_CODEC_PREFIX)) {
            String[] descriptor = slicePointerDescriptor(codec);
            int elementSize = slicePointerElementSize(descriptor);
            String elementCodec = descriptor[2].isEmpty() ? null : descriptor[2];
            Pointer data = fromSlice(value, elementSize, elementCodec);
            dataAddress = data.address();
            try {
                pointerMetadata = Integer.toUnsignedLong(
                        instanceField(value.getClass(), "length").getInt(value));
            } catch (ReflectiveOperationException error) {
                throw new IllegalArgumentException("invalid Rust slice fat pointer", error);
            }
        } else if (codec.startsWith(STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX)) {
            if (!(value instanceof Pointer)) {
                throw new IllegalArgumentException(
                        "Rust struct-tail pointer requires a Pointer carrier");
            }
            Pointer pointer = (Pointer) value;
            dataAddress = pointer.address();
            pointerMetadata = pointer.metadata();
        } else {
            if (!(value instanceof Pointer)) {
                throw new IllegalArgumentException(
                        "Rust raw trait-object pointer requires a Pointer carrier");
            }
            Pointer pointer = (Pointer) value;
            String metadataClass = codec.substring(TRAIT_POINTER_VIEW_CODEC_PREFIX.length());
            dataAddress = erasedAddress(pointer);
            pointerMetadata = traitMetadataMarker(pointer, metadataClass).address();
        }

        writeMemoryWord(image, 0, wordSize, dataAddress);
        writeMemoryWord(image, wordSize, wordSize, pointerMetadata);
        return image;
    }

    private static Object decodeFatPointer(
            byte[] bytes, int offset, int size, String codec) {
        int wordSize = fatPointerWordSize(size);
        long dataAddress = readMemoryWord(bytes, offset, wordSize);
        long pointerMetadata = readMemoryWord(bytes, offset + wordSize, wordSize);
        if (codec.startsWith(TRAIT_POINTER_VIEW_CODEC_PREFIX)) {
            return pointerObjectFromAddress(dataAddress);
        }

        if (codec.startsWith(STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX)) {
            String[] descriptor = structTailPointerDescriptor(codec);
            int elementSize = structTailPointerElementSize(descriptor);
            String elementCodec = descriptor[4].isEmpty() ? null : descriptor[4];
            Pointer data = pointerObjectFromAddress(dataAddress).retype(elementSize, elementCodec);
            return data.retype(
                            structTailPointerPrefixSize(descriptor),
                            STRUCT_TAIL_VIEW_CODEC_PREFIX
                                    + descriptor[0] + "\n"
                                    + descriptor[1] + "\n"
                                    + descriptor[2] + "\n"
                                    + descriptor[3] + "\n"
                                    + descriptor[4])
                    .withMetadata(pointerMetadata);
        }

        String[] descriptor = slicePointerDescriptor(codec);
        int elementSize = slicePointerElementSize(descriptor);
        String elementCodec = descriptor[2].isEmpty() ? null : descriptor[2];
        Pointer data = pointerObjectFromAddress(dataAddress).retype(elementSize, elementCodec);
        try {
            Class<?> viewClass = Class.forName(descriptor[0].replace('/', '.'));
            return viewClass
                    .getConstructor(Object.class, int.class, int.class)
                    .newInstance(data, 0, Math.toIntExact(pointerMetadata));
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not reconstruct Rust slice fat pointer", error);
        }
    }

    /** Writes a JVM fat-pointer carrier using Rust's native two-word layout. */
    public static void encodeFatPointerMemory(
            Object value, byte[] bytes, int offset, int size, String codec) {
        byte[] image = encodeFatPointer(value, size, codec);
        System.arraycopy(image, 0, bytes, offset, size);
    }

    /** Reconstructs a JVM fat-pointer carrier from Rust's native two-word layout. */
    public static Object decodeFatPointerMemory(
            byte[] bytes, int offset, int size, String codec) {
        return decodeFatPointer(bytes, offset, size, codec);
    }

    private static boolean isRustFunctionPointer(Class<?> valueClass) {
        for (Class<?> implementedInterface : valueClass.getInterfaces()) {
            if (implementedInterface.getName().startsWith("org.rustlang.runtime.FnPtr_")) {
                // Function-pointer carriers are immutable callable identities. This
                // also covers the hidden classes produced by LambdaMetafactory,
                // which intentionally expose no public value constructor.
                return true;
            }
        }
        return false;
    }

    private static Object defaultValue(Class<?> type) {
        if (!type.isPrimitive()) {
            return null;
        }
        if (type == boolean.class) {
            return false;
        }
        if (type == char.class) {
            return '\0';
        }
        if (type == byte.class) {
            return (byte) 0;
        }
        if (type == short.class) {
            return (short) 0;
        }
        if (type == int.class) {
            return 0;
        }
        if (type == long.class) {
            return 0L;
        }
        if (type == float.class) {
            return 0.0f;
        }
        if (type == double.class) {
            return 0.0d;
        }
        throw new IllegalArgumentException("unknown primitive type " + type.getName());
    }

    private static boolean isStructuralViewCodec(String codecClassName) {
        return codecClassName != null
                && (codecClassName.startsWith(STRUCTURAL_VIEW_CODEC_PREFIX)
                        || codecClassName.startsWith(STRUCT_TAIL_VIEW_CODEC_PREFIX));
    }

    private static Field instanceField(Class<?> owner, String name)
            throws NoSuchFieldException {
        Field field = owner.getField(name);
        if (Modifier.isStatic(field.getModifiers())) {
            throw new NoSuchFieldException(owner.getName() + "." + name + " is static");
        }
        return field;
    }

    private static Object sliceBackingForArray(Object slice, Class<?> targetArrayType)
            throws ReflectiveOperationException {
        Class<?> sliceClass = slice.getClass();
        Object backing = instanceField(sliceClass, "array").get(slice);
        int offset = instanceField(sliceClass, "offset").getInt(slice);
        int length = instanceField(sliceClass, "length").getInt(slice);
        if (backing instanceof Pointer) {
            backing = ((Pointer) backing).backingArray();
        }
        if (backing == null || !backing.getClass().isArray()) {
            throw new IllegalArgumentException("slice-tail view is not backed by an array");
        }
        if (!targetArrayType.getComponentType().isAssignableFrom(
                backing.getClass().getComponentType())
                && targetArrayType.getComponentType()
                        != backing.getClass().getComponentType()) {
            throw new IllegalArgumentException(
                    "slice-tail backing has incompatible array component type");
        }
        if (offset == 0 && length == Array.getLength(backing)
                && targetArrayType.isInstance(backing)) {
            return backing;
        }
        Object result = Array.newInstance(targetArrayType.getComponentType(), length);
        System.arraycopy(backing, offset, result, 0, length);
        return result;
    }

    /** Normalizes a fixed-array value whose optimized local may still hold a slice view. */
    public static Object arrayCarrier(Object value) {
        if (value == null || value.getClass().isArray()) {
            return value;
        }
        if (!isSliceViewCarrierType(value.getClass())) {
            throw new ClassCastException(
                    value.getClass().getName() + " is neither a JVM array nor a Rust slice view");
        }
        try {
            Object backing = instanceField(value.getClass(), "array").get(value);
            if (backing instanceof Pointer) {
                backing = ((Pointer) backing).backingArray();
            }
            if (backing == null || !backing.getClass().isArray()) {
                throw new IllegalArgumentException("Rust slice view is not backed by an array");
            }
            return sliceBackingForArray(value, backing.getClass());
        } catch (ReflectiveOperationException error) {
            throw new IllegalArgumentException("invalid Rust slice view", error);
        }
    }

    private static Object adaptStructuralField(Object value, Class<?> targetType)
            throws ReflectiveOperationException {
        if (value == null) {
            return defaultValue(targetType);
        }
        if (targetType.isInstance(value)
                || (targetType.isPrimitive()
                        && (value instanceof Number
                                || value instanceof Boolean
                                || value instanceof Character))) {
            return value;
        }
        if (isSliceViewType(targetType) && value.getClass().isArray()) {
            Constructor<?> constructor = targetType.getConstructor(
                    Object.class, int.class, int.class);
            return constructor.newInstance(value, 0, Array.getLength(value));
        }
        if (targetType.isArray() && isSliceViewType(value.getClass())) {
            return sliceBackingForArray(value, targetType);
        }
        return constructStructuralView(value, targetType);
    }

    private static Constructor<?> structuralConstructor(Class<?> targetClass, int fieldCount) {
        for (Constructor<?> constructor : targetClass.getConstructors()) {
            if (constructor.getParameterCount() == fieldCount) {
                return constructor;
            }
        }
        throw new IllegalArgumentException(
                "no generated Rust value constructor for " + targetClass.getName());
    }

    private static Object constructStructuralView(Object source, Class<?> targetClass) {
        try {
            Object transparentInner = null;
            for (Field field : source.getClass().getFields()) {
                if (!Modifier.isStatic(field.getModifiers())) {
                    if (transparentInner != null) {
                        transparentInner = null;
                        break;
                    }
                    transparentInner = field.get(source);
                }
            }
            if (transparentInner != null && targetClass.isInstance(transparentInner)) {
                return transparentInner;
            }
            Field[] targetFields = java.util.Arrays.stream(targetClass.getFields())
                    .filter(field -> !Modifier.isStatic(field.getModifiers()))
                    .toArray(Field[]::new);
            Constructor<?> constructor = structuralConstructor(targetClass, targetFields.length);
            Object[] args = new Object[constructor.getParameterCount()];
            java.lang.reflect.Parameter[] parameters = constructor.getParameters();
            for (int index = 0; index < parameters.length; index++) {
                if (parameters.length == 1 && parameters[index].getType().isInstance(source)) {
                    args[index] = source;
                } else {
                    String fieldName = parameters[index].isNamePresent()
                            ? parameters[index].getName()
                            : targetFields[index].getName();
                    Field sourceField = instanceField(source.getClass(), fieldName);
                    args[index] = adaptStructuralField(
                            sourceField.get(source), parameters[index].getType());
                }
            }
            return constructor.newInstance(args);
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException(
                    "could not construct structural Rust view "
                            + source.getClass().getName() + " -> " + targetClass.getName(),
                    error);
        }
    }

    private static void copyStructuralFields(Object source, Object target) {
        try {
            Field[] targetFields = java.util.Arrays.stream(target.getClass().getFields())
                    .filter(field -> !Modifier.isStatic(field.getModifiers()))
                    .toArray(Field[]::new);
            if (targetFields.length == 1 && targetFields[0].getType().isInstance(source)) {
                targetFields[0].set(target, source);
                return;
            }
            Field[] sourceFields = java.util.Arrays.stream(source.getClass().getFields())
                    .filter(field -> !Modifier.isStatic(field.getModifiers()))
                    .toArray(Field[]::new);
            if (sourceFields.length == 1) {
                Object inner = sourceFields[0].get(source);
                if (inner != null && target.getClass().isInstance(inner)) {
                    copyStructuralFields(inner, target);
                    return;
                }
            }
            for (Field targetField : targetFields) {
                Field sourceField = instanceField(source.getClass(), targetField.getName());
                Object sourceValue = sourceField.get(source);
                Object targetValue = targetField.get(target);
                Object adapted;
                if (sourceValue != null && targetValue != null
                        && !targetField.getType().isInstance(sourceValue)
                        && !targetField.getType().isArray()
                        && !isSliceViewType(targetField.getType())) {
                    copyStructuralFields(sourceValue, targetValue);
                    adapted = targetValue;
                } else {
                    adapted = adaptStructuralField(sourceValue, targetField.getType());
                }
                targetField.set(target, adapted);
            }
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException(
                    "could not synchronize structural Rust view "
                            + source.getClass().getName() + " -> " + target.getClass().getName(),
                    error);
        }
    }

    private StructuralViewState structuralViewState(Object source, boolean create) {
        synchronized (STRUCTURAL_VIEWS) {
            Map<Long, StructuralViewState> allocationViews = STRUCTURAL_VIEWS.get(allocation);
            if (allocationViews == null) {
                if (!create) {
                    return null;
                }
                allocationViews = new HashMap<>();
                STRUCTURAL_VIEWS.put(allocation, allocationViews);
            }
            StructuralViewState state = allocationViews.get(byteOffset);
            if (state == null && create) {
                state = new StructuralViewState(source);
                allocationViews.put(byteOffset, state);
            }
            return state;
        }
    }

    private void clearStructuralViewState() {
        synchronized (STRUCTURAL_VIEWS) {
            Map<Long, StructuralViewState> allocationViews = STRUCTURAL_VIEWS.get(allocation);
            if (allocationViews != null) {
                allocationViews.remove(byteOffset);
                if (allocationViews.isEmpty()) {
                    STRUCTURAL_VIEWS.remove(allocation);
                }
            }
        }
    }

    private static boolean rangesOverlap(long leftOffset, int leftSize,
            long rightOffset, int rightSize) {
        long leftEnd = Math.addExact(leftOffset, (long) leftSize);
        long rightEnd = Math.addExact(rightOffset, (long) rightSize);
        return leftOffset < rightEnd && rightOffset < leftEnd;
    }

    private void writeBackMemoryView(long offset, MemoryViewState state) {
        int previousDepth = MEMORY_VIEW_WRITEBACK_DEPTH.get();
        MEMORY_VIEW_WRITEBACK_DEPTH.set(previousDepth + 1);
        try {
            byte[] image = encodeAggregate(state.codecClassName, state.value);
            if (image.length != state.size) {
                throw new IllegalStateException("Rust aggregate codec returned "
                        + image.length + " bytes, expected " + state.size);
            }
            if (Arrays.equals(image, state.originalImage)) {
                return;
            }
            new Pointer(
                    allocation,
                    allocationElementSize,
                    offset,
                    state.size,
                    allocationCodecClassName,
                    allocationCodecClassName,
                    exposedAddress).storeRange(image);
            state.originalImage = image;
        } finally {
            MEMORY_VIEW_WRITEBACK_DEPTH.set(previousDepth);
        }
    }

    private void flushMemoryViewsOverlapping(long offset, int size) {
        java.util.List<Map.Entry<Long, MemoryViewState>> pending =
                new java.util.ArrayList<>();
        synchronized (MEMORY_VIEWS) {
            Map<Long, MemoryViewState> views = MEMORY_VIEWS.get(allocation);
            if (views == null) {
                return;
            }
            java.util.Iterator<Map.Entry<Long, MemoryViewState>> entries =
                    views.entrySet().iterator();
            while (entries.hasNext()) {
                Map.Entry<Long, MemoryViewState> entry = entries.next();
                if (rangesOverlap(offset, size, entry.getKey(), entry.getValue().size)) {
                    pending.add(entry);
                    entries.remove();
                }
            }
            if (views.isEmpty()) {
                MEMORY_VIEWS.remove(allocation);
            }
        }
        for (Map.Entry<Long, MemoryViewState> entry : pending) {
            writeBackMemoryView(entry.getKey(), entry.getValue());
        }
    }

    private void flushAllMemoryViews() {
        java.util.List<Map.Entry<Long, MemoryViewState>> pending;
        synchronized (MEMORY_VIEWS) {
            Map<Long, MemoryViewState> views = MEMORY_VIEWS.remove(allocation);
            if (views == null) {
                return;
            }
            pending = new java.util.ArrayList<>(views.entrySet());
        }
        for (Map.Entry<Long, MemoryViewState> entry : pending) {
            writeBackMemoryView(entry.getKey(), entry.getValue());
        }
    }

    private void discardMemoryViewsOverlapping(long offset, int size) {
        synchronized (MEMORY_VIEWS) {
            Map<Long, MemoryViewState> views = MEMORY_VIEWS.get(allocation);
            if (views == null) {
                return;
            }
            views.entrySet().removeIf(entry ->
                    rangesOverlap(offset, size, entry.getKey(), entry.getValue().size));
            if (views.isEmpty()) {
                MEMORY_VIEWS.remove(allocation);
            }
        }
    }

    /**
     * Preserves mutations made through decoded aggregate objects before an
     * ordinary byte write replaces part of their storage. During write-back,
     * however, the encoded bytes are already authoritative and the cached
     * view must only be invalidated to avoid recursively flushing itself.
     */
    private void prepareMemoryWrite(long offset, int size) {
        if (MEMORY_VIEW_WRITEBACK_DEPTH.get() > 0) {
            discardMemoryViewsOverlapping(offset, size);
        } else {
            flushMemoryViewsOverlapping(offset, size);
        }
    }

    private Object decodedMemoryView() {
        synchronized (atomicStripe(this)) {
            return decodedMemoryViewLocked();
        }
    }

    private Object decodedMemoryViewLocked() {
        int materializedSize = materializedViewSize();
        synchronized (MEMORY_VIEWS) {
            Map<Long, MemoryViewState> views = MEMORY_VIEWS.get(allocation);
            MemoryViewState cached = views == null ? null : views.get(byteOffset);
            if (cached != null
                    && cached.size == viewSize
                    && cached.codecClassName.equals(viewCodecClassName)) {
                boundMemoryViewState = cached;
                Object transparent = transparentManagedView(cached.value.getClass());
                if (transparent != null) {
                    registerMemoryViewOrigin(transparent);
                    return transparent;
                }
                registerMemoryViewOrigin(cached.value);
                return cached.value;
            }
        }

        flushMemoryViewsOverlapping(byteOffset, materializedSize);
        byte[] image = new byte[materializedSize];
        for (int index = 0; index < materializedSize; index++) {
            image[index] = (byte) loadByte(byteOffset + index);
        }
        Object decoded = decodeAggregate(viewCodecClassName, image);
        Object transparent = transparentManagedView(decoded.getClass());
        if (transparent != null) {
            registerMemoryViewOrigin(transparent);
            return transparent;
        }
        MemoryViewState state =
                new MemoryViewState(materializedSize, viewCodecClassName, decoded, image);
        synchronized (MEMORY_VIEWS) {
            MEMORY_VIEWS.computeIfAbsent(allocation, ignored -> new HashMap<>())
                    .put(byteOffset, state);
        }
        boundMemoryViewState = state;
        registerMemoryViewOrigin(decoded);
        return decoded;
    }

    private void registerMemoryViewOrigin(Object value) {
        if (value == null || allocation == null) {
            return;
        }
        synchronized (MEMORY_VIEW_ORIGINS) {
            MEMORY_VIEW_ORIGINS.put(value, new MemoryViewOrigin(this));
        }
    }

    /** Commits direct field mutations made through the current decoded view. */
    public void commitMemoryView() {
        discardProjectedFieldViews(managedViewObject());
        if (allocation == null || viewCodecClassName == null) {
            return;
        }
        synchronized (atomicStripe(this)) {
            commitMemoryViewLocked();
        }
    }

    private void commitMemoryViewLocked() {
        MemoryViewState state;
        synchronized (MEMORY_VIEWS) {
            Map<Long, MemoryViewState> views = MEMORY_VIEWS.get(allocation);
            state = views == null ? null : views.get(byteOffset);
            if (state == null
                    || state != boundMemoryViewState
                    || state.size != viewSize
                    || !state.codecClassName.equals(viewCodecClassName)) {
                return;
            }
        }

        writeBackMemoryView(byteOffset, state);
        // writeBackMemoryView updates the byte representation through the
        // ordinary store path, which invalidates overlapping decoded views.
        // This object is still the live Rust receiver, so keep it authoritative
        // for references derived from the call that just completed.
        synchronized (MEMORY_VIEWS) {
            Map<Long, MemoryViewState> views =
                    MEMORY_VIEWS.computeIfAbsent(allocation, ignored -> new HashMap<>());
            if (!views.containsKey(byteOffset)) {
                views.put(byteOffset, state);
            }
        }
    }

    private Object managedViewObject() {
        if (boundMemoryViewState != null) {
            return boundMemoryViewState.value;
        }
        if (allocation instanceof Cell) {
            return ((Cell) allocation).value;
        }
        if (allocation instanceof ReceiverCell) {
            return ((ReceiverCell) allocation).value;
        }
        if (allocation instanceof FieldCell) {
            return ((FieldCell) allocation).get();
        }
        return isDirectAllocationView() ? readAlignedElement() : null;
    }

    private static void discardProjectedFieldViews(Object owner) {
        if (owner == null) {
            return;
        }
        java.util.List<FieldCell> projected = new java.util.ArrayList<>();
        synchronized (FIELD_CELLS) {
            Map<String, WeakReference<FieldCell>> fields = FIELD_CELLS.get(owner);
            if (fields == null) {
                return;
            }
            for (WeakReference<FieldCell> reference : fields.values()) {
                FieldCell cell = reference.get();
                if (cell != null) {
                    projected.add(cell);
                }
            }
        }
        synchronized (MEMORY_VIEWS) {
            for (FieldCell cell : projected) {
                MEMORY_VIEWS.remove(cell);
            }
        }
        synchronized (STRUCTURAL_VIEWS) {
            for (FieldCell cell : projected) {
                STRUCTURAL_VIEWS.remove(cell);
            }
        }
        synchronized (MEMORY_VIEW_ORIGINS) {
            MEMORY_VIEW_ORIGINS.entrySet().removeIf(entry -> {
                Object originAllocation = entry.getValue().allocation.get();
                return projected.contains(originAllocation);
            });
        }
    }

    private static boolean hasProjectedFieldCells(Object owner) {
        if (owner == null) {
            return false;
        }
        synchronized (FIELD_CELLS) {
            Map<String, WeakReference<FieldCell>> fields = FIELD_CELLS.get(owner);
            if (fields == null) {
                return false;
            }
            fields.entrySet().removeIf(entry -> entry.getValue().get() == null);
            return !fields.isEmpty();
        }
    }

    /**
     * Returns the real inner object for a full-size transparent view of managed
     * storage. Rust wrappers such as {@code UnsafeCell<T>} have the same memory
     * as {@code T}; decoding a second JVM carrier would break mutation aliasing.
     */
    private Object transparentManagedView(Class<?> targetClass) {
        if (byteOffset != 0 || viewSize != allocationElementSize) {
            return null;
        }
        Object owner;
        if (allocation instanceof Cell) {
            owner = ((Cell) allocation).value;
        } else if (allocation instanceof ReceiverCell) {
            owner = ((ReceiverCell) allocation).value;
        } else if (allocation instanceof FieldCell) {
            owner = ((FieldCell) allocation).get();
        } else {
            return null;
        }
        if (owner == null || targetClass.isInstance(owner)) {
            return null;
        }
        Object match = null;
        try {
            for (Field field : owner.getClass().getFields()) {
                if (Modifier.isStatic(field.getModifiers())) {
                    continue;
                }
                Object candidate = field.get(owner);
                if (candidate != null && targetClass.isInstance(candidate)) {
                    if (match != null) {
                        return null;
                    }
                    match = candidate;
                }
            }
        } catch (IllegalAccessException error) {
            throw new IllegalStateException("could not inspect transparent Rust wrapper", error);
        }
        return match;
    }

    private String[] structuralViewDescriptor() {
        String descriptor = viewCodecClassName.substring(STRUCTURAL_VIEW_CODEC_PREFIX.length());
        String[] parts = descriptor.split("\n", 3);
        if (parts.length != 3) {
            throw new IllegalStateException("invalid structural Rust view descriptor");
        }
        return parts;
    }

    private String[] structTailViewDescriptor() {
        String descriptor = viewCodecClassName.substring(STRUCT_TAIL_VIEW_CODEC_PREFIX.length());
        String[] parts = descriptor.split("\n", 5);
        if (parts.length != 5) {
            throw new IllegalStateException("invalid Rust struct-tail view descriptor");
        }
        return parts;
    }

    private Object structuralSourceObject(String[] descriptor) {
        int sourceViewSize;
        try {
            sourceViewSize = Integer.parseInt(descriptor[1]);
        } catch (NumberFormatException error) {
            throw new IllegalStateException("invalid structural Rust source view size", error);
        }
        String sourceCodec = descriptor[2].isEmpty() ? null : descriptor[2];
        return new Pointer(
                        allocation,
                        allocationElementSize,
                        byteOffset,
                        sourceViewSize,
                        allocationCodecClassName,
                        sourceCodec,
                        exposedAddress)
                .withMetadata(metadata)
                .getObject();
    }

    private Object structTailSourceObject(String[] descriptor) {
        long prefixSize = structTailPointerPrefixSize(descriptor);
        int elementSize = structTailPointerElementSize(descriptor);
        String elementCodec = descriptor[4].isEmpty() ? null : descriptor[4];
        Pointer data = byte_offset(prefixSize).retype(elementSize, elementCodec);
        try {
            Class<?> viewClass = Class.forName(
                    descriptor[2].replace('/', '.'), true, Pointer.class.getClassLoader());
            return viewClass
                    .getConstructor(Object.class, int.class, int.class)
                    .newInstance(data, 0, Math.toIntExact(metadata()));
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not construct Rust struct-tail source view", error);
        }
    }

    private Object structuralSourceObject() {
        return viewCodecClassName.startsWith(STRUCT_TAIL_VIEW_CODEC_PREFIX)
                ? structTailSourceObject(structTailViewDescriptor())
                : structuralSourceObject(structuralViewDescriptor());
    }

    private String structuralTargetClassName() {
        String name = viewCodecClassName.startsWith(STRUCT_TAIL_VIEW_CODEC_PREFIX)
                ? structTailViewDescriptor()[0]
                : structuralViewDescriptor()[0];
        return name.replace('/', '.');
    }

    private Object structuralViewObject() {
        String targetClassName = structuralTargetClassName();
        Object source = structuralSourceObject();
        try {
            ClassLoader loader = source.getClass().getClassLoader();
            Class<?> targetClass = Class.forName(targetClassName, true, loader);
            return structuralViewState(source, true).activate(targetClass);
        } catch (ClassNotFoundException error) {
            throw new IllegalStateException(
                    "could not load structural Rust view " + targetClassName, error);
        }
    }

    private static long inferredStructuralMetadata(Object value) {
        if (value == null) {
            return -1;
        }
        try {
            if (isSliceViewType(value.getClass())) {
                return Integer.toUnsignedLong(
                        instanceField(value.getClass(), "length").getInt(value));
            }
            for (Field field : value.getClass().getFields()) {
                if (Modifier.isStatic(field.getModifiers()) || field.getType().isPrimitive()) {
                    continue;
                }
                long metadata = inferredStructuralMetadata(field.get(value));
                if (metadata >= 0) {
                    return metadata;
                }
            }
            return -1;
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not inspect Rust structural metadata", error);
        }
    }

    private static final class Cell {
        private Object value;

        private Cell(Object value) {
            this.value = value;
        }
    }

    /**
     * Pointer storage for a JVM instance method's Rust {@code &mut self}.
     * The receiver identity is fixed by the JVM, but Rust may replace the
     * entire value through that reference. Such writes must update the
     * receiver's fields instead of merely replacing a temporary cell.
     */
    private static final class ReceiverCell {
        private final Object value;

        private ReceiverCell(Object value) {
            if (value == null) {
                throw new NullPointerException("Rust instance receiver cannot be null");
            }
            this.value = value;
        }
    }

    private static final class FieldCell {
        private final Object owner;
        private final Field field;

        private FieldCell(Object owner, Field field) {
            this.owner = owner;
            this.field = field;
        }

        private Object get() {
            try {
                return field.get(owner);
            } catch (IllegalAccessException error) {
                throw new IllegalStateException("could not read Rust field pointer", error);
            }
        }

        private void set(Object value) {
            try {
                field.set(owner, value);
            } catch (IllegalAccessException error) {
                throw new IllegalStateException("could not write Rust field pointer", error);
            }
        }
    }

    private static final class ExposedTarget {
        // A JVM GC cannot see reachability through the numeric addresses that
        // Rust stores in raw-pointer fields.  Once an allocation's address is
        // exposed, retain its backing object conservatively so a later
        // pointer decode cannot turn a still-live Rust pointer into a dangling
        // JVM reference merely because no Pointer carrier was live at a GC
        // safepoint.
        private final Object allocation;
        private final int allocationElementSize;
        private final long byteOffset;
        private final String codecClassName;
        private final long viewSize;
        private final String viewCodecClassName;
        private final long metadata;
        private final long zeroSizedSourceViewSize;
        private final String zeroSizedSourceViewCodecClassName;
        private final Object traitObjectCarrier;

        private ExposedTarget(
                Object allocation,
                int allocationElementSize,
                long byteOffset,
                String codecClassName,
                long viewSize,
                String viewCodecClassName,
                long metadata,
                long zeroSizedSourceViewSize,
                String zeroSizedSourceViewCodecClassName,
                Object traitObjectCarrier) {
            this.allocation = allocation;
            this.allocationElementSize = allocationElementSize;
            this.byteOffset = byteOffset;
            this.codecClassName = codecClassName;
            this.viewSize = viewSize;
            this.viewCodecClassName = viewCodecClassName;
            this.metadata = metadata;
            this.zeroSizedSourceViewSize = zeroSizedSourceViewSize;
            this.zeroSizedSourceViewCodecClassName = zeroSizedSourceViewCodecClassName;
            this.traitObjectCarrier = traitObjectCarrier;
        }
    }

    private final Object allocation;
    private final int allocationElementSize;
    private final long byteOffset;
    private final long viewSize;
    private final String allocationCodecClassName;
    private final String viewCodecClassName;
    private final long exposedAddress;
    private MemoryViewState boundMemoryViewState;
    private long erasedAddressToken = -1;
    private long metadata = -1;
    private long zeroSizedSourceViewSize = -1;
    private String zeroSizedSourceViewCodecClassName;
    private Object traitObjectCarrier;

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
            long viewSize,
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
            long viewSize,
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

    public static Pointer cell(Object value, long size, String codecClassName) {
        return cellAligned(value, checkedArrayLength(size), codecClassName, 16);
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

    /** Creates a write-through pointer for a JVM instance method receiver. */
    public static Pointer receiverCellAligned(
            Object value,
            int size,
            String codecClassName,
            int alignment) {
        if (alignment <= 0 || (alignment & (alignment - 1)) != 0) {
            throw new IllegalArgumentException("Rust allocation alignment must be a power of two");
        }
        MemoryViewOrigin origin;
        synchronized (MEMORY_VIEW_ORIGINS) {
            origin = MEMORY_VIEW_ORIGINS.get(value);
        }
        if (origin != null && origin.viewSize == size) {
            Object allocation = origin.allocation.get();
            if (allocation != null) {
                return new Pointer(
                                allocation,
                                origin.allocationElementSize,
                                origin.byteOffset,
                                size,
                                origin.allocationCodecClassName,
                                codecClassName,
                                -1)
                        .withMetadata(origin.metadata);
            }
        }
        ReceiverCell cell = new ReceiverCell(value);
        synchronized (ALLOCATION_BASES) {
            ALLOCATION_ALIGNMENTS.put(cell, alignment);
        }
        return new Pointer(cell, size, 0, size, codecClassName);
    }

    public static Pointer cell(Object value, int size) {
        return cell(value, size, null);
    }

    public static Pointer cell(Object value) {
        Pointer pointer = cell(value, inferredCarrierSize(value));
        long metadata = inferredStructuralMetadata(value);
        return metadata < 0 ? pointer : pointer.withMetadata(metadata);
    }

    /** Returns a stable, write-through pointer to a generated Rust value field. */
    public static Pointer field(
            Object owner, String fieldName, int size, String codecClassName) {
        if (owner == null) {
            throw new NullPointerException("Rust field pointer requires an owner");
        }
        FieldCell cell;
        synchronized (FIELD_CELLS) {
            Map<String, WeakReference<FieldCell>> fields = FIELD_CELLS.computeIfAbsent(
                    owner, ignored -> new HashMap<>());
            WeakReference<FieldCell> reference = fields.get(fieldName);
            cell = reference == null ? null : reference.get();
            if (cell == null) {
                try {
                    cell = new FieldCell(owner, instanceField(owner.getClass(), fieldName));
                } catch (NoSuchFieldException error) {
                    throw new IllegalArgumentException(
                            "unknown Rust field " + owner.getClass().getName() + "." + fieldName,
                            error);
                }
                fields.put(fieldName, new WeakReference<>(cell));
            }
        }
        return new Pointer(cell, size, 0, size, codecClassName);
    }

    public static Pointer field(
            Object owner, String fieldName, long size, String codecClassName) {
        return field(owner, fieldName, checkedArrayLength(size), codecClassName);
    }

    private Object compatibleStructView(String ownerClassName) {
        try {
            Class<?> ownerClass = Class.forName(ownerClassName.replace('/', '.'));
            Object candidate = getObjectAs(ownerClassName);
            return ownerClass.isInstance(candidate) ? candidate : null;
        } catch (ClassNotFoundException error) {
            throw new IllegalArgumentException(
                    "unknown Rust aggregate class " + ownerClassName, error);
        }
    }

    /**
     * Whether projecting a field can safely retain a direct reference to the
     * JVM aggregate carrier. A decoded view of allocator-backed byte storage
     * is only a temporary cache: ordinary memory access may flush and replace
     * it, which would leave a FieldCell pointing at stale storage. Direct JVM
     * values and the cells used for locals/fields have stable identity.
     */
    private boolean hasStableManagedCarrier() {
        return isDirectAllocationView()
                || allocation instanceof Cell
                || allocation instanceof ReceiverCell
                || allocation instanceof FieldCell;
    }

    public Pointer projectStructField(
            String ownerClassName,
            String fieldName,
            int fieldOffset,
            int fieldSize,
            String fieldCodecClassName) {
        return projectStructField(
                ownerClassName,
                fieldName,
                (long) fieldOffset,
                (long) fieldSize,
                fieldCodecClassName);
    }

    public Pointer projectStructField(
            String ownerClassName,
            String fieldName,
            long fieldOffset,
            long fieldSize,
            String fieldCodecClassName) {
        if (hasStableManagedCarrier()) {
            Object owner = compatibleStructView(ownerClassName);
            if (owner != null
                    && (fieldSize == 0
                            || (fieldCodecClassName != null
                                    && !isBuiltInCodec(fieldCodecClassName)))) {
                return field(owner, fieldName, fieldSize, fieldCodecClassName);
            }
        }
        return byte_offset(fieldOffset).retype(fieldSize, fieldCodecClassName);
    }

    public Object projectStructSliceField(
            String ownerClassName,
            String fieldName,
            int fieldOffset,
            int elementSize,
            String elementCodecClassName) {
        return projectStructSliceField(
                ownerClassName,
                fieldName,
                (long) fieldOffset,
                (long) elementSize,
                elementCodecClassName);
    }

    public Object projectStructSliceField(
            String ownerClassName,
            String fieldName,
            long fieldOffset,
            long elementSize,
            String elementCodecClassName) {
        if (hasStableManagedCarrier()) {
            Object owner = compatibleStructView(ownerClassName);
            if (owner != null) {
                try {
                    return instanceField(owner.getClass(), fieldName).get(owner);
                } catch (ReflectiveOperationException error) {
                    throw new IllegalStateException("could not read Rust DST slice field", error);
                }
            }
        }

        Pointer data = byte_offset(fieldOffset).retype(elementSize, elementCodecClassName);
        try {
            Class<?> sliceView = Class.forName(SLICE_VIEW_CLASS_NAME);
            return sliceView
                    .getConstructor(Object.class, int.class, int.class)
                    .newInstance(data, 0, Math.toIntExact(metadata()));
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not construct Rust DST slice view", error);
        }
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

    public static Pointer array(
            Object array, int elementOffset, long elementSize, String codecClassName) {
        return array(array, elementOffset, checkedArrayLength(elementSize), codecClassName);
    }

    public static Pointer array(Object array, int elementOffset) {
        return array(array, elementOffset, inferredArrayElementSize(array));
    }

    public static Pointer allocateBytes(long byteCount, long alignment) {
        try {
            int size = checkedArrayLength(byteCount);
            int checkedAlignment = checkedAlignment(alignment);
            byte[] bytes = new byte[size];
            synchronized (ALLOCATION_BASES) {
                ALLOCATION_ALIGNMENTS.put(bytes, checkedAlignment);
            }
            return new Pointer(bytes, 1, 0, 1, null);
        } catch (IllegalArgumentException | ArithmeticException | OutOfMemoryError failure) {
            // GlobalAlloc reports allocation failure with a null pointer. In
            // particular, Rust's usize range is much larger than a JVM array.
            return null;
        }
    }

    /** Materializes a shared repeated-byte CTFE allocation and returns one view into it. */
    public static Pointer constantRepeatedByte(
            String identity,
            int initialByte,
            long length,
            long offset,
            long viewSize,
            long alignment,
            String viewCodecClassName) {
        int checkedLength = checkedArrayLength(length);
        int checkedOffset = checkedArrayLength(offset);
        int checkedAlignment = checkedAlignment(alignment);
        if (checkedOffset > checkedLength) {
            throw new IndexOutOfBoundsException("constant pointer offset exceeds its allocation");
        }
        byte[] allocation;
        synchronized (CONSTANT_ALLOCATIONS) {
            allocation = CONSTANT_ALLOCATIONS.get(identity);
            if (allocation == null) {
                allocation = new byte[checkedLength];
                Arrays.fill(allocation, (byte) initialByte);
                CONSTANT_ALLOCATIONS.put(identity, allocation);
                synchronized (ALLOCATION_BASES) {
                    ALLOCATION_ALIGNMENTS.put(allocation, checkedAlignment);
                }
            }
        }
        return new Pointer(
                allocation,
                1,
                checkedOffset,
                viewSize,
                null,
                viewCodecClassName,
                -1);
    }

    public static Pointer reallocateBytes(
            Pointer source,
            long oldByteCount,
            long alignment,
            long newByteCount) {
        if (source == null) {
            throw new NullPointerException("Rust realloc requires a non-null pointer");
        }
        try {
            int oldSize = checkedArrayLength(oldByteCount);
            int newSize = checkedArrayLength(newByteCount);
            Pointer destination = allocateBytes(newSize, alignment);
            if (destination == null) {
                return null;
            }
            copy(source, destination, Math.min(oldSize, newSize));
            deallocateBytes(source);
            return destination;
        } catch (IllegalArgumentException | ArithmeticException | OutOfMemoryError failure) {
            return null;
        }
    }

    /** Releases a byte allocation after Rust's allocator has ended its lifetime. */
    public static void deallocateBytes(Pointer pointer) {
        if (pointer == null || pointer.allocation == null) {
            return;
        }
        Object allocation = pointer.allocation;
        synchronized (ALLOCATION_BASES) {
            ALLOCATION_BASES.remove(allocation);
            ALLOCATION_ELEMENT_SIZES.remove(allocation);
            ALLOCATION_ALIGNMENTS.remove(allocation);
            ALLOCATION_CODECS.remove(allocation);
            EXPOSED_ADDRESSES.entrySet().removeIf(
                    entry -> entry.getValue().allocation == allocation);
        }
        synchronized (MEMORY_VIEWS) {
            MEMORY_VIEWS.remove(allocation);
        }
        synchronized (STRUCTURAL_VIEWS) {
            STRUCTURAL_VIEWS.remove(allocation);
        }
    }

    private static int checkedAlignment(long alignment) {
        if (alignment <= 0
                || alignment > Integer.MAX_VALUE
                || (alignment & (alignment - 1L)) != 0) {
            throw new IllegalArgumentException(
                    "Rust allocation alignment must be a positive power of two");
        }
        return (int) alignment;
    }

    public static Pointer fromSlice(
            Object sliceView,
            int elementSize,
            String codecClassName) {
        if (sliceView == null) {
            return nullPointer(elementSize);
        }
        // Optimized MIR can erase a fixed-array reference's SliceView wrapper
        // while retaining its pointer-backed storage. Extracting the data
        // pointer is then already complete; only its element view must change.
        if (sliceView instanceof Pointer) {
            return ((Pointer) sliceView).retype(elementSize, codecClassName);
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

    public static Pointer fromSlice(
            Object sliceView, long elementSize, String codecClassName) {
        if (sliceView == null) {
            return nullPointer(elementSize);
        }
        if (sliceView instanceof Pointer) {
            return ((Pointer) sliceView).retype(elementSize, codecClassName);
        }
        try {
            Field arrayField = sliceView.getClass().getField("array");
            Object backing = arrayField.get(sliceView);
            if (backing instanceof Pointer) {
                Field offsetField = sliceView.getClass().getField("offset");
                Field lengthField = sliceView.getClass().getField("length");
                int offset = offsetField.getInt(sliceView);
                int length = lengthField.getInt(sliceView);
                return ((Pointer) backing)
                        .sliceStorageView()
                        .add(offset)
                        .retype(elementSize, codecClassName)
                        .withMetadata(length);
            }
        } catch (ReflectiveOperationException error) {
            throw new IllegalArgumentException("invalid Rust slice view", error);
        }
        return fromSlice(sliceView, checkedArrayLength(elementSize), codecClassName);
    }

    public static Pointer fromSlice(Object sliceView, long elementSize) {
        return fromSlice(sliceView, elementSize, null);
    }

    public static Pointer fromSlice(Object sliceView) {
        if (sliceView == null) {
            return nullPointer();
        }
        try {
            Field arrayField = sliceView.getClass().getField("array");
            Object array = arrayField.get(sliceView);
            if (array instanceof Pointer) {
                Pointer pointer = (Pointer) array;
                return fromSlice(
                        sliceView, pointer.viewSize, pointer.viewCodecClassName);
            }
            return fromSlice(sliceView, inferredArrayElementSize(array));
        } catch (ReflectiveOperationException error) {
            throw new IllegalArgumentException("invalid Rust slice view", error);
        }
    }

    public static Pointer nullPointer(int viewSize) {
        return nullPointer((long) viewSize);
    }

    public static Pointer nullPointer(long viewSize) {
        return new Pointer(null, 1, 0, viewSize, null, null, 0);
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
        return fromAddress(address, (long) viewSize, null);
    }

    public static Pointer fromAddress(long address, long viewSize) {
        return fromAddress(address, viewSize, null);
    }

    public static Pointer fromAddress(int address, int viewSize) {
        return fromAddress(Integer.toUnsignedLong(address), viewSize);
    }

    public static Pointer fromAddress(long address, int viewSize, String viewCodecClassName) {
        return fromAddress(address, (long) viewSize, viewCodecClassName);
    }

    public static Pointer fromAddress(long address, long viewSize, String viewCodecClassName) {
        synchronized (ALLOCATION_BASES) {
            ExposedTarget target = EXPOSED_ADDRESSES.get(address);
            if (target != null) {
                Object allocation = target.allocation;
                if (allocation != null) {
                    Pointer pointer = new Pointer(
                            allocation,
                            target.allocationElementSize,
                            target.byteOffset,
                            viewSize,
                            target.codecClassName,
                            viewCodecClassName,
                            -1).withMetadata(target.metadata);
                    if (viewSize == 0 && target.zeroSizedSourceViewSize >= 0) {
                        pointer.zeroSizedSourceViewSize = target.zeroSizedSourceViewSize;
                        pointer.zeroSizedSourceViewCodecClassName =
                                target.zeroSizedSourceViewCodecClassName;
                    }
                    pointer.traitObjectCarrier = target.traitObjectCarrier;
                    return pointer;
                }
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
        Pointer exposed = new Pointer(null, 1, 0, viewSize, null, null, address);
        if (address != 0 && viewSize == 0 && viewCodecClassName != null) {
            // A Box/NonNull to a ZST commonly uses an aligned dangling
            // address. Give the typed view stable runtime identity so later
            // erasure can recover its nominal codec from that address.
            return exposed.retype(0, viewCodecClassName);
        }
        return new Pointer(null, 1, 0, viewSize, null, viewCodecClassName, address);
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
                Object allocation = target.allocation;
                if (allocation != null) {
                    Pointer pointer = new Pointer(
                            allocation,
                            target.allocationElementSize,
                            target.byteOffset,
                            target.viewSize,
                            target.codecClassName,
                            target.viewCodecClassName,
                            -1).withMetadata(target.metadata);
                    pointer.zeroSizedSourceViewSize = target.zeroSizedSourceViewSize;
                    pointer.zeroSizedSourceViewCodecClassName =
                            target.zeroSizedSourceViewCodecClassName;
                    pointer.traitObjectCarrier = target.traitObjectCarrier;
                    return pointer;
                }
            }
        }
        return fromAddress(address, 1);
    }

    public static Pointer fromErasedAddress(long address) {
        return pointerObjectFromAddress(address).retype(0);
    }

    public static Pointer fromAddress(long address) {
        return fromAddress(address, 1);
    }

    public static Pointer withoutProvenance(long address, int viewSize) {
        return withoutProvenance(address, (long) viewSize, null);
    }

    public static Pointer withoutProvenance(long address, long viewSize) {
        return withoutProvenance(address, viewSize, null);
    }

    public static Pointer withoutProvenance(int address, int viewSize) {
        return withoutProvenance(Integer.toUnsignedLong(address), viewSize);
    }

    public static Pointer withoutProvenance(
            long address, int viewSize, String viewCodecClassName) {
        return withoutProvenance(address, (long) viewSize, viewCodecClassName);
    }

    public static Pointer withoutProvenance(
            long address, long viewSize, String viewCodecClassName) {
        return new Pointer(null, 1, 0, viewSize, null, viewCodecClassName, address);
    }

    public static Pointer withoutProvenance(
            int address, int viewSize, String viewCodecClassName) {
        return withoutProvenance(Integer.toUnsignedLong(address), viewSize, viewCodecClassName);
    }

    public Pointer retype(int newViewSize) {
        return retype((long) newViewSize, null);
    }

    public Pointer retype(int newViewSize, String newViewCodecClassName) {
        return retype((long) newViewSize, newViewCodecClassName);
    }

    public Pointer retype(long newViewSize) {
        return retype(newViewSize, null);
    }

    public Pointer retype(long newViewSize, String newViewCodecClassName) {
        Object resultAllocation = allocation;
        int resultAllocationElementSize = allocationElementSize;
        String resultAllocationCodecClassName = allocationCodecClassName;
        long resultExposedAddress = exposedAddress;
        if (allocation == null
                && exposedAddress != 0
                && newViewSize == 0
                && newViewCodecClassName != null) {
            // Rust represents an allocated ZST with an aligned dangling
            // address. That is enough natively, but after erasure the JVM also
            // needs the nominal codec in order to reconstruct the concrete
            // callable/aggregate class. Give only this typed ZST view a
            // zero-byte backing so address round-trips retain that provenance.
            Cell zeroSizedBacking = new Cell(null);
            long inferredAlignment = Long.lowestOneBit(exposedAddress);
            int alignment = inferredAlignment <= 0
                    ? 1
                    : (int) Math.min(inferredAlignment, 1L << 30);
            synchronized (ALLOCATION_BASES) {
                ALLOCATION_ALIGNMENTS.put(zeroSizedBacking, alignment);
                // Preserve the Rust-visible dangling address. Multiple ZST
                // identities may legitimately share it; the exact exposed
                // target is refreshed whenever a pointer is published.
                ALLOCATION_BASES.put(
                        zeroSizedBacking, Math.subtractExact(exposedAddress, byteOffset));
                ALLOCATION_ELEMENT_SIZES.put(zeroSizedBacking, 0);
                ALLOCATION_CODECS.put(zeroSizedBacking, newViewCodecClassName);
            }
            resultAllocation = zeroSizedBacking;
            resultAllocationElementSize = 0;
            resultAllocationCodecClassName = newViewCodecClassName;
            resultExposedAddress = -1;
        }
        Pointer result = new Pointer(
                resultAllocation,
                resultAllocationElementSize,
                byteOffset,
                newViewSize,
                resultAllocationCodecClassName,
                newViewCodecClassName,
                resultExposedAddress).withMetadata(metadata);
        result.traitObjectCarrier = traitObjectCarrier;
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

    public static Pointer retype(Pointer pointer, long newViewSize) {
        return pointer.retype(newViewSize);
    }

    public static Pointer retype(
            Pointer pointer, int newViewSize, String newViewCodecClassName) {
        return pointer.retype(newViewSize, newViewCodecClassName);
    }

    public static Pointer attachTraitObjectCarrier(Pointer pointer, Object carrier) {
        if (pointer == null || carrier == null) {
            throw new NullPointerException("trait-object pointer and carrier must be non-null");
        }
        pointer.traitObjectCarrier = carrier;
        return pointer;
    }

    public static Pointer attachPointeeTraitObjectCarrier(Pointer pointer) {
        if (pointer == null) {
            throw new NullPointerException("trait-object pointer must be non-null");
        }
        Object carrier = pointer.getObject();
        if (carrier == null) {
            throw new NullPointerException("trait-object pointee must be non-null");
        }
        pointer.traitObjectCarrier = carrier;
        return pointer;
    }

    public static Pointer retype(
            Pointer pointer, long newViewSize, String newViewCodecClassName) {
        return pointer.retype(newViewSize, newViewCodecClassName);
    }

    public static Pointer retypeWithMetadata(
            Pointer pointer,
            int newViewSize,
            String newViewCodecClassName,
            long metadata) {
        return pointer.retype(newViewSize, newViewCodecClassName).withMetadata(metadata);
    }

    public static Pointer retypeWithMetadata(
            Pointer pointer,
            long newViewSize,
            String newViewCodecClassName,
            long metadata) {
        return pointer.retype(newViewSize, newViewCodecClassName).withMetadata(metadata);
    }

    /** Creates a coherent JVM carrier view for a Rust struct-tail unsizing coercion. */
    public static Pointer unsizeStruct(
            Pointer pointer, int newViewSize, String targetClassName) {
        return unsizeStruct(pointer, (long) newViewSize, targetClassName);
    }

    public static Pointer unsizeStruct(
            Pointer pointer, long newViewSize, String targetClassName) {
        if (pointer == null) {
            throw new NullPointerException("cannot unsize a null Pointer carrier");
        }
        if (targetClassName == null || targetClassName.isEmpty()) {
            throw new IllegalArgumentException("struct-tail view requires a target class");
        }
        String sourceCodec = pointer.viewCodecClassName == null
                ? ""
                : pointer.viewCodecClassName;
        return pointer.retype(
                newViewSize,
                STRUCTURAL_VIEW_CODEC_PREFIX
                        + targetClassName + "\n" + pointer.viewSize + "\n" + sourceCodec);
    }

    public static Pointer restoreAllocationView(Pointer pointer) {
        Pointer result = new Pointer(
                pointer.allocation,
                pointer.allocationElementSize,
                pointer.byteOffset,
                pointer.allocationElementSize,
                pointer.allocationCodecClassName,
                pointer.allocationCodecClassName,
                pointer.exposedAddress).withMetadata(pointer.metadata);
        result.traitObjectCarrier = pointer.traitObjectCarrier;
        return result;
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

    /** Computes the dynamic layout size of a slice/str or a slice-tailed DST. */
    public static long sizeOfSliceTailed(
            Object value, long prefixSize, long elementSize, long alignment) {
        if (prefixSize < 0 || elementSize < 0
                || alignment <= 0 || (alignment & (alignment - 1)) != 0) {
            throw new IllegalArgumentException("invalid Rust dynamically sized layout");
        }
        long length;
        if (value instanceof Pointer) {
            length = ((Pointer) value).metadata();
        } else if (value != null && isSliceViewCarrierType(value.getClass())) {
            try {
                length = Integer.toUnsignedLong(
                        instanceField(value.getClass(), "length").getInt(value));
            } catch (ReflectiveOperationException error) {
                throw new IllegalArgumentException("invalid Rust slice fat pointer", error);
            }
        } else {
            throw new IllegalArgumentException("Rust DST value does not carry slice metadata");
        }
        long unalignedSize = Math.addExact(prefixSize, Math.multiplyExact(length, elementSize));
        return Math.addExact(unalignedSize, alignment - 1) & -alignment;
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
        return address(pointer);
    }

    public long expose_provenance() {
        return address();
    }

    public static long expose_provenance(Pointer pointer) {
        return address(pointer);
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

    /** Compares both words of a Rust slice/str fat pointer. */
    public static boolean fatPointerEquals(Object left, Object right) {
        if (left == right) {
            return true;
        }
        if (left == null || right == null
                || !isSliceViewCarrierType(left.getClass())
                || !isSliceViewCarrierType(right.getClass())) {
            return false;
        }
        try {
            int leftLength = instanceField(left.getClass(), "length").getInt(left);
            int rightLength = instanceField(right.getClass(), "length").getInt(right);
            return leftLength == rightLength && fromSlice(left).sameAddress(fromSlice(right));
        } catch (ReflectiveOperationException error) {
            throw new IllegalArgumentException("invalid Rust fat pointer", error);
        }
    }

    /** Compares only the data-address word of a Rust slice/str fat pointer. */
    public static boolean fatPointerSameAddress(Object left, Object right) {
        if (left == right) {
            return true;
        }
        if (left == null || right == null
                || !isSliceViewCarrierType(left.getClass())
                || !isSliceViewCarrierType(right.getClass())) {
            return false;
        }
        return fromSlice(left).sameAddress(fromSlice(right));
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

    /** Implements the compiler's byte-wise comparison intrinsic. */
    public static int compareBytes(Pointer left, Pointer right, long length) {
        for (long index = 0; index < length; index++) {
            int leftByte = left.loadByte(Math.addExact(left.byteOffset, index)) & 0xff;
            int rightByte = right.loadByte(Math.addExact(right.byteOffset, index)) & 0xff;
            if (leftByte != rightByte) {
                return leftByte - rightByte;
            }
        }
        return 0;
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
        return (address(pointer) & (alignment - 1)) == 0;
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
                            metadata,
                            zeroSizedSourceViewSize,
                            zeroSizedSourceViewCodecClassName,
                            traitObjectCarrier));
            return address;
        }
    }

    /** Returns a Rust pointer's address, including Java {@code null} as address zero. */
    public static long address(Pointer pointer) {
        return pointer == null ? 0L : pointer.address();
    }

    /**
     * Publishes an opaque token for an erased trait-object data pointer.
     * Native fat pointers carry concrete dispatch identity in their metadata;
     * the JVM's erased interface carrier does not. A token therefore retains
     * the complete pointer view even when several ZST values share the same
     * ordinary Rust data address.
     */
    public static long erasedAddress(Pointer pointer) {
        if (pointer == null) {
            return 0L;
        }
        synchronized (pointer) {
            if (pointer.erasedAddressToken >= 0) {
                return pointer.erasedAddressToken;
            }
            pointer.address();
            synchronized (ALLOCATION_BASES) {
                long token = allocateAddress(16L, 16);
                EXPOSED_ADDRESSES.put(
                        token,
                        new ExposedTarget(
                                pointer.allocation,
                                pointer.allocationElementSize,
                                pointer.byteOffset,
                                pointer.allocationCodecClassName,
                                pointer.viewSize,
                                pointer.viewCodecClassName,
                                pointer.metadata,
                                pointer.zeroSizedSourceViewSize,
                                pointer.zeroSizedSourceViewCodecClassName,
                                pointer.traitObjectCarrier));
                pointer.erasedAddressToken = token;
                return token;
            }
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

    private Object readElement(int elementIndex) {
        if (allocation instanceof Cell || allocation instanceof ReceiverCell) {
            if (elementIndex != 0) {
                throw new IndexOutOfBoundsException(
                        "pointer arithmetic escaped scalar storage: element=" + elementIndex
                                + ", byte_offset=" + byteOffset
                                + ", allocation_element_size=" + allocationElementSize
                                + ", view_size=" + viewSize
                                + ", allocation_codec=" + allocationCodecClassName
                                + ", view_codec=" + viewCodecClassName);
            }
            return allocation instanceof Cell
                    ? ((Cell) allocation).value
                    : ((ReceiverCell) allocation).value;
        }
        if (allocation instanceof FieldCell) {
            if (elementIndex != 0) {
                throw new IndexOutOfBoundsException("pointer arithmetic escaped field storage");
            }
            return ((FieldCell) allocation).get();
        }
        return Array.get(allocation, elementIndex);
    }

    private Object readAlignedElement() {
        if (allocation == null) {
            throw new NullPointerException("attempted to dereference a null Rust pointer");
        }
        flushMemoryViewsOverlapping(
                byteOffset, Math.max(1, allocationElementSize));
        if (allocationElementSize == 0) {
            if (allocation instanceof Cell
                    || allocation instanceof ReceiverCell
                    || allocation instanceof FieldCell) {
                if (allocation instanceof Cell) {
                    return ((Cell) allocation).value;
                }
                return allocation instanceof ReceiverCell
                        ? ((ReceiverCell) allocation).value
                        : ((FieldCell) allocation).get();
            }
            return allocation.getClass().isArray() && Array.getLength(allocation) != 0
                    ? Array.get(allocation, 0)
                    : null;
        }
        if (byteOffset % allocationElementSize != 0) {
            throw new IllegalStateException("object dereference is not aligned to its allocation element");
        }
        int elementIndex = Math.toIntExact(byteOffset / allocationElementSize);
        return readElement(elementIndex);
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
        flushMemoryViewsOverlapping(absoluteByteOffset, 1);
        if (allocationElementSize == 0) {
            throw new IndexOutOfBoundsException("zero-sized storage has no addressable bytes");
        }
        int elementIndex = Math.toIntExact(Math.floorDiv(absoluteByteOffset, allocationElementSize));
        int withinElement = (int) Math.floorMod(absoluteByteOffset, allocationElementSize);
        Object value;
        value = readElement(elementIndex);
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
        if (isFatPointerCodec(allocationCodecClassName)) {
            byte[] bytes = encodeFatPointer(
                    value, allocationElementSize, allocationCodecClassName);
            return bytes[withinElement] & 0xff;
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
        flushMemoryViewsOverlapping(absoluteByteOffset, 1);
        if (allocationElementSize == 0) {
            throw new IndexOutOfBoundsException("zero-sized storage has no addressable bytes");
        }
        int elementIndex = Math.toIntExact(Math.floorDiv(absoluteByteOffset, allocationElementSize));
        int withinElement = (int) Math.floorMod(absoluteByteOffset, allocationElementSize);
        Object current;
        current = readElement(elementIndex);
        if (current instanceof BigInteger) {
            BigInteger updated = replaceBigIntegerByte(
                    (BigInteger) current,
                    withinElement,
                    value,
                    allocationElementSize,
                    SIGNED_BIG_INTEGER_CODEC.equals(allocationCodecClassName));
            writeElementPreservingIdentity(elementIndex, updated);
            return;
        }
        if (current instanceof I128) {
            writeElementPreservingIdentity(
                    elementIndex, ((I128) current).withByte(withinElement, value));
            return;
        }
        if (current instanceof U128) {
            writeElementPreservingIdentity(
                    elementIndex, ((U128) current).withByte(withinElement, value));
            return;
        }
        if (current instanceof F128) {
            BigInteger updated = replaceBigIntegerByte(
                    ((F128) current).toBits(),
                    withinElement,
                    value,
                    allocationElementSize,
                    false);
            writeElementPreservingIdentity(elementIndex, F128.fromBits(updated));
            return;
        }
        if (isFatPointerCodec(allocationCodecClassName)) {
            byte[] bytes = encodeFatPointer(
                    current, allocationElementSize, allocationCodecClassName);
            bytes[withinElement] = (byte) value;
            writeElementPreservingIdentity(
                    elementIndex,
                    decodeFatPointer(bytes, 0, allocationElementSize, allocationCodecClassName));
            return;
        }
        if (allocationCodecClassName != null
                && !RAW_POINTER_VIEW_CODEC.equals(allocationCodecClassName)) {
            byte[] bytes = encodeAggregate(allocationCodecClassName, current);
            if (withinElement >= bytes.length) {
                throw new IndexOutOfBoundsException("aggregate codec returned a short memory image");
            }
            bytes[withinElement] = (byte) value;
            writeElementPreservingIdentity(
                    elementIndex, decodeAggregate(allocationCodecClassName, bytes));
            return;
        }
        long bits = valueBits(current, allocationElementSize);
        long mask = 0xffL << (withinElement * 8);
        bits = (bits & ~mask) | (((long) value & 0xffL) << (withinElement * 8));
        writeElementPreservingIdentity(
                elementIndex, carrierFromBits(current, bits, allocationElementSize));
    }

    private static int checkedAtomicByteCount(int byteCount) {
        if (byteCount != 1 && byteCount != 2 && byteCount != 4 && byteCount != 8) {
            throw new IllegalArgumentException(
                    "Rust atomic scalar must occupy 1, 2, 4, or 8 bytes, found " + byteCount);
        }
        return byteCount;
    }

    private static int checkedAtomicOrdering(int ordering) {
        if (ordering < ATOMIC_RELAXED || ordering > ATOMIC_SEQ_CST) {
            throw new IllegalArgumentException("unknown Rust atomic ordering " + ordering);
        }
        return ordering;
    }

    private static Object[] createAtomicStripes() {
        Object[] stripes = new Object[ATOMIC_STRIPE_COUNT];
        for (int index = 0; index < stripes.length; index++) {
            stripes[index] = new Object();
        }
        return stripes;
    }

    /** Maps aliases of a Rust atomic location to the same striped monitor. */
    private static Object atomicStripe(Pointer pointer) {
        return ATOMIC_STRIPES[atomicStripeIndex(pointer)];
    }

    /** Shared monitor used by JVM futex wait/wake and Rust atomic operations. */
    static Object atomicMonitor(Pointer pointer) {
        if (pointer == null) {
            throw new NullPointerException("a futex address cannot be null");
        }
        return atomicStripe(pointer);
    }

    private static int atomicStripeIndex(Pointer pointer) {
        Object identity = pointer.allocation;
        int memberHash = 0;
        if (identity instanceof ReceiverCell) {
            identity = ((ReceiverCell) identity).value;
        } else if (identity instanceof FieldCell) {
            FieldCell cell = (FieldCell) identity;
            identity = cell.owner;
            memberHash = cell.field.getName().hashCode();
        }
        long key = identity == null
                ? pointer.exposedAddress
                : ((long) System.identityHashCode(identity) << 32)
                        ^ ((long) memberHash << 1)
                        ^ pointer.byteOffset;
        key ^= key >>> 33;
        key *= 0xff51afd7ed558ccdL;
        key ^= key >>> 33;
        key *= 0xc4ceb9fe1a85ec53L;
        key ^= key >>> 33;
        return ((int) key) & (ATOMIC_STRIPES.length - 1);
    }

    private static boolean isSequentiallyConsistent(int ordering) {
        return checkedAtomicOrdering(ordering) == ATOMIC_SEQ_CST;
    }

    /**
     * A full portable Java 8 fence. Taking every stripe in a stable order
     * connects the fence to atomic activity on every represented Rust
     * location while avoiding deadlock between concurrent fences.
     */
    private static void atomicFenceStripes(int index) {
        if (index == ATOMIC_STRIPES.length) {
            return;
        }
        synchronized (ATOMIC_STRIPES[index]) {
            atomicFenceStripes(index + 1);
        }
    }

    private static long atomicMask(int byteCount) {
        return byteCount == 8 ? -1L : (1L << (byteCount * 8)) - 1L;
    }

    private static long truncateAtomic(long value, int byteCount) {
        return value & atomicMask(byteCount);
    }

    private static long signExtendAtomic(long value, int byteCount) {
        int shift = 64 - byteCount * 8;
        return (value << shift) >> shift;
    }

    private static void atomicStoreLocked(Pointer pointer, long value, int byteCount) {
        pointer.prepareMemoryWrite(pointer.byteOffset, byteCount);
        pointer.storeBytes(truncateAtomic(value, byteCount), byteCount);
    }

    private static long atomicLoadStriped(Pointer pointer, int byteCount) {
        synchronized (atomicStripe(pointer)) {
            return truncateAtomic(pointer.loadUnsigned(byteCount), byteCount);
        }
    }

    public static long atomicLoad(Pointer pointer, int byteCount, int ordering) {
        checkedAtomicByteCount(byteCount);
        if (isSequentiallyConsistent(ordering)) {
            synchronized (ATOMIC_SEQUENCE_LOCK) {
                return atomicLoadStriped(pointer, byteCount);
            }
        }
        return atomicLoadStriped(pointer, byteCount);
    }

    private static void atomicStoreStriped(Pointer pointer, long value, int byteCount) {
        synchronized (atomicStripe(pointer)) {
            atomicStoreLocked(pointer, value, byteCount);
        }
    }

    public static void atomicStore(Pointer pointer, long value, int byteCount, int ordering) {
        checkedAtomicByteCount(byteCount);
        if (isSequentiallyConsistent(ordering)) {
            synchronized (ATOMIC_SEQUENCE_LOCK) {
                atomicStoreStriped(pointer, value, byteCount);
            }
            return;
        }
        atomicStoreStriped(pointer, value, byteCount);
    }

    private static long atomicRmwStriped(
            Pointer pointer, long operand, int byteCount, int operation) {
        synchronized (atomicStripe(pointer)) {
            long oldValue = truncateAtomic(pointer.loadUnsigned(byteCount), byteCount);
            long right = truncateAtomic(operand, byteCount);
            long newValue;
            switch (operation) {
                case 0:
                    newValue = right;
                    break;
                case 1:
                    newValue = oldValue + right;
                    break;
                case 2:
                    newValue = oldValue - right;
                    break;
                case 3:
                    newValue = oldValue & right;
                    break;
                case 4:
                    newValue = ~(oldValue & right);
                    break;
                case 5:
                    newValue = oldValue | right;
                    break;
                case 6:
                    newValue = oldValue ^ right;
                    break;
                case 7:
                    newValue = signExtendAtomic(oldValue, byteCount)
                                    >= signExtendAtomic(right, byteCount)
                            ? oldValue
                            : right;
                    break;
                case 8:
                    newValue = signExtendAtomic(oldValue, byteCount)
                                    <= signExtendAtomic(right, byteCount)
                            ? oldValue
                            : right;
                    break;
                case 9:
                    newValue = Long.compareUnsigned(oldValue, right) >= 0 ? oldValue : right;
                    break;
                case 10:
                    newValue = Long.compareUnsigned(oldValue, right) <= 0 ? oldValue : right;
                    break;
                default:
                    throw new IllegalArgumentException("unknown Rust atomic operation " + operation);
            }
            atomicStoreLocked(pointer, newValue, byteCount);
            return oldValue;
        }
    }

    private static long atomicRmw(
            Pointer pointer, long operand, int byteCount, int operation, int ordering) {
        checkedAtomicByteCount(byteCount);
        if (isSequentiallyConsistent(ordering)) {
            synchronized (ATOMIC_SEQUENCE_LOCK) {
                return atomicRmwStriped(pointer, operand, byteCount, operation);
            }
        }
        return atomicRmwStriped(pointer, operand, byteCount, operation);
    }

    public static long atomicExchange(
            Pointer pointer, long value, int byteCount, int ordering) {
        return atomicRmw(pointer, value, byteCount, 0, ordering);
    }

    public static long atomicAdd(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicRmw(pointer, value, byteCount, 1, ordering);
    }

    public static long atomicSubtract(
            Pointer pointer, long value, int byteCount, int ordering) {
        return atomicRmw(pointer, value, byteCount, 2, ordering);
    }

    public static long atomicAnd(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicRmw(pointer, value, byteCount, 3, ordering);
    }

    public static long atomicNand(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicRmw(pointer, value, byteCount, 4, ordering);
    }

    public static long atomicOr(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicRmw(pointer, value, byteCount, 5, ordering);
    }

    public static long atomicXor(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicRmw(pointer, value, byteCount, 6, ordering);
    }

    public static long atomicMax(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicRmw(pointer, value, byteCount, 7, ordering);
    }

    public static long atomicMin(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicRmw(pointer, value, byteCount, 8, ordering);
    }

    public static long atomicUnsignedMax(
            Pointer pointer, long value, int byteCount, int ordering) {
        return atomicRmw(pointer, value, byteCount, 9, ordering);
    }

    public static long atomicUnsignedMin(
            Pointer pointer, long value, int byteCount, int ordering) {
        return atomicRmw(pointer, value, byteCount, 10, ordering);
    }

    private static long atomicCompareExchangeStriped(
            Pointer pointer, long expected, long value, int byteCount) {
        synchronized (atomicStripe(pointer)) {
            long oldValue = truncateAtomic(pointer.loadUnsigned(byteCount), byteCount);
            if (oldValue == truncateAtomic(expected, byteCount)) {
                atomicStoreLocked(pointer, value, byteCount);
            }
            return oldValue;
        }
    }

    public static long atomicCompareExchange(
            Pointer pointer,
            long expected,
            long value,
            int byteCount,
            int successOrdering,
            int failureOrdering) {
        checkedAtomicByteCount(byteCount);
        boolean sequentiallyConsistent = isSequentiallyConsistent(successOrdering)
                | isSequentiallyConsistent(failureOrdering);
        if (sequentiallyConsistent) {
            synchronized (ATOMIC_SEQUENCE_LOCK) {
                return atomicCompareExchangeStriped(pointer, expected, value, byteCount);
            }
        }
        return atomicCompareExchangeStriped(pointer, expected, value, byteCount);
    }

    public static void atomicFence(int ordering) {
        if (isSequentiallyConsistent(ordering)) {
            synchronized (ATOMIC_SEQUENCE_LOCK) {
                atomicFenceStripes(0);
            }
        } else {
            atomicFenceStripes(0);
        }
    }

    private void requireScalarViewSize(int expectedSize, String scalarType) {
        if (viewSize == expectedSize) {
            return;
        }
        String sourceView = zeroSizedSourceViewSize >= 0
                ? "; recorded erased source view is " + zeroSizedSourceViewSize + " bytes"
                : "";
        throw new IllegalStateException(
                scalarType + " load requires a " + expectedSize + "-byte view, but pointer has a "
                        + viewSize + "-byte view" + sourceView);
    }

    public boolean getBoolean() {
        requireScalarViewSize(1, "bool");
        return loadUnsigned(1) != 0;
    }

    public byte getI8() {
        requireScalarViewSize(1, "i8/u8");
        return (byte) loadUnsigned(1);
    }

    public short getI16() {
        requireScalarViewSize(2, "i16/u16/f16");
        return (short) loadUnsigned(2);
    }

    public int getI32() {
        requireScalarViewSize(4, "i32/u32/char");
        return (int) loadUnsigned(4);
    }

    public long getI64() {
        requireScalarViewSize(8, "i64/u64");
        return loadUnsigned(8);
    }

    public float getF32() {
        requireScalarViewSize(4, "f32");
        return Float.intBitsToFloat((int) loadUnsigned(4));
    }

    public double getF64() {
        requireScalarViewSize(8, "f64");
        return Double.longBitsToDouble(loadUnsigned(8));
    }

    public Object getObject() {
        if (traitObjectCarrier != null) {
            return traitObjectCarrier;
        }
        Object direct = directCellValueOrSelf();
        if (direct != this && direct instanceof TraitObjectCarrier) {
            return direct;
        }
        if (viewSize == 0 && zeroSizedSourceViewSize >= 0) {
            Pointer pointer = new Pointer(
                            allocation,
                            allocationElementSize,
                            byteOffset,
                            zeroSizedSourceViewSize,
                            allocationCodecClassName,
                            zeroSizedSourceViewCodecClassName,
                            exposedAddress).withMetadata(metadata);
            pointer.traitObjectCarrier = traitObjectCarrier;
            return pointer.getObject();
        }
        if (MANAGED_OBJECT_VIEW_CODEC.equals(viewCodecClassName)
                && !isDirectAllocationView()) {
            return managedObjectFromAddress(loadUnsigned((int) Math.min(viewSize, 8)));
        }
        if (RAW_POINTER_VIEW_CODEC.equals(viewCodecClassName)
                && !isDirectAllocationView()) {
            return pointerObjectFromAddress(loadUnsigned((int) Math.min(viewSize, 8)));
        }
        if (isBigIntegerCodec(viewCodecClassName) && !isDirectAllocationView()) {
            BigInteger bits = bigIntegerFromPointerBytes(materializedViewSize(), false);
            return SIGNED_BIG_INTEGER_CODEC.equals(viewCodecClassName)
                    ? I128.fromBigInteger(bits)
                    : U128.fromBigInteger(bits);
        }
        if (F128_CODEC.equals(viewCodecClassName) && !isDirectAllocationView()) {
            return F128.fromBits(bigIntegerFromPointerBytes(materializedViewSize(), false));
        }
        if (isFatPointerCodec(viewCodecClassName) && !isDirectAllocationView()) {
            int materializedSize = materializedViewSize();
            byte[] image = new byte[materializedSize];
            for (int index = 0; index < materializedSize; index++) {
                image[index] = (byte) loadByte(byteOffset + index);
            }
            return decodeFatPointer(image, 0, materializedSize, viewCodecClassName);
        }
        if (isStructuralViewCodec(viewCodecClassName)) {
            return structuralViewObject();
        }
        if (viewCodecClassName != null && !isDirectAllocationView()) {
            return decodedMemoryView();
        }
        Object value = readAlignedElement();
        StructuralViewState state = structuralViewState(value, false);
        return state == null ? value : state.activate(value.getClass());
    }

    public Object receiverObject() {
        Object receiver = getObject();
        while (receiver instanceof Pointer) {
            Object next = ((Pointer) receiver).getObject();
            if (next == receiver) {
                throw new IllegalStateException("cyclic Rust receiver pointer");
            }
            receiver = next;
        }
        return receiver;
    }

    Object directCellValueOrSelf() {
        if (byteOffset == 0 && allocation instanceof Cell) {
            return ((Cell) allocation).value;
        }
        if (byteOffset == 0 && allocation instanceof ReceiverCell) {
            return ((ReceiverCell) allocation).value;
        }
        if (byteOffset == 0 && allocation instanceof FieldCell) {
            return ((FieldCell) allocation).get();
        }
        return this;
    }

    public Object getObjectAs(String targetClassName) {
        if (targetClassName == null || targetClassName.isEmpty()) {
            return getObject();
        }
        Object value;
        StructuralViewState state;
        if (isStructuralViewCodec(viewCodecClassName)) {
            value = structuralSourceObject();
            state = structuralViewState(value, true);
        } else {
            value = getObject();
            if (value == null) {
                return null;
            }
            state = structuralViewState(value, false);
            if (state == null) {
                return value;
            }
        }
        try {
            Class<?> targetClass = Class.forName(
                    targetClassName.replace('/', '.'), true, value.getClass().getClassLoader());
            return state.activate(targetClass);
        } catch (ClassNotFoundException error) {
            throw new IllegalStateException(
                    "could not load requested Rust structural view " + targetClassName, error);
        }
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
        flushAllMemoryViews();
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

    // A decoded view assigned back to its source already aliases that storage.
    // Flush it with its own codec instead of reinterpreting its JVM carrier.
    private boolean flushSameOriginMemoryView(Object value, int size) {
        MemoryViewOrigin origin;
        synchronized (MEMORY_VIEW_ORIGINS) {
            origin = MEMORY_VIEW_ORIGINS.get(value);
        }
        if (origin == null
                || origin.allocation.get() != allocation
                || origin.byteOffset != byteOffset
                || origin.viewSize != size) {
            return false;
        }
        flushMemoryViewsOverlapping(byteOffset, size);
        return true;
    }

    public void set(Object value) {
        if (viewSize == 0 || allocationElementSize == 0) {
            // Every pointer to an element of zero-sized storage has the same
            // address, and writing a ZST changes no Rust-observable bytes.
            // Slice bounds are checked before reaching this raw write, so an
            // array- or cell-backed ZST store is correctly represented as a
            // no-op. In particular, transparent wrappers need not have the
            // same JVM carrier class to represent the same empty Rust bytes.
            return;
        }

        if (allocation == null) {
            throw new NullPointerException("attempted to write through a null Rust pointer");
        }

        int materializedSize = materializedViewSize();
        if (flushSameOriginMemoryView(value, materializedSize)) {
            return;
        }
        prepareMemoryWrite(byteOffset, materializedSize);

        if (isStructuralViewCodec(viewCodecClassName)) {
            Object target = structuralViewObject();
            overwriteManagedObject(target, value);
            return;
        }

        if (isDirectAllocationView()) {
            clearStructuralViewState();
            int elementIndex = Math.toIntExact(byteOffset / allocationElementSize);
            Object current = readElement(elementIndex);
            writeElement(
                    elementIndex,
                    convertDirectValue(current, value, allocationElementSize));
            return;
        }

        if (viewCodecClassName != null) {
            if (isFatPointerCodec(viewCodecClassName)) {
                storeRange(encodeFatPointer(value, materializedSize, viewCodecClassName));
                return;
            }
            if (MANAGED_OBJECT_VIEW_CODEC.equals(viewCodecClassName)) {
                long address = managedObjectAddress(value);
                byte[] image = new byte[materializedSize];
                for (int index = 0; index < Math.min(materializedSize, 8); index++) {
                    image[index] = (byte) (address >>> (index * 8));
                }
                storeRange(image);
                return;
            }
            if (RAW_POINTER_VIEW_CODEC.equals(viewCodecClassName)) {
                long address = value == null
                        ? 0L
                        : ((Pointer) value).address();
                byte[] image = new byte[materializedSize];
                for (int index = 0; index < Math.min(materializedSize, 8); index++) {
                    image[index] = (byte) (address >>> (index * 8));
                }
                storeRange(image);
                return;
            }
            if (isBigIntegerCodec(viewCodecClassName)) {
                BigInteger bits = value instanceof I128
                        ? ((I128) value).toBigInteger()
                        : ((U128) value).toBigInteger();
                storeBigIntegerBytes(bits, materializedSize);
                return;
            }
            if (F128_CODEC.equals(viewCodecClassName)) {
                storeBigIntegerBytes(((F128) value).toBits(), materializedSize);
                return;
            }
            storeRange(encodeAggregate(viewCodecClassName, value));
            return;
        }

        storeBytes(incomingBits(value, materializedSize), materializedSize);
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

    private int materializedViewSize() {
        return checkedArrayLength(viewSize);
    }

    public static synchronized void volatileFence() {
        // TODO
    }

    private static Pointer slicePointer(Object backing, int index) {
        return backing instanceof Pointer
                ? ((Pointer) backing).sliceElementView().add(index)
                : null;
    }

    private Pointer sliceElementView() {
        return viewSize == 0 && viewCodecClassName == null ? sliceStorageView() : this;
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

    /** Materializes byte-oriented slice storage regardless of its JVM backing. */
    public static byte[] sliceToByteArray(Object backing, int offset, int length) {
        byte[] result = new byte[length];
        for (int index = 0; index < length; index++) {
            result[index] = sliceGetI8(backing, offset + index);
        }
        return result;
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
        prepareMemoryWrite(byteOffset, source.length);
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
            Object current = readElement(elementIndex);
            byte[] image = encodeAggregate(allocationCodecClassName, current);
            int chunk = Math.min(source.length - consumed, image.length - withinElement);
            System.arraycopy(source, consumed, image, withinElement, chunk);
            writeElementPreservingIdentity(
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
        if (allocation instanceof ReceiverCell) {
            if (elementIndex != 0) {
                throw new IndexOutOfBoundsException("pointer arithmetic escaped receiver storage");
            }
            overwriteManagedObject(((ReceiverCell) allocation).value, value);
            return;
        }
        if (allocation instanceof FieldCell) {
            if (elementIndex != 0) {
                throw new IndexOutOfBoundsException("pointer arithmetic escaped field storage");
            }
            ((FieldCell) allocation).set(value);
            return;
        }
        Array.set(allocation, elementIndex, value);
    }

    private void writeElementPreservingIdentity(int elementIndex, Object value) {
        Object current = readElement(elementIndex);
        if (isGeneratedAggregateCodec(allocationCodecClassName)
                && current != null
                && value != null
                && current.getClass() == value.getClass()
                && hasProjectedFieldCells(current)) {
            overwriteManagedObject(current, value);
        } else {
            writeElement(elementIndex, value);
        }
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

    private static Method[] scalarEnumMethods(Class<?> type) {
        synchronized (SCALAR_ENUM_METHODS) {
            Method[] cached = SCALAR_ENUM_METHODS.get(type);
            if (cached != null) {
                return cached;
            }
            Method[] methods = new Method[0];
            try {
                boolean hasPayload = false;
                for (Field field : type.getFields()) {
                    if (!Modifier.isStatic(field.getModifiers())) {
                        hasPayload = true;
                        break;
                    }
                }
                if (!hasPayload) {
                    Method discriminant = type.getMethod("_unionDiscriminant");
                    Method fromDiscriminant = type.getMethod("_fromUnionDiscriminant", long.class);
                    if (discriminant.getReturnType() == long.class
                            && Modifier.isStatic(fromDiscriminant.getModifiers())) {
                        methods = new Method[] {discriminant, fromDiscriminant};
                    }
                }
            } catch (NoSuchMethodException ignored) {
                // This is an ordinary non-enum JVM carrier.
            }
            SCALAR_ENUM_METHODS.put(type, methods);
            return methods;
        }
    }

    public static void copyUnionStorage(
            byte[] sourceBytes,
            Object[] sourceObjects,
            int sourceOffset,
            byte[] targetBytes,
            Object[] targetObjects,
            int targetOffset,
            int size) {
        System.arraycopy(sourceBytes, sourceOffset, targetBytes, targetOffset, size);
        System.arraycopy(sourceObjects, sourceOffset, targetObjects, targetOffset, size);
    }

    private static byte[] encodeAggregate(String codecClassName, Object value) {
        try {
            return (byte[]) codecMethods(codecClassName)[0].invoke(null, value);
        } catch (ReflectiveOperationException | IllegalArgumentException error) {
            throw new IllegalStateException(
                    "could not encode Rust aggregate memory with "
                            + codecClassName
                            + " from "
                            + (value == null ? "null" : value.getClass().getName()),
                    error);
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
        Method[] enumMethods = scalarEnumMethods(value.getClass());
        if (size <= 8 && enumMethods.length != 0) {
            try {
                return ((Number) enumMethods[0].invoke(value)).longValue();
            } catch (ReflectiveOperationException error) {
                throw new IllegalStateException("could not read Rust enum discriminant", error);
            }
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
                || RAW_POINTER_VIEW_CODEC.equals(codec)
                || isFatPointerCodec(codec);
    }

    private static boolean isGeneratedAggregateCodec(String codec) {
        return codec != null && !codec.startsWith("@");
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
        Method[] enumMethods = scalarEnumMethods(current.getClass());
        if (size <= 8 && enumMethods.length != 0) {
            try {
                return enumMethods[1].invoke(null, bits);
            } catch (ReflectiveOperationException error) {
                throw new IllegalStateException("could not reconstruct Rust enum", error);
            }
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
        if (value instanceof Boolean
                && (current instanceof Number || current instanceof Character)) {
            return carrierFromBits(
                    current, ((Boolean) value).booleanValue() ? 1L : 0L, size);
        }
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
