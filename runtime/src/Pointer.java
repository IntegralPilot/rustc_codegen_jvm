package org.rustlang.runtime;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicLongArray;

public final class Pointer {


    private static Object arrayGet(Object array, int index) {
        if (array instanceof byte[]) {
            return Byte.valueOf(((byte[]) array)[index]);
        }
        if (array instanceof boolean[]) {
            return Boolean.valueOf(((boolean[]) array)[index]);
        }
        if (array instanceof short[]) {
            return Short.valueOf(((short[]) array)[index]);
        }
        if (array instanceof char[]) {
            return Character.valueOf(((char[]) array)[index]);
        }
        if (array instanceof int[]) {
            return Integer.valueOf(((int[]) array)[index]);
        }
        if (array instanceof long[]) {
            return Long.valueOf(((long[]) array)[index]);
        }
        if (array instanceof float[]) {
            return Float.valueOf(((float[]) array)[index]);
        }
        if (array instanceof double[]) {
            return Double.valueOf(((double[]) array)[index]);
        }
        return ((Object[]) array)[index];
    }

    private static void arraySet(Object array, int index, Object value) {
        if (array instanceof byte[] && value instanceof Byte) {
            ((byte[]) array)[index] = ((Byte) value).byteValue();
        } else if (array instanceof boolean[] && value instanceof Boolean) {
            ((boolean[]) array)[index] = ((Boolean) value).booleanValue();
        } else if (array instanceof short[]
                && (value instanceof Byte || value instanceof Short)) {
            ((short[]) array)[index] = ((Number) value).shortValue();
        } else if (array instanceof char[] && value instanceof Character) {
            ((char[]) array)[index] = ((Character) value).charValue();
        } else if (array instanceof int[]
                && (value instanceof Byte
                        || value instanceof Short
                        || value instanceof Integer)) {
            ((int[]) array)[index] = ((Number) value).intValue();
        } else if (array instanceof int[] && value instanceof Character) {
            ((int[]) array)[index] = ((Character) value).charValue();
        } else if (array instanceof long[] && value instanceof Character) {
            ((long[]) array)[index] = ((Character) value).charValue();
        } else if (array instanceof long[]
                && (value instanceof Byte
                        || value instanceof Short
                        || value instanceof Integer
                        || value instanceof Long)) {
            ((long[]) array)[index] = ((Number) value).longValue();
        } else if (array instanceof float[]
                && value instanceof Number
                && !(value instanceof Double)) {
            ((float[]) array)[index] = ((Number) value).floatValue();
        } else if (array instanceof double[] && value instanceof Number) {
            ((double[]) array)[index] = ((Number) value).doubleValue();
        } else if (array instanceof Object[]) {
            ((Object[]) array)[index] = value;
        } else {
            Array.set(array, index, value);
        }
    }

    public static void dropRustValue(Object value) {
        if (value instanceof RustDrop) {
            ((RustDrop) value).rustDrop();
        } else if (value != null && value.getClass().isArray()) {
            Throwable pendingDropFailure = null;
            int length = Array.getLength(value);
            for (int index = 0; index < length; index++) {
                try {
                    dropRustValue(arrayGet(value, index));
                } catch (Throwable failure) {
                    PanicSupport.abortIfStackOverflow(failure);
                    if (pendingDropFailure != null) {
                        Runtime.getRuntime().halt(134);
                    }
                    pendingDropFailure = failure;
                }
            }
            if (pendingDropFailure != null) {
                rethrowUnchecked(pendingDropFailure);
            }
        } else if (value instanceof TraitObjectCarrier) {
            dropRustValue(((TraitObjectCarrier) value).rustTraitObjectPayload());
        } else if (value instanceof Pointer) {
            Object pointee = ((Pointer) value).directCellValueOrSelf();
            if (pointee != value) {
                dropRustValue(pointee);
            }
        }
    }

    public static boolean catchUnwind(Object tryFunction, Pointer data, Object catchFunction) {
        try {
            invokeRustFunction(tryFunction, data);
            return false;
        } catch (Throwable failure) {
            PanicSupport.abortIfStackOverflow(failure);
            if (failure instanceof VirtualMachineError || failure instanceof ThreadDeath) {
                rethrowUnchecked(failure);
            }
            if (Boolean.getBoolean("org.rustlang.debugUnwind")) {
                failure.printStackTrace(System.err);
            }
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
    private static final String ZERO_SIZED_CODEC_PREFIX = "@zero-sized:";
    private static final String RAW_POINTER_VIEW_CODEC = "@raw-pointer";
    private static final String ARRAY_REFERENCE_VIEW_CODEC_PREFIX = "@array-reference\n";
    private static final String SLICE_POINTER_VIEW_CODEC_PREFIX = "@slice-pointer\n";
    private static final String STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX =
            "@struct-tail-pointer\n";
    private static final String STRUCT_TRAIT_TAIL_CARRIER_PREFIX = "@trait:";
    private static final String TRAIT_POINTER_VIEW_CODEC_PREFIX = "@trait-pointer\n";
    private static final String SIGNED_BIG_INTEGER_CODEC = "@signed-big-integer";
    private static final String UNSIGNED_BIG_INTEGER_CODEC = "@unsigned-big-integer";
    private static final String F128_CODEC = "@f128";
    private static final String STRUCTURAL_VIEW_CODEC_PREFIX = "@structural-view:";
    private static final String STRUCT_TAIL_VIEW_CODEC_PREFIX = "@struct-tail-view:";
    private static final String SLICE_VIEW_CLASS_NAME = "org.rustlang.runtime.SliceView";
    private static final String UTF8_VIEW_CLASS_NAME = "org.rustlang.runtime.Utf8View";
    private static final AtomicLong NEXT_ADDRESS = new AtomicLong(0x1_0000_0000L);
    private static final Map<Object, AllocationInfo> ALLOCATIONS = new WeakIdentityMap<>();
    private static final Map<Object, Boolean> ALLOCATOR_OWNED_ALLOCATIONS =
            new IdentityHashMap<>();
    private static final ConcurrentHashMap<String, byte[]> CONSTANT_ALLOCATIONS =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<String, Pointer> CONSTANT_CELLS =
            new ConcurrentHashMap<>();
    private static final Map<Long, ExposedTarget> EXPOSED_ADDRESSES = new HashMap<>();
    private static final ConcurrentHashMap<Long, TypedExposedEntry>
            TYPED_EXPOSED_ADDRESSES = new ConcurrentHashMap<>();
    private static final ReferenceQueue<ExposedTarget> TYPED_EXPOSED_TARGET_QUEUE =
            new ReferenceQueue<>();
    private static final AtomicInteger TYPED_EXPOSED_OPERATIONS_UNTIL_QUEUE_DRAIN =
            new AtomicInteger(16);
    private static final Map<Object, Set<Long>> ALLOCATION_EXPOSED_ADDRESSES =
            new IdentityHashMap<>();
    private static final NavigableMap<Long, AllocationRange> ALLOCATION_RANGES =
            new TreeMap<>();
    private static final ReferenceQueue<Object> ALLOCATION_RANGE_QUEUE =
            new ReferenceQueue<>();
    private static final ConcurrentHashMap<String, MemoryCodec> CODEC_METHODS =
            new ConcurrentHashMap<>();
    private static final ThreadLocal<CodecPlanCache> RECENT_CODEC_PLANS =
            new ThreadLocal<CodecPlanCache>() {
                @Override
                protected CodecPlanCache initialValue() {
                    return new CodecPlanCache();
                }
            };
    private static final ThreadLocal<ResolvedClassCache> RECENT_RESOLVED_CLASSES =
            new ThreadLocal<ResolvedClassCache>() {
                @Override
                protected ResolvedClassCache initialValue() {
                    return new ResolvedClassCache();
                }
            };
    private static final ConcurrentHashMap<String, Object> SHARED_CONSTANTS =
            new ConcurrentHashMap<>();
    private static final Map<Object, Boolean> SHARED_CONSTANT_ARRAYS =
            new IdentityHashMap<>();
    private static final ConcurrentHashMap<String, String[]> CODEC_DESCRIPTORS =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<String, String> BINARY_CLASS_NAMES =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Class<?>, Boolean> STRUCTURAL_METADATA_TYPES =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Class<?>, Method[]> SCALAR_ENUM_METHODS =
            new ConcurrentHashMap<>();
    private static final ClassLoader RUNTIME_CLASS_LOADER =
            Pointer.class.getClassLoader();
    private static final ConcurrentHashMap<String, Class<?>> RUNTIME_RESOLVED_CLASSES =
            new ConcurrentHashMap<>();
    private static final Map<ClassLoader, ConcurrentHashMap<String, Class<?>>> RESOLVED_CLASSES =
            new IdentityHashMap<>();
    private static final ClassValue<ConcurrentHashMap<String, FieldAccess>> FIELD_ACCESSORS =
            new ClassValue<ConcurrentHashMap<String, FieldAccess>>() {
                @Override
                protected ConcurrentHashMap<String, FieldAccess> computeValue(Class<?> type) {
                    return new ConcurrentHashMap<>();
                }
            };
    private static final ClassValue<RustField[]> PUBLIC_INSTANCE_FIELDS = RustField.ALL;
    private static final ClassValue<Map<Integer, ConstructorPlan>> PUBLIC_CONSTRUCTORS_BY_ARITY =
            new ClassValue<Map<Integer, ConstructorPlan>>() {
                @Override
                protected Map<Integer, ConstructorPlan> computeValue(Class<?> type) {
                    Map<Integer, ConstructorPlan> constructors = new HashMap<>();
                    for (Constructor<?> constructor : type.getConstructors()) {
                        constructor.setAccessible(true);
                        try {
                            constructors.putIfAbsent(
                                    constructor.getParameterCount(),
                                    new ConstructorPlan(constructor));
                        } catch (IllegalAccessException error) {
                            throw new IllegalStateException(
                                    "could not access generated Rust value constructor", error);
                        }
                    }
                    return constructors;
                }
            };
    private static final ClassValue<Boolean> RUST_FUNCTION_POINTER_TYPES =
            new ClassValue<Boolean>() {
                @Override
                protected Boolean computeValue(Class<?> type) {
                    for (Class<?> implementedInterface : type.getInterfaces()) {
                        if (implementedInterface.getName()
                                .startsWith("org.rustlang.runtime.FnPtr_")) {
                            return Boolean.TRUE;
                        }
                    }
                    return Boolean.FALSE;
                }
            };
    private static final ClassValue<ManagedCopyPlan> MANAGED_COPY_PLANS =
            new ClassValue<ManagedCopyPlan>() {
                @Override
                protected ManagedCopyPlan computeValue(Class<?> type) {
                    RustField[] fields = PUBLIC_INSTANCE_FIELDS.get(type);
                    ManagedFieldPlan[] fieldPlans = new ManagedFieldPlan[fields.length];
                    for (int index = 0; index < fields.length; index++) {
                        try {
                            fieldPlans[index] = new ManagedFieldPlan(fields[index]);
                        } catch (IllegalAccessException error) {
                            throw new IllegalStateException(
                                    "could not access generated Rust value field", error);
                        }
                    }
                    ConstructorPlan constructor = constructorWithArity(type, fields.length);
                    Class<?>[] parameterTypes = constructor.parameterTypes;
                    Object[] defaults = new Object[parameterTypes.length];
                    for (int index = 0; index < parameterTypes.length; index++) {
                        defaults[index] = defaultValue(parameterTypes[index]);
                    }
                    return new ManagedCopyPlan(fieldPlans, constructor, defaults);
                }
            };
    private static final Map<Object, Long> MANAGED_OBJECT_ADDRESSES = new WeakIdentityMap<>();
    private static final Map<Long, ManagedObjectReference> MANAGED_OBJECTS = new HashMap<>();
    private static final ReferenceQueue<Object> MANAGED_OBJECT_QUEUE = new ReferenceQueue<>();
    // Function pointers denote static code and remain valid after the carrier
    // local that exposed them goes out of scope. Keep one canonical pointer
    // cell per callable identity so integer, raw-pointer, and union views all
    // observe the same nonzero address and can reconstruct the callable.
    private static final Map<Object, Pointer> FUNCTION_POINTER_CELLS = new IdentityHashMap<>();
    private static final Map<Class<?>, Map<Object, Pointer>> FUNCTION_POINTER_ADAPTER_CELLS =
            new HashMap<>();
    private static final Map<Long, Object> FUNCTION_POINTERS_BY_ADDRESS = new HashMap<>();
    private static final ConcurrentHashMap<String, JavaStringViews> JAVA_STRING_VIEWS =
            new ConcurrentHashMap<>();
    private static final Map<String, Pointer> TRAIT_METADATA_MARKERS = new HashMap<>();
    private static final ConcurrentHashMap<Long, TraitMetadataInfo> TRAIT_METADATA_INFO =
            new ConcurrentHashMap<>();
    private static final int STATE_STRIPE_COUNT = 256;
    private static final int LAZY_ARRAY_REPEAT_THRESHOLD = 2;
    private static final int REPEATED_ARRAY_FILTER_WORDS = 1 << 16;
    private static final long IDENTITY_FILTER_REBUILD_MARKS = 1L << 18;
    private static final long MEMORY_VIEW_ORIGIN_FILTER_REBUILD_MARKS = 1L << 20;
    private static final class RebuildableIdentityFilter {
        private final int wordCount;
        private final long rebuildMarks;
        private volatile AtomicLongArray primary;
        private volatile AtomicLongArray secondary;
        private final AtomicLong marks = new AtomicLong();
        private final AtomicInteger rebuilding = new AtomicInteger();

        private RebuildableIdentityFilter() {
            this(REPEATED_ARRAY_FILTER_WORDS, IDENTITY_FILTER_REBUILD_MARKS);
        }

        private RebuildableIdentityFilter(int wordCount, long rebuildMarks) {
            this.wordCount = wordCount;
            this.rebuildMarks = rebuildMarks;
            primary = new AtomicLongArray(wordCount);
        }
    }
    private static final AtomicLongArray REPEATED_ARRAY_FILTER =
            new AtomicLongArray(REPEATED_ARRAY_FILTER_WORDS);
    private static final RebuildableIdentityFilter STRUCTURAL_VIEW_FILTER =
            new RebuildableIdentityFilter();
    private static final RebuildableIdentityFilter MEMORY_VIEW_FILTER =
            new RebuildableIdentityFilter();
    private static final RebuildableIdentityFilter MEMORY_VIEW_ORIGIN_FILTER =
            new RebuildableIdentityFilter(REPEATED_ARRAY_FILTER_WORDS, MEMORY_VIEW_ORIGIN_FILTER_REBUILD_MARKS);
    private static final AtomicLongArray MEMORY_VIEW_EPOCHS =
            new AtomicLongArray(STATE_STRIPE_COUNT);
    private static final RebuildableIdentityFilter ENCODED_REFERENCE_FILTER =
            new RebuildableIdentityFilter();
    private static final RebuildableIdentityFilter ENCODED_POINTER_FILTER =
            new RebuildableIdentityFilter(1 << 20, 1L << 22);
    private static final AtomicLongArray FIELD_CELL_FILTER =
            new AtomicLongArray(REPEATED_ARRAY_FILTER_WORDS);
    private static final AtomicLongArray SHARED_CONSTANT_ARRAY_FILTER =
            new AtomicLongArray(REPEATED_ARRAY_FILTER_WORDS);
    private static final Map<Object, Map<Long, StructuralViewState>>[] STRUCTURAL_VIEWS =
            createWeakMapStripes();
    private static final Map<Object, LongRangeMap<MemoryViewState>>[] MEMORY_VIEWS =
            createWeakMapStripes();
    private static final Map<Object, MemoryViewOrigin>[] MEMORY_VIEW_ORIGINS =
            createWeakMapStripes();
    private static final Map<Object, Map<Object, Boolean>>[] MEMORY_ORIGIN_VIEWS =
            createWeakMapStripes();
    private static final Map<Object, RepeatedArrayState>[] REPEATED_ARRAYS =
            createWeakMapStripes();
    private static final Map<Object, Object>[] ENCODED_REFERENCES =
            createWeakMapStripes();
    private static final Map<Object, LongRangeMap<EncodedPointerState>>[]
            ENCODED_POINTERS = createWeakMapStripes();
    private static final Map<Object, Long>[] ALLOCATION_BASE_CACHE =
            createWeakMapStripes();
    private static final Map<Object, Long>[] PUBLISHED_ALLOCATION_BASE_CACHE =
            createWeakMapStripes();
    private static final ThreadLocal<Integer> MEMORY_VIEW_WRITEBACK_DEPTH =
            ThreadLocal.withInitial(() -> 0);
    private static final ThreadLocal<MemoryViewAbsenceCache> MEMORY_VIEW_ABSENCE =
            ThreadLocal.withInitial(MemoryViewAbsenceCache::new);
    private static final Map<Object, Map<String, WeakReference<FieldCell>>>[] FIELD_CELLS =
            createWeakMapStripes();
    private static final int ATOMIC_STRIPE_COUNT = 1024;
    private static final int ATOMIC_RELAXED = 0;
    private static final int ATOMIC_RELEASE = 1;
    private static final int ATOMIC_ACQUIRE = 2;
    private static final int ATOMIC_ACQ_REL = 3;
    private static final int ATOMIC_SEQ_CST = 4;
    private static final Object[] ATOMIC_STRIPES = createAtomicStripes();
    private static final Object ATOMIC_SEQUENCE_LOCK = new Object();
    private static final Object[] EMPTY_UNION_OBJECT_STORAGE = new Object[0];
    private static final AtomicLong ATOMIC_FENCE_EPOCH = new AtomicLong();
    @SuppressWarnings("unchecked")
    private static <V> Map<Object, V>[] createWeakMapStripes() {
        Map<Object, V>[] stripes = (Map<Object, V>[]) new Map<?, ?>[STATE_STRIPE_COUNT];
        for (int index = 0; index < stripes.length; index++) {
            stripes[index] = new WeakIdentityMap<>();
        }
        return stripes;
    }

    private static <V> Map<Object, V> stateStripe(Map<Object, V>[] stripes, Object key) {
        return stripes[stateStripeIndex(key)];
    }

    private static int stateStripeIndex(Object key) {
        int hash = key == null ? 0 : System.identityHashCode(key);
        hash ^= hash >>> 16;
        return hash & (STATE_STRIPE_COUNT - 1);
    }

    private static long memoryViewEpoch(Object allocation) {
        return MEMORY_VIEW_EPOCHS.get(stateStripeIndex(allocation));
    }

    private static void advanceMemoryViewEpoch(Object allocation) {
        MEMORY_VIEW_EPOCHS.incrementAndGet(stateStripeIndex(allocation));
    }

    private static boolean directCellHasNoMemoryViews(Object allocation) {
        if (allocation instanceof Cell) {
            return !((Cell) allocation).hasMemoryView;
        }
        if (allocation instanceof ReceiverCell) {
            return !((ReceiverCell) allocation).hasMemoryView;
        }
        return allocation instanceof FieldCell && !((FieldCell) allocation).hasMemoryView;
    }

    private static void setDirectCellHasMemoryView(Object allocation, boolean present) {
        if (allocation instanceof Cell) {
            ((Cell) allocation).hasMemoryView = present;
        } else if (allocation instanceof ReceiverCell) {
            ((ReceiverCell) allocation).hasMemoryView = present;
        } else if (allocation instanceof FieldCell) {
            ((FieldCell) allocation).hasMemoryView = present;
            if (present) {
                markProjectedViewParents((FieldCell) allocation);
            }
        }
    }

    private static Object encodedReferenceOwner(Object owner) {
        return owner instanceof FieldCell ? ((FieldCell) owner).owner() : owner;
    }

    private static void retainEncodedReference(Object owner, Object referencedAllocation) {
        owner = encodedReferenceOwner(owner);
        if (owner == null
                || referencedAllocation == null
                || owner == referencedAllocation) {
            return;
        }
        Map<Object, Object> stripe =
                stateStripe(ENCODED_REFERENCES, owner);
        synchronized (stripe) {
            Object current = stripe.get(owner);
            if (current == null) {
                stripe.put(owner, referencedAllocation);
            } else if (current != referencedAllocation) {
                EncodedReferenceSet references;
                if (current instanceof EncodedReferenceSet) {
                    references = (EncodedReferenceSet) current;
                } else {
                    references = new EncodedReferenceSet(current);
                    stripe.put(owner, references);
                }
                references.allocations.put(referencedAllocation, Boolean.TRUE);
            }
            markIdentityFilter(ENCODED_REFERENCE_FILTER, owner);
        }
        maybeRebuildIdentityFilter(ENCODED_REFERENCE_FILTER, ENCODED_REFERENCES);
    }

    private static void transferEncodedReferences(Object sourceOwner, Object targetOwner) {
        sourceOwner = encodedReferenceOwner(sourceOwner);
        targetOwner = encodedReferenceOwner(targetOwner);
        if (sourceOwner == null || targetOwner == null || sourceOwner == targetOwner) {
            return;
        }
        Object referenced;
        Object[] referencedSet = null;
        if (!mayBeInIdentityFilter(ENCODED_REFERENCE_FILTER, sourceOwner)) {
            return;
        }
        Map<Object, Object> sourceStripe =
                stateStripe(ENCODED_REFERENCES, sourceOwner);
        synchronized (sourceStripe) {
            referenced = sourceStripe.get(sourceOwner);
            if (referenced == null) {
                return;
            }
            if (referenced instanceof EncodedReferenceSet) {
                referencedSet = ((EncodedReferenceSet) referenced)
                        .allocations.keySet().toArray();
            }
        }
        if (referencedSet != null) {
            for (Object allocation : referencedSet) {
                retainEncodedReference(targetOwner, allocation);
            }
        } else {
            retainEncodedReference(targetOwner, referenced);
        }
    }

    private static void moveEncodedReferences(Object sourceOwner, Object targetOwner) {
        Object source = encodedReferenceOwner(sourceOwner);
        Object target = encodedReferenceOwner(targetOwner);
        if (source == null || source == target) {
            return;
        }
        transferEncodedReferences(source, target);
        discardEncodedReferences(source);
    }

    private static void discardEncodedReferences(Object owner) {
        owner = encodedReferenceOwner(owner);
        if (owner == null) {
            return;
        }
        if (mayBeInIdentityFilter(ENCODED_REFERENCE_FILTER, owner)) {
            Map<Object, Object> stripe =
                    stateStripe(ENCODED_REFERENCES, owner);
            synchronized (stripe) {
                stripe.remove(owner);
            }
        }
        discardEncodedPointers(owner);
    }

    private static final class EncodedReferenceSet {
        private final IdentityHashMap<Object, Boolean> allocations = new IdentityHashMap<>();

        private EncodedReferenceSet(Object first) {
            allocations.put(first, Boolean.TRUE);
        }
    }

    private static final class ManagedObjectReference extends WeakReference<Object> {
        private final long address;

        private ManagedObjectReference(Object value, long address) {
            super(value, MANAGED_OBJECT_QUEUE);
            this.address = address;
        }
    }

    private static final class JavaStringViews {
        private final byte[] bytes;
        private volatile Object slice;
        private volatile Object utf8;

        private JavaStringViews(String value) {
            bytes = value.getBytes(StandardCharsets.UTF_8);
        }
    }

    /**
     * Small sorted map for byte offsets. Pointer provenance and decoded-view
     * ranges normally contain only a handful of entries per allocation, so a
     * primitive array avoids TreeMap nodes, boxed Long keys, and entry copies.
     */
    private static final class LongRangeMap<V> {
        private long firstKey;
        private long secondKey;
        private Object firstValue;
        private Object secondValue;
        private long[] keys;
        private Object[] values;
        private int size;

        private int find(long key) {
            if (keys == null) {
                if (size == 0 || key < firstKey) {
                    return -1;
                }
                if (key == firstKey) {
                    return 0;
                }
                if (size == 1 || key < secondKey) {
                    return -2;
                }
                return key == secondKey ? 1 : -3;
            }
            int low = 0;
            int high = size - 1;
            while (low <= high) {
                int middle = (low + high) >>> 1;
                long candidate = keys[middle];
                if (candidate < key) {
                    low = middle + 1;
                } else if (candidate > key) {
                    high = middle - 1;
                } else {
                    return middle;
                }
            }
            return -low - 1;
        }

        private void promote() {
            keys = new long[4];
            values = new Object[4];
            keys[0] = firstKey;
            keys[1] = secondKey;
            values[0] = firstValue;
            values[1] = secondValue;
            firstValue = null;
            secondValue = null;
        }

        private void ensureCapacity() {
            if (keys == null) {
                promote();
                return;
            }
            if (size < keys.length) {
                return;
            }
            int capacity = keys.length << 1;
            keys = Arrays.copyOf(keys, capacity);
            values = Arrays.copyOf(values, capacity);
        }

        @SuppressWarnings("unchecked")
        private V valueAt(int index) {
            if (keys == null) {
                return (V) (index == 0 ? firstValue : secondValue);
            }
            return (V) values[index];
        }

        private long keyAt(int index) {
            if (keys == null) {
                return index == 0 ? firstKey : secondKey;
            }
            return keys[index];
        }

        private V get(long key) {
            int index = find(key);
            return index < 0 ? null : valueAt(index);
        }

        private void put(long key, V value) {
            int index = find(key);
            if (index >= 0) {
                if (keys == null) {
                    if (index == 0) {
                        firstValue = value;
                    } else {
                        secondValue = value;
                    }
                } else {
                    values[index] = value;
                }
                return;
            }
            index = -index - 1;
            if (keys == null && size < 2) {
                if (size == 0) {
                    firstKey = key;
                    firstValue = value;
                } else if (index == 0) {
                    secondKey = firstKey;
                    secondValue = firstValue;
                    firstKey = key;
                    firstValue = value;
                } else {
                    secondKey = key;
                    secondValue = value;
                }
                size++;
                return;
            }
            ensureCapacity();
            int moved = size - index;
            if (moved > 0) {
                System.arraycopy(keys, index, keys, index + 1, moved);
                System.arraycopy(values, index, values, index + 1, moved);
            }
            keys[index] = key;
            values[index] = value;
            size++;
        }

        private V remove(long key) {
            int index = find(key);
            return index < 0 ? null : removeAt(index);
        }

        private V removeAt(int index) {
            V previous = valueAt(index);
            if (keys == null) {
                if (index == 0 && size == 2) {
                    firstKey = secondKey;
                    firstValue = secondValue;
                }
                if (--size < 2) {
                    secondValue = null;
                }
                if (size == 0) {
                    firstValue = null;
                }
                return previous;
            }
            int moved = size - index - 1;
            if (moved > 0) {
                System.arraycopy(keys, index + 1, keys, index, moved);
                System.arraycopy(values, index + 1, values, index, moved);
            }
            values[--size] = null;
            return previous;
        }

        private boolean containsKey(long key) {
            return find(key) >= 0;
        }

        private int floorIndex(long key) {
            int index = find(key);
            return index >= 0 ? index : -index - 2;
        }

        private int ceilingIndex(long key) {
            int index = find(key);
            return index >= 0 ? index : -index - 1;
        }

        private int size() {
            return size;
        }

        private boolean isEmpty() {
            return size == 0;
        }
    }

    private static final class EncodedPointerState {
        private final int size;
        private final String codec;
        private final ExposedTarget target;
        private final boolean targetUsesOwnerAllocation;

        private EncodedPointerState(
                Object owner, int size, String codec, ExposedTarget target) {
            this.size = size;
            this.codec = codec;
            targetUsesOwnerAllocation = target.allocation == owner;
            this.target = targetUsesOwnerAllocation
                    ? target.withAllocation(null)
                    : target;
        }

        private ExposedTarget target(Object owner) {
            return targetUsesOwnerAllocation
                    ? target.withAllocation(owner)
                    : target;
        }
    }

    private static final class EncodedPointerCopy {
        private final long offset;
        private final int size;
        private final String codec;
        private final ExposedTarget target;

        private EncodedPointerCopy(
                long offset, int size, String codec, ExposedTarget target) {
            this.offset = offset;
            this.size = size;
            this.codec = codec;
            this.target = target;
        }
    }

    private static void rememberEncodedPointer(
            Object owner,
            long offset,
            int size,
            String codec,
            Pointer pointer) {
        rememberEncodedPointer(
                owner,
                offset,
                size,
                codec,
                pointer == null ? null : pointer.exposedTarget());
    }

    private static void rememberEncodedPointer(
            Object owner,
            long offset,
            int size,
            String codec,
            ExposedTarget target) {
        if (owner == null || target == null || size <= 0 || codec == null) {
            return;
        }
        Map<Object, LongRangeMap<EncodedPointerState>> stripe =
                stateStripe(ENCODED_POINTERS, owner);
        synchronized (stripe) {
            LongRangeMap<EncodedPointerState> pointers = stripe.get(owner);
            if (pointers == null) {
                pointers = new LongRangeMap<>();
                stripe.put(owner, pointers);
            }
            removeOverlappingEncodedPointers(pointers, offset, size);
            pointers.put(
                    offset,
                    new EncodedPointerState(owner, size, codec, target));
            markIdentityFilter(ENCODED_POINTER_FILTER, owner);
        }
        maybeRebuildIdentityFilter(ENCODED_POINTER_FILTER, ENCODED_POINTERS);
    }

    private static Pointer encodedPointer(
            Object owner, long offset, int size, String codec, long address) {
        if (owner != null
                && codec != null
                && mayBeInIdentityFilter(ENCODED_POINTER_FILTER, owner)) {
            Map<Object, LongRangeMap<EncodedPointerState>> stripe =
                    stateStripe(ENCODED_POINTERS, owner);
            EncodedPointerState state;
            synchronized (stripe) {
                LongRangeMap<EncodedPointerState> pointers = stripe.get(owner);
                state = pointers == null ? null : pointers.get(offset);
            }
            if (state != null
                    && state.size == size
                    && codec.equals(state.codec)) {
                Pointer pointer = pointerFromExposedTarget(state.target(owner));
                if (pointer.numericAddress() == address) {
                    return pointer;
                }
            }
        }
        return null;
    }

    private static void transferEncodedPointers(
            Object sourceOwner,
            long sourceOffset,
            Object targetOwner,
            long targetOffset,
            int length,
            boolean move) {
        if (sourceOwner == null
                || targetOwner == null
                || length <= 0
                || !mayBeInIdentityFilter(ENCODED_POINTER_FILTER, sourceOwner)) {
            return;
        }
        Map<Object, LongRangeMap<EncodedPointerState>> sourceStripe =
                stateStripe(ENCODED_POINTERS, sourceOwner);
        java.util.ArrayList<EncodedPointerCopy> copied = null;
        synchronized (sourceStripe) {
            LongRangeMap<EncodedPointerState> pointers = sourceStripe.get(sourceOwner);
            if (pointers == null) {
                return;
            }
            long sourceEnd = Math.addExact(sourceOffset, (long) length);
            for (int index = pointers.ceilingIndex(sourceOffset);
                    index < pointers.size() && pointers.keyAt(index) < sourceEnd;
                    index++) {
                long entryOffset = pointers.keyAt(index);
                EncodedPointerState state = pointers.valueAt(index);
                if (Math.addExact(entryOffset, (long) state.size) <= sourceEnd) {
                    if (copied == null) {
                        copied = new java.util.ArrayList<>();
                    }
                    copied.add(new EncodedPointerCopy(
                            entryOffset,
                            state.size,
                            state.codec,
                            state.target(sourceOwner)));
                }
            }
            if (move) {
                removeOverlappingEncodedPointers(pointers, sourceOffset, length);
                if (pointers.isEmpty()) {
                    sourceStripe.remove(sourceOwner);
                }
            }
        }
        if (copied == null) {
            return;
        }
        Map<Object, LongRangeMap<EncodedPointerState>> targetStripe =
                stateStripe(ENCODED_POINTERS, targetOwner);
        synchronized (targetStripe) {
            LongRangeMap<EncodedPointerState> pointers = targetStripe.get(targetOwner);
            if (pointers == null) {
                pointers = new LongRangeMap<>();
                targetStripe.put(targetOwner, pointers);
            }
            removeOverlappingEncodedPointers(pointers, targetOffset, length);
            for (int index = 0; index < copied.size(); index++) {
                EncodedPointerCopy entry = copied.get(index);
                pointers.put(
                        Math.addExact(
                                targetOffset,
                                Math.subtractExact(entry.offset, sourceOffset)),
                        new EncodedPointerState(
                                targetOwner,
                                entry.size,
                                entry.codec,
                                entry.target));
            }
            markIdentityFilter(ENCODED_POINTER_FILTER, targetOwner);
        }
    }

    private static void removeOverlappingEncodedPointers(
            LongRangeMap<EncodedPointerState> pointers, long offset, int size) {
        long end = Math.addExact(offset, (long) size);
        int index = pointers.floorIndex(offset);
        if (index < 0) {
            index = pointers.ceilingIndex(offset);
        }
        while (index < pointers.size() && pointers.keyAt(index) < end) {
            long entryOffset = pointers.keyAt(index);
            EncodedPointerState state = pointers.valueAt(index);
            if (offset < Math.addExact(entryOffset, (long) state.size)
                    && entryOffset < end) {
                pointers.removeAt(index);
            } else {
                index++;
            }
        }
    }

    private static void discardEncodedPointers(Object owner) {
        if (owner == null || !mayBeInIdentityFilter(ENCODED_POINTER_FILTER, owner)) {
            return;
        }
        Map<Object, LongRangeMap<EncodedPointerState>> stripe =
                stateStripe(ENCODED_POINTERS, owner);
        synchronized (stripe) {
            stripe.remove(owner);
        }
    }

    private static void discardEncodedPointers(Object owner, long offset, int size) {
        if (owner == null
                || size <= 0
                || !mayBeInIdentityFilter(ENCODED_POINTER_FILTER, owner)) {
            return;
        }
        Map<Object, LongRangeMap<EncodedPointerState>> stripe =
                stateStripe(ENCODED_POINTERS, owner);
        synchronized (stripe) {
            LongRangeMap<EncodedPointerState> pointers = stripe.get(owner);
            if (pointers == null) {
                return;
            }
            removeOverlappingEncodedPointers(pointers, offset, size);
            if (pointers.isEmpty()) {
                stripe.remove(owner);
            }
        }
    }

    /** Must be called while holding {@link #ALLOCATIONS}. */
    private static AllocationInfo allocationInfo(Object allocation) {
        AllocationInfo info = ALLOCATIONS.get(allocation);
        if (info == null) {
            info = new AllocationInfo();
            ALLOCATIONS.put(allocation, info);
        }
        return info;
    }

    private static Long cachedAllocationBase(
            Map<Object, Long>[] cache, Object allocation) {
        Map<Object, Long> stripe = stateStripe(cache, allocation);
        synchronized (stripe) {
            return stripe.get(allocation);
        }
    }

    private static void cacheAllocationBase(
            Map<Object, Long>[] cache, Object allocation, long base) {
        Map<Object, Long> stripe = stateStripe(cache, allocation);
        synchronized (stripe) {
            stripe.put(allocation, Long.valueOf(base));
        }
    }

    private static void discardCachedAllocationBases(Object allocation) {
        Map<Object, Long> baseStripe = stateStripe(ALLOCATION_BASE_CACHE, allocation);
        synchronized (baseStripe) {
            baseStripe.remove(allocation);
        }
        Map<Object, Long> publishedStripe =
                stateStripe(PUBLISHED_ALLOCATION_BASE_CACHE, allocation);
        synchronized (publishedStripe) {
            publishedStripe.remove(allocation);
        }
    }

    private static void recordAlignment(Object allocation, int alignment) {
        // Synthetic addresses are already 16-byte aligned. Avoid creating
        // allocation metadata for the overwhelmingly common weaker layouts.
        if (alignment <= 16) {
            return;
        }
        synchronized (ALLOCATIONS) {
            allocationInfo(allocation).alignment = alignment;
        }
    }

    private static boolean isSliceViewType(Class<?> type) {
        return type == SliceView.class;
    }

    private static boolean isSliceViewCarrierType(Class<?> type) {
        return type != null && SliceView.class.isAssignableFrom(type);
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
            return activate(targetClass, null);
        }

        private Object activate(Class<?> targetClass, Object traitTailCarrier) {
            Object target = views.get(targetClass);
            if (target == null) {
                target = constructStructuralView(active, targetClass, traitTailCarrier);
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
        private volatile boolean active = true;
        private byte[] originalImage;

        private MemoryViewState(
                int size, String codecClassName, Object value, byte[] originalImage) {
            this.size = size;
            this.codecClassName = codecClassName;
            this.value = value;
            this.originalImage = originalImage;
        }
    }

    private static final class MemoryViewWriteback {
        private final long offset;
        private final MemoryViewState state;

        private MemoryViewWriteback(long offset, MemoryViewState state) {
            this.offset = offset;
            this.state = state;
        }
    }

    private static final class MemoryViewAbsenceCache {
        private static final int CACHE_SIZE = 16;
        @SuppressWarnings("unchecked")
        private final WeakReference<Object>[] allocations =
                (WeakReference<Object>[]) new WeakReference<?>[CACHE_SIZE];
        private final long[] epochs = new long[allocations.length];

        private static int index(Object candidate) {
            int hash = System.identityHashCode(candidate);
            hash ^= hash >>> 16;
            return hash & (CACHE_SIZE - 1);
        }

        private boolean matches(Object candidate, long currentEpoch) {
            int index = index(candidate);
            WeakReference<Object> reference = allocations[index];
            return reference != null
                    && reference.get() == candidate
                    && epochs[index] == currentEpoch;
        }

        private void remember(Object candidate, long currentEpoch) {
            int index = index(candidate);
            allocations[index] = new WeakReference<>(candidate);
            epochs[index] = currentEpoch;
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

        private boolean matches(Pointer pointer) {
            return allocation.get() == pointer.allocation
                    && allocationElementSize == pointer.allocationElementSize
                    && byteOffset == pointer.byteOffset
                    && viewSize == pointer.viewSize
                    && java.util.Objects.equals(
                            allocationCodecClassName, pointer.allocationCodecClassName)
                    && metadata == pointer.metadata;
        }
    }

    private static final class ManagedCopyPlan {
        private final ManagedFieldPlan[] fields;
        private final ConstructorPlan constructor;
        private final Object[] defaults;

        private ManagedCopyPlan(
                ManagedFieldPlan[] fields, ConstructorPlan constructor, Object[] defaults) {
            this.fields = fields;
            this.constructor = constructor;
            this.defaults = defaults;
        }
    }

    private static final class ManagedFieldPlan {
        private final MethodHandle getter;
        private final MethodHandle setter;

        private ManagedFieldPlan(RustField field) throws IllegalAccessException {
            MethodHandles.Lookup lookup = MethodHandles.lookup();
            getter = field.getter().asType(MethodType.methodType(
                    Object.class, Object.class));
            setter = field.setter().asType(MethodType.methodType(
                    void.class, Object.class, Object.class));
        }

        private Object get(Object owner) throws Throwable {
            return (Object) getter.invokeExact(owner);
        }

        private void set(Object owner, Object value) throws Throwable {
            setter.invokeExact(owner, value);
        }
    }

    private static final class ConstructorPlan {
        private final Constructor<?> reflection;
        private final Class<?>[] parameterTypes;
        private final MethodHandle spreader;

        private ConstructorPlan(Constructor<?> constructor) throws IllegalAccessException {
            reflection = constructor;
            parameterTypes = constructor.getParameterTypes();
            spreader = MethodHandles.lookup()
                    .unreflectConstructor(constructor)
                    .asSpreader(Object[].class, parameterTypes.length)
                    .asType(MethodType.methodType(Object.class, Object[].class));
        }

        private Object newInstance(Object... arguments) throws ReflectiveOperationException {
            try {
                return (Object) spreader.invokeExact(arguments);
            } catch (RuntimeException | Error error) {
                throw error;
            } catch (Throwable error) {
                throw new ReflectiveOperationException(error);
            }
        }
    }

    /** Two-entry per-thread cache for the codecs used by tight pointer loops. */
    private static final class CodecPlanCache {
        private String firstName;
        private MemoryCodec firstPlan;
        private String secondName;
        private MemoryCodec secondPlan;

        private MemoryCodec get(String name) {
            if (name == firstName || (firstName != null && firstName.equals(name))) {
                return firstPlan;
            }
            if (name == secondName || (secondName != null && secondName.equals(name))) {
                String previousName = firstName;
                MemoryCodec previousPlan = firstPlan;
                firstName = secondName;
                firstPlan = secondPlan;
                secondName = previousName;
                secondPlan = previousPlan;
                return firstPlan;
            }
            return null;
        }

        private void remember(String name, MemoryCodec plan) {
            secondName = firstName;
            secondPlan = firstPlan;
            firstName = name;
            firstPlan = plan;
        }
    }

    /** Two-entry per-thread cache for the generated types used by tight loops. */
    private static final class ResolvedClassCache {
        private String firstName;
        private ClassLoader firstLoader;
        private Class<?> firstClass;
        private String secondName;
        private ClassLoader secondLoader;
        private Class<?> secondClass;

        private Class<?> get(String name, ClassLoader loader) {
            if (loader == firstLoader
                    && (name == firstName || (firstName != null && firstName.equals(name)))) {
                return firstClass;
            }
            if (loader == secondLoader
                    && (name == secondName || (secondName != null && secondName.equals(name)))) {
                String previousName = firstName;
                ClassLoader previousLoader = firstLoader;
                Class<?> previousClass = firstClass;
                firstName = secondName;
                firstLoader = secondLoader;
                firstClass = secondClass;
                secondName = previousName;
                secondLoader = previousLoader;
                secondClass = previousClass;
                return firstClass;
            }
            return null;
        }

        private void remember(String name, ClassLoader loader, Class<?> resolved) {
            secondName = firstName;
            secondLoader = firstLoader;
            secondClass = firstClass;
            firstName = name;
            firstLoader = loader;
            firstClass = resolved;
        }
    }

    private static final class RepeatedArrayState {
        private final Object template;

        private RepeatedArrayState(Object template) {
            this.template = template;
        }
    }

    private static ConstructorPlan constructorWithArity(Class<?> type, int arity) {
        ConstructorPlan constructor = PUBLIC_CONSTRUCTORS_BY_ARITY.get(type).get(arity);
        if (constructor == null) {
            throw new IllegalArgumentException(
                    "no generated Rust value constructor for " + type.getName());
        }
        return constructor;
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
        copyStructuralFields(replacement, target);
    }

    /** Creates an independent JVM carrier for a copied Rust aggregate value. */
    public static Object copyManagedValue(Object value) {
        if (value == null) {
            return null;
        }
        Class<?> valueClass = value.getClass();
        if (!valueClass.isArray()) {
            if (isManagedValueImmutable(value, valueClass)) {
                return value;
            }
            if (value instanceof RustCopy) {
                return ((RustCopy) value).rustCopy();
            }
        }
        return copyManagedValueSlow(value, valueClass);
    }

    private static Object copyManagedValueSlow(Object value, Class<?> valueClass) {
        if (valueClass.isArray()) {
            if (valueClass.getComponentType().isPrimitive()) {
                Object copy;
                int length;
                if (value instanceof byte[]) {
                    byte[] array = (byte[]) value;
                    copy = array.clone();
                    length = array.length;
                } else if (value instanceof short[]) {
                    short[] array = (short[]) value;
                    copy = array.clone();
                    length = array.length;
                } else if (value instanceof int[]) {
                    int[] array = (int[]) value;
                    copy = array.clone();
                    length = array.length;
                } else if (value instanceof long[]) {
                    long[] array = (long[]) value;
                    copy = array.clone();
                    length = array.length;
                } else if (value instanceof char[]) {
                    char[] array = (char[]) value;
                    copy = array.clone();
                    length = array.length;
                } else if (value instanceof float[]) {
                    float[] array = (float[]) value;
                    copy = array.clone();
                    length = array.length;
                } else if (value instanceof double[]) {
                    double[] array = (double[]) value;
                    copy = array.clone();
                    length = array.length;
                } else if (value instanceof boolean[]) {
                    boolean[] array = (boolean[]) value;
                    copy = array.clone();
                    length = array.length;
                } else {
                    throw new IllegalArgumentException(
                            "unsupported primitive array carrier " + valueClass.getName());
                }
                transferEncodedPointers(
                        value,
                        0,
                        copy,
                        0,
                        Math.multiplyExact(length, inferredArrayElementSize(value)),
                        false);
                transferEncodedReferences(value, copy);
                return copy;
            }
            int length = Array.getLength(value);
            Object copy = Array.newInstance(valueClass.getComponentType(), length);
            Object[] sourceElements = (Object[]) value;
            Object[] copyElements = (Object[]) copy;
            for (int index = 0; index < length; index++) {
                copyElements[index] = copyManagedValue(sourceElements[index]);
            }
            return copy;
        }
        try {
            ManagedCopyPlan plan = MANAGED_COPY_PLANS.get(valueClass);
            Object copy = plan.constructor.newInstance(plan.defaults);
            for (ManagedFieldPlan field : plan.fields) {
                field.set(copy, copyManagedValue(field.get(value)));
            }
            return copy;
        } catch (Throwable error) {
            throw new IllegalStateException("could not copy managed Rust value", error);
        }
    }

    /**
     * Lazily materializes a constant which codegen proved is only read.
     * Large Rust lookup tables otherwise rebuild their complete object graph
     * on every use because ordinary array constants require value semantics.
     */
    public static Object sharedConstant(String identity, MethodHandle factory) {
        Object cached = SHARED_CONSTANTS.get(identity);
        if (cached != null) {
            return cached;
        }
        synchronized (SHARED_CONSTANTS) {
            cached = SHARED_CONSTANTS.get(identity);
            if (cached != null) {
                return cached;
            }
            try {
                Object value = factory.invoke();
                if (value == null) {
                    throw new IllegalStateException(
                            "shared Rust constant factory returned null");
                }
                SHARED_CONSTANTS.put(identity, value);
                if (value.getClass().isArray()
                        && !value.getClass().getComponentType().isPrimitive()) {
                    synchronized (SHARED_CONSTANT_ARRAYS) {
                        SHARED_CONSTANT_ARRAYS.put(value, Boolean.TRUE);
                    }
                    markIdentityFilter(SHARED_CONSTANT_ARRAY_FILTER, value);
                }
                return value;
            } catch (Throwable error) {
                rethrowUnchecked(error);
                return null;
            }
        }
    }

    private static boolean isManagedValueImmutable(Object value, Class<?> valueClass) {
        String className = valueClass.getName();
        return valueClass.isPrimitive()
                || value instanceof Number
                || value instanceof Boolean
                || value instanceof Character
                || value instanceof String
                || valueClass.isEnum()
                || (className.length() > 13
                        && className.charAt(13) == 'r'
                        && className.startsWith("org.rustlang.runtime."))
                || isRustFunctionPointer(valueClass);
    }

    private static boolean cannotCarryMemoryViewOrigin(Object value) {
        return value == null
                || value instanceof Number
                || value instanceof Boolean
                || value instanceof Character
                || value instanceof String
                || value instanceof I128
                || value instanceof U128
                || value instanceof F128
                || value.getClass().isEnum();
    }

    /**
     * Implements a Rust array-repeat initializer without requiring the
     * compiler to emit one bytecode store for every element.
     */
    public static void fillArray(Object array, Object value, boolean copyValue) {
        int length = Array.getLength(array);
        Map<Object, RepeatedArrayState> stripe = stateStripe(REPEATED_ARRAYS, array);
        synchronized (stripe) {
            stripe.remove(array);
        }
        if (copyValue
                && length >= LAZY_ARRAY_REPEAT_THRESHOLD
                && value != null
                && !array.getClass().getComponentType().isPrimitive()
                && !isManagedValueImmutable(value, value.getClass())) {
            Arrays.fill((Object[]) array, value);
            synchronized (stripe) {
                stripe.put(array, new RepeatedArrayState(value));
            }
            markRepeatedArray(array);
            return;
        }
        for (int index = 0; index < length; index++) {
            arraySet(array, index, copyValue ? copyManagedValue(value) : value);
        }
    }

    private static Object independentRepeatedArrayElement(Object array, int index) {
        int length = Array.getLength(array);
        if (index < 0 || index >= length) {
            throw new ArrayIndexOutOfBoundsException(
                    "Rust pointer selected element " + index + " of " + length
                            + " in " + array.getClass().getName());
        }
        if (length < LAZY_ARRAY_REPEAT_THRESHOLD
                || array.getClass().getComponentType().isPrimitive()
                || !mayBeRepeatedArray(array)) {
            return arrayGet(array, index);
        }
        Map<Object, RepeatedArrayState> stripe = stateStripe(REPEATED_ARRAYS, array);
        synchronized (stripe) {
            RepeatedArrayState state = stripe.get(array);
            Object value = arrayGet(array, index);
            if (state == null || value != state.template) {
                return value;
            }
            Object copy = copyManagedValue(value);
            arraySet(array, index, copy);
            return copy;
        }
    }

    private static long nestedPrimitiveArrayByteSize(Object array) {
        if (array == null || !array.getClass().isArray()) {
            return -1;
        }
        Class<?> component = array.getClass().getComponentType();
        int length = Array.getLength(array);
        if (component.isPrimitive()) {
            return Math.multiplyExact((long) length, inferredArrayElementSize(array));
        }
        if (!component.isArray()) {
            return -1;
        }
        long size = 0;
        for (int index = 0; index < length; index++) {
            long elementSize = nestedPrimitiveArrayByteSize(arrayGet(array, index));
            if (elementSize < 0) {
                return -1;
            }
            size = Math.addExact(size, elementSize);
        }
        return size;
    }

    private static long nestedPrimitiveArrayElementByteSize(Object array) {
        if (array == null
                || !array.getClass().isArray()
                || !array.getClass().getComponentType().isArray()
                || Array.getLength(array) == 0) {
            return -1;
        }
        long elementSize = nestedPrimitiveArrayByteSize(arrayGet(array, 0));
        if (elementSize < 0) {
            return -1;
        }
        for (int index = 1; index < Array.getLength(array); index++) {
            if (nestedPrimitiveArrayByteSize(arrayGet(array, index)) != elementSize) {
                return -1;
            }
        }
        return elementSize;
    }

    private static Object nestedPrimitiveArrayElement(
            Object array, long byteOffset, int logicalElementSize) {
        Class<?> component = array.getClass().getComponentType();
        if (!component.isArray()) {
            int physicalElementSize = inferredArrayElementSize(array);
            if (logicalElementSize != physicalElementSize
                    || byteOffset % physicalElementSize != 0) {
                throw new IllegalStateException(
                        "flattened Rust array view does not match its primitive backing");
            }
            return independentRepeatedArrayElement(
                    array, Math.toIntExact(byteOffset / physicalElementSize));
        }
        long outerElementSize = nestedPrimitiveArrayElementByteSize(array);
        if (outerElementSize <= 0) {
            throw new IllegalStateException(
                    "could not determine nested primitive-array element layout");
        }
        int outerIndex = Math.toIntExact(byteOffset / outerElementSize);
        long withinOuter = byteOffset % outerElementSize;
        Object outer = independentRepeatedArrayElement(array, outerIndex);
        if (withinOuter == 0 && logicalElementSize == outerElementSize) {
            return outer;
        }
        return nestedPrimitiveArrayElement(outer, withinOuter, logicalElementSize);
    }

    private static void writeNestedPrimitiveArrayElement(
            Object array, long byteOffset, int logicalElementSize, Object value) {
        Class<?> component = array.getClass().getComponentType();
        if (!component.isArray()) {
            int physicalElementSize = inferredArrayElementSize(array);
            if (logicalElementSize != physicalElementSize
                    || byteOffset % physicalElementSize != 0) {
                throw new IllegalStateException(
                        "flattened Rust array view does not match its primitive backing");
            }
            arraySet(array, Math.toIntExact(byteOffset / physicalElementSize), value);
            return;
        }
        long outerElementSize = nestedPrimitiveArrayElementByteSize(array);
        if (outerElementSize <= 0) {
            throw new IllegalStateException(
                    "could not determine nested primitive-array element layout");
        }
        int outerIndex = Math.toIntExact(byteOffset / outerElementSize);
        long withinOuter = byteOffset % outerElementSize;
        if (withinOuter == 0 && logicalElementSize == outerElementSize) {
            arraySet(array, outerIndex, value);
            return;
        }
        writeNestedPrimitiveArrayElement(
                independentRepeatedArrayElement(array, outerIndex),
                withinOuter,
                logicalElementSize,
                value);
    }

    private static void markRepeatedArray(Object array) {
        int hash = System.identityHashCode(array);
        markFilterHash(REPEATED_ARRAY_FILTER, mixRepeatedArrayHash(hash));
        markFilterHash(REPEATED_ARRAY_FILTER, mixRepeatedArrayHash(hash ^ 0x9e37_79b9));
    }

    private static boolean mayBeRepeatedArray(Object array) {
        return mayBeInIdentityFilter(REPEATED_ARRAY_FILTER, array);
    }

    private static int mixRepeatedArrayHash(int hash) {
        hash ^= hash >>> 16;
        hash *= 0x7feb_352d;
        hash ^= hash >>> 15;
        return hash;
    }

    private static boolean markFilterHash(AtomicLongArray filter, int hash) {
        int bitIndex = hash & (filter.length() * Long.SIZE - 1);
        int wordIndex = bitIndex >>> 6;
        long bit = 1L << bitIndex;
        while (true) {
            long previous = filter.get(wordIndex);
            if ((previous & bit) != 0) {
                return false;
            }
            if (filter.compareAndSet(wordIndex, previous, previous | bit)) {
                return true;
            }
        }
    }

    private static boolean hasFilterHash(AtomicLongArray filter, int hash) {
        int bitIndex = hash & (filter.length() * Long.SIZE - 1);
        return (filter.get(bitIndex >>> 6) & (1L << bitIndex)) != 0;
    }

    /** Reads one reference-array element while preserving Rust array value semantics. */
    public static Object arrayGetObject(Object array, int index) {
        Object value = independentRepeatedArrayElement(array, index);
        if (!mayBeInIdentityFilter(SHARED_CONSTANT_ARRAY_FILTER, array)) {
            return value;
        }
        synchronized (SHARED_CONSTANT_ARRAYS) {
            return SHARED_CONSTANT_ARRAYS.containsKey(array)
                    ? copyManagedValue(value)
                    : value;
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
        if (mayBeInIdentityFilter(MEMORY_VIEW_FILTER, array)) {
            new Pointer(
                    array,
                    elementSize,
                    0,
                    Math.multiplyExact(length, elementSize),
                    elementCodec)
                    .flushAllMemoryViews();
        }
        Class<?> component = array.getClass().getComponentType();
        if (elementCodec == null && component.isPrimitive()
                && elementSize == ArrayMemoryCodec.elementSize(component)) {
            ArrayMemoryCodec.write(array, bytes, offset);
            return;
        }
        MemoryCodec aggregatePlan = isGeneratedAggregateCodec(elementCodec)
                ? codecPlan(elementCodec)
                : null;
        for (int index = 0; index < length; index++) {
            Object value = arrayGet(array, index);
            byte[] direct = aggregatePlan == null
                    ? null
                    : aggregatePlan.directUnionBytes(value);
            byte[] element = direct == null
                    ? encodeMemoryValue(value, elementSize, elementCodec)
                    : direct;
            if (element.length < elementSize) {
                throw new IllegalStateException("Rust aggregate storage contains "
                        + element.length + " bytes, expected " + elementSize);
            }
            System.arraycopy(element, 0, bytes, offset + index * elementSize, elementSize);
            if (elementCodec != null) {
                transferEncodedPointers(
                        element,
                        0,
                        bytes,
                        offset + (long) index * elementSize,
                        elementSize,
                        false);
                if (direct == null) {
                    moveEncodedReferences(element, bytes);
                } else {
                    transferEncodedReferences(element, bytes);
                }
            }
        }
    }

    /** Decodes Rust's contiguous, little-endian memory layout into an array. */
    public static void decodeArrayMemory(
            byte[] bytes, int offset, Object array, int elementSize, String elementCodec) {
        int length = Array.getLength(array);
        Class<?> componentType = array.getClass().getComponentType();
        if (elementCodec == null && componentType.isPrimitive()
                && elementSize == ArrayMemoryCodec.elementSize(componentType)) {
            ArrayMemoryCodec.read(bytes, offset, array);
            return;
        }
        for (int index = 0; index < length; index++) {
            Object element = decodeMemoryValue(
                    bytes, offset + index * elementSize, elementSize, elementCodec, componentType);
            arraySet(array, index, element);
        }
    }

    private static byte[] encodeMemoryValue(Object value, int size, String codec) {
        if (isFatPointerCodec(codec)) {
            return encodeFatPointer(value, size, codec);
        }
        if (codec != null
                && !MANAGED_OBJECT_VIEW_CODEC.equals(codec)
                && !isRawPointerCodec(codec)
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
            retainEncodedReference(encoded, value);
        } else if (isRawPointerCodec(codec)) {
            Pointer pointer = rawPointerCarrier(value, codec);
            bits = encodedAddress(pointer, encoded, 0, size, codec);
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
                && !isRawPointerCodec(codec)
                && !isBigIntegerCodec(codec)
                && !F128_CODEC.equals(codec)) {
            byte[] encoded = new byte[size];
            System.arraycopy(bytes, offset, encoded, 0, size);
            transferEncodedPointers(bytes, offset, encoded, 0, size, false);
            transferEncodedReferences(bytes, encoded);
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
        if (isRawPointerCodec(codec)) {
            Pointer pointer = encodedPointer(bytes, offset, size, codec, bits);
            if (isArrayReferenceCodec(codec)) {
                return decodeArrayReference(
                        pointer == null ? typedPointerObjectFromAddress(bits, codec) : pointer,
                        codec,
                        componentType);
            }
            return decodedRawPointer(
                    pointer == null ? typedPointerObjectFromAddress(bits, codec) : pointer,
                    codec);
        }
        return carrierFromBits(defaultValue(componentType), bits, size);
    }

    private static boolean isFatPointerCodec(String codec) {
        if (codec == null || codec.length() < 3 || codec.charAt(0) != '@') {
            return false;
        }
        char family = codec.charAt(1);
        if (family == 't') {
            return codec.startsWith(TRAIT_POINTER_VIEW_CODEC_PREFIX);
        }
        if (family != 's') {
            return false;
        }
        return codec.charAt(2) == 'l'
                ? codec.startsWith(SLICE_POINTER_VIEW_CODEC_PREFIX)
                : codec.startsWith(STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX);
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

    private static String[] slicePointerDescriptor(String codec) {
        // The element codec may itself be a structured fat-pointer codec, so
        // only the carrier and element-size separators are structural here.
        return splitCodecDescriptor(codec, SLICE_POINTER_VIEW_CODEC_PREFIX, 3);
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
        return splitCodecDescriptor(codec, STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX, 5);
    }

    private static String[] splitCodecDescriptor(String codec, String prefix, int partCount) {
        String[] cached = CODEC_DESCRIPTORS.get(codec);
        if (cached != null) {
            return cached;
        }
        String descriptor = codec.substring(prefix.length());
        String[] parts = new String[partCount];
        int start = 0;
        for (int index = 0; index < partCount - 1; index++) {
            int separator = descriptor.indexOf('\n', start);
            if (separator < 0) {
                throw new IllegalArgumentException("invalid Rust pointer codec descriptor");
            }
            parts[index] = descriptor.substring(start, separator);
            start = separator + 1;
        }
        parts[partCount - 1] = descriptor.substring(start);
        String[] previous = CODEC_DESCRIPTORS.putIfAbsent(codec, parts);
        return previous == null ? parts : previous;
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

    private static String structTailPointerTraitInterface(String[] descriptor) {
        String carrier = descriptor[2];
        return carrier.startsWith(STRUCT_TRAIT_TAIL_CARRIER_PREFIX)
                ? carrier.substring(STRUCT_TRAIT_TAIL_CARRIER_PREFIX.length())
                : null;
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
            try {
                pointerMetadata = sliceLogicalLength(value);
            } catch (ReflectiveOperationException error) {
                throw new IllegalArgumentException("invalid Rust slice fat pointer", error);
            }
            dataAddress = encodedAddress(data, image, 0, wordSize, codec);
        } else if (codec.startsWith(STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX)) {
            if (!(value instanceof Pointer)) {
                throw new IllegalArgumentException(
                        "Rust struct-tail pointer requires a Pointer carrier");
            }
            Pointer pointer = (Pointer) value;
            String[] descriptor = structTailPointerDescriptor(codec);
            dataAddress = encodedAddress(pointer, image, 0, wordSize, codec);
            String traitInterface = structTailPointerTraitInterface(descriptor);
            pointerMetadata = traitInterface == null
                    ? pointer.metadata()
                    : traitMetadataMarker(pointer, traitInterface).address();
        } else {
            if (!(value instanceof Pointer)) {
                throw new IllegalArgumentException(
                        "Rust raw trait-object pointer requires a Pointer carrier");
            }
            Pointer pointer = (Pointer) value;
            String metadataClass = codec.substring(TRAIT_POINTER_VIEW_CODEC_PREFIX.length());
            Pointer marker = traitMetadataMarker(pointer, metadataClass);
            dataAddress = erasedAddress(pointer);
            rememberEncodedPointer(image, 0, wordSize, codec, pointer);
            pointerMetadata = marker.address();
        }

        MemoryBytes.write(image, 0, wordSize, dataAddress);
        MemoryBytes.write(image, wordSize, wordSize, pointerMetadata);
        return image;
    }

    private static Object decodeFatPointer(
            byte[] bytes, int offset, int size, String codec) {
        int wordSize = fatPointerWordSize(size);
        long dataAddress = MemoryBytes.read(bytes, offset, wordSize);
        long pointerMetadata = MemoryBytes.read(bytes, offset + wordSize, wordSize);
        if (codec.startsWith(TRAIT_POINTER_VIEW_CODEC_PREFIX)) {
            Pointer data = encodedPointer(bytes, offset, wordSize, codec, dataAddress);
            if (data == null) {
                data = pointerObjectFromAddress(dataAddress);
            }
            // A vtable can come from a constant independently of the data.
            // Rebuild the dispatch carrier as well as the two Rust words.
            return fromRawTraitParts(data, pointerObjectFromAddress(pointerMetadata));
        }

        if (codec.startsWith(STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX)) {
            String[] descriptor = structTailPointerDescriptor(codec);
            int elementSize = structTailPointerElementSize(descriptor);
            String elementCodec = descriptor[4].isEmpty() ? null : descriptor[4];
            Pointer data = encodedPointer(bytes, offset, wordSize, codec, dataAddress);
            if (data == null) {
                data = typedPointerObjectFromAddress(dataAddress, codec);
            }
            String traitInterface = structTailPointerTraitInterface(descriptor);
            if (traitInterface == null) {
                data = data.retype(elementSize, elementCodec);
            } else {
                data = independentFieldMetadata(data);
                Pointer marker = pointerObjectFromAddress(pointerMetadata);
                TraitMetadataInfo info = TRAIT_METADATA_INFO.get(marker.numericAddress());
                data.traitMetadataMarker(marker);
                if (info != null) {
                    data.traitPointeeSize(info.size);
                    data.traitPointeeAlignment(info.alignment);
                    data.traitAdapterClassName(info.adapterClassName);
                    data.traitPointeeCodecClassName(info.pointeeCodecClassName);
                    if (data.traitMetadataCarrier() == null
                            && info.adapterClassName != null) {
                        try {
                            Pointer tailData = data.byteOffsetRetype(
                                    structTailPointerPrefixSize(descriptor),
                                    info.size,
                                    info.pointeeCodecClassName);
                            Class<?> adapter = resolvedRuntimeClass(info.adapterClassName);
                            data.traitMetadataCarrier(constructorWithArity(adapter, 1).newInstance(tailData));
                        } catch (ReflectiveOperationException error) {
                            throw new IllegalStateException(
                                    "could not reconstruct Rust trait-object tail", error);
                        }
                    }
                } else {
                    data.traitPointeeSize(marker.traitPointeeSize());
                    data.traitPointeeAlignment(marker.traitPointeeAlignment());
                    data.traitAdapterClassName(marker.traitAdapterClassName());
                    data.traitPointeeCodecClassName(marker.traitPointeeCodecClassName());
                }
            }
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
        Pointer data = encodedPointer(bytes, offset, wordSize, codec, dataAddress);
        if (data == null) {
            data = typedPointerObjectFromAddress(dataAddress, codec);
        }
        data = data.retype(elementSize, elementCodec);
        return SliceView.create(descriptor[0], data, 0, pointerMetadata);
    }

    /** Writes a JVM fat-pointer carrier using Rust's native two-word layout. */
    public static void encodeFatPointerMemory(
            Object value, byte[] bytes, int offset, int size, String codec) {
        byte[] image = encodeFatPointer(value, size, codec);
        System.arraycopy(image, 0, bytes, offset, size);
        transferEncodedPointers(image, 0, bytes, offset, size, false);
        moveEncodedReferences(image, bytes);
    }

    /** Reconstructs a JVM fat-pointer carrier from Rust's native two-word layout. */
    public static Object decodeFatPointerMemory(
            byte[] bytes, int offset, int size, String codec) {
        return decodeFatPointer(bytes, offset, size, codec);
    }

    private static boolean isRustFunctionPointer(Class<?> valueClass) {
        // Function-pointer carriers are immutable callable identities. This
        // includes the hidden classes produced by LambdaMetafactory.
        return RUST_FUNCTION_POINTER_TYPES.get(valueClass);
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
        if (codecClassName == null
                || codecClassName.length() < 8
                || codecClassName.charAt(0) != '@'
                || codecClassName.charAt(1) != 's'
                || codecClassName.charAt(2) != 't') {
            return false;
        }
        return codecClassName.charAt(7) == 'u'
                ? codecClassName.startsWith(STRUCTURAL_VIEW_CODEC_PREFIX)
                : codecClassName.startsWith(STRUCT_TAIL_VIEW_CODEC_PREFIX);
    }

    private static Class<?> resolvedClass(String className, ClassLoader loader)
            throws ClassNotFoundException {
        if (loader == null) {
            loader = RUNTIME_CLASS_LOADER;
        }
        ResolvedClassCache recent = RECENT_RESOLVED_CLASSES.get();
        Class<?> recentClass = recent.get(className, loader);
        if (recentClass != null) {
            return recentClass;
        }
        String binaryName = binaryClassName(className);
        ConcurrentHashMap<String, Class<?>> classes;
        if (loader == RUNTIME_CLASS_LOADER) {
            classes = RUNTIME_RESOLVED_CLASSES;
        } else {
            synchronized (RESOLVED_CLASSES) {
                classes = RESOLVED_CLASSES.get(loader);
                if (classes == null) {
                    classes = new ConcurrentHashMap<>();
                    RESOLVED_CLASSES.put(loader, classes);
                }
            }
        }
        Class<?> cached = classes.get(binaryName);
        if (cached != null) {
            recent.remember(className, loader, cached);
            return cached;
        }
        Class<?> resolved = Class.forName(binaryName, true, loader);
        Class<?> previous = classes.putIfAbsent(binaryName, resolved);
        Class<?> result = previous == null ? resolved : previous;
        recent.remember(className, loader, result);
        return result;
    }

    private static String binaryClassName(String className) {
        if (className.indexOf('/') < 0) {
            return className;
        }
        String cached = BINARY_CLASS_NAMES.get(className);
        if (cached != null) {
            return cached;
        }
        String converted = className.replace('/', '.');
        String previous = BINARY_CLASS_NAMES.putIfAbsent(className, converted);
        return previous == null ? converted : previous;
    }

    private static boolean matchesBinaryClassName(String className, String binaryName) {
        if (className.length() != binaryName.length()) {
            return false;
        }
        for (int index = 0; index < className.length(); index++) {
            char actual = className.charAt(index);
            char expected = binaryName.charAt(index);
            if (actual != expected && !(actual == '/' && expected == '.')) {
                return false;
            }
        }
        return true;
    }

    private static Class<?> resolvedRuntimeClass(String className)
            throws ClassNotFoundException {
        return resolvedClass(className, RUNTIME_CLASS_LOADER);
    }

    private static RustField instanceField(Class<?> owner, String name)
            throws NoSuchFieldException {
        return RustField.find(owner, name);
    }

    private static RustField optionalInstanceField(Class<?> owner, String name) {
        return RustField.optional(owner, name);
    }

    private static FieldAccess fieldAccess(Class<?> owner, String name)
            throws NoSuchFieldException {
        ConcurrentHashMap<String, FieldAccess> accesses = FIELD_ACCESSORS.get(owner);
        FieldAccess cached = accesses.get(name);
        if (cached != null) {
            return cached;
        }
        FieldAccess access = new FieldAccess(instanceField(owner, name));
        FieldAccess previous = accesses.putIfAbsent(name, access);
        return previous == null ? access : previous;
    }

    private static long sliceLogicalLength(Object slice)
            throws ReflectiveOperationException {
        return ((SliceView) slice).rustLength;
    }

    private static Object sliceBackingForArray(Object slice, Class<?> targetArrayType)
            throws ReflectiveOperationException {
        SliceView view = (SliceView) slice;
        Object backing = view.array;
        int offset = view.offset;
        int length = view.length;
        if (backing instanceof Pointer) {
            Object result = ((Pointer) backing).backingArrayRange(targetArrayType, offset, length);
            if (result != null) {
                return result;
            }
        } else {
            Object result = copyArrayRange(backing, targetArrayType, offset, length);
            if (result != null) {
                return result;
            }
        }
        throw new IllegalArgumentException("slice-tail view has no compatible array backing");
    }

    private static Object copyArrayRange(
            Object backing, Class<?> targetArrayType, int offset, int length) {
        return copyArrayRange(backing, targetArrayType, offset, length, -1);
    }

    private static Object copyArrayRange(
            Object backing, Class<?> targetArrayType, int offset, int length, int elementSize) {
        if (backing == null || !backing.getClass().isArray()
                || (targetArrayType != null && !targetArrayType.isInstance(backing))) {
            return null;
        }
        if (offset == 0 && length == Array.getLength(backing)) {
            return backing;
        }
        Class<?> component = (targetArrayType == null ? backing.getClass() : targetArrayType)
                .getComponentType();
        Object result = Array.newInstance(component, length);
        System.arraycopy(backing, offset, result, 0, length);
        int componentSize = elementSize < 0 ? inferredArrayElementSize(backing) : elementSize;
        transferEncodedPointers(
                backing,
                (long) offset * componentSize,
                result,
                0,
                Math.multiplyExact(length, componentSize),
                false);
        transferEncodedReferences(backing, result);
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
            return sliceBackingForArray(value, null);
        } catch (ReflectiveOperationException error) {
            throw new IllegalArgumentException("invalid Rust slice view", error);
        }
    }

    /** Materializes a fixed-array carrier of the exact type expected by generated bytecode. */
    public static Object arrayCarrier(Object value, String targetArrayClassName) {
        try {
            Class<?> targetArrayType = resolvedRuntimeClass(targetArrayClassName);
            if (!targetArrayType.isArray()) {
                throw new IllegalArgumentException(
                        "requested Rust fixed-array carrier is not an array type");
            }
            if (value == null || targetArrayType.isInstance(value)) {
                return value;
            }
            if (!isSliceViewCarrierType(value.getClass())) {
                throw new ClassCastException(
                        value.getClass().getName() + " is neither "
                                + targetArrayType.getName() + " nor a Rust slice view");
            }

            Class<?> sliceClass = value.getClass();
            Object backing = instanceField(sliceClass, "array").get(value);
            int offset = instanceField(sliceClass, "offset").getInt(value);
            int length = instanceField(sliceClass, "length").getInt(value);
            if (!(backing instanceof Pointer)) {
                return sliceBackingForArray(value, targetArrayType);
            }
            Object directBacking = ((Pointer) backing).backingArrayRange(
                    targetArrayType, offset, length);
            if (directBacking != null) {
                return directBacking;
            }
            Pointer first = ((Pointer) backing).add(offset);
            Class<?> component = targetArrayType.getComponentType();
            Object result = Array.newInstance(component, length);
            for (int index = 0; index < length; index++) {
                Pointer element = first.add(index);
                Object elementValue;
                if (component == boolean.class) {
                    elementValue = Boolean.valueOf(element.getBoolean());
                } else if (component == byte.class) {
                    elementValue = Byte.valueOf(element.getI8());
                } else if (component == short.class) {
                    elementValue = Short.valueOf(element.getI16());
                } else if (component == char.class) {
                    elementValue = Character.valueOf((char) element.getI16());
                } else if (component == int.class) {
                    elementValue = Integer.valueOf(element.getI32());
                } else if (component == long.class) {
                    elementValue = Long.valueOf(element.getI64());
                } else if (component == float.class) {
                    elementValue = Float.valueOf(element.getF32());
                } else if (component == double.class) {
                    elementValue = Double.valueOf(element.getF64());
                } else {
                    elementValue = element.getObjectAs(component.getName());
                }
                arraySet(result, index, elementValue);
            }
            transferEncodedReferences(backing, result);
            return result;
        } catch (ReflectiveOperationException error) {
            throw new IllegalArgumentException("invalid Rust fixed-array view", error);
        }
    }

    private static Object adaptStructuralField(Object value, Class<?> targetType)
            throws ReflectiveOperationException {
        return adaptStructuralField(value, targetType, null);
    }

    private static Object adaptStructuralField(
            Object value, Class<?> targetType, Object traitTailCarrier)
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
        if (isSliceViewCarrierType(targetType) && value.getClass().isArray()) {
            return SliceView.create(targetType, value, 0, Array.getLength(value));
        }
        if (targetType.isArray() && isSliceViewCarrierType(value.getClass())) {
            return sliceBackingForArray(value, targetType);
        }
        return constructStructuralView(value, targetType, traitTailCarrier);
    }

    private static ConstructorPlan structuralConstructor(Class<?> targetClass, int fieldCount) {
        return constructorWithArity(targetClass, fieldCount);
    }

    private static Object constructStructuralView(Object source, Class<?> targetClass) {
        return constructStructuralView(source, targetClass, null);
    }

    private static Object constructStructuralView(
            Object source, Class<?> targetClass, Object traitTailCarrier) {
        try {
            Object transparentInner = null;
            for (RustField field : PUBLIC_INSTANCE_FIELDS.get(source.getClass())) {
                if (transparentInner != null) {
                    transparentInner = null;
                    break;
                }
                transparentInner = field.get(source);
            }
            if (transparentInner != null && targetClass.isInstance(transparentInner)) {
                return transparentInner;
            }
            RustField[] targetFields = PUBLIC_INSTANCE_FIELDS.get(targetClass);
            ConstructorPlan constructor = structuralConstructor(targetClass, targetFields.length);
            Object[] args = new Object[constructor.parameterTypes.length];
            java.lang.reflect.Parameter[] parameters = constructor.reflection.getParameters();
            for (int index = 0; index < parameters.length; index++) {
                if (traitTailCarrier != null
                        && index == parameters.length - 1
                        && parameters[index].getType().isInstance(traitTailCarrier)) {
                    args[index] = traitTailCarrier;
                } else if (parameters.length == 1
                        && parameters[index].getType().isInstance(source)) {
                    args[index] = source;
                } else {
                    String fieldName = parameters[index].isNamePresent()
                            ? parameters[index].getName()
                            : targetFields[index].getName();
                    try {
                        RustField sourceField = instanceField(source.getClass(), fieldName);
                        args[index] = adaptStructuralField(
                                sourceField.get(source), parameters[index].getType(),
                                index == parameters.length - 1 ? traitTailCarrier : null);
                    } catch (NoSuchFieldException error) {
                        if (parameters.length != 1) {
                            throw error;
                        }
                        // Transparent DST wrappers can add more than one nominal
                        // layer around the same slice tail (for example OsStr).
                        args[index] = adaptStructuralField(
                                source, parameters[index].getType(), traitTailCarrier);
                    }
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
            RustField[] targetFields = PUBLIC_INSTANCE_FIELDS.get(target.getClass());
            if (targetFields.length == 1 && targetFields[0].getType().isInstance(source)) {
                targetFields[0].set(target, source);
                return;
            }
            RustField[] sourceFields = PUBLIC_INSTANCE_FIELDS.get(source.getClass());
            if (sourceFields.length == 1) {
                Object inner = sourceFields[0].get(source);
                if (inner != null && target.getClass().isInstance(inner)) {
                    copyStructuralFields(inner, target);
                    return;
                }
            }
            for (RustField targetField : targetFields) {
                RustField sourceField = instanceField(source.getClass(), targetField.getName());
                Object sourceValue = sourceField.get(source);
                Object targetValue = targetField.get(target);
                Object adapted;
                if (sourceValue != null
                        && targetValue != null
                        && sourceValue.getClass() == targetValue.getClass()
                        && hasProjectedFieldCells(targetValue)) {
                    copyStructuralFields(sourceValue, targetValue);
                    adapted = targetValue;
                } else if (sourceValue != null && targetValue != null
                        && !targetField.getType().isInstance(sourceValue)
                        && !targetField.getType().isPrimitive()
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
        if (!create && !mayHaveStructuralView(allocation)) {
            return null;
        }
        Map<Object, Map<Long, StructuralViewState>> stripe =
                stateStripe(STRUCTURAL_VIEWS, allocation);
        StructuralViewState state;
        boolean created = false;
        synchronized (stripe) {
            Map<Long, StructuralViewState> allocationViews = stripe.get(allocation);
            if (allocationViews == null) {
                if (!create) {
                    return null;
                }
                allocationViews = new HashMap<>();
                stripe.put(allocation, allocationViews);
            }
            state = allocationViews.get(byteOffset);
            if (state == null && create) {
                markStructuralView(allocation);
                state = new StructuralViewState(source);
                allocationViews.put(byteOffset, state);
                created = true;
            }
        }
        if (created) {
            maybeRebuildIdentityFilter(STRUCTURAL_VIEW_FILTER, STRUCTURAL_VIEWS);
        }
        return state;
    }

    private static void markStructuralView(Object allocation) {
        if (allocation instanceof Cell) {
            ((Cell) allocation).hasStructuralView = true;
            return;
        }
        if (allocation instanceof ReceiverCell) {
            ((ReceiverCell) allocation).hasStructuralView = true;
            return;
        }
        if (allocation instanceof FieldCell) {
            ((FieldCell) allocation).hasStructuralView = true;
            markProjectedViewParents((FieldCell) allocation);
            return;
        }
        markIdentityFilter(STRUCTURAL_VIEW_FILTER, allocation);
    }

    private static boolean mayHaveStructuralView(Object allocation) {
        if (allocation instanceof Cell) {
            return ((Cell) allocation).hasStructuralView;
        }
        if (allocation instanceof ReceiverCell) {
            return ((ReceiverCell) allocation).hasStructuralView;
        }
        if (allocation instanceof FieldCell) {
            return ((FieldCell) allocation).hasStructuralView;
        }
        return mayBeInIdentityFilter(STRUCTURAL_VIEW_FILTER, allocation);
    }

    private static boolean markIdentityFilter(AtomicLongArray filter, Object value) {
        int hash = System.identityHashCode(value);
        boolean first = markFilterHash(filter, mixRepeatedArrayHash(hash));
        boolean second =
                markFilterHash(filter, mixRepeatedArrayHash(hash ^ 0x9e37_79b9));
        return first || second;
    }

    private static boolean mayBeInIdentityFilter(AtomicLongArray filter, Object value) {
        int hash = System.identityHashCode(value);
        return hasFilterHash(filter, mixRepeatedArrayHash(hash))
                && hasFilterHash(filter, mixRepeatedArrayHash(hash ^ 0x9e37_79b9));
    }

    private static void markIdentityFilter(RebuildableIdentityFilter filter, Object value) {
        // The caller holds the owning stripe during publication.
        // Also mark the replacement filter if a rebuild has passed that stripe.
        // Recheck primary because the rebuild can publish it and clear secondary between reads.
        boolean added = false;
        AtomicLongArray primary;
        do {
            primary = filter.primary;
            added |= markIdentityFilter(primary, value);
            AtomicLongArray secondary = filter.secondary;
            if (secondary != null && secondary != primary) markIdentityFilter(secondary, value);
        } while (primary != filter.primary);
        if (added) {
            filter.marks.incrementAndGet();
        }
    }

    private static boolean mayBeInIdentityFilter(
            RebuildableIdentityFilter filter, Object value) {
        AtomicLongArray primary = filter.primary;
        if (mayBeInIdentityFilter(primary, value)) {
            return true;
        }
        AtomicLongArray secondary = filter.secondary;
        return secondary != null
                && secondary != primary
                && mayBeInIdentityFilter(secondary, value);
    }

    private static void maybeRebuildIdentityFilter(
            RebuildableIdentityFilter filter, Map<Object, ?>[] stripes) {
        if (filter.marks.get() < filter.rebuildMarks
                || !filter.rebuilding.compareAndSet(0, 1)) return;
        try {
            AtomicLongArray rebuilt = new AtomicLongArray(filter.wordCount);
            filter.secondary = rebuilt;
            for (Map<Object, ?> stripe : stripes) {
                synchronized (stripe) {
                    ((WeakIdentityMap<?>) stripe).forEachLiveKey(key -> markIdentityFilter(rebuilt, key));
                }
            }
            filter.primary = rebuilt;
            filter.secondary = null;
            filter.marks.set(0);
        } finally {
            filter.rebuilding.set(0);
        }
    }

    private void clearStructuralViewState() {
        if (!mayHaveStructuralView(allocation)) {
            return;
        }
        Map<Object, Map<Long, StructuralViewState>> stripe =
                stateStripe(STRUCTURAL_VIEWS, allocation);
        synchronized (stripe) {
            Map<Long, StructuralViewState> allocationViews = stripe.get(allocation);
            if (allocationViews != null) {
                allocationViews.remove(byteOffset);
                if (allocationViews.isEmpty()) {
                    stripe.remove(allocation);
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
                discardEncodedReferences(image);
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
        flushMemoryViewsOverlapping(offset, size, false);
    }

    private void flushMemoryViewsOverlapping(long offset, int size, boolean overwrite) {
        if (directCellHasNoMemoryViews(allocation)
                || !mayBeInIdentityFilter(MEMORY_VIEW_FILTER, allocation)) {
            return;
        }
        synchronized (atomicStripe(this)) {
            flushMemoryViewsOverlappingLocked(offset, size, overwrite);
        }
    }

    private void flushMemoryViewsOverlappingLocked(long offset, int size, boolean overwrite) {
        long observedEpoch = memoryViewEpoch(allocation);
        MemoryViewAbsenceCache absence = MEMORY_VIEW_ABSENCE.get();
        if (absence.matches(allocation, observedEpoch)) {
            return;
        }
        java.util.ArrayList<MemoryViewWriteback> pending;
        Map<Object, LongRangeMap<MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (stripe) {
            LongRangeMap<MemoryViewState> views = stripe.get(allocation);
            if (views == null) {
                setDirectCellHasMemoryView(allocation, false);
                if (memoryViewEpoch(allocation) == observedEpoch) {
                    absence.remember(allocation, observedEpoch);
                }
                return;
            }
            pending = removeOverlappingMemoryViews(views, offset, size);
            if (views.isEmpty()) {
                stripe.remove(allocation);
                setDirectCellHasMemoryView(allocation, false);
                absence.remember(allocation, memoryViewEpoch(allocation));
            }
        }
        if (pending == null) {
            return;
        }
        for (int index = 0; index < pending.size(); index++) {
            MemoryViewWriteback entry = pending.get(index);
            // A complete replacement makes the previous decoded bytes dead.
            // Partial writes still need the untouched portion of a live view.
            if (!overwrite || entry.offset < offset
                    || Math.addExact(entry.offset, entry.state.size) > Math.addExact(offset, size)) {
                writeBackMemoryView(entry.offset, entry.state);
            }
        }
    }

    private void flushAllMemoryViews() {
        if (directCellHasNoMemoryViews(allocation)
                || !mayBeInIdentityFilter(MEMORY_VIEW_FILTER, allocation)) {
            return;
        }
        synchronized (atomicStripe(this)) {
            flushAllMemoryViewsLocked();
        }
    }

    private void flushAllMemoryViewsLocked() {
        LongRangeMap<MemoryViewState> pending;
        Map<Object, LongRangeMap<MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (stripe) {
            pending = stripe.remove(allocation);
            if (pending == null) {
                setDirectCellHasMemoryView(allocation, false);
                return;
            }
            for (int index = 0; index < pending.size(); index++) {
                pending.valueAt(index).active = false;
            }
            setDirectCellHasMemoryView(allocation, false);
        }
        for (int index = 0; index < pending.size(); index++) {
            writeBackMemoryView(pending.keyAt(index), pending.valueAt(index));
        }
    }

    private void discardMemoryViewsOverlapping(long offset, int size) {
        if (directCellHasNoMemoryViews(allocation)
                || !mayBeInIdentityFilter(MEMORY_VIEW_FILTER, allocation)) {
            return;
        }
        long observedEpoch = memoryViewEpoch(allocation);
        MemoryViewAbsenceCache absence = MEMORY_VIEW_ABSENCE.get();
        if (absence.matches(allocation, observedEpoch)) {
            return;
        }
        Map<Object, LongRangeMap<MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (stripe) {
            LongRangeMap<MemoryViewState> views = stripe.get(allocation);
            if (views == null) {
                setDirectCellHasMemoryView(allocation, false);
                if (memoryViewEpoch(allocation) == observedEpoch) {
                    absence.remember(allocation, observedEpoch);
                }
                return;
            }
            removeOverlappingMemoryViews(views, offset, size);
            if (views.isEmpty()) {
                stripe.remove(allocation);
                setDirectCellHasMemoryView(allocation, false);
                absence.remember(allocation, memoryViewEpoch(allocation));
            }
        }
    }

    /**
     * Removes decoded views overlapping one range. Views are kept disjoint when
     * inserted, so the predecessor and entries beginning before the range end
     * are the only possible matches.
     */
    private static java.util.ArrayList<MemoryViewWriteback>
            removeOverlappingMemoryViews(
                    LongRangeMap<MemoryViewState> views, long offset, int size) {
        java.util.ArrayList<MemoryViewWriteback> removed = null;
        long end = Math.addExact(offset, (long) size);
        int index = views.floorIndex(offset);
        if (index < 0) {
            index = views.ceilingIndex(offset);
        }
        while (index < views.size() && views.keyAt(index) < end) {
            long entryOffset = views.keyAt(index);
            MemoryViewState state = views.valueAt(index);
            if (rangesOverlap(offset, size, entryOffset, state.size)) {
                if (removed == null) {
                    removed = new java.util.ArrayList<>();
                }
                state.active = false;
                removed.add(new MemoryViewWriteback(entryOffset, state));
                views.removeAt(index);
            } else {
                index++;
            }
        }
        return removed;
    }

    private static void deactivateMemoryViews(LongRangeMap<MemoryViewState> views) {
        if (views == null) {
            return;
        }
        for (int index = 0; index < views.size(); index++) {
            views.valueAt(index).active = false;
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
            flushMemoryViewsOverlapping(offset, size, true);
        }
    }

    private Object decodedMemoryView() {
        synchronized (atomicStripe(this)) {
            return decodedMemoryViewLocked();
        }
    }

    private Object activeBoundMemoryViewValue(int materializedSize) {
        MemoryViewState bound = boundMemoryViewState();
        if (bound != null
                && bound.active
                && bound.size == materializedSize
                && bound.codecClassName.equals(viewCodecClassName)) {
            Object transparent = transparentManagedView(bound.value.getClass());
            return transparent == null ? bound.value : transparent;
        }
        return null;
    }

    private Object decodedMemoryViewLocked() {
        int materializedSize = materializedViewSize();
        Object boundValue = activeBoundMemoryViewValue(materializedSize);
        if (boundValue != null) {
            Object value = boundValue;
            registerMemoryViewOrigin(value);
            return value;
        }
        Map<Object, LongRangeMap<MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        if (mayBeInIdentityFilter(MEMORY_VIEW_FILTER, allocation)) {
            synchronized (stripe) {
                LongRangeMap<MemoryViewState> views = stripe.get(allocation);
                MemoryViewState cached = views == null ? null : views.get(byteOffset);
                if (cached != null
                        && cached.size == viewSize
                        && cached.codecClassName.equals(viewCodecClassName)) {
                    setDirectCellHasMemoryView(allocation, true);
                    setBoundMemoryViewState(cached);
                    Object transparent = transparentManagedView(cached.value.getClass());
                    if (transparent != null) {
                        registerMemoryViewOrigin(transparent);
                        return transparent;
                    }
                    registerMemoryViewOrigin(cached.value);
                    return cached.value;
                }
            }
        }

        byte[] image = loadRange(materializedSize);
        Object decoded = decodeAggregate(viewCodecClassName, image);
        attachDecodedTraitTail(decoded);
        Object transparent = transparentManagedView(decoded.getClass());
        if (transparent != null) {
            registerMemoryViewOrigin(transparent);
            return transparent;
        }
        MemoryViewState state =
                new MemoryViewState(materializedSize, viewCodecClassName, decoded, image);
        advanceMemoryViewEpoch(allocation);
        setDirectCellHasMemoryView(allocation, true);
        synchronized (stripe) {
            LongRangeMap<MemoryViewState> views = stripe.get(allocation);
            if (views == null) {
                views = new LongRangeMap<>();
                stripe.put(allocation, views);
            }
            views.put(byteOffset, state);
            markIdentityFilter(MEMORY_VIEW_FILTER, allocation);
        }
        advanceMemoryViewEpoch(allocation);
        maybeRebuildIdentityFilter(MEMORY_VIEW_FILTER, MEMORY_VIEWS);
        setBoundMemoryViewState(state);
        registerMemoryViewOrigin(decoded);
        bindDecodedMemoryView(decoded);
        return decoded;
    }

    private void attachDecodedTraitTail(Object decoded) {
        Object carrier = traitMetadataCarrier();
        if (decoded == null || carrier == null) {
            return;
        }
        RustField[] fields = PUBLIC_INSTANCE_FIELDS.get(decoded.getClass());
        if (fields.length == 0) {
            return;
        }
        RustField tail = fields[fields.length - 1];
        if (!tail.getType().isInstance(carrier)) {
            return;
        }
        try {
            tail.set(decoded, carrier);
        } catch (IllegalAccessException error) {
            throw new IllegalStateException("could not attach Rust trait-object tail", error);
        }
    }

    private void registerMemoryViewOrigin(Object value) {
        if (value == null || allocation == null) {
            return;
        }
        MemoryViewOrigin previous;
        Map<Object, MemoryViewOrigin> stripe = stateStripe(MEMORY_VIEW_ORIGINS, value);
        synchronized (stripe) {
            previous = stripe.get(value);
            if (previous != null && previous.matches(this)) {
                return;
            }
            markIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, value);
            previous = stripe.put(value, new MemoryViewOrigin(this));
        }
        maybeRebuildIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, MEMORY_VIEW_ORIGINS);
        Object previousAllocation = previous == null ? null : previous.allocation.get();
        if (previousAllocation instanceof FieldCell && previousAllocation != allocation) {
            removeMemoryOriginView(previousAllocation, value);
        }
        if (!(allocation instanceof FieldCell)) {
            return;
        }
        Map<Object, Map<Object, Boolean>> reverseStripe =
                stateStripe(MEMORY_ORIGIN_VIEWS, allocation);
        synchronized (reverseStripe) {
            Map<Object, Boolean> views = reverseStripe.get(allocation);
            if (views == null) {
                views = new WeakIdentityMap<>();
                reverseStripe.put(allocation, views);
            }
            views.put(value, Boolean.TRUE);
            ((FieldCell) allocation).hasMemoryOrigins = true;
            markProjectedViewParents((FieldCell) allocation);
        }
    }

    private static void removeMemoryOriginView(Object allocation, Object value) {
        Map<Object, Map<Object, Boolean>> stripe =
                stateStripe(MEMORY_ORIGIN_VIEWS, allocation);
        synchronized (stripe) {
            Map<Object, Boolean> views = stripe.get(allocation);
            if (views == null) {
                return;
            }
            views.remove(value);
            if (views.isEmpty()) {
                stripe.remove(allocation);
                ((FieldCell) allocation).hasMemoryOrigins = false;
            }
        }
    }

    private static void discardMemoryOriginsForAllocation(Object allocation) {
        Map<Object, Map<Object, Boolean>> reverseStripe =
                stateStripe(MEMORY_ORIGIN_VIEWS, allocation);
        java.util.List<Object> views;
        synchronized (reverseStripe) {
            Map<Object, Boolean> indexed = reverseStripe.remove(allocation);
            ((FieldCell) allocation).hasMemoryOrigins = false;
            if (indexed == null || indexed.isEmpty()) {
                return;
            }
            views = new java.util.ArrayList<>(indexed.keySet());
        }
        for (Object view : views) {
            Map<Object, MemoryViewOrigin> originStripe =
                    stateStripe(MEMORY_VIEW_ORIGINS, view);
            synchronized (originStripe) {
                MemoryViewOrigin origin = originStripe.get(view);
                if (origin != null && origin.allocation.get() == allocation) {
                    originStripe.remove(view);
                }
            }
        }
    }

    private void bindDecodedMemoryView(Object value) {
        if (value == null
                || viewCodecClassName == null
                || isBuiltInCodec(viewCodecClassName)) {
            return;
        }
        CodecCalls.Binder bind = codecPlan(viewCodecClassName).bind();
        if (bind == null) {
            return;
        }
        try {
            bind.bind(this, value);
        } catch (Throwable error) {
            if (error instanceof RuntimeException || error instanceof Error) {
                rethrowUnchecked(error);
            }
            throw new IllegalStateException(
                    "could not bind nested Rust memory view " + viewCodecClassName, error);
        }
    }

    /** Binds a nested aggregate carrier to its canonical Rust memory range. */
    public void bindNestedMemoryView(
            Object value, long relativeOffset, long size, String codecClassName) {
        if (value == null || size == 0) {
            return;
        }
        Pointer nested = byteOffsetRetype(relativeOffset, size, codecClassName);
        nested.registerMemoryViewOrigin(value);
        nested.bindDecodedMemoryView(value);
    }

    /** Binds reference-valued fixed-array elements to their Rust memory slots. */
    public void bindArrayMemoryViews(Object array, long elementSize, String elementCodecClassName) {
        if (array == null
                || !array.getClass().isArray()
                || elementSize == 0
                || elementCodecClassName == null
                || isBuiltInCodec(elementCodecClassName)) {
            return;
        }
        int length = Array.getLength(array);
        for (int index = 0; index < length; index++) {
            Object value = arrayGet(array, index);
            if (value == null) {
                continue;
            }
            Pointer element = byteOffsetRetype(
                    Math.multiplyExact((long) index, elementSize),
                    elementSize,
                    elementCodecClassName);
            element.registerMemoryViewOrigin(value);
            element.bindDecodedMemoryView(value);
        }
    }

    /** Propagates a projected field write through its decoded aggregate view. */
    private static void commitOriginMemoryView(Object value) {
        if (value == null || !mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, value)) {
            return;
        }
        MemoryViewOrigin origin;
        Map<Object, MemoryViewOrigin> originStripe = stateStripe(MEMORY_VIEW_ORIGINS, value);
        synchronized (originStripe) {
            origin = originStripe.get(value);
        }
        if (origin == null) {
            return;
        }
        Object allocation = origin.allocation.get();
        if (allocation == null) {
            return;
        }
        long stateOffset;
        MemoryViewState state;
        Map<Object, LongRangeMap<MemoryViewState>> viewStripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (viewStripe) {
            LongRangeMap<MemoryViewState> views = viewStripe.get(allocation);
            int enclosing = views == null ? -1 : views.floorIndex(origin.byteOffset);
            stateOffset = enclosing < 0 ? -1 : views.keyAt(enclosing);
            state = enclosing < 0 ? null : views.valueAt(enclosing);
        }
        if (state == null
                || stateOffset > origin.byteOffset
                || Math.addExact(origin.byteOffset, origin.viewSize)
                        > Math.addExact(stateOffset, (long) state.size)) {
            return;
        }
        Pointer pointer = new Pointer(
                        allocation,
                        origin.allocationElementSize,
                        stateOffset,
                        state.size,
                        origin.allocationCodecClassName,
                        state.codecClassName,
                        -1)
                .withMetadata(origin.metadata);
        pointer.setBoundMemoryViewState(state);
        pointer.commitMemoryView();
    }

    /** Propagates a direct generated-field write through a decoded memory view. */
    public static void commitFieldOwner(Object owner) {
        discardProjectedFieldViews(owner);
        commitOriginMemoryView(owner);
    }

    /** Commits direct field mutations made through the current decoded view. */
    public void commitMemoryView() {
        discardProjectedFieldViews(managedViewObject());
        // rootField projections belong to the replaceable storage cell, while
        // direct generated writes mutate the object held by that cell.
        discardProjectedFieldViews(allocation);
        if (boundMemoryViewState() == null) {
            // Direct JVM aggregate carriers are already authoritative. Drop
            // byte-projected field views decoded before the mutation so a
            // later load cannot reuse stale pointer or scalar field state.
            if (allocation != null && viewSize > 0 && hasStableManagedCarrier()) {
                discardMemoryViewsOverlapping(byteOffset, materializedViewSize());
            }
            // An enum payload can be projected from a decoded carrier without
            // decoding this field separately. Nested direct writes still need
            // to reach the enclosing enum's original byte storage.
            if (allocation instanceof FieldCell) {
                ((FieldCell) allocation).commitOwners();
            }
            return;
        }
        if (allocation == null || viewCodecClassName == null) {
            return;
        }
        synchronized (atomicStripe(this)) {
            commitMemoryViewLocked();
        }
    }

    private void commitMemoryViewLocked() {
        MemoryViewState state;
        Map<Object, LongRangeMap<MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (stripe) {
            LongRangeMap<MemoryViewState> views = stripe.get(allocation);
            state = views == null ? null : views.get(byteOffset);
            if (state == null
                    || state != boundMemoryViewState()
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
        advanceMemoryViewEpoch(allocation);
        synchronized (stripe) {
            LongRangeMap<MemoryViewState> views = stripe.get(allocation);
            if (views == null) {
                views = new LongRangeMap<>();
                stripe.put(allocation, views);
            }
            if (!views.containsKey(byteOffset)) {
                views.put(byteOffset, state);
            }
            state.active = true;
            setDirectCellHasMemoryView(allocation, true);
        }
        advanceMemoryViewEpoch(allocation);
        registerMemoryViewOrigin(state.value);
        bindDecodedMemoryView(state.value);
    }

    private Object managedViewObject() {
        MemoryViewState bound = boundMemoryViewState();
        if (bound != null) {
            return bound.value;
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

    // Runtime-owned cells can answer exactly without saturating the shared
    // identity filter as short-lived field pointers accumulate in long runs.
    private static boolean mayHaveFieldCells(Object owner) {
        if (owner instanceof Cell) {
            return ((Cell) owner).fields != null;
        }
        if (owner instanceof FieldCell) {
            return ((FieldCell) owner).fields != null;
        }
        return owner != null && mayBeInIdentityFilter(FIELD_CELL_FILTER, owner);
    }

    private static final class FieldCellCache extends WeakReference<FieldCell> {
        private volatile FieldCellCache next;

        private FieldCellCache(FieldCell field, FieldCellCache next) {
            super(field);
            this.next = next;
        }
    }

    private static FieldCellCache ownedFields(Object owner) {
        return owner instanceof Cell ? ((Cell) owner).fields : ((FieldCell) owner).fields;
    }

    /** Keep cached field cells weak. A decoded value can refer to its owner.
     * A strong field cache would retain that cycle through the global view map. */
    private static FieldCell ownedField(Object owner, FieldAccess access) {
        for (FieldCellCache entry = ownedFields(owner); entry != null; entry = entry.next) {
            FieldCell field = entry.get();
            if (field != null && (field.access == access || field.access.cacheKey.equals(access.cacheKey)))
                return field;
        }
        synchronized (owner) {
            FieldCellCache head = ownedFields(owner), previous = null;
            for (FieldCellCache entry = head; entry != null; entry = entry.next) {
                FieldCell field = entry.get();
                if (field != null) {
                    if (field.access == access || field.access.cacheKey.equals(access.cacheKey)) return field;
                    previous = entry;
                } else if (previous == null) {
                    head = entry.next;
                } else {
                    // Keep the next link for readers that still hold this dead entry.
                    previous.next = entry.next;
                }
            }
            FieldCell field = new FieldCell(owner, access, true);
            FieldCellCache entry = new FieldCellCache(field, head);
            if (owner instanceof Cell) ((Cell) owner).fields = entry;
            else ((FieldCell) owner).fields = entry;
            return field;
        }
    }

    // Only view creation marks ancestors. Ordinary typed field projections
    // need no invalidation traversal until a descendant has a decoded view.
    // Marks remain conservative after invalidation, avoiding subtree counts
    // and any work on the common projection path.
    private static void markProjectedViewParents(FieldCell field) {
        Object parent = field.rootOwner;
        while (parent instanceof FieldCell) {
            FieldCell cell = (FieldCell) parent;
            cell.hasProjectedViews = true;
            parent = cell.rootOwner;
        }
        if (parent instanceof Cell) {
            ((Cell) parent).hasProjectedViews = true;
        }
    }

    private static boolean mayHaveProjectedViews(Object owner) {
        if (owner instanceof Cell) {
            return ((Cell) owner).hasProjectedViews;
        }
        if (owner instanceof FieldCell) {
            return ((FieldCell) owner).hasProjectedViews;
        }
        return mayHaveFieldCells(owner);
    }

    private static java.util.List<FieldCell> collectProjectedFieldViews(
            Object owner, java.util.List<FieldCell> projected) {
        if (!mayHaveProjectedViews(owner)) {
            return projected;
        }
        if (owner instanceof Cell || owner instanceof FieldCell) {
            for (FieldCellCache entry = ownedFields(owner); entry != null; entry = entry.next) {
                projected = collectProjectedFieldView(entry.get(), projected);
            }
            return projected;
        }
        Map<Object, Map<String, WeakReference<FieldCell>>> fieldStripe =
                stateStripe(FIELD_CELLS, owner);
        synchronized (fieldStripe) {
            Map<String, WeakReference<FieldCell>> fields = fieldStripe.get(owner);
            if (fields == null) {
                return projected;
            }
            for (WeakReference<FieldCell> reference : fields.values()) {
                projected = collectProjectedFieldView(reference.get(), projected);
            }
        }
        return projected;
    }

    private static java.util.List<FieldCell> collectProjectedFieldView(
            FieldCell cell, java.util.List<FieldCell> projected) {
        if (cell != null && (cell.hasStructuralView || cell.hasMemoryView
                || cell.hasMemoryOrigins || cell.hasProjectedViews)) {
            if (projected == null) projected = new java.util.ArrayList<>();
            projected.add(cell);
        }
        return projected;
    }

    private static void discardProjectedFieldViews(Object owner) {
        java.util.List<FieldCell> projected = collectProjectedFieldViews(owner, null);
        if (projected == null) {
            return;
        }
        // Nested projections retain their enclosing field cell. Replacing a
        // parent invalidates decoded views at every descendant, including when
        // the parent itself never needed a decoded view.
        for (int index = 0; index < projected.size(); index++) {
            collectProjectedFieldViews(projected.get(index), projected);
        }
        for (FieldCell cell : projected) {
            Map<Object, LongRangeMap<MemoryViewState>> stripe =
                    stateStripe(MEMORY_VIEWS, cell);
            synchronized (stripe) {
                deactivateMemoryViews(stripe.remove(cell));
                setDirectCellHasMemoryView(cell, false);
            }
        }
        for (FieldCell cell : projected) {
            Map<Object, Map<Long, StructuralViewState>> stripe =
                    stateStripe(STRUCTURAL_VIEWS, cell);
            synchronized (stripe) {
                stripe.remove(cell);
                cell.hasStructuralView = false;
            }
        }
        for (FieldCell cell : projected) {
            discardMemoryOriginsForAllocation(cell);
        }
    }

    private static boolean hasProjectedFieldCells(Object owner) {
        if (owner instanceof Cell || owner instanceof FieldCell) {
            for (FieldCellCache entry = ownedFields(owner); entry != null; entry = entry.next) {
                if (entry.get() != null) return true;
            }
            return false;
        }
        if (!mayHaveFieldCells(owner)) {
            return false;
        }
        Map<Object, Map<String, WeakReference<FieldCell>>> stripe =
                stateStripe(FIELD_CELLS, owner);
        synchronized (stripe) {
            Map<String, WeakReference<FieldCell>> fields = stripe.get(owner);
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
            for (RustField field : PUBLIC_INSTANCE_FIELDS.get(owner.getClass())) {
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
        return splitCodecDescriptor(viewCodecClassName, STRUCTURAL_VIEW_CODEC_PREFIX, 3);
    }

    private String[] structTailViewDescriptor() {
        return splitCodecDescriptor(viewCodecClassName, STRUCT_TAIL_VIEW_CODEC_PREFIX, 5);
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
        String traitInterface = structTailPointerTraitInterface(descriptor);
        Object managed = managedViewObject();
        if (traitInterface != null) {
            try {
                Class<?> targetClass = resolvedRuntimeClass(descriptor[0]);
                Object prefix = managed;
                if (prefix == null || isSliceViewCarrierType(prefix.getClass())) {
                    long prefixSize = structTailPointerPrefixSize(descriptor);
                    String prefixCodec = descriptor[4].isEmpty() ? null : descriptor[4];
                    prefix = retype(prefixSize, prefixCodec).getObject();
                }
                return constructStructuralView(prefix, targetClass, traitMetadataCarrier());
            } catch (ClassNotFoundException error) {
                throw new IllegalStateException(
                        "could not load Rust trait-tailed view " + descriptor[0], error);
            }
        }
        if (managed != null && !isSliceViewCarrierType(managed.getClass())) {
            try {
                Class<?> targetClass = resolvedClass(descriptor[0], managed.getClass().getClassLoader());
                if (targetClass.isInstance(managed)) {
                    return managed;
                }
                return constructStructuralView(managed, targetClass);
            } catch (ClassNotFoundException error) {
                throw new IllegalStateException(
                        "could not load Rust struct-tail view " + descriptor[0], error);
            }
        }
        long prefixSize = structTailPointerPrefixSize(descriptor);
        int elementSize = structTailPointerElementSize(descriptor);
        String elementCodec = descriptor[4].isEmpty() ? null : descriptor[4];
        Pointer data = byteOffsetRetype(prefixSize, elementSize, elementCodec);
        return SliceView.create(descriptor[2], data, 0, metadata());
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
        return binaryClassName(name);
    }

    private Object structuralViewObject() {
        String targetClassName = structuralTargetClassName();
        Object source = structuralSourceObject();
        try {
            ClassLoader loader = source.getClass().getClassLoader();
            Class<?> targetClass = resolvedClass(targetClassName, loader);
            return structuralViewState(source, true)
                    .activate(targetClass, traitMetadataCarrier());
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
                return sliceLogicalLength(value);
            }
            for (RustField field : PUBLIC_INSTANCE_FIELDS.get(value.getClass())) {
                if (!mayContainStructuralMetadata(field.getType())) {
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

    private static boolean mayContainStructuralMetadata(Class<?> type) {
        Boolean cached = STRUCTURAL_METADATA_TYPES.get(type);
        if (cached != null) {
            return cached.booleanValue();
        }
        return mayContainStructuralMetadata(
                type,
                java.util.Collections.newSetFromMap(new IdentityHashMap<>()),
                new boolean[1]);
    }

    private static boolean mayContainStructuralMetadata(
            Class<?> type, Set<Class<?>> visiting, boolean[] encounteredCycle) {
        Boolean cached = STRUCTURAL_METADATA_TYPES.get(type);
        if (cached != null) {
            return cached.booleanValue();
        }
        if (isSliceViewCarrierType(type)) {
            STRUCTURAL_METADATA_TYPES.putIfAbsent(type, Boolean.TRUE);
            return true;
        }
        if (type.isPrimitive()
                || type.isArray()
                || type == Object.class
                || type == String.class
                || type == Pointer.class
                || Number.class.isAssignableFrom(type)
                || type == Boolean.class
                || type == Character.class) {
            return false;
        }
        if (!visiting.add(type)) {
            encounteredCycle[0] = true;
            return false;
        }
        boolean result = false;
        for (RustField field : PUBLIC_INSTANCE_FIELDS.get(type)) {
            if (mayContainStructuralMetadata(
                    field.getType(), visiting, encounteredCycle)) {
                result = true;
                break;
            }
        }
        visiting.remove(type);
        if (result || !encounteredCycle[0]) {
            STRUCTURAL_METADATA_TYPES.putIfAbsent(type, Boolean.valueOf(result));
        }
        return result;
    }

    static class Cell {
        Object value;
        private volatile boolean hasStructuralView;
        private volatile boolean hasMemoryView;
        private volatile FieldCellCache fields;
        private volatile boolean hasProjectedViews;

        Cell(Object value) {
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
        private volatile boolean hasStructuralView;
        private volatile boolean hasMemoryView;

        private ReceiverCell(Object value) {
            if (value == null) {
                throw new NullPointerException("Rust instance receiver cannot be null");
            }
            this.value = value;
        }
    }

    private static Object memoryViewAllocation(Object value) {
        if (!mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, value)) {
            return null;
        }
        Map<Object, MemoryViewOrigin> stripe = stateStripe(MEMORY_VIEW_ORIGINS, value);
        synchronized (stripe) {
            MemoryViewOrigin origin = stripe.get(value);
            return origin == null ? null : origin.allocation.get();
        }
    }

    private static final class FieldCell {
        // Share only the same immutable origin and offset.
        private volatile AddressOriginState cachedAddressOrigin;
        // Cache thin projections only. Exclude published addresses, decoded views, and dynamic metadata.
        private volatile Pointer cachedProjection;
        private final Object fixedOwner;
        private final Object rootOwner;
        // Direct enum-payload borrows have no parent Pointer. Retain the
        // decoded owner's storage while this field cell remains reachable.
        private final Object memoryBacking;
        private final FieldAccess access;
        private volatile FieldCellCache fields;
        private volatile boolean hasProjectedViews;
        private volatile boolean hasMemoryOrigins;
        private final int fieldNameHash;
        private volatile boolean hasStructuralView;
        private volatile boolean hasMemoryView;

        private FieldCell(Object owner, FieldAccess access) {
            this(owner, access, false);
        }

        private FieldCell(Object owner, FieldAccess access, boolean rooted) {
            fixedOwner = rooted ? null : owner;
            rootOwner = rooted ? owner : null;
            memoryBacking = rooted ? null : memoryViewAllocation(owner);
            this.access = access;
            fieldNameHash = access.fieldNameHash;
        }

        private Object owner() {
            if (rootOwner == null) {
                return fixedOwner;
            }
            return rootOwner instanceof Cell
                    ? ((Cell) rootOwner).value
                    : ((FieldCell) rootOwner).get();
        }

        private Object ownerIdentity() {
            return rootOwner == null ? fixedOwner : rootOwner;
        }

        private Object get() {
            try {
                Object owner = owner();
                Object value = (Object) access.getter.invokeExact(owner);
                return value;
            } catch (RuntimeException | Error error) {
                throw error;
            } catch (Throwable error) {
                throw new IllegalStateException("could not read Rust field pointer", error);
            }
        }

        private void set(Object value) {
            Object owner = owner();
            try {
                access.setter.invokeExact(owner, value);
            } catch (RuntimeException | Error error) {
                throw error;
            } catch (Throwable error) {
                throw new IllegalStateException("could not write Rust field pointer", error);
            }
            // Replacing one field invalidates its descendants, not live views
            // of sibling fields (for example an inline vector's data when its
            // length changes before writing a newly inserted element).
            discardProjectedFieldViews(this);
            commitOwners();
        }

        private void commitOwners() {
            // An enum payload can belong to a decoded enum that owns the byte storage.
            // Intermediate field carriers need not have registered views.
            FieldCell current = this;
            while (true) {
                Object owner = current.owner();
                commitOriginMemoryView(owner);
                if (current.rootOwner instanceof FieldCell) {
                    current = (FieldCell) current.rootOwner;
                } else {
                    Object identity = current.ownerIdentity();
                    if (identity != owner) commitOriginMemoryView(identity);
                    return;
                }
            }
        }

    }

    private static final class FieldAccess {
        private final RustField field;
        private final MethodHandle getter;
        private final MethodHandle setter;
        private final int fieldNameHash;
        private final boolean primitive;
        private final boolean borrowed;
        // Reuse field metadata to avoid hashing generated class names on each projection.
        private final String cacheKey;

        private FieldAccess(RustField field) {
            this.field = field;
            cacheKey = field.getDeclaringClass().getName() + '\n' + field.getName();
            fieldNameHash = field.getName().hashCode();
            primitive = field.getType().isPrimitive();
            borrowed = field.isBorrowed();
            try {
                getter = field.getter().asType(
                        MethodType.methodType(Object.class, Object.class));
                setter = field.setter().asType(
                        MethodType.methodType(void.class, Object.class, Object.class));
            } catch (IllegalAccessException error) {
                throw new IllegalStateException("could not access Rust field pointer", error);
            }
        }
    }

    private static final class AllocationInfo {
        private Long base;
        private int alignment = 16;
        private boolean rangePublished;
    }

    private static final class AllocationRange {
        private final AllocationReference allocation;
        private final int elementSize;
        private final long capacity;
        private final String codecClassName;

        private AllocationRange(
                long base,
                Object allocation,
                int elementSize,
                long capacity,
                String codecClassName) {
            this.allocation =
                    new AllocationReference(allocation, base, ALLOCATION_RANGE_QUEUE);
            this.elementSize = elementSize;
            this.capacity = capacity;
            this.codecClassName = codecClassName;
        }
    }

    private static final class AllocationReference extends WeakReference<Object> {
        private final long base;

        private AllocationReference(
                Object allocation, long base, ReferenceQueue<Object> queue) {
            super(allocation, queue);
            this.base = base;
        }
    }

    private static final class TraitMetadataInfo {
        private final long size;
        private final long alignment;
        private final String adapterClassName;
        private final String pointeeCodecClassName;

        private TraitMetadataInfo(
                long size,
                long alignment,
                String adapterClassName,
                String pointeeCodecClassName) {
            this.size = size;
            this.alignment = alignment;
            this.adapterClassName = adapterClassName;
            this.pointeeCodecClassName = pointeeCodecClassName;
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
        private final long exposedAddress;
        private final String codecClassName;
        private final long viewSize;
        private final String viewCodecClassName;
        private final long metadata;
        private final long zeroSizedSourceViewSize;
        private final String zeroSizedSourceViewCodecClassName;
        private final Object traitObjectCarrier;
        private final Object traitMetadataCarrier;
        private final Pointer traitMetadataMarker;
        private final long traitPointeeSize;
        private final long traitPointeeAlignment;
        private final String traitAdapterClassName;
        private final String traitPointeeCodecClassName;
        private final Pointer addressOrigin;
        private final long addressOriginOffset;

        private ExposedTarget(
                Object allocation,
                int allocationElementSize,
                long byteOffset,
                long exposedAddress,
                String codecClassName,
                long viewSize,
                String viewCodecClassName,
                long metadata,
                long zeroSizedSourceViewSize,
                String zeroSizedSourceViewCodecClassName,
                Object traitObjectCarrier,
                Object traitMetadataCarrier,
                Pointer traitMetadataMarker,
                long traitPointeeSize,
                long traitPointeeAlignment,
                String traitAdapterClassName,
                String traitPointeeCodecClassName,
                Pointer addressOrigin,
                long addressOriginOffset) {
            this.allocation = allocation;
            this.allocationElementSize = allocationElementSize;
            this.byteOffset = byteOffset;
            this.exposedAddress = exposedAddress;
            this.codecClassName = codecClassName;
            this.viewSize = viewSize;
            this.viewCodecClassName = viewCodecClassName;
            this.metadata = metadata;
            this.zeroSizedSourceViewSize = zeroSizedSourceViewSize;
            this.zeroSizedSourceViewCodecClassName = zeroSizedSourceViewCodecClassName;
            this.traitObjectCarrier = traitObjectCarrier;
            this.traitMetadataCarrier = traitMetadataCarrier;
            this.traitMetadataMarker = traitMetadataMarker;
            this.traitPointeeSize = traitPointeeSize;
            this.traitPointeeAlignment = traitPointeeAlignment;
            this.traitAdapterClassName = traitAdapterClassName;
            this.traitPointeeCodecClassName = traitPointeeCodecClassName;
            this.addressOrigin = addressOrigin;
            this.addressOriginOffset = addressOriginOffset;
        }

        private ExposedTarget withAllocation(Object replacement) {
            return new ExposedTarget(
                    replacement,
                    allocationElementSize,
                    byteOffset,
                    exposedAddress,
                    codecClassName,
                    viewSize,
                    viewCodecClassName,
                    metadata,
                    zeroSizedSourceViewSize,
                    zeroSizedSourceViewCodecClassName,
                    traitObjectCarrier,
                    traitMetadataCarrier,
                    traitMetadataMarker,
                    traitPointeeSize,
                    traitPointeeAlignment,
                    traitAdapterClassName,
                    traitPointeeCodecClassName,
                    addressOrigin,
                    addressOriginOffset);
        }

        private boolean matches(Pointer pointer) {
            return allocation == pointer.allocation
                    && allocationElementSize == pointer.allocationElementSize
                    && byteOffset == pointer.byteOffset
                    && exposedAddress == pointer.exposedAddress
                    && java.util.Objects.equals(codecClassName, pointer.allocationCodecClassName)
                    && viewSize == pointer.viewSize
                    && java.util.Objects.equals(viewCodecClassName, pointer.viewCodecClassName)
                    && metadata == pointer.metadata
                    && zeroSizedSourceViewSize == pointer.zeroSizedSourceViewSize()
                    && java.util.Objects.equals(
                            zeroSizedSourceViewCodecClassName,
                            pointer.zeroSizedSourceViewCodecClassName())
                    && traitObjectCarrier == pointer.traitObjectCarrier()
                    && traitMetadataCarrier == pointer.traitMetadataCarrier()
                    && traitMetadataMarker == pointer.traitMetadataMarker()
                    && traitPointeeSize == pointer.traitPointeeSize()
                    && traitPointeeAlignment == pointer.traitPointeeAlignment()
                    && java.util.Objects.equals(
                            traitAdapterClassName, pointer.traitAdapterClassName())
                    && java.util.Objects.equals(
                            traitPointeeCodecClassName, pointer.traitPointeeCodecClassName())
                    && addressOrigin == pointer.addressOrigin()
                    && addressOriginOffset == pointer.addressOriginOffset();
        }
    }

    private static final class TypedExposedReference extends WeakReference<ExposedTarget> {
        private final long address;
        private final String codecClassName;

        private TypedExposedReference(
                long address, String codecClassName, ExposedTarget target) {
            super(target, TYPED_EXPOSED_TARGET_QUEUE);
            this.address = address;
            this.codecClassName = codecClassName;
        }
    }

    private static final class TypedExposedEntry {
        private String codecClassName;
        private TypedExposedReference target;
        private Map<String, TypedExposedReference> alternatives;

        private TypedExposedEntry(
                long address, String codecClassName, ExposedTarget target) {
            this.codecClassName = codecClassName;
            this.target = new TypedExposedReference(address, codecClassName, target);
        }

        private synchronized void put(
                long address, String codec, ExposedTarget exposedTarget) {
            if (codec.equals(codecClassName)) {
                if (target == null || target.get() != exposedTarget) {
                    target = new TypedExposedReference(address, codec, exposedTarget);
                }
                return;
            }
            if (alternatives == null) {
                alternatives = new HashMap<>();
            }
            TypedExposedReference current = alternatives.get(codec);
            if (current == null || current.get() != exposedTarget) {
                alternatives.put(
                        codec,
                        new TypedExposedReference(address, codec, exposedTarget));
            }
        }

        private synchronized ExposedTarget get(String codec) {
            TypedExposedReference reference = codec.equals(codecClassName)
                    ? target
                    : alternatives == null ? null : alternatives.get(codec);
            return reference == null ? null : reference.get();
        }

        private synchronized void remove(TypedExposedReference reference) {
            if (target == reference) {
                target = null;
            } else if (alternatives != null
                    && alternatives.get(reference.codecClassName) == reference) {
                alternatives.remove(reference.codecClassName);
                if (alternatives.isEmpty()) {
                    alternatives = null;
                }
            }
        }

        private synchronized void removeAllocation(Object allocation) {
            ExposedTarget primary = target == null ? null : target.get();
            if (primary != null && primary.allocation == allocation) {
                target = null;
            }
            if (alternatives != null) {
                alternatives.values().removeIf(reference -> {
                    ExposedTarget candidate = reference.get();
                    return candidate == null || candidate.allocation == allocation;
                });
                if (alternatives.isEmpty()) {
                    alternatives = null;
                }
            }
        }

        private synchronized boolean isEmpty() {
            if (target != null && target.get() == null) {
                target = null;
            }
            if (alternatives != null) {
                alternatives.values().removeIf(reference -> reference.get() == null);
                if (alternatives.isEmpty()) {
                    alternatives = null;
                }
            }
            return (target == null || target.get() == null)
                    && (alternatives == null || alternatives.isEmpty());
        }
    }

    private final Object allocation;
    private final int allocationElementSize;
    private final long byteOffset;
    private final long viewSize;
    private final String allocationCodecClassName;
    private final String viewCodecClassName;
    private final long exposedAddress;
    private volatile AddressOriginState addressState;
    private volatile RarePointerState rareState;
    private long metadata = -1;

    /** Immutable provenance can be shared without coupling derived views. */
    private static final class AddressOriginState {
        private final Pointer addressOrigin;
        private final long addressOriginOffset;

        private AddressOriginState(Pointer origin, long offset) {
            addressOrigin = origin;
            addressOriginOffset = offset;
        }
    }

    private static final class RarePointerState {
        private volatile long publishedAddress = Long.MIN_VALUE;
        private volatile ExposedTarget publishedTarget;
        private MemoryViewState boundMemoryViewState;
        private long erasedAddressToken = -1;
        private long zeroSizedSourceViewSize = -1;
        private String zeroSizedSourceViewCodecClassName;
        private Object traitObjectCarrier;
        private Object traitMetadataCarrier;
        private Pointer traitMetadataMarker;
        private long traitPointeeSize = -1;
        private long traitPointeeAlignment = -1;
        private String traitAdapterClassName;
        private String traitPointeeCodecClassName;
    }

    private RarePointerState mutableRareState() {
        RarePointerState current = rareState;
        if (current != null) {
            return current;
        }
        synchronized (this) {
            if (rareState == null) {
                rareState = new RarePointerState();
            }
            return rareState;
        }
    }

    private Object traitObjectCarrier() {
        RarePointerState current = rareState;
        return current == null ? null : current.traitObjectCarrier;
    }

    private void traitObjectCarrier(Object value) {
        RarePointerState current = rareState;
        if (current == null && value == null) {
            return;
        }
        mutableRareState().traitObjectCarrier = value;
    }

    private Object traitMetadataCarrier() {
        RarePointerState current = rareState;
        return current == null ? null : current.traitMetadataCarrier;
    }

    private void traitMetadataCarrier(Object value) {
        RarePointerState current = rareState;
        if (current == null && value == null) {
            return;
        }
        mutableRareState().traitMetadataCarrier = value;
    }

    private Pointer traitMetadataMarker() {
        RarePointerState current = rareState;
        return current == null ? null : current.traitMetadataMarker;
    }

    private void traitMetadataMarker(Pointer value) {
        RarePointerState current = rareState;
        if (current == null && value == null) {
            return;
        }
        mutableRareState().traitMetadataMarker = value;
    }

    private long traitPointeeSize() {
        RarePointerState current = rareState;
        return current == null ? -1 : current.traitPointeeSize;
    }

    private void traitPointeeSize(long value) {
        RarePointerState current = rareState;
        if (current == null && value == -1) {
            return;
        }
        mutableRareState().traitPointeeSize = value;
    }

    private long traitPointeeAlignment() {
        RarePointerState current = rareState;
        return current == null ? -1 : current.traitPointeeAlignment;
    }

    private void traitPointeeAlignment(long value) {
        RarePointerState current = rareState;
        if (current == null && value == -1) {
            return;
        }
        mutableRareState().traitPointeeAlignment = value;
    }

    private String traitAdapterClassName() {
        RarePointerState current = rareState;
        return current == null ? null : current.traitAdapterClassName;
    }

    private void traitAdapterClassName(String value) {
        RarePointerState current = rareState;
        if (current == null && value == null) {
            return;
        }
        mutableRareState().traitAdapterClassName = value;
    }

    private String traitPointeeCodecClassName() {
        RarePointerState current = rareState;
        return current == null ? null : current.traitPointeeCodecClassName;
    }

    private void traitPointeeCodecClassName(String value) {
        RarePointerState current = rareState;
        if (current == null && value == null) {
            return;
        }
        mutableRareState().traitPointeeCodecClassName = value;
    }

    private Pointer addressOrigin() {
        AddressOriginState current = addressState;
        return current == null ? null : current.addressOrigin;
    }

    private long addressOriginOffset() {
        AddressOriginState current = addressState;
        return current == null ? 0 : current.addressOriginOffset;
    }

    private void setAddressOrigin(Pointer origin, long offset) {
        AddressOriginState current = addressState;
        if (current != null && current.addressOrigin == origin && current.addressOriginOffset == offset) {
            return;
        }
        FieldCell field = byteOffset == 0 && allocation instanceof FieldCell ? (FieldCell) allocation : null;
        if (field != null) {
            current = field.cachedAddressOrigin;
            if (current != null && current.addressOrigin == origin && current.addressOriginOffset == offset) {
                addressState = current;
                return;
            }
        }
        current = new AddressOriginState(origin, offset);
        addressState = current;
        if (field != null) field.cachedAddressOrigin = current;
    }

    private long publishedAddress() {
        RarePointerState current = rareState;
        return current == null ? Long.MIN_VALUE : current.publishedAddress;
    }

    private void setPublishedAddress(long address) {
        mutableRareState().publishedAddress = address;
    }

    private ExposedTarget publishedTarget() {
        RarePointerState current = rareState;
        return current == null ? null : current.publishedTarget;
    }

    private void setPublishedTarget(long address, ExposedTarget target) {
        RarePointerState current = mutableRareState();
        current.publishedTarget = target;
        current.publishedAddress = address;
    }

    private MemoryViewState boundMemoryViewState() {
        RarePointerState current = rareState;
        return current == null ? null : current.boundMemoryViewState;
    }

    private void setBoundMemoryViewState(MemoryViewState view) {
        mutableRareState().boundMemoryViewState = view;
    }

    private long erasedAddressToken() {
        RarePointerState current = rareState;
        return current == null ? -1 : current.erasedAddressToken;
    }

    private void setErasedAddressToken(long token) {
        mutableRareState().erasedAddressToken = token;
    }

    private long zeroSizedSourceViewSize() {
        RarePointerState current = rareState;
        return current == null ? -1 : current.zeroSizedSourceViewSize;
    }

    private String zeroSizedSourceViewCodecClassName() {
        RarePointerState current = rareState;
        return current == null ? null : current.zeroSizedSourceViewCodecClassName;
    }

    private void setZeroSizedSourceView(long size, String codecClassName) {
        RarePointerState current = mutableRareState();
        current.zeroSizedSourceViewSize = size;
        current.zeroSizedSourceViewCodecClassName = codecClassName;
    }

    private void clearZeroSizedSourceView() {
        RarePointerState current = rareState;
        if (current != null) {
            current.zeroSizedSourceViewSize = -1;
            current.zeroSizedSourceViewCodecClassName = null;
        }
    }

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
            Object value, int size, String codec, int alignment, String scalarLayout) {
        return cellAligned(value, size, codec, alignment);
    }

    public static Pointer cellAligned(
            Object value,
            int size,
            String codecClassName,
            int alignment) {
        if (alignment <= 0 || (alignment & (alignment - 1)) != 0) {
            throw new IllegalArgumentException("Rust allocation alignment must be a power of two");
        }
        if (size == 0 && value != null) {
            recordAlignment(value, alignment);
            return new Pointer(value, 0, 0, 0, codecClassName);
        }
        Cell cell = new Cell(value);
        recordAlignment(cell, alignment);
        Pointer pointer = new Pointer(cell, size, 0, size, codecClassName);
        long metadata = mayCarryStructuralMetadata(value, size)
                ? inferredStructuralMetadata(value)
                : -1;
        return metadata < 0 ? pointer : pointer.withMetadata(metadata);
    }

    public static Object storage(Object value, int size, String codec) {
        return storageAligned(value, size, codec, 16);
    }

    public static Object storage(Object value, long size, String codec) {
        return storageAligned(value, checkedArrayLength(size), codec, 16);
    }

    /** Compiler-proven, nonzero typed storage, retaining the original identity. */
    public static Object storageAligned(Object value, int size, String codec, int alignment) {
        return storageAligned(value, size, codec, alignment, null);
    }

    public static Object storageAligned(Object value, int size, String codec, int alignment, String scalarLayout) {
        return storageAligned(value, size, codec, alignment, scalarLayout, -1);
    }

    public static Object borrowedStorage(Object value, int size, String codec, int kind) {
        return borrowedStorageAligned(value, size, codec, 16, kind);
    }

    public static Object borrowedStorage(Object value, long size, String codec, int kind) {
        return borrowedStorage(value, checkedArrayLength(size), codec, kind);
    }

    public static Object borrowedStorageAligned(Object value, int size, String codec, int alignment, int kind) {
        return borrowedStorageAligned(value, size, codec, alignment, null, kind);
    }

    public static Object borrowedStorageAligned(Object value, int size, String codec, int alignment, String layout, int kind) {
        return storageAligned(value, size, codec, alignment, layout, kind);
    }

    private static Object storageAligned(Object value, int size, String codec, int alignment,
            String scalarLayout, int borrowed) {
        if (size <= 0 || alignment <= 0 || (alignment & (alignment - 1)) != 0) {
            throw new IllegalArgumentException("invalid typed Rust storage layout");
        }
        long metadata = mayCarryStructuralMetadata(value, size)
                ? inferredStructuralMetadata(value) : -1;
        // Trait and DST addresses need the full codec carrier to preserve dynamic metadata.
        Storage storage = borrowed >= 0 && (borrowed != 0 || size == 8)
                ? new BorrowedStorage(value, size, codec, metadata, scalarLayout, borrowed != 0)
                : new Storage(value, size, codec, metadata, scalarLayout);
        recordAlignment(storage, alignment);
        return storage;
    }

    static Pointer storageBoundary(Storage storage) {
        Pointer result = new Pointer(storage, storage.size, 0, storage.size, storage.codec);
        return storage.metadata < 0 ? result : result.withMetadata(storage.metadata);
    }

    private static boolean directStorage(Storage storage) {
        return unescapedStorage(storage)
                && (!(storage instanceof BorrowedStorage) || !((BorrowedStorage) storage).split);
    }

    private static boolean unescapedStorage(Storage storage) {
        Cell cell = storage;
        return storage.boundary == null && !cell.hasStructuralView
                && !cell.hasMemoryView && cell.fields == null && !cell.hasProjectedViews
                && !isStructuralViewCodec(storage.codec);
    }

    /** Populate published static storage without changing the address captured
     * by self-references or by another static's initializer. Includes ZST carriers. */
    public void initializeStatic(Object value) {
        if (!(allocation instanceof Cell) || byteOffset != 0
                || viewSize != allocationElementSize) {
            throw new IllegalStateException("expected a complete static cell");
        }
        writeElement(0, value);
        metadata = mayCarryStructuralMetadata(value, allocationElementSize)
                ? inferredStructuralMetadata(value)
                : -1;
    }

    /** Installs a JVM carrier in a zero-sized local without changing its address.
     * Rust stores to ZST bytes remain no-ops; local initialization must still
     * supply the complete carrier, whose constructor may have ZST fields. */
    public void initializeZeroSizedLocal(Object value) {
        if (!(allocation instanceof Cell) || byteOffset != 0
                || viewSize != 0 || allocationElementSize != 0) {
            throw new IllegalStateException("expected a zero-sized local cell");
        }
        writeElement(0, value);
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
        MemoryViewOrigin origin = null;
        if (mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, value)) {
            Map<Object, MemoryViewOrigin> originStripe =
                    stateStripe(MEMORY_VIEW_ORIGINS, value);
            synchronized (originStripe) {
                origin = originStripe.get(value);
            }
        }
        MemoryViewState activeView = origin == null
                ? null
                : activeMemoryViewState(value, origin);
        if (origin != null
                && origin.viewSize == size
                && activeView != null) {
            Object allocation = origin.allocation.get();
            if (allocation != null) {
                Pointer pointer = new Pointer(
                                allocation,
                                origin.allocationElementSize,
                                origin.byteOffset,
                                size,
                                origin.allocationCodecClassName,
                                codecClassName,
                                -1)
                        .withMetadata(origin.metadata);
                pointer.setBoundMemoryViewState(activeView);
                return pointer;
            }
        }
        if (size == 0) {
            if (value != null) {
                recordAlignment(value, alignment);
            }
            return new Pointer(value, 0, 0, 0, codecClassName);
        }
        ReceiverCell cell = new ReceiverCell(value);
        recordAlignment(cell, alignment);
        Pointer pointer = new Pointer(cell, size, 0, size, codecClassName);
        long metadata = mayCarryStructuralMetadata(value, size)
                ? inferredStructuralMetadata(value)
                : -1;
        return metadata < 0 ? pointer : pointer.withMetadata(metadata);
    }

    private static boolean mayCarryStructuralMetadata(Object value, int size) {
        return size != 0
                && value != null
                && !(value instanceof Number)
                && !(value instanceof Boolean)
                && !(value instanceof Character)
                && !(value instanceof String)
                && !(value instanceof Pointer)
                && !value.getClass().isArray()
                && mayContainStructuralMetadata(value.getClass());
    }

    public static Pointer cell(Object value, int size) {
        return cell(value, size, null);
    }

    public static Pointer cell(Object value) {
        return cell(value, inferredCarrierSize(value));
    }

    /** Returns a stable, write-through pointer to a generated Rust value field. */
    public static Pointer field(
            Object owner, String fieldName, int size, String codecClassName) {
        return field(owner, fieldName, size, codecClassName, true);
    }

    private static Pointer field(
            Object owner, String fieldName, int size, String codecClassName, boolean cache) {
        if (owner == null) {
            throw new NullPointerException("Rust field pointer requires an owner");
        }
        // Erased zero-sized fields have no Java member. Avoid reflection errors and cache entries.
        if (size == 0 && optionalInstanceField(owner.getClass(), fieldName) == null) {
            return Pointer.cell(null, 0, codecClassName);
        }
        FieldCell cell;
        Map<Object, Map<String, WeakReference<FieldCell>>> stripe =
                stateStripe(FIELD_CELLS, owner);
        synchronized (stripe) {
            Map<String, WeakReference<FieldCell>> fields = stripe.get(owner);
            if (fields == null) {
                fields = new HashMap<>();
                stripe.put(owner, fields);
            }
            WeakReference<FieldCell> reference = fields.get(fieldName);
            cell = reference == null ? null : reference.get();
            if (cell == null) {
                try {
                    cell = new FieldCell(owner, fieldAccess(owner.getClass(), fieldName));
                } catch (NoSuchFieldException error) {
                    if (size == 0) {
                        return Pointer.cell(null, 0, codecClassName);
                    }
                    throw new IllegalArgumentException(
                            "unknown Rust field " + owner.getClass().getName() + "." + fieldName,
                            error);
                }
                fields.put(fieldName, new WeakReference<>(cell));
            }
            markIdentityFilter(FIELD_CELL_FILTER, owner);
        }
        Pointer cached = cell.cachedProjection;
        if (cache && cached != null && cached.metadata == -1 && cached.rareState == null
                && cached.addressState == null && cached.viewSize == size
                && java.util.Objects.equals(cached.viewCodecClassName, codecClassName)) return cached;
        Pointer result = new Pointer(cell, size, 0, size, codecClassName);
        if (cache) cell.cachedProjection = result;
        return result;
    }

    public static Pointer field(
            Object owner, String fieldName, long size, String codecClassName) {
        return field(owner, fieldName, checkedArrayLength(size), codecClassName);
    }

    /** Returns a stable field pointer with the exact alignment of its Rust field type. */
    public static Pointer fieldAligned(
            Object owner,
            String fieldName,
            long size,
            String codecClassName,
            long alignment) {
        int checkedAlignment = Math.toIntExact(alignment);
        if (checkedAlignment <= 0
                || (checkedAlignment & (checkedAlignment - 1)) != 0) {
            throw new IllegalArgumentException("Rust field alignment must be a power of two");
        }
        Pointer pointer = field(owner, fieldName, size, codecClassName);
        if (pointer.allocation != null) {
            recordAlignment(pointer.allocation, checkedAlignment);
        }
        return pointer;
    }

    /**
     * Projects a field through a replaceable root or enclosing field cell. The
     * projection follows the current aggregate at every depth. Mutations are
     * visible during unwinding, and later whole-place replacement remains visible
     * through previously derived field pointers.
     */
    private static Pointer rootField(
            Object owner,
            Class<?> ownerClass,
            String fieldName,
            long size,
            String codecClassName) {
        return rootField(owner, ownerClass, fieldName, size, codecClassName, null, 0);
    }

    private static Pointer rootField(Object owner, Class<?> ownerClass, String fieldName,
            long size, String codecClassName, Pointer source, long displacement) {
        // Zero-sized fields have no Java member.
        if (size == 0 && optionalInstanceField(ownerClass, fieldName) == null) {
            return finishFieldProjection(Pointer.cell(null, 0, codecClassName), source, displacement);
        }
        FieldAccess access;
        try {
            access = fieldAccess(ownerClass, fieldName);
        } catch (NoSuchFieldException error) {
            if (size == 0) {
                return finishFieldProjection(Pointer.cell(null, 0, codecClassName), source, displacement);
            }
            throw new IllegalArgumentException(
                    "unknown Rust field " + ownerClass.getName() + "." + fieldName, error);
        }
        FieldCell cell = ownedField(owner, access);
        Pointer cached = cell.cachedProjection;
        boolean reusable = source != null && source.metadata == -1 && source.rareState == null;
        if (reusable && cached != null && cached.metadata == -1 && cached.rareState == null
                && cached.viewSize == size && java.util.Objects.equals(cached.viewCodecClassName, codecClassName)) {
            AddressOriginState origin = source.addressState;
            Pointer ownerOrigin = origin == null || origin.addressOrigin == null ? source : origin.addressOrigin;
            long offset = origin == null || origin.addressOrigin == null ? displacement
                    : Math.addExact(origin.addressOriginOffset, displacement);
            AddressOriginState previous = cached.addressState;
            if (previous != null && previous.addressOrigin == ownerOrigin && previous.addressOriginOffset == offset)
                return cached;
        }
        Pointer result = finishFieldProjection(
                new Pointer(cell, checkedArrayLength(size), 0, size, codecClassName), source, displacement);
        if (reusable) cell.cachedProjection = result;
        return result;
    }

    private static Pointer finishFieldProjection(Pointer pointer, Pointer source, long displacement) {
        return source == null ? pointer : pointer.withMetadata(source.metadata).inheritAddressOrigin(source, displacement);
    }

    private Object compatibleStructView(String ownerClassName) {
        try {
            Class<?> ownerClass = resolvedRuntimeClass(ownerClassName);
            Object candidate = getObjectAs(ownerClassName);
            return ownerClass.isInstance(candidate) ? candidate : null;
        } catch (ClassNotFoundException error) {
            throw new IllegalArgumentException(
                    "unknown Rust aggregate class " + ownerClassName, error);
        }
    }

    private boolean hasStableManagedCarrier() {
        return isDirectAllocationView()
                || allocation instanceof Cell
                || allocation instanceof ReceiverCell
                || allocation instanceof FieldCell;
    }

    // A union (possibly inside transparent wrappers) has no independently
    // mutable Java fields for the Rust value. Its decoded carrier can remain
    // authoritative, which is also necessary for atomics inside MaybeUninit.
    private static final ClassValue<Boolean> BYTE_STORAGE_CARRIERS = new ClassValue<Boolean>() {
        protected Boolean computeValue(Class<?> type) {
            Set<Class<?>> visited = new HashSet<>();
            while (visited.add(type)) {
                RustField[] fields = PUBLIC_INSTANCE_FIELDS.get(type);
                if (fields.length == 2) {
                    RustField bytes = optionalInstanceField(type, "_bytes");
                    RustField objects = optionalInstanceField(type, "_objects");
                    return bytes != null && bytes.getType() == byte[].class
                            && objects != null && objects.getType() == Object[].class;
                }
                if (fields.length != 1) {
                    return false;
                }
                type = fields[0].getType();
            }
            return false;
        }
    };

    private boolean hasByteStorageCarrier() {
        if (byteOffset != 0 || viewSize != allocationElementSize) {
            return false;
        }
        Object value = directCellValueOrSelf();
        return value != null && value != this && BYTE_STORAGE_CARRIERS.get(value.getClass());
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
        // Rust field offsets locate byte storage without Java reflection or a decoded parent.
        if (allocation instanceof byte[] && traitMetadataCarrier() == null) {
            return byteOffsetRetype(fieldOffset, fieldSize, fieldCodecClassName);
        }
        Class<?> ownerClass = null;
        if ((allocation instanceof Cell || allocation instanceof FieldCell)
                && byteOffset == 0
                && viewSize == allocationElementSize) {
            try {
                if (ownerClass == null) {
                    ownerClass = resolvedRuntimeClass(ownerClassName);
                }
                if (ownerClass.isInstance(directCellCarrier(allocation))) {
                    return rootField(
                                    allocation,
                                    ownerClass,
                                    fieldName,
                                    fieldSize,
                                    fieldCodecClassName, this, fieldOffset);
                }
            } catch (ClassNotFoundException error) {
                throw new IllegalArgumentException(
                        "unknown Rust aggregate class " + ownerClassName, error);
            }
        }
        Class<?> fieldType = null;
        if (fieldSize != 0 || traitMetadataCarrier() != null) {
            try {
                if (ownerClass == null) ownerClass = resolvedRuntimeClass(ownerClassName);
                fieldType = instanceField(ownerClass, fieldName).getType();
            } catch (ClassNotFoundException error) {
                throw new IllegalArgumentException(
                        "unknown Rust aggregate class " + ownerClassName, error);
            } catch (NoSuchFieldException error) {
                throw new IllegalArgumentException(
                        "unknown Rust field " + ownerClassName + "." + fieldName, error);
            }
        }
        boolean managedField = fieldType == null || !fieldType.isPrimitive();
        // Replaceable roots and nested fields use rootField above. Only an
        // existing carrier may supply a live field here: decoding the whole
        // parent would write back stale siblings when just this field changes.
        boolean directPrimitiveField =
                allocation instanceof FieldCell
                        || allocation instanceof ReceiverCell
                        || boundMemoryViewState() != null;
        if ((managedField || directPrimitiveField) && hasStableManagedCarrier()) {
            Object owner;
            try {
                owner = isGeneratedAggregateCodec(viewCodecClassName)
                                && !isDirectAllocationView() && !hasByteStorageCarrier()
                        ? transparentManagedView(ownerClass != null
                                ? ownerClass : resolvedRuntimeClass(ownerClassName))
                        : managedField || isStructuralViewCodec(viewCodecClassName)
                        ? compatibleStructView(ownerClassName)
                        : directAggregate(ownerClass != null
                                ? ownerClass : resolvedRuntimeClass(ownerClassName));
            } catch (ClassNotFoundException error) {
                throw new IllegalArgumentException(
                        "unknown Rust aggregate class " + ownerClassName, error);
            }
            if (owner != null) {
                return field(owner, fieldName, checkedArrayLength(fieldSize), fieldCodecClassName, false)
                        .withMetadata(metadata)
                        .inheritAddressOrigin(this, fieldOffset);
            }
        }
        Pointer projected = byteOffsetRetype(fieldOffset, fieldSize, fieldCodecClassName);
        if (traitMetadataCarrier() != null
                && fieldType != null
                && fieldType.isInstance(traitMetadataCarrier())) {
            projected.traitObjectCarrier(traitMetadataCarrier());
            projected.traitMetadataCarrier(null);
        }
        return projected;
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

        Pointer data = byteOffsetRetype(fieldOffset, elementSize, elementCodecClassName);
        return SliceView.create(SLICE_VIEW_CLASS_NAME, data, 0, metadata());
    }

    public Object projectStructStrField(
            String ownerClassName, String fieldName, long fieldOffset) {
        if (hasStableManagedCarrier()) {
            Object owner = compatibleStructView(ownerClassName);
            if (owner != null) {
                try {
                    return instanceField(owner.getClass(), fieldName).get(owner);
                } catch (ReflectiveOperationException error) {
                    throw new IllegalStateException("could not read Rust DST string field", error);
                }
            }
        }

        Pointer data = byteOffsetRetype(fieldOffset, 1, null);
        return SliceView.create(UTF8_VIEW_CLASS_NAME, data, 0, metadata());
    }

    public static Pointer array(
            Object array,
            int elementOffset,
            int elementSize,
            String codecClassName) {
        if (array == null || !array.getClass().isArray()) {
            throw new IllegalArgumentException("Rust array pointer requires JVM array storage");
        }
        if (mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, array)) {
            MemoryViewOrigin origin;
            Map<Object, MemoryViewOrigin> stripe =
                    stateStripe(MEMORY_VIEW_ORIGINS, array);
            synchronized (stripe) {
                origin = stripe.get(array);
            }
            if (origin != null) {
                Object allocation = origin.allocation.get();
                if (allocation != null) {
                    long relativeOffset =
                            Math.multiplyExact((long) elementOffset, elementSize);
                    Pointer source = new Pointer(
                                    allocation,
                                    origin.allocationElementSize,
                                    origin.byteOffset,
                                    origin.viewSize,
                                    origin.allocationCodecClassName,
                                    origin.allocationCodecClassName,
                                    -1)
                            .withMetadata(origin.metadata);
                    if (activeMemoryViewMatches(array, origin)) {
                        return source.byteOffsetRetype(relativeOffset, elementSize, codecClassName);
                    }
                    return new Pointer(
                                    array,
                                    elementSize,
                                    relativeOffset,
                                    elementSize,
                                    codecClassName)
                            .inheritAddressOrigin(source, relativeOffset);
                }
            }
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
            recordAlignment(bytes, checkedAlignment);
            Pointer pointer = new Pointer(bytes, 1, 0, 1, null);
            synchronized (ALLOCATIONS) {
                ALLOCATOR_OWNED_ALLOCATIONS.put(bytes, Boolean.TRUE);
            }
            return pointer;
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
        byte[] allocation = CONSTANT_ALLOCATIONS.get(identity);
        if (allocation == null) {
            byte[] candidate = new byte[checkedLength];
            Arrays.fill(candidate, (byte) initialByte);
            byte[] existing = CONSTANT_ALLOCATIONS.putIfAbsent(identity, candidate);
            allocation = existing == null ? candidate : existing;
            if (existing == null) {
                recordAlignment(allocation, checkedAlignment);
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

    /** Materializes an arbitrary provenance-free CTFE byte allocation. */
    public static Pointer constantBytes(
            String identity,
            byte[] bytes,
            long offset,
            long viewSize,
            long alignment,
            String viewCodecClassName) {
        int checkedOffset = checkedArrayLength(offset);
        int checkedAlignment = checkedAlignment(alignment);
        if (checkedOffset > bytes.length) {
            throw new IndexOutOfBoundsException("constant pointer offset exceeds its allocation");
        }
        byte[] allocation = CONSTANT_ALLOCATIONS.putIfAbsent(identity, bytes);
        if (allocation == null) {
            allocation = bytes;
            recordAlignment(allocation, checkedAlignment);
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

    /** Materializes one stable JVM allocation for an anonymous CTFE allocation. */
    public static Pointer constantCell(
            String identity,
            Object value,
            long viewSize,
            String viewCodecClassName,
            long alignment) {
        int checkedSize = checkedArrayLength(viewSize);
        int checkedAlignment = checkedAlignment(alignment);
        Pointer pointer = CONSTANT_CELLS.get(identity);
        if (pointer == null) {
            pointer = CONSTANT_CELLS.computeIfAbsent(
                    identity,
                    ignored -> {
                        Object storedValue = value;
                        if (storedValue != null && storedValue.getClass().isArray()) {
                            // A Rust fixed array is one value, but its elements must
                            // remain individually addressable after pointer casts.
                            // Keep the JVM array instead of creating a scalar Cell.
                            if (storedValue instanceof byte[]) {
                                byte[] existing = CONSTANT_ALLOCATIONS.putIfAbsent(
                                        identity, (byte[]) storedValue);
                                if (existing != null) {
                                    storedValue = existing;
                                }
                            }
                            recordAlignment(storedValue, checkedAlignment);
                            return new Pointer(
                                    storedValue,
                                    inferredArrayElementSize(storedValue),
                                    0,
                                    checkedSize,
                                    null,
                                    viewCodecClassName,
                                    -1);
                        }
                        return cellAligned(
                                storedValue,
                                checkedSize,
                                viewCodecClassName,
                                checkedAlignment);
                    });
        }
        if (value != null
                && pointer.allocation != null
                && pointer.allocation.getClass().isArray()
                && !pointer.allocation.getClass().getComponentType().isPrimitive()
                && pointer.allocation.getClass().getComponentType().isInstance(value)) {
            // CTFE can name the same promoted allocation once as a fixed
            // array and once as its sole element. Release optimization may
            // materialize either view first, so recover the Rust element
            // layout instead of inheriting the JVM reference-slot width.
            return pointer.sliceStorageView(checkedSize, viewCodecClassName);
        }
        if (pointer.viewSize == checkedSize
                && java.util.Objects.equals(
                        pointer.viewCodecClassName, viewCodecClassName)) {
            return pointer;
        }
        return pointer.retype(checkedSize, viewCodecClassName);
    }

    /**
     * Materializes an interior typed view of a shared CTFE allocation.
     *
     * <p>Unlike {@link #constantCell}, the value occupies only one range of
     * the allocation. Keeping the complete byte storage is important for
     * pointer arithmetic and for alignments larger than the view itself.
     */
    public static Pointer constantCellAt(
            String identity,
            Object value,
            long allocationSize,
            long offset,
            long viewSize,
            String viewCodecClassName,
            long alignment) {
        int checkedAllocationSize = checkedArrayLength(allocationSize);
        int checkedOffset = checkedArrayLength(offset);
        int checkedViewSize = checkedArrayLength(viewSize);
        int checkedAlignment = checkedAlignment(alignment);
        int end;
        try {
            end = Math.addExact(checkedOffset, checkedViewSize);
        } catch (ArithmeticException error) {
            throw new IndexOutOfBoundsException("constant pointer range overflows");
        }
        if (end > checkedAllocationSize) {
            throw new IndexOutOfBoundsException(
                    "constant pointer range exceeds its allocation");
        }

        String viewIdentity = identity
                + "\0view:"
                + checkedOffset
                + ":"
                + checkedViewSize
                + ":"
                + String.valueOf(viewCodecClassName);
        Pointer pointer = CONSTANT_CELLS.computeIfAbsent(
                viewIdentity,
                ignored -> {
                    byte[] allocation = CONSTANT_ALLOCATIONS.get(identity);
                    if (allocation == null) {
                        byte[] candidate = new byte[checkedAllocationSize];
                        byte[] existing =
                                CONSTANT_ALLOCATIONS.putIfAbsent(identity, candidate);
                        allocation = existing == null ? candidate : existing;
                        if (existing == null) {
                            recordAlignment(allocation, checkedAlignment);
                        }
                    }
                    if (allocation.length != checkedAllocationSize) {
                        throw new IllegalStateException(
                                "constant allocation was materialized with a different size");
                    }
                    Pointer candidate = new Pointer(
                            allocation,
                            1,
                            checkedOffset,
                            checkedViewSize,
                            null,
                            viewCodecClassName,
                            -1);
                    if (value != null && checkedViewSize != 0) {
                        candidate.set(value);
                    }
                    return candidate;
                });
        return pointer.retype(checkedViewSize, viewCodecClassName);
    }

    /** Materializes one stable array-backed CTFE allocation. */
    public static Pointer constantArray(
            String identity,
            Object value,
            long elementSize,
            String elementCodecClassName,
            long alignment) {
        int checkedSize = checkedArrayLength(elementSize);
        int checkedAlignment = checkedAlignment(alignment);
        if (value == null || !value.getClass().isArray()) {
            throw new IllegalArgumentException("constant Rust array requires JVM array storage");
        }
        Pointer pointer = CONSTANT_CELLS.computeIfAbsent(
                identity,
                ignored -> {
                    recordAlignment(value, checkedAlignment);
                    return array(value, 0, checkedSize, elementCodecClassName);
                });
        if (Array.getLength(value) == 1
                && pointer.allocation != null
                && value.getClass().getComponentType().isArray()
                && value.getClass().getComponentType() == pointer.allocation.getClass()) {
            // CTFE can share an array with its sole element, which can also be an array.
            // Preserve the element value and the original allocation identity.
            return cellAligned(pointer.allocation, checkedSize,
                            elementCodecClassName, checkedAlignment)
                    .inheritAddressOrigin(pointer, 0);
        }
        return pointer.retype(checkedSize, elementCodecClassName);
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
        synchronized (ALLOCATIONS) {
            ALLOCATOR_OWNED_ALLOCATIONS.remove(allocation);
            AllocationInfo info = ALLOCATIONS.remove(allocation);
            Long base = info == null ? null : info.base;
            if (base != null && info.rangePublished) {
                AllocationRange range = ALLOCATION_RANGES.get(base);
                if (range != null && range.allocation.get() == allocation) {
                    ALLOCATION_RANGES.remove(base);
                }
            }
            Set<Long> addresses = ALLOCATION_EXPOSED_ADDRESSES.remove(allocation);
            if (addresses != null) {
                for (Long address : addresses) {
                    ExposedTarget target = EXPOSED_ADDRESSES.get(address);
                    if (target != null && target.allocation == allocation) {
                        EXPOSED_ADDRESSES.remove(address);
                    }
                    TypedExposedEntry typed = TYPED_EXPOSED_ADDRESSES.get(address);
                    if (typed != null) {
                        typed.removeAllocation(allocation);
                        if (typed.isEmpty()) {
                            TYPED_EXPOSED_ADDRESSES.remove(address, typed);
                        }
                    }
                }
            }
        }
        discardCachedAllocationBases(allocation);
        Map<Object, LongRangeMap<MemoryViewState>> memoryStripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (memoryStripe) {
            deactivateMemoryViews(memoryStripe.remove(allocation));
            setDirectCellHasMemoryView(allocation, false);
        }
        Map<Object, Map<Long, StructuralViewState>> structuralStripe =
                stateStripe(STRUCTURAL_VIEWS, allocation);
        synchronized (structuralStripe) {
            structuralStripe.remove(allocation);
        }
        discardEncodedReferences(allocation);
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
        SliceView view = (SliceView) sliceView;
        return fromSliceParts(view.array, view.offset, view.rustLength, elementSize, codecClassName);
    }

    /** Extract an address from SSA view components without constructing a view object. */
    public static Pointer fromSliceParts(
            Object backing, int offset, long length, int elementSize, String codecClassName) {
        if (backing == null) {
            return withoutProvenance(Math.multiplyExact((long) offset, elementSize),
                    elementSize, codecClassName).withMetadata(length);
        }
        if (backing instanceof Pointer) {
            return ((Pointer) backing)
                    .sliceStorageView(elementSize, codecClassName)
                    .add(offset)
                    .withMetadata(length);
        }
        MemoryViewOrigin origin = null;
        if (mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, backing)) {
            Map<Object, MemoryViewOrigin> originStripe =
                    stateStripe(MEMORY_VIEW_ORIGINS, backing);
            synchronized (originStripe) {
                origin = originStripe.get(backing);
            }
        }
        if (origin != null) {
            Object allocation = origin.allocation.get();
            if (allocation != null) {
                long relativeOffset = Math.multiplyExact((long) offset, elementSize);
                Pointer source = new Pointer(
                                allocation,
                                origin.allocationElementSize,
                                origin.byteOffset,
                                origin.viewSize,
                                origin.allocationCodecClassName,
                                origin.allocationCodecClassName,
                                -1)
                        .withMetadata(origin.metadata);
                if (activeMemoryViewMatches(backing, origin)) {
                    return source.byteOffsetRetype(relativeOffset, elementSize, codecClassName)
                            .withMetadata(length);
                }
                return new Pointer(
                                backing,
                                elementSize,
                                relativeOffset,
                                elementSize,
                                codecClassName)
                        .inheritAddressOrigin(source, relativeOffset)
                        .withMetadata(length);
            }
        }
        return array(
                backing,
                offset,
                elementSize,
                codecClassName).withMetadata(length);
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
        SliceView view = (SliceView) sliceView;
        if (view.array instanceof Pointer) {
            return ((Pointer) view.array)
                    .sliceStorageView(elementSize, codecClassName)
                    .add(view.offset)
                    .withMetadata(view.rustLength);
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
        Object array = ((SliceView) sliceView).array;
        if (array instanceof Pointer) {
            Pointer pointer = (Pointer) array;
            return fromSlice(sliceView, pointer.viewSize, pointer.viewCodecClassName);
        }
        return fromSlice(sliceView, inferredArrayElementSize(array));
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
        if (pointer != null && pointer.traitMetadataMarker() != null) {
            return pointer.traitMetadataMarker();
        }
        Object value = null;
        String concreteClassName = "<null>";
        if (pointer != null) {
            if (pointer.traitMetadataCarrier() != null) {
                value = pointer.traitMetadataCarrier();
                concreteClassName = value.getClass().getName();
            } else if (pointer.traitPointeeSize() == 0
                    && pointer.traitAdapterClassName() != null) {
                // A ZST data pointer is a non-null dangling address and has no
                // Java value to dereference. Its generated adapter still gives
                // the vtable a stable, concrete identity.
                concreteClassName = pointer.traitAdapterClassName();
            } else {
                value = pointer.getObject();
                concreteClassName = value == null ? "<null>" : value.getClass().getName();
            }
        }
        String key = metadataClassName + '\0' + concreteClassName;
        synchronized (TRAIT_METADATA_MARKERS) {
            Pointer marker = TRAIT_METADATA_MARKERS.get(key);
            if (marker == null) {
                marker = cell(null, 0, null);
                if (pointer != null) {
                    marker.traitPointeeSize(pointer.traitPointeeSize());
                    marker.traitPointeeAlignment(pointer.traitPointeeAlignment());
                    marker.traitAdapterClassName(pointer.traitAdapterClassName());
                }
                if (value instanceof TraitObjectCarrier) {
                    TraitObjectCarrier carrier = (TraitObjectCarrier) value;
                    marker.traitPointeeSize(carrier.rustTraitObjectSize());
                    marker.traitPointeeAlignment(carrier.rustTraitObjectAlignment());
                    marker.traitAdapterClassName(value.getClass().getName());
                    Object payload = carrier.rustTraitObjectPayload();
                    if (payload instanceof Pointer) {
                        marker.traitPointeeCodecClassName(((Pointer) payload).viewCodecClassName);
                    }
                }
                TRAIT_METADATA_MARKERS.put(key, marker);
            }
            if (marker.traitPointeeSize() >= 0 && marker.traitPointeeAlignment() > 0) {
                TRAIT_METADATA_INFO.put(
                        marker.numericAddress(),
                        new TraitMetadataInfo(
                                marker.traitPointeeSize(),
                                marker.traitPointeeAlignment(),
                                marker.traitAdapterClassName(),
                                marker.traitPointeeCodecClassName()));
            }
            if (pointer != null) {
                pointer.traitMetadataMarker(marker);
            }
            return marker;
        }
    }

    /** Materializes the symbolic vtable emitted by rustc's constant evaluator. */
    public static Pointer constantVtableMarker(
            String identity, long pointeeSize, long pointeeAlignment) {
        return constantVtableMarker(
                identity, pointeeSize, pointeeAlignment, null, null);
    }

    /** Materializes a vtable together with the JVM adapter used for raw reconstruction. */
    public static Pointer constantVtableMarker(
            String identity,
            long pointeeSize,
            long pointeeAlignment,
            String adapterClassName,
            String pointeeCodecClassName) {
        String key = "@const-vtable\0" + identity;
        synchronized (TRAIT_METADATA_MARKERS) {
            Pointer marker = TRAIT_METADATA_MARKERS.get(key);
            if (marker == null) {
                marker = cell(null, 0, null);
                TRAIT_METADATA_MARKERS.put(key, marker);
            }
            marker.traitPointeeSize(pointeeSize);
            marker.traitPointeeAlignment(pointeeAlignment);
            marker.traitAdapterClassName(adapterClassName);
            marker.traitPointeeCodecClassName(pointeeCodecClassName);
            TRAIT_METADATA_INFO.put(
                    marker.numericAddress(),
                    new TraitMetadataInfo(
                            pointeeSize,
                            pointeeAlignment,
                            adapterClassName,
                            pointeeCodecClassName));
            return marker;
        }
    }

    public static long vtableSize(Pointer marker) {
        if (marker == null) {
            throw new IllegalStateException("Rust vtable does not carry a pointee size");
        }
        if (marker.traitPointeeSize() >= 0) {
            return marker.traitPointeeSize();
        }
        TraitMetadataInfo info = TRAIT_METADATA_INFO.get(marker.numericAddress());
        if (info == null) {
            throw new IllegalStateException("Rust vtable does not carry a pointee size");
        }
        return info.size;
    }

    public static long vtableAlign(Pointer marker) {
        if (marker == null) {
            throw new IllegalStateException("Rust vtable does not carry a pointee alignment");
        }
        if (marker.traitPointeeAlignment() > 0) {
            return marker.traitPointeeAlignment();
        }
        TraitMetadataInfo info = TRAIT_METADATA_INFO.get(marker.numericAddress());
        if (info == null) {
            throw new IllegalStateException("Rust vtable does not carry a pointee alignment");
        }
        return info.alignment;
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
        synchronized (ALLOCATIONS) {
            discardCollectedAllocationRanges();
            ExposedTarget target = EXPOSED_ADDRESSES.get(address);
            if (target != null) {
                Object allocation = target.allocation;
                Pointer pointer = new Pointer(
                        allocation,
                        target.allocationElementSize,
                        target.byteOffset,
                        viewSize,
                        target.codecClassName,
                        viewCodecClassName,
                        target.exposedAddress).withMetadata(target.metadata);
                if (viewSize == 0 && target.zeroSizedSourceViewSize >= 0) {
                    pointer.setZeroSizedSourceView(
                            target.zeroSizedSourceViewSize,
                            target.zeroSizedSourceViewCodecClassName);
                }
                pointer.traitObjectCarrier(target.traitObjectCarrier);
                pointer.traitMetadataCarrier(target.traitMetadataCarrier);
                pointer.traitMetadataMarker(target.traitMetadataMarker);
                pointer.traitPointeeSize(target.traitPointeeSize);
                pointer.traitPointeeAlignment(target.traitPointeeAlignment);
                pointer.traitAdapterClassName(target.traitAdapterClassName);
                pointer.traitPointeeCodecClassName(target.traitPointeeCodecClassName);
                pointer.setAddressOrigin(target.addressOrigin, target.addressOriginOffset);
                return pointer;
            }
            Map.Entry<Long, AllocationRange> entry = ALLOCATION_RANGES.floorEntry(address);
            if (entry != null) {
                long base = entry.getKey();
                AllocationRange range = entry.getValue();
                Object allocation = range.allocation.get();
                if (allocation == null) {
                    ALLOCATION_RANGES.remove(base);
                    return fromAddress(address, viewSize, viewCodecClassName);
                }
                long delta = address - base;
                if (delta >= 0 && delta <= range.capacity) {
                    return new Pointer(
                            allocation,
                            range.elementSize,
                            delta,
                            viewSize,
                            range.codecClassName,
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
        if (isRustFunctionPointer(value.getClass())) {
            return functionPointerAddress(value);
        }
        synchronized (MANAGED_OBJECT_ADDRESSES) {
            discardCollectedManagedObjects();
            Long existing = MANAGED_OBJECT_ADDRESSES.get(value);
            if (existing != null) {
                return existing.longValue();
            }
            long address = allocateAddress(16, 16);
            MANAGED_OBJECT_ADDRESSES.put(value, address);
            MANAGED_OBJECTS.put(address, new ManagedObjectReference(value, address));
            return address;
        }
    }

    private static void discardCollectedManagedObjects() {
        ManagedObjectReference collected;
        while ((collected = (ManagedObjectReference) MANAGED_OBJECT_QUEUE.poll()) != null) {
            MANAGED_OBJECTS.remove(Long.valueOf(collected.address), collected);
        }
    }

    public static Object managedObjectFromAddress(long address) {
        if (address == 0) {
            return null;
        }
        synchronized (FUNCTION_POINTER_CELLS) {
            Object functionPointer = FUNCTION_POINTERS_BY_ADDRESS.get(address);
            if (functionPointer != null) {
                return functionPointer;
            }
        }
        synchronized (MANAGED_OBJECT_ADDRESSES) {
            discardCollectedManagedObjects();
            ManagedObjectReference reference = MANAGED_OBJECTS.get(address);
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

    private static Pointer canonicalFunctionPointer(Object value) {
        if (value == null || !isRustFunctionPointer(value.getClass())) {
            throw new IllegalArgumentException(
                    "value is not a non-null Rust function pointer");
        }
        synchronized (FUNCTION_POINTER_CELLS) {
            // Receiver erasure creates fresh wrappers around the same static code.
            // Numeric addresses identify the adaptation and target, not each wrapper.
            Map<Object, Pointer> cells = FUNCTION_POINTER_CELLS;
            Object identity = FunctionPointers.identity(value);
            if (value instanceof FunctionPointerAdapter) {
                identity = canonicalFunctionPointer(
                        ((FunctionPointerAdapter) value).functionPointerTarget());
                cells = FUNCTION_POINTER_ADAPTER_CELLS.computeIfAbsent(
                        value.getClass(), key -> new IdentityHashMap<>());
            }
            Pointer existing = cells.get(identity);
            if (existing != null) {
                return existing;
            }
            Pointer pointer = cellAligned(value, Long.BYTES, null, Long.BYTES);
            long address = pointer.address();
            cells.put(identity, pointer);
            FUNCTION_POINTERS_BY_ADDRESS.put(address, value);
            return pointer;
        }
    }

    /** Returns the stable nonzero Rust address of a JVM function-pointer carrier. */
    public static long functionPointerAddress(Object value) {
        return canonicalFunctionPointer(value).address();
    }

    /** Converts a function pointer to a raw pointer without changing its address word. */
    public static Pointer fromFunctionPointer(
            Object value, long viewSize, String viewCodecClassName) {
        return canonicalFunctionPointer(value).retype(viewSize, viewCodecClassName);
    }

    /** Reconstructs a function-pointer carrier from a previously exposed address. */
    public static Object functionPointerFromAddress(long address) {
        if (address == 0) {
            throw new IllegalArgumentException(
                    "null address cannot be converted to a Rust function pointer");
        }
        synchronized (FUNCTION_POINTER_CELLS) {
            Object value = FUNCTION_POINTERS_BY_ADDRESS.get(address);
            if (value == null) {
                throw new IllegalArgumentException(
                        "address does not identify an exposed Rust function pointer: "
                                + Long.toUnsignedString(address));
            }
            return value;
        }
    }

    private static JavaStringViews javaStringViews(String value) {
        return JAVA_STRING_VIEWS.computeIfAbsent(value, JavaStringViews::new);
    }

    /** Returns stable UTF-8 storage shared by equal immutable Java strings. */
    public static byte[] utf8Bytes(String value) {
        if (value == null) {
            throw new NullPointerException("Rust str source is null");
        }
        return javaStringViews(value).bytes;
    }

    /** Returns one immutable slice carrier per string value and view type. */
    public static Object stringView(String value, String viewClassName) {
        if (value == null) {
            throw new NullPointerException("Rust str source is null");
        }
        JavaStringViews views = javaStringViews(value);
        boolean utf8 = matchesBinaryClassName(viewClassName, UTF8_VIEW_CLASS_NAME);
        Object cached = utf8 ? views.utf8 : views.slice;
        if (cached != null) {
            return cached;
        }
        synchronized (views) {
            cached = utf8 ? views.utf8 : views.slice;
            if (cached != null) {
                return cached;
            }
            cached = SliceView.create(viewClassName, views.bytes, 0, views.bytes.length);
            if (utf8) {
                views.utf8 = cached;
            } else {
                views.slice = cached;
            }
            return cached;
        }
    }

    private static Pointer pointerObjectFromAddress(long address) {
        if (address == 0) {
            return nullPointer();
        }
        synchronized (ALLOCATIONS) {
            ExposedTarget target = EXPOSED_ADDRESSES.get(address);
            if (target != null) {
                return pointerFromExposedTarget(target);
            }
        }
        return fromAddress(address, 1);
    }

    private static Pointer typedPointerObjectFromAddress(long address, String pointerCodec) {
        discardCollectedTypedExposedTargets(8);
        TypedExposedEntry typed = TYPED_EXPOSED_ADDRESSES.get(address);
        ExposedTarget target = typed == null ? null : typed.get(pointerCodec);
        if (target != null) {
            return pointerFromExposedTarget(target);
        }
        if (typed != null && typed.isEmpty()) {
            TYPED_EXPOSED_ADDRESSES.remove(address, typed);
        }
        if (address == 0) {
            return nullPointer();
        }
        return pointerObjectFromAddress(address);
    }

    private static Pointer pointerFromExposedTarget(ExposedTarget target) {
        Pointer pointer = new Pointer(
                target.allocation,
                target.allocationElementSize,
                target.byteOffset,
                target.viewSize,
                target.codecClassName,
                target.viewCodecClassName,
                target.exposedAddress).withMetadata(target.metadata);
        if (target.zeroSizedSourceViewSize >= 0) {
            pointer.setZeroSizedSourceView(
                    target.zeroSizedSourceViewSize,
                    target.zeroSizedSourceViewCodecClassName);
        }
        pointer.traitObjectCarrier(target.traitObjectCarrier);
        pointer.traitMetadataCarrier(target.traitMetadataCarrier);
        pointer.traitMetadataMarker(target.traitMetadataMarker);
        pointer.traitPointeeSize(target.traitPointeeSize);
        pointer.traitPointeeAlignment(target.traitPointeeAlignment);
        pointer.traitAdapterClassName(target.traitAdapterClassName);
        pointer.traitPointeeCodecClassName(target.traitPointeeCodecClassName);
        if (target.addressOrigin != null) {
            pointer.setAddressOrigin(target.addressOrigin, target.addressOriginOffset);
        }
        return pointer;
    }

    public static Pointer fromErasedAddress(long address) {
        return pointerObjectFromAddress(address).retype(0);
    }

    public static Pointer fromAddress(long address) {
        return fromAddress(address, 1);
    }

    public static Pointer fromEncodedAddress(
            long address,
            long viewSize,
            String viewCodecClassName,
            String pointerCodec) {
        return typedPointerObjectFromAddress(address, pointerCodec)
                .retype(viewSize, viewCodecClassName);
    }

    /** Decodes an integer-to-pointer transmute without acquiring exposed provenance. */
    public static Pointer fromUnprovenancedAddress(
            long address, long viewSize, String viewCodecClassName) {
        return withoutProvenance(address, 1).retype(viewSize, viewCodecClassName);
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
            synchronized (ALLOCATIONS) {
                AllocationInfo info = allocationInfo(zeroSizedBacking);
                info.alignment = alignment;
                // Preserve the Rust-visible dangling address. Multiple ZST
                // identities may legitimately share it; the exact exposed
                // target is refreshed whenever a pointer is published.
                long base = Math.subtractExact(exposedAddress, byteOffset);
                info.base = base;
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
                resultExposedAddress).withMetadata(metadata)
                .copyDynamicMetadata(this)
                .copyAddressOrigin(this, 0);
        if (zeroSizedSourceViewSize() >= 0) {
            result.setZeroSizedSourceView(
                    zeroSizedSourceViewSize(),
                    zeroSizedSourceViewCodecClassName());
        } else if ((newViewSize == 0 && newViewCodecClassName == null)
                || (viewSize == 0 && viewCodecClassName != null)) {
            result.setZeroSizedSourceView(viewSize, viewCodecClassName);
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

    /** Returns only the data word of a potentially wide Rust pointer. */
    static Pointer dataPointerView(
            Pointer pointer, long newViewSize, String newViewCodecClassName) {
        Pointer result = pointer.retype(newViewSize, newViewCodecClassName);
        result.metadata = 0;
        result.traitObjectCarrier(null);
        result.traitMetadataCarrier(null);
        result.traitMetadataMarker(null);
        result.traitPointeeSize(-1);
        result.traitPointeeAlignment(-1);
        result.traitAdapterClassName(null);
        result.traitPointeeCodecClassName(null);
        return result;
    }

    public static Pointer traitObjectDataPointer(
            Pointer pointer, long newViewSize, String newViewCodecClassName) {
        return RuntimeSupport.traitObjectDataPointer(
                pointer, newViewSize, newViewCodecClassName);
    }

    /** Converts the direct JVM carrier used by {@code &dyn Trait} into a raw fat pointer. */
    public static Pointer fromTraitObjectReference(Object reference) {
        if (!(reference instanceof TraitObjectCarrier)) {
            // A Rust function pointer already implements the callable JVM
            // interface used for dyn Fn, so it does not need a generated
            // TraitObjectCarrier adapter. Recover its concrete pointer-sized
            // layout here while retaining the function object as metadata.
            if (reference != null && isRustFunctionPointer(reference.getClass())) {
                Pointer dataPointer = cell(reference, Long.BYTES, null).retype(0);
                return attachTraitObjectCarrier(
                        dataPointer, reference, Long.BYTES, Long.BYTES);
            }
            throw new IllegalArgumentException(
                    "Rust trait-object reference has no dynamic metadata carrier");
        }

        TraitObjectCarrier carrier = (TraitObjectCarrier) reference;
        Object data = carrier.rustTraitObjectPayload();
        while (data instanceof TraitObjectCarrier) {
            Object next = ((TraitObjectCarrier) data).rustTraitObjectPayload();
            if (next == data) {
                throw new IllegalArgumentException("cyclic Rust trait-object carrier");
            }
            data = next;
        }

        Pointer dataPointer = data instanceof Pointer
                ? ((Pointer) data).retype(0)
                : cell(data, carrier.rustTraitObjectSize(), null);
        return attachTraitObjectCarrier(
                dataPointer,
                reference,
                carrier.rustTraitObjectSize(),
                carrier.rustTraitObjectAlignment());
    }

    public static Pointer attachTraitObjectCarrier(Pointer pointer, Object carrier) {
        return attachTraitObjectCarrier(pointer, carrier, -1, -1);
    }

    public static Pointer attachTraitObjectCarrier(
            Pointer pointer, Object carrier, long pointeeSize, long pointeeAlignment) {
        if (pointer == null || carrier == null) {
            throw new NullPointerException("trait-object pointer and carrier must be non-null");
        }
        if ((pointeeSize < 0) != (pointeeAlignment < 0)
                || pointeeAlignment == 0
                || (pointeeAlignment > 0
                        && (pointeeAlignment & (pointeeAlignment - 1)) != 0)) {
            throw new IllegalArgumentException("invalid trait-object pointee layout");
        }
        pointer = independentFieldMetadata(pointer);
        pointer.traitObjectCarrier(carrier);
        pointer.traitPointeeSize(pointeeSize);
        pointer.traitPointeeAlignment(pointeeAlignment);
        pointer.traitAdapterClassName(carrier.getClass().getName());
        return pointer;
    }

    public static Pointer attachPointeeTraitObjectCarrier(Pointer pointer) {
        return attachPointeeTraitObjectCarrier(pointer, -1, -1);
    }

    public static Pointer attachPointeeTraitObjectCarrier(
            Pointer pointer, long pointeeSize, long pointeeAlignment) {
        if (pointer == null) {
            throw new NullPointerException("trait-object pointer must be non-null");
        }
        Object carrier = pointer.getObject();
        if (carrier == null) {
            throw new NullPointerException("trait-object pointee must be non-null");
        }
        return attachTraitObjectCarrier(pointer, carrier, pointeeSize, pointeeAlignment);
    }

    /** Gives a struct-tail DST the vtable carried by its erased final field. */
    public static Pointer attachStructTailTraitMetadata(Pointer pointer, Pointer tailPointer) {
        if (pointer == null || tailPointer == null) {
            throw new NullPointerException("struct-tail trait metadata requires a trait pointer");
        }
        Object carrier = tailPointer.traitObjectCarrier() != null
                ? tailPointer.traitObjectCarrier()
                : tailPointer.traitMetadataCarrier();
        Object direct = tailPointer.directCellValueOrSelf();
        while (carrier == null && direct instanceof Pointer && direct != tailPointer) {
            Pointer nested = (Pointer) direct;
            carrier = nested.traitObjectCarrier() != null
                    ? nested.traitObjectCarrier()
                    : nested.traitMetadataCarrier();
            Object next = nested.directCellValueOrSelf();
            if (next == direct) {
                break;
            }
            direct = next;
        }
        if (carrier == null && direct instanceof TraitObjectCarrier) {
            carrier = direct;
        }
        if (carrier == null) {
            throw new IllegalArgumentException(
                    "struct-tail trait pointer does not carry dynamic metadata");
        }
        TraitObjectCarrier traitCarrier = (TraitObjectCarrier) carrier;
        pointer = independentFieldMetadata(pointer);
        pointer.traitMetadataCarrier(carrier);
        pointer.traitMetadataMarker(tailPointer.traitMetadataMarker());
        pointer.traitPointeeSize(tailPointer.traitPointeeSize() >= 0
                ? tailPointer.traitPointeeSize()
                : traitCarrier.rustTraitObjectSize());
        pointer.traitPointeeAlignment(tailPointer.traitPointeeAlignment() > 0
                ? tailPointer.traitPointeeAlignment()
                : traitCarrier.rustTraitObjectAlignment());
        pointer.traitAdapterClassName(tailPointer.traitAdapterClassName() != null
                ? tailPointer.traitAdapterClassName()
                : carrier.getClass().getName());
        pointer.traitPointeeCodecClassName(tailPointer.traitPointeeCodecClassName());
        if (pointer.traitPointeeCodecClassName() == null) {
            Object payload = traitCarrier.rustTraitObjectPayload();
            if (payload instanceof Pointer) {
                pointer.traitPointeeCodecClassName(((Pointer) payload).viewCodecClassName);
            }
        }
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

    /** Retypes a data pointer while copying the metadata word of another wide pointer. */
    public static Pointer retypeWithMetadataOf(
            Pointer pointer,
            Pointer metadataSource,
            long newViewSize,
            String newViewCodecClassName) {
        Pointer result = pointer.retype(newViewSize, newViewCodecClassName);
        result.metadata = metadataSource.metadata;
        result.traitObjectCarrier(metadataSource.traitObjectCarrier());
        result.traitMetadataCarrier(metadataSource.traitMetadataCarrier());
        result.traitMetadataMarker(metadataSource.traitMetadataMarker());
        result.traitPointeeSize(metadataSource.traitPointeeSize());
        result.traitPointeeAlignment(metadataSource.traitPointeeAlignment());
        result.traitAdapterClassName(metadataSource.traitAdapterClassName());
        result.traitPointeeCodecClassName(metadataSource.traitPointeeCodecClassName());
        return result;
    }

    /** Retypes a struct-tail data pointer and attaches the source trait vtable. */
    public static Pointer retypeStructTailWithMetadataOf(
            Pointer pointer,
            Pointer metadataSource,
            long newViewSize,
            String newViewCodecClassName) {
        Pointer result = attachStructTailTraitMetadata(
                pointer.retype(newViewSize, newViewCodecClassName), metadataSource);
        result.traitObjectCarrier(null);
        if (result.traitAdapterClassName() == null) {
            return result;
        }
        try {
            Pointer tailData = result.byteOffsetRetype(
                    newViewSize,
                    result.traitPointeeSize(),
                    result.traitPointeeCodecClassName());
            Class<?> adapter = resolvedRuntimeClass(result.traitAdapterClassName());
            result.traitMetadataCarrier(constructorWithArity(adapter, 1).newInstance(tailData));
            return result;
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not retarget Rust trait-object tail", error);
        }
    }

    /** Retypes a trait-object data pointer as its enclosing trait-tailed struct. */
    public static Pointer retypeStructTailFromTraitPointer(
            Pointer pointer,
            long newViewSize,
            String newViewCodecClassName) {
        return retypeStructTailWithMetadataOf(
                pointer, pointer, newViewSize, newViewCodecClassName);
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
        if (pointer.viewCodecClassName != null
                && pointer.viewCodecClassName.startsWith(STRUCT_TAIL_VIEW_CODEC_PREFIX)) {
            String[] descriptor = pointer.structTailViewDescriptor();
            return retargetStructTail(pointer, targetClassName, descriptor[2]);
        }
        String sourceCodec = pointer.viewCodecClassName == null
                ? ""
                : pointer.viewCodecClassName;
        Pointer result = pointer.retype(
                newViewSize,
                STRUCTURAL_VIEW_CODEC_PREFIX
                        + targetClassName + "\n" + pointer.viewSize + "\n" + sourceCodec);
        if (pointer.allocation == null) {
            return result;
        }
        try {
            Object source = pointer.getObject();
            Class<?> targetClass = resolvedRuntimeClass(targetClassName);
            for (RustField targetField : PUBLIC_INSTANCE_FIELDS.get(targetClass)) {
                if (!isSliceViewCarrierType(targetField.getType())) {
                    continue;
                }
                Object tail = instanceField(source.getClass(), targetField.getName()).get(source);
                long length = tail != null && tail.getClass().isArray()
                        ? Array.getLength(tail)
                        : sliceLogicalLength(tail);
                return result.withMetadata(length);
            }
        } catch (ReflectiveOperationException | IllegalArgumentException ignored) {
            // This is an ordinary sized structural coercion, not a slice-tailed DST.
        }
        return result;
    }

    public static Pointer unsizeStructTail(
            Pointer pointer,
            long prefixSize,
            String targetClassName,
            String tailViewClassName,
            long elementSize,
            String elementCodecClassName,
            long length) {
        if (pointer == null || prefixSize < 0 || elementSize < 0 || length < 0) {
            throw new IllegalArgumentException("invalid Rust struct-tail unsizing coercion");
        }
        String codec = elementCodecClassName == null ? "" : elementCodecClassName;
        return pointer.retype(
                        prefixSize,
                        STRUCT_TAIL_VIEW_CODEC_PREFIX
                                + targetClassName + "\n"
                                + prefixSize + "\n"
                                + tailViewClassName + "\n"
                                + elementSize + "\n"
                                + codec)
                .withMetadata(length);
    }

    /** Retargets an existing struct-tail fat pointer without changing its two Rust words. */
    public static Pointer retargetStructTail(
            Pointer pointer, String targetClassName, String tailViewClassName) {
        if (pointer == null
                || targetClassName == null
                || targetClassName.isEmpty()
                || tailViewClassName == null
                || tailViewClassName.isEmpty()) {
            throw new IllegalArgumentException("invalid Rust struct-tail pointer target");
        }
        if (pointer.viewCodecClassName == null
                || !pointer.viewCodecClassName.startsWith(STRUCT_TAIL_VIEW_CODEC_PREFIX)) {
            throw new IllegalArgumentException("pointer is not a Rust struct-tail view");
        }
        String[] descriptor = pointer.structTailViewDescriptor();
        return pointer.retype(
                        pointer.viewSize,
                        STRUCT_TAIL_VIEW_CODEC_PREFIX
                                + targetClassName + "\n"
                                + descriptor[1] + "\n"
                                + tailViewClassName + "\n"
                                + descriptor[3] + "\n"
                                + descriptor[4])
                .withMetadata(pointer.metadata());
    }

    public static Pointer restoreAllocationView(Pointer pointer) {
        Pointer result = new Pointer(
                pointer.allocation,
                pointer.allocationElementSize,
                pointer.byteOffset,
                pointer.allocationElementSize,
                pointer.allocationCodecClassName,
                pointer.allocationCodecClassName,
                pointer.exposedAddress).withMetadata(pointer.metadata)
                .copyAddressOrigin(pointer, 0);
        result.traitObjectCarrier(pointer.traitObjectCarrier());
        result.traitMetadataCarrier(pointer.traitMetadataCarrier());
        result.traitMetadataMarker(pointer.traitMetadataMarker());
        result.traitPointeeSize(pointer.traitPointeeSize());
        result.traitPointeeAlignment(pointer.traitPointeeAlignment());
        result.traitAdapterClassName(pointer.traitAdapterClassName());
        result.traitPointeeCodecClassName(pointer.traitPointeeCodecClassName());
        return result;
    }

    public static Pointer restoreErasedView(Pointer pointer) {
        if (pointer.zeroSizedSourceViewSize() >= 0) {
            return pointer.retype(
                    pointer.zeroSizedSourceViewSize(),
                    pointer.zeroSizedSourceViewCodecClassName());
        }
        return restoreAllocationView(pointer);
    }

    /** Rebuilds a slice-like JVM carrier after its data pointer passed through `NonNull<()>`. */
    public static Object restoreErasedSliceView(Pointer pointer, String viewClassName) {
        if (pointer == null || viewClassName == null || viewClassName.isEmpty()) {
            throw new IllegalArgumentException("invalid erased Rust slice view");
        }
        Pointer data = restoreErasedView(pointer);
        return SliceView.create(viewClassName, data, 0, pointer.metadata());
    }

    private static Pointer traitMetadataPointer(Object metadata, int depth)
            throws IllegalAccessException {
        if (metadata instanceof Pointer) {
            return (Pointer) metadata;
        }
        if (metadata == null || depth == 0) {
            return null;
        }
        for (RustField field : PUBLIC_INSTANCE_FIELDS.get(metadata.getClass())) {
            Object nested = field.get(metadata);
            Pointer marker = traitMetadataPointer(nested, depth - 1);
            if (marker != null) {
                return marker;
            }
        }
        return null;
    }

    /** Rebuilds both words and the JVM dispatch carrier of a raw trait-object pointer. */
    public static Pointer fromRawTraitParts(Pointer data, Object metadata) {
        try {
            Pointer marker = traitMetadataPointer(metadata, 4);
            if (marker == null) {
                throw new IllegalArgumentException("Rust trait metadata has no vtable pointer");
            }
            TraitMetadataInfo info = TRAIT_METADATA_INFO.get(marker.numericAddress());
            long pointeeSize = marker.traitPointeeSize();
            long pointeeAlignment = marker.traitPointeeAlignment();
            String adapterClassName = marker.traitAdapterClassName();
            String pointeeCodecClassName = marker.traitPointeeCodecClassName();
            if (info != null) {
                pointeeSize = info.size;
                pointeeAlignment = info.alignment;
                adapterClassName = info.adapterClassName;
                pointeeCodecClassName = info.pointeeCodecClassName;
            }
            Pointer source = pointeeSize >= 0
                    ? dataPointerView(data, pointeeSize, pointeeCodecClassName)
                    : restoreErasedView(data);
            Pointer result = source.retype(0);
            result.traitMetadataMarker(marker);
            result.traitPointeeSize(pointeeSize);
            result.traitPointeeAlignment(pointeeAlignment);
            result.traitAdapterClassName(adapterClassName);
            result.traitPointeeCodecClassName(pointeeCodecClassName);
            if (adapterClassName != null) {
                Class<?> adapter = resolvedRuntimeClass(adapterClassName);
                Object carrier = TraitObjectCarrier.class.isAssignableFrom(adapter)
                        ? constructorWithArity(adapter, 1).newInstance(source)
                        : adapter.isInstance(data.traitObjectCarrier())
                                ? data.traitObjectCarrier()
                                : source.getObjectAs(adapterClassName);
                result.traitObjectCarrier(carrier);
            }
            return result;
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not rebuild Rust trait-object pointer", error);
        }
    }

    /** Rebuilds a trait-tailed struct using the supplied vtable, not stale data metadata. */
    public static Pointer fromRawStructTraitParts(
            Pointer data, long prefixSize, String prefixCodec, Object metadata) {
        Pointer tail = fromRawTraitParts(data.byte_offset(prefixSize), metadata);
        Pointer result = data.retype(prefixSize, prefixCodec);
        result.traitObjectCarrier(null);
        return attachStructTailTraitMetadata(result, tail);
    }

    private Pointer withMetadata(long metadata) {
        this.metadata = metadata;
        return this;
    }

    private Pointer copyDynamicMetadata(Pointer source) {
        RarePointerState sourceState = source.rareState;
        if (sourceState == null
                || (sourceState.traitObjectCarrier == null
                        && sourceState.traitMetadataCarrier == null
                        && sourceState.traitMetadataMarker == null
                        && sourceState.traitPointeeSize == -1
                        && sourceState.traitPointeeAlignment == -1
                        && sourceState.traitAdapterClassName == null
                        && sourceState.traitPointeeCodecClassName == null)) {
            return this;
        }
        RarePointerState targetState = mutableRareState();
        targetState.traitObjectCarrier = sourceState.traitObjectCarrier;
        targetState.traitMetadataCarrier = sourceState.traitMetadataCarrier;
        targetState.traitMetadataMarker = sourceState.traitMetadataMarker;
        targetState.traitPointeeSize = sourceState.traitPointeeSize;
        targetState.traitPointeeAlignment = sourceState.traitPointeeAlignment;
        targetState.traitAdapterClassName = sourceState.traitAdapterClassName;
        targetState.traitPointeeCodecClassName = sourceState.traitPointeeCodecClassName;
        return this;
    }

    private Pointer inheritAddressOrigin(Pointer source, long additionalOffset) {
        AddressOriginState current = source.addressState;
        // Keep decoded backing cells alive for as long as a projected alias.
        if (current == null || current.addressOrigin == null || source.boundMemoryViewState() != null) {
            setAddressOrigin(source, additionalOffset);
        } else if (additionalOffset == 0) {
            addressState = current;
        } else {
            setAddressOrigin(current.addressOrigin,
                    Math.addExact(current.addressOriginOffset, additionalOffset));
        }
        return this;
    }

    private Pointer copyAddressOrigin(Pointer source, long additionalOffset) {
        AddressOriginState current = source.addressState;
        if (current != null && current.addressOrigin != null) {
            if (additionalOffset == 0) {
                addressState = current;
            } else {
                // Origin offsets are address words and must support wrapping arithmetic.
                setAddressOrigin(current.addressOrigin,
                        current.addressOriginOffset + additionalOffset);
            }
        }
        return this;
    }

    private static Pointer independentFieldMetadata(Pointer pointer) {
        // Detach metadata even if this field pointer is no longer the cached projection.
        return pointer.allocation instanceof FieldCell
                ? pointer.retype(pointer.viewSize, pointer.viewCodecClassName) : pointer;
    }

    public static Pointer withMetadata(Pointer pointer, long metadata) {
        return (pointer.metadata == metadata ? pointer : independentFieldMetadata(pointer)).withMetadata(metadata);
    }

    public static Pointer withMetadata(Pointer pointer, int metadata) {
        return withMetadata(pointer, Integer.toUnsignedLong(metadata));
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
                length = sliceLogicalLength(value);
            } catch (ReflectiveOperationException error) {
                throw new IllegalArgumentException("invalid Rust slice fat pointer", error);
            }
        } else {
            throw new IllegalArgumentException("Rust DST value does not carry slice metadata");
        }
        long unalignedSize = Math.addExact(prefixSize, Math.multiplyExact(length, elementSize));
        return Math.addExact(unalignedSize, alignment - 1) & -alignment;
    }

    /** Computes the dynamic layout size of a trait object or trait-tailed DST. */
    public static long sizeOfTraitTailed(
            Object value, long prefixSize, long prefixAlignment) {
        long pointeeSize = traitPointeeSize(value);
        long alignment = Math.max(prefixAlignment, traitPointeeAlignment(value));
        validateDynamicLayout(prefixSize, pointeeSize, alignment);
        long unalignedSize = Math.addExact(prefixSize, pointeeSize);
        return Math.addExact(unalignedSize, alignment - 1) & -alignment;
    }

    /** Computes the dynamic alignment of a trait object or trait-tailed DST. */
    public static long alignOfTraitTailed(Object value, long prefixAlignment) {
        long alignment = Math.max(prefixAlignment, traitPointeeAlignment(value));
        validateDynamicLayout(0, 0, alignment);
        return alignment;
    }

    private static long traitPointeeSize(Object value) {
        if (value instanceof Pointer) {
            Pointer pointer = (Pointer) value;
            if (pointer.traitPointeeSize() >= 0) {
                return pointer.traitPointeeSize();
            }
            value = pointer.traitObjectCarrier() != null
                    ? pointer.traitObjectCarrier()
                    : pointer.traitMetadataCarrier();
            if (value == null) {
                Object direct = pointer.directCellValueOrSelf();
                value = direct == pointer ? null : direct;
            }
        }
        if (value instanceof TraitObjectCarrier) {
            return ((TraitObjectCarrier) value).rustTraitObjectSize();
        }
        throw new IllegalArgumentException("Rust DST value does not carry trait-object size");
    }

    private static long traitPointeeAlignment(Object value) {
        if (value instanceof Pointer) {
            Pointer pointer = (Pointer) value;
            if (pointer.traitPointeeAlignment() > 0) {
                return pointer.traitPointeeAlignment();
            }
            value = pointer.traitObjectCarrier() != null
                    ? pointer.traitObjectCarrier()
                    : pointer.traitMetadataCarrier();
            if (value == null) {
                Object direct = pointer.directCellValueOrSelf();
                value = direct == pointer ? null : direct;
            }
        }
        if (value instanceof TraitObjectCarrier) {
            return ((TraitObjectCarrier) value).rustTraitObjectAlignment();
        }
        throw new IllegalArgumentException("Rust DST value does not carry trait-object alignment");
    }

    private static void validateDynamicLayout(long prefixSize, long pointeeSize, long alignment) {
        if (prefixSize < 0 || pointeeSize < 0
                || alignment <= 0 || (alignment & (alignment - 1)) != 0) {
            throw new IllegalArgumentException("invalid Rust dynamically sized layout");
        }
    }

    /** Returns to canonical aggregate storage when field-pointer arithmetic leaves its field. */
    private Pointer escapedFieldStorage(long byteDelta) {
        Pointer origin = addressOrigin();
        if (!(allocation instanceof FieldCell)
                || !((FieldCell) allocation).access.primitive
                || byteDelta == 0
                || origin == null) {
            return null;
        }
        return origin.byteOffsetRetype(
                        Math.addExact(addressOriginOffset(), byteDelta),
                        viewSize,
                        viewCodecClassName)
                .withMetadata(metadata)
                .copyDynamicMetadata(this);
    }

    public Pointer offset(long elementCount) {
        long delta = Math.multiplyExact(elementCount, (long) viewSize);
        if (rareState == null && addressState == null && allocation != null) {
            return new Pointer(
                    allocation,
                    allocationElementSize,
                    Math.addExact(byteOffset, delta),
                    viewSize,
                    allocationCodecClassName,
                    viewCodecClassName,
                    -1).withMetadata(metadata);
        }
        return offsetSlow(delta);
    }

    private Pointer offsetSlow(long delta) {
        Pointer escaped = escapedFieldStorage(delta);
        if (escaped != null) {
            return escaped;
        }
        if (allocation == null) {
            return new Pointer(
                    null,
                    allocationElementSize,
                    Math.addExact(byteOffset, delta),
                    viewSize,
                    allocationCodecClassName,
                    viewCodecClassName,
                    Math.addExact(exposedAddress, delta)).withMetadata(metadata)
                    .copyDynamicMetadata(this)
                    .copyAddressOrigin(this, delta);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                Math.addExact(byteOffset, delta),
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata).copyDynamicMetadata(this).copyAddressOrigin(this, delta);
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
        Pointer escaped = escapedFieldStorage(byteCount);
        if (escaped != null) {
            return escaped;
        }
        if (allocation == null) {
            return new Pointer(
                    null,
                    allocationElementSize,
                    Math.addExact(byteOffset, byteCount),
                    viewSize,
                    allocationCodecClassName,
                    viewCodecClassName,
                    Math.addExact(exposedAddress, byteCount)).withMetadata(metadata)
                    .copyDynamicMetadata(this)
                    .copyAddressOrigin(this, byteCount);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                Math.addExact(byteOffset, byteCount),
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata).copyDynamicMetadata(this)
                .copyAddressOrigin(this, byteCount);
    }

    private Pointer byteOffsetRetype(
            long byteCount, long newViewSize, String newViewCodecClassName) {
        if (allocation == null
                && exposedAddress != 0
                && newViewSize == 0
                && newViewCodecClassName != null) {
            return byte_offset(byteCount).retype(newViewSize, newViewCodecClassName);
        }
        Pointer result = new Pointer(
                        allocation,
                        allocationElementSize,
                        Math.addExact(byteOffset, byteCount),
                        newViewSize,
                        allocationCodecClassName,
                        newViewCodecClassName,
                        allocation == null
                                ? Math.addExact(exposedAddress, byteCount)
                                : -1)
                .withMetadata(metadata)
                .copyDynamicMetadata(this)
                .copyAddressOrigin(this, byteCount);
        if (zeroSizedSourceViewSize() >= 0) {
            result.setZeroSizedSourceView(
                    zeroSizedSourceViewSize(),
                    zeroSizedSourceViewCodecClassName());
        } else if ((newViewSize == 0 && newViewCodecClassName == null)
                || (viewSize == 0 && viewCodecClassName != null)) {
            result.setZeroSizedSourceView(viewSize, viewCodecClassName);
        }
        return result;
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
                    exposedAddress + byteCount).withMetadata(metadata)
                    .copyDynamicMetadata(this)
                    .copyAddressOrigin(this, byteCount);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                byteOffset + byteCount,
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata).copyDynamicMetadata(this)
                .copyAddressOrigin(this, byteCount);
    }

    public long align_offset(long alignment) {
        return alignmentOffset(numericAddress(), viewSize, alignment);
    }

    private static long alignmentOffset(long address, long stride, long alignment) {
        if (alignment <= 0 || (alignment & (alignment - 1)) != 0) {
            throw new IllegalArgumentException("Rust pointer alignment must be a power of two");
        }
        if (stride < 0) throw new IllegalArgumentException("negative Rust pointee size");
        long divisor = stride == 0 ? alignment : Math.min(Long.lowestOneBit(stride), alignment);
        if ((address & (divisor - 1)) != 0) return -1;
        long mask = alignment / divisor - 1;
        if (mask == 0) return 0;
        // Solve address + n * stride = 0 modulo the power-of-two alignment.
        // Divide by the gcd to get an odd stride with an inverse modulo 2^64.
        long odd = stride / divisor;
        long inverse = odd;
        for (int i = 0; i < 6; i++) inverse *= 2 - odd * inverse;
        return -(address >>> Long.numberOfTrailingZeros(divisor)) * inverse & mask;
    }

    public static long align_offset(Pointer pointer, long alignment) {
        return pointer.align_offset(alignment);
    }

    public int align_offset(int alignment) { return Math.toIntExact(align_offset((long) alignment)); }

    public static int align_offset(Pointer pointer, int alignment) {
        return pointer.align_offset(alignment);
    }

    public long addr() {
        return numericAddress();
    }

    public static long addr(Pointer pointer) {
        return numericAddress(pointer);
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
                    address).withMetadata(metadata).copyDynamicMetadata(this);
        }
        long currentAddress = numericAddress();
        long base = currentAddress - byteOffset;
        return new Pointer(
                allocation,
                allocationElementSize,
                address - base,
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata)
                .copyDynamicMetadata(this)
                .copyAddressOrigin(this, address - currentAddress);
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
                    exposedAddress + delta).withMetadata(metadata)
                    .copyDynamicMetadata(this)
                    .copyAddressOrigin(this, delta);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                byteOffset + delta,
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata).copyDynamicMetadata(this).copyAddressOrigin(this, delta);
    }

    private Object provenanceAllocation() {
        Pointer origin = addressOrigin();
        return origin == null ? allocation : origin.provenanceAllocation();
    }

    private long provenanceByteOffset() {
        Pointer origin = addressOrigin();
        return origin == null
                ? byteOffset
                : Math.addExact(origin.provenanceByteOffset(), addressOriginOffset());
    }

    public long offsetFrom(Pointer origin) {
        if (origin == null || provenanceAllocation() != origin.provenanceAllocation()) {
            throw new IllegalArgumentException("offset_from requires pointers into one allocation");
        }
        if (viewSize == 0) {
            throw new ArithmeticException("offset_from is undefined for zero-sized pointees");
        }
        long bytes = Math.subtractExact(provenanceByteOffset(), origin.provenanceByteOffset());
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
        if (origin == null || provenanceAllocation() != origin.provenanceAllocation()) {
            throw new IllegalArgumentException(
                    "byte_offset_from requires pointers into one allocation");
        }
        return Math.subtractExact(provenanceByteOffset(), origin.provenanceByteOffset());
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
        if (other == null) {
            return false;
        }
        if (allocation != null && allocation == other.allocation) {
            return byteOffset == other.byteOffset;
        }
        if (allocation == null && other.allocation == null) {
            return exposedAddress == other.exposedAddress;
        }
        return numericAddress() == other.numericAddress();
    }

    /** Compares the data and metadata words of a Rust raw pointer. */
    public boolean samePointer(Pointer other) {
        if (other == null) {
            return false;
        }
        if (traitObjectCarrier() != null
                || traitAdapterClassName() != null
                || traitMetadataMarker() != null
                || other.traitObjectCarrier() != null
                || other.traitAdapterClassName() != null
                || other.traitMetadataMarker() != null) {
            Pointer leftData = RuntimeSupport.traitObjectDataPointer(this, 0, null);
            Pointer rightData = RuntimeSupport.traitObjectDataPointer(other, 0, null);
            return leftData.samePointerWords(rightData);
        }
        return samePointerWords(other);
    }

    private boolean samePointerWords(Pointer other) {
        if (!sameAddress(other)) {
            return false;
        }
        return samePointerMetadataWords(other);
    }

    private boolean samePointerMetadataWords(Pointer other) {
        boolean leftHasDstMetadata = viewCodecClassName != null
                && viewCodecClassName.startsWith(STRUCT_TAIL_VIEW_CODEC_PREFIX);
        boolean rightHasDstMetadata = other.viewCodecClassName != null
                && other.viewCodecClassName.startsWith(STRUCT_TAIL_VIEW_CODEC_PREFIX);
        if ((leftHasDstMetadata || rightHasDstMetadata) && metadata != other.metadata) {
            return false;
        }
        if (traitMetadataMarker() != null && other.traitMetadataMarker() != null) {
            return traitMetadataMarker().sameAddress(other.traitMetadataMarker());
        }
        if (traitMetadataMarker() == null && other.traitMetadataMarker() == null) {
            return java.util.Objects.equals(
                    traitAdapterClassName(), other.traitAdapterClassName());
        }
        Pointer marker = traitMetadataMarker() != null
                ? traitMetadataMarker()
                : other.traitMetadataMarker();
        String unmaterializedAdapter = traitMetadataMarker() == null
                ? traitAdapterClassName()
                : other.traitAdapterClassName();
        TraitMetadataInfo info = TRAIT_METADATA_INFO.get(marker.numericAddress());
        String markerAdapter = info == null ? marker.traitAdapterClassName() : info.adapterClassName;
        return markerAdapter != null && markerAdapter.equals(unmaterializedAdapter);
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
            long leftLength = sliceLogicalLength(left);
            long rightLength = sliceLogicalLength(right);
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
            Object leftValue = arrayGet(left, index);
            Object rightValue = arrayGet(right, index);
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
        if (other != null
                && allocation != null
                && allocation == other.allocation
                && allocationElementSize > 0 && other.allocationElementSize > 0
                && addressOrigin() == null
                && other.addressOrigin() == null
                && byteOffset >= 0
                && other.byteOffset >= 0) {
            return Long.compareUnsigned(byteOffset, other.byteOffset);
        }
        return Long.compareUnsigned(numericAddress(), other.numericAddress());
    }

    /** Implements the compiler's byte-wise comparison intrinsic. */
    public static int compareBytes(Pointer left, Pointer right, long length) {
        if (length < 0) {
            throw new IllegalArgumentException("byte comparison length must not be negative");
        }
        if (left.allocation instanceof byte[] && right.allocation instanceof byte[]
                && left.allocationElementSize == 1 && right.allocationElementSize == 1) {
            byte[] leftBytes = (byte[]) left.allocation;
            byte[] rightBytes = (byte[]) right.allocation;
            int leftOffset = Math.toIntExact(left.byteOffset);
            int rightOffset = Math.toIntExact(right.byteOffset);
            int byteCount = checkedArrayLength(length);
            if (leftOffset < 0 || rightOffset < 0
                    || byteCount > leftBytes.length - leftOffset
                    || byteCount > rightBytes.length - rightOffset) {
                throw new IndexOutOfBoundsException("byte comparison exceeds its allocation");
            }
            for (int index = 0; index < byteCount; index++) {
                int leftByte = leftBytes[leftOffset + index] & 0xff;
                int rightByte = rightBytes[rightOffset + index] & 0xff;
                if (leftByte != rightByte) {
                    return leftByte - rightByte;
                }
            }
            return 0;
        }
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

    /** Test a reference or NonNull niche without exposing an address or creating a boundary. */
    public static long nullableLocationTag(Object root, long offset) {
        if (root == null) return offset == 0 ? 0 : 1;
        if (!(root instanceof Pointer)) return 1;
        Pointer pointer = (Pointer) root;
        if (pointer.allocation != null) return 1;
        long address = pointer.addressOrigin() == null
                ? pointer.exposedAddress : pointer.numericAddress();
        return address + offset == 0 ? 0 : 1;
    }

    public static long nullableTag(Pointer pointer) {
        return nullableLocationTag(pointer, 0);
    }

    /** Test the data address for null. An empty array or string still has a non-null root. */
    public static long nullableViewLocationTag(Object root, int start) {
        return start == 0 ? nullableLocationTag(root, 0) : 1;
    }

    public static boolean is_null(Pointer pointer) {
        return pointer == null || pointer.numericAddress() == 0;
    }

    public static boolean is_aligned_to(Pointer pointer, long alignment) {
        if (alignment <= 0 || (alignment & (alignment - 1)) != 0) {
            throw new IllegalArgumentException(
                    "is_aligned_to: align is not a power-of-two");
        }
        return (numericAddress(pointer) & (alignment - 1)) == 0;
    }

    public static boolean is_aligned_to(Pointer pointer, int alignment) {
        return is_aligned_to(pointer, Integer.toUnsignedLong(alignment));
    }

    private static long allocationCapacity(Object allocation, int elementSize) {
        if (!allocation.getClass().isArray()) {
            return elementSize;
        }
        long direct = Math.multiplyExact((long) Array.getLength(allocation), elementSize);
        long nested = nestedPrimitiveArrayByteSize(allocation);
        return Math.max(direct, nested);
    }

    /** Must be called while holding {@link #ALLOCATIONS}. */
    private static void registerExposedTarget(long address, ExposedTarget target) {
        ExposedTarget previous = EXPOSED_ADDRESSES.put(address, target);
        if (previous != null && previous.allocation != target.allocation) {
            Set<Long> previousAddresses =
                    ALLOCATION_EXPOSED_ADDRESSES.get(previous.allocation);
            if (previousAddresses != null) {
                previousAddresses.remove(address);
                if (previousAddresses.isEmpty()) {
                    ALLOCATION_EXPOSED_ADDRESSES.remove(previous.allocation);
                }
            }
        }
        if (target.allocation != null) {
            Set<Long> addresses = ALLOCATION_EXPOSED_ADDRESSES.get(target.allocation);
            if (addresses == null) {
                addresses = new HashSet<>();
                ALLOCATION_EXPOSED_ADDRESSES.put(target.allocation, addresses);
            }
            addresses.add(address);
        }
    }

    private static void registerTypedExposedTarget(
            long address, String pointerCodec, ExposedTarget target) {
        discardCollectedTypedExposedTargets(8);
        Long key = Long.valueOf(address);
        TypedExposedEntry entry = TYPED_EXPOSED_ADDRESSES.get(key);
        if (entry == null) {
            TypedExposedEntry candidate =
                    new TypedExposedEntry(address, pointerCodec, target);
            entry = TYPED_EXPOSED_ADDRESSES.putIfAbsent(key, candidate);
            if (entry == null) {
                return;
            }
        }
        entry.put(address, pointerCodec, target);
    }

    private static void discardCollectedTypedExposedTargets(int limit) {
        if (TYPED_EXPOSED_OPERATIONS_UNTIL_QUEUE_DRAIN.decrementAndGet() > 0) {
            return;
        }
        TYPED_EXPOSED_OPERATIONS_UNTIL_QUEUE_DRAIN.set(16);
        for (int count = 0; count < limit * 8; count++) {
            TypedExposedReference reference =
                    (TypedExposedReference) TYPED_EXPOSED_TARGET_QUEUE.poll();
            if (reference == null) {
                return;
            }
            TypedExposedEntry entry = TYPED_EXPOSED_ADDRESSES.get(reference.address);
            if (entry == null) {
                continue;
            }
            entry.remove(reference);
            if (entry.isEmpty()) {
                TYPED_EXPOSED_ADDRESSES.remove(reference.address, entry);
            }
        }
    }

    public static Object asRefOption(Pointer pointer, String someClassName, String noneClassName) {
        String variantName = is_null(pointer) ? noneClassName : someClassName;
        try {
            // Shared enum interfaces need not own the payload class. Use the exact compiler-supplied variant.
            Class<?> variant = resolvedRuntimeClass(variantName);
            if (is_null(pointer)) {
                return constructorWithArity(variant, 0).newInstance();
            }
            ConstructorPlan constructor = constructorWithArity(variant, 1);
            Class<?> referentType = constructor.parameterTypes[0];
            Object referent = referentType.isInstance(pointer)
                    ? pointer
                    : pointer.getObject();
            if (referent == null || !referentType.isInstance(referent)) {
                throw new IllegalArgumentException(
                        "pointer referent "
                                + (referent == null ? "<null>" : referent.getClass().getName())
                                + " does not implement " + referentType.getName());
            }
            return constructor.newInstance(referent);
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException(
                    "could not construct Rust pointer option " + variantName, error);
        }
    }

    private long numericAddress() {
        Pointer origin = addressOrigin();
        if (origin != null) {
            return origin.numericAddress() + addressOriginOffset();
        }
        if (allocation == null) {
            return exposedAddress;
        }
        long cachedAddress = publishedAddress();
        if (cachedAddress != Long.MIN_VALUE) {
            return cachedAddress;
        }
        Long cachedBase =
                cachedAllocationBase(ALLOCATION_BASE_CACHE, allocation);
        if (cachedBase != null) {
            return cachedBase.longValue() + byteOffset;
        }
        synchronized (ALLOCATIONS) {
            AllocationInfo info = allocationInfo(allocation);
            // Address arithmetic can wrap. Memory accesses check their own ranges.
            return allocationBase(info) + byteOffset;
        }
    }

    private static long numericAddress(Pointer pointer) {
        return pointer == null ? 0L : pointer.numericAddress();
    }

    /** Must be called while holding {@link #ALLOCATIONS}. */
    private long allocationBase(AllocationInfo info) {
        return allocationBase(allocation, allocationElementSize, info);
    }

    private static long allocationBase(Object allocation, int allocationElementSize, AllocationInfo info) {
        Long cached = cachedAllocationBase(ALLOCATION_BASE_CACHE, allocation);
        if (cached != null) {
            return cached.longValue();
        }
        Long base = info.base;
        if (base == null) {
            long byteCapacity = allocationCapacity(allocation, allocationElementSize);
            long requiredSpan = Math.addExact(byteCapacity, 1L);
            long span = Math.max(16L, Math.addExact(requiredSpan, 15L) & ~15L);
            base = allocateAddress(span, info.alignment);
            info.base = base;
        }
        cacheAllocationBase(ALLOCATION_BASE_CACHE, allocation, base.longValue());
        return base;
    }

    /** Must be called while holding {@link #ALLOCATIONS}. */
    private static void discardCollectedAllocationRanges() {
        AllocationReference reference;
        while ((reference = (AllocationReference) ALLOCATION_RANGE_QUEUE.poll()) != null) {
            AllocationRange range = ALLOCATION_RANGES.get(reference.base);
            if (range != null && range.allocation == reference) {
                ALLOCATION_RANGES.remove(reference.base);
            }
        }
    }

    /** Must be called while holding {@link #ALLOCATIONS}. */
    private long publishAllocationRange(AllocationInfo info) {
        discardCollectedAllocationRanges();
        long base = allocationBase(info);
        if (!info.rangePublished) {
            long byteCapacity = allocationCapacity(allocation, allocationElementSize);
            ALLOCATION_RANGES.put(
                    base,
                    new AllocationRange(
                            base,
                            allocation,
                            allocationElementSize,
                            byteCapacity,
                            allocationCodecClassName));
            info.rangePublished = true;
        }
        return base;
    }

    /**
     * Encodes a pointer inside JVM-managed Rust storage without treating the
     * operation as a pointer-to-integer provenance exposure. The owner keeps
     * the referenced allocation alive for exactly as long as those bytes.
     */
    public static long encodedAddress(Pointer pointer, Object owner) {
        if (pointer == null) {
            return 0L;
        }
        Pointer origin = pointer.addressOrigin();
        if (origin != null) {
            long address = encodedAddress(origin, owner) + pointer.addressOriginOffset();
            pointer.setPublishedAddress(address);
            return address;
        }
        if (pointer.allocation == null) {
            return pointer.exposedAddress;
        }
        Long publishedBase = cachedAllocationBase(
                PUBLISHED_ALLOCATION_BASE_CACHE, pointer.allocation);
        long address;
        if (publishedBase != null) {
            address = publishedBase.longValue() + pointer.byteOffset;
        } else {
            synchronized (ALLOCATIONS) {
                AllocationInfo info = allocationInfo(pointer.allocation);
                long base = pointer.publishAllocationRange(info);
                cacheAllocationBase(
                        PUBLISHED_ALLOCATION_BASE_CACHE,
                        pointer.allocation,
                        base);
                address = base + pointer.byteOffset;
            }
        }
        pointer.setPublishedAddress(address);
        retainEncodedReference(owner, pointer.allocation);
        return address;
    }

    public static long encodedAddress(
            Pointer pointer, Object owner, String pointerCodec) {
        long address = encodedAddress(pointer, owner);
        if (pointer == null || pointerCodec == null) {
            return address;
        }
        ExposedTarget target = pointer.exposedTarget();
        pointer.setPublishedTarget(address, target);
        registerTypedExposedTarget(address, pointerCodec, target);
        if (pointer.allocation != null) {
            retainEncodedReference(owner, pointer.allocation);
        }
        return address;
    }

    /**
     * Encodes a typed pointer into a known byte range and preserves its exact
     * provenance independently of unrelated pointers retained by the owner.
     */
    public static long encodedAddress(
            Pointer pointer,
            Object owner,
            int ownerOffset,
            int encodedSize,
            String pointerCodec) {
        // The exact range state below retains the target. Adding the same
        // allocation to the owner's broad reference set makes temporary
        // aggregate copies inherit every pointer the owner ever contained.
        long address = encodedAddress(pointer, (Object) null);
        if (pointer != null && pointerCodec != null) {
            ExposedTarget target = pointer.exposedTarget();
            pointer.setPublishedTarget(address, target);
            registerTypedExposedTarget(address, pointerCodec, target);
            rememberEncodedPointer(
                    owner,
                    ownerOffset,
                    encodedSize,
                    pointerCodec,
                    target);
        }
        return address;
    }

    private ExposedTarget exposedTarget() {
        ExposedTarget cached = publishedTarget();
        if (cached != null && cached.matches(this)) {
            return cached;
        }
        return new ExposedTarget(
                allocation,
                allocationElementSize,
                byteOffset,
                exposedAddress,
                allocationCodecClassName,
                viewSize,
                viewCodecClassName,
                metadata,
                zeroSizedSourceViewSize(),
                zeroSizedSourceViewCodecClassName(),
                traitObjectCarrier(),
                traitMetadataCarrier(),
                traitMetadataMarker(),
                traitPointeeSize(),
                traitPointeeAlignment(),
                traitAdapterClassName(),
                traitPointeeCodecClassName(),
                addressOrigin(),
                addressOriginOffset());
    }

    private ExposedTarget exposedTargetAt(long displacement) {
        return new ExposedTarget(
                allocation,
                allocationElementSize,
                Math.addExact(byteOffset, displacement),
                allocation == null
                        ? Math.addExact(exposedAddress, displacement)
                        : exposedAddress,
                allocationCodecClassName,
                viewSize,
                viewCodecClassName,
                metadata,
                zeroSizedSourceViewSize(),
                zeroSizedSourceViewCodecClassName(),
                traitObjectCarrier(),
                traitMetadataCarrier(),
                traitMetadataMarker(),
                traitPointeeSize(),
                traitPointeeAlignment(),
                traitAdapterClassName(),
                traitPointeeCodecClassName(),
                addressOrigin(),
                Math.addExact(addressOriginOffset(), displacement));
    }

    /** Exposes this pointer's provenance so its numeric address can be recovered later. */
    public long address() {
        Pointer origin = addressOrigin();
        if (origin != null) {
            long address = origin.address() + addressOriginOffset();
            setPublishedAddress(address);
            return address;
        }
        if (allocation == null) {
            return exposedAddress;
        }
        ExposedTarget cachedTarget = publishedTarget();
        if (cachedTarget != null && cachedTarget.matches(this)) {
            return publishedAddress();
        }
        synchronized (ALLOCATIONS) {
            AllocationInfo info = allocationInfo(allocation);
            long base = publishAllocationRange(info);
            long address = base + byteOffset;
            ExposedTarget target = exposedTarget();
            registerExposedTarget(address, target);
            setPublishedTarget(address, target);
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
            if (pointer.erasedAddressToken() >= 0) {
                return pointer.erasedAddressToken();
            }
            synchronized (ALLOCATIONS) {
                long token = allocateAddress(16L, 16);
                registerExposedTarget(
                        token,
                        pointer.exposedTarget());
                pointer.setErasedAddressToken(token);
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
                Object scalar = allocation instanceof Cell
                        ? ((Cell) allocation).value
                        : ((ReceiverCell) allocation).value;
                String constantIdentity = null;
                for (Map.Entry<String, Pointer> entry : CONSTANT_CELLS.entrySet()) {
                    if (entry.getValue().allocation == allocation) {
                        constantIdentity = entry.getKey();
                        break;
                    }
                }
                throw new IndexOutOfBoundsException(
                        "pointer arithmetic escaped scalar storage: element=" + elementIndex
                                + ", byte_offset=" + byteOffset
                                + ", allocation_element_size=" + allocationElementSize
                                + ", view_size=" + viewSize
                                + ", allocation_codec=" + allocationCodecClassName
                                + ", view_codec=" + viewCodecClassName
                                + ", scalar_class="
                                + (scalar == null ? "null" : scalar.getClass().getName())
                                + ", constant_identity=" + constantIdentity);
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
        if (allocation.getClass().isArray()
                && allocation.getClass().getComponentType().isArray()) {
            long nestedElementSize = nestedPrimitiveArrayElementByteSize(allocation);
            if (nestedElementSize > allocationElementSize) {
                return nestedPrimitiveArrayElement(
                        allocation,
                        Math.multiplyExact((long) elementIndex, allocationElementSize),
                        allocationElementSize);
            }
        }
        return independentRepeatedArrayElement(allocation, elementIndex);
    }

    /**
     * Returns a primitive fixed-array value stored as one aggregate element.
     * Pointers projected into `[T; N]` otherwise encode and decode the entire
     * array for every scalar access, turning ordinary indexed loops quadratic.
     */
    private Object embeddedPrimitiveArray() {
        Object value;
        if (allocation instanceof Cell) {
            value = ((Cell) allocation).value;
        } else if (allocation instanceof ReceiverCell) {
            value = ((ReceiverCell) allocation).value;
        } else if (allocation instanceof FieldCell) {
            value = ((FieldCell) allocation).get();
        } else {
            return null;
        }
        int length = primitiveArrayLength(value);
        if (length < 0) {
            return null;
        }
        if (length == 0 || allocationElementSize % length != 0) {
            return null;
        }
        int elementSize = allocationElementSize / length;
        return elementSize > 0 && elementSize <= 8 ? value : null;
    }

    private int embeddedArrayElementSize(Object array) {
        return allocationElementSize / primitiveArrayLength(array);
    }

    /** Returns -1 for reference arrays and non-arrays. */
    private static int primitiveArrayLength(Object array) {
        if (array instanceof byte[]) {
            return ((byte[]) array).length;
        }
        if (array instanceof boolean[]) {
            return ((boolean[]) array).length;
        }
        if (array instanceof short[]) {
            return ((short[]) array).length;
        }
        if (array instanceof char[]) {
            return ((char[]) array).length;
        }
        if (array instanceof int[]) {
            return ((int[]) array).length;
        }
        if (array instanceof long[]) {
            return ((long[]) array).length;
        }
        if (array instanceof float[]) {
            return ((float[]) array).length;
        }
        if (array instanceof double[]) {
            return ((double[]) array).length;
        }
        return -1;
    }

    private static long primitiveArrayBits(Object array, int index) {
        if (array instanceof byte[]) {
            return ((byte[]) array)[index] & 0xffL;
        }
        if (array instanceof boolean[]) {
            return ((boolean[]) array)[index] ? 1L : 0L;
        }
        if (array instanceof short[]) {
            return ((short[]) array)[index] & 0xffffL;
        }
        if (array instanceof char[]) {
            return ((char[]) array)[index];
        }
        if (array instanceof int[]) {
            return Integer.toUnsignedLong(((int[]) array)[index]);
        }
        if (array instanceof long[]) {
            return ((long[]) array)[index];
        }
        if (array instanceof float[]) {
            return Integer.toUnsignedLong(
                    Float.floatToRawIntBits(((float[]) array)[index]));
        }
        if (array instanceof double[]) {
            return Double.doubleToRawLongBits(((double[]) array)[index]);
        }
        throw new IllegalArgumentException("not a primitive JVM array");
    }

    private static void storePrimitiveArrayBits(Object array, int index, long bits) {
        if (array instanceof byte[]) {
            ((byte[]) array)[index] = (byte) bits;
        } else if (array instanceof boolean[]) {
            ((boolean[]) array)[index] = bits != 0;
        } else if (array instanceof short[]) {
            ((short[]) array)[index] = (short) bits;
        } else if (array instanceof char[]) {
            ((char[]) array)[index] = (char) bits;
        } else if (array instanceof int[]) {
            ((int[]) array)[index] = (int) bits;
        } else if (array instanceof long[]) {
            ((long[]) array)[index] = bits;
        } else if (array instanceof float[]) {
            ((float[]) array)[index] = Float.intBitsToFloat((int) bits);
        } else if (array instanceof double[]) {
            ((double[]) array)[index] = Double.longBitsToDouble(bits);
        } else {
            throw new IllegalArgumentException("not a primitive JVM array");
        }
    }

    /** Returns the byte width of a primitive fixed-array carrier, if compatible. */
    private static int primitiveArrayElementSize(Object value, int aggregateSize) {
        if (value == null
                || !value.getClass().isArray()
                || !value.getClass().getComponentType().isPrimitive()) {
            return 0;
        }
        int length = Array.getLength(value);
        if (length == 0) {
            return aggregateSize == 0 ? 1 : 0;
        }
        int elementSize = inferredArrayElementSize(value);
        return (long) length * elementSize == aggregateSize ? elementSize : 0;
    }

    private static int loadPrimitiveArrayByte(Object array, int byteOffset, int elementSize) {
        int elementIndex = byteOffset / elementSize;
        int withinElement = byteOffset % elementSize;
        long bits = primitiveArrayBits(array, elementIndex);
        return (int) ((bits >>> (withinElement * 8)) & 0xffL);
    }

    private static void storePrimitiveArrayByte(
            Object array, int byteOffset, int elementSize, int incoming) {
        int elementIndex = byteOffset / elementSize;
        int withinElement = byteOffset % elementSize;
        long bits = primitiveArrayBits(array, elementIndex);
        long mask = 0xffL << (withinElement * 8);
        long updated = (bits & ~mask) | (((long) incoming & 0xffL) << (withinElement * 8));
        storePrimitiveArrayBits(array, elementIndex, updated);
    }

    private static long loadArrayCodecBits(
            Object array, int byteOffset, int byteCount, MemoryCodec arrayPlan) {
        if (!arrayPlan.isArrayCodecFor(array)) {
            throw new IllegalArgumentException("pointer codec is not a fixed-array codec");
        }
        int elementSize = arrayPlan.arrayElementSize;
        int length = Array.getLength(array);
        long totalSize = Math.multiplyExact((long) length, elementSize);
        if (byteOffset < 0 || byteCount < 0
                || Math.addExact((long) byteOffset, byteCount) > totalSize) {
            throw new IndexOutOfBoundsException("scalar access exceeds fixed-array storage");
        }

        long result = 0;
        int consumed = 0;
        while (consumed < byteCount) {
            int absolute = byteOffset + consumed;
            int elementIndex = absolute / elementSize;
            int withinElement = absolute % elementSize;
            int chunk = Math.min(byteCount - consumed, elementSize - withinElement);
            Object element = arrayGet(array, elementIndex);

            if (isPrimitiveScalarCarrier(element) && elementSize <= 8) {
                long bits = valueBits(element, elementSize);
                long selected = (bits >>> (withinElement * 8)) & atomicMask(chunk);
                result |= selected << (consumed * 8);
                consumed += chunk;
                continue;
            }

            byte[] image;
            boolean temporary = false;
            if (isGeneratedAggregateCodec(arrayPlan.arrayElementCodec)) {
                MemoryCodec elementPlan = codecPlan(arrayPlan.arrayElementCodec);
                if (elementPlan.isArrayCodecFor(element)) {
                    long selected =
                            loadArrayCodecBits(element, withinElement, chunk, elementPlan);
                    result |= selected << (consumed * 8);
                    consumed += chunk;
                    continue;
                }
                image = elementPlan.directUnionBytes(element);
                if (image == null) {
                    image = elementPlan.encode().encode(element);
                    temporary = true;
                }
            } else {
                image = encodeMemoryValue(
                        element, elementSize, arrayPlan.arrayElementCodec);
                temporary = true;
            }
            if (withinElement + chunk > image.length) {
                if (temporary) {
                    discardEncodedReferences(image);
                }
                throw new IndexOutOfBoundsException(
                        "array element codec returned a short memory image");
            }
            for (int index = 0; index < chunk; index++) {
                result |= ((long) image[withinElement + index] & 0xffL)
                        << ((consumed + index) * 8);
            }
            if (temporary) {
                discardEncodedReferences(image);
            }
            consumed += chunk;
        }
        return result;
    }

    private static void loadArrayCodecRange(
            Object array,
            int byteOffset,
            byte[] target,
            int targetOffset,
            int byteCount,
            MemoryCodec arrayPlan) {
        if (!arrayPlan.isArrayCodecFor(array)) {
            throw new IllegalArgumentException("pointer codec is not a fixed-array codec");
        }
        int elementSize = arrayPlan.arrayElementSize;
        int length = Array.getLength(array);
        long totalSize = Math.multiplyExact((long) length, elementSize);
        if (byteOffset < 0 || byteCount < 0
                || Math.addExact((long) byteOffset, byteCount) > totalSize
                || targetOffset < 0
                || targetOffset + byteCount > target.length) {
            throw new IndexOutOfBoundsException("range access exceeds fixed-array storage");
        }

        int consumed = 0;
        while (consumed < byteCount) {
            int absolute = byteOffset + consumed;
            int elementIndex = absolute / elementSize;
            int withinElement = absolute % elementSize;
            int chunk = Math.min(byteCount - consumed, elementSize - withinElement);
            Object element = arrayGet(array, elementIndex);

            if (isPrimitiveScalarCarrier(element) && elementSize <= 8) {
                long bits = valueBits(element, elementSize);
                for (int index = 0; index < chunk; index++) {
                    target[targetOffset + consumed + index] =
                            (byte) (bits >>> ((withinElement + index) * 8));
                }
                consumed += chunk;
                continue;
            }

            byte[] image;
            boolean temporary = false;
            if (isGeneratedAggregateCodec(arrayPlan.arrayElementCodec)) {
                MemoryCodec elementPlan = codecPlan(arrayPlan.arrayElementCodec);
                if (elementPlan.isArrayCodecFor(element)) {
                    loadArrayCodecRange(
                            element,
                            withinElement,
                            target,
                            targetOffset + consumed,
                            chunk,
                            elementPlan);
                    consumed += chunk;
                    continue;
                }
                image = elementPlan.directUnionBytes(element);
                if (image == null) {
                    image = elementPlan.encode().encode(element);
                    temporary = true;
                }
            } else {
                image = encodeMemoryValue(
                        element, elementSize, arrayPlan.arrayElementCodec);
                temporary = true;
            }
            if (withinElement + chunk > image.length) {
                if (temporary) {
                    discardEncodedReferences(image);
                }
                throw new IndexOutOfBoundsException(
                        "array element codec returned a short memory image");
            }
            System.arraycopy(
                    image,
                    withinElement,
                    target,
                    targetOffset + consumed,
                    chunk);
            transferEncodedPointers(
                    image,
                    withinElement,
                    target,
                    targetOffset + consumed,
                    chunk,
                    false);
            transferEncodedReferences(image, target);
            if (temporary) {
                discardEncodedReferences(image);
            }
            consumed += chunk;
        }
    }

    private static void storeArrayCodecBits(
            Object array, int byteOffset, long incoming, int byteCount, MemoryCodec arrayPlan) {
        if (!arrayPlan.isArrayCodecFor(array)) {
            throw new IllegalArgumentException("pointer codec is not a fixed-array codec");
        }
        int elementSize = arrayPlan.arrayElementSize;
        int length = Array.getLength(array);
        long totalSize = Math.multiplyExact((long) length, elementSize);
        if (byteOffset < 0 || byteCount < 0
                || Math.addExact((long) byteOffset, byteCount) > totalSize) {
            throw new IndexOutOfBoundsException("scalar access exceeds fixed-array storage");
        }

        int consumed = 0;
        while (consumed < byteCount) {
            int absolute = byteOffset + consumed;
            int elementIndex = absolute / elementSize;
            int withinElement = absolute % elementSize;
            int chunk = Math.min(byteCount - consumed, elementSize - withinElement);
            Object element = independentRepeatedArrayElement(array, elementIndex);
            long selected = (incoming >>> (consumed * 8)) & atomicMask(chunk);

            if (isPrimitiveScalarCarrier(element) && elementSize <= 8) {
                int shift = withinElement * 8;
                long mask = atomicMask(chunk) << shift;
                long current = valueBits(element, elementSize);
                arraySet(
                        array,
                        elementIndex,
                        carrierFromBits(
                                element,
                                (current & ~mask) | (selected << shift),
                                elementSize));
                consumed += chunk;
                continue;
            }

            if (isGeneratedAggregateCodec(arrayPlan.arrayElementCodec)) {
                MemoryCodec elementPlan = codecPlan(arrayPlan.arrayElementCodec);
                if (elementPlan.isArrayCodecFor(element)) {
                    storeArrayCodecBits(
                            element, withinElement, selected, chunk, elementPlan);
                    consumed += chunk;
                    continue;
                }
                byte[] direct = elementPlan.directUnionBytes(element);
                Object[] objects = elementPlan.directUnionObjects(element);
                if (direct != null
                        && elementSize == 1
                        && withinElement == 0
                        && chunk == 1
                        && direct.length == 1
                        && (objects == null || objects.length == 0 || objects[0] == null)) {
                    discardEncodedPointers(direct, 0, 1);
                    direct[0] = (byte) selected;
                    consumed++;
                    continue;
                }
            }

            byte[] image = encodeMemoryValue(
                    element, elementSize, arrayPlan.arrayElementCodec);
            if (withinElement + chunk > image.length) {
                discardEncodedReferences(image);
                throw new IndexOutOfBoundsException(
                        "array element codec returned a short memory image");
            }
            for (int index = 0; index < chunk; index++) {
                image[withinElement + index] =
                        (byte) (selected >>> (index * 8));
            }
            Object updated = decodeMemoryValue(
                    image,
                    0,
                    elementSize,
                    arrayPlan.arrayElementCodec,
                    array.getClass().getComponentType());
            discardEncodedReferences(image);
            arraySet(array, elementIndex, updated);
            consumed += chunk;
        }
    }

    private static void storeArrayCodecRange(
            Object array,
            int byteOffset,
            byte[] source,
            int sourceOffset,
            int byteCount,
            MemoryCodec arrayPlan) {
        if (!arrayPlan.isArrayCodecFor(array)) {
            throw new IllegalArgumentException("pointer codec is not a fixed-array codec");
        }
        int elementSize = arrayPlan.arrayElementSize;
        int length = Array.getLength(array);
        long totalSize = Math.multiplyExact((long) length, elementSize);
        if (byteOffset < 0 || byteCount < 0
                || Math.addExact((long) byteOffset, byteCount) > totalSize
                || sourceOffset < 0
                || sourceOffset + byteCount > source.length) {
            throw new IndexOutOfBoundsException("range access exceeds fixed-array storage");
        }

        int consumed = 0;
        while (consumed < byteCount) {
            int absolute = byteOffset + consumed;
            int elementIndex = absolute / elementSize;
            int withinElement = absolute % elementSize;
            int chunk = Math.min(byteCount - consumed, elementSize - withinElement);
            Object element = independentRepeatedArrayElement(array, elementIndex);

            if (isPrimitiveScalarCarrier(element) && elementSize <= 8) {
                long current = valueBits(element, elementSize);
                for (int index = 0; index < chunk; index++) {
                    int shift = (withinElement + index) * 8;
                    current = (current & ~(0xffL << shift))
                            | (((long) source[sourceOffset + consumed + index] & 0xffL)
                                    << shift);
                }
                arraySet(
                        array,
                        elementIndex,
                        carrierFromBits(element, current, elementSize));
                consumed += chunk;
                continue;
            }

            if (isGeneratedAggregateCodec(arrayPlan.arrayElementCodec)) {
                MemoryCodec elementPlan = codecPlan(arrayPlan.arrayElementCodec);
                if (elementPlan.isArrayCodecFor(element)) {
                    storeArrayCodecRange(
                            element,
                            withinElement,
                            source,
                            sourceOffset + consumed,
                            chunk,
                            elementPlan);
                    consumed += chunk;
                    continue;
                }
                byte[] direct = elementPlan.directUnionBytes(element);
                Object[] objects = elementPlan.directUnionObjects(element);
                if (direct != null
                        && elementSize == 1
                        && withinElement == 0
                        && chunk == 1
                        && direct.length == 1
                        && (objects == null || objects.length == 0 || objects[0] == null)
                        && !mayBeInIdentityFilter(
                                ENCODED_POINTER_FILTER, source)) {
                    discardEncodedPointers(direct, 0, 1);
                    direct[0] = source[sourceOffset + consumed];
                    consumed++;
                    continue;
                }
            }

            byte[] image = encodeMemoryValue(
                    element, elementSize, arrayPlan.arrayElementCodec);
            if (withinElement + chunk > image.length) {
                discardEncodedReferences(image);
                throw new IndexOutOfBoundsException(
                        "array element codec returned a short memory image");
            }
            transferEncodedPointers(
                    source,
                    sourceOffset + consumed,
                    image,
                    withinElement,
                    chunk,
                    false);
            transferEncodedReferences(source, image);
            System.arraycopy(
                    source,
                    sourceOffset + consumed,
                    image,
                    withinElement,
                    chunk);
            Object updated = decodeMemoryValue(
                    image,
                    0,
                    elementSize,
                    arrayPlan.arrayElementCodec,
                    array.getClass().getComponentType());
            discardEncodedReferences(image);
            arraySet(array, elementIndex, updated);
            consumed += chunk;
        }
    }

    private Long loadEmbeddedArrayBits(long absoluteByteOffset, int byteCount) {
        return loadEmbeddedArrayBits(absoluteByteOffset, byteCount, true);
    }

    private Long loadEmbeddedArrayBits(
            long absoluteByteOffset, int byteCount, boolean flushMemoryViews) {
        Object array = embeddedPrimitiveArray();
        if (array == null) {
            return null;
        }
        int elementSize = embeddedArrayElementSize(array);
        int withinElement = (int) Math.floorMod(absoluteByteOffset, elementSize);
        if (withinElement + byteCount > elementSize || byteCount > 8) {
            return null;
        }
        if (flushMemoryViews) {
            flushMemoryViewsOverlapping(absoluteByteOffset, byteCount);
        }
        int elementIndex = Math.toIntExact(Math.floorDiv(absoluteByteOffset, elementSize));
        long bits = primitiveArrayBits(array, elementIndex);
        return Long.valueOf((bits >>> (withinElement * 8)) & atomicMask(byteCount));
    }

    private boolean storeEmbeddedArrayBits(
            long absoluteByteOffset, long incoming, int byteCount) {
        Object array = embeddedPrimitiveArray();
        if (array == null) {
            return false;
        }
        int elementSize = embeddedArrayElementSize(array);
        int withinElement = (int) Math.floorMod(absoluteByteOffset, elementSize);
        if (withinElement + byteCount > elementSize || byteCount > 8) {
            return false;
        }
        prepareMemoryWrite(absoluteByteOffset, byteCount);
        int elementIndex = Math.toIntExact(Math.floorDiv(absoluteByteOffset, elementSize));
        int shift = withinElement * 8;
        long valueMask = atomicMask(byteCount);
        long mask = valueMask << shift;
        long currentBits = primitiveArrayBits(array, elementIndex);
        long updated = (currentBits & ~mask) | ((incoming & valueMask) << shift);
        storePrimitiveArrayBits(array, elementIndex, updated);
        return true;
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
            if (!allocation.getClass().isArray()) {
                return allocation;
            }
            return Array.getLength(allocation) != 0 ? arrayGet(allocation, 0) : null;
        }
        if (byteOffset % allocationElementSize != 0) {
            throw new IllegalStateException("object dereference is not aligned to its allocation element");
        }
        int elementIndex = Math.toIntExact(byteOffset / allocationElementSize);
        return readElement(elementIndex);
    }

    private long loadUnsigned(int byteCount) {
        return loadUnsignedAt(byteOffset, byteCount);
    }

    private long loadUnsignedAt(long absoluteByteOffset, int byteCount) {
        if (byteCount < 0 || byteCount > 8) {
            throw new IllegalArgumentException("scalar loads support at most eight bytes");
        }
        if (byteCount == 0) {
            return 0;
        }
        if (allocation instanceof byte[]) {
            byte[] bytes = (byte[]) allocation;
            int offset = Math.toIntExact(absoluteByteOffset);
            if (offset < 0 || offset > bytes.length - byteCount) {
                throw new IndexOutOfBoundsException(
                        "scalar load exceeds byte-addressable Rust storage");
            }
            flushMemoryViewsOverlapping(absoluteByteOffset, byteCount);
            long result = 0;
            for (int index = 0; index < byteCount; index++) {
                result |= ((long) bytes[offset + index] & 0xffL) << (index * 8);
            }
            return result;
        }
        return loadUnsignedAtSlow(absoluteByteOffset, byteCount);
    }

    private long loadUnsignedAtSlow(long absoluteByteOffset, int byteCount) {
        Long embedded = loadEmbeddedArrayBits(absoluteByteOffset, byteCount);
        if (embedded != null) {
            return embedded.longValue();
        }
        if (allocation != null && allocationElementSize > 0) {
            int withinElement = (int) Math.floorMod(absoluteByteOffset, allocationElementSize);
            if (withinElement + byteCount <= allocationElementSize) {
                int elementIndex = Math.toIntExact(
                        Math.floorDiv(absoluteByteOffset, allocationElementSize));
                flushMemoryViewsOverlapping(absoluteByteOffset, byteCount);
                Object value = readElement(elementIndex);
                if (value instanceof BigInteger
                        || value instanceof I128
                        || value instanceof U128
                        || value instanceof F128) {
                    long result = 0;
                    for (int index = 0; index < byteCount; index++) {
                        int byteIndex = withinElement + index;
                        int next;
                        if (value instanceof BigInteger) {
                            next = bigIntegerByte((BigInteger) value, byteIndex) & 0xff;
                        } else if (value instanceof I128) {
                            next = ((I128) value).byteAt(byteIndex) & 0xff;
                        } else if (value instanceof U128) {
                            next = ((U128) value).byteAt(byteIndex) & 0xff;
                        } else {
                            next = bigIntegerByte(((F128) value).toBits(), byteIndex) & 0xff;
                        }
                        result |= ((long) next) << (index * 8);
                    }
                    return result;
                }
                if (isPrimitiveScalarCarrier(value) && withinElement + byteCount <= 8) {
                    long bits = valueBits(value, allocationElementSize);
                    return (bits >>> (withinElement * 8)) & atomicMask(byteCount);
                }
                byte[] encoded = null;
                byte[] directUnionBytes = null;
                if (isFatPointerCodec(allocationCodecClassName)) {
                    encoded = encodeFatPointer(
                            value, allocationElementSize, allocationCodecClassName);
                } else if (isGeneratedAggregateCodec(allocationCodecClassName)) {
                    MemoryCodec plan = codecPlan(allocationCodecClassName);
                    if (plan.isArrayCodecFor(value)) {
                        return loadArrayCodecBits(
                                value, withinElement, byteCount, plan);
                    }
                    directUnionBytes = plan.directUnionBytes(value);
                    encoded = directUnionBytes == null
                            ? plan.encode().encode(value)
                            : directUnionBytes;
                }
                if (encoded != null) {
                    if (withinElement + byteCount > encoded.length) {
                        throw new IndexOutOfBoundsException(
                                "aggregate codec returned a short memory image");
                    }
                    long result = 0;
                    for (int index = 0; index < byteCount; index++) {
                        result |= ((long) encoded[withinElement + index] & 0xffL)
                                << (index * 8);
                    }
                    if (directUnionBytes == null) {
                        discardEncodedReferences(encoded);
                    }
                    return result;
                }
                if (withinElement + byteCount <= 8) {
                    long bits = valueBits(value, allocationElementSize);
                    return (bits >>> (withinElement * 8)) & atomicMask(byteCount);
                }
            }
        }
        long result = 0;
        for (int index = 0; index < byteCount; index++) {
            result |= ((long) loadByte(absoluteByteOffset + index)) << (index * 8);
        }
        return result;
    }

    private int loadByte(long absoluteByteOffset) {
        return loadByte(absoluteByteOffset, true);
    }

    private int loadByte(long absoluteByteOffset, boolean flushMemoryViews) {
        if (allocation == null) {
            throw new NullPointerException("attempted to dereference a null Rust pointer");
        }
        if (allocationElementSize == 0) {
            throw new IndexOutOfBoundsException(
                    "zero-sized storage has no addressable bytes: allocation="
                            + allocation.getClass().getName()
                            + ", byte_offset=" + byteOffset
                            + ", view_size=" + viewSize
                            + ", allocation_codec=" + allocationCodecClassName
                            + ", view_codec=" + viewCodecClassName
                            + ", recorded_source_size=" + zeroSizedSourceViewSize()
                            + ", recorded_source_codec=" + zeroSizedSourceViewCodecClassName());
        }
        Long embedded =
                loadEmbeddedArrayBits(absoluteByteOffset, 1, flushMemoryViews);
        if (embedded != null) {
            return embedded.intValue();
        }
        if (flushMemoryViews) {
            flushMemoryViewsOverlapping(absoluteByteOffset, 1);
        }
        int elementIndex = Math.toIntExact(Math.floorDiv(absoluteByteOffset, allocationElementSize));
        int withinElement = (int) Math.floorMod(absoluteByteOffset, allocationElementSize);
        Object value;
        value = readElement(elementIndex);
        int primitiveArrayElementSize =
                primitiveArrayElementSize(value, allocationElementSize);
        if (primitiveArrayElementSize != 0) {
            return loadPrimitiveArrayByte(value, withinElement, primitiveArrayElementSize);
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
        if (isPrimitiveScalarCarrier(value) && withinElement < 8) {
            long bits = valueBits(value, allocationElementSize);
            return (int) ((bits >>> (withinElement * 8)) & 0xffL);
        }
        if (isFatPointerCodec(allocationCodecClassName)) {
            byte[] bytes = encodeFatPointer(
                    value, allocationElementSize, allocationCodecClassName);
            int result = bytes[withinElement] & 0xff;
            discardEncodedReferences(bytes);
            return result;
        }
        if (isGeneratedAggregateCodec(allocationCodecClassName)) {
            MemoryCodec plan = codecPlan(allocationCodecClassName);
            if (plan.isArrayCodecFor(value)) {
                return (int) loadArrayCodecBits(value, withinElement, 1, plan);
            }
            byte[] direct = plan.directUnionBytes(value);
            byte[] bytes = direct == null ? plan.encode().encode(value) : direct;
            if (withinElement >= bytes.length) {
                if (direct == null) {
                    discardEncodedReferences(bytes);
                }
                throw new IndexOutOfBoundsException("aggregate codec returned a short memory image");
            }
            int result = bytes[withinElement] & 0xff;
            if (direct == null) {
                discardEncodedReferences(bytes);
            }
            return result;
        }
        try {
            long bits = valueBits(value, allocationElementSize);
            return (int) ((bits >>> (withinElement * 8)) & 0xffL);
        } catch (UnsupportedOperationException error) {
            throw new UnsupportedOperationException(
                    error.getMessage()
                            + " (allocation element size "
                            + allocationElementSize
                            + ", allocation codec "
                            + allocationCodecClassName
                            + ", view size "
                            + viewSize
                            + ", view codec "
                            + viewCodecClassName
                            + ")",
                    error);
        }
    }

    private byte[] loadRange(int byteCount) {
        if (byteCount < 0) {
            throw new IllegalArgumentException("byte count must not be negative");
        }
        byte[] result = new byte[byteCount];
        if (byteCount == 0) {
            return result;
        }
        if (allocation == null) {
            throw new NullPointerException("attempted to dereference a null Rust pointer");
        }
        if (allocationElementSize == 0) {
            throw new IndexOutOfBoundsException("zero-sized storage has no addressable bytes");
        }
        if (allocation instanceof byte[]) {
            byte[] bytes = (byte[]) allocation;
            int offset = Math.toIntExact(byteOffset);
            if (offset < 0 || offset > bytes.length - byteCount) {
                throw new IndexOutOfBoundsException(
                        "range load exceeds byte-addressable Rust storage");
            }
            flushMemoryViewsOverlapping(byteOffset, byteCount);
            System.arraycopy(bytes, offset, result, 0, byteCount);
            transferEncodedPointers(
                    allocation, byteOffset, result, 0, byteCount, false);
            transferEncodedReferences(allocation, result);
            return result;
        }

        flushMemoryViewsOverlapping(byteOffset, byteCount);
        int consumed = 0;
        while (consumed < byteCount) {
            long absoluteOffset = byteOffset + consumed;
            int elementIndex = Math.toIntExact(
                    Math.floorDiv(absoluteOffset, allocationElementSize));
            int withinElement = (int) Math.floorMod(absoluteOffset, allocationElementSize);
            int chunk = Math.min(byteCount - consumed, allocationElementSize - withinElement);
            Object value = readElement(elementIndex);
            byte[] image = null;
            boolean directNestedPrimitiveElement =
                    nestedPrimitiveArrayElementByteSize(allocation) > allocationElementSize;
            int directAggregateElementSize = isGeneratedAggregateCodec(allocationCodecClassName)
                            && allocation.getClass().isArray()
                            && allocation.getClass().getComponentType().isPrimitive()
                    ? inferredArrayElementSize(allocation)
                    : 0;
            boolean directPrimitiveArray = allocation.getClass().isArray()
                    && allocation.getClass().getComponentType().isPrimitive()
                    && allocationElementSize == inferredArrayElementSize(allocation);
            if (directNestedPrimitiveElement) {
                long bits = valueBits(value, allocationElementSize);
                for (int index = 0; index < chunk; index++) {
                    result[consumed + index] =
                            (byte) (bits >>> ((withinElement + index) * 8));
                }
            } else if (directPrimitiveArray) {
                int elementSize = inferredArrayElementSize(allocation);
                for (int index = 0; index < chunk; index++) {
                    result[consumed + index] = (byte) loadPrimitiveArrayByte(
                            allocation,
                            Math.toIntExact(absoluteOffset + index),
                            elementSize);
                }
            } else if (directAggregateElementSize > 0) {
                for (int index = 0; index < chunk; index++) {
                    result[consumed + index] = (byte) loadPrimitiveArrayByte(
                            allocation,
                            Math.toIntExact(absoluteOffset + index),
                            directAggregateElementSize);
                }
            } else if (!directPrimitiveArray && isFatPointerCodec(allocationCodecClassName)) {
                image = encodeFatPointer(value, allocationElementSize, allocationCodecClassName);
            } else if (!directPrimitiveArray
                    && isGeneratedAggregateCodec(allocationCodecClassName)) {
                MemoryCodec plan = codecPlan(allocationCodecClassName);
                if (plan.isArrayCodecFor(value)) {
                    loadArrayCodecRange(
                            value,
                            withinElement,
                            result,
                            consumed,
                            chunk,
                            plan);
                    consumed += chunk;
                    continue;
                }
                byte[] direct = plan.directUnionBytes(value);
                if (direct != null) {
                    if (withinElement + chunk > direct.length) {
                        throw new IndexOutOfBoundsException(
                                "aggregate storage contains a short memory image");
                    }
                    System.arraycopy(direct, withinElement, result, consumed, chunk);
                    transferEncodedPointers(
                            direct, withinElement, result, consumed, chunk, false);
                    transferEncodedReferences(direct, result);
                    consumed += chunk;
                    continue;
                }
                image = plan.encode().encode(value);
            }
            if (image != null) {
                if (withinElement + chunk > image.length) {
                    discardEncodedReferences(image);
                    throw new IndexOutOfBoundsException(
                            "aggregate codec returned a short memory image");
                }
                System.arraycopy(image, withinElement, result, consumed, chunk);
                transferEncodedPointers(
                        image, withinElement, result, consumed, chunk, false);
                transferEncodedReferences(image, result);
                discardEncodedReferences(image);
            } else if (directAggregateElementSize == 0
                    && !directNestedPrimitiveElement) {
                for (int index = 0; index < chunk; index++) {
                    result[consumed + index] =
                            (byte) loadByte(absoluteOffset + index, false);
                }
            }
            consumed += chunk;
        }
        transferEncodedPointers(
                allocation, byteOffset, result, 0, byteCount, false);
        transferEncodedReferences(allocation, result);
        return result;
    }

    private void storeBytes(long bits, int byteCount) {
        storeBytesAt(byteOffset, bits, byteCount);
    }

    private void storeBytesAt(long absoluteByteOffset, long bits, int byteCount) {
        if (byteCount < 0 || byteCount > 8) {
            throw new IllegalArgumentException("scalar stores support at most eight bytes");
        }
        if (byteCount == 0) {
            return;
        }
        if (allocation instanceof byte[]) {
            byte[] bytes = (byte[]) allocation;
            int offset = Math.toIntExact(absoluteByteOffset);
            if (offset < 0 || offset > bytes.length - byteCount) {
                throw new IndexOutOfBoundsException(
                        "scalar store exceeds byte-addressable Rust storage");
            }
            prepareMemoryWrite(absoluteByteOffset, byteCount);
            discardEncodedPointers(allocation, absoluteByteOffset, byteCount);
            for (int index = 0; index < byteCount; index++) {
                bytes[offset + index] = (byte) (bits >>> (index * 8));
            }
            return;
        }
        if (storeEmbeddedArrayBits(absoluteByteOffset, bits, byteCount)) {
            return;
        }
        if (allocation != null && allocationElementSize > 0) {
            int withinElement =
                    (int) Math.floorMod(absoluteByteOffset, allocationElementSize);
            if (withinElement + byteCount <= allocationElementSize) {
                int elementIndex = Math.toIntExact(
                        Math.floorDiv(absoluteByteOffset, allocationElementSize));
                prepareMemoryWrite(absoluteByteOffset, byteCount);
                Object current = readElement(elementIndex);
                if (isPrimitiveScalarCarrier(current)
                        && withinElement + byteCount <= 8) {
                    int shift = withinElement * 8;
                    long valueMask = atomicMask(byteCount);
                    long mask = valueMask << shift;
                    long currentBits = valueBits(current, allocationElementSize);
                    long updated = (currentBits & ~mask) | ((bits & valueMask) << shift);
                    writeElementPreservingIdentity(
                            elementIndex,
                            carrierFromBits(current, updated, allocationElementSize));
                    return;
                }
                byte[] encoded = null;
                boolean fatPointer = isFatPointerCodec(allocationCodecClassName);
                if (fatPointer) {
                    encoded = encodeFatPointer(
                            current, allocationElementSize, allocationCodecClassName);
                } else if (isGeneratedAggregateCodec(allocationCodecClassName)) {
                    MemoryCodec plan = codecPlan(allocationCodecClassName);
                    if (plan.isArrayCodecFor(current)) {
                        discardEncodedPointers(
                                allocation, absoluteByteOffset, byteCount);
                        storeArrayCodecBits(
                                current, withinElement, bits, byteCount, plan);
                        return;
                    }
                    byte[] direct = plan.directUnionBytes(current);
                    if (direct != null && direct.length == 1
                            && withinElement == 0 && byteCount == 1) {
                        discardEncodedPointers(direct, 0, 1);
                        discardEncodedPointers(allocation, absoluteByteOffset, 1);
                        direct[0] = (byte) bits;
                        return;
                    }
                    encoded = plan.encode().encode(current);
                }
                if (encoded != null) {
                    if (withinElement + byteCount > encoded.length) {
                        throw new IndexOutOfBoundsException(
                                "aggregate codec returned a short memory image");
                    }
                    for (int index = 0; index < byteCount; index++) {
                        encoded[withinElement + index] =
                                (byte) (bits >>> (index * 8));
                    }
                    Object updated = fatPointer
                            ? decodeFatPointer(
                                    encoded,
                                    0,
                                    allocationElementSize,
                                    allocationCodecClassName)
                            : decodeAggregate(allocationCodecClassName, encoded);
                    discardEncodedReferences(encoded);
                    writeElementPreservingIdentity(elementIndex, updated);
                    return;
                }
            }
        }
        for (int index = 0; index < byteCount; index++) {
            storeByte(
                    absoluteByteOffset + index,
                    (int) ((bits >>> (index * 8)) & 0xffL));
        }
    }

    private void storeByte(long absoluteByteOffset, int value) {
        if (allocation == null) {
            throw new NullPointerException("attempted to write through a null Rust pointer");
        }
        if (allocationElementSize == 0) {
            throw new IndexOutOfBoundsException("zero-sized storage has no addressable bytes");
        }
        if (storeEmbeddedArrayBits(absoluteByteOffset, value & 0xffL, 1)) {
            return;
        }
        flushMemoryViewsOverlapping(absoluteByteOffset, 1);
        int elementIndex = Math.toIntExact(Math.floorDiv(absoluteByteOffset, allocationElementSize));
        int withinElement = (int) Math.floorMod(absoluteByteOffset, allocationElementSize);
        Object current;
        try {
            current = readElement(elementIndex);
        } catch (ArrayIndexOutOfBoundsException error) {
            throw new ArrayIndexOutOfBoundsException(
                    error.getMessage() + ": absolute_byte_offset=" + absoluteByteOffset
                            + ", pointer_byte_offset=" + byteOffset
                            + ", allocation_element_size=" + allocationElementSize
                            + ", view_size=" + viewSize
                            + ", allocation_codec=" + allocationCodecClassName
                            + ", view_codec=" + viewCodecClassName);
        }
        int primitiveArrayElementSize =
                primitiveArrayElementSize(current, allocationElementSize);
        if (primitiveArrayElementSize != 0) {
            prepareMemoryWrite(absoluteByteOffset, 1);
            storePrimitiveArrayByte(
                    current, withinElement, primitiveArrayElementSize, value);
            return;
        }
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
        if (isPrimitiveScalarCarrier(current) && withinElement < 8) {
            long bits = valueBits(current, allocationElementSize);
            long mask = 0xffL << (withinElement * 8);
            bits = (bits & ~mask) | (((long) value & 0xffL) << (withinElement * 8));
            writeElementPreservingIdentity(
                    elementIndex, carrierFromBits(current, bits, allocationElementSize));
            return;
        }
        if (isFatPointerCodec(allocationCodecClassName)) {
            byte[] bytes = encodeFatPointer(
                    current, allocationElementSize, allocationCodecClassName);
            bytes[withinElement] = (byte) value;
            Object updated = decodeFatPointer(
                    bytes, 0, allocationElementSize, allocationCodecClassName);
            discardEncodedReferences(bytes);
            writeElementPreservingIdentity(
                    elementIndex,
                    updated);
            return;
        }
        if (isGeneratedAggregateCodec(allocationCodecClassName)) {
            MemoryCodec plan = codecPlan(allocationCodecClassName);
            if (plan.isArrayCodecFor(current)) {
                prepareMemoryWrite(absoluteByteOffset, 1);
                discardEncodedPointers(allocation, absoluteByteOffset, 1);
                storeArrayCodecBits(current, withinElement, value & 0xffL, 1, plan);
                return;
            }
            byte[] direct = plan.directUnionBytes(current);
            if (direct != null && direct.length == 1 && withinElement == 0) {
                prepareMemoryWrite(absoluteByteOffset, 1);
                discardEncodedPointers(direct, 0, 1);
                discardEncodedPointers(allocation, absoluteByteOffset, 1);
                direct[0] = (byte) value;
                return;
            }
            byte[] bytes = plan.encode().encode(current);
            if (withinElement >= bytes.length) {
                throw new IndexOutOfBoundsException("aggregate codec returned a short memory image");
            }
            bytes[withinElement] = (byte) value;
            Object updated = decodeAggregate(allocationCodecClassName, bytes);
            discardEncodedReferences(bytes);
            writeElementPreservingIdentity(
                    elementIndex, updated);
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

    /** Lock the allocation during atomic access and aggregate decoding or publication.
     * A decoded view can overlap several atomic fields, so offset locks are insufficient. */
    private static Object atomicStripe(Pointer pointer) {
        return ATOMIC_STRIPES[atomicStripeIndex(pointer)];
    }

    /** Stable address key used by the JVM futex wait queues. */
    static long atomicAddress(Pointer pointer) {
        if (pointer == null) {
            throw new NullPointerException("a futex address cannot be null");
        }
        return pointer.numericAddress();
    }

    private static int atomicStripeIndex(Pointer pointer) {
        return atomicStripeIndex(pointer.allocation, pointer.exposedAddress);
    }

    private static Object atomicStripe(Object root, long offset) {
        return root instanceof Pointer
                ? ATOMIC_STRIPES[atomicStripeIndex(((Pointer) root).allocation, ((Pointer) root).exposedAddress + offset)]
                : ATOMIC_STRIPES[atomicStripeIndex(root, offset)];
    }

    private static int atomicStripeIndex(Object identity, long exposedAddress) {
        if (identity instanceof ReceiverCell) {
            identity = ((ReceiverCell) identity).value;
        } else if (identity instanceof FieldCell) {
            FieldCell cell = (FieldCell) identity;
            identity = cell.owner();
        }
        long key = identity == null
                ? exposedAddress
                : System.identityHashCode(identity);
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

    public static long atomicLoad(Pointer pointer, int byteCount, int ordering) {
        return atomicLoad(pointer, 0L, byteCount, ordering);
    }

    public static void atomicStore(Pointer pointer, long value, int byteCount, int ordering) {
        atomicStore(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicExchange(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicExchange(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicAdd(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicAdd(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicSubtract(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicSubtract(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicAnd(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicAnd(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicNand(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicNand(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicOr(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicOr(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicXor(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicXor(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicMax(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicMax(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicMin(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicMin(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicUnsignedMax(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicUnsignedMax(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicUnsignedMin(Pointer pointer, long value, int byteCount, int ordering) {
        return atomicUnsignedMin(pointer, 0L, value, byteCount, ordering);
    }

    public static long atomicCompareExchange(Pointer pointer, long expected,
            long value,
            int byteCount,
            int successOrdering,
            int failureOrdering) {
        return atomicCompareExchange(pointer, 0L, expected, value, byteCount, successOrdering, failureOrdering);
    }

    private static void atomicStoreLocked(Object root, long offset, long value, int byteCount) {
        storeLocationBits(root, offset, truncateAtomic(value, byteCount), byteCount);
    }

    private static long atomicLoadStriped(Object root, long offset, int byteCount) {
        synchronized (atomicStripe(root, offset)) {
            return truncateAtomic(loadLocationBits(root, offset, byteCount), byteCount);
        }
    }

    public static long atomicLoad(Object root, long offset, int byteCount, int ordering) {
        root = normalizeLocationOrigin(root);
        checkedAtomicByteCount(byteCount);
        if (isSequentiallyConsistent(ordering)) {
            synchronized (ATOMIC_SEQUENCE_LOCK) {
                return atomicLoadStriped(root, offset, byteCount);
            }
        }
        return atomicLoadStriped(root, offset, byteCount);
    }

    private static void atomicStoreStriped(Object root, long offset, long value, int byteCount) {
        synchronized (atomicStripe(root, offset)) {
            atomicStoreLocked(root, offset, value, byteCount);
        }
    }

    public static void atomicStore(Object root, long offset, long value, int byteCount, int ordering) {
        root = normalizeLocationOrigin(root);
        checkedAtomicByteCount(byteCount);
        if (isSequentiallyConsistent(ordering)) {
            synchronized (ATOMIC_SEQUENCE_LOCK) {
                atomicStoreStriped(root, offset, value, byteCount);
            }
            return;
        }
        atomicStoreStriped(root, offset, value, byteCount);
    }

    private static long atomicRmwStriped(
            Object root, long offset, long operand, int byteCount, int operation) {
        synchronized (atomicStripe(root, offset)) {
            long oldValue = truncateAtomic(loadLocationBits(root, offset, byteCount), byteCount);
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
            atomicStoreLocked(root, offset, newValue, byteCount);
            return oldValue;
        }
    }

    private static long atomicRmw(
            Object root, long offset, long operand, int byteCount, int operation, int ordering) {
        root = normalizeLocationOrigin(root);
        checkedAtomicByteCount(byteCount);
        if (isSequentiallyConsistent(ordering)) {
            synchronized (ATOMIC_SEQUENCE_LOCK) {
                return atomicRmwStriped(root, offset, operand, byteCount, operation);
            }
        }
        return atomicRmwStriped(root, offset, operand, byteCount, operation);
    }

    public static long atomicExchange(
            Object root, long offset, long value, int byteCount, int ordering) {
        return atomicRmw(root, offset, value, byteCount, 0, ordering);
    }

    public static long atomicAdd(Object root, long offset, long value, int byteCount, int ordering) {
        return atomicRmw(root, offset, value, byteCount, 1, ordering);
    }

    public static long atomicSubtract(
            Object root, long offset, long value, int byteCount, int ordering) {
        return atomicRmw(root, offset, value, byteCount, 2, ordering);
    }

    public static long atomicAnd(Object root, long offset, long value, int byteCount, int ordering) {
        return atomicRmw(root, offset, value, byteCount, 3, ordering);
    }

    public static long atomicNand(Object root, long offset, long value, int byteCount, int ordering) {
        return atomicRmw(root, offset, value, byteCount, 4, ordering);
    }

    public static long atomicOr(Object root, long offset, long value, int byteCount, int ordering) {
        return atomicRmw(root, offset, value, byteCount, 5, ordering);
    }

    public static long atomicXor(Object root, long offset, long value, int byteCount, int ordering) {
        return atomicRmw(root, offset, value, byteCount, 6, ordering);
    }

    public static long atomicMax(Object root, long offset, long value, int byteCount, int ordering) {
        return atomicRmw(root, offset, value, byteCount, 7, ordering);
    }

    public static long atomicMin(Object root, long offset, long value, int byteCount, int ordering) {
        return atomicRmw(root, offset, value, byteCount, 8, ordering);
    }

    public static long atomicUnsignedMax(
            Object root, long offset, long value, int byteCount, int ordering) {
        return atomicRmw(root, offset, value, byteCount, 9, ordering);
    }

    public static long atomicUnsignedMin(
            Object root, long offset, long value, int byteCount, int ordering) {
        return atomicRmw(root, offset, value, byteCount, 10, ordering);
    }

    private static long atomicCompareExchangeStriped(
            Object root, long offset, long expected, long value, int byteCount) {
        synchronized (atomicStripe(root, offset)) {
            long oldValue = truncateAtomic(loadLocationBits(root, offset, byteCount), byteCount);
            if (oldValue == truncateAtomic(expected, byteCount)) {
                atomicStoreLocked(root, offset, value, byteCount);
            }
            return oldValue;
        }
    }

    public static long atomicCompareExchange(
            Object root, long offset,
            long expected,
            long value,
            int byteCount,
            int successOrdering,
            int failureOrdering) {
        root = normalizeLocationOrigin(root);
        checkedAtomicByteCount(byteCount);
        boolean sequentiallyConsistent = isSequentiallyConsistent(successOrdering)
                | isSequentiallyConsistent(failureOrdering);
        if (sequentiallyConsistent) {
            synchronized (ATOMIC_SEQUENCE_LOCK) {
                return atomicCompareExchangeStriped(root, offset, expected, value, byteCount);
            }
        }
        return atomicCompareExchangeStriped(root, offset, expected, value, byteCount);
    }

    public static void atomicFence(int ordering) {
        int checkedOrdering = checkedAtomicOrdering(ordering);
        if (checkedOrdering == ATOMIC_SEQ_CST) {
            synchronized (ATOMIC_SEQUENCE_LOCK) {
                ATOMIC_FENCE_EPOCH.incrementAndGet();
            }
        } else if (checkedOrdering == ATOMIC_ACQUIRE) {
            ATOMIC_FENCE_EPOCH.get();
        } else if (checkedOrdering != ATOMIC_RELAXED) {
            ATOMIC_FENCE_EPOCH.incrementAndGet();
        }
    }

    /** Thin-pointer comparison on storage components, preserving exposed addresses. */
    public static boolean sameLocation(Object left, long leftOffset, Object right, long rightOffset) {
        if (left == right) return leftOffset == rightOffset;
        if (left instanceof Pointer && right instanceof Pointer) {
            Pointer a = (Pointer) left;
            Pointer b = (Pointer) right;
            if (a.allocation != null && a.allocation == b.allocation) {
                return a.byteOffset + leftOffset == b.byteOffset + rightOffset;
            }
        }
        return locationAddress(left) + leftOffset == locationAddress(right) + rightOffset;
    }

    private static long locationAddress(Object root) {
        return locationAddr(root, 0);
    }

    /** Read an address word without materializing or exposing a Rust pointer. */
    public static long locationAddr(Object root, long offset) {
        if (root == null) return offset;
        if (root instanceof Pointer) return ((Pointer) root).numericAddress() + offset;
        Object normalized = normalizeLocationOrigin(root);
        if (normalized instanceof Pointer) return ((Pointer) normalized).numericAddress() + offset;
        Long cached = cachedAllocationBase(ALLOCATION_BASE_CACHE, root);
        if (cached != null) return cached.longValue() + offset;
        int size = root instanceof Storage ? ((Storage) root).size : inferredArrayElementSize(root);
        synchronized (ALLOCATIONS) {
            return allocationBase(root, size, allocationInfo(root)) + offset;
        }
    }

    private static Object normalizeLocationOrigin(Object root) {
        if (root == null || root instanceof Pointer || root instanceof Storage
                || !mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, root)) return root;
        Map<Object, MemoryViewOrigin> stripe = stateStripe(MEMORY_VIEW_ORIGINS, root);
        synchronized (stripe) {
            MemoryViewOrigin origin = stripe.get(root);
            // Filter matches can be stale or false. Only a live origin needs normalization.
            if (origin == null || origin.allocation.get() == null) return root;
        }
        return array(root, 0, inferredArrayElementSize(root));
    }

    /** Pointer differences use allocation identity, never exposed address lookup. */
    public static long byteOffsetLocations(Object root, long offset, Object origin, long originOffset) {
        root = normalizeLocationOrigin(root);
        origin = normalizeLocationOrigin(origin);
        Object allocation = root instanceof Pointer ? ((Pointer) root).provenanceAllocation() : root;
        Object originAllocation = origin instanceof Pointer
                ? ((Pointer) origin).provenanceAllocation() : origin;
        if (allocation != originAllocation) {
            throw new IllegalArgumentException("byte_offset_from requires pointers into one allocation");
        }
        long start = root instanceof Pointer
                ? Math.addExact(((Pointer) root).provenanceByteOffset(), offset) : offset;
        long end = origin instanceof Pointer
                ? Math.addExact(((Pointer) origin).provenanceByteOffset(), originOffset) : originOffset;
        return Math.subtractExact(start, end);
    }

    public static long offsetLocations(Object root, long offset, Object origin, long originOffset, long stride) {
        if (stride == -1) stride = locationStride(root);
        if (stride == 0) throw new ArithmeticException("offset_from is undefined for zero-sized pointees");
        long bytes = byteOffsetLocations(root, offset, origin, originOffset);
        if (bytes % stride != 0) {
            throw new ArithmeticException("pointer distance is not a whole number of elements");
        }
        return bytes / stride;
    }

    private static long unsignedLocationDistance(long distance) {
        if (distance < 0) {
            throw new ArithmeticException("offset_from_unsigned requires self at or after origin");
        }
        return distance;
    }

    public static long byteOffsetLocationsUnsigned(Object root, long offset, Object origin, long originOffset) {
        return unsignedLocationDistance(byteOffsetLocations(root, offset, origin, originOffset));
    }

    public static long offsetLocationsUnsigned(Object root, long offset, Object origin, long originOffset, long stride) {
        return unsignedLocationDistance(offsetLocations(root, offset, origin, originOffset, stride));
    }

    public static long alignLocation(Object root, long offset, long stride, long alignment) {
        return alignmentOffset(locationAddr(root, offset), stride == -1 ? locationStride(root) : stride, alignment);
    }

    /** Compare decomposed data addresses without creating temporary carriers. */
    public static int compareLocations(Object left, long leftOffset, Object right, long rightOffset) {
        if (left == right && left != null && leftOffset >= 0 && rightOffset >= 0
                && (left instanceof Storage || (left.getClass().isArray()
                    && !mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, left)))) {
            return Long.compareUnsigned(leftOffset, rightOffset);
        }
        if (left instanceof Pointer && right instanceof Pointer) {
            Pointer a = (Pointer) left, b = (Pointer) right;
            long x = a.byteOffset + leftOffset, y = b.byteOffset + rightOffset;
            if (a.allocation != null && a.allocation == b.allocation
                    && a.allocationElementSize > 0 && b.allocationElementSize > 0
                    && a.addressOrigin() == null && b.addressOrigin() == null && x >= 0 && y >= 0) {
                return Long.compareUnsigned(x, y);
            }
        }
        // Other offsets and allocations require the full unsigned-address and provenance checks.
        return Long.compareUnsigned(locationAddress(left) + leftOffset, locationAddress(right) + rightOffset);
    }

    /** Element layout retained by a general storage root. */
    public static long locationStride(Object root) {
        return root instanceof Storage ? ((Storage) root).size : ((Pointer) root).viewSize;
    }

    /** The root carries the complete layout and provenance of a non-scalar view. */
    public static Pointer fromStorageLocation(Object root, long offset) {
        if (root instanceof Storage) root = ((Storage) root).boundary();
        // Retain the pointer that binds a decoded view. A later commit must use the same owner.
        if (offset == 0) return (Pointer) root;
        return ((Pointer) root).byte_offset(offset);
    }

    /** The physical ABI records a reconstruction plan, never an inferred JVM layout. */
    public static Pointer addressFromParts(Object root, long offset, int plan) {
        if (plan == 64 || plan == 128) return fromBorrowedStorageLocation(root, offset, plan == 64);
        return plan == 0 ? fromStorageLocation(root, offset) : fromLocation(root, offset, plan);
    }

    private static BorrowedFieldPath borrowedPath(Storage storage, long offset, boolean view) {
        StorageLayout layout = storage.layout(((Cell) storage).value);
        return layout == null ? null : layout.borrowedAt(offset, view);
    }

    private static Pointer fromBorrowedStorageLocation(Object root, long offset, boolean view) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            BorrowedFieldPath path = borrowedPath(storage, offset, view);
            if (path != null) {
                // Delay path materialization until a boundary needs it. Preserve replaceable parents after escape.
                Pointer base = storage.boundary();
                Object owner = storage;
                Pointer result = null;
                for (int i = 0; i < path.fields.length; i++) {
                    RustField field = path.fields[i];
                    boolean last = i == path.fields.length - 1;
                    result = rootField(owner, field.getDeclaringClass(), field.getName(),
                            last ? path.size : 0, last ? path.codec : null);
                    owner = result.allocation;
                }
                return result.inheritAddressOrigin(base, offset);
            }
        }
        return fromStorageLocation(root, offset);
    }

    /** The address of a stored borrow keeps its enclosing allocation root. */
    public static Object storageBorrowedFieldRoot(Object root, long offset, String owner,
            String field, long fieldOffset, long size, String codec) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            if (directStorage(storage) && isGeneratedAggregateCodec(storage.codec)) {
                long absolute = Math.addExact(offset, fieldOffset);
                StorageLayout layout = storage.layout(((Cell) storage).value);
                if (layout != null) {
                    BorrowedFieldPath path = layout.borrowedAt(absolute, true);
                    if (path == null) path = layout.borrowedAt(absolute, false);
                    if (path != null && path.size == size && path.field().getName().equals(field)
                            && matchesBinaryClassName(owner, path.field().getDeclaringClass().getName())
                            && java.util.Objects.equals(codec, path.codec)) return storage;
                }
            }
        }
        return fromStorageLocation(root, offset).projectStructField(owner, field, fieldOffset, size, codec);
    }

    /** Register the containing allocation when an escaping array borrow exposes its JVM array. */
    public static Object loadStorageArray(Object root, long offset, String target) {
        return fromStorageLocation(root, offset).getObject();
    }

    public static Object loadStorageLocation(Object root, long offset, String target) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            if (offset == 0 && directStorage(storage)) return ((Cell) storage).value;
            root = storage.boundary();
        }
        Pointer base = (Pointer) root;
        if (base.rareState == null && base.addressState == null) {
            return loadObjectLocation(base, offset, target);
        }
        Pointer view = fromStorageLocation(root, offset);
        return target == null ? view.getObject() : view.getObjectAs(target);
    }

    /** A stored borrow returns its components in the ordinary caller-owned ABI. */
    public static Object loadBorrowedView(Object root, long offset, long[] metadata) {
        if (root instanceof BorrowedStorage && offset == 0 && unescapedStorage((Storage) root)
                && ((BorrowedStorage) root).view) {
            return ((BorrowedStorage) root).read(metadata, true);
        }
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            BorrowedFieldPath path = borrowedPath(storage, offset, true);
            if (path != null && directStorage(storage)) return path.read(((Cell) storage).value, metadata);
            root = fromBorrowedStorageLocation(root, offset, true);
            offset = 0;
        }
        FieldCell field = directBorrowedField(root, offset, true);
        if (field != null) return readBorrowedField(field, metadata);
        SliceView view = (SliceView) loadStorageLocation(root, offset, SLICE_VIEW_CLASS_NAME);
        metadata[0] = RustField.viewStart(view);
        metadata[1] = RustField.viewLength(view);
        return RustField.viewRoot(view);
    }

    public static Object loadBorrowedAddress(Object root, long offset, long[] metadata) {
        if (root instanceof BorrowedStorage && offset == 0 && unescapedStorage((Storage) root)
                && !((BorrowedStorage) root).view) {
            return ((BorrowedStorage) root).read(metadata, false);
        }
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            BorrowedFieldPath path = borrowedPath(storage, offset, false);
            if (path != null && directStorage(storage)) return path.read(((Cell) storage).value, metadata);
            root = fromBorrowedStorageLocation(root, offset, false);
            offset = 0;
        }
        FieldCell field = directBorrowedField(root, offset, false);
        if (field != null) return readBorrowedField(field, metadata);
        Object value = loadStorageLocation(root, offset, null);
        metadata[0] = 0;
        return value;
    }

    public static void storeBorrowedView(Object root, long offset, Object backing, int start, long length) {
        storeBorrowedView(root, offset, backing, start, length, false);
    }

    public static void storeBorrowedUtf8(Object root, long offset, Object backing, int start, long length) {
        storeBorrowedView(root, offset, backing, start, length, true);
    }

    private static void storeBorrowedView(Object root, long offset, Object backing, int start, long length, boolean utf8) {
        if (root instanceof BorrowedStorage && offset == 0 && unescapedStorage((Storage) root)
                && ((BorrowedStorage) root).view) {
            ((BorrowedStorage) root).store(backing, start, length, utf8 ? -2 : -1);
            return;
        }
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            BorrowedFieldPath path = borrowedPath(storage, offset, true);
            if (path != null && directStorage(storage)) {
                path.write(((Cell) storage).value, backing, start, length);
                return;
            }
            root = fromBorrowedStorageLocation(root, offset, true);
            offset = 0;
        }
        FieldCell field = directBorrowedField(root, offset, true);
        if (field != null) {
            writeBorrowedField(field, backing, start, length);
            return;
        }
        storeStorageLocation(root, offset, utf8 ? new Utf8View(backing, start, length)
                : new SliceView(backing, start, length));
    }

    public static void storeBorrowedAddress(Object root, long offset, Object backing, long displacement, int size) {
        if (root instanceof BorrowedStorage && offset == 0 && unescapedStorage((Storage) root)
                && !((BorrowedStorage) root).view && !(backing instanceof Storage)) {
            ((BorrowedStorage) root).store(backing, displacement, 0, size);
            return;
        }
        // Resolve referenced boundaries before assignment. Lazy materialization under owner locks could recurse through cycles.
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            BorrowedFieldPath path = borrowedPath(storage, offset, false);
            if (path != null && directStorage(storage)) {
                path.write(((Cell) storage).value, backing, displacement, 0);
                return;
            }
            root = fromBorrowedStorageLocation(root, offset, false);
            offset = 0;
        }
        FieldCell field = directBorrowedField(root, offset, false);
        if (field != null) {
            writeBorrowedField(field, backing, displacement, 0);
            return;
        }
        Pointer value = backing == null && displacement == 0 ? null
                : addressFromParts(backing, displacement, size);
        storeStorageLocation(root, offset, value);
    }

    private static void writeBorrowedField(FieldCell cell, Object root, long offset, long length) {
        try { cell.access.field.setBorrowedParts(cell.owner(), root, offset, length); }
        catch (IllegalAccessException error) {
            throw new IllegalStateException("could not write borrowed Rust field", error);
        }
        discardProjectedFieldViews(cell);
    }

    private static Object readBorrowedField(FieldCell cell, long[] metadata) {
        try { return cell.access.field.borrowedParts(cell.owner(), metadata); }
        catch (IllegalAccessException error) {
            throw new IllegalStateException("could not read borrowed Rust field", error);
        }
    }

    private static FieldCell directBorrowedField(Object root, long offset, boolean view) {
        if (!(root instanceof Pointer)) return null;
        Pointer pointer = (Pointer) root;
        if (offset != 0 || pointer.byteOffset != 0
                || pointer.viewSize != pointer.allocationElementSize
                || !pointer.isDirectAllocationView()
                || pointer.rareState != null || !(pointer.allocation instanceof FieldCell)) return null;
        FieldCell field = (FieldCell) pointer.allocation;
        if (!field.access.field.borrowedShape(view)) return null;
        Object owner = field;
        for (int depth = 0; depth < 16; depth++) {
            if (owner instanceof FieldCell) {
                FieldCell current = (FieldCell) owner;
                if (current.hasMemoryView || current.hasStructuralView || current.hasProjectedViews
                        || current.hasMemoryOrigins) return null;
                owner = current.rootOwner == null ? current.fixedOwner : current.rootOwner;
            } else if (owner instanceof Cell) {
                Cell current = (Cell) owner;
                if (current.hasMemoryView || current.hasStructuralView || current.hasProjectedViews) return null;
                owner = current.value;
            } else {
                return mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, owner) ? null : field;
            }
        }
        return null;
    }

    /** Copying a value never establishes a mutable decoded-view binding. */
    public static Object loadStorageCopy(Object root, long offset, String target) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            if (offset == 0 && directStorage(storage)) {
                return copyManagedValue(((Cell) storage).value);
            }
            root = storage.boundary();
        }
        Pointer base = (Pointer) root;
        if (base.allocation instanceof byte[] && base.rareState == null
                && base.addressState == null && base.viewSize > 0
                && isGeneratedAggregateCodec(base.viewCodecClassName)) {
            MemoryCodec plan = codecPlan(base.viewCodecClassName);
            if (plan.decodeAt() != null && target != null
                    && matchesBinaryClassName(target, plan.encodeParameterType.getName())) {
                byte[] bytes = (byte[]) base.allocation;
                long absolute = Math.addExact(base.byteOffset, offset);
                int start = Math.toIntExact(absolute);
                int size = base.materializedViewSize();
                if (start < 0 || start > bytes.length - size) {
                    throw new IndexOutOfBoundsException("aggregate read exceeds byte-addressable Rust storage");
                }
                base.flushMemoryViewsOverlapping(absolute, size);
                return plan.decodeAt().decode(bytes, start);
            }
        }
        return fromStorageLocation(base, offset).getObjectCopyAs(target);
    }

    /** Owned field read without parent and projected address wrappers on byte storage. */
    public static Object loadStorageFieldCopy(Object root, long offset, String owner,
            String field, long fieldOffset, long size, String codec, String target) {
        if (size > 0 && size <= Integer.MAX_VALUE
                && (root instanceof byte[] || (root instanceof Pointer
                    && ((Pointer) root).allocation instanceof byte[]
                    && ((Pointer) root).traitMetadataCarrier() == null))) {
            return loadTypedStorageCopy(root, Math.addExact(offset, fieldOffset),
                    (int) size, codec, target);
        }
        return fromStorageLocation(root, offset)
                .projectStructField(owner, field, fieldOffset, size, codec).getObjectCopyAs(target);
    }

    /** Store an owned field through its exact layout without two projected wrappers. */
    public static void storeStorageField(Object root, long offset, String owner, String field,
            long fieldOffset, long size, String codec, Object value) {
        if (size > 0 && size <= Integer.MAX_VALUE
                && (root instanceof byte[]
                    || (root instanceof Pointer && ((Pointer) root).allocation instanceof byte[]
                        && ((Pointer) root).traitMetadataCarrier() == null))) {
            storeTypedStorage(root, Math.addExact(offset, fieldOffset), (int) size, codec, value);
            return;
        }
        fromStorageLocation(root, offset).projectStructField(owner, field, fieldOffset, size, codec).set(value);
    }

    /** Read a borrowed value without a temporary Pointer. Keep binding and commit behavior for decoded views. */
    public static Object loadTypedStorage(Object root, long offset, int size, String codec, String target) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            if (offset == 0 && size == storage.size
                    && java.util.Objects.equals(codec, storage.codec) && directStorage(storage)) {
                Object value = ((Cell) storage).value;
                if (target != null && !target.isEmpty() || value == null || !value.getClass().isArray()) return value;
            }
        }
        if (root instanceof Pointer) {
            Pointer pointer = (Pointer) root;
            if (size > 0 && pointer.viewSize == size
                    && java.util.Objects.equals(codec, pointer.viewCodecClassName)
                    && pointer.rareState == null && pointer.addressState == null
                    && pointer.isDirectAllocationView() && !mayHaveStructuralView(pointer.allocation)) {
                Object value = loadObjectLocation(pointer, offset, target);
                if (target != null && !target.isEmpty() || value == null || !value.getClass().isArray()) return value;
            }
        }
        return fromTypedStorageLocation(root, offset, size, codec).getObjectAs(target);
    }

    /** The copy's layout is compiler metadata, independent of the backing view. */
    public static Object loadTypedStorageCopy(Object root, long offset, int size,
            String codec, String target) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            if (offset == 0 && size == storage.size
                    && java.util.Objects.equals(codec, storage.codec) && directStorage(storage)) {
                return copyManagedValue(((Cell) storage).value);
            }
            root = storage.boundary();
        }
        if (root instanceof byte[] && size > 0 && isGeneratedAggregateCodec(codec)
                && !mayBeInIdentityFilter(MEMORY_VIEW_FILTER, root)
                && !mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, root)) {
            MemoryCodec plan = codecPlan(codec);
            if (plan.decodeAt() != null && target != null
                    && matchesBinaryClassName(target, plan.encodeParameterType.getName())) {
                byte[] bytes = (byte[]) root;
                int start = Math.toIntExact(offset);
                if (start < 0 || start > bytes.length - size) {
                    throw new IndexOutOfBoundsException("aggregate read exceeds byte-addressable Rust storage");
                }
                return plan.decodeAt().decode(bytes, start);
            }
        }
        Pointer base = root instanceof Pointer ? (Pointer) root : fromLocation(root, 0, 1);
        if (base.allocation instanceof byte[] && base.rareState == null
                && base.addressState == null && size > 0 && isGeneratedAggregateCodec(codec)) {
            MemoryCodec plan = codecPlan(codec);
            if (plan.decodeAt() != null && target != null
                    && matchesBinaryClassName(target, plan.encodeParameterType.getName())) {
                byte[] bytes = (byte[]) base.allocation;
                long absolute = Math.addExact(base.byteOffset, offset);
                int start = Math.toIntExact(absolute);
                if (start < 0 || start > bytes.length - size) {
                    throw new IndexOutOfBoundsException("aggregate read exceeds byte-addressable Rust storage");
                }
                base.flushMemoryViewsOverlapping(absolute, size);
                return plan.decodeAt().decode(bytes, start);
            }
        }
        Pointer escaped = base.escapedFieldStorage(offset);
        return (escaped == null ? base.byteOffsetRetype(offset, size, codec)
                : escaped.retype(size, codec)).getObjectCopyAs(target);
    }

    /** Write with compiler-owned layout metadata, materializing only at a general boundary. */
    public static void storeTypedStorage(Object root, long offset, int size, String codec, Object value) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            if (offset == 0 && size == storage.size
                    && java.util.Objects.equals(codec, storage.codec) && directStorage(storage)) {
                Cell cell = storage;
                cell.value = convertDirectValue(cell.value, value, size);
                return;
            }
        }
        if (root instanceof byte[] && size > 0 && isGeneratedAggregateCodec(codec)
                && !mayBeInIdentityFilter(MEMORY_VIEW_FILTER, root)
                && !mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, root)
                && !mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, value)) {
            MemoryCodec plan = codecPlan(codec);
            CodecCalls.RangeEncoder encode = plan.encodeAt();
            if (encode != null) {
                byte[] bytes = (byte[]) root;
                int start = Math.toIntExact(offset);
                if (start < 0 || start > bytes.length - size) {
                    throw new IndexOutOfBoundsException("aggregate store exceeds byte-addressable Rust storage");
                }
                discardEncodedPointers(bytes, start, size);
                encode.encode(value, bytes, start);
                return;
            }
        }
        if (root instanceof Pointer
                && ((Pointer) root).storeAggregateRange(offset, size, codec, value)) {
            return;
        }
        fromTypedStorageLocation(root, offset, size, codec).set(value);
    }

    /** Write an exact byte window without a temporary address or encoded image. */
    private boolean storeAggregateRange(long offset, long byteSize, String codec, Object value) {
        if (!(allocation instanceof byte[]) || rareState != null || addressState != null
                || byteSize <= 0 || !isGeneratedAggregateCodec(codec)
                || mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, value)) {
            return false;
        }
        CodecCalls.RangeEncoder encode = codecPlan(codec).encodeAt();
        if (encode == null) return false;
        byte[] bytes = (byte[]) allocation;
        int size = checkedArrayLength(byteSize);
        int start = Math.toIntExact(Math.addExact(byteOffset, offset));
        if (start < 0 || start > bytes.length - size) {
            throw new IndexOutOfBoundsException("aggregate store exceeds byte-addressable Rust storage");
        }
        // Save pending writes outside this range before replacement. Live views use set() to preserve their origin.
        prepareMemoryWrite(start, size);
        discardEncodedPointers(bytes, start, size);
        encode.encode(value, bytes, start);
        return true;
    }

    public static Object directStorageAggregate(Object root, long offset, Class<?> type) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            if (offset == 0 && directStorage(storage)) {
                Object value = ((Cell) storage).value;
                return type.isInstance(value) ? value : null;
            }
            root = storage.boundary();
        }
        if (root instanceof byte[]) return null;
        Pointer base = (Pointer) root;
        if (offset == 0) {
            Object direct = base.directAggregate(type);
            if (direct != null) return direct;
        }
        Object embedded = base.directCellArrayElement(offset, type);
        if (embedded != null || offset == 0) return embedded;
        if (base.rareState != null || base.addressState != null
                || !(base.allocation instanceof Object[])
                || base.allocationElementSize <= 0
                || !base.isDirectAllocationView()
                || mayHaveStructuralView(base.allocation)) return null;
        long absolute = Math.addExact(base.byteOffset, offset);
        if (absolute % base.allocationElementSize != 0) return null;
        base.flushMemoryViewsOverlapping(absolute, base.allocationElementSize);
        Object value = base.readElement(Math.toIntExact(absolute / base.allocationElementSize));
        return type.isInstance(value) ? value : null;
    }

    /** Borrow an aggregate element directly from a Cell that owns a fixed array. */
    private Object directCellArrayElement(long offset, Class<?> type) {
        if (!(allocation instanceof Cell) || addressState != null
                || traitObjectCarrier() != null || traitMetadataCarrier() != null
                || zeroSizedSourceViewSize() >= 0 || mayHaveStructuralView(allocation)
                || !isGeneratedAggregateCodec(allocationCodecClassName)
                || !isGeneratedAggregateCodec(viewCodecClassName)) return null;
        MemoryCodec plan = codecPlan(allocationCodecClassName);
        int stride = plan.arrayElementSize;
        if (stride <= 0 || viewSize != stride
                || !java.util.Objects.equals(viewCodecClassName, plan.arrayElementCodec)) return null;
        long absolute = Math.addExact(byteOffset, offset);
        if (absolute < 0 || absolute % stride != 0) return null;
        Object value = ((Cell) allocation).value;
        if (!(value instanceof Object[]) || !plan.encodeParameterType.isInstance(value)
                || (long) ((Object[]) value).length * stride != allocationElementSize
                || absolute / stride >= ((Object[]) value).length) return null;
        flushMemoryViewsOverlapping(absolute, stride);
        // Read the current carrier again. A byte alias or whole-array assignment can replace it.
        value = ((Cell) allocation).value;
        if (!(value instanceof Object[]) || !plan.encodeParameterType.isInstance(value)
                || (long) ((Object[]) value).length * stride != allocationElementSize
                || absolute / stride >= ((Object[]) value).length) return null;
        Object element = independentRepeatedArrayElement(value, (int) (absolute / stride));
        // Nested array values require their separate origin-registration rules.
        return element != null && !element.getClass().isArray() && type.isInstance(element) ? element : null;
    }

    /** Keep a scalar projection on its typed owner while its layout is exact. */
    public static Object storageFieldRoot(Object root, long offset, String owner,
            String field, long fieldOffset, long size, String codec) {
        // Scalar components supply their own width. Byte storage needs no layout carrier.
        if (codec == null && size > 0 && size <= 8 && hasByteStorage(root)
                && (!(root instanceof Pointer) || ((Pointer) root).traitMetadataCarrier() == null)) {
            return root;
        }
        if (root instanceof Storage && size > 0 && size <= 8) {
            Storage storage = (Storage) root;
            // Prepare byte storage before a later byte alias uses this root and offset.
            if (directStorage(storage) && isGeneratedAggregateCodec(storage.codec)) {
                StorageLayout layout = storage.layout(((Cell) storage).value);
                if (layout != null && layout.at(Math.addExact(offset, fieldOffset), (int) size) != null) {
                    return storage;
                }
            }
        }
        return fromStorageLocation(root, offset).projectStructField(owner, field, fieldOffset, size, codec);
    }

    public static long storageFieldOffset(Object root, Object base, long offset) {
        return root == base ? offset : 0;
    }

    public static void storeStorageLocation(Object root, long offset, Object value) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            if (offset == 0 && directStorage(storage)) {
                Cell cell = storage;
                cell.value = convertDirectValue(cell.value, value, storage.size);
                return;
            }
            root = storage.boundary();
        }
        Pointer pointer = (Pointer) root;
        if (pointer.storeAggregateRange(offset, pointer.viewSize,
                pointer.viewCodecClassName, value)) {
            return;
        }
        if (offset == 0) pointer.set(value);
        else pointer.byte_offset(offset).set(value);
    }

    public static void commitStorageLocation(Object root, long offset) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            if (offset == 0 && directStorage(storage)) {
                discardProjectedFieldViews(((Cell) storage).value);
                return;
            }
            root = storage.boundary();
        }
        Pointer pointer = (Pointer) root;
        if (offset == 0) pointer.commitMemoryView();
        else pointer.byte_offset(offset).commitMemoryView();
    }

    /** Materialize a scalar address only where a JVM object is required. */
    public static Pointer fromLocation(Object root, long offset, int size) {
        return fromTypedStorageLocation(root, offset, size, null);
    }

    /** Materialize an exact aggregate view only at a carrier boundary. */
    public static Pointer fromTypedStorageLocation(Object root, long offset, int size, String codec) {
        if (root instanceof Storage) root = ((Storage) root).boundary();
        if (root == null && offset == 0) return null;
        if (root == null) return fromUnprovenancedAddress(offset, size, codec);
        if (!(root instanceof Pointer) && root.getClass().isArray()
                && !mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, root)) {
            return new Pointer(root, inferredArrayElementSize(root), offset, size, null, codec, -1);
        }
        Pointer pointer = root instanceof Pointer ? (Pointer) root
                : array(root, 0, inferredArrayElementSize(root));
        Pointer escaped = pointer.escapedFieldStorage(offset);
        if (escaped != null) return escaped.retype(size, codec);
        // Combine offset and retype without an intermediate Pointer.
        // Keep the result independent because later metadata changes must not affect the source.
        Pointer result = new Pointer(
                pointer.allocation,
                pointer.allocationElementSize,
                pointer.byteOffset + offset,
                size,
                pointer.allocationCodecClassName,
                codec,
                pointer.allocation == null ? pointer.exposedAddress + offset : -1)
                .withMetadata(pointer.metadata)
                .copyDynamicMetadata(pointer)
                .copyAddressOrigin(pointer, offset);
        if (offset == 0 && pointer.zeroSizedSourceViewSize() >= 0) {
            result.setZeroSizedSourceView(pointer.zeroSizedSourceViewSize(),
                    pointer.zeroSizedSourceViewCodecClassName());
        } else if (pointer.viewSize == 0 && pointer.viewCodecClassName != null) {
            result.setZeroSizedSourceView(pointer.viewSize, pointer.viewCodecClassName);
        }
        return result;
    }

    public static boolean hasByteStorage(Object root) {
        return root instanceof byte[]
                || (root instanceof Pointer && ((Pointer) root).allocation instanceof byte[]);
    }

    /** Scalar field fallback after generated code has tried the managed carrier. */
    public static long loadScalarField(Object root, long offset, String owner,
            String field, long fieldOffset, int size) {
        if (hasByteStorage(root)) {
            return loadLocationBits(root, Math.addExact(offset, fieldOffset), size);
        }
        Pointer projected = fromStorageLocation(root, offset)
                .projectStructField(owner, field, fieldOffset, size, null);
        return loadLocationBits(projected, 0, size);
    }

    public static void storeScalarField(Object root, long offset, String owner,
            String field, long fieldOffset, long bits, int size) {
        if (hasByteStorage(root)) {
            storeLocationBits(root, Math.addExact(offset, fieldOffset), bits, size);
        } else {
            Pointer projected = fromStorageLocation(root, offset)
                    .projectStructField(owner, field, fieldOffset, size, null);
            storeLocationBits(projected, 0, bits, size);
        }
    }

    public static long loadLocationBits(Object root, long offset, int size) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            Object value = ((Cell) storage).value;
            if (directStorage(storage)) {
                StorageLayout layout = storage.layout(value);
                StorageLayout.Leaf leaf = layout == null ? null : layout.at(offset, size);
                if (leaf != null) return leaf.read(value, offset, size);
            }
            root = storage.boundary();
        }
        if (root instanceof Pointer) {
            Pointer pointer = (Pointer) root;
            if (pointer.hasDirectPrimitiveArrayStorage()) {
                return loadLocationBits(pointer.allocation, Math.addExact(pointer.byteOffset, offset), size);
            }
            Pointer escaped = pointer.escapedFieldStorage(offset);
            return escaped != null ? escaped.loadUnsigned(size)
                    : pointer.loadUnsignedAt(Math.addExact(pointer.byteOffset, offset), size);
        }
        int elementSize = inferredArrayElementSize(root);
        if (mayBeInIdentityFilter(MEMORY_VIEW_FILTER, root)) {
            // Keep the array used by direct JVM access.
            // array() can redirect a decoded view to its byte origin and detach later array reads.
            return new Pointer(root, elementSize, 0, elementSize, null).loadUnsignedAt(offset, size);
        }
        if (root instanceof byte[]) return MemoryBytes.read((byte[]) root, Math.toIntExact(offset), size);
        int index = Math.toIntExact(offset / elementSize);
        int within = (int) (offset % elementSize);
        if (within == 0 && size == elementSize) return primitiveArrayBits(root, index);
        long bits = 0;
        for (int i = 0; i < size; i++) {
            bits |= (long) loadPrimitiveArrayByte(root, Math.toIntExact(offset + i), elementSize) << (8 * i);
        }
        return bits;
    }

    public static void storeLocationBits(Object root, long offset, long bits, int size) {
        if (root instanceof Storage) {
            Storage storage = (Storage) root;
            Object value = ((Cell) storage).value;
            if (directStorage(storage)) {
                StorageLayout layout = storage.layout(value);
                StorageLayout.Leaf leaf = layout == null ? null : layout.at(offset, size);
                if (leaf != null) {
                    leaf.write(value, offset, bits, size);
                    return;
                }
            }
            root = storage.boundary();
        }
        if (root instanceof Pointer) {
            Pointer pointer = (Pointer) root;
            if (pointer.hasDirectPrimitiveArrayStorage()) {
                storeLocationBits(pointer.allocation, Math.addExact(pointer.byteOffset, offset), bits, size);
                return;
            }
            Pointer escaped = pointer.escapedFieldStorage(offset);
            if (escaped != null) escaped.storeBytes(bits, size);
            else pointer.storeBytesAt(Math.addExact(pointer.byteOffset, offset), bits, size);
            return;
        }
        int elementSize = inferredArrayElementSize(root);
        if (hasScalarWriteTracking(root)) {
            new Pointer(root, elementSize, 0, elementSize, null).storeBytesAt(offset, bits, size);
            return;
        }
        if (root instanceof byte[]) {
            MemoryBytes.write((byte[]) root, Math.toIntExact(offset), size, bits);
            return;
        }
        int index = Math.toIntExact(offset / elementSize);
        int within = (int) (offset % elementSize);
        if (within == 0 && size == elementSize) {
            storePrimitiveArrayBits(root, index, bits);
            return;
        }
        for (int i = 0; i < size; i++) {
            storePrimitiveArrayByte(root, Math.toIntExact(offset + i), elementSize, (int) (bits >>> (8 * i)) & 255);
        }
    }

    private boolean hasDirectPrimitiveArrayStorage() {
        return allocation != null && rareState == null && addressState == null
                && allocation.getClass().isArray()
                && allocation.getClass().getComponentType().isPrimitive()
                && allocationElementSize == inferredArrayElementSize(allocation)
                && !hasScalarWriteTracking(allocation);
    }

    private void requireScalarViewSize(int expectedSize, String scalarType) {
        if (viewSize == expectedSize) {
            return;
        }
        String sourceView = zeroSizedSourceViewSize() >= 0
                ? "; recorded erased source view is " + zeroSizedSourceViewSize() + " bytes"
                : "";
        throw new IllegalStateException(
                scalarType + " load requires a " + expectedSize + "-byte view, but pointer has a "
                        + viewSize + "-byte view" + sourceView);
    }

    /**
     * Loads a reference-valued pointee without allocating its derived pointer
     * when the JVM allocation already stores the requested Rust value directly.
     */
    private static Object loadObjectLocation(
            Pointer base,
            long byteOffset,
            String targetClassName) {
        if (base == null) {
            throw new NullPointerException("attempted to dereference a null Rust pointer");
        }
        long absoluteByteOffset =
                Math.addExact(base.byteOffset, byteOffset);
        boolean sameCodec = base.allocationCodecClassName == null
                ? base.viewCodecClassName == null
                : base.allocationCodecClassName.equals(base.viewCodecClassName);
        if (byteOffset == 0
                && base.allocation instanceof Cell
                && base.byteOffset == 0
                && base.viewSize == base.allocationElementSize
                && base.rareState == null
                && !((Cell) base.allocation).hasStructuralView
                && !isStructuralViewCodec(base.viewCodecClassName)
                && sameCodec) {
            return ((Cell) base.allocation).value;
        }
        if (base.allocation != null
                && base.allocationElementSize > 0
                && absoluteByteOffset % base.allocationElementSize == 0
                && base.viewSize == base.allocationElementSize
                && sameCodec
                && !mayHaveStructuralView(base.allocation)) {
            int elementIndex = Math.toIntExact(
                    Math.floorDiv(absoluteByteOffset, base.allocationElementSize));
            Object value = base.readElement(elementIndex);
            boolean trustedDirectCarrier = base.allocation instanceof Cell
                    || base.allocation instanceof ReceiverCell
                    || base.allocation instanceof FieldCell;
            if (targetClassName == null
                    || targetClassName.isEmpty()
                    || value == null
                    || trustedDirectCarrier) {
                base.flushMemoryViewsOverlapping(
                        absoluteByteOffset, base.allocationElementSize);
                value = base.readElement(elementIndex);
                return value;
            }
            try {
                Class<?> valueClass = value.getClass();
                if (matchesBinaryClassName(targetClassName, valueClass.getName())) {
                    base.flushMemoryViewsOverlapping(
                            absoluteByteOffset, base.allocationElementSize);
                    value = base.readElement(elementIndex);
                    if (value != null
                            && matchesBinaryClassName(
                                    targetClassName, value.getClass().getName())) {
                        return value;
                    }
                }
                Class<?> targetClass =
                        resolvedClass(targetClassName, valueClass.getClassLoader());
                if (targetClass.isInstance(value)) {
                    base.flushMemoryViewsOverlapping(
                            absoluteByteOffset, base.allocationElementSize);
                    value = base.readElement(elementIndex);
                    if (targetClass.isInstance(value)) {
                        return value;
                    }
                }
            } catch (ClassNotFoundException error) {
                throw new IllegalStateException(
                        "could not load requested Rust value " + targetClassName,
                        error);
            }
        }
        Pointer pointer = fromStorageLocation(base, byteOffset);
        return targetClassName == null || targetClassName.isEmpty()
                ? pointer.getObject()
                : pointer.getObjectAs(targetClassName);
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

    private boolean hasPrimitiveArrayStorage() {
        return allocation != null
                && allocation.getClass().isArray()
                && allocation.getClass().getComponentType().isPrimitive();
    }

    public void set(boolean value) {
        requireScalarViewSize(1, "bool");
        if (hasPrimitiveArrayStorage()) {
            storeBytes(value ? 1 : 0, 1);
        } else {
            set((Object) Boolean.valueOf(value));
        }
    }

    public void set(byte value) {
        requireScalarViewSize(1, "i8/u8");
        if (hasPrimitiveArrayStorage()) {
            storeBytes(value, 1);
        } else {
            set((Object) Byte.valueOf(value));
        }
    }

    public void set(short value) {
        requireScalarViewSize(2, "i16/f16");
        if (hasPrimitiveArrayStorage()) {
            storeBytes(value, 2);
        } else {
            set((Object) Short.valueOf(value));
        }
    }

    public void set(char value) {
        requireScalarViewSize(2, "u16");
        if (hasPrimitiveArrayStorage()) {
            storeBytes(value, 2);
        } else {
            set((Object) Character.valueOf(value));
        }
    }

    public void set(int value) {
        requireScalarViewSize(4, "i32/u32");
        if (hasPrimitiveArrayStorage()) {
            storeBytes(value, 4);
        } else {
            set((Object) Integer.valueOf(value));
        }
    }

    public void set(long value) {
        requireScalarViewSize(8, "i64/u64");
        if (hasPrimitiveArrayStorage()) {
            storeBytes(value, 8);
        } else {
            set((Object) Long.valueOf(value));
        }
    }

    public void set(float value) {
        requireScalarViewSize(4, "f32");
        if (hasPrimitiveArrayStorage()) {
            storeBytes(Float.floatToRawIntBits(value), 4);
        } else {
            set((Object) Float.valueOf(value));
        }
    }

    public void set(double value) {
        requireScalarViewSize(8, "f64");
        if (hasPrimitiveArrayStorage()) {
            storeBytes(Double.doubleToRawLongBits(value), 8);
        } else {
            set((Object) Double.valueOf(value));
        }
    }

    public Object getObject() {
        if (traitObjectCarrier() != null) {
            return traitObjectCarrier();
        }
        Object direct = directCellValueOrSelf();
        if (direct != this && direct instanceof TraitObjectCarrier) {
            return direct;
        }
        if (direct != this
                && MANAGED_OBJECT_VIEW_CODEC.equals(viewCodecClassName)
                && isDirectAllocationView()) {
            return direct;
        }
        if (viewSize == 0 && zeroSizedSourceViewSize() >= 0) {
            Pointer pointer = new Pointer(
                            allocation,
                            allocationElementSize,
                            byteOffset,
                            zeroSizedSourceViewSize(),
                            allocationCodecClassName,
                            zeroSizedSourceViewCodecClassName(),
                            exposedAddress).withMetadata(metadata)
                    .copyAddressOrigin(this, 0);
            pointer.traitObjectCarrier(traitObjectCarrier());
            pointer.traitMetadataCarrier(traitMetadataCarrier());
            pointer.traitMetadataMarker(traitMetadataMarker());
            pointer.traitPointeeSize(traitPointeeSize());
            pointer.traitPointeeAlignment(traitPointeeAlignment());
            pointer.traitAdapterClassName(traitAdapterClassName());
            pointer.traitPointeeCodecClassName(traitPointeeCodecClassName());
            return pointer.getObject();
        }
        if (direct != this
                && !isStructuralViewCodec(viewCodecClassName)
                && isDirectAllocationView()) {
            if (direct != null && direct.getClass().isArray()) {
                registerMemoryViewOrigin(direct);
            }
            StructuralViewState state = structuralViewState(direct, false);
            return state == null ? direct : state.activate(direct.getClass());
        }
        if (MANAGED_OBJECT_VIEW_CODEC.equals(viewCodecClassName)
                && !isDirectAllocationView()) {
            return managedObjectFromAddress(loadUnsigned((int) Math.min(viewSize, 8)));
        }
        if (isRawPointerCodec(viewCodecClassName)
                && !isDirectAllocationView()) {
            int encodedSize = (int) Math.min(viewSize, 8);
            long address = loadUnsigned(encodedSize);
            Pointer pointer = encodedPointer(
                    allocation,
                    byteOffset,
                    encodedSize,
                    viewCodecClassName,
                    address);
            if (isArrayReferenceCodec(viewCodecClassName)) {
                try {
                    return decodeArrayReference(
                            pointer == null
                                    ? typedPointerObjectFromAddress(
                                            address, viewCodecClassName)
                                    : pointer,
                            viewCodecClassName,
                            resolvedRuntimeClass(SLICE_VIEW_CLASS_NAME));
                } catch (ClassNotFoundException error) {
                    throw new IllegalStateException(
                            "could not load fixed-array reference carrier", error);
                }
            }
            return decodedRawPointer(
                    pointer == null
                            ? typedPointerObjectFromAddress(address, viewCodecClassName)
                            : pointer,
                    viewCodecClassName);
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
            byte[] image = loadRange(materializedSize);
            try {
                return decodeFatPointer(image, 0, materializedSize, viewCodecClassName);
            } finally {
                discardEncodedReferences(image);
            }
        }
        if (isStructuralViewCodec(viewCodecClassName)) {
            return structuralViewObject();
        }
        if (viewCodecClassName != null && !isDirectAllocationView()) {
            return decodedMemoryView();
        }
        Object value = readAlignedElement();
        if (value != null && value.getClass().isArray()) {
            // References to a fixed-array local are represented as SliceViews
            // over its JVM array carrier. Retain the local's storage origin so
            // casting that reference and taking the local's address agree.
            registerMemoryViewOrigin(value);
        }
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
        if (byteOffset == 0
                && viewSize == 0
                && allocationElementSize == 0
                && allocation != null
                && !allocation.getClass().isArray()) {
            return allocation;
        }
        return this;
    }

    /**
     * Returns an existing aggregate carrier without decoding surrounding bytes.
     * A single field access is valid even when neighboring fields are still
     * uninitialized, so callers must project the field if this returns null.
     */
    public Object directAggregate(Class<?> type) {
        Object bound = activeBoundMemoryViewValue(materializedViewSize());
        if (type.isInstance(bound)) {
            return bound;
        }
        if (traitObjectCarrier() != null
                || zeroSizedSourceViewSize() >= 0
                || isStructuralViewCodec(viewCodecClassName)
                || !isDirectAllocationView()
                || mayHaveStructuralView(allocation)) {
            return null;
        }
        Object direct = directCellValueOrSelf();
        if (direct == this && allocation instanceof Object[]) {
            direct = readAlignedElement();
        }
        return direct != this && type.isInstance(direct) ? direct : null;
    }

    /** Reads an owned aggregate value without creating a live alias of byte storage. */
    public Object getObjectCopyAs(String targetClassName) {
        if (viewSize > 0 && !isDirectAllocationView()
                && isGeneratedAggregateCodec(viewCodecClassName)) {
            MemoryCodec plan = codecPlan(viewCodecClassName);
            if (targetClassName != null
                    && matchesBinaryClassName(targetClassName, plan.encodeParameterType.getName())) {
                if (allocation instanceof byte[] && rareState == null && plan.decodeAt() != null) {
                    byte[] bytes = (byte[]) allocation;
                    int offset = Math.toIntExact(byteOffset);
                    int size = materializedViewSize();
                    if (offset < 0 || offset > bytes.length - size) {
                        throw new IndexOutOfBoundsException(
                                "aggregate read exceeds byte-addressable Rust storage");
                    }
                    flushMemoryViewsOverlapping(byteOffset, size);
                    return plan.decodeAt().decode(bytes, offset);
                }
                // Owned reads need an independent image with provenance metadata.
                // A live decoded view could overwrite source padding when flushed.
                return decodeAggregate(viewCodecClassName, loadRange(materializedViewSize()));
            }
        }
        return copyManagedValue(getObjectAs(targetClassName));
    }

    public Object getObjectAs(String targetClassName) {
        // Keep the ordinary typed cell read small enough for the JVM to inline
        // at generated call sites. All decoded/DST views use the slow path.
        if (rareState == null
                && targetClassName != null && !targetClassName.isEmpty()
                && allocation instanceof Cell
                && byteOffset == 0
                && viewSize == allocationElementSize
                && !((Cell) allocation).hasStructuralView
                && !isStructuralViewCodec(viewCodecClassName)
                && (allocationCodecClassName == null
                        ? viewCodecClassName == null
                        : allocationCodecClassName.equals(viewCodecClassName))) {
            return ((Cell) allocation).value;
        }
        return getObjectAsSlow(targetClassName);
    }

    private Object getObjectAsSlow(String targetClassName) {
        if (targetClassName == null || targetClassName.isEmpty()) {
            return getObject();
        }
        Object boundValue = activeBoundMemoryViewValue(materializedViewSize());
        if (boundValue != null
                && matchesBinaryClassName(
                        targetClassName, boundValue.getClass().getName())) {
            return boundValue;
        }
        Object direct = directCellValueOrSelf();
        if (direct != this
                && traitObjectCarrier() == null
                && zeroSizedSourceViewSize() < 0
                && !isStructuralViewCodec(viewCodecClassName)
                && isDirectAllocationView()
                && !mayHaveStructuralView(allocation)) {
            return direct;
        }
        if (viewSize == 0
                && zeroSizedSourceViewSize() >= 0
                && isGeneratedAggregateCodec(viewCodecClassName)
                && !viewCodecClassName.equals(zeroSizedSourceViewCodecClassName())) {
            Object requestedView = decodedMemoryView();
            if (requestedView != null
                    && matchesBinaryClassName(
                            targetClassName, requestedView.getClass().getName())) {
                return requestedView;
            }
        }
        if (matchesBinaryClassName(targetClassName, SLICE_VIEW_CLASS_NAME)
                && isArrayReferenceCodec(viewCodecClassName)
                && !isDirectAllocationView()) {
            try {
                int encodedSize = (int) Math.min(viewSize, 8);
                long address = loadUnsigned(encodedSize);
                Pointer pointer = encodedPointer(
                        allocation,
                        byteOffset,
                        encodedSize,
                        viewCodecClassName,
                        address);
                Class<?> targetClass = resolvedRuntimeClass(targetClassName);
                return decodeArrayReference(
                        pointer == null
                                ? typedPointerObjectFromAddress(
                                        address, viewCodecClassName)
                                : pointer,
                        viewCodecClassName,
                        targetClass);
            } catch (ClassNotFoundException error) {
                throw new IllegalStateException(
                        "could not load fixed-array reference carrier", error);
            }
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
            Class<?> targetClass = resolvedClass(targetClassName, value.getClass().getClassLoader());
            return state.activate(targetClass, traitMetadataCarrier());
        } catch (ClassNotFoundException error) {
            throw new IllegalStateException(
                    "could not load requested Rust structural view " + targetClassName, error);
        }
    }

    private boolean isDirectAllocationView() {
        boolean sameCodec = allocationCodecClassName == null
                ? viewCodecClassName == null
                : allocationCodecClassName.equals(viewCodecClassName);
        if (allocationElementSize == 0) {
            return byteOffset == 0
                    && viewSize == 0
                    && sameCodec
                    && ((allocation != null && !allocation.getClass().isArray())
                            || allocation instanceof Cell
                            || allocation instanceof ReceiverCell
                            || allocation instanceof FieldCell)
                    && directCellValueOrSelf() != null;
        }
        return allocationElementSize != 0
                && byteOffset % allocationElementSize == 0
                && viewSize == allocationElementSize
                && sameCodec;
    }

    public Object backingArray() {
        return backingArray(null);
    }

    private Object backingArrayRange(Class<?> requestedArrayType, int offset, int length) {
        if (allocation == null || !allocation.getClass().isArray()) {
            return copyArrayRange(backingArray(requestedArrayType), requestedArrayType, offset, length);
        }
        if (requestedArrayType != null && !requestedArrayType.isInstance(allocation)) {
            return null;
        }
        if (allocationElementSize != 0) {
            if (byteOffset % allocationElementSize != 0) {
                throw new IllegalStateException("unaligned pointer cannot be exposed as a JVM array");
            }
            offset = Math.addExact(offset, Math.toIntExact(byteOffset / allocationElementSize));
        }
        flushAllMemoryViews();
        // Materialize only the requested window. Copying the remaining allocation
        // first makes small fixed-array reads proportional to the backing size.
        return copyArrayRange(allocation, requestedArrayType, offset, length, allocationElementSize);
    }

    private Object backingArray(Class<?> requestedArrayType) {
        if (allocation == null) {
            return null;
        }
        flushAllMemoryViews();
        if (allocation.getClass().isArray()) {
            if (requestedArrayType != null
                    && (!requestedArrayType.isArray()
                            || (!requestedArrayType.getComponentType().isAssignableFrom(
                                            allocation.getClass().getComponentType())
                                    && requestedArrayType.getComponentType()
                                            != allocation.getClass().getComponentType()))) {
                return null;
            }
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
            transferEncodedPointers(
                    allocation,
                    (long) elementOffset * allocationElementSize,
                    tail,
                    0,
                    Math.multiplyExact(remaining, allocationElementSize),
                    false);
            transferEncodedReferences(allocation, tail);
            return tail;
        }
        if (allocationElementSize != 0 && byteOffset % allocationElementSize != 0) {
            // A fixed-array reference may point into an aggregate allocation.
            // There is no directly exposable JVM array in that case; callers
            // with a requested array type will materialize it element by element.
            return null;
        }
        return readAlignedElement();
    }

    public Object sliceBackingArray() {
        return hasDirectSliceArrayBacking() ? allocation : this;
    }

    public int sliceElementOffset() {
        if (!hasDirectSliceArrayBacking()) {
            // The returned slice backing is this already-offset Pointer.
            return 0;
        }
        if (allocationElementSize == 0) {
            return 0;
        }
        if (byteOffset % allocationElementSize != 0) {
            throw new IllegalStateException("slice data pointer is not element-aligned");
        }
        return Math.toIntExact(byteOffset / allocationElementSize);
    }

    private boolean hasDirectSliceArrayBacking() {
        if (allocation == null || !allocation.getClass().isArray()) {
            return false;
        }
        if (allocationElementSize != viewSize) {
            return false;
        }
        if (allocation.getClass().getComponentType().isArray()
                && nestedPrimitiveArrayElementByteSize(allocation) > allocationElementSize) {
            return false;
        }
        return allocationCodecClassName == null
                ? viewCodecClassName == null
                : allocationCodecClassName.equals(viewCodecClassName);
    }

    // A decoded view assigned back to its source already aliases that storage.
    // Flush it with its own codec instead of reinterpreting its JVM carrier.
    private boolean flushSameOriginMemoryView(Object value, int size) {
        if (directCellHasNoMemoryViews(allocation)
                || cannotCarryMemoryViewOrigin(value)) {
            return false;
        }
        if (!mayBeInIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, value)) {
            return false;
        }
        MemoryViewOrigin origin;
        Map<Object, MemoryViewOrigin> originStripe =
                stateStripe(MEMORY_VIEW_ORIGINS, value);
        synchronized (originStripe) {
            origin = originStripe.get(value);
        }
        if (origin == null
                || origin.allocation.get() != allocation
                || origin.byteOffset != byteOffset
                || origin.viewSize != size) {
            return false;
        }
        if (!activeMemoryViewMatches(value, origin)) {
            return false;
        }
        flushMemoryViewsOverlapping(byteOffset, size);
        return true;
    }

    private static boolean activeMemoryViewMatches(Object value, MemoryViewOrigin origin) {
        return activeMemoryViewState(value, origin) != null;
    }

    private static MemoryViewState activeMemoryViewState(
            Object value, MemoryViewOrigin origin) {
        Object allocation = origin.allocation.get();
        if (allocation == null) {
            return null;
        }
        MemoryViewState active;
        Map<Object, LongRangeMap<MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (stripe) {
            LongRangeMap<MemoryViewState> views = stripe.get(allocation);
            active = views == null ? null : views.get(origin.byteOffset);
            if (active == null || active.size != origin.viewSize) {
                return null;
            }
        }
        if (active.value == value) {
            return active;
        }
        Pointer pointer = new Pointer(
                allocation,
                origin.allocationElementSize,
                origin.byteOffset,
                origin.viewSize,
                origin.allocationCodecClassName,
                active.codecClassName,
                -1);
        return pointer.transparentManagedView(active.value.getClass()) == value
                ? active
                : null;
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
            if (allocation instanceof FieldCell && ((FieldCell) allocation).access.borrowed) {
                // Do not read the old borrowed value. Partially initialized storage may not contain a valid carrier.
                writeElement(elementIndex, value);
                return;
            }
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
                retainEncodedReference(image, value);
                for (int index = 0; index < Math.min(materializedSize, 8); index++) {
                    image[index] = (byte) (address >>> (index * 8));
                }
                storeRange(image);
                return;
            }
            if (isRawPointerCodec(viewCodecClassName)) {
                byte[] image = new byte[materializedSize];
                Pointer pointer = rawPointerCarrier(value, viewCodecClassName);
                long address = encodedAddress(
                        pointer,
                        image,
                        0,
                        materializedSize,
                        viewCodecClassName);
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
        if (tryCopyScalarRange(
                source,
                source.byteOffset,
                destination,
                destination.byteOffset,
                byteCount)) {
            return;
        }
        if (tryCopyDirectUnionRange(
                source,
                source.byteOffset,
                destination,
                destination.byteOffset,
                byteCount)) {
            return;
        }
        if (tryCopyPrimitiveArrayRange(
                source, source.byteOffset, destination, destination.byteOffset, byteCount)) {
            return;
        }
        byte[] temporary = source.loadRange(byteCount);
        transferEncodedReferences(source.allocation, temporary);
        destination.storeRange(temporary);
    }

    private static boolean tryCopyScalarRange(
            Pointer source,
            long sourceByteOffset,
            Pointer destination,
            long destinationByteOffset,
            int byteCount) {
        if (byteCount < 0 || byteCount > 8
                || source.allocation == null
                || destination.allocation == null) {
            return false;
        }
        if (byteCount == 0
                || (source.allocation == destination.allocation
                        && sourceByteOffset == destinationByteOffset)) {
            return true;
        }
        if (source.allocation == destination.allocation) {
            long sourceEnd = Math.addExact(sourceByteOffset, byteCount);
            long destinationEnd = Math.addExact(destinationByteOffset, byteCount);
            if (sourceByteOffset < destinationEnd
                    && destinationByteOffset < sourceEnd) {
                return false;
            }
        }
        long bits = source.loadUnsignedAt(sourceByteOffset, byteCount);
        destination.storeBytesAt(destinationByteOffset, bits, byteCount);
        transferEncodedPointers(
                source.allocation,
                sourceByteOffset,
                destination.allocation,
                destinationByteOffset,
                byteCount,
                false);
        transferEncodedReferences(source.allocation, destination.allocation);
        return true;
    }

    private static Object directCellCarrier(Object allocation) {
        if (allocation instanceof Cell) {
            return ((Cell) allocation).value;
        }
        if (allocation instanceof ReceiverCell) {
            return ((ReceiverCell) allocation).value;
        }
        if (allocation instanceof FieldCell) {
            return ((FieldCell) allocation).get();
        }
        return null;
    }

    /**
     * Copies a range between generated union carriers without serialising the
     * complete containing aggregate. Sorting scratch buffers such as
     * {@code [MaybeUninit<String>; N]} use these byte/object planes directly.
     */
    private static boolean tryCopyDirectUnionRange(
            Pointer source,
            long sourceByteOffset,
            Pointer destination,
            long destinationByteOffset,
            int byteCount) {
        if (byteCount < 0
                || source.allocationCodecClassName == null
                || destination.allocationCodecClassName == null
                || !isGeneratedAggregateCodec(source.allocationCodecClassName)
                || !isGeneratedAggregateCodec(destination.allocationCodecClassName)) {
            return false;
        }
        Object sourceCarrier = directCellCarrier(source.allocation);
        Object destinationCarrier = directCellCarrier(destination.allocation);
        if (sourceCarrier == null || destinationCarrier == null) {
            return false;
        }
        MemoryCodec sourcePlan = codecPlan(source.allocationCodecClassName);
        MemoryCodec destinationPlan = codecPlan(destination.allocationCodecClassName);
        byte[] sourceBytes = sourcePlan.directUnionBytes(sourceCarrier);
        byte[] destinationBytes = destinationPlan.directUnionBytes(destinationCarrier);
        if (sourceBytes == null || destinationBytes == null) {
            return false;
        }
        Object[] sourceObjects = sourcePlan.directUnionObjects(sourceCarrier);
        Object[] destinationObjects = destinationPlan.directUnionObjects(destinationCarrier);
        if (sourceObjects == null || destinationObjects == null) {
            return false;
        }
        int sourceOffset;
        int destinationOffset;
        try {
            sourceOffset = Math.toIntExact(sourceByteOffset);
            destinationOffset = Math.toIntExact(destinationByteOffset);
        } catch (ArithmeticException error) {
            return false;
        }
        if (sourceOffset < 0
                || destinationOffset < 0
                || byteCount > sourceBytes.length - sourceOffset
                || byteCount > destinationBytes.length - destinationOffset
                || (sourceObjects.length != 0
                        && byteCount > sourceObjects.length - sourceOffset)
                || (destinationObjects.length != 0
                        && byteCount > destinationObjects.length - destinationOffset)) {
            return false;
        }
        if (byteCount == 0
                || (sourceBytes == destinationBytes
                        && sourceOffset == destinationOffset)) {
            return true;
        }
        source.flushMemoryViewsOverlapping(sourceByteOffset, byteCount);
        destination.prepareMemoryWrite(destinationByteOffset, byteCount);
        copyUnionStorage(
                sourceBytes,
                sourceObjects,
                sourceOffset,
                destinationBytes,
                destinationObjects,
                destinationOffset,
                byteCount);
        return true;
    }

    private static boolean tryCopyPrimitiveArrayRange(
            Pointer source,
            long sourceByteOffset,
            Pointer destination,
            long destinationByteOffset,
            int byteCount) {
        if (byteCount < 0
                || source.allocation == null
                || destination.allocation == null
                || !source.allocation.getClass().isArray()
                || source.allocation.getClass() != destination.allocation.getClass()
                || !source.allocation.getClass().getComponentType().isPrimitive()) {
            return false;
        }
        int elementSize = inferredArrayElementSize(source.allocation);
        if (elementSize <= 0
                || source.allocationElementSize != elementSize
                || destination.allocationElementSize != elementSize
                || Math.floorMod(sourceByteOffset, elementSize) != 0
                || Math.floorMod(destinationByteOffset, elementSize) != 0
                || byteCount % elementSize != 0
                || mayBeInIdentityFilter(ENCODED_POINTER_FILTER, source.allocation)
                || mayBeInIdentityFilter(ENCODED_POINTER_FILTER, destination.allocation)
                || mayBeInIdentityFilter(ENCODED_REFERENCE_FILTER, source.allocation)
                || mayBeInIdentityFilter(ENCODED_REFERENCE_FILTER, destination.allocation)) {
            return false;
        }
        source.flushMemoryViewsOverlapping(sourceByteOffset, byteCount);
        destination.prepareMemoryWrite(destinationByteOffset, byteCount);
        System.arraycopy(
                source.allocation,
                Math.toIntExact(sourceByteOffset / elementSize),
                destination.allocation,
                Math.toIntExact(destinationByteOffset / elementSize),
                byteCount / elementSize);
        return true;
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

    private static void swapBytes(Pointer left, Pointer right, int byteCount) {
        if (trySwapAlignedElements(left, right, byteCount)) {
            return;
        }
        byte[] leftBytes = left.loadRange(byteCount);
        byte[] rightBytes = right.loadRange(byteCount);
        transferEncodedReferences(left.allocation, leftBytes);
        transferEncodedReferences(right.allocation, rightBytes);
        left.storeRange(rightBytes);
        right.storeRange(leftBytes);
    }

    private static boolean trySwapAlignedElements(
            Pointer left, Pointer right, int byteCount) {
        if (byteCount <= 0
                || left.allocation == null
                || left.allocation != right.allocation
                || left.allocationElementSize != byteCount
                || right.allocationElementSize != byteCount
                || Math.floorMod(left.byteOffset, byteCount) != 0
                || Math.floorMod(right.byteOffset, byteCount) != 0) {
            return false;
        }
        int leftIndex = Math.toIntExact(left.byteOffset / byteCount);
        int rightIndex = Math.toIntExact(right.byteOffset / byteCount);
        if (leftIndex == rightIndex) {
            return true;
        }
        left.prepareMemoryWrite(left.byteOffset, byteCount);
        right.prepareMemoryWrite(right.byteOffset, byteCount);
        Object leftValue = left.readElement(leftIndex);
        Object rightValue = right.readElement(rightIndex);
        if (hasProjectedFieldCells(leftValue) || hasProjectedFieldCells(rightValue)) {
            return false;
        }
        left.writeElement(leftIndex, rightValue);
        right.writeElement(rightIndex, leftValue);
        return true;
    }

    public static void swap(Pointer left, Pointer right, long byteCount) {
        swapBytes(left, right, checkedArrayLength(byteCount));
    }

    public static void swapNonOverlapping(Pointer left, Pointer right, int byteCount) {
        if (left.allocation == right.allocation) {
            long leftEnd = left.byteOffset + byteCount;
            long rightEnd = right.byteOffset + byteCount;
            if (left.byteOffset < rightEnd && right.byteOffset < leftEnd) {
                throw new IllegalArgumentException("swap_nonoverlapping regions overlap");
            }
        }
        swapBytes(left, right, byteCount);
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
            RustField[] fields = PUBLIC_INSTANCE_FIELDS.get(current.getClass());
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

    /**
     * Finds a slice carried through transparent single-field Rust wrappers,
     * such as {@code UnsafeCell<[T]>}.
     */
    private static Object transparentSliceView(Pointer storage, Object value) {
        if (value == storage
                || (storage.allocationElementSize != 0
                        && (value == null || !isSliceViewType(value.getClass())))) {
            return null;
        }
        try {
            for (int depth = 0; value != null && depth < 16; depth++) {
                if (isSliceViewType(value.getClass())) {
                    return value;
                }
                RustField[] fields = PUBLIC_INSTANCE_FIELDS.get(value.getClass());
                if (fields.length != 1) {
                    return null;
                }
                Object next = fields[0].get(value);
                if (next == value) {
                    return null;
                }
                value = next;
            }
            return null;
        } catch (IllegalAccessException error) {
            throw new IllegalStateException(
                    "could not inspect transparent Rust slice wrapper", error);
        }
    }

    private Pointer sliceElementView() {
        Pointer storage = sliceStorageView();
        Object direct = storage.directCellValueOrSelf();
        Object slice = transparentSliceView(storage, direct);
        return slice == null
                ? storage
                : fromSlice(slice, storage.viewSize, storage.viewCodecClassName);
    }

    private Pointer sliceStorageView() {
        if (viewSize != 0) {
            return this;
        }
        if (zeroSizedSourceViewSize() >= 0) {
            return retype(
                    zeroSizedSourceViewSize(),
                    zeroSizedSourceViewCodecClassName());
        }
        return restoreAllocationView(this);
    }

    /** Applies the Rust element layout to a generic reference-array slice backing. */
    private Pointer sliceStorageView(long elementSize, String elementCodecClassName) {
        Pointer storage = sliceStorageView();
        int checkedElementSize = checkedArrayLength(elementSize);
        Object direct = storage.directCellValueOrSelf();
        Object slice = transparentSliceView(storage, direct);
        if (slice != null) {
            try {
                Object backing = instanceField(slice.getClass(), "array").get(slice);
                boolean selfBacked = backing == storage || backing == this;
                if (backing instanceof Pointer) {
                    Pointer pointerBacking = (Pointer) backing;
                    selfBacked |= pointerBacking.allocation == storage.allocation;
                }
                if (!selfBacked) {
                    return fromSlice(slice, checkedElementSize, elementCodecClassName);
                }
            } catch (ReflectiveOperationException error) {
                throw new IllegalArgumentException("invalid Rust slice view", error);
            }
        }
        if (storage.allocation != null
                && storage.allocation.getClass().isArray()
                && !storage.allocation.getClass().getComponentType().isPrimitive()) {
            String allocationCodec = storage.allocationCodecClassName != null
                    ? storage.allocationCodecClassName
                    : elementCodecClassName;
            if (checkedElementSize == 0) {
                return new Pointer(
                                storage.allocation,
                                0,
                                0,
                                0,
                                allocationCodec,
                                elementCodecClassName,
                                -1)
                        .withMetadata(storage.metadata)
                        .copyAddressOrigin(storage, 0);
            }
            if (storage.allocationElementSize == 0
                    || storage.byteOffset % storage.allocationElementSize != 0) {
                throw new IllegalStateException(
                        "reference-array slice backing has an unaligned storage offset");
            }
            long elementOffset = storage.byteOffset / storage.allocationElementSize;
            long retypedByteOffset =
                    nestedPrimitiveArrayElementByteSize(storage.allocation) > 0
                            ? storage.byteOffset
                            : Math.multiplyExact(elementOffset, elementSize);
            return new Pointer(
                            storage.allocation,
                            checkedElementSize,
                            retypedByteOffset,
                            checkedElementSize,
                            allocationCodec,
                            elementCodecClassName,
                            -1)
                    .withMetadata(storage.metadata)
                    .copyAddressOrigin(storage, 0);
        }
        if (storage.viewSize == checkedElementSize
                && java.util.Objects.equals(
                        storage.viewCodecClassName, elementCodecClassName)) {
            return storage;
        }
        return storage.retype(checkedElementSize, elementCodecClassName);
    }

    private static boolean hasScalarWriteTracking(Object root) {
        return mayBeInIdentityFilter(MEMORY_VIEW_FILTER, root)
                || mayBeInIdentityFilter(ENCODED_POINTER_FILTER, root)
                || mayBeInIdentityFilter(ENCODED_REFERENCE_FILTER, root);
    }

    public static boolean sliceGetBoolean(Object backing, int index) {
        if (backing instanceof boolean[]) {
            return ((boolean[]) backing)[index];
        }
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getBoolean()
                : ((Boolean) arrayGet(backing, index)).booleanValue();
    }

    public static byte sliceGetI8(Object backing, int index) {
        if (backing instanceof byte[]) {
            return ((byte[]) backing)[index];
        }
        if (backing instanceof boolean[]) {
            return (byte) (((boolean[]) backing)[index] ? 1 : 0);
        }
        Pointer pointer = slicePointer(backing, index);
        if (pointer != null) {
            return pointer.getI8();
        }
        Object value = arrayGet(backing, index);
        return value instanceof Boolean
                ? (byte) (((Boolean) value).booleanValue() ? 1 : 0)
                : ((Number) value).byteValue();
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
        if (backing instanceof short[]) {
            return ((short[]) backing)[index];
        }
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getI16()
                : ((Number) arrayGet(backing, index)).shortValue();
    }

    public static char sliceGetU16(Object backing, int index) {
        if (backing instanceof char[]) {
            return ((char[]) backing)[index];
        }
        if (backing instanceof short[]) {
            return (char) (((short[]) backing)[index] & 0xffff);
        }
        Pointer pointer = slicePointer(backing, index);
        if (pointer != null) {
            return (char) (pointer.getI16() & 0xffff);
        }
        Object value = arrayGet(backing, index);
        return value instanceof Character
                ? ((Character) value).charValue()
                : (char) (((Number) value).intValue() & 0xffff);
    }

    public static int sliceGetI32(Object backing, int index) {
        if (backing instanceof int[]) {
            return ((int[]) backing)[index];
        }
        if (backing instanceof char[]) {
            return ((char[]) backing)[index];
        }
        Pointer pointer = slicePointer(backing, index);
        Object value =
                pointer != null ? Integer.valueOf(pointer.getI32()) : arrayGet(backing, index);
        return value instanceof Character
                ? ((Character) value).charValue()
                : ((Number) value).intValue();
    }

    public static long sliceGetI64(Object backing, int index) {
        if (backing instanceof long[]) {
            return ((long[]) backing)[index];
        }
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getI64()
                : ((Number) arrayGet(backing, index)).longValue();
    }

    public static float sliceGetF32(Object backing, int index) {
        if (backing instanceof float[]) {
            return ((float[]) backing)[index];
        }
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getF32()
                : ((Number) arrayGet(backing, index)).floatValue();
    }

    public static double sliceGetF64(Object backing, int index) {
        if (backing instanceof double[]) {
            return ((double[]) backing)[index];
        }
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getF64()
                : ((Number) arrayGet(backing, index)).doubleValue();
    }

    public static Object sliceGetObject(Object backing, int index) {
        if (backing instanceof Object[]) {
            return independentRepeatedArrayElement(backing, index);
        }
        Pointer pointer = slicePointer(backing, index);
        return pointer != null
                ? pointer.getObject()
                : independentRepeatedArrayElement(backing, index);
    }

    public static void sliceSetBoolean(Object backing, int index, boolean value) {
        if (backing instanceof boolean[]) {
            ((boolean[]) backing)[index] = value;
            return;
        }
        sliceSetObject(backing, index, Boolean.valueOf(value));
    }

    public static void sliceSetI8(Object backing, int index, byte value) {
        if (backing instanceof byte[]) {
            ((byte[]) backing)[index] = value;
            return;
        }
        if (backing instanceof boolean[]) {
            ((boolean[]) backing)[index] = value != 0;
            return;
        }
        sliceSetObject(backing, index, Byte.valueOf(value));
    }

    public static void sliceSetI16(Object backing, int index, short value) {
        if (backing instanceof short[]) {
            ((short[]) backing)[index] = value;
            return;
        }
        sliceSetObject(backing, index, Short.valueOf(value));
    }

    public static void sliceSetU16(Object backing, int index, char value) {
        if (backing instanceof char[]) {
            ((char[]) backing)[index] = value;
            return;
        }
        if (backing instanceof short[]) {
            ((short[]) backing)[index] = (short) value;
            return;
        }
        sliceSetObject(backing, index, Character.valueOf(value));
    }

    public static void sliceSetI32(Object backing, int index, int value) {
        if (backing instanceof int[]) {
            ((int[]) backing)[index] = value;
            return;
        }
        if (backing instanceof char[]) {
            ((char[]) backing)[index] = (char) value;
            return;
        }
        Pointer pointer = slicePointer(backing, index);
        if (pointer != null) {
            pointer.set(Integer.valueOf(value));
            return;
        }
        Class<?> component = backing.getClass().getComponentType();
        if (component == byte.class) {
            Array.setByte(backing, index, (byte) value);
        } else if (component == short.class) {
            Array.setShort(backing, index, (short) value);
        } else if (component == char.class) {
            Array.setChar(backing, index, (char) value);
        } else if (component == int.class) {
            Array.setInt(backing, index, value);
        } else {
            arraySet(backing, index, Integer.valueOf(value));
        }
    }

    public static void sliceSetI64(Object backing, int index, long value) {
        if (backing instanceof long[]) {
            ((long[]) backing)[index] = value;
            return;
        }
        sliceSetObject(backing, index, Long.valueOf(value));
    }

    public static void sliceSetF32(Object backing, int index, float value) {
        if (backing instanceof float[]) {
            ((float[]) backing)[index] = value;
            return;
        }
        sliceSetObject(backing, index, Float.valueOf(value));
    }

    public static void sliceSetF64(Object backing, int index, double value) {
        if (backing instanceof double[]) {
            ((double[]) backing)[index] = value;
            return;
        }
        sliceSetObject(backing, index, Double.valueOf(value));
    }

    public static void sliceSetObject(Object backing, int index, Object value) {
        if (backing instanceof Object[]) {
            ((Object[]) backing)[index] = value;
            return;
        }
        Pointer pointer = slicePointer(backing, index);
        if (pointer != null) {
            pointer.set(value);
        } else {
            arraySet(backing, index, value);
        }
    }

    private void storeRange(byte[] source) {
        prepareMemoryWrite(byteOffset, source.length);
        discardEncodedPointers(allocation, byteOffset, source.length);
        int directPrimitiveElementSize =
                allocation != null
                        && allocation.getClass().isArray()
                        && allocation.getClass().getComponentType().isPrimitive()
                        && allocationElementSize == inferredArrayElementSize(allocation)
                ? allocationElementSize
                : 0;
        if (directPrimitiveElementSize > 0) {
            for (int index = 0; index < source.length; index++) {
                storePrimitiveArrayByte(
                        allocation,
                        Math.toIntExact(byteOffset + index),
                        directPrimitiveElementSize,
                        source[index] & 0xff);
            }
        } else if (allocationCodecClassName == null) {
            for (int index = 0; index < source.length; index++) {
                storeByte(byteOffset + index, source[index] & 0xff);
            }
        } else {
            int consumed = 0;
            while (consumed < source.length) {
                long absoluteOffset = byteOffset + consumed;
                int elementIndex =
                        Math.toIntExact(Math.floorDiv(absoluteOffset, allocationElementSize));
                int withinElement = (int) Math.floorMod(absoluteOffset, allocationElementSize);
                Object current = readElement(elementIndex);
                MemoryCodec plan = isBuiltInCodec(allocationCodecClassName)
                        ? null : codecPlan(allocationCodecClassName);
                int chunk = Math.min(
                        source.length - consumed,
                        allocationElementSize - withinElement);
                if (plan != null && plan.isArrayCodecFor(current)) {
                    storeArrayCodecRange(
                            current,
                            withinElement,
                            source,
                            consumed,
                            chunk,
                            plan);
                    consumed += chunk;
                    continue;
                }
                // Decode built-in pointer carriers with their pointee codec
                // too. Rebuilding an address byte by byte loses its typed view
                // and cannot represent an intermediate dangling reference.
                byte[] image = encodeMemoryValue(
                        current, allocationElementSize, allocationCodecClassName);
                transferEncodedPointers(
                        allocation,
                        (long) elementIndex * allocationElementSize,
                        image,
                        0,
                        image.length,
                        false);
                transferEncodedPointers(
                        source,
                        consumed,
                        image,
                        withinElement,
                        Math.min(source.length - consumed, image.length - withinElement),
                        false);
                transferEncodedReferences(allocation, image);
                transferEncodedReferences(source, image);
                chunk = Math.min(source.length - consumed, image.length - withinElement);
                System.arraycopy(source, consumed, image, withinElement, chunk);
                Object decoded = decodeMemoryValue(image, 0, allocationElementSize,
                        allocationCodecClassName, current == null ? Object.class : current.getClass());
                discardEncodedReferences(image);
                writeElementPreservingIdentity(
                        elementIndex,
                        decoded);
                consumed += chunk;
            }
        }
        transferEncodedPointers(
                source, 0, allocation, byteOffset, source.length, false);
        moveEncodedReferences(source, allocation);
    }

    private void writeElement(int elementIndex, Object value) {
        if (allocation instanceof Cell) {
            if (elementIndex != 0) {
                throw new IndexOutOfBoundsException("pointer arithmetic escaped scalar storage");
            }
            // Reinitializing the same MIR local replaces its carrier while
            // cached nested projections still denote that storage location.
            discardProjectedFieldViews(allocation);
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
        if (allocation.getClass().getComponentType().isArray()) {
            long nestedElementSize = nestedPrimitiveArrayElementByteSize(allocation);
            if (nestedElementSize > allocationElementSize) {
                writeNestedPrimitiveArrayElement(
                        allocation,
                        Math.multiplyExact((long) elementIndex, allocationElementSize),
                        allocationElementSize,
                        value);
                return;
            }
        }
        arraySet(allocation, elementIndex, value);
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

    private static MemoryCodec codecPlan(String codecClassName) {
        if (codecClassName == null) {
            throw new IllegalStateException("pointer view has no aggregate codec");
        }
        CodecPlanCache recent = RECENT_CODEC_PLANS.get();
        MemoryCodec local = recent.get(codecClassName);
        if (local != null) {
            return local;
        }
        MemoryCodec cached = CODEC_METHODS.get(codecClassName);
        if (cached != null) {
            recent.remember(codecClassName, cached);
            return cached;
        }
        try {
            return rememberCodecPlan(codecClassName, MemoryCodec.load(codecClassName, RUNTIME_CLASS_LOADER), recent);
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not load Rust pointer codec " + codecClassName, error);
        }
    }

    private static MemoryCodec rememberCodecPlan(
            String name, MemoryCodec plan, CodecPlanCache recent) {
        MemoryCodec previous = CODEC_METHODS.putIfAbsent(name, plan);
        MemoryCodec result = previous == null ? plan : previous;
        recent.remember(name, result);
        return result;
    }

    private static Method[] scalarEnumMethods(Class<?> type) {
        Method[] cached = SCALAR_ENUM_METHODS.get(type);
        if (cached != null) {
            return cached;
        }
        Method[] methods = new Method[0];
        try {
            boolean hasPayload = PUBLIC_INSTANCE_FIELDS.get(type).length != 0;
            if (!hasPayload) {
                methods = scalarEnumMethodsOnCarrier(type);
            }
        } catch (SecurityException ignored) {
            // Reflection is unavailable for this carrier.
        }
        Method[] previous = SCALAR_ENUM_METHODS.putIfAbsent(type, methods);
        return previous == null ? methods : previous;
    }

    private static Method[] scalarEnumMethodsOnCarrier(Class<?> type) {
        for (Class<?> enumInterface : type.getInterfaces()) {
            try {
                Method discriminant = enumInterface.getMethod(
                        "_unionDiscriminant", enumInterface);
                Method fromDiscriminant = enumInterface.getMethod(
                        "_fromUnionDiscriminant", long.class);
                if (discriminant.getReturnType() == long.class
                        && Modifier.isStatic(discriminant.getModifiers())
                        && Modifier.isStatic(fromDiscriminant.getModifiers())) {
                    discriminant.setAccessible(true);
                    fromDiscriminant.setAccessible(true);
                    return new Method[] {discriminant, fromDiscriminant};
                }
            } catch (NoSuchMethodException ignored) {
                Method[] inherited = scalarEnumMethodsOnCarrier(enumInterface);
                if (inherited.length != 0) {
                    return inherited;
                }
            }
        }
        return new Method[0];
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
        if (targetObjects.length != 0) {
            if (sourceObjects.length == 0) {
                Arrays.fill(targetObjects, targetOffset, targetOffset + size, null);
            } else {
                System.arraycopy(sourceObjects, sourceOffset, targetObjects, targetOffset, size);
            }
        }
        transferEncodedPointers(
                sourceBytes, sourceOffset, targetBytes, targetOffset, size, false);
        transferEncodedReferences(sourceBytes, targetBytes);
    }

    public static Object[] emptyUnionObjectStorage() {
        return EMPTY_UNION_OBJECT_STORAGE;
    }

    private static byte[] encodeAggregate(String codecClassName, Object value) {
        try {
            return codecPlan(codecClassName).encode().encode(value);
        } catch (Throwable error) {
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
            return codecPlan(codecClassName).decode().decode(bytes);
        } catch (Throwable error) {
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
                Object result = enumMethods[0].invoke(null, value);
                return ((Number) result).longValue();
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

    private static boolean isRawPointerCodec(String codec) {
        if (codec == null || codec.length() < 2 || codec.charAt(0) != '@') {
            return false;
        }
        char family = codec.charAt(1);
        if (family == 'a') {
            return isArrayReferenceCodec(codec);
        }
        if (family != 'r') {
            return false;
        }
        return codec.length() == RAW_POINTER_VIEW_CODEC.length()
                ? RAW_POINTER_VIEW_CODEC.equals(codec)
                : codec.startsWith(RAW_POINTER_VIEW_CODEC + "\n");
    }

    private static boolean isArrayReferenceCodec(String codec) {
        return codec != null
                && codec.length() > 1
                && codec.charAt(0) == '@'
                && codec.charAt(1) == 'a'
                && codec.startsWith(ARRAY_REFERENCE_VIEW_CODEC_PREFIX);
    }

    private static String[] arrayReferenceDescriptor(String codec) {
        return splitCodecDescriptor(codec, ARRAY_REFERENCE_VIEW_CODEC_PREFIX, 3);
    }

    private static long arrayReferenceLength(String codec) {
        try {
            long length = Long.parseLong(arrayReferenceDescriptor(codec)[0]);
            if (length < 0) {
                throw new IllegalArgumentException("negative Rust fixed-array length");
            }
            return length;
        } catch (NumberFormatException error) {
            throw new IllegalArgumentException("invalid Rust fixed-array length", error);
        }
    }

    private static int arrayReferenceElementSize(String codec) {
        try {
            int size = Integer.parseInt(arrayReferenceDescriptor(codec)[1]);
            if (size < 0) {
                throw new IllegalArgumentException("negative Rust fixed-array element size");
            }
            return size;
        } catch (NumberFormatException error) {
            throw new IllegalArgumentException("invalid Rust fixed-array element size", error);
        }
    }

    private static String arrayReferenceElementCodec(String codec) {
        String elementCodec = arrayReferenceDescriptor(codec)[2];
        return elementCodec.isEmpty() ? null : elementCodec;
    }

    private static Pointer decodedRawPointer(long address, String codec) {
        return decodedRawPointer(typedPointerObjectFromAddress(address, codec), codec);
    }

    private static Pointer decodedRawPointer(Pointer pointer, String codec) {
        if (RAW_POINTER_VIEW_CODEC.equals(codec)) {
            return pointer;
        }
        if (isArrayReferenceCodec(codec)) {
            return pointer.retype(
                    arrayReferenceElementSize(codec), arrayReferenceElementCodec(codec));
        }
        pointer = pointer.retype(rawPointerPointeeSize(codec), rawPointerPointeeCodec(codec));
        String pointeeClass = rawPointerPointeeClass(codec);
        return pointeeClass.isEmpty() ? pointer : pointer.nominalManagedPointee(pointeeClass);
    }

    private static Object decodeArrayReference(long address, String codec, Class<?> targetClass) {
        return decodeArrayReference(
                typedPointerObjectFromAddress(address, codec), codec, targetClass);
    }

    private static Object decodeArrayReference(
            Pointer pointer, String codec, Class<?> targetClass) {
        Pointer data = decodedRawPointer(pointer, codec);
        return SliceView.create(targetClass, data, 0, arrayReferenceLength(codec));
    }

    private static long rawPointerPointeeSize(String codec) {
        if (isArrayReferenceCodec(codec)) {
            return Math.multiplyExact(
                    arrayReferenceLength(codec), (long) arrayReferenceElementSize(codec));
        }
        int sizeStart = RAW_POINTER_VIEW_CODEC.length() + 1;
        int codecStart = codec.indexOf('\n', sizeStart);
        if (codecStart < 0) {
            throw new IllegalArgumentException("invalid Rust raw-pointer codec descriptor");
        }
        long pointeeSize;
        try {
            pointeeSize = Long.parseLong(codec.substring(sizeStart, codecStart));
        } catch (NumberFormatException error) {
            throw new IllegalArgumentException("invalid Rust raw-pointer pointee size", error);
        }
        if (pointeeSize < 0) {
            throw new IllegalArgumentException("negative Rust raw-pointer pointee size");
        }
        return pointeeSize;
    }

    private static String rawPointerPointeeClass(String codec) {
        if (isArrayReferenceCodec(codec)) {
            return "";
        }
        int classStart = codec.indexOf('\n', RAW_POINTER_VIEW_CODEC.length() + 1);
        if (classStart < 0) {
            throw new IllegalArgumentException("invalid Rust raw-pointer codec descriptor");
        }
        int classEnd = codec.indexOf('\n', classStart + 1);
        if (classEnd < 0) {
            throw new IllegalArgumentException("invalid Rust raw-pointer codec descriptor");
        }
        return codec.substring(classStart + 1, classEnd);
    }

    private static String rawPointerPointeeCodec(String codec) {
        if (isArrayReferenceCodec(codec)) {
            return arrayReferenceElementCodec(codec);
        }
        int classStart = codec.indexOf('\n', RAW_POINTER_VIEW_CODEC.length() + 1);
        int codecStart = classStart < 0 ? -1 : codec.indexOf('\n', classStart + 1);
        if (codecStart < 0) {
            throw new IllegalArgumentException("invalid Rust raw-pointer codec descriptor");
        }
        String pointeeCodec = codec.substring(codecStart + 1);
        return pointeeCodec.isEmpty() ? null : pointeeCodec;
    }

    /**
     * Recovers an offset-zero nominal pointee after an address round trip
     * through a transparent JVM wrapper such as {@code Pin<&mut (T,)>}.
     */
    private Pointer nominalManagedPointee(String targetClassName) {
        // Copying a raw pointer must not dereference it: null, dangling,
        // one-past-end, and uninitialized pointees are all valid here. Only
        // inspect existing managed carriers to recover transparent wrappers.
        Object value = directCellValueOrSelf();
        if (value == this
                && allocation instanceof Object[]
                && allocationElementSize > 0
                && byteOffset >= 0
                && byteOffset % allocationElementSize == 0) {
            long index = byteOffset / allocationElementSize;
            Object[] elements = (Object[]) allocation;
            if (index < elements.length) {
                value = elements[(int) index];
            }
        }
        if (value == this || value == null) {
            return this;
        }
        try {
            Class<?> targetClass =
                    resolvedClass(targetClassName, value.getClass().getClassLoader());
            if (targetClass.isInstance(value)) {
                return this;
            }
            RustField match = null;
            for (RustField candidate : PUBLIC_INSTANCE_FIELDS.get(value.getClass())) {
                if (targetClass.isAssignableFrom(candidate.getType())) {
                    if (match != null) {
                        return this;
                    }
                    match = candidate;
                }
            }
            if (match == null) {
                return this;
            }
            return field(value, match.getName(), checkedArrayLength(viewSize), viewCodecClassName, false)
                    .withMetadata(metadata)
                    .inheritAddressOrigin(this, 0);
        } catch (ClassNotFoundException error) {
            throw new IllegalStateException(
                    "could not load Rust raw-pointer pointee " + targetClassName, error);
        }
    }

    private static Pointer rawPointerCarrier(Object value, String codec) {
        if (value instanceof Pointer) {
            return isArrayReferenceCodec(codec)
                    ? ((Pointer) value).retype(
                            arrayReferenceElementSize(codec),
                            arrayReferenceElementCodec(codec))
                    : (Pointer) value;
        }
        if (RAW_POINTER_VIEW_CODEC.equals(codec)) {
            throw new IllegalArgumentException(
                    "untyped Rust raw pointer requires a Pointer carrier");
        }

        long pointeeSize = rawPointerPointeeSize(codec);
        String pointeeCodec = rawPointerPointeeCodec(codec);
        try {
            long length;
            Pointer data;
            if (value != null && isSliceViewCarrierType(value.getClass())) {
                length = sliceLogicalLength(value);
                long elementSize = isArrayReferenceCodec(codec)
                        ? arrayReferenceElementSize(codec)
                        : length == 0 ? 0 : pointeeSize / length;
                if (length != 0 && elementSize * length != pointeeSize) {
                    throw new IllegalArgumentException(
                            "fixed-array reference has incompatible slice length");
                }
                data = fromSlice(
                        value,
                        elementSize,
                        isArrayReferenceCodec(codec) ? pointeeCodec : null);
            } else if (value != null && value.getClass().isArray()) {
                length = Array.getLength(value);
                long elementSize = isArrayReferenceCodec(codec)
                        ? arrayReferenceElementSize(codec)
                        : length == 0 ? 0 : pointeeSize / length;
                if (length != 0 && elementSize * length != pointeeSize) {
                    throw new IllegalArgumentException(
                            "fixed-array reference has incompatible JVM array length");
                }
                data = array(
                        value,
                        0,
                        elementSize,
                        isArrayReferenceCodec(codec) ? pointeeCodec : null);
            } else {
                throw new IllegalArgumentException(
                        "Rust raw pointer requires a Pointer or fixed-array view carrier");
            }
            // Thin references to fixed arrays retain no native length word,
            // but the JVM carrier needs that length to rebuild its SliceView.
            // Keep it as provenance metadata on the published element pointer;
            // ordinary raw-pointer decoding still applies the pointee view.
            return data.withMetadata(length);
        } catch (ReflectiveOperationException error) {
            throw new IllegalArgumentException("invalid Rust fixed-array reference", error);
        }
    }

    private static boolean isBuiltInCodec(String codec) {
        return isBigIntegerCodec(codec)
                || F128_CODEC.equals(codec)
                || isRawPointerCodec(codec)
                || isFatPointerCodec(codec);
    }

    private static boolean isGeneratedAggregateCodec(String codec) {
        return codec != null && !codec.isEmpty()
                && (codec.charAt(0) != '@' || codec.startsWith(ZERO_SIZED_CODEC_PREFIX));
    }

    private static boolean isPrimitiveScalarCarrier(Object value) {
        return value instanceof Boolean
                || value instanceof Byte
                || value instanceof Short
                || value instanceof Character
                || value instanceof Integer
                || value instanceof Long
                || value instanceof Float
                || value instanceof Double;
    }

    private static long valueBits(Object value, int size) {
        if (value == null) {
            return 0;
        }
        return incomingBits(value, size);
    }

    private static Object carrierFromBits(Object current, long bits, int size) {
        if (current == null) {
            if (size <= 4) {
                return Integer.valueOf((int) bits);
            }
            return Long.valueOf(bits);
        }
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
