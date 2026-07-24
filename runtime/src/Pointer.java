package org.rustlang.runtime;

import java.lang.ref.ReferenceQueue;
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
import java.nio.charset.StandardCharsets;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicLongArray;

public final class Pointer {
    public static void dropRustValue(Object value) {
        if (value instanceof RustDrop) {
            ((RustDrop) value).rustDrop();
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
    private static final String RAW_POINTER_VIEW_CODEC = "@raw-pointer";
    private static final String ARRAY_REFERENCE_VIEW_CODEC_PREFIX = "@array-reference\n";
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
    private static final Map<IdentityWeakReference, AllocationInfo> ALLOCATIONS = new HashMap<>();
    private static final ReferenceQueue<Object> ALLOCATION_INFO_QUEUE = new ReferenceQueue<>();
    private static final Map<Object, Boolean> ALLOCATOR_OWNED_ALLOCATIONS =
            new IdentityHashMap<>();
    private static final Map<String, byte[]> CONSTANT_ALLOCATIONS = new HashMap<>();
    private static final Map<String, Pointer> CONSTANT_CELLS = new HashMap<>();
    private static final Map<Long, ExposedTarget> EXPOSED_ADDRESSES = new HashMap<>();
    private static final Map<Long, Map<String, ExposedTarget>> TYPED_EXPOSED_ADDRESSES =
            new HashMap<>();
    private static final Map<Object, Set<Long>> ALLOCATION_EXPOSED_ADDRESSES =
            new IdentityHashMap<>();
    private static final NavigableMap<Long, AllocationRange> ALLOCATION_RANGES =
            new TreeMap<>();
    private static final ReferenceQueue<Object> ALLOCATION_RANGE_QUEUE =
            new ReferenceQueue<>();
    private static final ConcurrentHashMap<String, Method[]> CODEC_METHODS =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<String, String[]> CODEC_DESCRIPTORS =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<String, String> BINARY_CLASS_NAMES =
            new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Class<?>, Method[]> SCALAR_ENUM_METHODS =
            new ConcurrentHashMap<>();
    private static final Map<ClassLoader, ConcurrentHashMap<String, Class<?>>> RESOLVED_CLASSES =
            new IdentityHashMap<>();
    private static final ClassValue<ConcurrentHashMap<String, Field>> INSTANCE_FIELDS =
            new ClassValue<ConcurrentHashMap<String, Field>>() {
                @Override
                protected ConcurrentHashMap<String, Field> computeValue(Class<?> type) {
                    return new ConcurrentHashMap<>();
                }
            };
    private static final ClassValue<Field[]> PUBLIC_INSTANCE_FIELDS =
            new ClassValue<Field[]>() {
                @Override
                protected Field[] computeValue(Class<?> type) {
                    Field[] all = type.getFields();
                    int count = 0;
                    for (Field field : all) {
                        if (!Modifier.isStatic(field.getModifiers())) {
                            count++;
                        }
                    }
                    Field[] fields = new Field[count];
                    int index = 0;
                    for (Field field : all) {
                        if (!Modifier.isStatic(field.getModifiers())) {
                            field.setAccessible(true);
                            fields[index++] = field;
                        }
                    }
                    return fields;
                }
            };
    private static final ClassValue<Map<Integer, Constructor<?>>> PUBLIC_CONSTRUCTORS_BY_ARITY =
            new ClassValue<Map<Integer, Constructor<?>>>() {
                @Override
                protected Map<Integer, Constructor<?>> computeValue(Class<?> type) {
                    Map<Integer, Constructor<?>> constructors = new HashMap<>();
                    for (Constructor<?> constructor : type.getConstructors()) {
                        constructor.setAccessible(true);
                        constructors.putIfAbsent(constructor.getParameterCount(), constructor);
                    }
                    return constructors;
                }
            };
    private static final ClassValue<Constructor<?>> SLICE_VIEW_CONSTRUCTORS =
            new ClassValue<Constructor<?>>() {
                @Override
                protected Constructor<?> computeValue(Class<?> type) {
                    try {
                        Constructor<?> constructor =
                                type.getConstructor(Object.class, int.class, int.class);
                        constructor.setAccessible(true);
                        return constructor;
                    } catch (NoSuchMethodException error) {
                        throw new IllegalStateException(
                                "Rust slice view has no array/offset/length constructor", error);
                    }
                }
            };
    private static final ClassValue<Constructor<?>> LONG_SLICE_VIEW_CONSTRUCTORS =
            new ClassValue<Constructor<?>>() {
                @Override
                protected Constructor<?> computeValue(Class<?> type) {
                    try {
                        Constructor<?> constructor =
                                type.getConstructor(Object.class, int.class, long.class);
                        constructor.setAccessible(true);
                        return constructor;
                    } catch (NoSuchMethodException error) {
                        throw new IllegalStateException(
                                "Rust slice view has no long-length constructor", error);
                    }
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
                    Field[] fields = PUBLIC_INSTANCE_FIELDS.get(type);
                    Constructor<?> constructor = constructorWithArity(type, fields.length);
                    Class<?>[] parameterTypes = constructor.getParameterTypes();
                    Object[] defaults = new Object[parameterTypes.length];
                    for (int index = 0; index < parameterTypes.length; index++) {
                        defaults[index] = defaultValue(parameterTypes[index]);
                    }
                    return new ManagedCopyPlan(fields, constructor, defaults);
                }
            };
    private static final Map<Object, Long> MANAGED_OBJECT_ADDRESSES = new IdentityHashMap<>();
    private static final Map<Long, WeakReference<Object>> MANAGED_OBJECTS = new HashMap<>();
    private static final Map<String, byte[]> JAVA_STRING_UTF8 = new IdentityHashMap<>();
    private static final Map<String, Pointer> TRAIT_METADATA_MARKERS = new HashMap<>();
    private static final ConcurrentHashMap<Long, TraitMetadataInfo> TRAIT_METADATA_INFO =
            new ConcurrentHashMap<>();
    private static final int STATE_STRIPE_COUNT = 64;
    private static final int LAZY_ARRAY_REPEAT_THRESHOLD = 2;
    private static final int REPEATED_ARRAY_FILTER_WORDS = 1 << 16;
    private static final AtomicLongArray REPEATED_ARRAY_FILTER =
            new AtomicLongArray(REPEATED_ARRAY_FILTER_WORDS);
    private static final AtomicLongArray STRUCTURAL_VIEW_FILTER =
            new AtomicLongArray(REPEATED_ARRAY_FILTER_WORDS);
    private static final AtomicLongArray MEMORY_VIEW_FILTER =
            new AtomicLongArray(REPEATED_ARRAY_FILTER_WORDS);
    private static final AtomicLongArray MEMORY_VIEW_ORIGIN_FILTER =
            new AtomicLongArray(REPEATED_ARRAY_FILTER_WORDS);
    private static final AtomicLongArray ENCODED_REFERENCE_FILTER =
            new AtomicLongArray(REPEATED_ARRAY_FILTER_WORDS);
    private static final Map<Object, Map<Long, StructuralViewState>>[] STRUCTURAL_VIEWS =
            createWeakMapStripes();
    private static final Map<Object, NavigableMap<Long, MemoryViewState>>[] MEMORY_VIEWS =
            createWeakMapStripes();
    private static final Map<Object, MemoryViewOrigin>[] MEMORY_VIEW_ORIGINS =
            createWeakMapStripes();
    private static final Map<Object, Map<Object, Boolean>>[] MEMORY_ORIGIN_VIEWS =
            createWeakMapStripes();
    private static final Map<Object, RepeatedArrayState>[] REPEATED_ARRAYS =
            createWeakMapStripes();
    private static final Map<Object, Object>[] ENCODED_REFERENCES =
            createWeakMapStripes();
    private static final ThreadLocal<Integer> MEMORY_VIEW_WRITEBACK_DEPTH =
            ThreadLocal.withInitial(() -> 0);
    private static final Map<Object, Map<String, WeakReference<FieldCell>>>[] FIELD_CELLS =
            createWeakMapStripes();
    private static final int ATOMIC_STRIPE_COUNT = 64;
    private static final int ATOMIC_RELAXED = 0;
    private static final int ATOMIC_RELEASE = 1;
    private static final int ATOMIC_ACQUIRE = 2;
    private static final int ATOMIC_ACQ_REL = 3;
    private static final int ATOMIC_SEQ_CST = 4;
    private static final Object[] ATOMIC_STRIPES = createAtomicStripes();
    private static final Object ATOMIC_SEQUENCE_LOCK = new Object();
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
        int hash = key == null ? 0 : System.identityHashCode(key);
        hash ^= hash >>> 16;
        return stripes[hash & (stripes.length - 1)];
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
        markIdentityFilter(ENCODED_REFERENCE_FILTER, owner);
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
        }
    }

    private static void transferEncodedReferences(Object sourceOwner, Object targetOwner) {
        sourceOwner = encodedReferenceOwner(sourceOwner);
        targetOwner = encodedReferenceOwner(targetOwner);
        if (sourceOwner == null || targetOwner == null || sourceOwner == targetOwner) {
            return;
        }
        if (!mayBeInIdentityFilter(ENCODED_REFERENCE_FILTER, sourceOwner)) {
            return;
        }
        Map<Object, Object> sourceStripe =
                stateStripe(ENCODED_REFERENCES, sourceOwner);
        Object referenced;
        Object[] referencedSet = null;
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
        if (!mayBeInIdentityFilter(ENCODED_REFERENCE_FILTER, owner)) {
            return;
        }
        Map<Object, Object> stripe =
                stateStripe(ENCODED_REFERENCES, owner);
        synchronized (stripe) {
            stripe.remove(owner);
        }
    }

    private static final class EncodedReferenceSet {
        private final IdentityHashMap<Object, Boolean> allocations = new IdentityHashMap<>();

        private EncodedReferenceSet(Object first) {
            allocations.put(first, Boolean.TRUE);
        }
    }

    private static final class IdentityWeakReference extends WeakReference<Object> {
        private final int identityHash;

        private IdentityWeakReference(Object value, ReferenceQueue<Object> queue) {
            super(value, queue);
            identityHash = System.identityHashCode(value);
        }

        @Override
        public int hashCode() {
            return identityHash;
        }

        @Override
        public boolean equals(Object other) {
            if (this == other) {
                return true;
            }
            if (!(other instanceof IdentityWeakReference)) {
                return false;
            }
            Object value = get();
            return value != null && value == ((IdentityWeakReference) other).get();
        }
    }

    private static final class WeakIdentityMap<V> extends AbstractMap<Object, V> {
        private static final class Entry<V> extends WeakReference<Object> {
            private final int identityHash;
            private V value;
            private Entry<V> next;

            private Entry(
                    Object key, V value, ReferenceQueue<Object> queue, Entry<V> next) {
                super(key, queue);
                identityHash = System.identityHashCode(key);
                this.value = value;
                this.next = next;
            }
        }

        private final ReferenceQueue<Object> queue = new ReferenceQueue<>();
        private Entry<V>[] buckets = newTable(16);
        private int size;

        @SuppressWarnings("unchecked")
        private static <V> Entry<V>[] newTable(int length) {
            return (Entry<V>[]) new Entry<?>[length];
        }

        private static int bucketIndex(int hash, int length) {
            hash ^= hash >>> 16;
            return hash & (length - 1);
        }

        private void removeEntry(Entry<V> target) {
            int index = bucketIndex(target.identityHash, buckets.length);
            Entry<V> previous = null;
            for (Entry<V> entry = buckets[index]; entry != null; entry = entry.next) {
                if (entry == target) {
                    if (previous == null) {
                        buckets[index] = entry.next;
                    } else {
                        previous.next = entry.next;
                    }
                    entry.next = null;
                    size--;
                    return;
                }
                previous = entry;
            }
        }

        private void discardCollected(int limit) {
            for (int count = 0; count < limit; count++) {
                @SuppressWarnings("unchecked")
                Entry<V> reference = (Entry<V>) queue.poll();
                if (reference == null) {
                    return;
                }
                removeEntry(reference);
            }
        }

        private void resize() {
            Entry<V>[] oldBuckets = buckets;
            buckets = newTable(oldBuckets.length << 1);
            for (Entry<V> bucket : oldBuckets) {
                Entry<V> entry = bucket;
                while (entry != null) {
                    Entry<V> next = entry.next;
                    int index = bucketIndex(entry.identityHash, buckets.length);
                    entry.next = buckets[index];
                    buckets[index] = entry;
                    entry = next;
                }
            }
        }

        @Override
        public V get(Object key) {
            discardCollected(8);
            int hash = System.identityHashCode(key);
            int index = bucketIndex(hash, buckets.length);
            for (Entry<V> entry = buckets[index]; entry != null; entry = entry.next) {
                if (entry.identityHash == hash && entry.get() == key) {
                    return entry.value;
                }
            }
            return null;
        }

        @Override
        public V put(Object key, V value) {
            discardCollected(8);
            int hash = System.identityHashCode(key);
            int index = bucketIndex(hash, buckets.length);
            for (Entry<V> entry = buckets[index]; entry != null; entry = entry.next) {
                if (entry.identityHash == hash && entry.get() == key) {
                    V previous = entry.value;
                    entry.value = value;
                    return previous;
                }
            }
            buckets[index] = new Entry<>(key, value, queue, buckets[index]);
            size++;
            if (size * 4 >= buckets.length * 3) {
                resize();
            }
            return null;
        }

        @Override
        public V remove(Object key) {
            discardCollected(8);
            int hash = System.identityHashCode(key);
            int index = bucketIndex(hash, buckets.length);
            Entry<V> previous = null;
            for (Entry<V> entry = buckets[index]; entry != null; entry = entry.next) {
                if (entry.identityHash == hash && entry.get() == key) {
                    if (previous == null) {
                        buckets[index] = entry.next;
                    } else {
                        previous.next = entry.next;
                    }
                    entry.next = null;
                    size--;
                    return entry.value;
                }
                previous = entry;
            }
            return null;
        }

        @Override
        public Set<Map.Entry<Object, V>> entrySet() {
            discardCollected(Integer.MAX_VALUE);
            Set<Map.Entry<Object, V>> entries = new HashSet<>();
            for (Entry<V> bucket : buckets) {
                for (Entry<V> entry = bucket; entry != null; entry = entry.next) {
                    Object key = entry.get();
                    if (key != null) {
                        entries.add(new java.util.AbstractMap.SimpleImmutableEntry<>(
                                key, entry.value));
                    }
                }
            }
            return entries;
        }
    }

    /** Must be called while holding {@link #ALLOCATIONS}. */
    private static void discardCollectedAllocationInfo(int limit) {
        for (int count = 0; count < limit; count++) {
            IdentityWeakReference reference =
                    (IdentityWeakReference) ALLOCATION_INFO_QUEUE.poll();
            if (reference == null) {
                return;
            }
            ALLOCATIONS.remove(reference);
        }
    }

    /** Must be called while holding {@link #ALLOCATIONS}. */
    private static AllocationInfo allocationInfo(Object allocation) {
        discardCollectedAllocationInfo(8);
        IdentityWeakReference lookup = new IdentityWeakReference(allocation, null);
        AllocationInfo info = ALLOCATIONS.get(lookup);
        if (info == null) {
            info = new AllocationInfo();
            ALLOCATIONS.put(
                    new IdentityWeakReference(allocation, ALLOCATION_INFO_QUEUE), info);
        }
        return info;
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
        private final Field[] fields;
        private final Constructor<?> constructor;
        private final Object[] defaults;

        private ManagedCopyPlan(
                Field[] fields, Constructor<?> constructor, Object[] defaults) {
            this.fields = fields;
            this.constructor = constructor;
            this.defaults = defaults;
        }
    }

    private static final class RepeatedArrayState {
        private final Object template;

        private RepeatedArrayState(Object template) {
            this.template = template;
        }
    }

    private static Constructor<?> constructorWithArity(Class<?> type, int arity) {
        Constructor<?> constructor = PUBLIC_CONSTRUCTORS_BY_ARITY.get(type).get(arity);
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
                        Class<?> owner = resolvedRuntimeClass(ownerClassName);
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
            PanicSupport.abortIfStackOverflow(error);
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
                transferEncodedReferences(value, copy);
            } else {
                Object[] sourceElements = (Object[]) value;
                Object[] copyElements = (Object[]) copy;
                for (int index = 0; index < length; index++) {
                    copyElements[index] = copyManagedValue(sourceElements[index]);
                }
            }
            return copy;
        }
        if (isManagedValueImmutable(value, valueClass)) {
            return value;
        }

        try {
            ManagedCopyPlan plan = MANAGED_COPY_PLANS.get(valueClass);
            Object copy = plan.constructor.newInstance(plan.defaults);
            for (Field field : plan.fields) {
                field.set(copy, copyManagedValue(field.get(value)));
            }
            return copy;
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not copy managed Rust value", error);
        }
    }

    private static boolean isManagedValueImmutable(Object value, Class<?> valueClass) {
        return valueClass.isPrimitive()
                || value instanceof Number
                || value instanceof Boolean
                || value instanceof Character
                || value instanceof String
                || valueClass.isEnum()
                || valueClass.getName().startsWith("org.rustlang.runtime.")
                || isRustFunctionPointer(valueClass);
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
            Array.set(array, index, copyValue ? copyManagedValue(value) : value);
        }
    }

    private static Object independentRepeatedArrayElement(Object array, int index) {
        if (Array.getLength(array) < LAZY_ARRAY_REPEAT_THRESHOLD
                || !mayBeRepeatedArray(array)) {
            return Array.get(array, index);
        }
        Map<Object, RepeatedArrayState> stripe = stateStripe(REPEATED_ARRAYS, array);
        synchronized (stripe) {
            RepeatedArrayState state = stripe.get(array);
            Object value = Array.get(array, index);
            if (state == null || value != state.template) {
                return value;
            }
            Object copy = copyManagedValue(value);
            Array.set(array, index, copy);
            return copy;
        }
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

    private static void markFilterHash(AtomicLongArray filter, int hash) {
        int bitIndex = hash & (REPEATED_ARRAY_FILTER_WORDS * Long.SIZE - 1);
        int wordIndex = bitIndex >>> 6;
        long bit = 1L << bitIndex;
        while (true) {
            long previous = filter.get(wordIndex);
            if ((previous & bit) != 0
                    || filter.compareAndSet(wordIndex, previous, previous | bit)) {
                return;
            }
        }
    }

    private static boolean hasFilterHash(AtomicLongArray filter, int hash) {
        int bitIndex = hash & (REPEATED_ARRAY_FILTER_WORDS * Long.SIZE - 1);
        return (filter.get(bitIndex >>> 6) & (1L << bitIndex)) != 0;
    }

    /** Reads one reference-array element while preserving Rust array value semantics. */
    public static Object arrayGetObject(Object array, int index) {
        return independentRepeatedArrayElement(array, index);
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
            if (elementCodec != null) {
                moveEncodedReferences(element, bytes);
            }
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
        } else if (isRawPointerCodec(codec)) {
            bits = encodedAddress(rawPointerCarrier(value, codec), encoded, codec);
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
            if (isArrayReferenceCodec(codec)) {
                return decodeArrayReference(bits, codec, componentType);
            }
            return decodedRawPointer(bits, codec);
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
            dataAddress = encodedAddress(data, image, codec);
            try {
                pointerMetadata = sliceLogicalLength(value);
            } catch (ReflectiveOperationException error) {
                throw new IllegalArgumentException("invalid Rust slice fat pointer", error);
            }
        } else if (codec.startsWith(STRUCT_TAIL_POINTER_VIEW_CODEC_PREFIX)) {
            if (!(value instanceof Pointer)) {
                throw new IllegalArgumentException(
                        "Rust struct-tail pointer requires a Pointer carrier");
            }
            Pointer pointer = (Pointer) value;
            dataAddress = encodedAddress(pointer, image, codec);
            pointerMetadata = pointer.metadata();
        } else {
            if (!(value instanceof Pointer)) {
                throw new IllegalArgumentException(
                        "Rust raw trait-object pointer requires a Pointer carrier");
            }
            Pointer pointer = (Pointer) value;
            String metadataClass = codec.substring(TRAIT_POINTER_VIEW_CODEC_PREFIX.length());
            Pointer marker = traitMetadataMarker(pointer, metadataClass);
            dataAddress = erasedAddress(pointer);
            pointerMetadata = marker.address();
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
            Pointer data =
                    typedPointerObjectFromAddress(dataAddress, codec).retype(elementSize, elementCodec);
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
        Pointer data =
                typedPointerObjectFromAddress(dataAddress, codec).retype(elementSize, elementCodec);
        try {
            Class<?> viewClass = resolvedRuntimeClass(descriptor[0]);
            return LONG_SLICE_VIEW_CONSTRUCTORS
                    .get(viewClass)
                    .newInstance(data, 0, pointerMetadata);
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not reconstruct Rust slice fat pointer", error);
        }
    }

    /** Writes a JVM fat-pointer carrier using Rust's native two-word layout. */
    public static void encodeFatPointerMemory(
            Object value, byte[] bytes, int offset, int size, String codec) {
        byte[] image = encodeFatPointer(value, size, codec);
        System.arraycopy(image, 0, bytes, offset, size);
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
        return codecClassName != null
                && (codecClassName.startsWith(STRUCTURAL_VIEW_CODEC_PREFIX)
                        || codecClassName.startsWith(STRUCT_TAIL_VIEW_CODEC_PREFIX));
    }

    private static Class<?> resolvedClass(String className, ClassLoader loader)
            throws ClassNotFoundException {
        String binaryName = binaryClassName(className);
        if (loader == null) {
            loader = Pointer.class.getClassLoader();
        }
        ConcurrentHashMap<String, Class<?>> classes;
        synchronized (RESOLVED_CLASSES) {
            classes = RESOLVED_CLASSES.get(loader);
            if (classes == null) {
                classes = new ConcurrentHashMap<>();
                RESOLVED_CLASSES.put(loader, classes);
            }
        }
        Class<?> cached = classes.get(binaryName);
        if (cached != null) {
            return cached;
        }
        Class<?> resolved = Class.forName(binaryName, true, loader);
        Class<?> previous = classes.putIfAbsent(binaryName, resolved);
        return previous == null ? resolved : previous;
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

    private static Class<?> resolvedRuntimeClass(String className)
            throws ClassNotFoundException {
        return resolvedClass(className, Pointer.class.getClassLoader());
    }

    private static Field instanceField(Class<?> owner, String name)
            throws NoSuchFieldException {
        ConcurrentHashMap<String, Field> fields = INSTANCE_FIELDS.get(owner);
        Field cached = fields.get(name);
        if (cached != null) {
            return cached;
        }
        Field field = owner.getField(name);
        if (Modifier.isStatic(field.getModifiers())) {
            throw new NoSuchFieldException(owner.getName() + "." + name + " is static");
        }
        field.setAccessible(true);
        Field previous = fields.putIfAbsent(name, field);
        return previous == null ? field : previous;
    }

    private static long sliceLogicalLength(Object slice)
            throws ReflectiveOperationException {
        return instanceField(slice.getClass(), "rustLength").getLong(slice);
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
            Object directBacking = ((Pointer) backing).backingArray();
            if (directBacking != null
                    && directBacking.getClass().isArray()
                    && (targetArrayType.getComponentType().isAssignableFrom(
                                    directBacking.getClass().getComponentType())
                            || targetArrayType.getComponentType()
                                    == directBacking.getClass().getComponentType())) {
                if (offset == 0
                        && length == Array.getLength(directBacking)
                        && targetArrayType.isInstance(directBacking)) {
                    return directBacking;
                }
                Object result = Array.newInstance(targetArrayType.getComponentType(), length);
                System.arraycopy(directBacking, offset, result, 0, length);
                transferEncodedReferences(directBacking, result);
                return result;
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
                Array.set(result, index, elementValue);
            }
            transferEncodedReferences(backing, result);
            return result;
        } catch (ReflectiveOperationException error) {
            throw new IllegalArgumentException("invalid Rust fixed-array view", error);
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
        if (isSliceViewCarrierType(targetType) && value.getClass().isArray()) {
            Constructor<?> constructor = SLICE_VIEW_CONSTRUCTORS.get(targetType);
            return constructor.newInstance(value, 0, Array.getLength(value));
        }
        if (targetType.isArray() && isSliceViewCarrierType(value.getClass())) {
            return sliceBackingForArray(value, targetType);
        }
        return constructStructuralView(value, targetType);
    }

    private static Constructor<?> structuralConstructor(Class<?> targetClass, int fieldCount) {
        return constructorWithArity(targetClass, fieldCount);
    }

    private static Object constructStructuralView(Object source, Class<?> targetClass) {
        return constructStructuralView(source, targetClass, null);
    }

    private static Object constructStructuralView(
            Object source, Class<?> targetClass, Object traitTailCarrier) {
        try {
            Object transparentInner = null;
            for (Field field : PUBLIC_INSTANCE_FIELDS.get(source.getClass())) {
                if (transparentInner != null) {
                    transparentInner = null;
                    break;
                }
                transparentInner = field.get(source);
            }
            if (transparentInner != null && targetClass.isInstance(transparentInner)) {
                return transparentInner;
            }
            Field[] targetFields = PUBLIC_INSTANCE_FIELDS.get(targetClass);
            Constructor<?> constructor = structuralConstructor(targetClass, targetFields.length);
            Object[] args = new Object[constructor.getParameterCount()];
            java.lang.reflect.Parameter[] parameters = constructor.getParameters();
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
                        Field sourceField = instanceField(source.getClass(), fieldName);
                        args[index] = adaptStructuralField(
                                sourceField.get(source), parameters[index].getType());
                    } catch (NoSuchFieldException error) {
                        if (parameters.length != 1) {
                            throw error;
                        }
                        // Transparent DST wrappers can add more than one nominal
                        // layer around the same slice tail (for example OsStr).
                        args[index] = adaptStructuralField(source, parameters[index].getType());
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
            Field[] targetFields = PUBLIC_INSTANCE_FIELDS.get(target.getClass());
            if (targetFields.length == 1 && targetFields[0].getType().isInstance(source)) {
                targetFields[0].set(target, source);
                return;
            }
            Field[] sourceFields = PUBLIC_INSTANCE_FIELDS.get(source.getClass());
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
        synchronized (stripe) {
            Map<Long, StructuralViewState> allocationViews = stripe.get(allocation);
            if (allocationViews == null) {
                if (!create) {
                    return null;
                }
                allocationViews = new HashMap<>();
                stripe.put(allocation, allocationViews);
            }
            StructuralViewState state = allocationViews.get(byteOffset);
            if (state == null && create) {
                markStructuralView(allocation);
                state = new StructuralViewState(source);
                allocationViews.put(byteOffset, state);
            }
            return state;
        }
    }

    private static void markStructuralView(Object allocation) {
        markIdentityFilter(STRUCTURAL_VIEW_FILTER, allocation);
    }

    private static boolean mayHaveStructuralView(Object allocation) {
        return mayBeInIdentityFilter(STRUCTURAL_VIEW_FILTER, allocation);
    }

    private static void markIdentityFilter(AtomicLongArray filter, Object value) {
        int hash = System.identityHashCode(value);
        markFilterHash(filter, mixRepeatedArrayHash(hash));
        markFilterHash(filter, mixRepeatedArrayHash(hash ^ 0x9e37_79b9));
    }

    private static boolean mayBeInIdentityFilter(AtomicLongArray filter, Object value) {
        int hash = System.identityHashCode(value);
        return hasFilterHash(filter, mixRepeatedArrayHash(hash))
                && hasFilterHash(filter, mixRepeatedArrayHash(hash ^ 0x9e37_79b9));
    }

    private void clearStructuralViewState() {
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
        if (!mayBeInIdentityFilter(MEMORY_VIEW_FILTER, allocation)) {
            return;
        }
        java.util.List<Map.Entry<Long, MemoryViewState>> pending;
        Map<Object, NavigableMap<Long, MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (stripe) {
            NavigableMap<Long, MemoryViewState> views = stripe.get(allocation);
            if (views == null) {
                return;
            }
            pending = removeOverlappingMemoryViews(views, offset, size);
            if (views.isEmpty()) {
                stripe.remove(allocation);
            }
        }
        for (Map.Entry<Long, MemoryViewState> entry : pending) {
            writeBackMemoryView(entry.getKey(), entry.getValue());
        }
    }

    private void flushAllMemoryViews() {
        if (!mayBeInIdentityFilter(MEMORY_VIEW_FILTER, allocation)) {
            return;
        }
        java.util.List<Map.Entry<Long, MemoryViewState>> pending;
        Map<Object, NavigableMap<Long, MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (stripe) {
            NavigableMap<Long, MemoryViewState> views = stripe.remove(allocation);
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
        if (!mayBeInIdentityFilter(MEMORY_VIEW_FILTER, allocation)) {
            return;
        }
        Map<Object, NavigableMap<Long, MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (stripe) {
            NavigableMap<Long, MemoryViewState> views = stripe.get(allocation);
            if (views == null) {
                return;
            }
            removeOverlappingMemoryViews(views, offset, size);
            if (views.isEmpty()) {
                stripe.remove(allocation);
            }
        }
    }

    /**
     * Removes decoded views overlapping one range. Views are kept disjoint when
     * inserted, so the predecessor and entries beginning before the range end
     * are the only possible matches.
     */
    private static java.util.List<Map.Entry<Long, MemoryViewState>>
            removeOverlappingMemoryViews(
                    NavigableMap<Long, MemoryViewState> views, long offset, int size) {
        java.util.List<Map.Entry<Long, MemoryViewState>> removed =
                new java.util.ArrayList<>();
        long end = Math.addExact(offset, (long) size);
        Map.Entry<Long, MemoryViewState> entry = views.floorEntry(offset);
        if (entry == null) {
            entry = views.ceilingEntry(offset);
        }
        while (entry != null && entry.getKey() < end) {
            long entryOffset = entry.getKey();
            MemoryViewState state = entry.getValue();
            Map.Entry<Long, MemoryViewState> next = views.higherEntry(entryOffset);
            if (rangesOverlap(offset, size, entryOffset, state.size)) {
                removed.add(new java.util.AbstractMap.SimpleImmutableEntry<>(
                        entryOffset, state));
                views.remove(entryOffset);
            }
            entry = next;
        }
        return removed;
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
        Map<Object, NavigableMap<Long, MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        if (mayBeInIdentityFilter(MEMORY_VIEW_FILTER, allocation)) {
            synchronized (stripe) {
                NavigableMap<Long, MemoryViewState> views = stripe.get(allocation);
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
        markIdentityFilter(MEMORY_VIEW_FILTER, allocation);
        synchronized (stripe) {
            stripe.computeIfAbsent(allocation, ignored -> new TreeMap<>())
                    .put(byteOffset, state);
        }
        boundMemoryViewState = state;
        registerMemoryViewOrigin(decoded);
        bindDecodedMemoryView(decoded);
        return decoded;
    }

    private void registerMemoryViewOrigin(Object value) {
        if (value == null || allocation == null) {
            return;
        }
        markIdentityFilter(MEMORY_VIEW_ORIGIN_FILTER, value);
        MemoryViewOrigin previous;
        Map<Object, MemoryViewOrigin> stripe = stateStripe(MEMORY_VIEW_ORIGINS, value);
        synchronized (stripe) {
            previous = stripe.get(value);
            if (previous != null && previous.matches(this)) {
                return;
            }
            previous = stripe.put(value, new MemoryViewOrigin(this));
        }
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
            reverseStripe
                    .computeIfAbsent(allocation, ignored -> new WeakIdentityMap<>())
                    .put(value, Boolean.TRUE);
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
            }
        }
    }

    private static void discardMemoryOriginsForAllocation(Object allocation) {
        Map<Object, Map<Object, Boolean>> reverseStripe =
                stateStripe(MEMORY_ORIGIN_VIEWS, allocation);
        java.util.List<Object> views;
        synchronized (reverseStripe) {
            Map<Object, Boolean> indexed = reverseStripe.remove(allocation);
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
        Method[] methods = codecMethods(viewCodecClassName);
        if (methods.length < 3 || methods[2] == null) {
            return;
        }
        try {
            methods[2].invoke(null, this, value);
        } catch (InvocationTargetException error) {
            rethrowUnchecked(error.getCause());
        } catch (ReflectiveOperationException | IllegalArgumentException error) {
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
        Pointer nested = byte_offset(relativeOffset).retype(size, codecClassName);
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
            Object value = Array.get(array, index);
            if (value == null) {
                continue;
            }
            Pointer element = byte_offset(Math.multiplyExact((long) index, elementSize))
                    .retype(elementSize, elementCodecClassName);
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
        Map<Object, NavigableMap<Long, MemoryViewState>> viewStripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (viewStripe) {
            NavigableMap<Long, MemoryViewState> views = viewStripe.get(allocation);
            Map.Entry<Long, MemoryViewState> enclosing =
                    views == null ? null : views.floorEntry(origin.byteOffset);
            stateOffset = enclosing == null ? -1 : enclosing.getKey();
            state = enclosing == null ? null : enclosing.getValue();
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
        pointer.boundMemoryViewState = state;
        pointer.commitMemoryView();
    }

    /** Commits direct field mutations made through the current decoded view. */
    public void commitMemoryView() {
        if (allocation instanceof ReceiverCell && boundMemoryViewState == null) {
            return;
        }
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
        Map<Object, NavigableMap<Long, MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (stripe) {
            NavigableMap<Long, MemoryViewState> views = stripe.get(allocation);
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
        synchronized (stripe) {
            NavigableMap<Long, MemoryViewState> views =
                    stripe.computeIfAbsent(allocation, ignored -> new TreeMap<>());
            if (!views.containsKey(byteOffset)) {
                views.put(byteOffset, state);
            }
        }
        registerMemoryViewOrigin(state.value);
        bindDecodedMemoryView(state.value);
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
        Map<Object, Map<String, WeakReference<FieldCell>>> fieldStripe =
                stateStripe(FIELD_CELLS, owner);
        synchronized (fieldStripe) {
            Map<String, WeakReference<FieldCell>> fields = fieldStripe.get(owner);
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
        for (FieldCell cell : projected) {
            Map<Object, NavigableMap<Long, MemoryViewState>> stripe =
                    stateStripe(MEMORY_VIEWS, cell);
            synchronized (stripe) {
                stripe.remove(cell);
            }
        }
        for (FieldCell cell : projected) {
            Map<Object, Map<Long, StructuralViewState>> stripe =
                    stateStripe(STRUCTURAL_VIEWS, cell);
            synchronized (stripe) {
                stripe.remove(cell);
            }
        }
        for (FieldCell cell : projected) {
            discardMemoryOriginsForAllocation(cell);
        }
    }

    private static boolean hasProjectedFieldCells(Object owner) {
        if (owner == null) {
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
            for (Field field : PUBLIC_INSTANCE_FIELDS.get(owner.getClass())) {
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
        Object managed = managedViewObject();
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
        Pointer data = byte_offset(prefixSize).retype(elementSize, elementCodec);
        try {
            Class<?> viewClass = resolvedRuntimeClass(descriptor[2]);
            return LONG_SLICE_VIEW_CONSTRUCTORS
                    .get(viewClass)
                    .newInstance(data, 0, metadata());
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
        return binaryClassName(name);
    }

    private Object structuralViewObject() {
        String targetClassName = structuralTargetClassName();
        Object source = structuralSourceObject();
        try {
            ClassLoader loader = source.getClass().getClassLoader();
            Class<?> targetClass = resolvedClass(targetClassName, loader);
            return structuralViewState(source, true).activate(targetClass, traitMetadataCarrier);
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
            for (Field field : PUBLIC_INSTANCE_FIELDS.get(value.getClass())) {
                if (field.getType().isPrimitive()) {
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

        private Object owner() {
            return owner;
        }

        private Object get() {
            try {
                return field.get(owner());
            } catch (IllegalAccessException error) {
                throw new IllegalStateException("could not read Rust field pointer", error);
            }
        }

        private void set(Object value) {
            try {
                field.set(owner(), value);
            } catch (IllegalAccessException error) {
                throw new IllegalStateException("could not write Rust field pointer", error);
            }
            commitOriginMemoryView(owner());
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
            this.addressOrigin = addressOrigin;
            this.addressOriginOffset = addressOriginOffset;
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
                    && zeroSizedSourceViewSize == pointer.zeroSizedSourceViewSize
                    && java.util.Objects.equals(
                            zeroSizedSourceViewCodecClassName,
                            pointer.zeroSizedSourceViewCodecClassName)
                    && traitObjectCarrier == pointer.traitObjectCarrier
                    && traitMetadataCarrier == pointer.traitMetadataCarrier
                    && traitMetadataMarker == pointer.traitMetadataMarker
                    && traitPointeeSize == pointer.traitPointeeSize
                    && traitPointeeAlignment == pointer.traitPointeeAlignment
                    && java.util.Objects.equals(
                            traitAdapterClassName, pointer.traitAdapterClassName)
                    && addressOrigin == pointer.addressOrigin
                    && addressOriginOffset == pointer.addressOriginOffset;
        }
    }

    private final Object allocation;
    private final int allocationElementSize;
    private final long byteOffset;
    private final long viewSize;
    private final String allocationCodecClassName;
    private final String viewCodecClassName;
    private final long exposedAddress;
    private Pointer addressOrigin;
    private long addressOriginOffset;
    private volatile long publishedAddress = Long.MIN_VALUE;
    private volatile ExposedTarget publishedTarget;
    private MemoryViewState boundMemoryViewState;
    private long erasedAddressToken = -1;
    private long metadata = -1;
    private long zeroSizedSourceViewSize = -1;
    private String zeroSizedSourceViewCodecClassName;
    private Object traitObjectCarrier;
    private Object traitMetadataCarrier;
    private Pointer traitMetadataMarker;
    private long traitPointeeSize = -1;
    private long traitPointeeAlignment = -1;
    private String traitAdapterClassName;
    private String traitPointeeCodecClassName;

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
        recordAlignment(cell, alignment);
        Pointer pointer = new Pointer(cell, size, 0, size, codecClassName);
        long metadata = inferredStructuralMetadata(value);
        return metadata < 0 ? pointer : pointer.withMetadata(metadata);
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
        if (origin != null
                && origin.viewSize == size
                && activeMemoryViewMatches(value, origin)) {
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
        recordAlignment(cell, alignment);
        Pointer pointer = new Pointer(cell, size, 0, size, codecClassName);
        long metadata = inferredStructuralMetadata(value);
        return metadata < 0 ? pointer : pointer.withMetadata(metadata);
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
        if (owner == null) {
            throw new NullPointerException("Rust field pointer requires an owner");
        }
        FieldCell cell;
        Map<Object, Map<String, WeakReference<FieldCell>>> stripe =
                stateStripe(FIELD_CELLS, owner);
        synchronized (stripe) {
            Map<String, WeakReference<FieldCell>> fields = stripe.computeIfAbsent(
                    owner, ignored -> new HashMap<>());
            WeakReference<FieldCell> reference = fields.get(fieldName);
            cell = reference == null ? null : reference.get();
            if (cell == null) {
                try {
                    cell = new FieldCell(owner, instanceField(owner.getClass(), fieldName));
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
        }
        return new Pointer(cell, size, 0, size, codecClassName);
    }

    public static Pointer field(
            Object owner, String fieldName, long size, String codecClassName) {
        return field(owner, fieldName, checkedArrayLength(size), codecClassName);
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
        boolean managedField = fieldSize == 0;
        if (!managedField) {
            try {
                Class<?> ownerClass = resolvedRuntimeClass(ownerClassName);
                Class<?> fieldType = instanceField(ownerClass, fieldName).getType();
                managedField = !fieldType.isPrimitive()
                        && fieldType != Pointer.class
                        && !isSliceViewType(fieldType);
            } catch (ClassNotFoundException error) {
                throw new IllegalArgumentException(
                        "unknown Rust aggregate class " + ownerClassName, error);
            } catch (NoSuchFieldException error) {
                throw new IllegalArgumentException(
                        "unknown Rust field " + ownerClassName + "." + fieldName, error);
            }
        }
        if (managedField && hasStableManagedCarrier()) {
            Object owner = compatibleStructView(ownerClassName);
            if (owner != null) {
                return field(owner, fieldName, fieldSize, fieldCodecClassName)
                        .withMetadata(metadata)
                        .inheritAddressOrigin(this, fieldOffset);
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
            Class<?> sliceView = resolvedRuntimeClass(SLICE_VIEW_CLASS_NAME);
            return LONG_SLICE_VIEW_CONSTRUCTORS
                    .get(sliceView)
                    .newInstance(data, 0, metadata());
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
                        return source.byte_offset(relativeOffset)
                                .retype(elementSize, codecClassName);
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
        byte[] allocation;
        synchronized (CONSTANT_ALLOCATIONS) {
            allocation = CONSTANT_ALLOCATIONS.get(identity);
            if (allocation == null) {
                allocation = new byte[checkedLength];
                Arrays.fill(allocation, (byte) initialByte);
                CONSTANT_ALLOCATIONS.put(identity, allocation);
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
        byte[] allocation;
        synchronized (CONSTANT_ALLOCATIONS) {
            allocation = CONSTANT_ALLOCATIONS.get(identity);
            if (allocation == null) {
                allocation = bytes;
                CONSTANT_ALLOCATIONS.put(identity, allocation);
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

    /** Materializes one stable JVM allocation for an anonymous CTFE allocation. */
    public static Pointer constantCell(
            String identity,
            Object value,
            long viewSize,
            String viewCodecClassName,
            long alignment) {
        int checkedSize = checkedArrayLength(viewSize);
        int checkedAlignment = checkedAlignment(alignment);
        synchronized (CONSTANT_CELLS) {
            Pointer pointer = CONSTANT_CELLS.get(identity);
            if (pointer == null) {
                if (value != null && value.getClass().isArray()) {
                    // A Rust fixed array is one value, but its elements must
                    // remain individually addressable after pointer casts.
                    // Keeping the JVM array as the allocation preserves that
                    // storage instead of turning it into one scalar Cell.
                    recordAlignment(value, checkedAlignment);
                    pointer = new Pointer(
                            value,
                            inferredArrayElementSize(value),
                            0,
                            checkedSize,
                            null,
                            viewCodecClassName,
                            -1);
                } else {
                    pointer = cellAligned(
                            value,
                            checkedSize,
                            viewCodecClassName,
                            checkedAlignment);
                }
                CONSTANT_CELLS.put(identity, pointer);
            }
            if (value != null
                    && !value.getClass().isArray()
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
            return pointer.retype(checkedSize, viewCodecClassName);
        }
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
        synchronized (CONSTANT_CELLS) {
            Pointer pointer = CONSTANT_CELLS.get(identity);
            if (pointer == null) {
                recordAlignment(value, checkedAlignment);
                pointer = array(value, 0, checkedSize, elementCodecClassName);
                CONSTANT_CELLS.put(identity, pointer);
            }
            return pointer.retype(checkedSize, elementCodecClassName);
        }
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
            AllocationInfo info = ALLOCATIONS.remove(new IdentityWeakReference(allocation, null));
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
                    Map<String, ExposedTarget> typed = TYPED_EXPOSED_ADDRESSES.get(address);
                    if (typed != null) {
                        typed.values().removeIf(candidate -> candidate.allocation == allocation);
                        if (typed.isEmpty()) {
                            TYPED_EXPOSED_ADDRESSES.remove(address);
                        }
                    }
                }
            }
        }
        Map<Object, NavigableMap<Long, MemoryViewState>> memoryStripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (memoryStripe) {
            memoryStripe.remove(allocation);
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
        try {
            Field arrayField = instanceField(sliceView.getClass(), "array");
            Field offsetField = instanceField(sliceView.getClass(), "offset");
            Object backing = arrayField.get(sliceView);
            int offset = offsetField.getInt(sliceView);
            long length = sliceLogicalLength(sliceView);
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
                        return source.byte_offset(relativeOffset)
                                .retype(elementSize, codecClassName)
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
            Field arrayField = instanceField(sliceView.getClass(), "array");
            Object backing = arrayField.get(sliceView);
            if (backing instanceof Pointer) {
                Field offsetField = instanceField(sliceView.getClass(), "offset");
                int offset = offsetField.getInt(sliceView);
                long length = sliceLogicalLength(sliceView);
                return ((Pointer) backing)
                        .sliceStorageView(elementSize, codecClassName)
                        .add(offset)
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
            Field arrayField = instanceField(sliceView.getClass(), "array");
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
        if (pointer != null && pointer.traitMetadataMarker != null) {
            return pointer.traitMetadataMarker;
        }
        Object value = null;
        String concreteClassName = "<null>";
        if (pointer != null) {
            if (pointer.traitMetadataCarrier != null) {
                value = pointer.traitMetadataCarrier;
                concreteClassName = value.getClass().getName();
            } else if (pointer.traitPointeeSize == 0
                    && pointer.traitAdapterClassName != null) {
                // A ZST data pointer is a non-null dangling address and has no
                // Java value to dereference. Its generated adapter still gives
                // the vtable a stable, concrete identity.
                concreteClassName = pointer.traitAdapterClassName;
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
                    marker.traitPointeeSize = pointer.traitPointeeSize;
                    marker.traitPointeeAlignment = pointer.traitPointeeAlignment;
                    marker.traitAdapterClassName = pointer.traitAdapterClassName;
                }
                if (value instanceof TraitObjectCarrier) {
                    TraitObjectCarrier carrier = (TraitObjectCarrier) value;
                    marker.traitPointeeSize = carrier.rustTraitObjectSize();
                    marker.traitPointeeAlignment = carrier.rustTraitObjectAlignment();
                    marker.traitAdapterClassName = value.getClass().getName();
                    Object payload = carrier.rustTraitObjectPayload();
                    if (payload instanceof Pointer) {
                        marker.traitPointeeCodecClassName =
                                ((Pointer) payload).viewCodecClassName;
                    }
                }
                TRAIT_METADATA_MARKERS.put(key, marker);
            }
            if (marker.traitPointeeSize >= 0 && marker.traitPointeeAlignment > 0) {
                TRAIT_METADATA_INFO.put(
                        marker.numericAddress(),
                        new TraitMetadataInfo(
                                marker.traitPointeeSize,
                                marker.traitPointeeAlignment,
                                marker.traitAdapterClassName,
                                marker.traitPointeeCodecClassName));
            }
            if (pointer != null) {
                pointer.traitMetadataMarker = marker;
            }
            return marker;
        }
    }

    /** Materializes the symbolic vtable emitted by rustc's constant evaluator. */
    public static Pointer constantVtableMarker(
            String identity, long pointeeSize, long pointeeAlignment) {
        String key = "@const-vtable\0" + identity;
        synchronized (TRAIT_METADATA_MARKERS) {
            Pointer marker = TRAIT_METADATA_MARKERS.get(key);
            if (marker == null) {
                marker = cell(null, 0, null);
                marker.traitPointeeSize = pointeeSize;
                marker.traitPointeeAlignment = pointeeAlignment;
                TRAIT_METADATA_MARKERS.put(key, marker);
            }
            TRAIT_METADATA_INFO.put(
                    marker.numericAddress(),
                    new TraitMetadataInfo(pointeeSize, pointeeAlignment, null, null));
            return marker;
        }
    }

    public static long vtableSize(Pointer marker) {
        if (marker == null) {
            throw new IllegalStateException("Rust vtable does not carry a pointee size");
        }
        if (marker.traitPointeeSize >= 0) {
            return marker.traitPointeeSize;
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
        if (marker.traitPointeeAlignment > 0) {
            return marker.traitPointeeAlignment;
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
                    pointer.zeroSizedSourceViewSize = target.zeroSizedSourceViewSize;
                    pointer.zeroSizedSourceViewCodecClassName =
                            target.zeroSizedSourceViewCodecClassName;
                }
                pointer.traitObjectCarrier = target.traitObjectCarrier;
                pointer.traitMetadataCarrier = target.traitMetadataCarrier;
                pointer.traitMetadataMarker = target.traitMetadataMarker;
                pointer.traitPointeeSize = target.traitPointeeSize;
                pointer.traitPointeeAlignment = target.traitPointeeAlignment;
                pointer.traitAdapterClassName = target.traitAdapterClassName;
                pointer.addressOrigin = target.addressOrigin;
                pointer.addressOriginOffset = target.addressOriginOffset;
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

    /** Returns stable UTF-8 storage for one Java String identity. */
    public static byte[] utf8Bytes(String value) {
        if (value == null) {
            throw new NullPointerException("Rust str source is null");
        }
        synchronized (JAVA_STRING_UTF8) {
            byte[] bytes = JAVA_STRING_UTF8.get(value);
            if (bytes == null) {
                bytes = value.getBytes(StandardCharsets.UTF_8);
                JAVA_STRING_UTF8.put(value, bytes);
            }
            return bytes;
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
        if (address == 0) {
            return nullPointer();
        }
        synchronized (ALLOCATIONS) {
            Map<String, ExposedTarget> typed = TYPED_EXPOSED_ADDRESSES.get(address);
            ExposedTarget target = typed == null ? null : typed.get(pointerCodec);
            if (target != null) {
                return pointerFromExposedTarget(target);
            }
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
        pointer.zeroSizedSourceViewSize = target.zeroSizedSourceViewSize;
        pointer.zeroSizedSourceViewCodecClassName =
                target.zeroSizedSourceViewCodecClassName;
        pointer.traitObjectCarrier = target.traitObjectCarrier;
        pointer.traitMetadataCarrier = target.traitMetadataCarrier;
        pointer.traitMetadataMarker = target.traitMetadataMarker;
        pointer.traitPointeeSize = target.traitPointeeSize;
        pointer.traitPointeeAlignment = target.traitPointeeAlignment;
        pointer.traitAdapterClassName = target.traitAdapterClassName;
        pointer.addressOrigin = target.addressOrigin;
        pointer.addressOriginOffset = target.addressOriginOffset;
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
                resultExposedAddress).withMetadata(metadata).copyAddressOrigin(this, 0);
        result.traitObjectCarrier = traitObjectCarrier;
        result.traitMetadataCarrier = traitMetadataCarrier;
        result.traitMetadataMarker = traitMetadataMarker;
        result.traitPointeeSize = traitPointeeSize;
        result.traitPointeeAlignment = traitPointeeAlignment;
        result.traitAdapterClassName = traitAdapterClassName;
        result.traitPointeeCodecClassName = traitPointeeCodecClassName;
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

    /** Returns only the data word of a potentially wide Rust pointer. */
    static Pointer dataPointerView(
            Pointer pointer, long newViewSize, String newViewCodecClassName) {
        Pointer result = pointer.retype(newViewSize, newViewCodecClassName);
        result.metadata = 0;
        result.traitObjectCarrier = null;
        result.traitMetadataCarrier = null;
        result.traitMetadataMarker = null;
        result.traitPointeeSize = -1;
        result.traitPointeeAlignment = -1;
        result.traitAdapterClassName = null;
        result.traitPointeeCodecClassName = null;
        return result;
    }

    public static Pointer traitObjectDataPointer(
            Pointer pointer, long newViewSize, String newViewCodecClassName) {
        return RuntimeSupport.traitObjectDataPointer(
                pointer, newViewSize, newViewCodecClassName);
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
        pointer.traitObjectCarrier = carrier;
        pointer.traitPointeeSize = pointeeSize;
        pointer.traitPointeeAlignment = pointeeAlignment;
        pointer.traitAdapterClassName = carrier.getClass().getName();
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
        Object carrier = tailPointer.traitObjectCarrier != null
                ? tailPointer.traitObjectCarrier
                : tailPointer.traitMetadataCarrier;
        if (carrier == null) {
            throw new IllegalArgumentException(
                    "struct-tail trait pointer does not carry dynamic metadata");
        }
        pointer.traitMetadataCarrier = carrier;
        pointer.traitMetadataMarker = tailPointer.traitMetadataMarker;
        pointer.traitPointeeSize = tailPointer.traitPointeeSize;
        pointer.traitPointeeAlignment = tailPointer.traitPointeeAlignment;
        pointer.traitAdapterClassName = tailPointer.traitAdapterClassName;
        pointer.traitPointeeCodecClassName = tailPointer.traitPointeeCodecClassName;
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
        try {
            Object source = pointer.getObject();
            Class<?> targetClass = resolvedRuntimeClass(targetClassName);
            for (Field targetField : PUBLIC_INSTANCE_FIELDS.get(targetClass)) {
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
        result.traitObjectCarrier = pointer.traitObjectCarrier;
        result.traitMetadataCarrier = pointer.traitMetadataCarrier;
        result.traitMetadataMarker = pointer.traitMetadataMarker;
        result.traitPointeeSize = pointer.traitPointeeSize;
        result.traitPointeeAlignment = pointer.traitPointeeAlignment;
        result.traitAdapterClassName = pointer.traitAdapterClassName;
        result.traitPointeeCodecClassName = pointer.traitPointeeCodecClassName;
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

    /** Rebuilds a slice-like JVM carrier after its data pointer passed through `NonNull<()>`. */
    public static Object restoreErasedSliceView(Pointer pointer, String viewClassName) {
        if (pointer == null || viewClassName == null || viewClassName.isEmpty()) {
            throw new IllegalArgumentException("invalid erased Rust slice view");
        }
        Pointer data = restoreErasedView(pointer);
        try {
            Class<?> viewClass = resolvedRuntimeClass(viewClassName);
            return LONG_SLICE_VIEW_CONSTRUCTORS
                    .get(viewClass)
                    .newInstance(data, 0, pointer.metadata());
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not rebuild erased Rust slice view", error);
        }
    }

    private static Pointer traitMetadataPointer(Object metadata, int depth)
            throws IllegalAccessException {
        if (metadata instanceof Pointer) {
            return (Pointer) metadata;
        }
        if (metadata == null || depth == 0) {
            return null;
        }
        for (Field field : PUBLIC_INSTANCE_FIELDS.get(metadata.getClass())) {
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
            long pointeeSize = marker.traitPointeeSize;
            long pointeeAlignment = marker.traitPointeeAlignment;
            String adapterClassName = marker.traitAdapterClassName;
            String pointeeCodecClassName = marker.traitPointeeCodecClassName;
            if (info != null) {
                pointeeSize = info.size;
                pointeeAlignment = info.alignment;
                adapterClassName = info.adapterClassName;
                pointeeCodecClassName = info.pointeeCodecClassName;
            }
            Pointer source = pointeeSize >= 0
                    ? data.retype(pointeeSize, pointeeCodecClassName)
                    : restoreErasedView(data);
            Pointer result = source.retype(0);
            result.traitMetadataMarker = marker;
            result.traitPointeeSize = pointeeSize;
            result.traitPointeeAlignment = pointeeAlignment;
            result.traitAdapterClassName = adapterClassName;
            if (adapterClassName != null) {
                Class<?> adapter = resolvedRuntimeClass(adapterClassName);
                Object carrier = constructorWithArity(adapter, 1).newInstance(source);
                result.traitObjectCarrier = carrier;
            }
            return result;
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not rebuild Rust trait-object pointer", error);
        }
    }

    private Pointer withMetadata(long metadata) {
        this.metadata = metadata;
        return this;
    }

    private Pointer inheritAddressOrigin(Pointer source, long additionalOffset) {
        if (source.addressOrigin == null) {
            addressOrigin = source;
            addressOriginOffset = additionalOffset;
        } else {
            addressOrigin = source.addressOrigin;
            addressOriginOffset = Math.addExact(
                    source.addressOriginOffset, additionalOffset);
        }
        return this;
    }

    private Pointer copyAddressOrigin(Pointer source, long additionalOffset) {
        if (source.addressOrigin != null) {
            addressOrigin = source.addressOrigin;
            addressOriginOffset = Math.addExact(
                    source.addressOriginOffset, additionalOffset);
        }
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
            if (pointer.traitPointeeSize >= 0) {
                return pointer.traitPointeeSize;
            }
            value = pointer.traitObjectCarrier != null
                    ? pointer.traitObjectCarrier
                    : pointer.traitMetadataCarrier;
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
            if (pointer.traitPointeeAlignment > 0) {
                return pointer.traitPointeeAlignment;
            }
            value = pointer.traitObjectCarrier != null
                    ? pointer.traitObjectCarrier
                    : pointer.traitMetadataCarrier;
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
                    Math.addExact(exposedAddress, delta)).withMetadata(metadata)
                    .copyAddressOrigin(this, delta);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                Math.addExact(byteOffset, delta),
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata).copyAddressOrigin(this, delta);
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
                    Math.addExact(exposedAddress, byteCount)).withMetadata(metadata)
                    .copyAddressOrigin(this, byteCount);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                Math.addExact(byteOffset, byteCount),
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata).copyAddressOrigin(this, byteCount);
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
                    .copyAddressOrigin(this, byteCount);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                byteOffset + byteCount,
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata).copyAddressOrigin(this, byteCount);
    }

    public long align_offset(long alignment) {
        if (alignment <= 0 || (alignment & (alignment - 1)) != 0) {
            throw new IllegalArgumentException("Rust pointer alignment must be a power of two");
        }
        long current = numericAddress();
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
                    address).withMetadata(metadata);
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
                    .copyAddressOrigin(this, delta);
        }
        return new Pointer(
                allocation,
                allocationElementSize,
                byteOffset + delta,
                viewSize,
                allocationCodecClassName,
                viewCodecClassName,
                -1).withMetadata(metadata).copyAddressOrigin(this, delta);
    }

    private Object provenanceAllocation() {
        return addressOrigin == null ? allocation : addressOrigin.provenanceAllocation();
    }

    private long provenanceByteOffset() {
        return addressOrigin == null
                ? byteOffset
                : Math.addExact(addressOrigin.provenanceByteOffset(), addressOriginOffset);
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
        return other != null && numericAddress() == other.numericAddress();
    }

    /** Compares the data and metadata words of a Rust raw pointer. */
    public boolean samePointer(Pointer other) {
        if (other == null) {
            return false;
        }
        if (traitObjectCarrier != null
                || traitAdapterClassName != null
                || traitMetadataMarker != null
                || other.traitObjectCarrier != null
                || other.traitAdapterClassName != null
                || other.traitMetadataMarker != null) {
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
        boolean leftHasDstMetadata = viewCodecClassName != null
                && viewCodecClassName.startsWith(STRUCT_TAIL_VIEW_CODEC_PREFIX);
        boolean rightHasDstMetadata = other.viewCodecClassName != null
                && other.viewCodecClassName.startsWith(STRUCT_TAIL_VIEW_CODEC_PREFIX);
        if ((leftHasDstMetadata || rightHasDstMetadata) && metadata != other.metadata) {
            return false;
        }
        if (traitMetadataMarker != null && other.traitMetadataMarker != null) {
            return traitMetadataMarker.sameAddress(other.traitMetadataMarker);
        }
        if (traitMetadataMarker == null && other.traitMetadataMarker == null) {
            return java.util.Objects.equals(
                    traitAdapterClassName, other.traitAdapterClassName);
        }
        Pointer marker = traitMetadataMarker != null
                ? traitMetadataMarker
                : other.traitMetadataMarker;
        String unmaterializedAdapter = traitMetadataMarker == null
                ? traitAdapterClassName
                : other.traitAdapterClassName;
        TraitMetadataInfo info = TRAIT_METADATA_INFO.get(marker.numericAddress());
        String markerAdapter = info == null ? marker.traitAdapterClassName : info.adapterClassName;
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
        return Long.compareUnsigned(numericAddress(), other.numericAddress());
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
        return allocation.getClass().isArray()
                ? Math.multiplyExact((long) Array.getLength(allocation), elementSize)
                : elementSize;
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
            ALLOCATION_EXPOSED_ADDRESSES
                    .computeIfAbsent(target.allocation, ignored -> new HashSet<>())
                    .add(address);
        }
    }

    /** Must be called while holding {@link #ALLOCATIONS}. */
    private static void registerTypedExposedTarget(
            long address, String pointerCodec, ExposedTarget target) {
        TYPED_EXPOSED_ADDRESSES
                .computeIfAbsent(address, ignored -> new HashMap<>())
                .put(pointerCodec, target);
        if (target.allocation != null) {
            ALLOCATION_EXPOSED_ADDRESSES
                    .computeIfAbsent(target.allocation, ignored -> new HashSet<>())
                    .add(address);
        }
    }

    public static Object asRefOption(Pointer pointer, String optionClassName) {
        String variantName = optionClassName + (is_null(pointer) ? "$None" : "$Some");
        try {
            Class<?> variant = resolvedRuntimeClass(variantName);
            if (is_null(pointer)) {
                return constructorWithArity(variant, 0).newInstance();
            }
            Constructor<?> constructor = constructorWithArity(variant, 1);
            Class<?> referentType = constructor.getParameterTypes()[0];
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
                    "could not construct Rust pointer option " + optionClassName, error);
        }
    }

    private long numericAddress() {
        if (addressOrigin != null) {
            return Math.addExact(addressOrigin.numericAddress(), addressOriginOffset);
        }
        if (allocation == null) {
            return exposedAddress;
        }
        long cachedAddress = publishedAddress;
        if (cachedAddress != Long.MIN_VALUE) {
            return cachedAddress;
        }
        synchronized (ALLOCATIONS) {
            AllocationInfo info = allocationInfo(allocation);
            long address = allocationBase(info) + byteOffset;
            publishedAddress = address;
            return address;
        }
    }

    private static long numericAddress(Pointer pointer) {
        return pointer == null ? 0L : pointer.numericAddress();
    }

    /** Must be called while holding {@link #ALLOCATIONS}. */
    private long allocationBase(AllocationInfo info) {
        Long base = info.base;
        if (base == null) {
            long byteCapacity = allocationCapacity(allocation, allocationElementSize);
            long requiredSpan = Math.addExact(byteCapacity, 1L);
            long span = Math.max(16L, Math.addExact(requiredSpan, 15L) & ~15L);
            base = allocateAddress(span, info.alignment);
            info.base = base;
        }
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
        if (pointer.addressOrigin != null) {
            long address = Math.addExact(
                    encodedAddress(pointer.addressOrigin, owner),
                    pointer.addressOriginOffset);
            pointer.publishedAddress = address;
            return address;
        }
        if (pointer.allocation == null) {
            return pointer.exposedAddress;
        }
        long address;
        synchronized (ALLOCATIONS) {
            AllocationInfo info = allocationInfo(pointer.allocation);
            address = pointer.publishAllocationRange(info) + pointer.byteOffset;
            pointer.publishedAddress = address;
        }
        retainEncodedReference(owner, pointer.allocation);
        return address;
    }

    public static long encodedAddress(
            Pointer pointer, Object owner, String pointerCodec) {
        long address = encodedAddress(pointer, owner);
        if (pointer == null || pointerCodec == null) {
            return address;
        }
        synchronized (ALLOCATIONS) {
            registerTypedExposedTarget(address, pointerCodec, pointer.exposedTarget());
        }
        if (pointer.allocation != null) {
            retainEncodedReference(owner, pointer.allocation);
        }
        return address;
    }

    private ExposedTarget exposedTarget() {
        return new ExposedTarget(
                allocation,
                allocationElementSize,
                byteOffset,
                exposedAddress,
                allocationCodecClassName,
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
                addressOrigin,
                addressOriginOffset);
    }

    /** Exposes this pointer's provenance so its numeric address can be recovered later. */
    public long address() {
        if (addressOrigin != null) {
            long address = Math.addExact(addressOrigin.address(), addressOriginOffset);
            publishedAddress = address;
            return address;
        }
        if (allocation == null) {
            return exposedAddress;
        }
        ExposedTarget cachedTarget = publishedTarget;
        if (cachedTarget != null && cachedTarget.matches(this)) {
            return publishedAddress;
        }
        synchronized (ALLOCATIONS) {
            AllocationInfo info = allocationInfo(allocation);
            long base = publishAllocationRange(info);
            long address = base + byteOffset;
            ExposedTarget target = exposedTarget();
            registerExposedTarget(address, target);
            publishedTarget = target;
            publishedAddress = address;
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
            synchronized (ALLOCATIONS) {
                long token = allocateAddress(16L, 16);
                registerExposedTarget(
                        token,
                        pointer.exposedTarget());
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
                Object scalar = allocation instanceof Cell
                        ? ((Cell) allocation).value
                        : ((ReceiverCell) allocation).value;
                String constantIdentity = null;
                synchronized (CONSTANT_CELLS) {
                    for (Map.Entry<String, Pointer> entry : CONSTANT_CELLS.entrySet()) {
                        if (entry.getValue().allocation == allocation) {
                            constantIdentity = entry.getKey();
                            break;
                        }
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
        if (value == null || !value.getClass().isArray()
                || !value.getClass().getComponentType().isPrimitive()) {
            return null;
        }
        int length = Array.getLength(value);
        if (length == 0 || allocationElementSize % length != 0) {
            return null;
        }
        int elementSize = allocationElementSize / length;
        return elementSize > 0 && elementSize <= 8 ? value : null;
    }

    private int embeddedArrayElementSize(Object array) {
        return allocationElementSize / Array.getLength(array);
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
        long bits = valueBits(Array.get(array, elementIndex), elementSize);
        return (int) ((bits >>> (withinElement * 8)) & 0xffL);
    }

    private static void storePrimitiveArrayByte(
            Object array, int byteOffset, int elementSize, int incoming) {
        int elementIndex = byteOffset / elementSize;
        int withinElement = byteOffset % elementSize;
        Object current = Array.get(array, elementIndex);
        long bits = valueBits(current, elementSize);
        long mask = 0xffL << (withinElement * 8);
        long updated = (bits & ~mask) | (((long) incoming & 0xffL) << (withinElement * 8));
        Array.set(array, elementIndex, carrierFromBits(current, updated, elementSize));
    }

    private Long loadEmbeddedArrayBits(long absoluteByteOffset, int byteCount) {
        Object array = embeddedPrimitiveArray();
        if (array == null) {
            return null;
        }
        int elementSize = embeddedArrayElementSize(array);
        int withinElement = (int) Math.floorMod(absoluteByteOffset, elementSize);
        if (withinElement + byteCount > elementSize || byteCount > 8) {
            return null;
        }
        flushMemoryViewsOverlapping(absoluteByteOffset, byteCount);
        int elementIndex = Math.toIntExact(Math.floorDiv(absoluteByteOffset, elementSize));
        Object current = Array.get(array, elementIndex);
        long bits = valueBits(current, elementSize);
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
        Object current = Array.get(array, elementIndex);
        int shift = withinElement * 8;
        long valueMask = atomicMask(byteCount);
        long mask = valueMask << shift;
        long currentBits = valueBits(current, elementSize);
        long updated = (currentBits & ~mask) | ((incoming & valueMask) << shift);
        Array.set(array, elementIndex, carrierFromBits(current, updated, elementSize));
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
        if (byteCount == 0) {
            return 0;
        }
        Long embedded = loadEmbeddedArrayBits(byteOffset, byteCount);
        if (embedded != null) {
            return embedded.longValue();
        }
        if (allocation != null && allocationElementSize > 0) {
            int withinElement = (int) Math.floorMod(byteOffset, allocationElementSize);
            if (withinElement + byteCount <= allocationElementSize) {
                int elementIndex = Math.toIntExact(
                        Math.floorDiv(byteOffset, allocationElementSize));
                flushMemoryViewsOverlapping(byteOffset, byteCount);
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
                if (isFatPointerCodec(allocationCodecClassName)) {
                    encoded = encodeFatPointer(
                            value, allocationElementSize, allocationCodecClassName);
                } else if (isGeneratedAggregateCodec(allocationCodecClassName)) {
                    encoded = encodeAggregate(allocationCodecClassName, value);
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
                    discardEncodedReferences(encoded);
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
            result |= ((long) loadByte(byteOffset + index)) << (index * 8);
        }
        return result;
    }

    private int loadByte(long absoluteByteOffset) {
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
                            + ", recorded_source_size=" + zeroSizedSourceViewSize
                            + ", recorded_source_codec=" + zeroSizedSourceViewCodecClassName);
        }
        Long embedded = loadEmbeddedArrayBits(absoluteByteOffset, 1);
        if (embedded != null) {
            return embedded.intValue();
        }
        flushMemoryViewsOverlapping(absoluteByteOffset, 1);
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
            byte[] bytes = encodeAggregate(allocationCodecClassName, value);
            if (withinElement >= bytes.length) {
                discardEncodedReferences(bytes);
                throw new IndexOutOfBoundsException("aggregate codec returned a short memory image");
            }
            int result = bytes[withinElement] & 0xff;
            discardEncodedReferences(bytes);
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
            if (isFatPointerCodec(allocationCodecClassName)) {
                image = encodeFatPointer(value, allocationElementSize, allocationCodecClassName);
            } else if (isGeneratedAggregateCodec(allocationCodecClassName)) {
                image = encodeAggregate(allocationCodecClassName, value);
            }
            if (image != null) {
                if (withinElement + chunk > image.length) {
                    discardEncodedReferences(image);
                    throw new IndexOutOfBoundsException(
                            "aggregate codec returned a short memory image");
                }
                System.arraycopy(image, withinElement, result, consumed, chunk);
                transferEncodedReferences(image, result);
                discardEncodedReferences(image);
            } else {
                for (int index = 0; index < chunk; index++) {
                    result[consumed + index] = (byte) loadByte(absoluteOffset + index);
                }
            }
            consumed += chunk;
        }
        return result;
    }

    private void storeBytes(long bits, int byteCount) {
        if (byteCount < 0 || byteCount > 8) {
            throw new IllegalArgumentException("scalar stores support at most eight bytes");
        }
        if (byteCount == 0) {
            return;
        }
        if (storeEmbeddedArrayBits(byteOffset, bits, byteCount)) {
            return;
        }
        if (allocation != null && allocationElementSize > 0) {
            int withinElement = (int) Math.floorMod(byteOffset, allocationElementSize);
            if (withinElement + byteCount <= allocationElementSize
                    && withinElement + byteCount <= 8) {
                int elementIndex = Math.toIntExact(
                        Math.floorDiv(byteOffset, allocationElementSize));
                prepareMemoryWrite(byteOffset, byteCount);
                Object current = readElement(elementIndex);
                if (isPrimitiveScalarCarrier(current)) {
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
                    encoded = encodeAggregate(allocationCodecClassName, current);
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
        if (storeEmbeddedArrayBits(absoluteByteOffset, value & 0xffL, 1)) {
            return;
        }
        flushMemoryViewsOverlapping(absoluteByteOffset, 1);
        int elementIndex = Math.toIntExact(Math.floorDiv(absoluteByteOffset, allocationElementSize));
        int withinElement = (int) Math.floorMod(absoluteByteOffset, allocationElementSize);
        Object current;
        current = readElement(elementIndex);
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
            byte[] bytes = encodeAggregate(allocationCodecClassName, current);
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
            identity = cell.owner();
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
            long value = truncateAtomic(pointer.loadUnsigned(byteCount), byteCount);
            return value;
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
        if (direct != this && MANAGED_OBJECT_VIEW_CODEC.equals(viewCodecClassName)) {
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
                            exposedAddress).withMetadata(metadata)
                    .copyAddressOrigin(this, 0);
            pointer.traitObjectCarrier = traitObjectCarrier;
            pointer.traitMetadataCarrier = traitMetadataCarrier;
            pointer.traitMetadataMarker = traitMetadataMarker;
            pointer.traitPointeeSize = traitPointeeSize;
            pointer.traitPointeeAlignment = traitPointeeAlignment;
            pointer.traitAdapterClassName = traitAdapterClassName;
            pointer.traitPointeeCodecClassName = traitPointeeCodecClassName;
            return pointer.getObject();
        }
        if (MANAGED_OBJECT_VIEW_CODEC.equals(viewCodecClassName)
                && !isDirectAllocationView()) {
            return managedObjectFromAddress(loadUnsigned((int) Math.min(viewSize, 8)));
        }
        if (isRawPointerCodec(viewCodecClassName)
                && !isDirectAllocationView()) {
            if (isArrayReferenceCodec(viewCodecClassName)) {
                try {
                    return decodeArrayReference(
                            loadUnsigned((int) Math.min(viewSize, 8)),
                            viewCodecClassName,
                            resolvedRuntimeClass(SLICE_VIEW_CLASS_NAME));
                } catch (ClassNotFoundException error) {
                    throw new IllegalStateException(
                            "could not load fixed-array reference carrier", error);
                }
            }
            return decodedRawPointer(
                    loadUnsigned((int) Math.min(viewSize, 8)), viewCodecClassName);
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
        return this;
    }

    public Object getObjectAs(String targetClassName) {
        if (targetClassName == null || targetClassName.isEmpty()) {
            return getObject();
        }
        if (SLICE_VIEW_CLASS_NAME.equals(binaryClassName(targetClassName))
                && isArrayReferenceCodec(viewCodecClassName)
                && !isDirectAllocationView()) {
            try {
                Class<?> targetClass = resolvedRuntimeClass(targetClassName);
                return decodeArrayReference(
                        loadUnsigned((int) Math.min(viewSize, 8)),
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
            return state.activate(targetClass, traitMetadataCarrier);
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
        return allocationCodecClassName == null
                ? viewCodecClassName == null
                : allocationCodecClassName.equals(viewCodecClassName);
    }

    // A decoded view assigned back to its source already aliases that storage.
    // Flush it with its own codec instead of reinterpreting its JVM carrier.
    private boolean flushSameOriginMemoryView(Object value, int size) {
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
        Object allocation = origin.allocation.get();
        if (allocation == null) {
            return false;
        }
        MemoryViewState active;
        Map<Object, NavigableMap<Long, MemoryViewState>> stripe =
                stateStripe(MEMORY_VIEWS, allocation);
        synchronized (stripe) {
            NavigableMap<Long, MemoryViewState> views = stripe.get(allocation);
            active = views == null ? null : views.get(origin.byteOffset);
            if (active == null || active.size != origin.viewSize) {
                return false;
            }
        }
        if (active.value == value) {
            return true;
        }
        Pointer pointer = new Pointer(
                allocation,
                origin.allocationElementSize,
                origin.byteOffset,
                origin.viewSize,
                origin.allocationCodecClassName,
                active.codecClassName,
                -1);
        return pointer.transparentManagedView(active.value.getClass()) == value;
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
            if (isRawPointerCodec(viewCodecClassName)) {
                byte[] image = new byte[materializedSize];
                long address = encodedAddress(
                        rawPointerCarrier(value, viewCodecClassName),
                        image,
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
        byte[] temporary = source.loadRange(byteCount);
        transferEncodedReferences(source.allocation, temporary);
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
            Field[] fields = PUBLIC_INSTANCE_FIELDS.get(current.getClass());
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
        Pointer storage = sliceStorageView();
        Object direct = storage.directCellValueOrSelf();
        return direct != null
                        && direct != storage
                        && isSliceViewType(direct.getClass())
                ? storage.sliceStorageView(storage.viewSize, storage.viewCodecClassName)
                : storage;
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

    /** Applies the Rust element layout to a generic reference-array slice backing. */
    private Pointer sliceStorageView(long elementSize, String elementCodecClassName) {
        Pointer storage = sliceStorageView();
        int checkedElementSize = checkedArrayLength(elementSize);
        Object direct = storage.directCellValueOrSelf();
        if (direct != null
                && direct != storage
                && isSliceViewType(direct.getClass())) {
            try {
                Object backing = instanceField(direct.getClass(), "array").get(direct);
                boolean selfBacked = backing == storage || backing == this;
                if (backing instanceof Pointer) {
                    Pointer pointerBacking = (Pointer) backing;
                    selfBacked |= pointerBacking.allocation == storage.allocation;
                }
                if (!selfBacked) {
                    return fromSlice(direct, checkedElementSize, elementCodecClassName);
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
            return new Pointer(
                            storage.allocation,
                            checkedElementSize,
                            Math.multiplyExact(elementOffset, elementSize),
                            checkedElementSize,
                            allocationCodec,
                            elementCodecClassName,
                            -1)
                    .withMetadata(storage.metadata)
                    .copyAddressOrigin(storage, 0);
        }
        return storage.retype(checkedElementSize, elementCodecClassName);
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
        return pointer != null
                ? pointer.getObject()
                : independentRepeatedArrayElement(backing, index);
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
        if (component == byte.class) {
            Array.setByte(backing, index, (byte) value);
        } else if (component == short.class) {
            Array.setShort(backing, index, (short) value);
        } else if (component == char.class) {
            Array.setChar(backing, index, (char) value);
        } else if (component == int.class) {
            Array.setInt(backing, index, value);
        } else {
            Array.set(backing, index, Integer.valueOf(value));
        }
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
        moveEncodedReferences(source, allocation);
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
            transferEncodedReferences(allocation, image);
            transferEncodedReferences(source, image);
            int chunk = Math.min(source.length - consumed, image.length - withinElement);
            System.arraycopy(source, consumed, image, withinElement, chunk);
            Object decoded = decodeAggregate(allocationCodecClassName, image);
            discardEncodedReferences(image);
            writeElementPreservingIdentity(
                    elementIndex,
                    decoded);
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
        Method[] cached = CODEC_METHODS.get(codecClassName);
        if (cached != null) {
            return cached;
        }
        try {
            Class<?> codec = resolvedRuntimeClass(codecClassName);
            Method encode = null;
            Method decode = null;
            Method bind = null;
            for (Method method : codec.getMethods()) {
                if (method.getName().equals("encode") && method.getParameterTypes().length == 1) {
                    encode = method;
                } else if (method.getName().equals("decode")
                        && method.getParameterTypes().length == 1) {
                    decode = method;
                } else if (method.getName().equals("bind")
                        && method.getParameterTypes().length == 2) {
                    bind = method;
                }
            }
            if (encode == null || decode == null) {
                throw new NoSuchMethodException(
                        "pointer codec must define public static encode/decode methods");
            }
            encode.setAccessible(true);
            decode.setAccessible(true);
            if (bind != null) {
                bind.setAccessible(true);
            }
            Method[] methods = new Method[] {encode, decode, bind};
            Method[] previous = CODEC_METHODS.putIfAbsent(codecClassName, methods);
            return previous == null ? methods : previous;
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not load Rust pointer codec " + codecClassName, error);
        }
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
                Method discriminant = type.getMethod("_unionDiscriminant");
                Method fromDiscriminant = type.getMethod("_fromUnionDiscriminant", long.class);
                if (discriminant.getReturnType() == long.class
                        && Modifier.isStatic(fromDiscriminant.getModifiers())) {
                    discriminant.setAccessible(true);
                    fromDiscriminant.setAccessible(true);
                    methods = new Method[] {discriminant, fromDiscriminant};
                }
            }
        } catch (NoSuchMethodException ignored) {
            // This is an ordinary non-enum JVM carrier.
        }
        Method[] previous = SCALAR_ENUM_METHODS.putIfAbsent(type, methods);
        return previous == null ? methods : previous;
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
        transferEncodedReferences(sourceBytes, targetBytes);
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

    private static boolean isRawPointerCodec(String codec) {
        return RAW_POINTER_VIEW_CODEC.equals(codec)
                || (codec != null && codec.startsWith(RAW_POINTER_VIEW_CODEC + "\n"))
                || isArrayReferenceCodec(codec);
    }

    private static boolean isArrayReferenceCodec(String codec) {
        return codec != null && codec.startsWith(ARRAY_REFERENCE_VIEW_CODEC_PREFIX);
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
        Pointer pointer = typedPointerObjectFromAddress(address, codec);
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
        Pointer data = decodedRawPointer(address, codec);
        try {
            return LONG_SLICE_VIEW_CONSTRUCTORS
                    .get(targetClass)
                    .newInstance(data, 0, arrayReferenceLength(codec));
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not reconstruct fixed-array reference", error);
        }
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
        Object value = getObject();
        if (value == null) {
            return this;
        }
        try {
            Class<?> targetClass =
                    resolvedClass(targetClassName, value.getClass().getClassLoader());
            if (targetClass.isInstance(value)) {
                return this;
            }
            Field match = null;
            for (Field candidate : PUBLIC_INSTANCE_FIELDS.get(value.getClass())) {
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
            return field(value, match.getName(), viewSize, viewCodecClassName)
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
        return codec != null && !codec.startsWith("@");
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
