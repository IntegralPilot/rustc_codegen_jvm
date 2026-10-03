package org.rustlang.runtime;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Field;

/** Resolves codec operations for an exact Rust layout only when needed. */
final class MemoryCodec {
    final Class<?> encodeParameterType;
    final int arrayElementSize;
    final String arrayElementCodec;
    private final Class<?> owner;
    private final String key;
    private final int byteSize;
    private final Object[] operations = new Object[5];
    private volatile int initialized;

    static MemoryCodec load(String recipe, ClassLoader loader) throws ReflectiveOperationException {
        if (recipe.startsWith("@zero-sized:")) {
            return new MemoryCodec(Class.forName(recipe.substring("@zero-sized:".length()).replace('/', '.'), false, loader));
        }
        int first = recipe.indexOf('#'), second = recipe.indexOf('#', first + 1);
        if (first < 1 || second <= first + 1) throw new IllegalArgumentException("invalid pointer codec recipe: " + recipe);
        Class<?> owner = Class.forName(recipe.substring(0, first).replace('/', '.'), false, loader);
        int third = recipe.indexOf('#', second + 1);
        String descriptor = third < 0 ? recipe.substring(second + 1) : recipe.substring(second + 1, third);
        int size = third < 0 ? -1 : Integer.parseInt(recipe.substring(third + 1));
        if (third >= 0 && size < 0) throw new IllegalArgumentException("invalid codec byte size: " + recipe);
        Class<?> value = MethodType.fromMethodDescriptorString("()" + descriptor, loader).returnType();
        return new MemoryCodec(owner, recipe.substring(first + 1, second), value, size);
    }

    private MemoryCodec(Class<?> value) throws ReflectiveOperationException {
        owner = null;
        key = null;
        byteSize = 0;
        encodeParameterType = value;
        arrayElementSize = -1;
        arrayElementCodec = null;
        MethodHandle constructor = MethodHandles.publicLookup().unreflectConstructor(value.getConstructor())
                .asType(MethodType.methodType(Object.class));
        operations[0] = (CodecCalls.Encoder) ignored -> new byte[0];
        operations[2] = (CodecCalls.Decoder) ignored -> {
            try { return (Object) constructor.invokeExact(); }
            catch (Throwable error) { throw failure(error); }
        };
        initialized = 31;
    }

    private MemoryCodec(Class<?> owner, String key, Class<?> value, int layoutSize) throws ReflectiveOperationException {
        this.owner = owner;
        this.key = key;
        byteSize = layoutSize;
        encodeParameterType = value;
        if (owner == ArrayMemoryCodec.class && key.equals("array")) {
            Class<?> element = value.getComponentType();
            arrayElementSize = ArrayMemoryCodec.elementSize(element);
            arrayElementCodec = null;
            if (layoutSize < 0 || layoutSize % arrayElementSize != 0)
                throw new IllegalArgumentException("invalid primitive array codec size");
            int length = layoutSize / arrayElementSize;
            CodecCalls.RangeEncoder encoder = (array, bytes, offset) -> {
                if (!value.isInstance(array) || java.lang.reflect.Array.getLength(array) != length)
                    throw new IllegalArgumentException("primitive array codec layout mismatch");
                Pointer.encodeArrayMemory(array, bytes, offset, arrayElementSize, null);
            };
            CodecCalls.RangeDecoder decoder = (bytes, offset) -> {
                Object array = java.lang.reflect.Array.newInstance(element, length);
                Pointer.decodeArrayMemory(bytes, offset, array, arrayElementSize, null);
                return array;
            };
            operations[0] = CodecCalls.wholeEncoder(encoder, layoutSize);
            operations[1] = encoder;
            operations[2] = CodecCalls.wholeDecoder(decoder);
            operations[3] = decoder;
            initialized = 31;
            return;
        }
        // Resolve array metadata only for array layouts.
        if (value.isArray()) {
            MethodHandle size = optional("s$", MethodType.methodType(int.class));
            MethodHandle codec = optional("c$", MethodType.methodType(String.class));
            try {
                arrayElementSize = size == null ? -1 : (int) size.invokeExact();
                arrayElementCodec = codec == null ? null : (String) codec.invokeExact();
            } catch (Throwable error) { throw failure(error); }
        } else {
            arrayElementSize = -1;
            arrayElementCodec = null;
        }
    }

    CodecCalls.Encoder encode() { return (CodecCalls.Encoder) operation(0); }
    CodecCalls.RangeEncoder encodeAt() { return (CodecCalls.RangeEncoder) operation(1); }
    CodecCalls.Decoder decode() { return (CodecCalls.Decoder) operation(2); }
    CodecCalls.RangeDecoder decodeAt() { return (CodecCalls.RangeDecoder) operation(3); }
    CodecCalls.Binder bind() { return (CodecCalls.Binder) operation(4); }

    MethodHandle directRangeDecoder() throws IllegalAccessException {
        return owner == null || owner == ArrayMemoryCodec.class ? null
                : optional("a$", MethodType.methodType(encodeParameterType, byte[].class, int.class));
    }

    private Object operation(int index) {
        if ((initialized & (1 << index)) == 0) initialize(index);
        return operations[index];
    }

    private synchronized void initialize(int index) {
        int flag = 1 << index;
        if ((initialized & flag) != 0) return;
        try {
            MethodHandle target;
            switch (index) {
                case 0:
                    target = optional("e$", MethodType.methodType(byte[].class, encodeParameterType));
                    if (target != null) operations[index] = CodecCalls.encoder(target);
                    else if (byteSize >= 0 && encodeAt() != null) {
                        operations[index] = CodecCalls.wholeEncoder(encodeAt(), byteSize);
                    } else throw new NoSuchMethodException("codec has no encoder");
                    break;
                case 1:
                    target = optional("w$", MethodType.methodType(void.class, encodeParameterType, byte[].class, int.class));
                    operations[index] = target == null ? null : CodecCalls.rangeEncoder(target);
                    break;
                case 2:
                    target = optional("d$", MethodType.methodType(encodeParameterType, byte[].class));
                    if (target != null) operations[index] = CodecCalls.decoder(target);
                    else if (decodeAt() != null) operations[index] = CodecCalls.wholeDecoder(decodeAt());
                    else throw new NoSuchMethodException("codec has no decoder");
                    break;
                case 3:
                    target = optional("a$", MethodType.methodType(encodeParameterType, byte[].class, int.class));
                    operations[index] = target == null ? null : CodecCalls.rangeDecoder(target);
                    break;
                case 4:
                    target = optional("b$", MethodType.methodType(void.class, Pointer.class, encodeParameterType));
                    operations[index] = target == null ? null : CodecCalls.binder(target);
                    break;
                default: throw new AssertionError(index);
            }
            // Release publication also records a missing optional operation.
            initialized |= flag;
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("could not resolve Rust codec " + owner.getName() + "#" + key, error);
        }
    }

    private MethodHandle optional(String operation, MethodType type) throws IllegalAccessException {
        try { return MethodHandles.lookup().findStatic(owner, operation + key, type); }
        catch (NoSuchMethodException missing) { return null; }
    }

    boolean isArrayCodecFor(Object value) {
        return arrayElementSize > 0 && value != null && value.getClass().isArray();
    }

    // Resolve union fields only when a union byte operation needs them.
    private static final ClassValue<UnionAccess> UNIONS = new ClassValue<UnionAccess>() {
        protected UnionAccess computeValue(Class<?> type) { return new UnionAccess(type); }
    };

    byte[] directUnionBytes(Object value) {
        if (value == null || !encodeParameterType.isInstance(value)) return null;
        MethodHandle read = UNIONS.get(encodeParameterType).bytes;
        if (read == null) return null;
        try { return (byte[]) read.invokeExact(value); }
        catch (Throwable error) { throw failure(error); }
    }

    Object[] directUnionObjects(Object value) {
        if (value == null || !encodeParameterType.isInstance(value)) return null;
        MethodHandle read = UNIONS.get(encodeParameterType).objects;
        if (read == null) return null;
        try { return (Object[]) read.invokeExact(value); }
        catch (Throwable error) { throw failure(error); }
    }

    private static final class UnionAccess {
        final MethodHandle bytes, objects;
        UnionAccess(Class<?> type) {
            MethodHandle b = null, o = null;
            try {
                Field bytes = type.getField("_bytes"), objects = type.getField("_objects");
                if (bytes.getType() == byte[].class && objects.getType() == Object[].class) {
                    b = MethodHandles.lookup().unreflectGetter(bytes).asType(MethodType.methodType(byte[].class, Object.class));
                    o = MethodHandles.lookup().unreflectGetter(objects).asType(MethodType.methodType(Object[].class, Object.class));
                }
            } catch (NoSuchFieldException ignored) {
            } catch (IllegalAccessException error) { throw failure(error); }
            this.bytes = b;
            this.objects = o;
        }
    }

    private static RuntimeException failure(Throwable error) {
        if (error instanceof RuntimeException) return (RuntimeException) error;
        if (error instanceof Error) throw (Error) error;
        return new IllegalStateException("Rust memory codec failed", error);
    }
}
