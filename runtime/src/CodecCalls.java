package org.rustlang.runtime;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodType;

/** One adapter class per operation, shared by every exact Rust layout. */
final class CodecCalls {
    private CodecCalls() {}

    interface Encoder { byte[] encode(Object value); }
    interface RangeEncoder { void encode(Object value, byte[] bytes, int offset); }
    interface Decoder { Object decode(byte[] bytes); }
    interface RangeDecoder { Object decode(byte[] bytes, int offset); }
    interface Binder { void bind(Pointer pointer, Object value); }

    static Encoder wholeEncoder(RangeEncoder operation, int byteSize) {
        return value -> {
            byte[] bytes = new byte[byteSize];
            operation.encode(value, bytes, 0);
            return bytes;
        };
    }

    static Decoder wholeDecoder(RangeDecoder operation) {
        return bytes -> operation.decode(bytes, 0);
    }

    static Encoder encoder(MethodHandle target) {
        MethodHandle call = target.asType(MethodType.methodType(byte[].class, Object.class));
        return value -> {
            try { return (byte[]) call.invokeExact(value); }
            catch (Throwable error) { throw failure(error); }
        };
    }

    static RangeEncoder rangeEncoder(MethodHandle target) {
        MethodHandle call = target.asType(MethodType.methodType(
                void.class, Object.class, byte[].class, int.class));
        return (value, bytes, offset) -> {
            try { call.invokeExact(value, bytes, offset); }
            catch (Throwable error) { throw failure(error); }
        };
    }

    static Decoder decoder(MethodHandle target) {
        MethodHandle call = target.asType(MethodType.methodType(Object.class, byte[].class));
        return bytes -> {
            try { return (Object) call.invokeExact(bytes); }
            catch (Throwable error) { throw failure(error); }
        };
    }

    static RangeDecoder rangeDecoder(MethodHandle target) {
        MethodHandle call = target.asType(MethodType.methodType(Object.class, byte[].class, int.class));
        return (bytes, offset) -> {
            try { return (Object) call.invokeExact(bytes, offset); }
            catch (Throwable error) { throw failure(error); }
        };
    }

    static Binder binder(MethodHandle target) {
        MethodHandle call = target.asType(MethodType.methodType(void.class, Pointer.class, Object.class));
        return (pointer, value) -> {
            try { call.invokeExact(pointer, value); }
            catch (Throwable error) { throw failure(error); }
        };
    }

    private static RuntimeException failure(Throwable error) {
        if (error instanceof RuntimeException) return (RuntimeException) error;
        if (error instanceof Error) throw (Error) error;
        return new IllegalStateException("Rust codec threw a checked exception", error);
    }
}
