package org.rustlang.runtime;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/** Minimal host services used by the JVM standard-library platform overlay. */
public final class RuntimeSupport {
    private static final SecureRandom RANDOM = new SecureRandom();
    private static final Map<String, String> ENVIRONMENT_OVERRIDES = new HashMap<>();
    private static final Set<String> REMOVED_ENVIRONMENT = new HashSet<>();
    private static final AtomicLong NEXT_ENVIRONMENT_SNAPSHOT = new AtomicLong(1);
    private static final ConcurrentHashMap<Long, EnvironmentEntry[]> ENVIRONMENT_SNAPSHOTS =
            new ConcurrentHashMap<>();
    private static volatile String[] arguments = new String[] {"rust-jvm"};

    private static final class EnvironmentEntry {
        private final byte[] key;
        private final byte[] value;

        private EnvironmentEntry(String key, String value) {
            this.key = key.getBytes(StandardCharsets.UTF_8);
            this.value = value.getBytes(StandardCharsets.UTF_8);
        }
    }

    private RuntimeSupport() {}

    public static void initializeArgs(String[] javaArguments) {
        PanicSupport.installMainThreadHandler();
        String[] initialized = new String[javaArguments.length + 1];
        initialized[0] = "rust-jvm";
        System.arraycopy(javaArguments, 0, initialized, 1, javaArguments.length);
        arguments = initialized;
    }

    public static Pointer traitObjectDataPointer(Pointer pointer, long viewSize, String viewCodec) {
        Object value = pointer.directCellValueOrSelf();
        if (value instanceof Pointer && value != pointer) {
            return traitObjectDataPointer((Pointer) value, viewSize, viewCodec);
        }

        boolean unwrappedCarrier = false;
        while (value instanceof TraitObjectCarrier) {
            TraitObjectCarrier carrier = (TraitObjectCarrier) value;
            value = carrier.rustTraitObjectPayload();
            unwrappedCarrier = true;
            if (value instanceof Pointer) {
                return Pointer.dataPointerView((Pointer) value, viewSize, viewCodec);
            }
        }
        return unwrappedCarrier
                ? Pointer.cell(value, viewSize, viewCodec)
                : Pointer.dataPointerView(pointer, viewSize, viewCodec);
    }

    public static long argumentCount() {
        return arguments.length;
    }

    public static long argumentLength(long index) {
        return argumentBytes(index).length;
    }

    public static void copyArgument(long index, Pointer destination) {
        copyBytes(argumentBytes(index), destination);
    }

    private static byte[] argumentBytes(long index) {
        String[] current = arguments;
        int checkedIndex = Math.toIntExact(index);
        if (checkedIndex < 0 || checkedIndex >= current.length) {
            throw new IndexOutOfBoundsException("Rust argument index " + index);
        }
        return current[checkedIndex].getBytes(StandardCharsets.UTF_8);
    }

    public static long environmentValueLength(Pointer keyBytes, long keyLength) {
        String value = environmentValue(utf8(keyBytes, keyLength));
        return value == null ? -1 : value.getBytes(StandardCharsets.UTF_8).length;
    }

    public static void copyEnvironmentValue(
            Pointer keyBytes, long keyLength, Pointer destination) {
        String key = utf8(keyBytes, keyLength);
        String value = environmentValue(key);
        if (value == null) {
            throw new IllegalStateException("environment variable disappeared: " + key);
        }
        copyBytes(value.getBytes(StandardCharsets.UTF_8), destination);
    }

    public static synchronized void setEnvironment(
            Pointer keyBytes, long keyLength, Pointer valueBytes, long valueLength) {
        String key = utf8(keyBytes, keyLength);
        String value = utf8(valueBytes, valueLength);
        validateEnvironmentKey(key);
        ENVIRONMENT_OVERRIDES.put(key, value);
        REMOVED_ENVIRONMENT.remove(key);
    }

    public static synchronized void removeEnvironment(Pointer keyBytes, long keyLength) {
        String key = utf8(keyBytes, keyLength);
        validateEnvironmentKey(key);
        ENVIRONMENT_OVERRIDES.remove(key);
        REMOVED_ENVIRONMENT.add(key);
    }

    private static synchronized String environmentValue(String key) {
        if (ENVIRONMENT_OVERRIDES.containsKey(key)) {
            return ENVIRONMENT_OVERRIDES.get(key);
        }
        if (REMOVED_ENVIRONMENT.contains(key)) {
            return null;
        }
        return System.getenv(key);
    }

    public static synchronized long beginEnvironmentSnapshot() {
        Map<String, String> values = new LinkedHashMap<>(System.getenv());
        for (String removed : REMOVED_ENVIRONMENT) {
            values.remove(removed);
        }
        values.putAll(ENVIRONMENT_OVERRIDES);

        EnvironmentEntry[] entries = new EnvironmentEntry[values.size()];
        int index = 0;
        for (Map.Entry<String, String> entry : values.entrySet()) {
            entries[index++] = new EnvironmentEntry(entry.getKey(), entry.getValue());
        }
        long handle = NEXT_ENVIRONMENT_SNAPSHOT.getAndIncrement();
        ENVIRONMENT_SNAPSHOTS.put(handle, entries);
        return handle;
    }

    public static long environmentSnapshotCount(long handle) {
        return environmentSnapshot(handle).length;
    }

    public static long environmentSnapshotKeyLength(long handle, long index) {
        return environmentEntry(handle, index).key.length;
    }

    public static long environmentSnapshotValueLength(long handle, long index) {
        return environmentEntry(handle, index).value.length;
    }

    public static void copyEnvironmentSnapshotKey(long handle, long index, Pointer destination) {
        copyBytes(environmentEntry(handle, index).key, destination);
    }

    public static void copyEnvironmentSnapshotValue(long handle, long index, Pointer destination) {
        copyBytes(environmentEntry(handle, index).value, destination);
    }

    public static void endEnvironmentSnapshot(long handle) {
        if (ENVIRONMENT_SNAPSHOTS.remove(handle) == null) {
            throw new IllegalStateException("unknown JVM environment snapshot " + handle);
        }
    }

    private static EnvironmentEntry[] environmentSnapshot(long handle) {
        EnvironmentEntry[] entries = ENVIRONMENT_SNAPSHOTS.get(handle);
        if (entries == null) {
            throw new IllegalStateException("unknown JVM environment snapshot " + handle);
        }
        return entries;
    }

    private static EnvironmentEntry environmentEntry(long handle, long index) {
        EnvironmentEntry[] entries = environmentSnapshot(handle);
        int checkedIndex = Math.toIntExact(index);
        if (checkedIndex < 0 || checkedIndex >= entries.length) {
            throw new IndexOutOfBoundsException("environment snapshot index " + index);
        }
        return entries[checkedIndex];
    }

    private static void validateEnvironmentKey(String key) {
        if (key.isEmpty() || key.indexOf('=') >= 0 || key.indexOf('\0') >= 0) {
            throw new IllegalArgumentException("invalid environment variable name");
        }
    }

    public static void fillRandom(Pointer destination, long length) {
        int checkedLength = Math.toIntExact(length);
        byte[] bytes = new byte[checkedLength];
        RANDOM.nextBytes(bytes);
        copyBytes(bytes, destination);
    }

    public static long monotonicNanos() {
        // Bias the signed JVM counter into Rust's ordered u64 Duration range.
        return System.nanoTime() ^ Long.MIN_VALUE;
    }

    public static long unixEpochMillis() {
        return Math.max(0L, System.currentTimeMillis());
    }

    public static void writeStdout(Pointer bytes, long length) {
        byte[] copy = copyFromPointer(bytes, length);
        System.out.write(copy, 0, copy.length);
    }

    public static void writeStderr(Pointer bytes, long length) {
        byte[] copy = copyFromPointer(bytes, length);
        System.err.write(copy, 0, copy.length);
    }

    /** Returns zero at EOF and -1 if the host stream reports an I/O error. */
    public static long readStdin(Pointer destination, long length) {
        int checkedLength = Math.toIntExact(length);
        if (checkedLength == 0) {
            return 0;
        }
        byte[] copy = new byte[checkedLength];
        final int read;
        try {
            read = System.in.read(copy);
        } catch (IOException error) {
            return -1;
        }
        if (read < 0) {
            return 0;
        }
        for (int index = 0; index < read; index++) {
            destination.add(index).set(Byte.valueOf(copy[index]));
        }
        return read;
    }

    public static void flushStdout() {
        System.out.flush();
    }

    public static void flushStderr() {
        System.err.flush();
    }

    public static void exit(int code) {
        System.exit(code);
    }

    private static String utf8(Pointer bytes, long length) {
        return new String(copyFromPointer(bytes, length), StandardCharsets.UTF_8);
    }

    private static byte[] copyFromPointer(Pointer source, long length) {
        int checkedLength = Math.toIntExact(length);
        byte[] copy = new byte[checkedLength];
        for (int index = 0; index < checkedLength; index++) {
            copy[index] = source.add(index).getI8();
        }
        return copy;
    }

    private static void copyBytes(byte[] source, Pointer destination) {
        for (int index = 0; index < source.length; index++) {
            destination.add(index).set(Byte.valueOf(source[index]));
        }
    }
}
