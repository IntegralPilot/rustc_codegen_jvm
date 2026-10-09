package org.rustlang.runtime;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.AbstractMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/** Identity-keyed weak table. Callers synchronize every operation externally. */
final class WeakIdentityMap<V> extends AbstractMap<Object, V> {
    private static final class OpenWeakReference extends WeakReference<Object> {
        private final int identityHash;

        private OpenWeakReference(Object key, ReferenceQueue<Object> queue) {
            super(key, queue);
            identityHash = System.identityHashCode(key);
        }
    }

    private final ReferenceQueue<Object> collectedKeys = new ReferenceQueue<>();
    private OpenWeakReference[] keys;
    private Object[] values;
    private int size;
    private int used;
    private int readsUntilCleanup = 256;

    WeakIdentityMap() {
        this(16);
    }

    WeakIdentityMap(int initialCapacity) {
        int capacity = 16;
        while (capacity < initialCapacity) {
            capacity <<= 1;
        }
        keys = new OpenWeakReference[capacity];
        values = new Object[capacity];
    }

    private static int tableIndex(int hash, int length) {
        hash ^= hash >>> 16;
        hash *= 0x7feb352d;
        hash ^= hash >>> 15;
        return hash & (length - 1);
    }

    /** Removes an entry without leaving a tombstone in its probe chain. */
    private void deleteEntry(int deleted) {
        if (values[deleted] != null) {
            size--;
        }
        keys[deleted] = null;
        values[deleted] = null;
        used--;

        int mask = keys.length - 1;
        for (int index = (deleted + 1) & mask;
                keys[index] != null;
                index = (index + 1) & mask) {
            OpenWeakReference reference = keys[index];
            int home = tableIndex(reference.identityHash, keys.length);
            if ((index < home && (home <= deleted || deleted <= index))
                    || (home <= deleted && deleted <= index)) {
                keys[deleted] = reference;
                values[deleted] = values[index];
                keys[index] = null;
                values[index] = null;
                deleted = index;
            }
        }
    }

    private void reset() {
        keys = new OpenWeakReference[16];
        values = new Object[16];
        size = 0;
        used = 0;
        readsUntilCleanup = 256;
        while (collectedKeys.poll() != null) {
            // Entries no longer exist after reset.
        }
    }

    private void discardCollectedKeys() {
        OpenWeakReference collected;
        while ((collected = (OpenWeakReference) collectedKeys.poll()) != null) {
            int index = tableIndex(collected.identityHash, keys.length);
            while (keys[index] != null) {
                if (keys[index] == collected) {
                    deleteEntry(index);
                    break;
                }
                index = (index + 1) & (keys.length - 1);
            }
        }
    }

    private void maybeDiscardCollectedKeys() {
        if (--readsUntilCleanup == 0) {
            discardCollectedKeys();
            readsUntilCleanup = 256;
        }
    }

    private void rehashForInsert() {
        discardCollectedKeys();
        readsUntilCleanup = 256;
        int newLength =
                size * 4 >= keys.length * 3
                        ? keys.length << 1
                        : keys.length;
        OpenWeakReference[] oldKeys = keys;
        Object[] oldValues = values;
        keys = new OpenWeakReference[newLength];
        values = new Object[newLength];
        size = 0;
        used = 0;
        for (int oldIndex = 0; oldIndex < oldKeys.length; oldIndex++) {
            OpenWeakReference reference = oldKeys[oldIndex];
            Object key = reference == null ? null : reference.get();
            if (key == null) {
                continue;
            }
            int index = tableIndex(reference.identityHash, keys.length);
            while (keys[index] != null) {
                index = (index + 1) & (keys.length - 1);
            }
            keys[index] = reference;
            values[index] = oldValues[oldIndex];
            size++;
            used++;
        }
    }

    @SuppressWarnings("unchecked")
    private V valueAt(int index) {
        return (V) values[index];
    }

    @Override
    public V get(Object key) {
        maybeDiscardCollectedKeys();
        int index = tableIndex(System.identityHashCode(key), keys.length);
        while (true) {
            OpenWeakReference reference = keys[index];
            if (reference == null) {
                return null;
            }
            Object live = reference.get();
            // Keep dead entries in the probe chain until the next queue drain.
            if (live != null && live == key) {
                return valueAt(index);
            }
            index = (index + 1) & (keys.length - 1);
        }
    }

    @Override
    public V put(Object key, V value) {
        discardCollectedKeys();
        readsUntilCleanup = 256;
        if (used * 4 >= keys.length * 3) {
            rehashForInsert();
        }
        int index = tableIndex(System.identityHashCode(key), keys.length);
        while (true) {
            OpenWeakReference reference = keys[index];
            if (reference == null) {
                used++;
                keys[index] = new OpenWeakReference(key, collectedKeys);
                values[index] = value;
                size++;
                return null;
            }
            Object live = reference.get();
            if (live == null) {
                deleteEntry(index);
                continue;
            } else if (live == key) {
                V previous = valueAt(index);
                values[index] = value;
                return previous;
            }
            index = (index + 1) & (keys.length - 1);
        }
    }

    @Override
    public V remove(Object key) {
        discardCollectedKeys();
        readsUntilCleanup = 256;
        int index = tableIndex(System.identityHashCode(key), keys.length);
        while (true) {
            OpenWeakReference reference = keys[index];
            if (reference == null) {
                return null;
            }
            Object live = reference.get();
            if (live == null) {
                deleteEntry(index);
                continue;
            } else if (live == key) {
                V previous = valueAt(index);
                deleteEntry(index);
                if (size == 0) {
                    reset();
                }
                return previous;
            }
            index = (index + 1) & (keys.length - 1);
        }
    }

    @Override
    public int size() {
        discardCollectedKeys();
        for (int index = 0; index < keys.length; ) {
            OpenWeakReference reference = keys[index];
            if (reference != null && reference.get() == null) {
                deleteEntry(index);
            } else {
                index++;
            }
        }
        return size;
    }

    @Override
    public boolean isEmpty() {
        discardCollectedKeys();
        if (size == 0) {
            return true;
        }
        for (int index = 0; index < keys.length; ) {
            OpenWeakReference reference = keys[index];
            if (reference == null) {
                index++;
                continue;
            }
            if (reference.get() != null) {
                return false;
            }
            deleteEntry(index);
        }
        reset();
        return true;
    }

    @Override
    public Set<Map.Entry<Object, V>> entrySet() {
        discardCollectedKeys();
        Set<Map.Entry<Object, V>> entries = new HashSet<>();
        for (int index = 0; index < keys.length; ) {
            OpenWeakReference reference = keys[index];
            Object key = reference == null ? null : reference.get();
            if (key == null) {
                if (reference != null) {
                    deleteEntry(index);
                    continue;
                }
            } else {
                entries.add(new java.util.AbstractMap.SimpleImmutableEntry<>(
                        key, valueAt(index)));
            }
            index++;
        }
        return entries;
    }

    void forEachLiveKey(java.util.function.Consumer<Object> visit) {
        discardCollectedKeys();
        for (int index = 0; index < keys.length; ) {
            OpenWeakReference reference = keys[index];
            Object key = reference == null ? null : reference.get();
            if (key == null) {
                if (reference != null) {
                    deleteEntry(index);
                    continue;
                }
            } else {
                visit.accept(key);
            }
            index++;
        }
    }
}
