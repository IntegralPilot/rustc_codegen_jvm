package org.rustlang.runtime;

/** Owns a typed Rust location. Code carries its byte offset separately.
 * Create a Pointer only when an operation needs the general memory API.
 */
class Storage extends Pointer.Cell {
    final int size;
    final String codec;
    final long metadata;
    final String scalarLayout;
    private volatile StorageLayout layout;
    volatile Pointer boundary;

    Storage(Object value, int size, String codec, long metadata, String scalarLayout) {
        super(value);
        this.size = size;
        this.codec = codec;
        this.metadata = metadata;
        this.scalarLayout = scalarLayout;
    }

    StorageLayout layout(Object value) {
        StorageLayout current = layout;
        if (current == null && value != null && scalarLayout != null) {
            layout = current = StorageLayout.of(value.getClass(), scalarLayout);
        }
        return current;
    }

    Pointer boundary() {
        Pointer result = boundary;
        if (result == null) {
            synchronized (this) {
                result = boundary;
                if (result == null) {
                    materialize();
                    result = Pointer.storageBoundary(this);
                    boundary = result;
                }
            }
        }
        return result;
    }

    void materialize() {}
}
