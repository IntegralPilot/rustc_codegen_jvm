package org.rustlang.runtime;

/** Stores local reference components until a boundary is needed.
 * After materialization, Cell.value holds the value shared by all aliases.
 */
final class BorrowedStorage extends Storage {
    private Object root;
    private long offset, length;
    private int plan;
    final boolean view;
    boolean split;

    BorrowedStorage(Object value, int size, String codec, long metadata, String layout, boolean view) {
        super(value, size, codec, metadata, layout);
        this.view = view;
    }

    void store(Object root, long offset, long length, int plan) {
        this.root = root;
        this.offset = offset;
        this.length = length;
        this.plan = plan;
        value = null;
        split = true;
    }

    Object read(long[] metadata, boolean view) {
        if (split) {
            metadata[0] = offset;
            if (view) metadata[1] = length;
            return root;
        }
        metadata[0] = view ? RustField.viewStart((SliceView) value) : 0;
        if (view) metadata[1] = RustField.viewLength((SliceView) value);
        return view ? RustField.viewRoot((SliceView) value) : value;
    }

    @Override void materialize() {
        if (!split) return;
        if (plan < 0) value = plan == -2
                ? new Utf8View(root, Math.toIntExact(offset), length)
                : new SliceView(root, Math.toIntExact(offset), length);
        else value = root == null && offset == 0 ? null : Pointer.addressFromParts(root, offset, plan);
        split = false;
        root = null;
    }
}
