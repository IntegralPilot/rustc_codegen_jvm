package org.rustlang.runtime;

/** One allocation-owned projection recipe, shared by every reference to a field. */
final class BorrowedFieldPath {
    final int offset, size;
    final boolean view;
    final String codec;
    final RustField[] fields;

    BorrowedFieldPath(Class<?> type, int offset, int size, String path, boolean view, String codec)
            throws ReflectiveOperationException {
        this.offset = offset;
        this.size = size;
        this.view = view;
        this.codec = codec;
        String[] names = path.split("/");
        fields = new RustField[names.length];
        for (int i = 0; i < names.length; i++) {
            fields[i] = RustField.find(type, names[i]);
            type = fields[i].getType();
        }
    }

    RustField field() { return fields[fields.length - 1]; }

    private Object owner(Object root) throws IllegalAccessException {
        for (int i = 0; i < fields.length - 1; i++) root = fields[i].get(root);
        return root;
    }

    Object read(Object root, long[] metadata) {
        try { return field().borrowedParts(owner(root), metadata); }
        catch (IllegalAccessException error) { throw new IllegalStateException(error); }
    }

    void write(Object root, Object backing, long offset, long length) {
        try { field().setBorrowedParts(owner(root), backing, offset, length); }
        catch (IllegalAccessException error) { throw new IllegalStateException(error); }
    }
}
