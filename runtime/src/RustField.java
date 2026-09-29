package org.rustlang.runtime;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.LinkedHashMap;
import java.util.Map;

/** Logical fields of Rust storage, independent of their physical JVM slots. */
public final class RustField {
    private static final ClassValue<Map<String, RustField>> FIELDS =
            new ClassValue<Map<String, RustField>>() {
                protected Map<String, RustField> computeValue(Class<?> type) {
                    Map<String, RustField> result = new LinkedHashMap<>();
                    Field[] fields = type.getFields();
                    for (Field field : fields) {
                        if (Modifier.isStatic(field.getModifiers()) || field.isSynthetic()) continue;
                        Field displacement = null;
                        String shape = null;
                        if (field.getType() == Object.class || field.getType() == long.class) {
                            for (Field candidate : fields) {
                                String name = candidate.getName();
                                if (!candidate.isSynthetic() || Modifier.isStatic(candidate.getModifiers())
                                        || !name.startsWith("$rust$")) continue;
                                int separator = name.indexOf('$', 6);
                                if (separator < 0 || !name.substring(separator + 1).equals(field.getName())) continue;
                                String kind = name.substring(6, separator);
                                if (kind.equals("l")) continue;
                                if (field.getType() == long.class && !kind.equals("t")) continue;
                                if (field.getType() == Object.class && kind.equals("t")) continue;
                                displacement = candidate;
                                shape = kind;
                                break;
                            }
                        }
                        result.put(field.getName(), new RustField(field, displacement, shape));
                    }
                    return result;
                }
            };

    static final ClassValue<RustField[]> ALL = new ClassValue<RustField[]>() {
        protected RustField[] computeValue(Class<?> type) {
            return FIELDS.get(type).values().toArray(new RustField[0]);
        }
    };

    public static RustField find(Class<?> owner, String name) throws NoSuchFieldException {
        RustField field = optional(owner, name);
        if (field == null) throw new NoSuchFieldException(owner.getName() + "." + name);
        return field;
    }

    static RustField optional(Class<?> owner, String name) {
        return FIELDS.get(owner).get(name);
    }

    private final Field root;
    private final Field displacement;
    private final Field length;
    private final int size;
    private final String codec;
    private final boolean typedAddress;
    private final Class<?> logicalType;

    private RustField(Field root, Field displacement, String shape) {
        this.root = root;
        this.displacement = displacement;
        root.setAccessible(true);
        if (displacement != null) displacement.setAccessible(true);
        typedAddress = shape != null && shape.startsWith("a");
        String layoutCodec = null;
        try {
            if (typedAddress) {
                size = Integer.parseInt(shape.substring(1));
                length = null;
                logicalType = Pointer.class;
                try {
                    Field metadata = root.getDeclaringClass().getField("$rust$c$" + root.getName());
                    if (metadata.getType() != String.class || !Modifier.isStatic(metadata.getModifiers())) {
                        throw new IllegalStateException("invalid address layout " + metadata);
                    }
                    layoutCodec = (String) metadata.get(null);
                } catch (NoSuchFieldException absent) {
                    // Scalar or untyped byte layouts have no codec constant.
                }
            } else if ("t".equals(shape)) {
                size = 0; length = null; logicalType = TaggedLong.class;
            } else if ("s".equals(shape) || "u".equals(shape)) {
                size = 0;
                length = root.getDeclaringClass().getField("$rust$l$" + root.getName());
                length.setAccessible(true);
                logicalType = shape.equals("u") ? Utf8View.class : SliceView.class;
            } else {
                size = shape == null ? 0 : Integer.parseInt(shape);
                length = null;
                logicalType = displacement == null ? root.getType() : Pointer.class;
            }
        } catch (ReflectiveOperationException error) {
            throw new IllegalStateException("invalid borrowed field " + root, error);
        }
        codec = layoutCodec;
    }

    public String getName() { return root.getName(); }
    public Class<?> getType() { return logicalType; }
    public Class<?> getDeclaringClass() { return root.getDeclaringClass(); }
    boolean isBorrowed() { return displacement != null && logicalType != TaggedLong.class; }
    boolean borrowedShape(boolean view) {
        return isBorrowed() && (length != null) == view;
    }

    /** Read the physical fields without constructing a logical carrier. */
    Object borrowedParts(Object owner, long[] metadata) throws IllegalAccessException {
        Object value = root.get(owner);
        metadata[0] = length == null ? displacement.getLong(owner) : displacement.getInt(owner);
        if (length != null) metadata[1] = length.getLong(owner);
        return value;
    }
    void setBorrowedParts(Object owner, Object value, long offset, long count) throws IllegalAccessException {
        if (length == null) displacement.setLong(owner, offset);
        else {
            displacement.setInt(owner, Math.toIntExact(offset));
            length.setLong(owner, count);
        }
        root.set(owner, value);
    }
    MethodHandle getter() throws IllegalAccessException {
        if (displacement == null) return MethodHandles.lookup().unreflectGetter(root);
        return access("get", MethodType.methodType(Object.class, Object.class));
    }
    MethodHandle setter() throws IllegalAccessException {
        if (displacement == null) return MethodHandles.lookup().unreflectSetter(root);
        return access("set", MethodType.methodType(void.class, Object.class, Object.class));
    }
    private MethodHandle access(String name, MethodType type) throws IllegalAccessException {
        try { return MethodHandles.lookup().findVirtual(RustField.class, name, type).bindTo(this); }
        catch (NoSuchMethodException error) { throw new AssertionError(error); }
    }
    public int getInt(Object owner) throws IllegalAccessException { return root.getInt(owner); }

    public Object get(Object owner) throws IllegalAccessException {
        Object value = root.get(owner);
        if (displacement == null) return value;
        if (logicalType == TaggedLong.class) return new TaggedLong(((Long) value).longValue(), displacement.getLong(owner));
        if (length == null) return typedAddress
                ? Pointer.fromTypedStorageLocation(value, displacement.getLong(owner), size, codec)
                : Pointer.addressFromParts(value, displacement.getLong(owner), size);
        return SliceView.create(logicalType, value, displacement.getInt(owner), length.getLong(owner));
    }

    public void set(Object owner, Object value) throws IllegalAccessException {
        if (logicalType == TaggedLong.class && displacement != null) {
            TaggedLong tagged = (TaggedLong) value;
            root.setLong(owner, TaggedLong.value(tagged));
            displacement.setLong(owner, TaggedLong.tag(tagged));
            return;
        }
        if (length != null) {
            displacement.setInt(owner, viewStart(value));
            length.setLong(owner, viewLength(value));
            value = viewRoot(value);
        } else if (displacement != null) {
            value = (Pointer) value;
            displacement.setLong(owner, 0);
        }
        root.set(owner, value);
    }

    public static Object viewRoot(Object value) {
        return value == null ? null : ((SliceView) value).array;
    }
    public static int viewStart(Object value) {
        return value == null ? 0 : ((SliceView) value).offset;
    }
    public static long viewLength(Object value) {
        return value == null ? 0 : ((SliceView) value).rustLength;
    }
}
