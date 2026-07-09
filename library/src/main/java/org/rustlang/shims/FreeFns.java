package org.rustlang.shims;

import org.rustlang.core.Core;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public final class FreeFns {
    private FreeFns() {}

    public static void panic_fmt(Object args) {
        throw new RuntimeException("Rust panic: " + Core.formatArgumentObject(args));
    }

    public static boolean eq_String_String(String a, String b) {
        return Objects.equals(a, b);
    }

    public static boolean starts_with_char(String value, char prefix) {
        return value.startsWith(String.valueOf(prefix));
    }

    public static boolean eq_Object_Object(Object a, Object b) {
        if (a == b) {
            return true;
        }
        if (a == null || b == null || a.getClass() != b.getClass()) {
            return false;
        }

        Class<?> type = a.getClass();
        if (type.isArray()) {
            return arraysEqual(a, b);
        }
        if (isValueType(type)) {
            return a.equals(b);
        }

        List<Field> fields = instanceFields(type);
        if (fields.isEmpty()) {
            return a.equals(b);
        }

        try {
            for (Field field : fields) {
                field.setAccessible(true);
                if (!eq_Object_Object(field.get(a), field.get(b))) {
                    return false;
                }
            }
            return true;
        } catch (ReflectiveOperationException | RuntimeException e) {
            return a.equals(b);
        }
    }

    private static boolean isValueType(Class<?> type) {
        return type.isPrimitive()
            || Number.class.isAssignableFrom(type)
            || CharSequence.class.isAssignableFrom(type)
            || Boolean.class == type
            || Character.class == type
            || Enum.class.isAssignableFrom(type);
    }

    private static List<Field> instanceFields(Class<?> type) {
        Field[] declared = type.getDeclaredFields();
        List<Field> fields = new ArrayList<>(declared.length);
        for (Field field : declared) {
            int modifiers = field.getModifiers();
            if (!field.isSynthetic() && !Modifier.isStatic(modifiers)) {
                fields.add(field);
            }
        }
        fields.sort(Comparator.comparing(Field::getName));
        return fields;
    }

    private static boolean arraysEqual(Object a, Object b) {
        if (a instanceof Object[] && b instanceof Object[]) {
            return Arrays.deepEquals((Object[]) a, (Object[]) b);
        }
        if (a instanceof byte[] && b instanceof byte[]) {
            return Arrays.equals((byte[]) a, (byte[]) b);
        }
        if (a instanceof short[] && b instanceof short[]) {
            return Arrays.equals((short[]) a, (short[]) b);
        }
        if (a instanceof int[] && b instanceof int[]) {
            return Arrays.equals((int[]) a, (int[]) b);
        }
        if (a instanceof long[] && b instanceof long[]) {
            return Arrays.equals((long[]) a, (long[]) b);
        }
        if (a instanceof char[] && b instanceof char[]) {
            return Arrays.equals((char[]) a, (char[]) b);
        }
        if (a instanceof float[] && b instanceof float[]) {
            return Arrays.equals((float[]) a, (float[]) b);
        }
        if (a instanceof double[] && b instanceof double[]) {
            return Arrays.equals((double[]) a, (double[]) b);
        }
        if (a instanceof boolean[] && b instanceof boolean[]) {
            return Arrays.equals((boolean[]) a, (boolean[]) b);
        }
        return false;
    }
}
