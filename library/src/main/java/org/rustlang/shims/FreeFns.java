package org.rustlang.shims;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public final class FreeFns {
    private FreeFns() {}

    public static void panic_fmt(Object args) {
        throw new RuntimeException("Rust panic: " + args);
    }

    public static boolean eq_String_String(String a, String b) {
        return Objects.equals(a, b);
    }

    public static boolean starts_with_char(String s, char ch) {
        return s.startsWith(String.valueOf(ch));
    }

    /** Structural equality for compiler-generated data classes and enums. */
    public static boolean eq_Object_Object(Object a, Object b) {
        if (a == b) {
            return true;
        }
        if (a == null || b == null) {
            return false;
        }
        if (a.getClass() != b.getClass()) {
            return false;
        }

        Field[] declared = a.getClass().getDeclaredFields();
        List<Field> fields = new ArrayList<>();
        for (Field f : declared) {
            if (!f.isSynthetic() && !Modifier.isStatic(f.getModifiers())) {
                fields.add(f);
            }
        }
        fields.sort(Comparator.comparing(Field::getName));

        try {
            for (Field f : fields) {
                f.setAccessible(true);
                Object av = f.get(a);
                Object bv = f.get(b);
                if (!eq_Object_Object(av, bv)) {
                    return false;
                }
            }
        } catch (IllegalAccessException e) {
            return false;
        }
        return true;
    }
}
