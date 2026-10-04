package org.rustlang.runtime;

import java.util.HashMap;
import java.util.Map;

/** Nested storage identities come from class metadata, not binary-name suffixes. */
final class RustClasses {
    private static final ClassValue<Map<String, Class<?>>> NESTED = new ClassValue<Map<String, Class<?>>>() {
        @Override
        protected Map<String, Class<?>> computeValue(Class<?> owner) {
            Map<String, Class<?>> classes = new HashMap<>();
            for (Class<?> nested : owner.getDeclaredClasses()) {
                classes.put(nested.getSimpleName(), nested);
            }
            return classes;
        }
    };

    private RustClasses() {}

    static boolean hasNested(Class<?> owner, String first, String second) {
        Map<String, Class<?>> classes = NESTED.get(owner);
        return classes.containsKey(first) && classes.containsKey(second);
    }

    static Class<?> nested(Class<?> owner, String name) throws ClassNotFoundException {
        Class<?> nested = NESTED.get(owner).get(name);
        if (nested == null) throw new ClassNotFoundException(owner.getName() + " nested " + name);
        return nested;
    }
}
