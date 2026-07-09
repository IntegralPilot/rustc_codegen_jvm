package org.rustlang.core;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;

public final class Core {
    private static final Map<Object, String> DISPLAY_VALUES =
            Collections.synchronizedMap(new WeakHashMap<Object, String>());
    private static final Map<Object, String> ARGUMENT_MESSAGES =
            Collections.synchronizedMap(new WeakHashMap<Object, String>());

    private Core() {}

    public static short[] toShortArray(String value) {
        char[] chars = value.toCharArray();
        short[] result = new short[chars.length];
        for (int i = 0; i < chars.length; i++) {
            result[i] = (short) chars[i];
        }
        return result;
    }

    public static String fromShortArray(short[] value) {
        StringBuilder result = new StringBuilder(value.length);
        for (short ch : value) {
            result.append((char) ch);
        }
        return result.toString();
    }

    public static String formatArgs(short[] template, Object[] args) {
        return fillFormatTemplate(template == null ? "" : fromShortArray(template), formatArgArray(args));
    }

    public static String formatArguments(String message, Object template, Object args) {
        if (message != null) {
            return message;
        }

        String raw = formatTemplateString(unwrapNonNull(template));
        Object[] values = formatArgArray(unwrapNonNull(args));
        return fillFormatTemplate(raw, values);
    }

    public static String formatArgumentObject(Object args) {
        if (args == null) {
            return "";
        }

        Object message = readField(args, "message");
        if (message != null) {
            return message.toString();
        }

        String rememberedMessage = ARGUMENT_MESSAGES.get(args);
        if (rememberedMessage != null) {
            return rememberedMessage;
        }

        return formatArguments(null, readField(args, "template"), readField(args, "args"));
    }

    public static Object new_display(Object value) {
        String text = String.valueOf(value);
        Object argument = constructGenerated("org.rustlang.core.fmt.rt.Argument__", text);
        if (argument != null) {
            DISPLAY_VALUES.put(argument, text);
            setFieldIfPresent(argument, "value", text);
        }
        return argument;
    }

    public static Object new_arguments(short[] template, Object[] args) {
        String message = formatArgs(template, args);
        Object arguments = constructArguments(template, args, message);
        if (arguments != null) {
            ARGUMENT_MESSAGES.put(arguments, message);
            setFieldIfPresent(arguments, "message", message);
        }
        return arguments;
    }

    public static Object arguments_from_str(String message) {
        Object arguments = constructArguments(null, null, message);
        if (arguments != null) {
            ARGUMENT_MESSAGES.put(arguments, message);
            setFieldIfPresent(arguments, "message", message);
        }
        return arguments;
    }

    public static void panic(String arg) {
        throw new RuntimeException("Rust panic: " + arg);
    }

    public static void panic_fmt(Object args) {
        throw new RuntimeException("Rust panic: " + formatArgumentObject(args));
    }

    public static int compare_bytes(short[] left, short[] right, int len) {
        for (int i = 0; i < len; i++) {
            int a = left[i] & 0xff;
            int b = right[i] & 0xff;
            if (a != b) {
                return a - b;
            }
        }
        return 0;
    }

    public static short[][] encode_utf8_raw(long code, short[][] dst) {
        byte[] bytes = new String(Character.toChars((int) code)).getBytes(StandardCharsets.UTF_8);
        short[] encoded = new short[bytes.length];
        for (int i = 0; i < bytes.length; i++) {
            encoded[i] = (short) (bytes[i] & 0xff);
        }
        dst[0] = encoded;
        return dst;
    }

    public static boolean starts_with(short[] value, short[] prefix) {
        if (prefix.length > value.length) {
            return false;
        }
        for (int i = 0; i < prefix.length; i++) {
            if (value[i] != prefix[i]) {
                return false;
            }
        }
        return true;
    }

    private static String formatTemplateString(Object value) {
        if (value instanceof short[]) {
            return fromShortArray((short[]) value);
        }
        if (value instanceof byte[]) {
            return new String((byte[]) value, StandardCharsets.UTF_8);
        }
        return value == null ? "" : value.toString();
    }

    private static Object[] formatArgArray(Object value) {
        if (!(value instanceof Object[])) {
            return new Object[0];
        }

        Object[] input = (Object[]) value;
        Object[] result = new Object[input.length];
        for (int i = 0; i < input.length; i++) {
            result[i] = stringifyFormatArg(input[i]);
        }
        return result;
    }

    private static String fillFormatTemplate(String raw, Object[] args) {
        String format = raw.startsWith("#") ? raw.substring(1) : raw;
        StringBuilder result = new StringBuilder();
        int argIndex = 0;
        int index = 0;

        while (index < format.length()) {
            char ch = format.charAt(index);
            if (ch == 0xC0 && index + 1 < format.length() && format.charAt(index + 1) == 0) {
                Object arg = args != null && argIndex < args.length ? args[argIndex] : null;
                result.append(arg == null ? "" : arg);
                argIndex++;
                index += 2;
            } else {
                result.append(ch);
                index++;
            }
        }

        return result.toString();
    }

    private static String stringifyFormatArg(Object arg) {
        String rememberedValue = DISPLAY_VALUES.get(arg);
        if (rememberedValue != null) {
            return rememberedValue;
        }

        Object value = readField(arg, "value");
        if (value != null) {
            return value.toString();
        }

        Object ty = readField(arg, "ty");
        Object placeholder = readField(ty, "value");
        if (placeholder == null) {
            placeholder = readField(ty, "field0");
        }

        Object unwrapped = unwrapNonNull(placeholder);
        return unwrapped == null ? "" : unwrapped.toString();
    }

    private static Object constructArguments(short[] template, Object[] args, String message) {
        Class<?> argumentsClass = findClass("org.rustlang.core.fmt.Arguments__");
        if (argumentsClass == null) {
            return null;
        }

        Object templateValue = wrapFieldValue(argumentsClass, "template", template);
        Object argsValue = wrapFieldValue(argumentsClass, "args", args);
        return construct(argumentsClass, templateValue, argsValue, message);
    }

    private static Object wrapFieldValue(Class<?> ownerClass, String fieldName, Object value) {
        Field field = findField(ownerClass, fieldName);
        if (field == null || value == null || field.getType().isInstance(value)) {
            return value;
        }
        return construct(field.getType(), value);
    }

    private static Object constructGenerated(String className, Object... preferredValues) {
        Class<?> generatedClass = findClass(className);
        return generatedClass == null ? null : construct(generatedClass, preferredValues);
    }

    private static Object construct(Class<?> type, Object... preferredValues) {
        Constructor<?>[] constructors = type.getConstructors();
        for (Constructor<?> constructor : constructors) {
            Object[] args = constructorArgs(constructor.getParameterTypes(), preferredValues);
            if (args == null) {
                continue;
            }
            try {
                return constructor.newInstance(args);
            } catch (ReflectiveOperationException | RuntimeException ignored) {
                // Try the next public constructor.
            }
        }
        return null;
    }

    private static Object[] constructorArgs(Class<?>[] parameterTypes, Object[] preferredValues) {
        Object[] args = new Object[parameterTypes.length];
        boolean[] used = new boolean[preferredValues.length];

        for (int i = 0; i < parameterTypes.length; i++) {
            Class<?> parameterType = parameterTypes[i];
            int valueIndex = firstCompatibleValue(parameterType, preferredValues, used);
            if (valueIndex >= 0) {
                args[i] = preferredValues[valueIndex];
                used[valueIndex] = true;
            } else if (parameterType.isPrimitive()) {
                args[i] = primitiveDefault(parameterType);
            } else {
                args[i] = null;
            }
        }

        return args;
    }

    private static int firstCompatibleValue(
            Class<?> parameterType,
            Object[] preferredValues,
            boolean[] used
    ) {
        for (int i = 0; i < preferredValues.length; i++) {
            if (!used[i] && isCompatible(parameterType, preferredValues[i])) {
                return i;
            }
        }
        return -1;
    }

    private static boolean isCompatible(Class<?> parameterType, Object value) {
        if (value == null) {
            return !parameterType.isPrimitive();
        }
        if (parameterType.isPrimitive()) {
            return primitiveWrapper(parameterType).isInstance(value);
        }
        return parameterType.isInstance(value);
    }

    private static Class<?> primitiveWrapper(Class<?> primitiveType) {
        if (primitiveType == boolean.class) {
            return Boolean.class;
        }
        if (primitiveType == byte.class) {
            return Byte.class;
        }
        if (primitiveType == short.class) {
            return Short.class;
        }
        if (primitiveType == char.class) {
            return Character.class;
        }
        if (primitiveType == int.class) {
            return Integer.class;
        }
        if (primitiveType == long.class) {
            return Long.class;
        }
        if (primitiveType == float.class) {
            return Float.class;
        }
        if (primitiveType == double.class) {
            return Double.class;
        }
        return Void.class;
    }

    private static Object primitiveDefault(Class<?> primitiveType) {
        if (primitiveType == boolean.class) {
            return false;
        }
        if (primitiveType == byte.class) {
            return (byte) 0;
        }
        if (primitiveType == short.class) {
            return (short) 0;
        }
        if (primitiveType == char.class) {
            return (char) 0;
        }
        if (primitiveType == int.class) {
            return 0;
        }
        if (primitiveType == long.class) {
            return 0L;
        }
        if (primitiveType == float.class) {
            return 0.0f;
        }
        if (primitiveType == double.class) {
            return 0.0d;
        }
        return null;
    }

    private static Object unwrapNonNull(Object value) {
        Object pointer = readField(value, "pointer");
        return pointer == null ? value : pointer;
    }

    private static Class<?> findClass(String name) {
        try {
            return Class.forName(name);
        } catch (ClassNotFoundException | RuntimeException e) {
            return null;
        }
    }

    private static Field findField(Class<?> type, String name) {
        Class<?> current = type;
        while (current != null) {
            try {
                Field field = current.getDeclaredField(name);
                field.setAccessible(true);
                return field;
            } catch (ReflectiveOperationException | RuntimeException e) {
                current = current.getSuperclass();
            }
        }
        return null;
    }

    private static void setFieldIfPresent(Object value, String name, Object fieldValue) {
        if (value == null) {
            return;
        }
        Field field = findField(value.getClass(), name);
        if (field == null) {
            return;
        }
        try {
            field.set(value, fieldValue);
        } catch (ReflectiveOperationException | RuntimeException ignored) {
            // The generated class layout may not have this field; remembered maps still cover it.
        }
    }

    private static Object readField(Object value, String name) {
        if (value == null) {
            return null;
        }
        Field field = findField(value.getClass(), name);
        if (field == null) {
            return null;
        }
        try {
            return field.get(value);
        } catch (ReflectiveOperationException | RuntimeException e) {
            return null;
        }
    }
}
