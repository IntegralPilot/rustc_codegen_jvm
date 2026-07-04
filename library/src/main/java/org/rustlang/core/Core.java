package org.rustlang.core;

import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;

public final class Core {
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
        return fillFormatTemplate(template == null ? "" : fromShortArray(template), args);
    }

    public static String formatArguments(String message, Object template, Object args) {
        if (message != null) {
            return message;
        }

        String raw = formatTemplateString(unwrapNonNull(template));
        Object[] values = formatArgArray(unwrapNonNull(args));
        return fillFormatTemplate(raw, values);
    }

    public static void panic(String arg) {
        throw new RuntimeException("Rust panic: " + arg);
    }

    public static void panic_fmt(Object args) {
        throw new RuntimeException("Rust panic: " + args);
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

    private static Object unwrapNonNull(Object value) {
        Object pointer = readField(value, "pointer");
        return pointer == null ? value : pointer;
    }

    private static Object readField(Object value, String name) {
        if (value == null) {
            return null;
        }
        try {
            Field field = value.getClass().getDeclaredField(name);
            field.setAccessible(true);
            return field.get(value);
        } catch (ReflectiveOperationException | RuntimeException e) {
            return null;
        }
    }
}
