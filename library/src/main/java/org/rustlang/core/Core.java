package org.rustlang.core;

import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;

public final class Core {
    private Core() {}

    public static short[] toShortArray(String value) {
        char[] chars = value.toCharArray();
        short[] out = new short[chars.length];
        for (int i = 0; i < chars.length; i++) {
            // Full 16-bit char -> short reinterpretation (no 0xff masking).
            out[i] = (short) chars[i];
        }
        return out;
    }

    public static String fromShortArray(short[] value) {
        StringBuilder sb = new StringBuilder(value.length);
        for (short s : value) {
            sb.append((char) s);
        }
        return sb.toString();
    }

    public static String formatArgs(short[] template, Object[] args) {
        String raw;
        if (template == null) {
            raw = "";
        } else {
            StringBuilder sb = new StringBuilder(template.length);
            for (short s : template) {
                sb.append((char) s);
            }
            raw = sb.toString();
        }
        return fillFormatTemplate(raw, args);
    }

    public static String formatArguments(String message, Object template, Object args) {
        if (message != null) {
            return message;
        }

        Object templateValue = unwrapNonNull(template);
        String raw;
        if (templateValue instanceof short[]) {
            short[] shorts = (short[]) templateValue;
            StringBuilder sb = new StringBuilder(shorts.length);
            for (short s : shorts) {
                sb.append((char) s);
            }
            raw = sb.toString();
        } else if (templateValue instanceof byte[]) {
            raw = new String((byte[]) templateValue, StandardCharsets.UTF_8);
        } else if (templateValue == null) {
            raw = "";
        } else {
            raw = templateValue.toString();
        }

        Object argValues = unwrapNonNull(args);
        Object[] argArray;
        // Kotlin `is Array<*>` matches reference arrays only, not primitive arrays.
        if (argValues instanceof Object[]) {
            Object[] in = (Object[]) argValues;
            argArray = new Object[in.length];
            for (int i = 0; i < in.length; i++) {
                argArray[i] = stringifyFormatArg(in[i]);
            }
        } else {
            argArray = new Object[0];
        }
        return fillFormatTemplate(raw, argArray);
    }

    private static String fillFormatTemplate(String raw, Object[] args) {
        String format = raw.startsWith("#") ? raw.substring(1) : raw;
        StringBuilder result = new StringBuilder();
        int argIndex = 0;
        int index = 0;

        while (index < format.length()) {
            char ch = format.charAt(index);
            // Placeholder marker: 0xC0 followed by 0x00.
            if (ch == 0xC0 && index + 1 < format.length() && format.charAt(index + 1) == 0) {
                Object arg = (args != null && argIndex < args.length) ? args[argIndex] : null;
                result.append(arg != null ? arg.toString() : "");
                argIndex += 1;
                index += 2;
            } else {
                result.append(ch);
                index += 1;
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
        Object placeholderValue = readField(ty, "value");
        if (placeholderValue == null) {
            placeholderValue = readField(ty, "field0");
        }
        Object unwrapped = unwrapNonNull(placeholderValue);
        return unwrapped != null ? unwrapped.toString() : "";
    }

    private static Object unwrapNonNull(Object value) {
        Object pointer = readField(value, "pointer");
        return pointer != null ? pointer : value;
    }

    private static Object readField(Object value, String name) {
        if (value == null) {
            return null;
        }
        try {
            Field field = value.getClass().getDeclaredField(name);
            field.setAccessible(true);
            return field.get(value);
        } catch (ReflectiveOperationException e) {
            return null;
        }
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
}
