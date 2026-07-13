package org.rustlang.core;

import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;
import org.rustlang.core.fmt.Arguments__;
import org.rustlang.core.fmt.rt.Argument__;
import org.rustlang.runtime.Pointer;

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

    public static Argument__ new_display(Object value) {
        String text = String.valueOf(value);
        Argument__ argument = new Argument__(text);
        DISPLAY_VALUES.put(argument, text);
        return argument;
    }

    public static Arguments__ new_arguments(short[] template, Object[] args) {
        String message = formatArgs(template, args);
        Arguments__ arguments = new Arguments__(template, args, message);
        ARGUMENT_MESSAGES.put(arguments, message);
        return arguments;
    }

    public static Arguments__ arguments_from_str(String message) {
        Arguments__ arguments = Arguments__.from_str(message);
        ARGUMENT_MESSAGES.put(arguments, message);
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

    public static int compare_bytes(Pointer left, Pointer right, int len) {
        for (int i = 0; i < len; i++) {
            int a = left.offset(i).getI16() & 0xff;
            int b = right.offset(i).getI16() & 0xff;
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

        if (arg instanceof Argument__) {
            Object value = ((Argument__) arg).value;
            return value == null ? "" : value.toString();
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

    public static String formatArgumentType(Object ty) {
        Object placeholder = readField(ty, "value");
        if (placeholder == null) {
            placeholder = readField(ty, "field0");
        }

        Object unwrapped = unwrapNonNull(placeholder);
        return unwrapped == null ? "" : unwrapped.toString();
    }

    private static Object unwrapNonNull(Object value) {
        Object pointer = readField(value, "pointer");
        if (pointer instanceof Pointer) {
            return ((Pointer) pointer).getObject();
        }
        return pointer == null ? value : pointer;
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
