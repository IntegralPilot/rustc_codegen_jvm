package org.rustlang.core

public object Core {
    @JvmStatic
    public fun toShortArray(value: String): ShortArray {
        return value.toCharArray().map { it.code.toShort() }.toShortArray()
    }

    @JvmStatic
    public fun fromShortArray(value: ShortArray): String {
        return value.map { it.toInt().toChar() }.joinToString("")
    }

    @JvmStatic
    public fun formatArgs(template: ShortArray?, args: Array<Any?>?): String {
        val raw = template?.map { it.toInt().toChar() }?.joinToString("") ?: ""
        return fillFormatTemplate(raw, args)
    }

    @JvmStatic
    public fun formatArguments(message: String?, template: Any?, args: Any?): String {
        if (message != null) {
            return message
        }

        val templateValue = unwrapNonNull(template)
        val raw = when (templateValue) {
            is ShortArray -> templateValue.map { it.toInt().toChar() }.joinToString("")
            is ByteArray -> templateValue.toString(Charsets.UTF_8)
            null -> ""
            else -> templateValue.toString()
        }
        val argValues = unwrapNonNull(args)
        val argArray = if (argValues is Array<*>) {
            argValues.map { stringifyFormatArg(it) }.toTypedArray()
        } else {
            emptyArray()
        }
        return fillFormatTemplate(raw, argArray)
    }

    private fun fillFormatTemplate(raw: String, args: Array<out Any?>?): String {
        val format = if (raw.startsWith("#")) raw.drop(1) else raw
        val result = StringBuilder()
        var argIndex = 0
        var index = 0

        while (index < format.length) {
            val ch = format[index]
            if (ch.code == 0xC0 && index + 1 < format.length && format[index + 1].code == 0) {
                result.append(args?.getOrNull(argIndex)?.toString() ?: "")
                argIndex += 1
                index += 2
            } else {
                result.append(ch)
                index += 1
            }
        }

        return result.toString()
    }

    private fun stringifyFormatArg(arg: Any?): String {
        readField(arg, "value")?.let { return it.toString() }
        val ty = readField(arg, "ty")
        val placeholderValue = readField(ty, "value") ?: readField(ty, "field0")
        return unwrapNonNull(placeholderValue)?.toString() ?: ""
    }

    private fun unwrapNonNull(value: Any?): Any? {
        return readField(value, "pointer") ?: value
    }

    private fun readField(value: Any?, name: String): Any? {
        if (value == null) {
            return null
        }
        return try {
            val field = value.javaClass.getDeclaredField(name)
            field.isAccessible = true
            field.get(value)
        } catch (_: ReflectiveOperationException) {
            null
        }
    }

    @JvmStatic
    public fun panic(arg: String?) {
        throw RuntimeException("Rust panic: $arg")
    }

    @JvmStatic
    public fun panic_fmt(args: Any?) {
        throw RuntimeException("Rust panic: $args")
    }

    @JvmStatic
    public fun compare_bytes(left: ShortArray, right: ShortArray, len: Int): Int {
        for (i in 0 until len) {
            val a = left[i].toInt() and 0xff
            val b = right[i].toInt() and 0xff
            if (a != b) {
                return a - b
            }
        }
        return 0
    }

    @JvmStatic
    public fun encode_utf8_raw(code: Long, dst: Array<ShortArray>): Array<ShortArray> {
        val bytes = String(Character.toChars(code.toInt())).toByteArray(Charsets.UTF_8)
        val encoded = ShortArray(bytes.size)
        for (i in bytes.indices) {
            encoded[i] = (bytes[i].toInt() and 0xff).toShort()
        }
        dst[0] = encoded
        return dst
    }

    @JvmStatic
    public fun starts_with(value: ShortArray, prefix: ShortArray): Boolean {
        if (prefix.size > value.size) {
            return false
        }
        for (i in prefix.indices) {
            if (value[i] != prefix[i]) {
                return false
            }
        }
        return true
    }
}
