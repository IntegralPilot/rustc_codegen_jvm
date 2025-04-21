package org.rustlang.core

import java.lang.reflect.Field
import java.util.Objects

/**
 * Core functions needed by the Rust JVM backend stdlib shim.
 */
public object Core {
    @JvmStatic
    public fun panic_fmt(message: String?) {
        // note to future self: consider using a more specific Rust exception type if needed
        throw RuntimeException("Rust panic: " + (message ?: "<no message>"))
    }

    @JvmStatic
    public fun arguments_new_const(pieces: Array<String>): String {
        // Concatenate all the string pieces together.
        // This mimics the simplest formatting scenario where pieces are just joined.
        // If the array is empty, it correctly returns an empty string.
        return pieces.joinToString("")
    }

    @JvmStatic
    public fun core_fmt_rt_argument_new_display_i32(value: Int): String {
        // Convert the integer to a string.
        return value.toString()
    }

    @JvmStatic
    public fun core_fmt_rt_argument_new_display(value: Any?): String {
        // Convert the value to a string.
        return value?.toString() ?: "null"
    }

    @JvmStatic
    public fun core_fmt_rt_argument_new_display_bool(value: Boolean): String {
        // Convert the boolean to a string.
        return value.toString()
    }

    @JvmStatic
    public fun core_fmt_rt_argument_new_display_f64(value: Double): String {
        // Convert the double to a string.
        return value.toString()
    }

    @JvmStatic
    public fun arguments_new_v1(pieces: Array<String>, args: Array<Any?>): String {
        val sb = StringBuilder()
        var argIndex = 0
        for (i in pieces.indices) {
            sb.append(pieces[i]) // Append static piece
            if (argIndex < args.size) {
                // Append the corresponding argument (already formatted or convertible via toString)
                sb.append(args[argIndex]?.toString() ?: "null")
                argIndex++
            }
        }
        return sb.toString()
    }

    @JvmStatic 
    public fun to_string(value: Any?): String {
        // Convert the value to a string.
        return value?.toString() ?: "null"
    }

    @JvmStatic
    public fun eq(value1: Any?, value2: Any?): Boolean {
        // 1. Identity and Null checks
        if (value1 === value2) { // Same reference or both null
            return true
        }
        if (value1 == null || value2 == null) { // One is null, the other isn't
            return false
        }

        // Special handling for floating point types
        // Java's .equals() method does not handle NaN correctly (NaN == NaN which is the opposite of what Rust expects)
        if ((value1 is Double || value1 is Float) && (value2 is Double || value2 is Float)) {
            val d1 = if (value1 is Float) value1.toDouble() else value1 as Double
            val d2 = if (value2 is Float) value2.toDouble() else value2 as Double
            // Direct comparison using IEEE 754 rules (NaN != NaN)
            @Suppress("FloatingPointLiteralComparison") // We WANT this specific comparison
            return d1 == d2
        }

        // 2. Handle common Kotlin/Java types where '==' gives value equality
        //    or specific content checks are needed.
        val class1 = value1::class.java
        val class2 = value2::class.java

        if (class1 != class2) {
             // Optimization: If classes are different, they generally can't be equal
             // unless dealing with complex inheritance/interface scenarios not typical here.
             // We might miss edge cases like comparing an Int to a Long with the same value,
             // but Rust's PartialEq usually requires the types to be the same anyway.
             return false
        }

        // Primitives/Wrappers and String -> rely on Kotlin's == (which calls .equals)
        if (value1 is Number || value1 is String || value1 is Boolean || value1 is Char) {
            return value1 == value2 // Uses overridden equals for these types
        }

        // Array Types -> use contentEquals
        if (class1.isArray) {
            return when (value1) {
                is BooleanArray -> value1.contentEquals(value2 as BooleanArray)
                is ByteArray -> value1.contentEquals(value2 as ByteArray)
                is CharArray -> value1.contentEquals(value2 as CharArray)
                is ShortArray -> value1.contentEquals(value2 as ShortArray)
                is IntArray -> value1.contentEquals(value2 as IntArray)
                is LongArray -> value1.contentEquals(value2 as LongArray)
                is FloatArray -> value1.contentEquals(value2 as FloatArray)
                is DoubleArray -> value1.contentEquals(value2 as DoubleArray)
                is Array<*> -> value1.contentEquals(value2 as Array<*>) // For object arrays
                else -> false // Should not happen if class1.isArray is true
            }
        }

        // 3. Custom Generated Types (Heuristic: Not standard Java/Kotlin & same class)
        // This assumes generated classes are not in standard packages.
        val packageName = class1.packageName ?: ""
        if (!packageName.startsWith("java.") && !packageName.startsWith("kotlin.")) {
            try {
                // Get all declared fields (including private) from the class definition
                val fields: Array<Field> = class1.declaredFields

                for (field in fields) {
                     // Allow accessing private fields for comparison
                    field.isAccessible = true

                    // Recursively compare the field values
                    val fieldValue1 = field.get(value1)
                    val fieldValue2 = field.get(value2)

                    // Recursive Call
                    if (!eq(fieldValue1, fieldValue2)) {
                        // If any field is not equal, the objects are not equal
                        return false
                    }
                }
                // If all fields were equal, the objects are equal
                return true
            } catch (e: Exception) {
                // Log the exception if needed (e.g., IllegalAccessException)
                System.err.println("Reflection error during eq: ${e.message}")
                // Fallback: If reflection fails, assume not equal for safety
                return false
            }
        }

        // 4. Fallback: If none of the above, assume not equal.
        return false
    }

    @JvmStatic
    public fun core_panic(message: String?) {
        // This is a placeholder for the panic function.
        // In a real implementation, this would handle the panic appropriately.
        throw RuntimeException("Rust panic: " + (message ?: "<no message>"))
    }

    @JvmStatic
    public fun core_assert_failed(message: String?) {
        // This is a placeholder for the assert failed function.
        // In a real implementation, this would handle the assertion failure appropriately.
        throw AssertionError("Rust assertion failed: " + (message ?: "<no message>"))
    }

    @JvmStatic
    public fun deref(value: Any?): Any? {
        // This is a placeholder for dereferencing.
        return value
    }

    @JvmStatic 
    fun core_starts_with(value: Any, prefixChar: Int): Boolean {
        // Convert int to char/String and call original or implement directly
        return value.toString().startsWith(Char(prefixChar).toString())
    }

    @JvmStatic
    public fun option_unwrap(optionObj: Any?): Any? {
        if (optionObj == null) {
             // This shouldn't happen if the codegen is correct, as unwrap is called on an instance.
             panic_fmt("FATAL: Called option_unwrap on a null reference. This indicates a bug in the code generator.")
             // Need a return path for the compiler, even though panic throws.
             throw RuntimeException("Unreachable after panic")
        }

        // Determine the variant using instanceof (Kotlin 'is')
        if (optionObj::class.java.name.endsWith("option\$some")) {
             // It's Some(value). Extract the value from field0.
             try {
                 // Use reflection to get the field, assuming we don't know the exact class type statically
                 // but know the naming convention.
                 val field = optionObj::class.java.getDeclaredField("field0")
                 field.isAccessible = true // Ensure we can access it even if not public
                 return field.get(optionObj)
             } catch (e: NoSuchFieldException) {
                 panic_fmt("Internal Compiler Error: option\$some class generated without 'field0'. Exception: ${e.message}")
                 throw RuntimeException("Unreachable after panic") // For compiler
             } catch (e: IllegalAccessException) {
                 panic_fmt("Internal Compiler Error: Cannot access 'field0' in generated option\$some class. Exception: ${e.message}")
                 throw RuntimeException("Unreachable after panic") // For compiler
             }
        } else if (optionObj::class.java.name.endsWith("option\$none")) {
             // It's None. Panic with the standard message.
             panic_fmt("called `Option::unwrap()` on a `None` value")
             throw RuntimeException("Unreachable after panic") // For compiler
        } else {
             // Input object was not an expected Option variant. This indicates a codegen bug.
             val className = optionObj::class.java.name
             panic_fmt("Internal Compiler Error: Called option_unwrap on an unexpected type: $className. Expected type ending in option\$some or option\$none.")
             throw RuntimeException("Unreachable after panic") // For compiler
        }
    }

    @JvmStatic
    // same as unwrap but return true/false if None instead of exception
    public fun option_is_none(optionObj: Any?): Boolean {
        if (optionObj == null) {
            // This shouldn't happen if the codegen is correct, as unwrap is called on an instance.
            panic_fmt("FATAL: Called option_is_none on a null reference. This indicates a bug in the code generator.")
            // Need a return path for the compiler, even though panic throws.
            throw RuntimeException("Unreachable after panic")
        }

        // Determine the variant using instanceof (Kotlin 'is')
        if (optionObj::class.java.name.endsWith("option\$some")) {
            // It's Some(value). Return false.
            return false
        } else if (optionObj::class.java.name.endsWith("option\$none")) {
            // It's None. Return true.
            return true
        } else {
            // Input object was not an expected Option variant. This indicates a codegen bug.
            val className = optionObj::class.java.name
            panic_fmt("Internal Compiler Error: Called option_is_none on an unexpected type: $className. Expected type ending in option\$some or option\$none.")
            throw RuntimeException("Unreachable after panic") // For compiler
        }
    }

}