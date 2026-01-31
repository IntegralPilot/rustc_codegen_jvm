package org.rustlang.core

import java.lang.reflect.Field
import java.util.Objects


public interface partialeq {
    
}

/**
 * Core functions needed by the Rust JVM backend stdlib shim.
 */
public object Core {
    @JvmStatic
    public fun panic_fmt(arg: Any?) {
        val message: String = when (arg) {
            // Case 1: It's already a String (maybe from a different panic path)
            is String -> {
                arg ?: "<panic with null message>"
            }
            // Case 2: It's likely the 'arguments' object
            null -> {
                "<panic with null arguments object>"
            }
            else -> {
                // Attempt to interpret it as the 'arguments' structure via reflection
                try {
                    val argClass = arg::class.java // Get the Class object
                    // --- Expected field names based on OOMIR ---
                    var piecesField: Field? = null
                    var argsField: Field? = null // Field for actual format arguments

                    try {
                        piecesField = argClass.getDeclaredField("pieces")
                        argsField = argClass.getDeclaredField("args")
                    } catch (e: NoSuchFieldException) {
                        // happens usually due to our Reflection and Any? hacking which means r8 optimises the fields away
                        // won't be an issue once the shim is outgrown and we compile core for real and don't need this hack
                        throw NoSuchFieldException("<panic: object missing expected fields>")
                    }

                    piecesField?.isAccessible = true // Allow access even if private
                    argsField?.isAccessible = true
                    // fmtField.isAccessible = true

                    val piecesObj = piecesField?.get(arg)
                    val argsObj = argsField?.get(arg)
                    // val fmtObj = fmtField.get(arg)

                    // --- Process the extracted fields ---

                    // Basic interpretation: Assume panic!("literal")
                    // piecesObj should be String[]
                    if (piecesObj is Array<*> && argsObj is Array<*> && argsObj.isEmpty()) {
                        // Check if pieces contains Strings and concatenate them
                        val builder = StringBuilder()
                        var first = true
                        for (piece in piecesObj) {
                            if (!first) builder.append(" ") // Simple concatenation, adjust as needed
                            builder.append(piece as? String ?: "<non-string piece>")
                            first = false
                        }
                        builder.toString()
                    } else {
                        // TODO: Implement full formatting logic here!
                        // This would involve inspecting 'argsObj', potentially 'fmtObj',
                        // and interleaving formatted args with pieces.
                        // For now, a fallback message:
                        val piecesSummary = if (piecesObj is Array<*>) piecesObj.joinToString() else piecesObj?.toString() ?: "null"
                        "<panic: complex arguments object received - pieces: [${piecesSummary}] - requires full formatting impl>"
                    }

                } catch (e: Exception) {
                    "<panic: error processing arguments object: ${e.message}> (Type: ${arg::class.java.name})"
                }
            }
        }

        // Throw the exception with the determined message
        throw RuntimeException("Rust panic: $message")
    }

    @JvmStatic
    public fun std_rt_panic_fmt(arg: Any?) {
        // Newer Rust nightly uses std::rt::panic_fmt instead of core::panicking::panic_fmt
        // Just delegate to panic_fmt which has the same behavior
        panic_fmt(arg)
    }

    @JvmStatic
    public fun arguments_new_const_1(pieces: Array<String>): String {
        // Concatenate all the string pieces together.
        // This mimics the simplest formatting scenario where pieces are just joined.
        // If the array is empty, it correctly returns an empty string.
        return pieces.joinToString("")
    }

    @JvmStatic
    public fun core_fmt_rt_Argument_new_display_i32(value: Int): String {
        // Convert the integer to a string.
        return value.toString()
    }

    @JvmStatic
    public fun core_fmt_rt_Argument_new_display(value: Any?): String {
        // Convert the value to a string.
        return value?.toString() ?: "null"
    }

    @JvmStatic
    public fun core_fmt_rt_Argument_new_display_bool(value: Boolean): String {
        // Convert the boolean to a string.
        return value.toString()
    }

    @JvmStatic
    public fun core_fmt_rt_Argument_new_display_f64(value: Double): String {
        // Convert the double to a string.
        return value.toString()
    }

    @JvmStatic
    public fun core_fmt_rt_Arguments_new_const_1(pieces: Array<String>): String {
        val sb = StringBuilder()
        var argIndex = 0
        for (i in pieces.indices) {
            sb.append(pieces[i])
        }
        return sb.toString()
    }

    @JvmStatic
    public fun Arguments_from_str(msg: String): String {
        // Arguments::from_str creates a simple Arguments from a static string
        // In our simplified model, we just return the string as-is
        return msg
    }

    @JvmStatic 
    public fun to_string(value: Any?): String {
        // Convert the value to a string.
        return value?.toString() ?: "null"
    }

    @JvmStatic
    public fun len(str: String): Int {
        // Return the length of the string in characters (code points, not bytes)
        return str.length
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


    // variants of eq that just call the main one. need A LOT of these since we are now properly resolving generics
    // will no longer be needed once we compile the real `core` library which will generate all of these - support is almost there
    // placeholders just for now

    @JvmStatic
    public fun str_eq(value1: String?, value2: String?): Boolean {
        return eq(value1, value2)
    }

    @JvmStatic
    public fun f16_eq(value1: Any?, value2: Any?): Boolean {
        return eq(value1, value2)
    }

    @JvmStatic
    public fun f32_eq(value1: Any?, value2: Any?): Boolean {
        return eq(value1, value2)
    }

    @JvmStatic
    // (f32, f32) tuple
    public fun f32_f32_eq(value1: Any?, value2: Any?): Boolean {
        return eq(value1, value2)
    }

    @JvmStatic
    public fun f64_eq(value1: Any?, value2: Any?): Boolean {
        return eq(value1, value2)
    }

    @JvmStatic
    public fun f128_eq(value1: Any?, value2: Any?): Boolean {
        return eq(value1, value2)
    }

    @JvmStatic
    public fun i64_eq(value1: Any?, value2: Any?): Boolean {
        return eq(value1, value2)
    }

    @JvmStatic
    // tuple (i32, i32, i32)
    public fun i32_i32_i32_eq(value1: Any?, value2: Any?): Boolean {
        return eq(value1, value2)
    }
    
    @JvmStatic
    // tuple (i32, u8)
    public fun i32_u8_eq(value1: Any?, value2: Any?): Boolean {
        return eq(value1, value2)
    }

    @JvmStatic
    // [u8; 8]
    public fun u8_8_eq(value1: Any?, value2: Any?): Boolean {
        return eq(value1, value2)
    }

    @JvmStatic
    public fun raw_eq_u8_8(slice1: Any?, slice2: Any?): Boolean {
        return eq(slice1, slice2)
    }

    @JvmStatic
    public fun u8_eq(value1: Any?, value2: Any?): Boolean {
        return eq(value1, value2)
    }
    
    @JvmStatic
    public fun Option_usize_eq(value1: Any?, value2: Any?): Boolean {
        return eq(value1, value2)
    }

    @JvmStatic
    public fun core_panicking_panic(message: String?) {
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
fun core_str_str_starts_with_char(value: Any, prefix: Any): Boolean {
    // Runtime type check needed here!
    return when {
        value is String && prefix is String -> { // Both are strings
            value.startsWith(prefix)
        }
        value is String && prefix is Char -> { // String and Char
            value.startsWith(prefix)
        }
        value is ShortArray && prefix is ShortArray -> {
            // Calculate logical length of prefix (ends at first 0 or array end)
            val logicalPrefixLength = prefix.indexOfFirst { it == 0.toShort() }.let {
                if (it == -1) prefix.size else it
            }

            // Check if the value is long enough to contain the logical prefix
            if (logicalPrefixLength > value.size) {
                false
            } else {
                // Compare elements up to the logical prefix length
                val result = (0 until logicalPrefixLength).all { value[it] == prefix[it] }
                result
            }
        }
        else -> {
            // Throw an exception for unexpected type combinations
            throw IllegalArgumentException("Unsupported types for core_starts_with: ${value::class.simpleName}, ${prefix::class.simpleName}")
        }
    }
}

// wrapper that just calls the above
@JvmStatic
public fun core_slice_u8_starts_with(value: Any, prefix: Any): Boolean {
    return core_str_str_starts_with_char(value, prefix)
}
    @JvmStatic
    public fun Option_unwrap(optionObj: Any?): Any? {
        if (optionObj == null) {
             // This shouldn't happen if the codegen is correct, as unwrap is called on an instance.
             panic_fmt("FATAL: Called option_unwrap on a null reference. This indicates a bug in the code generator.")
             // Need a return path for the compiler, even though panic throws.
             throw RuntimeException("Unreachable after panic")
        }

        // Determine the variant using instanceof (Kotlin 'is')
        if (optionObj::class.java.name.endsWith("Option\$Some")) {
             // It's Some(value). Extract the value from field0.
             try {
                 // Use reflection to get the field, assuming we don't know the exact class type statically
                 // but know the naming convention.
                 val field = optionObj::class.java.getDeclaredField("field0")
                 field.isAccessible = true // Ensure we can access it even if not public
                 return field.get(optionObj)
             } catch (e: NoSuchFieldException) {
                 panic_fmt("Internal Compiler Error: Option\$Some class generated without 'field0'. Exception: ${e.message}")
                 throw RuntimeException("Unreachable after panic") // For compiler
             } catch (e: IllegalAccessException) {
                 panic_fmt("Internal Compiler Error: Cannot access 'field0' in generated Option\$Some class. Exception: ${e.message}")
                 throw RuntimeException("Unreachable after panic") // For compiler
             }
        } else if (optionObj::class.java.name.endsWith("Option\$None")) {
             // It's None. Panic with the standard message.
             panic_fmt("called `Option::unwrap()` on a `None` value")
             throw RuntimeException("Unreachable after panic") // For compiler
        } else {
             // Input object was not an expected Option variant. This indicates a codegen bug.
             val className = optionObj::class.java.name
             panic_fmt("Internal Compiler Error: Called option_unwrap on an unexpected type: $className. Expected type ending in Option\$Some or Option\$None.")
             throw RuntimeException("Unreachable after panic") // For compiler
        }
    }

    @JvmStatic
    // same as unwrap but return true/false if None instead of exception
    public fun Option_is_none(optionObj: Any?): Boolean {
        if (optionObj == null) {
            // This shouldn't happen if the codegen is correct, as unwrap is called on an instance.
            panic_fmt("FATAL: Called option_is_none on a null reference. This indicates a bug in the code generator.")
            // Need a return path for the compiler, even though panic throws.
            throw RuntimeException("Unreachable after panic")
        }

        // Determine the variant using instanceof (Kotlin 'is')
        if (optionObj::class.java.name.endsWith("\$Some")) {
            // It's Some(value). Return false.
            return false
        } else if (optionObj::class.java.name.endsWith("\$None")) {
            // It's None. Return true.
            return true
        } else {
            // Input object was not an expected Option variant. This indicates a codegen bug.
            val className = optionObj::class.java.name
            panic_fmt("Internal Compiler Error: Called option_is_none on an unexpected type: $className. Expected type ending in Option\$Some or Option\$None.")
            throw RuntimeException("Unreachable after panic") // For compiler
        }
    }

    // redirectors for monomorphised versions of the above, to just call the above
    @JvmStatic
    public fun Option_usize_is_none(optionObj: Any?): Boolean {
        return Option_is_none(optionObj)
    }

    /**
     * Shim for `<[T] as SlicePartialEq<T>>::equal`.
     * Handles comparison of primitive arrays based on OOMIR types.
     * Primarily expects ByteArray (for u8) or ShortArray (due to I16 OOMIR type from String casts).
     */
    @JvmStatic
    public fun equal(slice1: Any?, slice2: Any?): Boolean {
        // 1. Identity and Null checks
        if (slice1 === slice2) return true
        if (slice1 == null || slice2 == null) return false

        // 2. Type checks and content comparison
        return when (slice1) {
            is ByteArray -> slice2 is ByteArray && slice1.contentEquals(slice2)
            // Handle I16 from OOMIR, which likely corresponds to ShortArray
            is ShortArray -> slice2 is ShortArray && slice1.contentEquals(slice2)
             // Handle CharArray as well, just in case I16 is interpreted as Char
             is CharArray -> slice2 is CharArray && slice1.contentEquals(slice2)
            // Add other primitive array types if needed for different SlicePartialEq impls
            is IntArray -> slice2 is IntArray && slice1.contentEquals(slice2)
            is LongArray -> slice2 is LongArray && slice1.contentEquals(slice2)
            is FloatArray -> slice2 is FloatArray && slice1.contentEquals(slice2)
            is DoubleArray -> slice2 is DoubleArray && slice1.contentEquals(slice2)
            is BooleanArray -> slice2 is BooleanArray && slice1.contentEquals(slice2)
            // Handle generic object arrays - recursive call to general 'eq' might be needed if elements aren't simple
            is Array<*> -> slice2 is Array<*> && slice1.contentEquals(slice2) // Note: contentEquals for Array<*> does shallow comparison by default! Deep comparison might need the main `eq` recursively. Let's keep it simple for now.
            else -> false // Not a recognized array type for comparison
        }
    }

    // monomorphised versions of the above
    @JvmStatic
    // a slice of u8
    public fun u8_equal(slice1: Any?, slice2: Any?): Boolean {
        return equal(slice1, slice2)
    }
 

    /**
     * Convert a Java String into a ShortArray, by casting each char to a short.
     */
    @JvmStatic
    public fun toShortArray(s: String?): ShortArray? {
        if (s == null) return null
        val len = s.length
        val arr = ShortArray(len)
        for (i in 0 until len) {
            arr[i] = s[i].toShort()
        }
        return arr
    }

    /**
     * Convert a ShortArray back into a Java String, by casting each short to a char.
     */
    @JvmStatic
    public fun fromShortArray(arr: ShortArray): String? {
        if (arr == null) {
            throw NullPointerException("Input ShortArray is null")
        }
        val chars = CharArray(arr.size)
        for (i in arr.indices) {
            chars[i] = arr[i].toChar()
        }
        return String(chars)
    }

    @JvmStatic
    public fun encode_utf8_raw(code: Long, dstOuter: Array<ShortArray>): Array<ShortArray> {
        // --- Modification Start ---
        // Validate the outer array structure expected from the "array hack"
        if (dstOuter.isEmpty()) {
            throw IllegalArgumentException("Outer array wrapper for destination cannot be empty.")
        }
        // The actual destination ShortArray is inside the first element of the outer array
        val dst = dstOuter[0]
        // --- Modification End ---


        // 1. Validate input code point (remains the same)
        if (code < 0L || code > 0x10FFFFL) {
            throw IllegalArgumentException("Invalid Unicode code point: $code (must be 0..0x10FFFF)")
        }
        // Exclude surrogates U+D800 to U+DFFF (remains the same)
        if (code >= 0xD800L && code <= 0xDFFFL) {
             throw IllegalArgumentException("Invalid Unicode code point: $code (surrogate range 0xD800..0xDFFF)")
        }

        // We can safely cast to Int as valid codes fit (remains the same)
        val c = code.toInt()
        var index = 0

        // Helper function to write a byte value into a Short element
        // Ensures the value is treated as an unsigned byte (0-255)
        // and stored in the lower 8 bits of the Short.
        // --- Operates on the inner 'dst' array ---
        fun writeByteToShortArray(byteValue: Int) {
            // Use the size of the inner array 'dst' for bounds checking
            if (index >= dst.size) {
                 // Determine needed size dynamically based on 'when' branch for a clearer message
                 val needed = when {
                    c <= 0x7F -> 1
                    c <= 0x7FF -> 2
                    c <= 0xFFFF -> 3
                    else -> 4 // c <= 0x10FFFF
                 }
                 // Report size of the inner array 'dst'
                 throw IndexOutOfBoundsException("Destination array (inner) too small (index $index >= size ${dst.size}, needed $needed)")
            }
            // Mask to get the unsigned byte value and convert to Short
            // Modify the inner array 'dst'
            dst[index++] = (byteValue and 0xFF).toShort()
        }

        // UTF-8 encoding logic (remains the same, uses the helper)
        when {
            // 1-byte sequence (ASCII)
            c <= 0x7F -> {
                writeByteToShortArray(c) // Byte 1
            }
            // 2-byte sequence
            c <= 0x7FF -> {
                writeByteToShortArray(0xC0 or (c shr 6))   // Byte 1: 110xxxxx
                writeByteToShortArray(0x80 or (c and 0x3F)) // Byte 2: 10xxxxxx
            }
            // 3-byte sequence
            c <= 0xFFFF -> {
                writeByteToShortArray(0xE0 or (c shr 12))           // Byte 1: 1110xxxx
                writeByteToShortArray(0x80 or ((c shr 6) and 0x3F)) // Byte 2: 10xxxxxx
                writeByteToShortArray(0x80 or (c and 0x3F))         // Byte 3: 10xxxxxx
            }
            // 4-byte sequence
            c <= 0x10FFFF -> {
                writeByteToShortArray(0xF0 or (c shr 18))           // Byte 1: 11110xxx
                writeByteToShortArray(0x80 or ((c shr 12) and 0x3F)) // Byte 2: 10xxxxxx
                writeByteToShortArray(0x80 or ((c shr 6) and 0x3F)) // Byte 3: 10xxxxxx
                writeByteToShortArray(0x80 or (c and 0x3F))         // Byte 4: 10xxxxxx
            }
        }

        // Bounds check after writing (remains the same logic, uses inner 'dst')
        val bytesWritten = index
        if (bytesWritten > dst.size) {
             throw IndexOutOfBoundsException("Internal error: Wrote $bytesWritten bytes into inner array of size ${dst.size}")
        }

        return arrayOf(dst)
    }

    @JvmStatic
    public fun std_char_encode_utf8_raw(code: Long, dstOuter: Array<ShortArray>): Array<ShortArray> {
        // New Rust nightly uses std::char::encode_utf8_raw instead of core path
        // Delegate to existing implementation
        return encode_utf8_raw(code, dstOuter)
    }

    @JvmStatic
    public fun std_intrinsics_size_of_val_u8(value: Any?): Long {
        if (value == null) {
            // size_of_val in Rust operates on a reference, which cannot be null.
            // Receiving a null here indicates a potential codegen issue.
            throw NullPointerException("Argument to std_intrinsics_size_of_val_u8 cannot be null.")
        }

        // The name implies a slice of u8, which is represented as a ByteArray.
        // We can make this more general to handle other potential slice types as well.
        return when (value) {
            // For a byte array, the size in bytes is simply its length.
            is ByteArray -> value.size.toLong()
            
            // Handle other primitive array types for completeness, calculating size in bytes.
            is ShortArray -> value.size.toLong() * 2
            is CharArray -> value.size.toLong() * 2
            is IntArray -> value.size.toLong() * 4
            is LongArray -> value.size.toLong() * 8
            is FloatArray -> value.size.toLong() * 4
            is DoubleArray -> value.size.toLong() * 8
            is BooleanArray -> value.size.toLong() // JVM representation is 1 byte per boolean in an array.

            else -> {
                // If it's not an array, size_of_val is not applicable in this context for DSTs.
                // This indicates an unexpected type was passed.
                throw IllegalArgumentException("Unsupported type for std_intrinsics_size_of_val: ${value::class.java.name}")
            }
        }
    }

    @JvmStatic
    public fun compare_bytes(ptr1: Any?, ptr2: Any?, len: Long): Int {
        if (ptr1 === ptr2) return 0
        // The intrinsic doesn't expect nulls, this indicates a codegen issue.
        if (ptr1 == null || ptr2 == null) {
            throw NullPointerException("compare_bytes received a null pointer")
        }

        // The representation of &[u8] from a string is ShortArray in this backend
        if (ptr1 !is ShortArray || ptr2 !is ShortArray) {
            throw IllegalArgumentException("compare_bytes expects ShortArray arguments, got ${ptr1::class.simpleName} and ${ptr2::class.simpleName}")
        }

        val arr1 = ptr1 as ShortArray
        val arr2 = ptr2 as ShortArray
        val n = len.toInt() // Assuming len fits in Int

        // The 'len' parameter is the number of elements to check
        val limit = Math.min(Math.min(arr1.size, arr2.size), n)

        for (i in 0 until limit) {
            // Compare elements directly. Use compareTo for standard signed comparison.
            val cmp = arr1[i].compareTo(arr2[i])
            if (cmp != 0) {
                return cmp
            }
        }

        // If we've checked `n` elements and all are equal, the regions are considered equal
        // for the purpose of this intrinsic. The intrinsic does not compare overall length
        // if the compared prefix is identical.
        return 0
    }

    @JvmStatic
    public fun compare_bytes(ptr1: Any?, ptr2: Any?, len: Int): Int {
        // Overload for when length is passed as Int (release mode optimization)
        return compare_bytes(ptr1, ptr2, len.toLong())
    }
}