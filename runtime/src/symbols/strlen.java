package org.rustlang.runtime.symbols;

import org.rustlang.runtime.Pointer;

/** JVM implementation of the C `strlen` symbol used by `CStr::from_ptr`. */
public final class strlen {
    private strlen() {}

    public static long strlen(Pointer pointer) {
        long length = 0;
        while (pointer.byte_offset(length).retype(1).getI8() != 0) {
            length++;
        }
        return length;
    }
}
