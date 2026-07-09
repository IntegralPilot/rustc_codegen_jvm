package org.rustlang.core.fmt;

import org.rustlang.core.Core;
import org.rustlang.core.ptr.non_null.NonNull_Argument__;
import org.rustlang.core.ptr.non_null.NonNull_i16;

public final class Arguments__ {
    public short[] template;
    public Object[] args;
    public String message;

    public Arguments__(short[] template, Object[] args, String message) {
        this.template = template;
        this.args = args;
        this.message = message;
    }

    public Arguments__(short[] template, Object[] args) {
        this(template, args, Core.formatArgs(template, args));
    }

    public Arguments__(NonNull_i16 template, NonNull_Argument__ args) {
        this(
                template == null ? null : template.pointer,
                args == null ? null : args.pointer
        );
    }

    public static Arguments__ from_str(String message) {
        return new Arguments__(null, null, message);
    }

    @Override
    public String toString() {
        return Core.formatArgumentObject(this);
    }
}
