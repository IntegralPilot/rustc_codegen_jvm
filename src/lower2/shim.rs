//! Explicit mappings from Rust OOMIR calls to the Java runtime ABI.

#[derive(Debug, Clone, Copy)]
pub(super) struct ShimInfo {
    pub(super) rust_class: Option<&'static str>,
    pub(super) rust_method: &'static str,
    pub(super) java_class: &'static str,
    pub(super) java_method: &'static str,
    pub(super) descriptor: &'static str,
}

const CORE_CLASS: &str = "org/rustlang/core/Core";
const PANICKING_CLASS: &str = "org/rustlang/core/panicking";

const fn direct(
    java_class: &'static str,
    method: &'static str,
    descriptor: &'static str,
) -> ShimInfo {
    ShimInfo {
        rust_class: None,
        rust_method: method,
        java_class,
        java_method: method,
        descriptor,
    }
}

const fn alias(
    rust_class: &'static str,
    rust_method: &'static str,
    java_method: &'static str,
    descriptor: &'static str,
) -> ShimInfo {
    ShimInfo {
        rust_class: Some(rust_class),
        rust_method,
        java_class: CORE_CLASS,
        java_method,
        descriptor,
    }
}

const SHIMS: &[ShimInfo] = &[
    /*direct(
        CORE_CLASS,
        "arguments_from_str",
        "(Ljava/lang/String;)Lorg/rustlang/core/fmt/Arguments__;",
    ),
    direct(CORE_CLASS, "encode_utf8_raw", "(J[[B)[[B"),
    direct(
        CORE_CLASS,
        "formatArgs",
        "([B[Ljava/lang/Object;)Ljava/lang/String;",
    ),
    direct(
        CORE_CLASS,
        "formatArgumentObject",
        "(Ljava/lang/Object;)Ljava/lang/String;",
    ),
    direct(
        CORE_CLASS,
        "formatArgumentType",
        "(Ljava/lang/Object;)Ljava/lang/String;",
    ),
    direct(
        CORE_CLASS,
        "formatArguments",
        "(Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/String;",
    ),
    direct(CORE_CLASS, "fromShortArray", "([S)Ljava/lang/String;"),
    direct(
        CORE_CLASS,
        "new_arguments",
        "([B[Ljava/lang/Object;)Lorg/rustlang/core/fmt/Arguments__;",
    ),
    direct(
        CORE_CLASS,
        "new_display",
        "(Ljava/lang/Object;)Lorg/rustlang/core/fmt/rt/Argument__;",
    ),
    direct(PANICKING_CLASS, "panic", "(Ljava/lang/String;)V"),
    direct(PANICKING_CLASS, "panic_fmt", "(Ljava/lang/Object;)V"),
    direct(CORE_CLASS, "starts_with", "([B[B)Z"),
    direct(CORE_CLASS, "toShortArray", "(Ljava/lang/String;)[S"),*/
    alias(
        "org/rustlang/core/intrinsics",
        "compare_bytes",
        "compare_bytes",
        "(Lorg/rustlang/runtime/Pointer;Lorg/rustlang/runtime/Pointer;J)I",
    ),
    /*ShimInfo {
        rust_class: Some("org/rustlang/core/char/methods"),
        rust_method: "encode_utf8_raw",
        java_class: "org/rustlang/runtime/SliceView",
        java_method: "encodeUtf8",
        descriptor: "(ILorg/rustlang/runtime/SliceView;)Lorg/rustlang/runtime/Utf8View;",
    },
    alias(
        "org/rustlang/core/fmt/Arguments__",
        "from_str",
        "arguments_from_str",
        "(Ljava/lang/String;)Lorg/rustlang/core/fmt/Arguments__;",
    ),
    alias(
        "org/rustlang/core/fmt/Arguments__",
        "new",
        "new_arguments",
        "([B[Ljava/lang/Object;)Lorg/rustlang/core/fmt/Arguments__;",
    ),
    alias(
        "org/rustlang/core/fmt/rt/Argument__",
        "new_display",
        "new_display",
        "(Ljava/lang/Object;)Lorg/rustlang/core/fmt/rt/Argument__;",
    ),*/
];

pub(super) fn find_shim(class_name: &str, method_name: &str) -> Option<&'static ShimInfo> {
    SHIMS.iter().find(|shim| {
        shim.rust_method == method_name
            && shim
                .rust_class
                .map_or(shim.java_class == class_name, |rust_class| {
                    rust_class == class_name
                })
    })
}
