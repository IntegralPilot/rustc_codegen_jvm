#![feature(register_tool)]
#![register_tool(jvm_codegen)]

#[jvm::annotation("java.lang.Deprecated")]
#[jvm::annotation("Main.AllValues",
    flag = true, small = -128_i8, medium = -32768_i16,
    count = -2147483648_i32, big = -9223372036854775808_i64,
    float_value = -0.0_f32, double_value = 1.25_f64, letter = 'λ', text = "é\0😀",
    kind = enum_value("Main.Mode", "SECOND"), r#type = class("java.lang.String[]"),
    types = [class("int"), class("void"), class("java.lang.Object")],
    nested = annotation("Main.Tag", value = "nested"), numbers = [1, -2, 3], empty = [],
    values(retention = "an annotation element")
)]
#[jvm::annotation("Main.ClassTag", retention = "class")]
#[jvm::annotation("Main.SourceTag", retention = "source")]
pub struct Annotated {
    pub value: i32,
}

#[jvm::annotation("Main.Tags", value = [
    annotation("Main.Tag", value = "first"),
    annotation("Main.Tag", value = "second"),
])]
pub struct Repeated;

#[jvm::interface("java.lang.Runnable")]
pub trait Runnable {
    fn run(&mut self);
}

impl Runnable for Annotated {
    fn run(&mut self) {
        self.value += 1;
    }
}

pub fn generic() -> annotation_api::Generic<i32> {
    let result = annotation_api::make();
    assert_eq!(result.answer(), 42);
    result
}

pub fn generic_long() -> annotation_api::Generic<i64> {
    annotation_api::Generic { value: 9 }
}
