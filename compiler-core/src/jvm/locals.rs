//! JVM local categories and their shortest legal load/store encodings.
use crate::classfile::attributes::Instruction;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LocalKind {
    Int,
    Long,
    Float,
    Double,
    Reference,
}

impl LocalKind {
    pub fn width(self) -> u16 {
        if matches!(self, Self::Long | Self::Double) {
            2
        } else {
            1
        }
    }
    pub fn return_op(self) -> Instruction {
        match self {
            Self::Int => Instruction::Ireturn,
            Self::Long => Instruction::Lreturn,
            Self::Float => Instruction::Freturn,
            Self::Double => Instruction::Dreturn,
            Self::Reference => Instruction::Areturn,
        }
    }
}

// Each row defines both encoding and decoding, including compact and wide forms.
macro_rules! local_forms {
    ($method:ident, $decode:ident; $($kind:ident: $short:ident, $wide:ident, $a:ident, $b:ident, $c:ident, $d:ident);* $(;)?) => {
        impl LocalKind {
            pub fn $method(self, index: u16) -> Instruction {
                match self { $(Self::$kind => match index {
                    0 => Instruction::$a, 1 => Instruction::$b, 2 => Instruction::$c, 3 => Instruction::$d,
                    4..=255 => Instruction::$short(index as u8), _ => Instruction::$wide(index),
                }),* }
            }
        }
        pub fn $decode(instruction: &Instruction) -> Option<(LocalKind, u16)> {
            Some(match instruction { $(
                Instruction::$a => (LocalKind::$kind, 0), Instruction::$b => (LocalKind::$kind, 1),
                Instruction::$c => (LocalKind::$kind, 2), Instruction::$d => (LocalKind::$kind, 3),
                Instruction::$short(index) => (LocalKind::$kind, u16::from(*index)),
                Instruction::$wide(index) => (LocalKind::$kind, *index),
            )* _ => return None })
        }
    };
}
local_forms! { load, loaded_local;
    Int: Iload, Iload_w, Iload_0, Iload_1, Iload_2, Iload_3;
    Long: Lload, Lload_w, Lload_0, Lload_1, Lload_2, Lload_3;
    Float: Fload, Fload_w, Fload_0, Fload_1, Fload_2, Fload_3;
    Double: Dload, Dload_w, Dload_0, Dload_1, Dload_2, Dload_3;
    Reference: Aload, Aload_w, Aload_0, Aload_1, Aload_2, Aload_3;
}
local_forms! { store, stored_local;
    Int: Istore, Istore_w, Istore_0, Istore_1, Istore_2, Istore_3;
    Long: Lstore, Lstore_w, Lstore_0, Lstore_1, Lstore_2, Lstore_3;
    Float: Fstore, Fstore_w, Fstore_0, Fstore_1, Fstore_2, Fstore_3;
    Double: Dstore, Dstore_w, Dstore_0, Dstore_1, Dstore_2, Dstore_3;
    Reference: Astore, Astore_w, Astore_0, Astore_1, Astore_2, Astore_3;
}
