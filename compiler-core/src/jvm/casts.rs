//! Rust primitive casts on JVM scalar carriers, shared by both selectors.
use super::constants::{get_int_const_instr, get_long_const_instr};
use crate::{
    classfile::{self as jvm, attributes::Instruction, constant_pool::InternedConstantPool},
    scalar::ScalarType,
};

/// Semantic Rust primitive casts.  The JVM descriptor alone is insufficient here:
/// `u32` is carried in an `int`, `u64` in a `long`, and `f16` in a `short` bit-pattern.
pub fn primitive(
    src: &ScalarType,
    dest: &ScalarType,
    cp: &mut InternedConstantPool,
) -> Result<Vec<Instruction>, jvm::Error> {
    use Instruction as JI;

    fn int_width(ty: &ScalarType) -> Option<u32> {
        match ty {
            ScalarType::Bool => Some(1),
            ScalarType::I8 | ScalarType::U8 => Some(8),
            ScalarType::I16 | ScalarType::U16 | ScalarType::Char => Some(16),
            ScalarType::I32 | ScalarType::U32 => Some(32),
            ScalarType::I64 | ScalarType::U64 => Some(64),
            _ => None,
        }
    }

    fn is_unsigned(ty: &ScalarType) -> bool {
        matches!(
            ty,
            ScalarType::Bool
                | ScalarType::U8
                | ScalarType::U16
                | ScalarType::U32
                | ScalarType::U64
                | ScalarType::Char
        )
    }

    fn narrow(ty: &ScalarType) -> Option<Instruction> {
        match ty {
            ScalarType::I8 | ScalarType::U8 => Some(JI::I2b),
            ScalarType::I16 => Some(JI::I2s),
            ScalarType::U16 | ScalarType::Char => Some(JI::I2c),
            _ => None,
        }
    }

    fn numbers_call(
        cp: &mut InternedConstantPool,
        name: &str,
        descriptor: &str,
    ) -> Result<Instruction, jvm::Error> {
        let class = cp.add_class("org/rustlang/runtime/Numbers")?;
        let method = cp.add_method_ref(class, name, descriptor)?;
        Ok(JI::Invokestatic(method))
    }

    // binary16 is stored as raw bits. Decode before a cast out, and round once on a cast in.
    if src == &ScalarType::F16 {
        let mut result = vec![numbers_call(cp, "f16ToF32", "(S)F")?];
        result.extend(primitive(&ScalarType::F32, dest, cp)?);
        return Ok(result);
    }
    if dest == &ScalarType::F16 {
        return match src {
            ScalarType::F32 => Ok(vec![numbers_call(cp, "f32ToF16", "(F)S")?]),
            ScalarType::F64 => Ok(vec![numbers_call(cp, "f64ToF16", "(D)S")?]),
            _ if int_width(src).is_some() => {
                let mut result = primitive(src, &ScalarType::F64, cp)?;
                result.push(numbers_call(cp, "f64ToF16", "(D)S")?);
                Ok(result)
            }
            _ => Err(jvm::Error::VerificationError {
                context: "primitive_to_primitive".into(),
                message: format!("No path {src:?}→F16"),
            }),
        };
    }

    if matches!(src, ScalarType::F32 | ScalarType::F64)
        && matches!(dest, ScalarType::F32 | ScalarType::F64)
    {
        return Ok(match (src, dest) {
            (ScalarType::F32, ScalarType::F64) => vec![JI::F2d],
            (ScalarType::F64, ScalarType::F32) => vec![JI::D2f],
            _ => Vec::new(),
        });
    }

    if matches!(src, ScalarType::F32 | ScalarType::F64) && int_width(dest).is_some() {
        let prefix = if src == &ScalarType::F32 {
            "f32"
        } else {
            "f64"
        };
        let source_descriptor = if src == &ScalarType::F32 { "F" } else { "D" };
        let (suffix, return_descriptor, direct) = match dest {
            ScalarType::I8 => ("ToI8", "B", None),
            ScalarType::I16 => ("ToI16", "S", None),
            ScalarType::I32 => (
                "",
                "",
                Some(if src == &ScalarType::F32 {
                    JI::F2i
                } else {
                    JI::D2i
                }),
            ),
            ScalarType::I64 => (
                "",
                "",
                Some(if src == &ScalarType::F32 {
                    JI::F2l
                } else {
                    JI::D2l
                }),
            ),
            ScalarType::U8 | ScalarType::Bool => ("ToU8", "B", None),
            ScalarType::U16 | ScalarType::Char => ("ToU16", "C", None),
            ScalarType::U32 => ("ToU32", "I", None),
            ScalarType::U64 => ("ToU64", "J", None),
            _ => unreachable!(),
        };
        if let Some(op) = direct {
            return Ok(vec![op]);
        }
        return Ok(vec![numbers_call(
            cp,
            &format!("{prefix}{suffix}"),
            &format!("({source_descriptor}){return_descriptor}"),
        )?]);
    }

    if int_width(src).is_some() && matches!(dest, ScalarType::F32 | ScalarType::F64) {
        let to_f32 = dest == &ScalarType::F32;
        return Ok(match src {
            ScalarType::U32 => vec![numbers_call(
                cp,
                if to_f32 { "u32ToF32" } else { "u32ToF64" },
                if to_f32 { "(I)F" } else { "(I)D" },
            )?],
            ScalarType::U64 => vec![numbers_call(
                cp,
                if to_f32 { "u64ToF32" } else { "u64ToF64" },
                if to_f32 { "(J)F" } else { "(J)D" },
            )?],
            ScalarType::I64 => vec![if to_f32 { JI::L2f } else { JI::L2d }],
            ScalarType::U8 => vec![
                get_int_const_instr(cp, 0xff),
                JI::Iand,
                if to_f32 { JI::I2f } else { JI::I2d },
            ],
            _ => vec![if to_f32 { JI::I2f } else { JI::I2d }],
        });
    }

    if let (Some(src_width), Some(dest_width)) = (int_width(src), int_width(dest)) {
        let mut result = Vec::new();
        if dest_width <= 32 {
            if src_width == 64 {
                result.push(JI::L2i);
            }
            // The JVM sign-extends a byte local when it is loaded. Preserve the
            // Rust u8 value before widening it to any larger integer type.
            if src == &ScalarType::U8 && dest_width > 8 {
                result.push(get_int_const_instr(cp, 0xff));
                result.push(JI::Iand);
            }
            if let Some(op) = narrow(dest) {
                result.push(op);
            }
            return Ok(result);
        }

        if src_width < 64 {
            match src {
                ScalarType::U8 => {
                    result.push(get_int_const_instr(cp, 0xff));
                    result.push(JI::Iand);
                    result.push(JI::I2l);
                }
                ScalarType::U32 => {
                    result.push(JI::I2l);
                    result.push(get_long_const_instr(cp, 0xffff_ffff));
                    result.push(JI::Land);
                }
                _ if is_unsigned(src) => result.push(JI::I2l),
                _ => result.push(JI::I2l),
            }
        }
        return Ok(result);
    }

    Err(jvm::Error::VerificationError {
        context: "primitive_to_primitive".into(),
        message: format!("No path {src:?}→{dest:?}"),
    })
}
