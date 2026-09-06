use super::*;

pub(super) fn kind(ty: ScalarType) -> jvm::Result<Kind> {
    use ScalarType::*;
    Ok(match ty {
        I64 | U64 => Kind::Long,
        F32 => Kind::Float,
        F64 => Kind::Double,
        I128 | U128 | Char => return Err(error("scalar representation not yet selected")),
        _ => Kind::Int,
    })
}

/// Resolve simultaneous edge assignments without overwriting a still-needed
/// source. A single two-word scratch slot handles cycles of either JVM width.
pub(super) fn parallel_copies(
    copies: &mut Vec<(u16, u16, Kind)>,
    scratch: u16,
    code: &mut Vec<Instruction>,
) -> bool {
    copies.retain(|(to, from, _)| to != from);
    let mut used_scratch = false;
    while !copies.is_empty() {
        if let Some(index) = copies
            .iter()
            .position(|(to, _, _)| !copies.iter().any(|(_, from, _)| to == from))
        {
            let (to, from, kind) = copies.swap_remove(index);
            code.extend([kind.load(from), kind.store(to)]);
        } else {
            let (_, from, kind) = copies[0];
            code.extend([kind.load(from), kind.store(scratch)]);
            for (_, source, _) in copies.iter_mut() {
                if *source == from {
                    *source = scratch;
                }
            }
            used_scratch = true;
        }
    }
    used_scratch
}
