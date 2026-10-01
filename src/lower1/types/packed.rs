//! Small integer aggregates use their storage word. Rust identity still controls methods, traits,
//! layout, and destruction.
use super::*;
use crate::oomir::{BinaryOp, Constant, Instruction, Operand, Type};

#[derive(Clone, Copy)]
pub(crate) struct PackedWord<'tcx> {
    pub scalar: Ty<'tcx>,
    size: u8,
    fields: [(u8, u8); 4],
}

pub(crate) fn value_scalar_ty<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Option<Ty<'tcx>> {
    enum_scalar_ty(ty, tcx).or_else(|| packed_word(ty, tcx).map(|word| word.scalar))
}

pub(crate) fn packed_word<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Option<PackedWord<'tcx>> {
    if !matches!(
        ty.kind(),
        TyKind::Adt(..) | TyKind::Tuple(..) | TyKind::Alias(..)
    ) || ty.has_param()
        || ty.has_escaping_bound_vars()
    {
        return None;
    }
    let ty = normalize_union_ty(tcx, ty).ok()?;
    let mut fields = [tcx.types.unit; 4];
    let len = match ty.kind() {
        TyKind::Adt(def, args)
            if def.is_struct()
                && !tcx
                    .is_lang_item(def.did(), rustc_attr_ir::lang_items::LangItem::UnsafeCell)
                && !def.has_dtor(tcx)
                && adt_class_kind(tcx, def, args) == oomir::ClassKind::Value =>
        {
            let members = &def.non_enum_variant().fields;
            if members.len() > fields.len() {
                return None;
            }
            for (i, field) in members.iter().enumerate() {
                fields[i] = normalize_union_ty(tcx, field.ty(tcx, args).skip_norm_wip()).ok()?;
            }
            members.len()
        }
        TyKind::Tuple(members) if members.len() <= fields.len() => {
            for (i, field) in members.iter().enumerate() {
                fields[i] = field;
            }
            members.len()
        }
        _ => return None,
    };
    let fields = &fields[..len];
    if fields.is_empty() {
        return None;
    }
    // Pointer identity, floats, padding and interior mutation are not words.
    for ty in fields {
        if !matches!(
            ty.kind(),
            TyKind::Int(IntTy::I8 | IntTy::I16 | IntTy::I32 | IntTy::I64 | IntTy::Isize)
                | TyKind::Uint(
                    UintTy::U8 | UintTy::U16 | UintTy::U32 | UintTy::U64 | UintTy::Usize
                )
        ) {
            return None;
        }
    }
    let layout = tcx
        .layout_of(TypingEnv::fully_monomorphized().as_query_input(ty))
        .ok()?;
    let size = layout.size.bytes();
    let scalar = match size {
        1 => tcx.types.u8,
        2 => tcx.types.u16,
        4 => tcx.types.u32,
        8 => tcx.types.u64,
        _ => return None,
    };
    let mut result = PackedWord {
        scalar,
        size: size as u8,
        fields: [(0, 0); 4],
    };
    let mut covered = 0;
    for (index, field) in fields.iter().enumerate() {
        let field_size = layout_size_bytes(tcx, *field).ok()?;
        let offset = layout.fields.offset(index).bytes_usize();
        if offset + field_size > size as usize {
            return None;
        }
        result.fields[index] = (offset as u8, field_size as u8);
        covered += field_size;
    }
    // Padding may be uninitialized: never turn it into a scalar load.
    (covered == size as usize).then_some(result)
}

impl PackedWord<'_> {
    pub(crate) fn jvm_type(self) -> Type {
        match self.size {
            1 => Type::U8,
            2 => Type::U16,
            4 => Type::U32,
            8 => Type::U64,
            _ => unreachable!(),
        }
    }

    pub(crate) fn zero(self) -> Operand {
        Operand::Constant(match self.size {
            1 => Constant::U8(0),
            2 => Constant::U16(0),
            4 => Constant::U32(0),
            8 => Constant::U64(0),
            _ => unreachable!(),
        })
    }

    fn bits_type(self) -> Type {
        if self.size == 8 { Type::U64 } else { Type::U32 }
    }

    fn constant(self, bits: u64) -> Operand {
        Operand::Constant(if self.size == 8 {
            Constant::U64(bits)
        } else {
            Constant::U32(bits as u32)
        })
    }

    fn mask(self, index: usize) -> u64 {
        let size = self.fields[index].1;
        if size == 8 {
            u64::MAX
        } else {
            (1u64 << (size * 8)) - 1
        }
    }

    pub(crate) fn extract(
        self,
        value: Operand,
        index: usize,
        ty: &Type,
        dest: &str,
        code: &mut Vec<Instruction>,
    ) -> Operand {
        let value = cast(value, self.bits_type(), &format!("{dest}_bits"), code);
        let shift = self.fields[index].0 * 8;
        let value = if shift == 0 {
            value
        } else {
            binary(
                BinaryOp::Shr,
                value,
                self.constant(shift as u64),
                &format!("{dest}_shift"),
                code,
            )
        };
        cast(value, ty.clone(), dest, code)
    }

    pub(crate) fn insert(
        self,
        value: Operand,
        index: usize,
        field: Operand,
        dest: &str,
        code: &mut Vec<Instruction>,
    ) -> Operand {
        let shift = self.fields[index].0 * 8;
        let mask = self.mask(index);
        let value = cast(value, self.bits_type(), &format!("{dest}_bits"), code);
        let field = cast(field, self.bits_type(), &format!("{dest}_field_bits"), code);
        let field = binary(
            BinaryOp::BitAnd,
            field,
            self.constant(mask),
            &format!("{dest}_masked"),
            code,
        );
        let field = if shift == 0 {
            field
        } else {
            binary(
                BinaryOp::Shl,
                field,
                self.constant(shift as u64),
                &format!("{dest}_shift"),
                code,
            )
        };
        let value = binary(
            BinaryOp::BitAnd,
            value,
            self.constant(!(mask << shift)),
            &format!("{dest}_clear"),
            code,
        );
        let value = binary(
            BinaryOp::BitOr,
            value,
            field,
            &format!("{dest}_merge"),
            code,
        );
        cast(value, self.jvm_type(), dest, code)
    }

    pub(crate) fn field_address(
        self,
        owner: Operand,
        index: usize,
        field: &Type,
        dest: &str,
        code: &mut Vec<Instruction>,
    ) -> Operand {
        let owner_type = owner.get_type().expect("packed owner has a type");
        let offset = format!("{dest}_offset");
        code.push(Instruction::AddressOffset {
            dest: Some(offset.clone()),
            source: owner,
            count: Operand::Constant(Constant::U64(self.fields[index].0 as u64)),
            ty: owner_type.clone(),
            bytes: true,
            wrapping: false,
            subtract: false,
        });
        let pointer_type = Type::pointer(field.clone());
        let address = format!("{dest}_address");
        code.push(Instruction::AddressRetype {
            dest: Some(address.clone()),
            source: operand_var(offset, owner_type),
            layout: Box::new(oomir::AddressLayout {
                pointer_type: pointer_type.clone(),
                size: Operand::Constant(Constant::I32(self.fields[index].1 as i32)),
                codec: Operand::Constant(Constant::Null(Type::java_string())),
            }),
        });
        operand_var(address, pointer_type)
    }
}

fn cast(value: Operand, ty: Type, dest: &str, code: &mut Vec<Instruction>) -> Operand {
    code.push(Instruction::Cast {
        dest: dest.into(),
        op: value,
        ty: ty.clone(),
    });
    operand_var(dest, ty)
}

fn binary(
    op: BinaryOp,
    left: Operand,
    right: Operand,
    dest: &str,
    code: &mut Vec<Instruction>,
) -> Operand {
    let ty = left.get_type().unwrap();
    code.push(Instruction::Binary {
        dest: dest.into(),
        op,
        op1: left,
        op2: right,
    });
    operand_var(dest, ty)
}
