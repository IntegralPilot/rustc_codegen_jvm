use super::*;
use crate::lower1::context::Definitions;
use crate::oomir::{Instruction, Operand, Type};
use rustc_middle::ty::Instance;

pub(crate) fn tuple_value<'tcx>(
    ty: Ty<'tcx>,
    args: Vec<(Operand, Type)>,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    definitions: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
    code: &mut Vec<Instruction>,
) -> Operand {
    let ty = definitions.normalize(tcx, ty, instance);
    let jvm = ty_to_oomir_type(ty, tcx, definitions, instance);
    if !jvm.has_jvm_value() {
        return Operand::Constant(oomir::Constant::Unit);
    }
    if let Some(word) = packed_word(ty, tcx) {
        let mut value = word.zero();
        for (index, (field, _)) in args.into_iter().enumerate() {
            value = word.insert(value, index, field, &format!("{dest}_field_{index}"), code);
        }
        code.push(Instruction::Move {
            dest: dest.into(),
            src: value,
        });
    } else {
        code.push(Instruction::ConstructObject {
            dest: dest.into(),
            class_name: jvm.get_class_name().expect("tuple carrier").into(),
            args,
        });
    }
    operand_var(dest, jvm)
}

pub(crate) fn tuple_fields<'tcx>(
    ty: Ty<'tcx>,
    value: Operand,
    dest: &str,
    tcx: TyCtxt<'tcx>,
    definitions: &mut Definitions<'tcx>,
    instance: Instance<'tcx>,
    code: &mut Vec<Instruction>,
) -> Vec<Operand> {
    let ty = definitions.normalize(tcx, ty, instance);
    let TyKind::Tuple(fields) = ty.kind() else {
        panic!("expected tuple, found {ty:?}")
    };
    let word = packed_word(ty, tcx);
    fields
        .iter()
        .enumerate()
        .map(|(index, ty)| {
            let jvm = ty_to_oomir_type(ty, tcx, definitions, instance);
            if !jvm.has_jvm_value() {
                return Operand::Constant(oomir::Constant::Unit);
            }
            let name = format!("{dest}_{index}");
            if let Some(word) = word {
                word.extract(value.clone(), index, &jvm, &name, code)
            } else {
                code.push(Instruction::GetField {
                    dest: name.clone(),
                    object: value.clone(),
                    field_name: format!("field{index}"),
                    field_ty: jvm.clone(),
                    owner_class: value
                        .get_type()
                        .unwrap()
                        .get_class_name()
                        .expect("tuple carrier")
                        .into(),
                });
                operand_var(name, jvm)
            }
        })
        .collect()
}
