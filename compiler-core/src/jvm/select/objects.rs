use super::super::abi::{
    relative_pointer_byte_offset_field, relative_pointer_element_offset_field,
};
use super::*;

impl Selector<'_> {
    pub(super) fn object(&mut self, inst: Inst) -> jvm::Result<bool> {
        if let Op::Cast(value) = inst.op {
            if self.value_kind(value)? == Kind::Reference
                && self.value_kind(inst.result.unwrap())? == Kind::Reference
            {
                self.load(value)?;
                let mut descriptor = String::new();
                representation::descriptor(
                    self.types,
                    self.body.value_type(inst.result.unwrap()),
                    &mut descriptor,
                )?;
                let name = descriptor
                    .strip_prefix('L')
                    .and_then(|s| s.strip_suffix(';'))
                    .unwrap_or(&descriptor);
                let class = self.cp.add_class(name)?;
                self.assembly.code.push(Instruction::Checkcast(class));
                return Ok(true);
            }
        }
        let field = match inst.op {
            Op::GetField { field, .. }
            | Op::SetField { field, .. }
            | Op::GetStatic(field)
            | Op::SetStatic { field, .. } => field,
            Op::Constant(id) if matches!(self.body.constants[id.index()], Constant::Null(_)) => {
                self.assembly.code.push(Instruction::Aconst_null);
                return Ok(true);
            }
            _ => return Ok(false),
        };
        let field = &self.body.fields[field.index()];
        let Some(Type::Class(symbol) | Type::Interface(symbol)) = self.types.get(field.owner)
        else {
            return Err(error("invalid field owner"));
        };
        let class = self.cp.add_class(self.types.symbol_name(symbol).unwrap())?;
        let mut descriptor = String::new();
        representation::descriptor(self.types, field.ty, &mut descriptor)?;
        let reference = self.cp.add_field_ref(class, &field.name, &descriptor)?;
        let op = match inst.op {
            Op::GetField { object, .. } => {
                self.load(object)?;
                Instruction::Getfield(reference)
            }
            Op::SetField { object, value, .. } => {
                self.load(object)?;
                self.argument(value)?;
                Instruction::Putfield(reference)
            }
            Op::GetStatic(_) => Instruction::Getstatic(reference),
            Op::SetStatic { value, .. } => {
                self.argument(value)?;
                Instruction::Putstatic(reference)
            }
            _ => unreachable!(),
        };
        self.assembly.code.push(op);
        if field.relative_pointer {
            let (object, store) = match inst.op {
                Op::GetField { object, .. } => (object, false),
                Op::SetField { object, .. } => (object, true),
                _ => return Err(error("relative pointer fields require an instance")),
            };
            for name in [
                relative_pointer_element_offset_field(&field.name),
                relative_pointer_byte_offset_field(&field.name),
            ] {
                let offset = self.cp.add_field_ref(class, name, "J")?;
                self.load(object)?;
                if store {
                    self.assembly
                        .code
                        .extend([Instruction::Lconst_0, Instruction::Putfield(offset)]);
                } else {
                    self.assembly.code.push(Instruction::Getfield(offset));
                }
            }
            if !store {
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let materialize = self.cp.add_method_ref(
                    owner,
                    "materializeRelative",
                    "(Lorg/rustlang/runtime/Pointer;JJ)Lorg/rustlang/runtime/Pointer;",
                )?;
                self.assembly
                    .code
                    .push(Instruction::Invokestatic(materialize));
            }
        }
        Ok(true)
    }
}
