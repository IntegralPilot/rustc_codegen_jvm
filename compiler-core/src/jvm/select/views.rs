//! Fat-pointer operations retain logical lengths and runtime pointer provenance.
use super::*;
use representation::{SLICE_VIEW_CLASS, UTF8_VIEW_CLASS};

impl Selector<'_> {
    pub(super) fn view(&mut self, inst: Inst) -> jvm::Result<bool> {
        match inst.op {
            Op::Length(view) => {
                self.load(view)?;
                let owner = self.cp.add_class(SLICE_VIEW_CLASS)?;
                let field = self.cp.add_field_ref(owner, "rustLength", "J")?;
                self.assembly.code.push(Instruction::Getfield(field));
            }
            Op::View { data, length } => {
                let owner = self.cp.add_class(
                    match self.types.get(self.body.value_type(inst.result.unwrap())) {
                        Some(Type::Str) => UTF8_VIEW_CLASS,
                        Some(Type::Slice(_)) => SLICE_VIEW_CLASS,
                        _ => return Err(error("invalid view representation")),
                    },
                )?;
                let init = self
                    .cp
                    .add_method_ref(owner, "<init>", "(Ljava/lang/Object;IJ)V")?;
                self.assembly
                    .code
                    .extend([Instruction::New(owner), Instruction::Dup]);
                self.load(data)?;
                self.assembly.code.push(Instruction::Iconst_0);
                self.load(length)?;
                self.assembly.code.push(Instruction::Invokespecial(init));
            }
            Op::ViewData { view, size, codec } => {
                self.load(view)?;
                self.assembly
                    .code
                    .push(get_int_const_instr(self.cp, size as i32));
                self.assembly.code.push(match codec {
                    Some(id) => {
                        Instruction::Ldc_w(self.cp.add_string(self.types.symbol_name(id).unwrap())?)
                    }
                    None => Instruction::Aconst_null,
                });
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(
                    owner,
                    "fromSlice",
                    "(Ljava/lang/Object;ILjava/lang/String;)Lorg/rustlang/runtime/Pointer;",
                )?;
                self.assembly.code.push(Instruction::Invokestatic(method));
            }
            _ => return Ok(false),
        }
        Ok(true)
    }
}
