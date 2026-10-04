//! Fat-pointer operations retain logical lengths and runtime pointer provenance.
use super::*;
use representation::{SLICE_VIEW_CLASS, UTF8_VIEW_CLASS};

impl Selector<'_> {
    pub(super) fn view(&mut self, inst: Inst) -> jvm::Result<bool> {
        match inst.op {
            Op::TaggedPack(parts) => {
                self.materialize_tagged(parts)?;
            }
            Op::TaggedPart { value, index } => {
                self.load(value)?;
                let owner = self.cp.add_class(super::super::abi::TAGGED_LONG_CLASS)?;
                self.assembly
                    .code
                    .push(Instruction::Invokestatic(self.cp.add_method_ref(
                        owner,
                        if index == 0 { "value" } else { "tag" },
                        "(Lorg/rustlang/runtime/TaggedLong;)J",
                    )?));
            }
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
            Op::ViewPart { view, index } => {
                self.load(view)?;
                self.assembly.code.push(super::super::abi::view_part_access(
                    self.cp,
                    index as usize,
                )?);
            }
            Op::ViewPack(parts) => {
                self.materialize_view(self.body.value_type(inst.result.unwrap()), parts)?;
            }
            Op::ViewData { view, size, codec } => {
                self.load(view)?;
                self.assembly
                    .code
                    .push(get_int_const_instr(self.cp, size as i32));
                self.assembly.code.push(match codec {
                    Some(id) => Instruction::Ldc_w(
                        self.cp
                            .add_name_string(self.types.symbol_name(id).unwrap())?,
                    ),
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
            Op::ViewAddress { parts, size, codec } => {
                for &part in &self.body.args[parts.range()] {
                    self.load(part)?;
                }
                self.assembly
                    .code
                    .push(get_int_const_instr(self.cp, size as i32));
                self.assembly.code.push(match codec {
                    Some(id) => Instruction::Ldc_w(
                        self.cp
                            .add_name_string(self.types.symbol_name(id).unwrap())?,
                    ),
                    None => Instruction::Aconst_null,
                });
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(
                    owner,
                    "fromSliceParts",
                    "(Ljava/lang/Object;IJILjava/lang/String;)Lorg/rustlang/runtime/Pointer;",
                )?;
                self.assembly.code.push(Instruction::Invokestatic(method));
            }
            Op::ViewRoot {
                backing,
                size,
                codec,
            } => {
                self.load(backing)?;
                if codec.is_some() {
                    self.address_layout(size, codec)?;
                } else {
                    self.assembly
                        .code
                        .push(get_int_const_instr(self.cp, size as i32));
                }
                let owner = self.cp.add_class(POINTER_CLASS)?;
                let method = self.cp.add_method_ref(
                    owner,
                    if codec.is_some() {
                        "sliceAddressRoot"
                    } else {
                        "scalarSliceRoot"
                    },
                    if codec.is_some() {
                        "(Ljava/lang/Object;ILjava/lang/String;)Ljava/lang/Object;"
                    } else {
                        "(Ljava/lang/Object;I)Ljava/lang/Object;"
                    },
                )?;
                self.assembly.code.push(Instruction::Invokestatic(method));
            }
            _ => return Ok(false),
        }
        Ok(true)
    }
    pub(super) fn materialize_tagged(&mut self, parts: List) -> jvm::Result<()> {
        let owner = self.cp.add_class(super::super::abi::TAGGED_LONG_CLASS)?;
        self.assembly
            .code
            .extend([Instruction::New(owner), Instruction::Dup]);
        for &value in &self.body.args[parts.range()] {
            self.load(value)?;
        }
        self.assembly.code.push(Instruction::Invokespecial(
            self.cp.add_method_ref(owner, "<init>", "(JJ)V")?,
        ));
        Ok(())
    }
    pub(super) fn materialize_view(&mut self, ty: TypeId, parts: List) -> jvm::Result<()> {
        let owner = self
            .cp
            .add_class(if matches!(self.types.get(ty), Some(Type::Str))
                || matches!(self.types.get(ty), Some(Type::Class(s)) if self.types.symbol_name(s) == Some(UTF8_VIEW_CLASS)) {
                UTF8_VIEW_CLASS
            } else {
                SLICE_VIEW_CLASS
            })?;
        let init = self
            .cp
            .add_method_ref(owner, "<init>", "(Ljava/lang/Object;IJ)V")?;
        self.assembly
            .code
            .extend([Instruction::New(owner), Instruction::Dup]);
        for &value in &self.body.args[parts.range()] {
            self.load(value)?;
        }
        self.assembly.code.push(Instruction::Invokespecial(init));
        Ok(())
    }
}
