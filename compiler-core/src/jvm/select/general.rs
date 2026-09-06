//! Allocation, representation boundaries and stateless callable construction.
use super::*;
use jvm::attributes::{ArrayType, BootstrapMethod};

impl Selector<'_> {
    pub(super) fn general(&mut self, inst: Inst) -> jvm::Result<bool> {
        match inst.op {
            Op::Exception => self.assembly.code.push(
                Kind::Reference.load(
                    self.exception_slot
                        .ok_or_else(|| error("exception value outside unwind body"))?,
                ),
            ),
            Op::Binary { op, left, right } if self.value_kind(left)? == Kind::Reference => {
                self.load(left)?;
                self.load(right)?;
                let yes = self.assembly.label();
                let done = self.assembly.label();
                self.assembly.branch(
                    match op {
                        BinaryOp::Eq => Instruction::If_acmpeq(0),
                        BinaryOp::Ne => Instruction::If_acmpne(0),
                        _ => return Err(error("invalid reference comparison")),
                    },
                    yes,
                );
                self.assembly.code.push(Instruction::Iconst_0);
                self.assembly.branch(Instruction::Goto_w(0), done);
                self.assembly.bind(yes);
                self.assembly.code.push(Instruction::Iconst_1);
                self.assembly.bind(done);
            }
            Op::Reinterpret(value) => self.load(value)?,
            Op::Adapt(value) => {
                self.load(value)?;
                self.constants
                    .ok_or_else(|| error("missing ABI provider"))?
                    .adapt(
                        self.body.value_type(value),
                        self.body.value_type(inst.result.unwrap()),
                        self.types,
                        &mut self.assembly.code,
                        self.cp,
                    )?;
            }
            Op::NewArray(size) => {
                self.load(size)?;
                let Some(Type::Array(element)) =
                    self.types.get(self.body.value_type(inst.result.unwrap()))
                else {
                    return Err(error("invalid array allocation type"));
                };
                use ScalarType::*;
                let primitive = match self.types.get(element) {
                    Some(Type::Scalar(Bool)) => Some(ArrayType::Boolean),
                    Some(Type::Scalar(I8 | U8)) => Some(ArrayType::Byte),
                    Some(Type::Scalar(I16 | F16)) => Some(ArrayType::Short),
                    Some(Type::Scalar(U16 | Char)) => Some(ArrayType::Char),
                    Some(Type::Scalar(I32 | U32)) => Some(ArrayType::Int),
                    Some(Type::Scalar(I64 | U64)) => Some(ArrayType::Long),
                    Some(Type::Scalar(F32)) => Some(ArrayType::Float),
                    Some(Type::Scalar(F64)) => Some(ArrayType::Double),
                    _ => None,
                };
                self.assembly.code.push(if let Some(primitive) = primitive {
                    Instruction::Newarray(primitive)
                } else {
                    let mut name = String::new();
                    representation::descriptor(self.types, element, &mut name)?;
                    let name = name
                        .strip_prefix('L')
                        .and_then(|n| n.strip_suffix(';'))
                        .unwrap_or(&name);
                    Instruction::Anewarray(self.cp.add_class(name)?)
                });
            }
            Op::FunctionPointer { signature, target } => {
                const METAFACTORY: &str = concat!(
                    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;",
                    "Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
                );
                let sam = &self.body.methods[signature.index()];
                let target = &self.body.methods[target.index()];
                let descriptor = method_descriptor(self.types, sam)?;
                let class = self.cp.add_class("java/lang/invoke/LambdaMetafactory")?;
                let method = self.cp.add_method_ref(class, "metafactory", METAFACTORY)?;
                let bootstrap_method_ref = self
                    .cp
                    .add_method_handle(jvm::ReferenceKind::InvokeStatic, method)?;
                let method_type = self.cp.add_method_type(&descriptor)?;
                let class = self.cp.add_class(&target.owner)?;
                let method = self.cp.add_method_ref(class, &target.name, &descriptor)?;
                let implementation = self
                    .cp
                    .add_method_handle(jvm::ReferenceKind::InvokeStatic, method)?;
                let bootstrap = self
                    .bootstrap
                    .as_mut()
                    .ok_or_else(|| error("missing bootstrap method table"))?;
                let index = u16::try_from(bootstrap.len())?;
                bootstrap.push(BootstrapMethod {
                    bootstrap_method_ref,
                    arguments: vec![method_type, implementation, method_type],
                });
                let site =
                    self.cp
                        .add_invoke_dynamic(index, &sam.name, format!("()L{};", sam.owner))?;
                self.assembly.code.push(Instruction::Invokedynamic(site));
            }
            _ => return Ok(false),
        }
        Ok(true)
    }
}

pub(super) fn method_descriptor(types: &Types, method: &MethodRef) -> jvm::Result<String> {
    let mut descriptor = String::from("(");
    for &ty in &method.params {
        representation::descriptor(types, ty, &mut descriptor)?;
    }
    descriptor.push(')');
    representation::descriptor(types, method.returns, &mut descriptor)?;
    Ok(descriptor)
}
