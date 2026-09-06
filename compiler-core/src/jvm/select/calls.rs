use super::*;

use super::representation::descriptor;

impl Selector<'_> {
    pub(super) fn call(&mut self, id: MethodId, kind: CallKind, args: List) -> jvm::Result<()> {
        let method = &self.body.methods[id.index()];
        let mut signature = String::from("(");
        for &ty in &method.params {
            descriptor(self.types, ty, &mut signature)?;
        }
        signature.push(')');
        descriptor(self.types, method.returns, &mut signature)?;
        let owner = self.cp.add_class(&method.owner)?;
        let target = if method.interface {
            self.cp
                .add_interface_method_ref(owner, &method.name, &signature)?
        } else {
            self.cp.add_method_ref(owner, &method.name, &signature)?
        };
        if kind == CallKind::Constructor {
            self.assembly
                .code
                .extend([Instruction::New(owner), Instruction::Dup]);
        }
        for &arg in &self.body.args[args.range()] {
            self.argument(arg)?;
        }
        self.assembly.code.push(match kind {
            CallKind::RustStatic | CallKind::JvmStatic => Instruction::Invokestatic(target),
            CallKind::Constructor => Instruction::Invokespecial(target),
            CallKind::Virtual => Instruction::Invokevirtual(target),
            CallKind::Interface => {
                let words = self.body.args[args.range()]
                    .iter()
                    .try_fold(0u16, |n, &v| {
                        Ok::<_, jvm::Error>(n + self.value_kind(v)?.width())
                    })?;
                Instruction::Invokeinterface(target, u8::try_from(words)?)
            }
            _ => return Err(error("instance call selection requires reference values")),
        });
        Ok(())
    }
}
