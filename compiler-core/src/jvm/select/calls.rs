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
        if matches!(kind, CallKind::RustStatic | CallKind::JvmStatic)
            && method.owner == POINTER_CLASS
            && method.name == "field"
            && signature
                == "(Ljava/lang/Object;Ljava/lang/String;JLjava/lang/String;)Lorg/rustlang/runtime/Pointer;"
            && self.runtime_call_site(
                "org/rustlang/runtime/FieldProjections",
                "field",
                &signature,
            )?
        {
            return Ok(());
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

    fn runtime_call_site(
        &mut self,
        owner: &str,
        name: &str,
        descriptor: &str,
    ) -> jvm::Result<bool> {
        let Some(bootstrap) = &mut self.bootstrap else {
            return Ok(false);
        };
        let owner = self.cp.add_class(owner)?;
        let method = self.cp.add_method_ref(
            owner,
            "bootstrap",
            concat!(
                "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;",
                "Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
            ),
        )?;
        let bootstrap_method_ref = self
            .cp
            .add_method_handle(jvm::ReferenceKind::InvokeStatic, method)?;
        let index = u16::try_from(bootstrap.len())?;
        bootstrap.push(jvm::attributes::BootstrapMethod {
            bootstrap_method_ref,
            arguments: vec![],
        });
        let site = self.cp.add_invoke_dynamic(index, name, descriptor)?;
        self.assembly.code.push(Instruction::Invokedynamic(site));
        Ok(true)
    }
}
