//! Explicit SSA operations for transparent carrier packing and unpacking.
use super::*;
use ir::{CallKind, MethodRef, Op};

impl Emission<'_> {
    pub(super) fn wrapper_adaptation(
        &mut self,
        mut value: ir::ValueId,
        target: ir::TypeId,
    ) -> Result<Option<ir::ValueId>> {
        let source = source_type(&self.vocabulary.types, self.builder.body.value_type(value));
        let destination = source_type(&self.vocabulary.types, target);
        if self.context.is_subtype(&source, &destination) {
            return Ok(None);
        }
        if let Some(path) = self
            .context
            .path(source.clone(), &destination)
            .filter(|p| !p.is_empty())
        {
            for (owner, name, ty) in path {
                let ty = self.ty(&ty);
                let field = self.builder.field(ir::FieldRef {
                    owner: self.ty(&oomir::Type::Class(owner)),
                    name,
                    ty,
                    is_static: false,
                    relative_pointer: matches!(
                        self.vocabulary.types.get(ty),
                        Some(ir::Type::Pointer(_))
                    ),
                });
                value = self
                    .emit(
                        Op::GetField {
                            object: value,
                            field,
                        },
                        Some(ty),
                    )
                    .unwrap();
            }
            return self.adapt(value, target).map(Some);
        }
        if let Some(path) = self
            .context
            .path(destination, &source)
            .filter(|p| !p.is_empty())
        {
            for (owner, payload, _) in path.into_iter().rev() {
                let fields = self.context.wrappers[&owner].clone();
                let mut args = Vec::new();
                for (name, ty) in fields.iter().filter(|(_, ty)| ty.has_jvm_value()) {
                    args.push(if *name == payload {
                        value
                    } else {
                        self.zero_value(ty)?
                    });
                }
                value = self.construct(owner, &fields, args)?;
            }
            return self.adapt(value, target).map(Some);
        }
        Ok(None)
    }
    fn construct(
        &mut self,
        owner: String,
        fields: &[(String, oomir::Type)],
        values: Vec<ir::ValueId>,
    ) -> Result<ir::ValueId> {
        let method = self.builder.method(MethodRef {
            owner: owner.clone(),
            name: "<init>".into(),
            params: fields
                .iter()
                .filter(|(_, ty)| ty.has_jvm_value())
                .map(|(_, ty)| self.ty(ty))
                .collect(),
            returns: self.ty(&oomir::Type::Unit),
            interface: false,
        });
        let args = self.builder.args(values);
        Ok(self
            .emit(
                Op::Call {
                    method,
                    kind: CallKind::Constructor,
                    args,
                },
                Some(self.ty(&oomir::Type::Class(owner))),
            )
            .unwrap())
    }
    pub(super) fn zero_value(&mut self, ty: &oomir::Type) -> Result<ir::ValueId> {
        if !ty.has_jvm_value() {
            return self.constant(oomir::Constant::Unit);
        }
        let oomir::Type::Class(owner) = ty else {
            return Err("invalid zero-sized carrier".into());
        };
        let fields = self.context.wrappers[owner].clone();
        let values = fields
            .iter()
            .filter(|(_, ty)| ty.has_jvm_value())
            .map(|(_, ty)| self.zero_value(ty))
            .collect::<Result<Vec<_>>>()?;
        self.construct(owner.clone(), &fields, values)
    }
}
