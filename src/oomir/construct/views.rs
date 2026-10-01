//! Preserve view construction and data extraction until physical lowering.
use super::*;

impl Emission<'_> {
    pub(super) fn construct_view(
        &mut self,
        class: &str,
        args: &[(oomir::Operand, oomir::Type)],
    ) -> Result<Option<ir::ValueId>> {
        if !matches!(class, oomir::SLICE_VIEW_CLASS | oomir::UTF8_VIEW_CLASS) || args.len() != 3 {
            return Ok(None);
        }
        let mut values = Vec::new();
        for ((value, _), ty) in
            args.iter()
                .zip([types::object(), oomir::Type::I32, oomir::Type::U64])
        {
            let value = self.operand(value.clone())?;
            values.push(self.adapt(value, self.ty(&ty))?);
        }
        let parts = self.builder.args(values);
        Ok(self.emit(
            ir::Op::ViewPack(parts),
            Some(self.ty(&oomir::Type::Class(class.into()))),
        ))
    }

    pub(super) fn view_data(
        &mut self,
        name: &str,
        args: &[oomir::Operand],
        dest: &Option<String>,
    ) -> Result<bool> {
        if name == "nullableViewTag" && args.len() == 1 {
            let view = self.operand(args[0].clone())?;
            let root = self
                .emit(
                    ir::Op::ViewPart { view, index: 0 },
                    Some(self.ty(&types::object())),
                )
                .unwrap();
            let start = self
                .emit(
                    ir::Op::ViewPart { view, index: 1 },
                    Some(self.ty(&oomir::Type::I32)),
                )
                .unwrap();
            let tag = self
                .call(
                    oomir::POINTER_CLASS.into(),
                    "nullableViewLocationTag".into(),
                    vec![self.ty(&types::object()), self.ty(&oomir::Type::I32)],
                    self.ty(&oomir::Type::I64),
                    ir::CallKind::JvmStatic,
                    vec![root, start],
                )?
                .unwrap();
            if let Some(dest) = dest {
                self.write(dest, tag)?;
            }
            return Ok(true);
        }
        Ok(false)
    }
}
