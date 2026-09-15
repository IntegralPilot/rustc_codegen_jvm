//! Array and view operations share explicit runtime representation boundaries.
use super::*;
use ir::{CallKind, Op};
impl Emission<'_> {
    fn element(&self, value: ir::ValueId) -> Result<ir::TypeId> {
        match self
            .vocabulary
            .types
            .get(self.builder.body.value_type(value))
        {
            Some(ir::Type::Array(ty) | ir::Type::Slice(ty) | ir::Type::Pointer(ty)) => Ok(ty),
            Some(ir::Type::Str) => Ok(self.ty(&oomir::Type::U8)),
            other => Err(format!("array element unavailable for {other:?}")),
        }
    }
    pub(super) fn array_get(
        &mut self,
        dest: String,
        array: oomir::Operand,
        index: oomir::Operand,
    ) -> Result<()> {
        let array = self.operand(array)?;
        let element = self.element(array)?;
        let index = self.operand(index)?;
        let index = self.adapt(index, self.ty(&oomir::Type::I32))?;
        if self.vocabulary.types.get(element) == Some(ir::Type::Unit) {
            let value = self.constant(oomir::Constant::Unit)?;
            return self.write(&dest, value);
        }
        let value = self
            .emit(Op::ArrayGet { array, index }, Some(element))
            .unwrap();
        self.write(&dest, value)
    }
    pub(super) fn array_store(
        &mut self,
        array: oomir::Operand,
        index: oomir::Operand,
        value: oomir::Operand,
        copy: bool,
    ) -> Result<()> {
        let array = self.operand(array)?;
        let element = self.element(array)?;
        let index = self.operand(index)?;
        let index = self.adapt(index, self.ty(&oomir::Type::I32))?;
        let mut value = self.operand(value)?;
        if self.vocabulary.types.get(element) == Some(ir::Type::Unit) {
            return Ok(());
        }
        if copy && self.vocabulary.types.get(element).unwrap().carrier() == 5 {
            value = self
                .call(
                    oomir::POINTER_CLASS.into(),
                    "copyManagedValue".into(),
                    vec![self.ty(&types::object())],
                    self.ty(&types::object()),
                    CallKind::JvmStatic,
                    vec![value],
                )?
                .unwrap();
        }
        let value = self.adapt(value, element)?;
        self.emit(
            Op::ArraySet {
                array,
                index,
                value,
            },
            None,
        );
        Ok(())
    }
    pub(super) fn array_fill(
        &mut self,
        array: oomir::Operand,
        value: oomir::Operand,
        copy: bool,
    ) -> Result<()> {
        let array = self.operand(array)?;
        let value = self.operand(value)?;
        let copy = self.constant(oomir::Constant::Boolean(copy))?;
        self.call(
            oomir::POINTER_CLASS.into(),
            "fillArray".into(),
            vec![
                self.ty(&types::object()),
                self.ty(&types::object()),
                self.ty(&oomir::Type::Boolean),
            ],
            self.ty(&oomir::Type::Unit),
            CallKind::JvmStatic,
            vec![array, value, copy],
        )?;
        Ok(())
    }
    pub(super) fn length(&mut self, dest: String, array: oomir::Operand) -> Result<()> {
        let value = self.operand(array)?;
        let result = self
            .emit(Op::ArrayLength(value), Some(self.ty(&oomir::Type::I32)))
            .unwrap();
        self.write(&dest, result)
    }
}
