//! Platform allocation enters SSA as storage operations, before JVM selection.
use super::*;

impl Emission<'_> {
    pub(super) fn heap(
        &mut self,
        operation: ir::HeapOp,
        operands: Vec<oomir::Operand>,
        dest: Option<String>,
    ) -> Result<()> {
        let pointer = self.ty(&oomir::Type::pointer(oomir::Type::U8));
        let object = self.ty(&oomir::Type::Class("java/lang/Object".into()));
        let long = self.ty(&oomir::Type::I64);
        let mut values = operands
            .into_iter()
            .map(|op| self.operand(op))
            .collect::<Result<Vec<_>>>()?;
        let mut args = Vec::new();
        if operation != ir::HeapOp::Allocate {
            let address = self.adapt(values.remove(0), pointer)?;
            for (index, ty) in [(0, object), (1, long)] {
                args.push(
                    self.emit(ir::Op::AddressPart { address, index }, Some(ty))
                        .unwrap(),
                );
            }
        }
        for value in values {
            args.push(self.adapt(value, long)?);
        }
        let returns = operation != ir::HeapOp::Deallocate;
        let args = self.builder.args(args);
        let root = self.emit(ir::Op::Heap { operation, args }, returns.then_some(object));
        if let (Some(root), Some(dest)) = (root, dest) {
            let zero = self.constant(oomir::Constant::I64(0))?;
            let parts = self.builder.args([root, zero]);
            let address = self
                .emit(ir::Op::AddressPack(parts), Some(pointer))
                .unwrap();
            self.write(&dest, address)?;
        }
        Ok(())
    }
}
