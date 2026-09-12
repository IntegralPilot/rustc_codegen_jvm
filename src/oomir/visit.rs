//! Operands in the semantic construction buffer.
use super::{Instruction, Operand};

macro_rules! operand_visitor {
    ($name:ident $(, $mutable:tt)?) => {
        pub fn $name<'a>(&'a $($mutable)? self, mut visit: impl FnMut(&'a $($mutable)? Operand)) {
            use Instruction::*;
            match self {
                Binary { op1, op2, .. } => { visit(op1); visit(op2); }
                Not { src, .. } | Neg { src, .. } | Move { src, .. } => visit(src),
                Branch { condition, .. } => visit(condition),
                Return { operand } => { if let Some(operand) = operand { visit(operand); } }
                InvokeStatic { args, .. } | InvokeRustStatic { args, .. } => {
                    for arg in args { visit(arg); }
                }
                CallIndirect { function_ptr, args, .. } => {
                    visit(function_ptr);
                    for arg in args { visit(arg); }
                }
                InvokeInterface { operand, args, .. } | InvokeVirtual { operand, args, .. } => {
                    visit(operand);
                    for arg in args { visit(arg); }
                }
                Switch { discr, .. } => visit(discr),
                NewArray { size, .. } | Length { array: size, .. } => visit(size),
                ArrayStore { array, index, value, .. } => { visit(array); visit(index); visit(value); }
                ArrayFill { array, value, .. } => { visit(array); visit(value); }
                SetField { value, .. } | SetStaticField { value, .. } => visit(value),
                ArrayGet { array, index, .. } => { visit(array); visit(index); }
                ConstructObject { args, .. } => { for (arg, _) in args { visit(arg); } }
                SetJvmField { object, value, .. } => { visit(object); visit(value); }
                GetField { object, .. } | GetJvmField { object, .. } | Cast { op: object, .. } => visit(object),
                SourceLocation(_) | LocalVariableScope(_) | UnwindStart { .. } | UnwindEnd
                | Rethrow | CreateFunctionPointer { .. } | GetStaticField { .. } | Jump { .. }
                | ThrowNewWithMessage { .. } | Label { .. } => {}
            }
        }
    }
}

impl Instruction {
    operand_visitor!(visit_operands);
}
