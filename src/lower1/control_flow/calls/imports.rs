//! Imports.
use super::*;

pub(super) fn emit<'tcx>(
    tcx: TyCtxt<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    terminator: &rustc_middle::mir::Terminator<'tcx>,
    func_instance: Instance<'tcx>,
    mut oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    mut method_signature: oomir::Signature,
    jvm_import: crate::lower1::naming::JvmImport,
) {
    match jvm_import {
        crate::lower1::naming::JvmImport::Static(jvm_import) => {
            method_signature.is_static = true;
            let rust_descriptor = method_signature.to_jvm_descriptor_with_explicit_params();
            if let Some(explicit_descriptor) = &jvm_import.descriptor
                && rust_descriptor != *explicit_descriptor
            {
                tcx.dcx().span_fatal(
                terminator.source_info.span,
                format!(
                    "JVM import descriptor `{}` does not match the lowered Rust signature `{rust_descriptor}`",
                    explicit_descriptor
                ),
            );
            }
            instructions.push(oomir::Instruction::InvokeStatic {
                class_name: jvm_import.class_name,
                method_name: jvm_import.method_name,
                method_ty: method_signature,
                args: oomir_operands,
                dest: effective_dest,
            });
        }
        crate::lower1::naming::JvmImport::Virtual(jvm_import) => {
            let class_name =
                crate::lower1::naming::jvm_virtual_receiver_class_from_instance(tcx, func_instance)
                    .unwrap_or_else(|message| {
                        tcx.dcx().span_fatal(terminator.source_info.span, message)
                    });
            let receiver = oomir_operands.remove(0);
            method_signature.is_static = false;
            let rust_descriptor = method_signature.to_string();
            if let Some(explicit_descriptor) = &jvm_import.descriptor
                && rust_descriptor != *explicit_descriptor
            {
                tcx.dcx().span_fatal(
                terminator.source_info.span,
                format!(
                    "JVM import descriptor `{}` does not match the lowered Rust signature `{rust_descriptor}`",
                    explicit_descriptor
                ),
            );
            }
            instructions.push(oomir::Instruction::InvokeVirtual {
                class_name,
                method_name: jvm_import.method_name,
                method_ty: method_signature,
                args: oomir_operands,
                operand: receiver,
                dest: effective_dest,
            });
        }
        crate::lower1::naming::JvmImport::Field(jvm_import) => {
            let class_name =
                crate::lower1::naming::jvm_field_receiver_class_from_instance(tcx, func_instance)
                    .unwrap_or_else(|message| {
                        tcx.dcx().span_fatal(terminator.source_info.span, message)
                    });
            let access = crate::lower1::naming::classify_jvm_field_access(&method_signature, false)
                .unwrap_or_else(|message| {
                    tcx.dcx().span_fatal(terminator.source_info.span, message)
                });
            let receiver = oomir_operands.remove(0);
            match access {
                crate::lower1::naming::JvmFieldAccess::Getter { field_ty } => {
                    let dest = effective_dest.unwrap_or_else(|| {
                        tcx.dcx().span_fatal(
                            terminator.source_info.span,
                            "a JVM field getter must return a JVM value",
                        )
                    });
                    instructions.push(oomir::Instruction::GetJvmField {
                        dest,
                        object: receiver,
                        field_name: jvm_import.field_name,
                        field_ty,
                        class_name,
                    });
                }
                crate::lower1::naming::JvmFieldAccess::Setter { field_ty } => {
                    let value = oomir_operands.pop().unwrap_or_else(|| {
                        tcx.dcx().span_fatal(
                            terminator.source_info.span,
                            "a JVM field setter requires a value parameter",
                        )
                    });
                    instructions.push(oomir::Instruction::SetJvmField {
                        object: receiver,
                        field_name: jvm_import.field_name,
                        value,
                        field_ty,
                        class_name,
                    });
                }
            }
        }
        crate::lower1::naming::JvmImport::StaticField(jvm_import) => {
            let access = crate::lower1::naming::classify_jvm_field_access(&method_signature, true)
                .unwrap_or_else(|message| {
                    tcx.dcx().span_fatal(terminator.source_info.span, message)
                });
            match access {
                crate::lower1::naming::JvmFieldAccess::Getter { field_ty } => {
                    let dest = effective_dest.unwrap_or_else(|| {
                        tcx.dcx().span_fatal(
                            terminator.source_info.span,
                            "a JVM static field getter must return a JVM value",
                        )
                    });
                    instructions.push(oomir::Instruction::GetStaticField {
                        dest,
                        class_name: jvm_import.class_name,
                        field_name: jvm_import.field_name,
                        field_ty,
                    });
                }
                crate::lower1::naming::JvmFieldAccess::Setter { field_ty } => {
                    let value = oomir_operands.pop().unwrap_or_else(|| {
                        tcx.dcx().span_fatal(
                            terminator.source_info.span,
                            "a JVM static field setter requires a value parameter",
                        )
                    });
                    instructions.push(oomir::Instruction::SetStaticField {
                        class_name: jvm_import.class_name,
                        field_name: jvm_import.field_name,
                        value,
                        field_ty,
                    });
                }
            }
        }
        crate::lower1::naming::JvmImport::Constructor(jvm_import) => {
            let dest = effective_dest.unwrap_or_else(|| {
                tcx.dcx().span_fatal(
                    terminator.source_info.span,
                    "a JVM constructor import must return its linked extern type",
                )
            });
            let args = oomir_operands
                .into_iter()
                .zip(method_signature.params.into_iter().map(|(_, ty)| ty))
                .collect();
            instructions.push(oomir::Instruction::ConstructObject {
                dest,
                class_name: jvm_import.class_name,
                args,
            });
        }
    }
}
