//! This is the stage 1 lowering pass of the compiler.
//! It is responsible for coverting the MIR into a lower-level IR, called OOMIR (see src/oomir.rs).
//! It is a simple pass that converts the MIR into a more object-oriented representation.

// lower1.rs
//! This module converts Rust MIR into an object-oriented MIR (OOMIR)
//! that sits between MIR and JVM bytecode. It supports a subset of Rust constructs
//! (arithmetic, branching, returns) and can be extended to support more of Rust.

use crate::oomir;
use rustc_abi::Size;
use rustc_middle::mir::interpret::{AllocRange, ErrorHandled};
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, BinOp, Body, Operand as MirOperand, Place, Rvalue, StatementKind,
    TerminatorKind,
};
use rustc_middle::mir::{Const, ConstOperand, ConstValue, interpret::Scalar};
use rustc_middle::ty::TypingEnv;
use rustc_middle::ty::{
    FloatTy, Instance, IntTy, PseudoCanonicalInput, Ty, TyCtxt, TyKind, UintTy,
};
use std::collections::HashMap;

/// Converts a Rust MIR type (`Ty`) to an OOMIR type (`oomir::Type`).
fn ty_to_oomir_type<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> oomir::Type {
    match ty.kind() {
        rustc_middle::ty::TyKind::Bool => oomir::Type::Boolean,
        rustc_middle::ty::TyKind::Char => oomir::Type::Char,
        rustc_middle::ty::TyKind::Int(int_ty) => match int_ty {
            IntTy::I8 => oomir::Type::I8,
            IntTy::I16 => oomir::Type::I16,
            IntTy::I32 => oomir::Type::I32,
            IntTy::I64 => oomir::Type::I64,
            IntTy::Isize => oomir::Type::I64,
            IntTy::I128 => oomir::Type::I64,
        },
        rustc_middle::ty::TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => oomir::Type::U8,
            UintTy::U16 => oomir::Type::U16,
            UintTy::U32 => oomir::Type::U32,
            UintTy::U64 => oomir::Type::U64,
            UintTy::Usize => oomir::Type::U64,
            UintTy::U128 => oomir::Type::U64,
        },
        rustc_middle::ty::TyKind::Float(float_ty) => match float_ty {
            FloatTy::F32 => oomir::Type::F32,
            FloatTy::F64 => oomir::Type::F64,
            FloatTy::F16 => oomir::Type::F32,
            FloatTy::F128 => oomir::Type::F64,
        },
        rustc_middle::ty::TyKind::Adt(adt_def, _) => {
            oomir::Type::Class(format!("{:?}", adt_def.did()))
        }
        rustc_middle::ty::TyKind::Ref(_, inner_ty, _)
        | rustc_middle::ty::TyKind::RawPtr(inner_ty, _) => {
            oomir::Type::Reference(Box::new(ty_to_oomir_type(*inner_ty, tcx)))
        }
        rustc_middle::ty::TyKind::Array(component_ty, _) => {
            oomir::Type::Array(Box::new(ty_to_oomir_type(*component_ty, tcx)))
        }
        rustc_middle::ty::TyKind::Tuple(tuple_struct) if tuple_struct.len() == 0 => {
            // ADD THIS CASE
            oomir::Type::Void // Unit type maps to OOMIR Void
        }
        _ => {
            println!("Warning: Unhandled type {:?}", ty);
            oomir::Type::Class("UnsupportedType".to_string())
        }
    }
}

fn place_to_string<'tcx>(place: &Place<'tcx>, _tcx: TyCtxt<'tcx>) -> String {
    // Base variable name (e.g., "_1")
    let mut name = format!("_{}", place.local.index()); // Start with base local "_N"

    // Append projections cleanly
    for proj_elem in place.projection.iter() {
        match proj_elem {
            rustc_middle::mir::ProjectionElem::Field(field, _ty) => {
                // Append ".index" for field access
                name.push_str(&format!(".{}", field.index()));
            }
            // --- Handle other projections as needed, cleanly ---
            rustc_middle::mir::ProjectionElem::Deref => {
                name = format!("deref_{}", name); // Or maybe just skip for simple cases?
            }
            rustc_middle::mir::ProjectionElem::Index(local) => {
                name.push_str(&format!(".idx_{}", local.index())); // Example for index
            }
            // ... etc ...
            _ => {
                name.push_str(".?"); // Placeholder for others
            }
        }
    }
    name // Return the simple, projected name (e.g., "_7.0")
}

/// Convert a MIR operand to an OOMIR operand.
fn convert_operand<'tcx>(mir_op: &MirOperand<'tcx>, tcx: TyCtxt<'tcx>) -> oomir::Operand {
    match mir_op {
        MirOperand::Constant(box constant) => {
            match constant.const_ {
                Const::Val(const_val, ty) => handle_const_value(constant, const_val, &ty, tcx),
                Const::Ty(const_ty, _) => {
                    println!("Warning: Const::Ty constants not handled: {:?}", const_ty);
                    oomir::Operand::Constant(oomir::Constant::I32(0))
                }
                Const::Unevaluated(uv, ty) => {
                    // Create the parameter environment. reveal_all is usually okay for codegen.
                    let typing_env = TypingEnv::post_analysis(tcx, uv.def);

                    // Try to evaluate the unevaluated constant using the correct function
                    // Use uv (the UnevaluatedConst) directly.
                    // Pass Some(span) for better error location if evaluation fails.
                    let span = tcx.def_span(uv.def); // Define `span` using the definition span of `uv`
                    match tcx.const_eval_resolve(typing_env, uv, span) {
                        Ok(const_val) => {
                            // Evaluation succeeded!
                            println!(
                                "Successfully evaluated Unevaluated constant ({:?} at {:?}): {:?}",
                                uv, span, const_val
                            );
                            // Now handle the resulting ConstValue using the existing function
                            handle_const_value(&constant, const_val, &ty, tcx)
                        }
                        Err(ErrorHandled::Reported(error_reported, ..)) => {
                            // An error occurred during evaluation and rustc has already reported it.
                            println!(
                                "ERROR: Const evaluation failed for {:?} (already reported) at {:?}. Error: {:?}",
                                uv,
                                span,
                                error_reported // error_reported is a DiagnosticBuilder emission guarantee token
                            );
                            // You might want to propagate this error or panic.
                            oomir::Operand::Constant(oomir::Constant::I32(-1)) // Error placeholder
                        }
                        Err(ErrorHandled::TooGeneric(..)) => {
                            // The constant couldn't be evaluated because it depends on generic
                            // parameters that weren't fully specified. This is often an error
                            // for final codegen.
                            println!(
                                "Warning: Could not evaluate Unevaluated constant {:?} at {:?} due to generics.",
                                uv, span
                            );
                            oomir::Operand::Constant(oomir::Constant::I32(-2)) // Placeholder for generic error
                        }
                    }
                    // --- END OF const_eval_resolve LOGIC ---
                }
            }
        }
        MirOperand::Copy(place) | MirOperand::Move(place) => {
            let var_name = place_to_string(place, tcx);
            oomir::Operand::Variable(var_name)
        }
    }
}

fn handle_const_value<'tcx>(
    constant: &ConstOperand,
    const_val: ConstValue<'tcx>,
    ty: &Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> oomir::Operand {
    match const_val {
        ConstValue::Scalar(scalar) => {
            match scalar {
                Scalar::Int(scalar_int) => {
                    match ty.kind() {
                        rustc_middle::ty::TyKind::Int(int_ty) => {
                            let val = match int_ty {
                                IntTy::I8 => scalar_int.to_i8() as i32,
                                IntTy::I16 => scalar_int.to_i16() as i32,
                                IntTy::I32 => scalar_int.to_i32(),
                                _ => scalar_int.to_i32(), // Default
                            };
                            oomir::Operand::Constant(oomir::Constant::I32(val))
                        }
                        rustc_middle::ty::TyKind::Uint(uint_ty) => {
                            let val = match uint_ty {
                                UintTy::U8 => scalar_int.to_u8() as u32,
                                UintTy::U16 => scalar_int.to_u16() as u32,
                                UintTy::U32 => scalar_int.to_u32(),
                                _ => scalar_int.to_u32(), // Default
                            };
                            oomir::Operand::Constant(oomir::Constant::U32(val))
                        }
                        rustc_middle::ty::TyKind::Bool => {
                            let val = scalar_int.try_to_bool().unwrap_or(false);
                            oomir::Operand::Constant(oomir::Constant::Boolean(val))
                        }
                        rustc_middle::ty::TyKind::Char => {
                            let val = char::from_u32(scalar_int.to_u32()).unwrap_or('\0');
                            oomir::Operand::Constant(oomir::Constant::Char(val))
                        }
                        _ => {
                            println!("Warning: Unhandled scalar type {:?} for Scalar::Int", ty);
                            oomir::Operand::Constant(oomir::Constant::I32(0))
                        }
                    }
                }
                Scalar::Ptr(pointer, _) => {
                    let alloc_id = pointer.provenance.alloc_id();
                    let alloc = tcx.global_alloc(alloc_id);
                    match alloc {
                        rustc_middle::mir::interpret::GlobalAlloc::Memory(const_allocation) => {
                            let allocation = const_allocation.inner();

                            // Determine the range to read
                            let size = allocation.size();
                            let range = 0..size.bytes_usize();

                            // Read the raw bytes, ignoring provenance and initialization checks
                            // Should be okay as we are "outisde the interpreter" as the name suggests
                            let bytes: &[u8] =
                                allocation.inspect_with_uninit_and_ptr_outside_interpreter(range);

                            // Now, try to interpret the bytes based on the original type 'ty'
                            match ty.kind() {
                                // If the original type was a reference to a string literal...
                                TyKind::Ref(_, inner_ty, _) if inner_ty.is_str() => {
                                    match String::from_utf8(bytes.to_vec()) {
                                        Ok(s) => {
                                            println!(
                                                "Info: Successfully extracted string constant from allocation: \"{}\"",
                                                s
                                            );
                                            oomir::Operand::Constant(oomir::Constant::String(s))
                                        }
                                        Err(e) => {
                                            println!(
                                                "Warning: Bytes from allocation {:?} for &str were not valid UTF-8: {}",
                                                alloc_id, e
                                            );
                                            // TODO: make OOMIR support raw bytes?
                                            oomir::Operand::Constant(oomir::Constant::String(
                                                "Invalid UTF8".to_string(),
                                            ))
                                        }
                                    }
                                }
                                TyKind::Ref(_, inner_ty, _) => {
                                    if let TyKind::Array(elem_ty, array_len_const) = inner_ty.kind()
                                    {
                                        // We are expecting [&str; 1] from panic!/assert! format strings
                                        let array_len = array_len_const.try_to_target_usize(tcx);
                                        if array_len == Some(1) {
                                            // Check it's a single-element array
                                            // Check if the element type is &str
                                            if let TyKind::Ref(_, str_ty, _) = elem_ty.kind() {
                                                if str_ty.is_str() {
                                                    println!(
                                                        "Info: Handling Ref-to-[&str; 1]. Reading inner fat pointer from allocation {:?}.",
                                                        alloc_id
                                                    );

                                                    let typing_env =
                                                        TypingEnv::fully_monomorphized();
                                                    // Get layout of the element type (&str)
                                                    match tcx.layout_of(PseudoCanonicalInput {
                                                        typing_env: typing_env,
                                                        value: *elem_ty,
                                                    }) {
                                                        Ok(elem_layout) => {
                                                            // Check layout size is correct for fat pointer (*const u8, usize)
                                                            let expected_size = tcx
                                                                .data_layout
                                                                .pointer_size
                                                                .checked_mul(2, &tcx.data_layout)
                                                                .expect(
                                                                    "Pointer size * 2 overflowed",
                                                                );
                                                            if !elem_layout.is_unsized()
                                                                && elem_layout.size == expected_size
                                                            {
                                                                let pointer_size =
                                                                    tcx.data_layout.pointer_size;

                                                                // Read the data pointer part (offset 0 in *this* allocation)
                                                                let data_ptr_range = AllocRange {
                                                                    start: Size::from_bytes(0),
                                                                    size: pointer_size,
                                                                };
                                                                let data_ptr_scalar_res =
                                                                    allocation.read_scalar(
                                                                        &tcx.data_layout,
                                                                        data_ptr_range,
                                                                        true,
                                                                    ); // Read provenance

                                                                // Read the length part (offset pointer_size in *this* allocation)
                                                                let len_range = AllocRange {
                                                                    start: pointer_size,
                                                                    size: pointer_size,
                                                                };
                                                                let len_scalar_res = allocation
                                                                    .read_scalar(
                                                                        &tcx.data_layout,
                                                                        len_range,
                                                                        false,
                                                                    ); // No provenance for len

                                                                match (
                                                                    data_ptr_scalar_res,
                                                                    len_scalar_res,
                                                                ) {
                                                                    (
                                                                        Ok(Scalar::Ptr(
                                                                            data_ptr,
                                                                            _,
                                                                        )),
                                                                        Ok(Scalar::Int(len_scalar)),
                                                                    ) => {
                                                                        let len = len_scalar
                                                                            .to_target_isize(tcx);

                                                                        // Get AllocId and offset from the *inner* data pointer
                                                                        let final_alloc_id =
                                                                            data_ptr
                                                                                .provenance
                                                                                .alloc_id();
                                                                        let (_, final_offset) =
                                                                            data_ptr.into_parts();

                                                                        // Get the *final* allocation containing the string bytes
                                                                        match tcx.global_alloc(final_alloc_id) {
                                                                            rustc_middle::mir::interpret::GlobalAlloc::Memory(final_const_alloc) => {
                                                                                let final_alloc = final_const_alloc.inner();
                                                                                let start_byte = final_offset.bytes_usize();
                                                                                let end_byte = start_byte.checked_add(len as usize).expect("String slice range overflowed");
                                                                                let final_range = start_byte..end_byte;
                                                                                // Bounds check
                                                                                if end_byte > final_alloc.size().bytes_usize() {
                                                                                    println!("Warning: Calculated string slice range {:?} out of bounds for allocation {:?} size {}", final_range, final_alloc_id, final_alloc.size().bytes());
                                                                                    return oomir::Operand::Constant(oomir::Constant::I64(0));
                                                                                }
                                                                                // Read the final bytes
                                                                                let final_bytes = final_alloc.inspect_with_uninit_and_ptr_outside_interpreter(final_range);
            
                                                                                // Convert to string
                                                                                match String::from_utf8(final_bytes.to_vec()) {
                                                                                    Ok(s) => {
                                                                                        println!("Info: Successfully extracted inner &str constant via fat pointer: \"{}\"", s);
                                                                                        oomir::Operand::Constant(oomir::Constant::String(s))
                                                                                    }
                                                                                    Err(e) => {
                                                                                        println!("Warning: Final string bytes from allocation {:?} were not valid UTF-8: {}", final_alloc_id, e);
                                                                                        oomir::Operand::Constant(oomir::Constant::String("Invalid UTF8 (Inner)".to_string()))
                                                                                    }
                                                                                }
                                                                            }
                                                                            // Handle case where inner pointer points to function/static/vtable (unlikely for string)
                                                                            _ => {
                                                                                println!("Warning: Inner string pointer {:?} points to unexpected GlobalAlloc kind {:?}", data_ptr, tcx.global_alloc(final_alloc_id));
                                                                                oomir::Operand::Constant(oomir::Constant::I64(0))
                                                                            }
                                                                        }
                                                                    }
                                                                    // Error handling for reading scalars
                                                                    (Err(e), _) => {
                                                                        println!(
                                                                            "Error reading inner string data pointer scalar from allocation {:?}: {:?}",
                                                                            alloc_id, e
                                                                        );
                                                                        oomir::Operand::Constant(
                                                                            oomir::Constant::I64(0),
                                                                        )
                                                                    }
                                                                    (_, Err(e)) => {
                                                                        println!(
                                                                            "Error reading inner string length scalar from allocation {:?}: {:?}",
                                                                            alloc_id, e
                                                                        );
                                                                        oomir::Operand::Constant(
                                                                            oomir::Constant::I64(0),
                                                                        )
                                                                    }
                                                                    (Ok(data), Ok(len)) => {
                                                                        println!(
                                                                            "Warning: Read unexpected scalar types for fat pointer. Data: {:?}, Len: {:?}",
                                                                            data, len
                                                                        );
                                                                        oomir::Operand::Constant(
                                                                            oomir::Constant::I64(0),
                                                                        )
                                                                    }
                                                                }
                                                            } else {
                                                                println!(
                                                                    "Warning: Layout of element type {:?} doesn't look like a fat pointer (&str). Size: {:?}, Expected: {:?}",
                                                                    elem_ty,
                                                                    elem_layout.size,
                                                                    expected_size
                                                                );
                                                                oomir::Operand::Constant(
                                                                    oomir::Constant::I64(0),
                                                                )
                                                            }
                                                        }
                                                        Err(e) => {
                                                            println!(
                                                                "Error getting layout for element type {:?}: {:?}",
                                                                elem_ty, e
                                                            );
                                                            oomir::Operand::Constant(
                                                                oomir::Constant::I64(0),
                                                            )
                                                        }
                                                    }
                                                } else {
                                                    /* Array element not &str */
                                                    println!(
                                                        "Warning: Scalar::Ptr points to Ref-to-Array where element type {:?} is not &str.",
                                                        elem_ty
                                                    );
                                                    return oomir::Operand::Constant(
                                                        oomir::Constant::I64(0),
                                                    );
                                                }
                                            } else {
                                                /* Array element not Ref */
                                                println!(
                                                    "Warning: Scalar::Ptr points to Ref-to-Array where element type {:?} is not a reference.",
                                                    elem_ty
                                                );
                                                return oomir::Operand::Constant(
                                                    oomir::Constant::I64(0),
                                                );
                                            }
                                        } else {
                                            // Array length != 1
                                            println!(
                                                "Warning: Scalar::Ptr points to Ref-to-Array with unexpected length: {:?}. Expected 1.",
                                                array_len
                                            );
                                            return oomir::Operand::Constant(oomir::Constant::I64(
                                                0,
                                            ));
                                        }
                                    } else {
                                        // Inner type not array
                                        println!(
                                            "Warning: Scalar::Ptr points to Ref to non-Array type {:?}. Not a recognized string pattern.",
                                            inner_ty
                                        );
                                        return oomir::Operand::Constant(oomir::Constant::I64(0));
                                    }
                                } // End Case 2
                                // Handle other pointer types if necessary
                                _ => {
                                    println!(
                                        "Warning: Scalar::Ptr points to an allocation, but the type {:?} is not a recognized string or slice ref.",
                                        ty
                                    );
                                    oomir::Operand::Constant(oomir::Constant::I64(0))
                                }
                            }
                        }
                        _ => {
                            println!("Warning: Pointer to non-memory allocation not handled yet.");
                            oomir::Operand::Constant(oomir::Constant::I32(0))
                        }
                    }
                }
            }
        }
        ConstValue::Slice { data: _, meta: _ } => {
            if ty.is_str() || ty.is_slice() {
                match const_val.try_get_slice_bytes_for_diagnostics(tcx) {
                    Some(bytes) => match String::from_utf8(bytes.to_vec()) {
                        Ok(s) => {
                            println!("Info: Correctly extracted string constant: \"{}\"", s);
                            oomir::Operand::Constant(oomir::Constant::String(s))
                        }
                        Err(_) => {
                            println!(
                                "Warning: Slice constant bytes are not valid UTF-8: {:?}",
                                bytes
                            );
                            oomir::Operand::Constant(oomir::Constant::String(
                                "Invalid UTF8".to_string(),
                            ))
                        }
                    },
                    None => {
                        println!(
                            "Warning: Could not get slice bytes for diagnostics for: {:?}",
                            constant
                        );
                        oomir::Operand::Constant(oomir::Constant::String(
                            "SliceReadError".to_string(),
                        ))
                    }
                }
            } else {
                println!(
                    "Warning: ConstValue::Slice found for non-slice type: {:?}",
                    ty
                );
                oomir::Operand::Constant(oomir::Constant::String("NonSliceTypeError".to_string()))
            }
        }
        ConstValue::ZeroSized => {
            println!("Info: Encountered ZeroSized constant for type {:?}", ty);
            oomir::Operand::Constant(oomir::Constant::I32(0)) // Placeholder
        }
        ConstValue::Indirect {
            alloc_id: _,
            offset: _,
        } => {
            println!("Warning: Indirect constants not fully handled yet.");
            oomir::Operand::Constant(oomir::Constant::I32(0)) // Placeholder
        }
    }
}

// Other functions (e.g., `convert_basic_block`, `mir_to_oomir`) should be updated similarly.
/// Convert a single MIR basic block into an OOMIR basic block.
fn convert_basic_block<'tcx>(
    bb: BasicBlock,
    bb_data: &BasicBlockData<'tcx>,
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    return_oomir_type: &oomir::Type, // Pass function return type
    basic_blocks: &mut HashMap<String, oomir::BasicBlock>,
) -> oomir::BasicBlock {
    // Use the basic block index as its label.
    let label = format!("bb{}", bb.index());
    let mut instructions = Vec::new();

    // Convert each MIR statement in the block.
    for stmt in &bb_data.statements {
        if let StatementKind::Assign(box (place, rvalue)) = &stmt.kind {
            let dest = format!("{:?}", place); // Destination variable
            match rvalue {
                Rvalue::BinaryOp(bin_op, box (op1, op2)) => {
                    let oomir_op1 = convert_operand(op1, tcx);
                    let oomir_op2 = convert_operand(op2, tcx);

                    match bin_op {
                        // Non-checked ops remain the same
                        BinOp::Add => instructions.push(oomir::Instruction::Add {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::Sub => instructions.push(oomir::Instruction::Sub {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::Mul => instructions.push(oomir::Instruction::Mul {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::Eq => instructions.push(oomir::Instruction::Eq {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::BitAnd => instructions.push(oomir::Instruction::BitAnd {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::Rem => {
                            instructions.push(oomir::Instruction::Rem {
                                dest,
                                op1: oomir_op1,
                                op2: oomir_op2,
                            });
                        }
                        // Checked ops: Generate a tuple with result and overflow flag
                        BinOp::AddWithOverflow => {
                            let tuple_dest = format!("{}_tuple", dest);
                            instructions.push(oomir::Instruction::AddWithOverflow {
                                dest: tuple_dest.clone(),
                                op1: oomir_op1,
                                op2: oomir_op2,
                            });

                            // Split the tuple into result and overflow flag
                            let result_dest = format!("{}_result", tuple_dest);
                            let overflow_dest = format!("{}_overflow", tuple_dest);
                            instructions.push(oomir::Instruction::Move {
                                src: oomir::Operand::Variable(result_dest.clone()),
                                dest: format!("{}.0", dest),
                            });
                            instructions.push(oomir::Instruction::Move {
                                src: oomir::Operand::Variable(overflow_dest.clone()),
                                dest: format!("{}.1", dest),
                            });
                        }
                        BinOp::SubWithOverflow => {
                            let tuple_dest = format!("{}_tuple", dest);
                            instructions.push(oomir::Instruction::SubWithOverflow {
                                dest: tuple_dest.clone(),
                                op1: oomir_op1,
                                op2: oomir_op2,
                            });

                            // Split the tuple into result and overflow flag
                            let result_dest = format!("{}_result", tuple_dest);
                            let overflow_dest = format!("{}_overflow", tuple_dest);
                            instructions.push(oomir::Instruction::Move {
                                src: oomir::Operand::Variable(result_dest.clone()),
                                dest: format!("{}.0", dest),
                            });
                            instructions.push(oomir::Instruction::Move {
                                src: oomir::Operand::Variable(overflow_dest.clone()),
                                dest: format!("{}.1", dest),
                            });
                        }
                        // Default case for other binary ops
                        _ => {
                            println!(
                                "Warning: Unsupported binary operation {:?} in block {}",
                                bin_op,
                                bb.index()
                            );
                            // Optionally push a placeholder or skip
                        }
                    }
                }
                Rvalue::Use(mir_operand) => {
                    // Convert the MIR operand (_1, constant, _7.0, etc.)
                    let mut src_oomir_operand = convert_operand(mir_operand, tcx);

                    // Check if the *source* operand is a variable that looks like a tuple field access
                    if let oomir::Operand::Variable(ref src_name) = src_oomir_operand {
                        if src_name.contains('.') {
                            // Handle tuple projections (e.g., _7.0, _7.1)
                            if let Some((base_var, field)) = src_name.split_once('.') {
                                let projection_var = format!("{}_proj_{}", base_var, field);
                                instructions.push(oomir::Instruction::Move {
                                    dest: projection_var.clone(),
                                    src: oomir::Operand::Variable(src_name.clone()),
                                });
                                src_oomir_operand = oomir::Operand::Variable(projection_var);
                            }
                        }
                    }

                    // Generate the OOMIR Move instruction with the (potentially adjusted) source
                    instructions.push(oomir::Instruction::Move {
                        dest: dest.clone(),
                        src: src_oomir_operand,
                    });
                }
                Rvalue::Aggregate(box kind, operands) => {
                    match kind {
                        rustc_middle::mir::AggregateKind::Tuple => {
                            // 'dest' here is the name of the tuple itself (e.g., "_2")
                            // We need to define its fields.
                            if operands.len() > 2 {
                                println!(
                                    "Warning: Tuple aggregate with >2 fields not fully handled: {}",
                                    dest
                                );
                            }
                            for (i, mir_op) in operands.iter().enumerate() {
                                // Define the field variable name (e.g., "_2.0")
                                let field_dest = format!("{}.{}", dest, i);
                                let field_src = convert_operand(mir_op, tcx);
                                // Generate a Move to define the field variable
                                instructions.push(oomir::Instruction::Move {
                                    dest: field_dest,
                                    src: field_src,
                                });
                            }
                            // We might not need an explicit instruction for the tuple 'dest' itself
                            // if we only ever access its fields directly via the ".N" names.
                        }
                        _ => {
                            println!(
                                "Warning: Unhandled aggregate kind {:?} for dest {}",
                                kind, dest
                            );
                            // Assign a default/dummy value? Might cause issues later.
                            // For now, maybe push a placeholder move:
                            instructions.push(oomir::Instruction::Move {
                                dest: dest.clone(),
                                src: oomir::Operand::Constant(oomir::Constant::I32(0)), // Placeholder!
                            });
                        }
                    }
                }
                _ => {
                    println!("Warning: Unhandled rvalue {:?}", rvalue);
                }
            }
        }
    }

    // Convert the MIR terminator into corresponding OOMIR instructions.
    if let Some(terminator) = &bb_data.terminator {
        match &terminator.kind {
            TerminatorKind::Return => {
                // Handle Return without operand
                if *return_oomir_type == oomir::Type::Void {
                    instructions.push(oomir::Instruction::Return { operand: None });
                } else {
                    let return_operand =
                        convert_operand(&MirOperand::Move(Place::return_place()), tcx);
                    instructions.push(oomir::Instruction::Return {
                        operand: Some(return_operand),
                    });
                }
            }
            TerminatorKind::Goto { target } => {
                let target_label = format!("bb{}", target.index());
                instructions.push(oomir::Instruction::Jump {
                    target: target_label,
                });
            }
            TerminatorKind::SwitchInt { discr, targets, .. } => {
                // --- GENERAL SwitchInt Handling ---
                let discr_operand = convert_operand(discr, tcx);
                // Get the actual type of the discriminant from MIR local declarations
                let discr_ty = discr.ty(&mir.local_decls, tcx);

                // Convert the MIR targets into OOMIR (Constant, Label) pairs
                let oomir_targets: Vec<(oomir::Constant, String)> = targets
                    .iter()
                    .map(|(value, target_bb)| {
                        // Convert MIR value (u128) to appropriate OOMIR constant based on discr_ty
                        let oomir_const = mir_int_to_oomir_const(value, discr_ty, tcx); // Use helper
                        // Check if the constant type is suitable for a JVM switch
                        if !oomir_const.is_integer_like() {
                             println!("Warning: SwitchInt target value {:?} for type {:?} cannot be directly used in JVM switch. Block: {}", oomir_const, discr_ty, label);
                             // Decide on fallback: error, skip target, default value?
                             // For now, let's potentially create an invalid switch target for lower2 to handle/error on.
                        }
                        let target_label = format!("bb{}", target_bb.index());
                        (oomir_const, target_label)
                    })
                    .collect();

                // Get the label for the 'otherwise' block
                let otherwise_label = format!("bb{}", targets.otherwise().index());

                // Add the single OOMIR Switch instruction
                instructions.push(oomir::Instruction::Switch {
                    discr: discr_operand,
                    targets: oomir_targets,
                    otherwise: otherwise_label,
                });
                // This Switch instruction terminates the current OOMIR basic block.
            }
            TerminatorKind::Call {
                func,
                args,
                destination,
                target,
                ..
            } => {
                let function_name = format!("{:?}", func); // Get function name - needs refinement to extract actual name
                let oomir_args = args
                    .iter()
                    .map(|arg| convert_operand(&arg.node, tcx))
                    .collect();
                let dest = Some(format!("{:?}", destination.local));

                instructions.push(oomir::Instruction::Call {
                    dest,
                    function: function_name,
                    args: oomir_args,
                });

                if let Some(target_bb) = target {
                    let target_label = format!("bb{}", target_bb.index());
                    println!(
                        "Info: Adding Jump to {} after Call in bb{}",
                        target_label,
                        bb.index()
                    ); // Add log
                    instructions.push(oomir::Instruction::Jump {
                        target: target_label,
                    });
                } else {
                    // Function diverges (e.g., panic!) - No jump needed.
                    println!(
                        "Info: Call in bb{} has no return target (diverges).",
                        bb.index()
                    );
                }
            }
            TerminatorKind::Assert {
                target,
                cond,
                expected,
                msg,
                unwind: _,
            } => {
                // Convert the condition operand
                let condition = convert_operand(cond, tcx);

                // Generate a comparison instruction to check if the condition matches the expected value
                let comparison_dest = format!("assert_cmp_{}", bb.index());
                instructions.push(oomir::Instruction::Eq {
                    dest: comparison_dest.clone(),
                    op1: condition,
                    op2: oomir::Operand::Constant(oomir::Constant::Boolean(*expected)),
                });

                // Generate a branch based on the comparison result
                let true_block = format!("bb{}", target.index()); // Success path
                let false_block = format!("assert_fail_{}", bb.index()); // Failure path label
                instructions.push(oomir::Instruction::Branch {
                    condition: oomir::Operand::Variable(comparison_dest),
                    true_block, // Jump here if assertion holds (cond == expected)
                    false_block: false_block.clone(), // Jump here if assertion fails
                });

                // Add a new basic block for the assertion failure (panic)
                let fail_instructions = vec![oomir::Instruction::ThrowNewWithMessage {
                    // For now, hardcode RuntimeException. Could be configurable later.
                    exception_class: "java/lang/RuntimeException".to_string(),
                    // Extract the format string as the message.
                    // TODO: Handle msg.args() for formatted messages later.
                    message: format!("{:?}", msg),
                }];
                basic_blocks.insert(
                    // Ensure 'basic_blocks' map is mutable and passed in
                    false_block.clone(),
                    oomir::BasicBlock {
                        label: false_block,
                        instructions: fail_instructions,
                    },
                );
            }
            // Other terminator kinds (like Resume, etc.) can be added as needed.
            _ => {
                println!("Warning: Unhandled terminator {:?}", terminator.kind);
            }
        }
    }

    oomir::BasicBlock {
        label,
        instructions,
    }
}

/// Converts a MIR Body into an OOMIR Function.
/// This function extracts a function’s signature (currently minimal) and builds
/// a control flow graph of basic blocks.
pub fn mir_to_oomir<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &mut Body<'tcx>,
) -> oomir::Function {
    // Get a function name from the instance.
    let fn_name = tcx.item_name(instance.def_id()).to_string();

    // Extract function signature
    let mir_sig = tcx.type_of(instance.def_id()).skip_binder().fn_sig(tcx);
    let params_ty = mir_sig.inputs();
    let return_ty = mir_sig.output();

    let params_oomir_ty: Vec<oomir::Type> = params_ty
        .skip_binder()
        .iter()
        .map(|ty| ty_to_oomir_type(*ty, tcx))
        .collect();
    let return_oomir_ty: oomir::Type = ty_to_oomir_type(return_ty.skip_binder(), tcx);

    let mut signature = oomir::Signature {
        params: params_oomir_ty,
        ret: Box::new(return_oomir_ty.clone()), // Clone here to pass to convert_basic_block
    };

    // check if txc.entry_fn() matches the DefId of the function
    // note: libraries exist and don't have an entry function, handle that case
    if let Some(entry_fn) = tcx.entry_fn(()) {
        if entry_fn.0 == instance.def_id() {
            // see if the name is "main"
            if fn_name == "main" {
                // manually override the signature to match the JVM main method
                signature = oomir::Signature {
                    params: vec![oomir::Type::Array(Box::new(oomir::Type::Class(
                        "java/lang/String".to_string(),
                    )))],
                    ret: Box::new(oomir::Type::Void),
                };
            }
        }
    }

    // Build a CodeBlock from the MIR basic blocks.
    let mut basic_blocks = HashMap::new();
    // MIR guarantees that the start block is BasicBlock 0.
    let entry_label = "bb0".to_string();

    let mir_cloned = mir.clone();

    // Need read-only access to mir for local_decls inside the loop
    for (bb, bb_data) in mir.basic_blocks_mut().iter_enumerated() {
        let bb_ir = convert_basic_block(
            bb,
            bb_data,
            tcx,
            &mir_cloned,
            &return_oomir_ty,
            &mut basic_blocks,
        ); // Pass return type here
        basic_blocks.insert(bb_ir.label.clone(), bb_ir);
    }

    let codeblock = oomir::CodeBlock {
        basic_blocks,
        entry: entry_label,
    };

    // Return the OOMIR representation of the function.
    oomir::Function {
        name: fn_name,
        signature,
        body: codeblock,
    }
}

// A helper function to convert MIR integer values to OOMIR Constants, respecting type
fn mir_int_to_oomir_const<'tcx>(value: u128, ty: Ty<'tcx>, _tcx: TyCtxt<'tcx>) -> oomir::Constant {
    match ty.kind() {
        TyKind::Int(int_ty) => match int_ty {
            // Cast u128 carefully to avoid panic/wrap-around if value is out of range
            IntTy::I8 => oomir::Constant::I8(value as i8), // Be careful with large values
            IntTy::I16 => oomir::Constant::I16(value as i16), // Be careful
            IntTy::I32 => oomir::Constant::I32(value as i32), // Be careful
            IntTy::I64 => oomir::Constant::I64(value as i64),
            IntTy::Isize => oomir::Constant::I64(value as i64), // Assuming 64-bit target
            IntTy::I128 => oomir::Constant::I64(value as i64), // Truncate for JVM compatibility? Or error?
        },
        TyKind::Uint(uint_ty) => match uint_ty {
            // JVM uses signed types, treat appropriately
            UintTy::U8 => oomir::Constant::I8(value as i8), // Treat as signed byte for JVM switch
            UintTy::U16 => oomir::Constant::I16(value as i16), // Treat as signed short
            UintTy::U32 => oomir::Constant::I32(value as i32), // Treat as signed int
            UintTy::U64 => oomir::Constant::I64(value as i64), // Treat as signed long
            UintTy::Usize => oomir::Constant::I64(value as i64), // Assuming 64-bit
            UintTy::U128 => oomir::Constant::I64(value as i64), // Truncate? Error?
        },
        TyKind::Bool => oomir::Constant::Boolean(value != 0), // 0 is false, non-zero is true
        TyKind::Char => {
            // u128 -> u32 -> char
            oomir::Constant::Char(char::from_u32(value as u32).unwrap_or('\0')) // Handle potential invalid char
        }
        _ => {
            // This case should ideally not happen if MIR is well-typed
            println!(
                "Warning: Cannot convert MIR integer value {} to OOMIR constant for non-integer type {:?}",
                value, ty
            );
            oomir::Constant::I32(0) // Default fallback
        }
    }
}
