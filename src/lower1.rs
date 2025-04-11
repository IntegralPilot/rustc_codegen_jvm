//! This is the stage 1 lowering pass of the compiler.
//! It is responsible for coverting the MIR into a lower-level IR, called OOMIR (see src/oomir.rs).
//! It is a simple pass that converts the MIR into a more object-oriented representation.

// lower1.rs
//! This module converts Rust MIR into an object-oriented MIR (OOMIR)
//! that sits between MIR and JVM bytecode. It supports a subset of Rust constructs
//! (arithmetic, branching, returns) and can be extended to support more of Rust.

use crate::oomir;
use regex::Regex;
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
use std::sync::OnceLock;

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
            IntTy::I128 => oomir::Type::Class("java/lang/BigInteger".to_string()), // doesn't fit in a primitive
        },
        rustc_middle::ty::TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => oomir::Type::I16, // make it the next size up to capture full range
            UintTy::U16 => oomir::Type::I32,
            UintTy::U32 => oomir::Type::I64,
            UintTy::U64 => oomir::Type::Class("java/lang/BigInteger".to_string()),
            UintTy::Usize => oomir::Type::Class("java/lang/BigInteger".to_string()),
            UintTy::U128 => oomir::Type::Class("java/lang/BigInteger".to_string()),
        },
        rustc_middle::ty::TyKind::Float(float_ty) => match float_ty {
            FloatTy::F32 => oomir::Type::F32,
            FloatTy::F64 => oomir::Type::F64,
            FloatTy::F16 => oomir::Type::F32,
            FloatTy::F128 => oomir::Type::F64,
        },
        rustc_middle::ty::TyKind::Adt(adt_def, _substs) => {
            // Get the full path string for the ADT
            let full_path_str = tcx.def_path_str(adt_def.did());

            // --- Check for specific types we need to map ---
            if full_path_str == "core::fmt::rt::Argument" {
                println!("Info: Mapping core::fmt::rt::Argument to java.lang.Object");
                oomir::Type::Class("java/lang/Object".to_string())
            }
            // --- Check for Arguments struct itself ---
            else if full_path_str == "core::fmt::Arguments" {
                // Arguments are often handled opaquely or via specific shims, mapping to Object might be safe
                println!("Info: Mapping core::fmt::Arguments to java.lang.Object");
                oomir::Type::Class("java/lang/Object".to_string())
            }
            // TODO: Add more mappings here for other core types if needed later
            // else if full_path_str == "some::other::Struct" {
            //     oomir::Type::Class("your/jvm/package/SomeOtherStruct".to_string())
            // }
            else {
                // --- Fallback: Generate a *valid* but perhaps non-functional JVM name ---
                // Replace :: with / and remove generics <...> for a basic valid name
                println!(
                    "Warning: Generating placeholder JVM name for ADT: {}",
                    full_path_str
                );
                // Use make_jvm_safe logic first, then replace . with / (make_jvm_safe might handle some ::)
                let safe_name = make_jvm_safe(&full_path_str);
                let jvm_name = safe_name.replace("::", "/").replace('.', "/"); // Ensure all separators are /
                oomir::Type::Class(jvm_name)
            }
        }
        rustc_middle::ty::TyKind::Str => {
            println!("Info: Mapping Rust str to java.lang.String");
            oomir::Type::Class("java/lang/String".to_string())
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
fn convert_operand<'tcx>(
    mir_op: &MirOperand<'tcx>,
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
) -> oomir::Operand {
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
            // --- Get the type of the place ---
            let oomir_type = get_place_type(place, mir, tcx);
            // --- Create typed Variable operand ---
            oomir::Operand::Variable {
                name: var_name,
                ty: oomir_type,
            }
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
                                IntTy::I8 => oomir::Constant::I8(scalar_int.to_i8()),
                                IntTy::I16 => oomir::Constant::I16(scalar_int.to_i16()),
                                IntTy::I32 => oomir::Constant::I32(scalar_int.to_i32()),
                                IntTy::I64 => oomir::Constant::I64(scalar_int.to_i64()),
                                _ => oomir::Constant::I32(scalar_int.to_i32()), // Default fallback
                            };
                            oomir::Operand::Constant(val)
                        }
                        rustc_middle::ty::TyKind::Uint(uint_ty) => {
                            // The JVM only has signed integers, so we need to convert
                            // Ensure that the FULL RANGE is covered, i.e., u8 must become i16
                            let val = match uint_ty {
                                UintTy::U8 => oomir::Constant::I16(scalar_int.to_u8() as i16),
                                UintTy::U16 => oomir::Constant::I32(scalar_int.to_u16() as i32),
                                UintTy::U32 => oomir::Constant::I64(scalar_int.to_u32() as i64),
                                UintTy::U64 => {
                                    oomir::Constant::Class("java/lang/BigInteger".to_string())
                                } // Doesn't fit in a primitive
                                _ => oomir::Constant::I32(scalar_int.to_u32() as i32), // Default fallback
                            };
                            oomir::Operand::Constant(val)
                        }
                        rustc_middle::ty::TyKind::Bool => {
                            let val = scalar_int.try_to_bool().unwrap_or(false);
                            oomir::Operand::Constant(oomir::Constant::Boolean(val))
                        }
                        rustc_middle::ty::TyKind::Char => {
                            let val = char::from_u32(scalar_int.to_u32()).unwrap_or('\0');
                            oomir::Operand::Constant(oomir::Constant::Char(val))
                        }
                        rustc_middle::ty::TyKind::Float(float_ty) => {
                            let val = match float_ty {
                                FloatTy::F32 => {
                                    let bits = scalar_int.to_u32(); // Get the raw bits as u32
                                    oomir::Constant::F32(f32::from_bits(bits))
                                }
                                FloatTy::F64 => {
                                    let bits = scalar_int.to_u64(); // Get the raw bits as u64
                                    oomir::Constant::F64(f64::from_bits(bits))
                                }
                                _ => oomir::Constant::I32(scalar_int.to_i32()), // Default fallback
                            };
                            oomir::Operand::Constant(val)
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
                                        // Try to get the length of the array constant
                                        if let Some(array_len) =
                                            array_len_const.try_to_target_usize(tcx)
                                        {
                                            // Check if the element type is &str
                                            if let TyKind::Ref(_, str_ty, _) = elem_ty.kind() {
                                                if str_ty.is_str() {
                                                    println!(
                                                        "Info: Handling Ref-to-[&str; {}]. Reading inner fat pointers from allocation {:?}.",
                                                        array_len, alloc_id
                                                    );

                                                    // Get layout of the element type (&str, which is a fat pointer)
                                                    let typing_env =
                                                        TypingEnv::fully_monomorphized(); // Use appropriate Env
                                                    match tcx.layout_of(PseudoCanonicalInput {
                                                        typing_env: typing_env,
                                                        value: *elem_ty,
                                                    }) {
                                                        Ok(elem_layout) => {
                                                            // Sanity check: element layout should be a fat pointer
                                                            let pointer_size =
                                                                tcx.data_layout.pointer_size;
                                                            let expected_elem_size = pointer_size
                                                                .checked_mul(2, &tcx.data_layout)
                                                                .expect(
                                                                    "Pointer size * 2 overflowed",
                                                                );

                                                            if elem_layout.is_unsized()
                                                                || elem_layout.size
                                                                    != expected_elem_size
                                                            {
                                                                println!(
                                                                    "Warning: Layout of element type {:?} doesn't look like a fat pointer (&str). Size: {:?}, Expected: {:?}",
                                                                    elem_ty,
                                                                    elem_layout.size,
                                                                    expected_elem_size
                                                                );
                                                                return oomir::Operand::Constant(
                                                                    oomir::Constant::I64(-1),
                                                                ); // Indicate layout error
                                                            }

                                                            // Vector to hold the resulting string constants
                                                            let mut string_constants =
                                                                Vec::with_capacity(
                                                                    array_len as usize,
                                                                );
                                                            let mut encountered_error = false; // Flag errors during loop

                                                            for i in 0..array_len {
                                                                // Calculate the offset of the current element within the array allocation
                                                                let current_elem_offset = expected_elem_size.checked_mul(i as u64, &tcx.data_layout)
                                                                    .expect("Array element offset calculation overflowed");

                                                                // Read the data pointer part from the current element's offset
                                                                let data_ptr_range = AllocRange {
                                                                    start: current_elem_offset,
                                                                    size: pointer_size,
                                                                };
                                                                let data_ptr_scalar_res =
                                                                    allocation.read_scalar(
                                                                        &tcx.data_layout,
                                                                        data_ptr_range,
                                                                        true, // Read provenance
                                                                    );

                                                                // Read the length part from the current element's offset
                                                                let len_range = AllocRange {
                                                                    start: current_elem_offset
                                                                        + pointer_size,
                                                                    size: pointer_size,
                                                                };
                                                                let len_scalar_res = allocation
                                                                    .read_scalar(
                                                                        &tcx.data_layout,
                                                                        len_range,
                                                                        false, // No provenance for len
                                                                    );

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
                                                                                // Use checked_add for length calculation
                                                                                if let Some(end_byte) = start_byte.checked_add(len as usize) {
                                                                                    // Bounds check
                                                                                    if end_byte <= final_alloc.size().bytes_usize() {
                                                                                        let final_range = start_byte..end_byte;
                                                                                        // Read the final bytes
                                                                                        let final_bytes = final_alloc.inspect_with_uninit_and_ptr_outside_interpreter(final_range);

                                                                                        // Convert to string
                                                                                        match String::from_utf8(final_bytes.to_vec()) {
                                                                                            Ok(s) => {
                                                                                                println!("Info: Successfully extracted string const at index {}: \"{}\"", i, s);
                                                                                                string_constants.push(oomir::Constant::String(s));
                                                                                            }
                                                                                            Err(e) => {
                                                                                                println!("Warning: Final string bytes (idx {}) from allocation {:?} were not valid UTF-8: {}", i, final_alloc_id, e);
                                                                                                string_constants.push(oomir::Constant::String("Invalid UTF8 (Inner)".to_string()));
                                                                                            }
                                                                                        }
                                                                                    } else {
                                                                                         println!("Warning: Calculated string slice range {}..{} (idx {}) out of bounds for allocation {:?} size {}", start_byte, end_byte, i, final_alloc_id, final_alloc.size().bytes());
                                                                                         encountered_error = true;
                                                                                         break; // Stop processing this array
                                                                                    }
                                                                                } else {
                                                                                    println!("Warning: String slice length calculation overflowed (idx {})", i);
                                                                                    encountered_error = true;
                                                                                    break; // Stop processing this array
                                                                                }
                                                                            }
                                                                            _ => {
                                                                                println!("Warning: Inner string pointer (idx {}) {:?} points to unexpected GlobalAlloc kind {:?}", i, data_ptr, tcx.global_alloc(final_alloc_id));
                                                                                encountered_error = true;
                                                                                break; // Stop processing this array
                                                                            }
                                                                        }
                                                                    }
                                                                    // Error handling for reading scalars for *this element*
                                                                    (Err(e), _) => {
                                                                        println!(
                                                                            "Error reading inner string data pointer scalar (idx {}) from allocation {:?}: {:?}",
                                                                            i, alloc_id, e
                                                                        );
                                                                        encountered_error = true;
                                                                        break; // Stop processing this array
                                                                    }
                                                                    (_, Err(e)) => {
                                                                        println!(
                                                                            "Error reading inner string length scalar (idx {}) from allocation {:?}: {:?}",
                                                                            i, alloc_id, e
                                                                        );
                                                                        encountered_error = true;
                                                                        break; // Stop processing this array
                                                                    }
                                                                    (Ok(data), Ok(len)) => {
                                                                        println!(
                                                                            "Warning: Read unexpected scalar types for fat pointer (idx {}). Data: {:?}, Len: {:?}",
                                                                            i, data, len
                                                                        );
                                                                        encountered_error = true;
                                                                        break; // Stop processing this array
                                                                    }
                                                                }
                                                            } // End loop over array elements

                                                            // Check if errors occurred during the loop
                                                            if encountered_error {
                                                                println!(
                                                                    "Warning: Encountered errors while processing elements of array constant in allocation {:?}. Returning error placeholder.",
                                                                    alloc_id
                                                                );
                                                                return oomir::Operand::Constant(
                                                                    oomir::Constant::I64(-2),
                                                                ); // Indicate partial/failed array read
                                                            } else {
                                                                // Success: return the array of string constants
                                                                println!(
                                                                    "Info: Successfully extracted array of {} string constants from allocation {:?}.",
                                                                    array_len, alloc_id
                                                                );
                                                                return oomir::Operand::Constant(
                                                                    oomir::Constant::Array(
                                                                        Box::new(
                                                                            oomir::Type::String,
                                                                        ),
                                                                        string_constants,
                                                                    ),
                                                                );
                                                            }
                                                        }
                                                        Err(e) => {
                                                            println!(
                                                                "Error getting layout for element type {:?}: {:?}",
                                                                elem_ty, e
                                                            );
                                                            oomir::Operand::Constant(
                                                                oomir::Constant::I64(-3),
                                                            ) // Indicate layout error
                                                        }
                                                    }
                                                } else {
                                                    /* Array element type is Ref, but not to str */
                                                    println!(
                                                        "Warning: Scalar::Ptr points to Ref-to-Array where element type {:?} is Ref but not &str.",
                                                        elem_ty
                                                    );
                                                    return oomir::Operand::Constant(
                                                        oomir::Constant::I64(-4),
                                                    ); // Indicate wrong element type
                                                }
                                            } else {
                                                /* Array element type is not Ref */
                                                println!(
                                                    "Warning: Scalar::Ptr points to Ref-to-Array where element type {:?} is not a reference.",
                                                    elem_ty
                                                );
                                                return oomir::Operand::Constant(
                                                    oomir::Constant::I64(-5),
                                                ); // Indicate wrong element type
                                            }
                                        } else {
                                            // Could not determine array length (e.g., generic)
                                            println!(
                                                "Warning: Scalar::Ptr points to Ref-to-Array but could not determine constant length: {:?}",
                                                array_len_const
                                            );
                                            return oomir::Operand::Constant(oomir::Constant::I64(
                                                -6,
                                            )); // Indicate unknown length
                                        }
                                    } else {
                                        // Inner type of the Ref is not an Array
                                        println!(
                                            "Warning: Scalar::Ptr points to Ref to non-Array type {:?}. Not a recognized string array pattern.",
                                            inner_ty
                                        );
                                        // Fall through to default handling or return specific error
                                        oomir::Operand::Constant(oomir::Constant::I64(-7)) // Indicate not ref-to-array
                                    }
                                }
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
            let is_str_slice_or_ref = match ty.kind() {
                TyKind::Str | TyKind::Slice(_) => true, // Direct str or slice type
                TyKind::Ref(_, inner_ty, _) => inner_ty.is_str() || inner_ty.is_slice(), // Reference to str or slice
                _ => false, // Not a str/slice or reference to one
            };
            if is_str_slice_or_ref {
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
            let dest = place_to_string(place, tcx);
            let get_placeholder_operand = |dest_place: &Place<'tcx>| -> oomir::Operand {
                let dest_oomir_type = get_place_type(dest_place, mir, tcx);
                if dest_oomir_type.is_jvm_reference_type() {
                    // Destination needs a reference, use Null placeholder
                    println!(
                        "Info: Generating Null placeholder for unhandled assignment to reference type var '{}' ({:?})",
                        place_to_string(dest_place, tcx), // Recalculate name if needed, or use dest_name
                        dest_oomir_type
                    );
                    oomir::Operand::Constant(oomir::Constant::Null)
                } else {
                    // Destination is likely a primitive, use I32(0) as placeholder
                    println!(
                        "Info: Generating I32(0) placeholder for unhandled assignment to primitive type var '{}' ({:?})",
                        place_to_string(dest_place, tcx),
                        dest_oomir_type
                    );
                    oomir::Operand::Constant(oomir::Constant::I32(0))
                }
            };
            match rvalue {
                Rvalue::BinaryOp(bin_op, box (op1, op2)) => {
                    let oomir_op1 = convert_operand(op1, tcx, mir);
                    let oomir_op2 = convert_operand(op2, tcx, mir);

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
                        BinOp::AddWithOverflow | BinOp::SubWithOverflow => {
                            use oomir::Operand as OO;
                            use oomir::Type as OomType;

                            let dest_name = dest;

                            // Need special handling for tuple assignment
                            let tuple_dest_name = format!("{}_tuple", dest_name); // Intermediate name

                            match bin_op {
                                BinOp::AddWithOverflow => {
                                    instructions.push(oomir::Instruction::AddWithOverflow {
                                        dest: tuple_dest_name.clone(), // Store in intermediate
                                        op1: oomir_op1.clone(),
                                        op2: oomir_op2,
                                    })
                                }
                                BinOp::SubWithOverflow => {
                                    instructions.push(oomir::Instruction::SubWithOverflow {
                                        dest: tuple_dest_name.clone(), // Store in intermediate
                                        op1: oomir_op1.clone(),
                                        op2: oomir_op2,
                                    })
                                }
                                _ => unreachable!(), // Already covered
                            }

                            // Get types for the tuple fields
                            let op1_type = match &oomir_op1 {
                                OO::Constant(c) => OomType::from_constant(c),
                                OO::Variable { ty, .. } => ty.clone(),
                            };

                            let bool_type = OomType::Boolean;

                            // Generate moves to split the conceptual tuple
                            instructions.push(oomir::Instruction::Move {
                                dest: format!("{}.0", dest_name), // Field name
                                src: oomir::Operand::Variable {
                                    name: format!("{}_result", tuple_dest_name),
                                    ty: op1_type,
                                }, // Use typed intermediate var
                            });
                            instructions.push(oomir::Instruction::Move {
                                dest: format!("{}.1", dest_name), // Field name
                                src: oomir::Operand::Variable {
                                    name: format!("{}_overflow", tuple_dest_name),
                                    ty: bool_type,
                                }, // Use typed intermediate var
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
                    let dest_name = dest;
                    let src_oomir_operand = convert_operand(mir_operand, tcx, mir);
                    // The destination's type will be the same as the source's type
                    instructions.push(oomir::Instruction::Move {
                        dest: dest_name, // Just the name
                        src: src_oomir_operand,
                    });
                }
                Rvalue::Aggregate(box kind, operands) => {
                    let dest_name = place_to_string(place, tcx);

                    match kind {
                        rustc_middle::mir::AggregateKind::Tuple => {
                            if operands.len() > 2 {
                                println!(
                                    "Warning: Tuple aggregate with >2 fields not fully handled: {}",
                                    dest_name
                                );
                            }
                            for (i, mir_op) in operands.iter().enumerate() {
                                let field_dest = format!("{}.{}", dest_name, i);
                                let field_src = convert_operand(mir_op, tcx, mir);
                                instructions.push(oomir::Instruction::Move {
                                    dest: field_dest,
                                    src: field_src,
                                });
                            }
                        }

                        rustc_middle::mir::AggregateKind::Array(mir_element_ty) => {
                            println!(
                                "Info: Handling Rvalue::Aggregate Array for dest '{}'",
                                dest_name
                            );

                            // 1. Get the OOMIR element type
                            let oomir_element_type = ty_to_oomir_type(*mir_element_ty, tcx);

                            // 2. Get the array size
                            let array_size = operands.len();
                            let size_operand =
                                oomir::Operand::Constant(oomir::Constant::I32(array_size as i32));

                            // 3. Generate the NewArray instruction
                            //    The dest_name ('_6') will hold the reference to the new array.
                            instructions.push(oomir::Instruction::NewArray {
                                dest: dest_name.clone(), // Assign the new array ref to _6
                                element_type: oomir_element_type.clone(),
                                size: size_operand,
                            });
                            println!(
                                "   -> Emitted NewArray: dest={}, type={:?}, size={}",
                                dest_name, oomir_element_type, array_size
                            );

                            // 4. Generate ArrayStore instructions for each element
                            for (i, mir_operand) in operands.iter().enumerate() {
                                let value_operand = convert_operand(mir_operand, tcx, mir);
                                let index_operand =
                                    oomir::Operand::Constant(oomir::Constant::I32(i as i32));

                                instructions.push(oomir::Instruction::ArrayStore {
                                    array_var: dest_name.clone(), // Store into the array ref in _6
                                    index: index_operand,
                                    value: value_operand.clone(), // Clone operand if needed elsewhere
                                });
                                println!(
                                    "   -> Emitted ArrayStore: array={}, index={}, value={:?}",
                                    dest_name, i, value_operand
                                );
                            }
                        }
                        _ => {
                            println!(
                                "Warning: Unhandled aggregate kind {:?} for dest {}",
                                kind, dest_name
                            );
                            // Assign a type-aware placeholder
                            instructions.push(oomir::Instruction::Move {
                                dest: dest_name,                     // Use the extracted dest_name
                                src: get_placeholder_operand(place), // Use helper
                            });
                        }
                    }
                }
                Rvalue::Ref(_region, _borrow_kind, source_place) => {
                    let dest_name = dest;
                    let source_name = place_to_string(source_place, tcx); // e.g., "_1"

                    // Get the type of the place BEING REFERENCED (the inner type)
                    let source_type = get_place_type(source_place, mir, tcx);
                    // Get the type of the DESTINATION (the reference itself)

                    // Create the source operand (Variable)
                    let source_operand = oomir::Operand::Variable {
                        name: source_name,
                        ty: source_type.clone(), // The variable being referenced
                    };

                    // Generate a Move instruction.
                    // In OOMIR/JVM, passing a reference often means passing the object/value itself.
                    // The type information associated with the destination variable `dest_name`
                    // in lower2 will be `Reference(source_type)`, ensuring Aload/Astore are used if needed.
                    instructions.push(oomir::Instruction::Move {
                        dest: dest_name,
                        src: source_operand,
                    });
                    println!(
                        "Info: Handled Rvalue::Ref: {} = &{} (via Move with types src={:?})",
                        place_to_string(place, tcx),
                        place_to_string(source_place, tcx),
                        source_type
                    );
                }
                _ => {
                    println!("Warning: Unhandled rvalue {:?}", rvalue);
                    // Assign a type-aware placeholder
                    instructions.push(oomir::Instruction::Move {
                        dest,                                // Use the calculated name
                        src: get_placeholder_operand(place), // Use helper
                    });
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
                        convert_operand(&MirOperand::Move(Place::return_place()), tcx, mir);
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
                let discr_operand = convert_operand(discr, tcx, mir);
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
                let function_name = make_jvm_safe(format!("{:?}", func).as_str()); // Get function name - needs refinement to extract actual name
                let oomir_args = args
                    .iter()
                    .map(|arg| convert_operand(&arg.node, tcx, mir))
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
                let condition = convert_operand(cond, tcx, mir);

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
                    condition: oomir::Operand::Variable {
                        name: comparison_dest,
                        ty: oomir::Type::Boolean,
                    },
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

// Make a rust idenfier (i.e. function name) JVM safe
// i.e. Arguments::<'_>::new_const::<1> -> arguments_new_const
pub fn make_jvm_safe(input: &str) -> String {
    static RE_ANGLES: OnceLock<Regex> = OnceLock::new();
    static RE_COLONS: OnceLock<Regex> = OnceLock::new();
    static RE_TRAILING_UNDERSCORE: OnceLock<Regex> = OnceLock::new();
    // 1. Remove anything inside < > (including the angle brackets themselves),
    // but keep the inner thing if it is a Rust primitive type.
    let re_angles = RE_ANGLES.get_or_init(|| Regex::new(r"<([^>]+)>").expect("Invalid regex"));
    let without_angles = re_angles.replace_all(input, |caps: &regex::Captures| {
        let inner = &caps[1];
        match inner {
            "i8" | "i16" | "i32" | "i64" | "i128" | "u8" | "u16" | "u32" | "u64" | "u128"
            | "f32" | "f64" | "bool" | "char" | "str" => inner.to_string(),
            _ => "".to_string(),
        }
    });

    // 2. Replace occurrences of :: (or more consecutive ':') with a single underscore
    let re_colons = RE_COLONS.get_or_init(|| Regex::new(r":{2,}").expect("Invalid regex"));
    let replaced = re_colons.replace_all(&without_angles, "_");

    // 3. Ensure there is not a trailing underscore
    let re_trailing_underscore =
        RE_TRAILING_UNDERSCORE.get_or_init(|| Regex::new(r"_$").expect("Invalid regex"));
    let replaced = re_trailing_underscore.replace_all(&replaced, "");

    // 4. Convert all to lowercase
    replaced.to_lowercase()
}

// --- Helper to get OOMIR Type for a Place ---
fn get_place_type<'tcx>(place: &Place<'tcx>, mir: &Body<'tcx>, tcx: TyCtxt<'tcx>) -> oomir::Type {
    let place_ty = place.ty(&mir.local_decls, tcx);
    ty_to_oomir_type(place_ty.ty, tcx)
}
