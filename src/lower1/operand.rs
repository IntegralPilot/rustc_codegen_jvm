use super::place::{get_place_type, place_to_string};
use crate::oomir;

use rustc_middle::{
    mir::{
        Body, Const, ConstOperand, ConstValue, Operand as MirOperand, Place,
        interpret::{AllocRange, ErrorHandled, Scalar},
    },
    ty::{FloatTy, IntTy, PseudoCanonicalInput, Ty, TyCtxt, TyKind, TypingEnv, UintTy},
};
use std::collections::HashMap;

/// Convert a MIR operand to an OOMIR operand.
pub fn convert_operand<'tcx>(
    mir_op: &MirOperand<'tcx>,
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
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
            let oomir_type = get_place_type(place, mir, tcx, data_types);
            // --- Create typed Variable operand ---
            oomir::Operand::Variable {
                name: var_name,
                ty: oomir_type,
            }
        }
    }
}

pub fn handle_const_value<'tcx>(
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
                                    } else if inner_ty.is_str() {
                                        // Handle the case where the inner type is a reference to a string slice
                                        println!(
                                            "Info: Scalar::Ptr points to Ref to str type {:?}. Mapping to OOMIR String.",
                                            inner_ty
                                        );
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
                                                oomir::Operand::Constant(oomir::Constant::String(
                                                    "Invalid UTF8".to_string(),
                                                ))
                                            }
                                        }
                                    } else if inner_ty.is_ref() {
                                        println!(
                                            "Info: Scalar::Ptr points to Ref-to-Ref type {:?}. Attempting to read inner fat pointer.",
                                            ty // The original type, like &&str
                                        );

                                        // Get the type of the *inner* reference (e.g., &str)
                                        let inner_ref_ty = *inner_ty; // inner_ty is the &str from &&str

                                        // *** Check if the *next* inner type is str ***
                                        // We expect inner_ref_ty to be a Ref itself.
                                        if let TyKind::Ref(_, next_inner_ty, _) =
                                            inner_ref_ty.kind()
                                        {
                                            if !next_inner_ty.is_str() {
                                                println!(
                                                    "Warning: Scalar::Ptr points to Ref-to-Ref, but the innermost type {:?} is not str. Type was {:?}.",
                                                    next_inner_ty, ty
                                                );
                                                return oomir::Operand::Constant(
                                                    oomir::Constant::I64(-11),
                                                ); // Indicate wrong inner type
                                            }
                                            // Proceed, we expect &&str layout
                                        } else {
                                            // This shouldn't happen if inner_ty.is_ref() was true, but safety check.
                                            println!(
                                                "Warning: Scalar::Ptr points to Ref-to-Ref, but inner type {:?} is not a Ref itself? Type was {:?}.",
                                                inner_ref_ty, ty
                                            );
                                            return oomir::Operand::Constant(oomir::Constant::I64(
                                                -12,
                                            ));
                                        }

                                        // *** Start copied/adapted logic from the array case ***

                                        // Get layout of the inner reference type (&str)
                                        let typing_env = TypingEnv::fully_monomorphized();
                                        match tcx.layout_of(PseudoCanonicalInput {
                                            typing_env: typing_env,
                                            value: inner_ref_ty, // Get layout of &str
                                        }) {
                                            Ok(inner_ref_layout) => {
                                                // Sanity check: layout should be a fat pointer
                                                let pointer_size = tcx.data_layout.pointer_size;
                                                let expected_size = pointer_size
                                                    .checked_mul(2, &tcx.data_layout)
                                                    .expect("Pointer size * 2 overflowed");

                                                if inner_ref_layout.is_unsized()
                                                    || inner_ref_layout.size != expected_size
                                                {
                                                    println!(
                                                        "Warning: Layout of inner ref type {:?} doesn't look like a fat pointer (&str). Size: {:?}, Expected: {:?}. Original type was {:?}.",
                                                        inner_ref_ty,
                                                        inner_ref_layout.size,
                                                        expected_size,
                                                        ty
                                                    );
                                                    return oomir::Operand::Constant(
                                                        oomir::Constant::I64(-8),
                                                    ); // Indicate layout error
                                                }

                                                // Read the fat pointer parts from the *current* allocation
                                                // Read the data pointer part
                                                let data_ptr_range = AllocRange {
                                                    start: rustc_abi::Size::ZERO, // Start at the beginning
                                                    size: pointer_size,
                                                };
                                                let data_ptr_scalar_res = allocation.read_scalar(
                                                    &tcx.data_layout,
                                                    data_ptr_range,
                                                    true, // Read provenance
                                                );

                                                // Read the length part
                                                let len_range = AllocRange {
                                                    start: pointer_size, // Offset by pointer size
                                                    size: pointer_size,
                                                };
                                                let len_scalar_res = allocation.read_scalar(
                                                    &tcx.data_layout,
                                                    len_range,
                                                    false, // No provenance for len
                                                );

                                                match (data_ptr_scalar_res, len_scalar_res) {
                                                    (
                                                        Ok(Scalar::Ptr(data_ptr, _)),
                                                        Ok(Scalar::Int(len_scalar)),
                                                    ) => {
                                                        let len = len_scalar.to_target_usize(tcx); // Use usize directly

                                                        // Get AllocId and offset from the *inner* data pointer
                                                        let final_alloc_id =
                                                            data_ptr.provenance.alloc_id();
                                                        let (_, final_offset) =
                                                            data_ptr.into_parts(); // offset within final_alloc

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
                                                                            println!("Info: Successfully extracted string const from &&str indirection: \"{}\"", s);
                                                                            oomir::Operand::Constant(oomir::Constant::String(s))
                                                                        }
                                                                        Err(e) => {
                                                                            println!("Warning: Final string bytes from allocation {:?} (via &&str) were not valid UTF-8: {}", final_alloc_id, e);
                                                                            oomir::Operand::Constant(oomir::Constant::String("Invalid UTF8 (Inner)".to_string()))
                                                                        }
                                                                    }
                                                                } else {
                                                                    println!("Warning: Calculated string slice range {}..{} (via &&str) out of bounds for allocation {:?} size {}", start_byte, end_byte, final_alloc_id, final_alloc.size().bytes());
                                                                    oomir::Operand::Constant(oomir::Constant::I64(-9)) // Indicate bounds error
                                                                }
                                                            } else {
                                                                println!("Warning: String slice length calculation overflowed (via &&str)");
                                                                 oomir::Operand::Constant(oomir::Constant::I64(-10)) // Indicate overflow
                                                            }
                                                        }
                                                        _ => {
                                                            println!("Warning: Inner string pointer {:?} (via &&str) points to unexpected GlobalAlloc kind {:?}", data_ptr, tcx.global_alloc(final_alloc_id));
                                                            oomir::Operand::Constant(oomir::Constant::I64(-11)) // Indicate wrong alloc kind
                                                        }
                                                    }
                                                    }
                                                    // Error handling for reading scalars
                                                    (Err(e), _) => {
                                                        println!(
                                                            "Error reading inner string data pointer scalar (via &&str) from allocation {:?}: {:?}",
                                                            alloc_id, e
                                                        );
                                                        oomir::Operand::Constant(
                                                            oomir::Constant::I64(-12),
                                                        )
                                                    }
                                                    (_, Err(e)) => {
                                                        println!(
                                                            "Error reading inner string length scalar (via &&str) from allocation {:?}: {:?}",
                                                            alloc_id, e
                                                        );
                                                        oomir::Operand::Constant(
                                                            oomir::Constant::I64(-13),
                                                        )
                                                    }
                                                    (Ok(data), Ok(len)) => {
                                                        println!(
                                                            "Warning: Read unexpected scalar types for fat pointer (via &&str). Data: {:?}, Len: {:?}",
                                                            data, len
                                                        );
                                                        oomir::Operand::Constant(
                                                            oomir::Constant::I64(-14),
                                                        )
                                                    }
                                                }
                                            }
                                            Err(e) => {
                                                println!(
                                                    "Error getting layout for inner ref type {:?} (from original {:?}): {:?}",
                                                    inner_ref_ty, ty, e
                                                );
                                                oomir::Operand::Constant(oomir::Constant::I64(-15)) // Indicate layout error
                                            }
                                        }
                                        // *** End copied/adapted logic ***
                                    } else {
                                        // Inner type of the Ref is not an Array or str
                                        println!(
                                            "Warning: Scalar::Ptr points to Ref to non-Array and non-str type {:?}. Not a recognized pattern.",
                                            inner_ty
                                        );
                                        // Fall through to default handling or return specific error
                                        oomir::Operand::Constant(oomir::Constant::I64(-7)) // Indicate not ref-to-array or str
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

pub fn get_placeholder_operand<'tcx>(
    dest_place: &Place<'tcx>,
    mir: &Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::Operand {
    let dest_oomir_type = get_place_type(dest_place, mir, tcx, data_types);
    if dest_oomir_type.is_jvm_reference_type() {
        // Destination needs a reference, use Null placeholder
        println!(
            "Info: Generating Object placeholder for unhandled assignment to reference type var '{}' ({:?})",
            place_to_string(dest_place, tcx), // Recalculate name if needed, or use dest_name
            dest_oomir_type
        );
        oomir::Operand::Constant(oomir::Constant::Class(
            "java/lang/Object".to_string(), // Use Object as a placeholder
        ))
    } else {
        // Destination is likely a primitive, use I32(0) as placeholder
        println!(
            "Info: Generating I32(0) placeholder for unhandled assignment to primitive type var '{}' ({:?})",
            place_to_string(dest_place, tcx),
            dest_oomir_type
        );
        oomir::Operand::Constant(oomir::Constant::I32(0))
    }
}
