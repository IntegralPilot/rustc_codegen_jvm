use super::place::{get_place_type, place_to_string};
use crate::oomir;

use super::place::emit_instructions_to_get_on_own;
use rustc_abi::Size;
use rustc_middle::{
    mir::{
        Body, Const, ConstOperand, ConstValue, Operand as MirOperand, Place,
        interpret::{AllocRange, ErrorHandled, GlobalAlloc, Provenance, Scalar},
    },
    ty::{
        ConstKind, FloatTy, IntTy, PseudoCanonicalInput, ScalarInt, Ty, TyCtxt, TyKind, TypingEnv,
        UintTy,
    },
};
use std::collections::HashMap;

mod experimental;
mod float;

use experimental::read_constant_value_from_memory;
use float::f128_to_string;

/// Convert a MIR operand to an OOMIR operand.
pub fn convert_operand<'tcx>(
    mir_op: &MirOperand<'tcx>,
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
    instructions: &mut Vec<oomir::Instruction>,
) -> oomir::Operand {
    match mir_op {
        MirOperand::Constant(box constant) => {
            match constant.const_ {
                Const::Val(const_val, ty) => {
                    handle_const_value(Some(constant), const_val, &ty, tcx, data_types)
                }
                Const::Ty(const_ty, ty_const) => {
                    // ty_const is NOT a type, naming is b/c it's a ty::Const (as opposed to mir::Const)
                    let kind = ty_const.kind();
                    match kind {
                        ConstKind::Value(val) => {
                            let constval = tcx.valtree_to_const_val(val);
                            handle_const_value(None, constval, &const_ty, tcx, data_types)
                        }
                        _ => {
                            println!(
                                "Warning: unhandled constant kind for a Ty const: {:?}",
                                kind
                            );
                            oomir::Operand::Constant(oomir::Constant::I32(-1))
                        }
                    }
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
                            handle_const_value(Some(constant), const_val, &ty, tcx, data_types)
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
                }
            }
        }
        MirOperand::Copy(place) | MirOperand::Move(place) => {
            if place.projection.is_empty() {
                let var_name = place_to_string(place, tcx);
                let oomir_type = get_place_type(place, mir, tcx, data_types);
                oomir::Operand::Variable {
                    name: var_name,
                    ty: oomir_type,
                }
            } else {
                let (final_var_name, get_instructions, final_type) =
                    emit_instructions_to_get_on_own(place, tcx, mir, data_types);

                instructions.extend(get_instructions);

                oomir::Operand::Variable {
                    name: final_var_name,
                    ty: final_type,
                }
            }
        }
    }
}

pub fn handle_const_value<'tcx>(
    constant: Option<&ConstOperand>,
    const_val: ConstValue<'tcx>,
    ty: &Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::Operand {
    match const_val {
        ConstValue::Scalar(scalar) => {
            match scalar {
                Scalar::Int(scalar_int) => {
                    let oomir_const = scalar_int_to_oomir_constant(scalar_int, ty);
                    oomir::Operand::Constant(oomir_const)
                }
                Scalar::Ptr(pointer, _) => {
                    let alloc_id = pointer.provenance.alloc_id();
                    let alloc = tcx.global_alloc(alloc_id);
                    match alloc {
                        rustc_middle::mir::interpret::GlobalAlloc::Memory(const_allocation) => {
                            let allocation = const_allocation.inner();

                            // eventually the read_constant_value_from_memory function (which is much better than the hardcoded/messy code here) will be used
                            // for every pointer type (it supports all of them). but currently it's just used for ADTs and scalar pointers for now, slowly it will
                            // be phased in for more types as it's support is expanded (currently it doesn't have full support, yet).
                            //
                            /*let offset = pointer.into_parts().1;

                            let constant_oomir = read_constant_value_from_memory(
                                tcx,
                                allocation,
                                offset,
                                *ty,
                                data_types
                            );

                            match constant_oomir {
                                Ok(oomir_constant) => {
                                    println!(
                                        "Info: Successfully read constant value from memory: {:?}",
                                        oomir_constant
                                    );
                                    return oomir::Operand::Constant(oomir_constant)
                                }
                                Err(e) => {
                                    println!(
                                        "Warning: Failed to read constant value from memory for allocation {:?}. Error: {:?}",
                                        alloc_id, e
                                    );
                                    return oomir::Operand::Constant(oomir::Constant::I32(-1))
                                }
                            }*/

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

                                                                        // Get AllocId and offset from the inner data pointer
                                                                        let final_alloc_id =
                                                                            data_ptr
                                                                                .provenance
                                                                                .alloc_id();
                                                                        let (_, final_offset) =
                                                                            data_ptr.into_parts();

                                                                        // Get the final allocation containing the string bytes
                                                                        match tcx.global_alloc(final_alloc_id.get_alloc_id().unwrap()) {
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
                                                                    // Error handling for reading scalars for this element
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
                                            "Info: Scalar::Ptr points to Ref-to-Ref type {:?}. Reading inner pointer.",
                                            ty // The original type, like &&u8 or &&str
                                        );

                                        // Get the type of the inner reference (e.g., &u8 or &str)
                                        let inner_ref_ty = *inner_ty;

                                        // Get the type of the innermost value (e.g., u8 or str)
                                        let next_inner_ty = match inner_ref_ty.kind() {
                                            TyKind::Ref(_, ty, _) => *ty,
                                            _ => {
                                                // This case should technically not be reachable if inner_ty.is_ref() was true
                                                println!(
                                                    "Warning: Inner type {:?} of Ref-to-Ref was not itself a Ref? Original type: {:?}",
                                                    inner_ref_ty, ty
                                                );
                                                return oomir::Operand::Constant(
                                                    oomir::Constant::I64(-20),
                                                ); // Indicate unexpected type structure
                                            }
                                        };

                                        // Read the inner pointer scalar from the current allocation (`allocation`)
                                        let pointer_size = tcx.data_layout.pointer_size;
                                        let inner_ptr_range = AllocRange {
                                            start: rustc_abi::Size::ZERO,
                                            size: pointer_size,
                                        };
                                        let inner_ptr_scalar_res = allocation.read_scalar(
                                            &tcx.data_layout,
                                            inner_ptr_range,
                                            true, // Read provenance, we need the AllocId
                                        );

                                        match inner_ptr_scalar_res {
                                            Ok(Scalar::Ptr(inner_ptr, _)) => {
                                                // Successfully read the inner pointer. Now find where it points.
                                                let (final_alloc_id, final_offset) =
                                                    inner_ptr.into_parts(); // Get AllocId and offset

                                                // Get the final allocation containing the actual value (e.g., the u8)
                                                match tcx.global_alloc(
                                                    final_alloc_id.get_alloc_id().unwrap(),
                                                ) {
                                                    GlobalAlloc::Memory(final_const_alloc) => {
                                                        let final_alloc = final_const_alloc.inner();

                                                        // Determine the layout (size) of the innermost type (e.g., u8)
                                                        match tcx.layout_of(PseudoCanonicalInput {
                                                            typing_env:
                                                                TypingEnv::fully_monomorphized(),
                                                            value: inner_ref_ty, // Get layout of &str
                                                        }) {
                                                            Ok(final_layout) => {
                                                                if next_inner_ty.is_str() {
                                                                    // It's actually &&str
                                                                    // We need the length too, which requires reading the fat pointer from the first allocation
                                                                    println!(
                                                                        "Info: Detected &&str, reading fat pointer from initial allocation."
                                                                    );

                                                                    // Read the length part from the first allocation
                                                                    let len_range = AllocRange {
                                                                        start: pointer_size,
                                                                        size: pointer_size,
                                                                    };
                                                                    let len_scalar_res = allocation
                                                                        .read_scalar(
                                                                            &tcx.data_layout,
                                                                            len_range,
                                                                            false,
                                                                        );

                                                                    match len_scalar_res {
                                                                        Ok(Scalar::Int(
                                                                            len_scalar,
                                                                        )) => {
                                                                            let len = len_scalar
                                                                                .to_target_usize(
                                                                                    tcx,
                                                                                );
                                                                            let start_byte =
                                                                                final_offset
                                                                                    .bytes_usize();

                                                                            if let Some(end_byte) =
                                                                                start_byte
                                                                                    .checked_add(
                                                                                    len as usize,
                                                                                )
                                                                            {
                                                                                if end_byte <= final_alloc.size().bytes_usize() {
                                                                                        let final_range = start_byte..end_byte;
                                                                                        let final_bytes = final_alloc.inspect_with_uninit_and_ptr_outside_interpreter(final_range);
                                                                                        match String::from_utf8(final_bytes.to_vec()) {
                                                                                            Ok(s) => {
                                                                                                 println!("Info: Successfully extracted string const from &&str indirection: \"{}\"", s);
                                                                                                 return oomir::Operand::Constant(oomir::Constant::String(s));
                                                                                            }
                                                                                            Err(e) => {
                                                                                                println!("Warning: Final string bytes from allocation {:?} (via &&str) were not valid UTF-8: {}", final_alloc_id, e);
                                                                                                return oomir::Operand::Constant(oomir::Constant::String("Invalid UTF8 (Inner)".to_string()));
                                                                                            }
                                                                                        }
                                                                                     } else { /* bounds error */ return oomir::Operand::Constant(oomir::Constant::I64(-9)); }
                                                                            } else {
                                                                                /* overflow error */
                                                                                return oomir::Operand::Constant(oomir::Constant::I64(-10));
                                                                            }
                                                                        }
                                                                        Err(e) => {
                                                                            println!(
                                                                                "Error reading length scalar for &&str: {:?}",
                                                                                e
                                                                            );
                                                                            return oomir::Operand::Constant(oomir::Constant::I64(-13));
                                                                        }
                                                                        Ok(_) => {
                                                                            println!(
                                                                                "Warning: Expected Int scalar for &&str length, got something else."
                                                                            );
                                                                            return oomir::Operand::Constant(oomir::Constant::I64(-14));
                                                                        }
                                                                    }
                                                                } else {
                                                                    // It's a sized type (like u8, i32, bool, etc.)

                                                                    // Get the allocation size
                                                                    let alloc_size =
                                                                        final_alloc.size();
                                                                    // Get the starting offset in bytes
                                                                    let start_offset_bytes =
                                                                        final_offset.bytes_usize();

                                                                    // Calculate the number of bytes *actually available* in the allocation
                                                                    // starting from the offset.
                                                                    let available_bytes_count =
                                                                        alloc_size
                                                                            .bytes_usize()
                                                                            .saturating_sub(
                                                                                start_offset_bytes,
                                                                            );

                                                                    // Determine the number of bytes to read. This should correspond
                                                                    // to the intrinsic size of the type, which is reflected in the
                                                                    // available bytes in the allocation for this constant.
                                                                    // We previously got final_layout.size which was potentially too large (8).
                                                                    // Let's trust the available_bytes_count if it's smaller than the layout size,
                                                                    // but also sanity check against the layout size if available_bytes_count seems too large.
                                                                    // For primitives like u8, the available_bytes_count (1) should be correct.

                                                                    // Ensure we don't try to read zero bytes if offset is at the end
                                                                    if available_bytes_count == 0 {
                                                                        println!(
                                                                            "Warning: Calculated available bytes is zero. Offset: {:?}, Alloc size: {:?}, Type: {:?}",
                                                                            final_offset,
                                                                            alloc_size,
                                                                            next_inner_ty
                                                                        );
                                                                        return oomir::Operand::Constant(oomir::Constant::I64(-31)); // Indicate zero available bytes error
                                                                    }

                                                                    // Calculate the end offset for the read operation
                                                                    let end_offset_bytes =
                                                                        start_offset_bytes
                                                                            + available_bytes_count;

                                                                    // Define the range based *only* on available bytes in this specific allocation
                                                                    let final_range_to_read =
                                                                        start_offset_bytes
                                                                            ..end_offset_bytes;

                                                                    let final_size =
                                                                        end_offset_bytes
                                                                            - start_offset_bytes;

                                                                    println!(
                                                                        "Debug: Reading final value for type {:?}. \
                         Final Alloc ID: {:?}, Offset: {} bytes, Alloc Size: {} bytes, \
                         Available Bytes: {}, Reading Range: {:?}",
                                                                        next_inner_ty,
                                                                        final_alloc_id,
                                                                        start_offset_bytes,
                                                                        alloc_size.bytes(),
                                                                        available_bytes_count,
                                                                        final_range_to_read
                                                                    );

                                                                    // Read the raw bytes using the calculated range based on available data
                                                                    let final_bytes = final_alloc.inspect_with_uninit_and_ptr_outside_interpreter(
                       final_range_to_read // Use the safe range
                    );

                                                                    // We expect only `final_size` bytes (e.g., 1 byte for u8)
                                                                    if final_bytes.len()
                                                                        != final_size
                                                                    {
                                                                        println!(
                                                                            "Warning: Read {} bytes via get_bytes, but expected size was {}. Type: {:?}",
                                                                            final_bytes.len(),
                                                                            final_size,
                                                                            next_inner_ty
                                                                        );
                                                                        // Decide how to handle this size mismatch - maybe proceed if enough bytes?
                                                                        // For now, let's return an error placeholder.
                                                                        return oomir::Operand::Constant(oomir::Constant::I64(-30)); // Indicate size mismatch error
                                                                    }

                                                                    println!(
                                                                        "Info: Read raw bytes {:?} for type {:?} via Ref-to-Ref.",
                                                                        final_bytes, next_inner_ty
                                                                    );

                                                                    // Determine the expected size based on the type's layout
                                                                    let expected_size =
                                                                        final_layout.size; // Use the size from layout_of

                                                                    // Convert raw bytes back to a standard Rust integer type first,
                                                                    // handling endianness and size.
                                                                    // Convert raw bytes back to a standard Rust integer type first,
                                                                    // handling endianness and size.
                                                                    let scalar_int_res: Result<ScalarInt, String> = match next_inner_ty.kind() {
                        // Unsigned Integers
                        TyKind::Uint(uty) => {
                            // Determine the intrinsic size based on the type
                            let intrinsic_size = Size::from_bytes(match uty {
                                UintTy::U8 => 1,
                                UintTy::U16 => 2,
                                UintTy::U32 => 4,
                                UintTy::Usize => expected_size.bytes(), // Use layout size for usize
                                UintTy::U64 => 8,
                                UintTy::U128 => 16,
                            });
                            // Size check performed by try_into inside match arms
                            let value_res: Result<u128, String> = match uty {
                                UintTy::U8 => final_bytes.try_into().map(u8::from_le_bytes).map(|v| v as u128)
                                    .map_err(|_| "Slice to [u8; 1] failed for U8".to_string()),
                                UintTy::U16 => final_bytes.try_into().map(u16::from_le_bytes).map(|v| v as u128)
                                    .map_err(|_| "Slice to [u8; 2] failed for U16".to_string()),
                                UintTy::U32 => final_bytes.try_into().map(u32::from_le_bytes).map(|v| v as u128)
                                    .map_err(|_| "Slice to [u8; 4] failed for U32".to_string()),
                                UintTy::Usize => {
                                    // Determine expected size for usize conversion
                                    if expected_size.bytes() == 8 {
                                        final_bytes.try_into().map(u64::from_le_bytes).map(|v| v as u128)
                                           .map_err(|_| "Slice to [u8; 8] failed for Usize(64)".to_string())
                                    } else if expected_size.bytes() == 4 {
                                        final_bytes.try_into().map(u32::from_le_bytes).map(|v| v as u128)
                                            .map_err(|_| "Slice to [u8; 4] failed for Usize(32)".to_string())
                                    } else {
                                         Err(format!("Unexpected size {} for Usize", expected_size.bytes()))
                                    }
                                }
                                UintTy::U64 => final_bytes.try_into().map(u64::from_le_bytes).map(|v| v as u128)
                                    .map_err(|_| "Slice to [u8; 8] failed for U64".to_string()),
                                UintTy::U128 => final_bytes.try_into().map(u128::from_le_bytes)
                                    .map_err(|_| "Slice to [u8; 16] failed for U128".to_string()),

                            };
                            value_res.and_then(|value| {
                                ScalarInt::try_from_uint(value, intrinsic_size)
                                    .ok_or_else(|| format!("ScalarInt::try_from_uint failed for {:?} value {} size {}", uty, value, intrinsic_size.bytes()))
                            })
                        }
                        // Signed Integers
                        TyKind::Int(ity) => {
                            // Determine the intrinsic size based on the type
                            let intrinsic_size = Size::from_bytes(match ity {
                                IntTy::I8 => 1,
                                IntTy::I16 => 2,
                                IntTy::I32 => 4,
                                IntTy::Isize => expected_size.bytes(), // Use layout size for isize
                                IntTy::I64 => 8,
                                IntTy::I128 => 16,
                            });
                            let value_res: Result<i128, String> = match ity {
                                 IntTy::I8 => final_bytes.try_into().map(i8::from_le_bytes).map(|v| v as i128)
                                    .map_err(|_| "Slice to [u8; 1] failed for I8".to_string()),
                                IntTy::I16 => final_bytes.try_into().map(i16::from_le_bytes).map(|v| v as i128)
                                    .map_err(|_| "Slice to [u8; 2] failed for I16".to_string()),
                                IntTy::I32 => final_bytes.try_into().map(i32::from_le_bytes).map(|v| v as i128)
                                    .map_err(|_| "Slice to [u8; 4] failed for I32".to_string()),
                                IntTy::Isize => {
                                     if expected_size.bytes() == 8 {
                                        final_bytes.try_into().map(i64::from_le_bytes).map(|v| v as i128)
                                            .map_err(|_| "Slice to [u8; 8] failed for Isize(64)".to_string())
                                    } else if expected_size.bytes() == 4 {
                                        final_bytes.try_into().map(i32::from_le_bytes).map(|v| v as i128)
                                            .map_err(|_| "Slice to [u8; 4] failed for Isize(32)".to_string())
                                    } else {
                                         Err(format!("Unexpected size {} for Isize", expected_size.bytes()))
                                    }
                                }
                                IntTy::I64 => final_bytes.try_into().map(i64::from_le_bytes).map(|v| v as i128)
                                    .map_err(|_| "Slice to [u8; 8] failed for I64".to_string()),
                                IntTy::I128 => final_bytes.try_into().map(i128::from_le_bytes)
                                    .map_err(|_| "Slice to [u8; 16] failed for I128".to_string()),
                            };
                            value_res.and_then(|value| {
                                ScalarInt::try_from_int(value, intrinsic_size)
                                    .ok_or_else(|| format!("ScalarInt::try_from_int failed for {:?} value {} size {}", ity, value, intrinsic_size.bytes()))
                            })
                        }
                        // Bool (usually stored as u8)
                        TyKind::Bool => {
                            final_bytes.try_into().map(u8::from_le_bytes).map_err(|_| "Slice to [u8; 1] failed for Bool".to_string())
                                .and_then(|value| {
                                    ScalarInt::try_from_uint(value as u128, expected_size)
                                         .ok_or_else(|| format!("ScalarInt::try_from_uint failed for Bool value {}", value))
                                })
                        }
                        // Char (usually stored as u32)
                        TyKind::Char => {
                            final_bytes.try_into().map(u32::from_le_bytes).map_err(|_| "Slice to [u8; 4] failed for Char".to_string())
                                .and_then(|value| {
                                    ScalarInt::try_from_uint(value as u128, expected_size)
                                         .ok_or_else(|| format!("ScalarInt::try_from_uint failed for Char value {}", value))
                                })
                        }
                        // Floats (store their bits as integers)
                        TyKind::Float(fty) => {
                             let bits_res: Result<u128, String> = match fty {
                                FloatTy::F16 => final_bytes.try_into().map(u16::from_le_bytes).map(|v| v as u128)
                                    .map_err(|_| "Slice to [u8; 2] failed for F16".to_string()),
                                FloatTy::F32 => final_bytes.try_into().map(u32::from_le_bytes).map(|v| v as u128)
                                    .map_err(|_| "Slice to [u8; 4] failed for F32".to_string()),
                                FloatTy::F64 => final_bytes.try_into().map(u64::from_le_bytes).map(|v| v as u128)
                                    .map_err(|_| "Slice to [u8; 8] failed for F64".to_string()),
                                FloatTy::F128 => final_bytes.try_into().map(u128::from_le_bytes).map(|v| v as u128)
                                .map_err(|_| "Slice to [u8; 16] failed for F128".to_string()),
                            };
                            bits_res.and_then(|bits_value| {
                                ScalarInt::try_from_uint(bits_value, expected_size)
                                    .ok_or_else(|| format!("ScalarInt::try_from_uint failed for {:?} bits {}", fty, bits_value))
                            })
                        }
                        TyKind::Str => {
                            // Handle str as a special case, usually stored as u8
                            final_bytes.try_into().map(u8::from_le_bytes).map_err(|_| "Slice to [u8; 1] failed for Str".to_string())
                                .and_then(|value| {
                                    ScalarInt::try_from_uint(value as u128, expected_size)
                                         .ok_or_else(|| format!("ScalarInt::try_from_uint failed for Str value {}", value))
                                })
                        }
                        // Other types?
                        _ => Err(format!("Cannot create ScalarInt from raw bytes for unexpected type: {:?}", next_inner_ty)),
                    };

                                                                    // Handle the result of creating ScalarInt
                                                                    match scalar_int_res {
                                                                        Ok(scalar_int) => {
                                                                            // Use the helper function to convert ScalarInt to OOMIR constant
                                                                            let oomir_const = scalar_int_to_oomir_constant(scalar_int, &next_inner_ty);
                                                                            return oomir::Operand::Constant(oomir_const);
                                                                        }
                                                                        Err(e) => {
                                                                            println!(
                                                                                "Error creating ScalarInt from bytes for type {:?}: {}",
                                                                                next_inner_ty, e
                                                                            );
                                                                            return oomir::Operand::Constant(oomir::Constant::I64(-33)); // Indicate ScalarInt creation error
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                            Err(e) => {
                                                                println!(
                                                                    "Error getting layout for innermost type {:?}: {:?}",
                                                                    next_inner_ty, e
                                                                );
                                                                return oomir::Operand::Constant(
                                                                    oomir::Constant::I64(-15),
                                                                ); // Indicate layout error
                                                            }
                                                        }
                                                    }
                                                    _ => {
                                                        println!(
                                                            "Warning: Inner pointer {:?} (via Ref-to-Ref) points to unexpected GlobalAlloc kind {:?}",
                                                            inner_ptr,
                                                            tcx.global_alloc(
                                                                final_alloc_id
                                                                    .get_alloc_id()
                                                                    .unwrap()
                                                            )
                                                        );
                                                        return oomir::Operand::Constant(
                                                            oomir::Constant::I64(-11),
                                                        ); // Indicate wrong alloc kind
                                                    }
                                                }
                                            }
                                            // Error handling for reading the inner pointer itself
                                            Err(e) => {
                                                println!(
                                                    "Error reading inner pointer scalar (for Ref-to-Ref) from allocation {:?}: {:?}",
                                                    alloc_id, e
                                                );
                                                return oomir::Operand::Constant(
                                                    oomir::Constant::I64(-12),
                                                );
                                            }
                                            Ok(other_scalar) => {
                                                println!(
                                                    "Warning: Expected inner pointer (Scalar::Ptr) when handling Ref-to-Ref, but got {:?}.",
                                                    other_scalar
                                                );
                                                return oomir::Operand::Constant(
                                                    oomir::Constant::I64(-25),
                                                ); // Indicate unexpected scalar type for inner ptr
                                            }
                                        }
                                    } else if inner_ty.is_adt() {
                                        /*let _ = inner_ty.ty_adt_def().expect("ADT type should have ADT def");
                                        let _ = match inner_ty.kind() {
                                            TyKind::Adt(_, substs) => substs,
                                            _ => panic!("Expected ADT type"),
                                        };*/

                                        println!(
                                            "Info: Handling Ref-to-ADT {:?}. Reading constant data from allocation {:?}.",
                                            inner_ty, alloc_id
                                        );

                                        // We need a way to read the constant value from the allocation based on its type.
                                        // This often involves recursion, so let's define a helper function.
                                        // The initial call uses the `pointer.offset` as the starting point within the `allocation`.
                                        match read_constant_value_from_memory(
                                            tcx,
                                            allocation,
                                            pointer.into_parts().1,
                                            *inner_ty,
                                            data_types,
                                        ) {
                                            Ok(oomir_const) => {
                                                // Successfully read the constant ADT value
                                                println!(
                                                    "Info: Successfully extracted constant ADT: {:?}",
                                                    oomir_const
                                                );
                                                oomir::Operand::Constant(oomir_const)
                                            }
                                            Err(e) => {
                                                println!(
                                                    "Error: Failed to read constant ADT of type {:?} from allocation {:?}: {}",
                                                    inner_ty, alloc_id, e
                                                );
                                                // Return an error placeholder or panic, depending on desired robustness
                                                oomir::Operand::Constant(oomir::Constant::I64(-50)) // Placeholder for ADT read error
                                            }
                                        }
                                    } else if inner_ty.is_scalar() {
                                        // Handle scalar types (e.g., i32, f64, etc.) using the experimental resolution engine
                                        println!(
                                            "Info: Handling Ref-to-scalar {:?}. Reading constant value from allocation {:?}.",
                                            inner_ty, alloc_id
                                        );

                                        // Read the constant value from the allocation
                                        match read_constant_value_from_memory(
                                            tcx,
                                            allocation,
                                            pointer.into_parts().1,
                                            *inner_ty,
                                            data_types,
                                        ) {
                                            Ok(oomir_const) => {
                                                println!(
                                                    "Info: Successfully extracted constant scalar: {:?}",
                                                    oomir_const
                                                );
                                                oomir::Operand::Constant(oomir_const)
                                            }
                                            Err(e) => {
                                                println!(
                                                    "Error: Failed to read constant scalar of type {:?} from allocation {:?}: {}",
                                                    inner_ty, alloc_id, e
                                                );
                                                oomir::Operand::Constant(oomir::Constant::I64(-51)) // Placeholder for scalar read error
                                            }
                                        }
                                    } else if inner_ty.is_array() {
                                        // Handle array types
                                        println!(
                                            "Info: Handling Ref-to-array {:?}. Reading constant value from allocation {:?}.",
                                            inner_ty, alloc_id
                                        );

                                        // Read the constant value from the allocation
                                        match read_constant_value_from_memory(
                                            tcx,
                                            allocation,
                                            pointer.into_parts().1,
                                            *inner_ty,
                                            data_types,
                                        ) {
                                            Ok(oomir_const) => {
                                                println!(
                                                    "Info: Successfully extracted constant array: {:?}",
                                                    oomir_const
                                                );
                                                oomir::Operand::Constant(oomir_const)
                                            }
                                            Err(e) => {
                                                println!(
                                                    "Error: Failed to read constant array of type {:?} from allocation {:?}: {}",
                                                    inner_ty, alloc_id, e
                                                );
                                                oomir::Operand::Constant(oomir::Constant::I64(-52)) // Placeholder for array read error
                                            }
                                        }
                                    } else {
                                        // Inner type of the Ref is not an Array, str, ADT or another Ref
                                        println!(
                                            "Warning: Scalar::Ptr points to Ref to non-Array, non-str, non-Ref, non-ADT type {:?}. Not a recognized pattern.",
                                            inner_ty
                                        );
                                        // Fall through to default handling or return specific error
                                        oomir::Operand::Constant(oomir::Constant::I64(-7)) // Indicate not ref-to-array or str or ref
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

// For when you have an OOMIR Operand but just want the inner number it holds (only works for Consts)
// I8, I16, I32, I64, Char: Returns inner value
// F32, F64: Returns rounded inner value
// Boolean: Returns 1 for true, 0 for false
// Others: Returns None
pub fn extract_number_from_operand(operand: oomir::Operand) -> Option<i64> {
    match operand {
        oomir::Operand::Constant(constant) => match constant {
            oomir::Constant::I8(val) => Some(val as i64),
            oomir::Constant::I16(val) => Some(val as i64),
            oomir::Constant::I32(val) => Some(val as i64),
            oomir::Constant::I64(val) => Some(val),
            oomir::Constant::Boolean(val) => Some(if val { 1 } else { 0 }),
            oomir::Constant::Char(val) => Some(val as i64),
            oomir::Constant::F32(val) => Some(val.round() as i64),
            oomir::Constant::F64(val) => Some(val.round() as i64),
            _ => None,
        },
        oomir::Operand::Variable { .. } => None, // can't be known at compiletime
    }
}

/// Converts a Rust MIR Scalar::Int into the appropriate OOMIR Constant based on the type.
fn scalar_int_to_oomir_constant(scalar_int: ScalarInt, ty: &Ty<'_>) -> oomir::Constant {
    match ty.kind() {
        TyKind::Int(int_ty) => match int_ty {
            IntTy::I8 => oomir::Constant::I8(scalar_int.to_i8() as i8),
            IntTy::I16 => oomir::Constant::I16(scalar_int.to_i16() as i16),
            IntTy::I32 => oomir::Constant::I32(scalar_int.to_i32() as i32),
            IntTy::Isize => oomir::Constant::I32(scalar_int.to_i64() as i32), // JVM usize -> i32
            IntTy::I64 => oomir::Constant::I64(scalar_int.to_i64()),
            IntTy::I128 => {
                let value = scalar_int.to_i128();
                let param = oomir::Constant::String(value.to_string());
                oomir::Constant::Instance {
                    class_name: "java/math/BigInteger".into(),
                    fields: HashMap::new(),
                    params: vec![param],
                }
            }
        },
        TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => oomir::Constant::I16(scalar_int.to_u8() as i16), // Widen to cover range
            UintTy::U16 => oomir::Constant::I32(scalar_int.to_u16() as i32), // Widen
            UintTy::U32 => oomir::Constant::I64(scalar_int.to_u32() as i64), // Widen
            UintTy::Usize => oomir::Constant::I32(scalar_int.to_u64() as i32), // JVM usize -> i32
            UintTy::U64 => {
                let value = scalar_int.to_u64();
                let param = oomir::Constant::String(value.to_string());
                oomir::Constant::Instance {
                    class_name: "java/math/BigInteger".into(),
                    fields: HashMap::new(),
                    params: vec![param],
                }
            }
            UintTy::U128 => {
                let value = scalar_int.to_u128();
                let param = oomir::Constant::String(value.to_string());
                oomir::Constant::Instance {
                    class_name: "java/math/BigInteger".into(),
                    fields: HashMap::new(),
                    params: vec![param],
                }
            }
        },
        TyKind::Bool => {
            let val = scalar_int.try_to_bool().unwrap_or(false);
            oomir::Constant::Boolean(val)
        }
        TyKind::Char => {
            let val = char::from_u32(scalar_int.to_u32()).unwrap_or('\0');
            oomir::Constant::Char(val)
        }
        TyKind::Float(float_ty) => match float_ty {
            FloatTy::F16 => {
                // 1. Get the raw bits
                let f16_bits = scalar_int.to_u16();
                // 2. Create an f16 from the bits.
                // need to use half::f16 so we can call to_f32()
                let f16_val = half::f16::from_bits(f16_bits);
                // 3. Convert the f16 value to an f32 value
                let f32_val = f16_val.to_f32();
                oomir::Constant::F32(f32_val)
            }
            FloatTy::F32 => oomir::Constant::F32(f32::from_bits(scalar_int.to_u32())),
            FloatTy::F64 => oomir::Constant::F64(f64::from_bits(scalar_int.to_u64())),
            FloatTy::F128 => {
                // 1. Pull out the raw bits
                let bits: u128 = scalar_int.to_u128();
                // 2. Reinterpret as f128
                let val: f128 = f128::from_bits(bits);

                // 3. Special‑case NaN & ±∞ before we try to format them:
                if val.is_nan() {
                    panic!("Attempt to store NaN as a constant BigDecimal/F128 (impossible).");
                } else if val.is_infinite() {
                    panic!("Attempt to store ±∞ as a constant BigDecimal/F128 (impossible).");
                } else {
                    // 4. Normal finite values → decimal string → BigDecimal
                    let s = f128_to_string(val);
                    let param = oomir::Constant::String(s);
                    oomir::Constant::Instance {
                        class_name: "java/math/BigDecimal".into(),
                        fields: HashMap::new(),
                        params: vec![param],
                    }
                }
            }
        },
        TyKind::Str => {
            let val = scalar_int.to_u64();
            oomir::Constant::String(val.to_string())
        }
        TyKind::Ref(_, ty, _) => {
            // Handle references by converting to the underlying type
            let inner_ty = ty;
            let inner_scalar_int = scalar_int.to_u64();
            let inner_constant = scalar_int_to_oomir_constant(inner_scalar_int.into(), inner_ty);
            return inner_constant;
        }
        _ => {
            // non-int type
            panic!("Unsupported type for ScalarInt conversion: {:?}", ty);
        }
    }
}
