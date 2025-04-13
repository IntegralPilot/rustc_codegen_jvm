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
use sha2::Digest;

/// Converts a Rust MIR type (`Ty`) to an OOMIR type (`oomir::Type`).
fn ty_to_oomir_type<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::Type {
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
        rustc_middle::ty::TyKind::Adt(adt_def, substs) => {
            // Get the full path string for the ADT
            let full_path_str = tcx.def_path_str(adt_def.did());

            if full_path_str == "String" || full_path_str == "std::string::String" {
                oomir::Type::String
            } else {
                let full_path_str = tcx.def_path_str(adt_def.did());
                let safe_name = make_jvm_safe(&full_path_str);
                let jvm_name = safe_name.replace("::", "/").replace('.', "/");
                if adt_def.is_struct() {
                    let variant = adt_def.variant(0usize.into());
                    let oomir_fields = variant
                        .fields
                        .iter()
                        .map(|field_def| {
                            let field_name = field_def.ident(tcx).to_string();
                            let field_mir_ty = field_def.ty(tcx, substs);
                            let field_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);
                            (field_name, field_oomir_type)
                        })
                        .collect::<Vec<_>>();
                    data_types.insert(
                        jvm_name.clone(),
                        oomir::DataType {
                            name: jvm_name.clone(),
                            fields: oomir_fields,
                        },
                    );
                }
                oomir::Type::Class(jvm_name)
            }
        }
        rustc_middle::ty::TyKind::Str => oomir::Type::String,
        rustc_middle::ty::TyKind::Ref(_, inner_ty, _) => {
            ty_to_oomir_type(*inner_ty, tcx, data_types)
        }
        rustc_middle::ty::TyKind::RawPtr(inner_ty, _) => {
            println!("Info: Mapping RawPtr({:?}) to OOMIR I64", inner_ty); // Add logging
            oomir::Type::I64 // Represent raw pointers as integer addresses
        }
        rustc_middle::ty::TyKind::Array(component_ty, _) => {
            // Special case for arrays of string references
            if let TyKind::Ref(_, inner_ty, _) = component_ty.kind() {
                if inner_ty.is_str() {
                    return oomir::Type::Array(Box::new(oomir::Type::String));
                }
            }
            // Default array handling
            oomir::Type::Array(Box::new(ty_to_oomir_type(*component_ty, tcx, data_types)))
        }
        rustc_middle::ty::TyKind::Tuple(tuple_elements) => {
            // Handle the unit type () -> Void
            if tuple_elements.is_empty() {
                return oomir::Type::Void;
            }

            // Handle non-empty tuples -> generate a class
            let element_mir_tys: Vec<Ty<'tcx>> = tuple_elements.iter().collect(); // Collect MIR types

            // Generate the JVM class name for this specific tuple type
            let tuple_class_name = generate_tuple_jvm_class_name(&element_mir_tys, tcx, data_types);

            // Check if we've already created the DataType for this tuple signature
            if !data_types.contains_key(&tuple_class_name) {
                println!(
                    "Info: Defining new tuple type class: {} for MIR type {:?}",
                    tuple_class_name, ty
                );
                // Create the fields ("field0", "field1", ...) and their OOMIR types
                let oomir_fields = element_mir_tys
                    .iter()
                    .enumerate()
                    .map(|(i, &elem_ty)| {
                        let field_name = format!("field{}", i);
                        // Recursively convert element type to OOMIR type
                        let field_oomir_type = ty_to_oomir_type(elem_ty, tcx, data_types);
                        (field_name, field_oomir_type)
                    })
                    .collect::<Vec<_>>();

                // Create and insert the DataType definition
                let tuple_data_type = oomir::DataType {
                    name: tuple_class_name.clone(),
                    fields: oomir_fields,
                };
                data_types.insert(tuple_class_name.clone(), tuple_data_type);
                 println!("   -> Added DataType: {:?}", data_types[&tuple_class_name]);
            } else {
                 println!("Info: Reusing existing tuple type class: {}", tuple_class_name);
            }

            // Return the OOMIR type as a Class reference
            oomir::Type::Class(tuple_class_name)
        }
        rustc_middle::ty::TyKind::Slice(component_ty) => {
            // Special case for slices of string references
            if let TyKind::Ref(_, inner_ty, _) = component_ty.kind() {
                if inner_ty.is_str() {
                    return oomir::Type::Array(Box::new(oomir::Type::String));
                }
            }
            // Default slice handling
            oomir::Type::Array(Box::new(ty_to_oomir_type(*component_ty, tcx, data_types)))
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

// Other functions (e.g., `convert_basic_block`, `mir_to_oomir`) should be updated similarly.
/// Convert a single MIR basic block into an OOMIR basic block.
fn convert_basic_block<'tcx>(
    bb: BasicBlock,
    bb_data: &BasicBlockData<'tcx>,
    tcx: TyCtxt<'tcx>,
    mir: &Body<'tcx>,
    return_oomir_type: &oomir::Type, // Pass function return type
    basic_blocks: &mut HashMap<String, oomir::BasicBlock>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::BasicBlock {
    // Use the basic block index as its label.
    let label = format!("bb{}", bb.index());
    let mut instructions = Vec::new();

    // Convert each MIR statement in the block.
    for stmt in &bb_data.statements {
        if let StatementKind::Assign(box (place, rvalue)) = &stmt.kind {
            let dest = place_to_string(place, tcx);
            match rvalue {
                Rvalue::BinaryOp(bin_op, box (op1, op2)) => {
                    let oomir_op1 = {
                        convert_operand(op1, tcx, mir, data_types)
                    };
                    let oomir_op2 = {
                        convert_operand(op2, tcx, mir, data_types)
                    };

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
                        BinOp::AddWithOverflow | BinOp::SubWithOverflow | BinOp::MulWithOverflow => {
                            let dest_place = place;
                            let dest_name = place_to_string(dest_place, tcx);

                            // 1. Get OOMIR operands
                            let oomir_op1 = convert_operand(op1, tcx, mir, data_types);
                            let oomir_op2 = convert_operand(op2, tcx, mir, data_types);

                            // 2. Determine types
                            let place_ty = dest_place.ty(&mir.local_decls, tcx).ty;
                            let (result_mir_ty, overflow_mir_ty) = if let TyKind::Tuple(elements) = place_ty.kind() {
                                if elements.len() == 2 { (elements[0], elements[1]) }
                                else { panic!("Checked op destination is not a 2-tuple: {:?}", place_ty); }
                            } else { panic!("Checked op destination is not a tuple: {:?}", place_ty); };
                            let op_oomir_ty = ty_to_oomir_type(result_mir_ty, tcx, data_types);
                            let overflow_oomir_ty = ty_to_oomir_type(overflow_mir_ty, tcx, data_types);
                            if overflow_oomir_ty != oomir::Type::Boolean {
                                println!("Warning: Expected boolean overflow type, got {:?}", overflow_oomir_ty);
                            }

                            // 3. Get tuple class name
                            let tuple_class_type = ty_to_oomir_type(place_ty, tcx, data_types); // Ensure DataType exists
                            let tuple_class_name = match &tuple_class_type {
                                oomir::Type::Class(name) => name.clone(),
                                _ => panic!("Internal Error: Checked op tuple type did not resolve to OOMIR Class"),
                            };

                            // 4. Emit the OOMIR instructions for the checked operation calculation
                            let operation_string = match bin_op {
                                BinOp::AddWithOverflow => "add",
                                BinOp::SubWithOverflow => "sub",
                                BinOp::MulWithOverflow => "mul",
                                _ => unreachable!(),
                            };
                            // Pass current instruction count as unique offset
                            let (checked_instructions, tmp_result_var, tmp_overflow_var) =
                                emit_checked_arithmetic_oomir_instructions( // Call the renamed function
                                    &dest_name,
                                    &oomir_op1,
                                    &oomir_op2,
                                    &op_oomir_ty,
                                    operation_string,
                                    instructions.len(), // Use current length as offset
                                );
                            // Add the generated instructions to the current block
                            instructions.extend(checked_instructions);

                            // 5. Construct the tuple object instance *after* the check instructions
                            instructions.push(oomir::Instruction::ConstructObject {
                                dest: dest_name.clone(), // Assign to the original tuple variable _7
                                class_name: tuple_class_name.clone(),
                            });

                            // 6. Set the fields using the temporary variables returned by the helper
                            instructions.push(oomir::Instruction::SetField {
                                object_var: dest_name.clone(),
                                field_name: "field0".to_string(),
                                value: oomir::Operand::Variable { name: tmp_result_var, ty: op_oomir_ty.clone() }, // Use returned temp name
                                field_ty: op_oomir_ty.clone(),
                                owner_class: tuple_class_name.clone(),
                            });
                            instructions.push(oomir::Instruction::SetField {
                                object_var: dest_name.clone(),
                                field_name: "field1".to_string(),
                                value: oomir::Operand::Variable { name: tmp_overflow_var, ty: oomir::Type::Boolean }, // Use returned temp name
                                field_ty: oomir::Type::Boolean,
                                owner_class: tuple_class_name.clone(),
                            });
                             println!("Info: Finished emitting checked op {:?} -> {}", bin_op, dest_name);
                        }
                        BinOp::BitOr => instructions.push(oomir::Instruction::BitOr {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::BitXor => instructions.push(oomir::Instruction::BitXor {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::Shl => instructions.push(oomir::Instruction::Shl {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::Shr => instructions.push(oomir::Instruction::Shr {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::Ne => instructions.push(oomir::Instruction::Ne {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::Le => instructions.push(oomir::Instruction::Le {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::Ge => instructions.push(oomir::Instruction::Ge {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::Lt => instructions.push(oomir::Instruction::Lt {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        BinOp::Gt => instructions.push(oomir::Instruction::Gt {
                            dest,
                            op1: oomir_op1,
                            op2: oomir_op2,
                        }),
                        _ => {
                            println!(
                                "Warning: Unhandled binary operation {:?} for dest {}",
                                bin_op, dest
                            );
                            instructions.push(oomir::Instruction::Move {
                                dest,
                                src: get_placeholder_operand(place, mir, tcx, data_types),
                            });
                        }
                    }
                }
                Rvalue::Use(mir_operand) => {
                    let dest_name = place_to_string(place, tcx);
                    match mir_operand {
                        MirOperand::Copy(src_place) | MirOperand::Move(src_place) => {
                            // Check if the SOURCE place has a direct field projection
                            if let Some((base_place, field_index, field_mir_ty)) =
                                extract_base_and_field(tcx, src_place)
                            {
                                // It's a field access (struct OR tuple)! Generate GetField
                                let object_var_name = place_to_string(&base_place, tcx); // e.g., "_1" or "_7"

                                // Get the OOMIR type of the base object (_1 or _7)
                                // For tuples, this will now be Type::Class("rust/tuples/Tuple_...")
                                let owner_oomir_type =
                                    get_place_type(&base_place, mir, tcx, data_types);
                                let owner_class_name = match owner_oomir_type {
                                    oomir::Type::Class(name) => name,
                                    // Added check for Ref<Class> just in case base_place is a reference
                                    oomir::Type::Reference(inner) => {
                                         if let oomir::Type::Class(name) = *inner { name }
                                         else {
                                             panic!("GetField source ref inner type is not Class: {:?}, Place: {:?}", inner, src_place);
                                         }
                                    }
                                    _ => panic!(
                                        "GetField source '{}' is not a Class or Ref<Class> type: {:?}. Place: {:?}",
                                        object_var_name, owner_oomir_type, src_place
                                    ),
                                };

                                // Get the field NAME from the index.
                                // This helper works for both structs and tuples because the DataType
                                // for tuples now contains fields named "field0", "field1", etc.
                                let field_name = match get_field_name_from_index(
                                    &owner_class_name,
                                    field_index.index(),
                                    data_types,
                                ) {
                                    Ok(name) => name,
                                    Err(e) => {
                                        // This error is now more serious if it happens for a Class type
                                        panic!(
                                            "Error getting field name for class '{}' index {}: {}. MIR Place: {:?}",
                                            owner_class_name, field_index.index(), e, src_place
                                        );
                                    }
                                };

                                // Convert the MIR field type to OOMIR type
                                let field_oomir_type =
                                    ty_to_oomir_type(field_mir_ty, tcx, data_types);

                                println!(
                                    "Info: Translating MIR assignment from field (struct/tuple): {} = {}.{} (GetField)",
                                    dest_name, object_var_name, field_name
                                );

                                instructions.push(oomir::Instruction::GetField {
                                    dest: dest_name,
                                    object_var: object_var_name,
                                    field_name,
                                    field_ty: field_oomir_type,
                                    owner_class: owner_class_name, // Use extracted name
                                });
                            } else {
                                // No field projection on source (e.g., let x = y;)
                                let src_oomir_operand =
                                    convert_operand(mir_operand, tcx, mir, data_types);
                                println!(
                                    "Info: Translating MIR assignment from simple place: {} = {:?} (Move)",
                                    dest_name, src_oomir_operand
                                );
                                instructions.push(oomir::Instruction::Move {
                                    dest: dest_name,
                                    src: src_oomir_operand,
                                });
                            }
                        }
                        MirOperand::Constant(_) => {
                            // Source is a constant, generate Move
                            let src_oomir_operand =
                                convert_operand(mir_operand, tcx, mir, data_types);
                            instructions.push(oomir::Instruction::Move {
                                dest: dest_name,
                                src: src_oomir_operand,
                            });
                        }
                    }
                }
                Rvalue::Aggregate(box kind, operands) => {
                    let dest_name = place_to_string(place, tcx);

                    match kind {
                        rustc_middle::mir::AggregateKind::Tuple => {
                            // 1. Get the OOMIR type of the destination (should be Class(tuple_class_name))
                            let dest_oomir_type = get_place_type(place, mir, tcx, data_types);
                            let tuple_class_name = match dest_oomir_type {
                                oomir::Type::Class(name) => name,
                                _ => panic!("Internal Error: Destination of Tuple Aggregate is not an OOMIR Class type, got {:?}", dest_oomir_type),
                            };

                            println!(
                                "Info: Handling Tuple Aggregate for '{}' (Class: {})",
                                dest_name, tuple_class_name
                            );

                            // 2. Construct the tuple object
                            instructions.push(oomir::Instruction::ConstructObject {
                                dest: dest_name.clone(),
                                class_name: tuple_class_name.clone(),
                            });
                             println!("   -> Emitted ConstructObject: dest={}, class={}", dest_name, tuple_class_name);

                            // 3. Set fields ("field0", "field1", ...)
                            for (i, mir_op) in operands.iter().enumerate() {
                                let field_name = format!("field{}", i);
                                // Get the OOMIR type of the element being assigned
                                // We need the corresponding MIR type from the destination Place's type
                                let place_ty = place.ty(&mir.local_decls, tcx).ty;
                                let element_mir_ty = if let TyKind::Tuple(elements) = place_ty.kind() {
                                     elements.get(i).expect("Tuple index out of bounds for Aggregate")
                                } else {
                                    panic!("Internal Error: AggregateKind::Tuple destination Place is not TyKind::Tuple");
                                };
                                let element_oomir_type = ty_to_oomir_type(element_mir_ty.clone(), tcx, data_types);
                                let value_operand = convert_operand(mir_op, tcx, mir, data_types);

                                instructions.push(oomir::Instruction::SetField {
                                    object_var: dest_name.clone(), // The tuple object
                                    field_name: field_name.clone(),
                                    value: value_operand.clone(),
                                    field_ty: element_oomir_type.clone(),
                                    owner_class: tuple_class_name.clone(),
                                });
                                println!(
                                    "   -> Emitted SetField: object={}, field={}, type={:?}, value={:?}",
                                    dest_name, field_name, element_oomir_type, value_operand
                                );
                            }
                        }

                        rustc_middle::mir::AggregateKind::Array(mir_element_ty) => {
                            println!(
                                "Info: Handling Rvalue::Aggregate Array for dest '{}'",
                                dest_name
                            );

                            // 1. Get the OOMIR element type
                            let oomir_element_type =
                                ty_to_oomir_type(*mir_element_ty, tcx, data_types);

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
                                let value_operand =
                                    convert_operand(mir_operand, tcx, mir, data_types);
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
                        rustc_middle::mir::AggregateKind::Adt(def_id, variant_idx, substs, _user_type_annotation_index, _field_index) => {
                            let adt_def = tcx.adt_def(*def_id);
                            if adt_def.is_struct() {
                                let variant = adt_def.variant(*variant_idx);
                                let rust_path = tcx.def_path_str(adt_def.did());
                                let jvm_class_name = make_jvm_safe(&rust_path).replace("::", "/");
       
                                println!(
                                    "Info: Handling Struct Aggregate for '{}' (Class: {})",
                                    dest_name, // Use simple name "_1"
                                    jvm_class_name
                                );
       
                                // 1. Construct object (assigns to simple name like "_1")
                                instructions.push(oomir::Instruction::ConstructObject {
                                    dest: dest_name.clone(), // Correct: dest is "_1"
                                    class_name: jvm_class_name.clone(),
                                });
                                 println!( // Log correction
                                       "   -> Emitted ConstructObject: dest={}, class={}",
                                       dest_name, // Log simple name
                                       jvm_class_name
                                   );
       
                                // 2. Set fields (uses simple name "_1" as object_var)
                                let oomir_fields = variant
                                    .fields
                                    .iter()
                                    .map(|field_def| {
                                        let field_name = field_def.ident(tcx).to_string();
                                        let field_mir_ty = field_def.ty(tcx, substs);
                                        let field_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);
                                        (field_name, field_oomir_type)
                                    })
                                    .collect::<Vec<_>>();

                                for (field_def, mir_operand) in variant.fields.iter().zip(operands.iter()) {
                                    let field_name = field_def.ident(tcx).to_string();
                                    // Get the *monomorphized* type of the field using substitutions
                                    let field_mir_ty = field_def.ty(tcx, substs);
                                    let field_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);
                                    let value_operand = convert_operand(mir_operand, tcx, mir, data_types);

                                    instructions.push(oomir::Instruction::SetField {
                                        object_var: dest_name.clone(), // The struct object we just created
                                        field_name: field_name.clone(),
                                        value: value_operand.clone(),
                                        field_ty: field_oomir_type.clone(),
                                        owner_class: jvm_class_name.clone(), // The class where the field is defined
                                    });
                                    println!(
                                        "   -> Emitted SetField: object={}, field={}, type={:?}, value={:?}",
                                        dest_name, field_name, field_oomir_type, value_operand
                                    );
                                }
                                // Ensure DataType insertion uses JVM name, not dest_name
                                 if !data_types.contains_key(&jvm_class_name) { // Use JVM name as key
                                       data_types.insert(
                                           jvm_class_name.clone(), // Key is JVM name
                                           oomir::DataType { name: jvm_class_name, fields: oomir_fields }, // Ensure oomir_fields captured correctly
                                       );
                                   }
       
                                   } else if adt_def.is_enum() {
                                // Enum handling is more complex (needs discriminant, potentially different layouts)
                                // do it later
                                println!(
                                    "Warning: Enum aggregate kind not yet fully handled for dest {}. ADT: {:?}",
                                    dest_name,
                                    adt_def.did()
                                );
                                instructions.push(oomir::Instruction::Move {
                                    dest: dest_name,
                                    src: get_placeholder_operand(place, mir, tcx, data_types),
                                });
                            } else if adt_def.is_union() {
                                println!(
                                    "Warning: Union aggregate kind not yet handled for dest {}. ADT: {:?}",
                                    dest_name,
                                    adt_def.did()
                                );
                                instructions.push(oomir::Instruction::Move {
                                    dest: dest_name,
                                    src: get_placeholder_operand(place, mir, tcx, data_types),
                                });
                            } else {
                                println!(
                                    "Warning: Unhandled ADT kind {:?} for dest {}",
                                    adt_def, dest_name
                                );
                                instructions.push(oomir::Instruction::Move {
                                    dest: dest_name,
                                    src: get_placeholder_operand(place, mir, tcx, data_types),
                                });
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
                                src: get_placeholder_operand(place, mir, tcx, data_types), // Use helper
                            });
                        }
                    }
                }
                Rvalue::Ref(_region, _borrow_kind, source_place) => {
                    let dest_name = dest; // e.g., "_9"

                    // Check if the place being referenced (source_place) involves a field access
                    if let Some((base_place, field_index, field_mir_ty)) = extract_base_and_field(tcx, source_place) {
                        // --- Field access inside Ref ---
                        let object_var_name = place_to_string(&base_place, tcx); // e.g., "_1"

                        // Get the type of the base object to find the owner class name
                        let owner_class_oomir_type = get_place_type(&base_place, mir, tcx, data_types);
                        let owner_class_name = match &owner_class_oomir_type {
                             oomir::Type::Class(name) => name.clone(),
                             oomir::Type::Reference(inner) => { // Handle if base is &Struct
                                 if let oomir::Type::Class(name) = inner.as_ref() { name.clone() }
                                 else { panic!("Ref source's inner type is not a Class: {:?}", inner); }
                             }
                             _ => panic!("Ref source base '{}' is not a class type: {:?}", object_var_name, owner_class_oomir_type),
                         };

                        // Get the field name
                        let field_name = match get_field_name_from_index(&owner_class_name, field_index.index(), data_types) {
                            Ok(name) => name,
                            Err(e) => {
                                println!("Error getting field name for Ref source: {}. Defaulting.", e);
                                format!("field{}", field_index.index()) // Fallback name
                            }
                        };

                        // Get the field type
                        let field_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);

                        // Create a temporary OOMIR variable name to hold the field value
                        let temp_field_var = format!("{}_ref_field_temp", dest_name);

                         println!( // Add log
                            "Info: Handling Rvalue::Ref with field source: {} = &({}.{}). Temp var: {}",
                            dest_name, object_var_name, field_name, temp_field_var
                        );

                        // 1. Emit GetField to load the field value into the temporary variable
                        instructions.push(oomir::Instruction::GetField {
                            dest: temp_field_var.clone(), // Store field value here
                            object_var: object_var_name,
                            field_name,
                            field_ty: field_oomir_type.clone(), // Type of the actual field
                            owner_class: owner_class_name,
                        });

                        // 2. Emit Move to assign the temporary variable to the final destination
                        //    The type of the *source operand* here is the field's type.
                        //    The destination variable (`dest_name`, e.g., "_9") will conceptually hold
                        //    a reference, but in OOMIR/JVM this often means just moving the value/object ref.
                        //    The type system in lower2 needs to know `_9` is a reference type.
                        instructions.push(oomir::Instruction::Move {
                            dest: dest_name, // Final destination, e.g., "_9"
                            src: oomir::Operand::Variable {
                                name: temp_field_var,
                                ty: field_oomir_type, // Use the type of the value obtained from GetField
                            },
                        });

                    } else {
                        // --- Simple Ref (no field access in source) ---
                        let source_name = place_to_string(source_place, tcx); // e.g., "_3" if it was _9 = &_3
                        let source_type = get_place_type(source_place, mir, tcx, data_types);

                        println!( // Keep existing log for simple refs
                            "Info: Handled Rvalue::Ref: {} = &{} (via Move with types src={:?})",
                            dest_name, // Use dest_name calculated earlier
                            source_name,
                            source_type
                        );

                        instructions.push(oomir::Instruction::Move {
                            dest: dest_name, // Final destination
                            src: oomir::Operand::Variable {
                                name: source_name,
                                ty: source_type, // Type of the variable being referenced
                            },
                        });
                    }
                }
                _ => {
                    println!("Warning: Unhandled rvalue {:?}", rvalue);
                    // Assign a type-aware placeholder
                    instructions.push(oomir::Instruction::Move {
                        dest,                                // Use the calculated name
                        src: get_placeholder_operand(place, mir, tcx, data_types), // Use helper
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
                    let return_operand = convert_operand(
                        &MirOperand::Move(Place::return_place()),
                        tcx,
                        mir,
                        data_types,
                    );
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
                let discr_operand = convert_operand(discr, tcx, mir, data_types);
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
                println!("the function name is {:?}", func);
                let function_name = make_jvm_safe(format!("{:?}", func).as_str()); // Get function name - needs refinement to extract actual name
                let oomir_args = args
                    .iter()
                    .map(|arg| convert_operand(&arg.node, tcx, mir, data_types))
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
                let condition_operand: oomir::Operand;
                let temp_condition_value_var; // To store the name if GetField is used

                // Check if the condition operand is a direct use of a place (Copy or Move)
                let condition_place_opt = match cond {
                    MirOperand::Copy(place) | MirOperand::Move(place) => Some(place),
                    _ => None, // If it's a constant, handle directly
                };

                if let Some(place) = condition_place_opt {
                    // Now, check if this place has a field projection
                    if let Some((base_place, field_index, field_mir_ty)) = extract_base_and_field(tcx, place) {
                        // It's a field access (e.g., _7.1)! Generate GetField
                        let object_var_name = place_to_string(&base_place, tcx); // e.g., "_7"
                        let owner_oomir_type = get_place_type(&base_place, mir, tcx, data_types);
                        let owner_class_name = match &owner_oomir_type {
                             oomir::Type::Class(name) => name.clone(),
                             oomir::Type::Reference(inner) => { // Handle if base is &Tuple
                                 if let oomir::Type::Class(name) = inner.as_ref() { name.clone() }
                                 else { panic!("Assert cond field source's inner type is not a Class: {:?}", inner); }
                             }
                             _ => panic!("Assert cond field source base '{}' is not a class type: {:?}", object_var_name, owner_oomir_type),
                         };

                        let field_name = match get_field_name_from_index(&owner_class_name, field_index.index(), data_types) {
                             Ok(name) => name,
                             Err(e) => panic!("Error getting field name for assert condition: {}", e),
                         };

                        let field_oomir_type = ty_to_oomir_type(field_mir_ty, tcx, data_types);

                        // Generate a temporary variable to hold the result of GetField
                        let temp_dest = format!("assert_cond_val_{}", bb.index());
                        instructions.push(oomir::Instruction::GetField {
                            dest: temp_dest.clone(),
                            object_var: object_var_name,
                            field_name,
                            field_ty: field_oomir_type.clone(),
                            owner_class: owner_class_name,
                        });

                        // Use the temporary variable as the condition operand
                        condition_operand = oomir::Operand::Variable {
                            name: temp_dest.clone(),
                            ty: field_oomir_type,
                        };
                        temp_condition_value_var = Some(temp_dest); // Store for potential negation

                         println!( // Log the GetField generation
                            "Info: Assert condition uses field access {:?}. Emitted GetField to temp '{}'",
                            place_to_string(place, tcx),
                            temp_condition_value_var.as_ref().unwrap()
                         );

                    } else {
                        // It's a simple place (e.g., _3), convert normally
                         println!( // Log simple case
                            "Info: Assert condition uses simple place {:?}",
                            place_to_string(place, tcx)
                         );
                        condition_operand = convert_operand(cond, tcx, mir, data_types);
                    }
                } else {
                     println!( // Log constant case
                        "Info: Assert condition uses constant operand {:?}",
                        cond
                     );
                    // Condition is likely a constant itself
                    condition_operand = convert_operand(cond, tcx, mir, data_types);
                }
                // --- End of condition operand handling ---


                // The MIR assert checks `!cond == expected`. Rust asserts check `cond == expected`.
                // Standard Rust `assert!(expr)` lowers to MIR `assert(expr, expected: true, ...)`
                // Standard Rust `assert_eq!(a,b)` might lower differently, but `assert!(a==b)` lowers like above.
                // The `checked_add` MIR uses `assert(!move (_7.1: bool), expected: true, ...)` effectively meaning "panic if _7.1 is true".
                // So, we need to check if `condition_operand == *expected`.

                // Generate a comparison instruction to check if the *actual condition value*
                // matches the expected boolean value.
                let comparison_dest = format!("assert_cmp_{}", bb.index()); // e.g., assert_cmp_3

                // Handle potential negation: MIR `assert(!cond)` means panic if `cond` is true.
                // MIR `assert(cond)` means panic if `cond` is false.
                // The `expected` field tells us what the non-panic value should be.
                // We want to branch to the failure block if `condition_operand != expected`.

                 println!( // Log the comparison generation
                    "Info: Generating Assert comparison: '{}' = ({:?}) == {:?}",
                    comparison_dest,
                    condition_operand,
                    *expected
                 );

                instructions.push(oomir::Instruction::Eq {
                    dest: comparison_dest.clone(),
                    op1: condition_operand, // Use the potentially GetField'd value
                    op2: oomir::Operand::Constant(oomir::Constant::Boolean(*expected)),
                });

                // Generate a branch based on the comparison result
                let success_block = format!("bb{}", target.index()); // Success path
                let failure_block = format!("assert_fail_{}", bb.index()); // Failure path label

                println!( // Log the branch generation
                    "Info: Generating Assert branch: if '{}' == true goto {} else goto {}",
                    comparison_dest, success_block, failure_block
                 );

                instructions.push(oomir::Instruction::Branch {
                    condition: oomir::Operand::Variable {
                        name: comparison_dest, // Use the result of the Eq comparison
                        ty: oomir::Type::Boolean,
                    },
                    true_block: success_block, // Jump here if condition == expected (assertion holds)
                    false_block: failure_block.clone(), // Jump here if assertion fails
                });

                // --- Add the failure block ---
                // Extract the message. msg is an AssertMessage.
                // We need to handle different kinds of AssertMessage.
                let panic_message = match &**msg {
                    rustc_middle::mir::AssertKind::BoundsCheck { len, index } => {
                        // TODO: More sophisticated message generation using len/index operands later
                        format!("BoundsCheck failed (len: {:?}, index: {:?})", len, index)
                    }
                    rustc_middle::mir::AssertKind::Overflow(op, l, r) => {
                         // TODO: Convert l and r operands to strings if possible later
                        format!("Overflow({:?}, {:?}, {:?})", op, l, r)
                    }
                    rustc_middle::mir::AssertKind::OverflowNeg(op) => {
                         format!("OverflowNeg({:?})", op)
                    }
                     rustc_middle::mir::AssertKind::DivisionByZero(op) => {
                         format!("DivisionByZero({:?})", op)
                    }
                     rustc_middle::mir::AssertKind::RemainderByZero(op) => {
                         format!("RemainderByZero({:?})", op)
                    }
                     rustc_middle::mir::AssertKind::ResumedAfterReturn(_) => {
                         "ResumedAfterReturn".to_string()
                    }
                     rustc_middle::mir::AssertKind::ResumedAfterPanic(_) => {
                         "ResumedAfterPanic".to_string()
                    }
                     rustc_middle::mir::AssertKind::MisalignedPointerDereference { required, found } => {
                        format!("MisalignedPointerDereference (required: {:?}, found: {:?})", required, found)
                    }
                    rustc_middle::mir::AssertKind::NullPointerDereference => {
                        "NullPointerDereference".to_string()
                    }
                };

                let fail_instructions = vec![oomir::Instruction::ThrowNewWithMessage {
                    exception_class: "java/lang/RuntimeException".to_string(), // Or ArithmeticException for overflows?
                    message: panic_message,
                }];
                 println!( // Log the failure block creation
                    "Info: Creating failure block '{}'",
                    failure_block
                 );
                basic_blocks.insert( // Ensure 'basic_blocks' map is mutable and passed in
                    failure_block.clone(),
                    oomir::BasicBlock {
                        label: failure_block,
                        instructions: fail_instructions,
                    },
                );
            }
            TerminatorKind::Drop {
                place: dropped_place,
                target,
                unwind: _,
                replace: _,
            } => {
                // In simple cases (no custom Drop trait), a MIR drop often just signifies
                // the end of a scope before control flow continues.
                // We need to emit the jump to the target block.
                // We ignore unwind paths for now.
                // We also don't emit an explicit OOMIR 'drop' instruction yet,
                // as standard GC handles memory. If explicit resource cleanup (like file.close())
                // were needed, this would require much more complex handling (e.g., try-finally).

                println!(
                    "Info: Handling Drop terminator for place {:?}. Jumping to target bb{}.",
                    place_to_string(dropped_place, tcx), // Log which variable is conceptually dropped
                    target.index()
                );

                let target_label = format!("bb{}", target.index());
                instructions.push(oomir::Instruction::Jump {
                    target: target_label,
                });
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
) -> (oomir::Function, HashMap<String, oomir::DataType>) {
    // Get a function name from the instance.
    let fn_name = tcx.item_name(instance.def_id()).to_string();

    // Extract function signature
    let mir_sig = tcx.type_of(instance.def_id()).skip_binder().fn_sig(tcx);
    let params_ty = mir_sig.inputs();
    let return_ty = mir_sig.output();

    let data_types = &mut HashMap::new();

    let params_oomir_ty: Vec<oomir::Type> = params_ty
        .skip_binder()
        .iter()
        .map(|ty| ty_to_oomir_type(*ty, tcx, data_types))
        .collect();
    let return_oomir_ty: oomir::Type = ty_to_oomir_type(return_ty.skip_binder(), tcx, data_types);

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
            data_types,
        ); // Pass return type here
        basic_blocks.insert(bb_ir.label.clone(), bb_ir);
    }

    let codeblock = oomir::CodeBlock {
        basic_blocks,
        entry: entry_label,
    };

    // Return the OOMIR representation of the function.
    (
        oomir::Function {
            name: fn_name,
            signature,
            body: codeblock,
        },
        data_types.clone(),
    )
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
// and <String as PartialEq<&str>>::eq -> eq
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
    let replaced: std::borrow::Cow<'_, str> = re_colons.replace_all(&without_angles, "_");

    // 3. ensure there are no starting or trailing > or < or _
    let re_trailing_underscore = RE_TRAILING_UNDERSCORE
        .get_or_init(|| Regex::new(r"^[<>_]+|[<>_]+$").expect("Invalid regex"));
    let replaced = re_trailing_underscore.replace_all(&replaced, "");
    
    // 4. Convert all to lowercase
    let lower = replaced.to_lowercase();

    // 5. if the first or last character is _, rm it
    let trimmed = lower.trim_start_matches('_').trim_end_matches('_');

    trimmed.to_string()
}

// --- Helper to get OOMIR Type for a Place ---
fn get_place_type<'tcx>(
    place: &Place<'tcx>,
    mir: &Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>,
) -> oomir::Type {
    let place_ty = place.ty(&mir.local_decls, tcx);
    ty_to_oomir_type(place_ty.ty, tcx, data_types)
}

// Helper to check if a place involves a field projection and extract info
fn extract_base_and_field<'tcx>(
    tcx: TyCtxt<'tcx>,
    place: &Place<'tcx>,
) -> Option<(Place<'tcx>, rustc_abi::FieldIdx, Ty<'tcx>)> {
    if let Some(proj_elem) = place.projection.last() {
        if let rustc_middle::mir::ProjectionElem::Field(field_index, field_ty) = proj_elem {
            // Create a new Place representing the base (without the last field projection)
            // This requires creating a new Place which isn't trivial as Place is complex.
            // A simpler approach for now is to assume the base is just the local.
            // WARNING: This simplification currently doesn't handle nested field access like _1.0.1 correctly.
            // For simple cases like _1.0 it works.
            // TODO: Handle nested field access properly.
            if place.projection.len() == 1 {
                // Only handle direct field access on local for now
                let base_place = Place {
                    local: place.local,
                    projection: tcx.mk_place_elems(&[]), // Empty projection for the base
                };
                // We need the actual MIR Ty<'tcx> of the field, which proj_elem gives us.
                return Some((base_place, *field_index, *field_ty));
            }
        }
    }
    None
}

// Helper to get field name from index using DataType info
fn get_field_name_from_index(
    owner_class_name: &str,
    index: usize,
    data_types: &HashMap<String, oomir::DataType>,
) -> Result<String, String> {
    // Return Result for error handling
    data_types
        .get(owner_class_name)
        .ok_or_else(|| format!("DataType not found for class '{}'", owner_class_name))
        .and_then(|data_type| {
            data_type
                .fields
                .get(index)
                .ok_or_else(|| {
                    format!(
                        "Field index {} out of bounds for class '{}' (has {} fields)",
                        index,
                        owner_class_name,
                        data_type.fields.len()
                    )
                })
                .map(|(name, _)| name.clone())
        })
}

fn short_hash(input: &str, length: usize) -> String {
    let mut hasher = sha2::Sha256::new();
    hasher.update(input);
    let full_hash = format!("{:x}", hasher.finalize());
    full_hash[..length].to_string()
}

fn generate_tuple_jvm_class_name<'tcx>(
    element_tys: &[Ty<'tcx>],
    tcx: TyCtxt<'tcx>,
    data_types: &mut HashMap<String, oomir::DataType>, // Needed for recursive calls
) -> String {

    let mut name_parts = String::new();
    for ty in element_tys {
        // convert to oomir type
        let ty = ty_to_oomir_type(*ty, tcx, data_types);
        name_parts.push_str(&ty.to_jvm_descriptor());
        name_parts.push_str("_");
    }

    // now hash the name parts
    // with a length of 10, the chanxce of a collision is approx 1 in 1.1 trillion
    let hash = short_hash(&name_parts, 10);

    format!("Tuple_{}", hash)
}

fn emit_checked_arithmetic_oomir_instructions( // Renamed
    dest_base_name: &str,
    op1: &oomir::Operand,
    op2: &oomir::Operand,
    op_ty: &oomir::Type,
    operation: &str,
    unique_id_offset: usize, // Used to ensure unique labels/temps
) -> (Vec<oomir::Instruction>, String, String) // Return Vec and temp names
{
    let mut generated_instructions = Vec::new(); // Create local Vec

    // --- Generate unique temporary variable names and labels ---
    let unique_id = unique_id_offset; // Use offset for uniqueness
    let tmp_a = format!("{}_{}_chk_a_{}", dest_base_name, operation, unique_id);
    let tmp_b = format!("{}_{}_chk_b_{}", dest_base_name, operation, unique_id);
    let tmp_result = format!("{}_{}_chk_res_{}", dest_base_name, operation, unique_id);
    let tmp_overflow = format!("{}_{}_chk_ovf_{}", dest_base_name, operation, unique_id);

    // Labels for control flow *within* this sequence
    let label_check_neg = format!("label_{}_{}_chk_neg_{}", dest_base_name, operation, unique_id);
    let label_overflow = format!("label_{}_{}_chk_ovf_{}", dest_base_name, operation, unique_id);
    let label_no_overflow = format!("label_{}_{}_chk_no_ovf_{}", dest_base_name, operation, unique_id);
    let label_end = format!("label_{}_{}_chk_end_{}", dest_base_name, operation, unique_id);

    // Labels for intermediate targets within the checks to avoid unstructured jumps
    let lbl_pos_check_b_non_neg = format!("lbl_{}_{}_pos_chk_b_non_neg_{}", dest_base_name, operation, unique_id);
    let lbl_pos_check_final_cmp = format!("lbl_{}_{}_pos_check_final_cmp_{}", dest_base_name, operation, unique_id);
    let lbl_neg_check_b_non_pos = format!("lbl_{}_{}_neg_chk_b_non_pos_{}", dest_base_name, operation, unique_id);
    let lbl_neg_check_final_cmp = format!("lbl_{}_{}_neg_chk_final_cmp_{}", dest_base_name, operation, unique_id);


    // --- Load operands into temporary variables ---
    generated_instructions.push(oomir::Instruction::Move { dest: tmp_a.clone(), src: op1.clone() });
    generated_instructions.push(oomir::Instruction::Move { dest: tmp_b.clone(), src: op2.clone() });

    let op1_var = oomir::Operand::Variable { name: tmp_a.clone(), ty: op_ty.clone() };
    let op2_var = oomir::Operand::Variable { name: tmp_b.clone(), ty: op_ty.clone() };

    // --- Get MIN/MAX constants ---
    let (const_max, const_min) = match op_ty {
        oomir::Type::I32 => (oomir::Constant::I32(i32::MAX), oomir::Constant::I32(i32::MIN)),
        // TODO: Add other types like I64
        _ => panic!("Checked arithmetic not implemented for OOMIR type {:?}", op_ty),
    };

    // --- Start Positive Overflow Check ---
    let tmp_cmp1 = format!("{}_{}_chk_cmp1_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Gt { dest: tmp_cmp1.clone(), op1: op1_var.clone(), op2: oomir::Operand::Constant(oomir::Constant::I32(0)) });
    generated_instructions.push(oomir::Instruction::Branch { condition: oomir::Operand::Variable { name: tmp_cmp1.clone(), ty: oomir::Type::Boolean }, true_block: lbl_pos_check_b_non_neg.clone(), false_block: label_check_neg.clone() }); // If a > 0 goto check b, else goto neg check

    // --- Positive Check: Check B --- (Label: lbl_pos_check_b_non_neg)
    generated_instructions.push(oomir::Instruction::Label { name: lbl_pos_check_b_non_neg.clone() });
    let tmp_cmp2 = format!("{}_{}_chk_cmp2_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Gt { dest: tmp_cmp2.clone(), op1: op2_var.clone(), op2: oomir::Operand::Constant(oomir::Constant::I32(0)) });
    generated_instructions.push(oomir::Instruction::Branch { condition: oomir::Operand::Variable { name: tmp_cmp2.clone(), ty: oomir::Type::Boolean }, true_block: lbl_pos_check_final_cmp.clone(), false_block: label_check_neg.clone() }); // If b > 0 goto final check, else goto neg check

    // --- Positive Check: Final Comparison --- (Label: lbl_pos_check_final_cmp)
    generated_instructions.push(oomir::Instruction::Label { name: lbl_pos_check_final_cmp.clone() });
    let tmp_max_minus_a = format!("{}_{}_chk_max_minus_a_{}", dest_base_name, operation, unique_id);
    let tmp_cmp3 = format!("{}_{}_chk_cmp3_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Sub { dest: tmp_max_minus_a.clone(), op1: oomir::Operand::Constant(const_max), op2: op1_var.clone() });
    generated_instructions.push(oomir::Instruction::Gt { dest: tmp_cmp3.clone(), op1: op2_var.clone(), op2: oomir::Operand::Variable { name: tmp_max_minus_a.clone(), ty: op_ty.clone() }});
    generated_instructions.push(oomir::Instruction::Branch { condition: oomir::Operand::Variable { name: tmp_cmp3.clone(), ty: oomir::Type::Boolean }, true_block: label_overflow.clone(), false_block: label_check_neg.clone() }); // If b > MAX-a goto overflow, else goto neg check

    // --- Start Negative Overflow Check --- (Label: label_check_neg)
    generated_instructions.push(oomir::Instruction::Label { name: label_check_neg.clone() });
    let tmp_cmp4 = format!("{}_{}_chk_cmp4_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Lt { dest: tmp_cmp4.clone(), op1: op1_var.clone(), op2: oomir::Operand::Constant(oomir::Constant::I32(0)) });
    generated_instructions.push(oomir::Instruction::Branch { condition: oomir::Operand::Variable { name: tmp_cmp4.clone(), ty: oomir::Type::Boolean }, true_block: lbl_neg_check_b_non_pos.clone(), false_block: label_no_overflow.clone() }); // If a < 0 goto check b, else goto no_overflow

    // --- Negative Check: Check B --- (Label: lbl_neg_check_b_non_pos)
    generated_instructions.push(oomir::Instruction::Label { name: lbl_neg_check_b_non_pos.clone() });
    let tmp_cmp5 = format!("{}_{}_chk_cmp5_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Lt { dest: tmp_cmp5.clone(), op1: op2_var.clone(), op2: oomir::Operand::Constant(oomir::Constant::I32(0)) });
    generated_instructions.push(oomir::Instruction::Branch { condition: oomir::Operand::Variable { name: tmp_cmp5.clone(), ty: oomir::Type::Boolean }, true_block: lbl_neg_check_final_cmp.clone(), false_block: label_no_overflow.clone() }); // If b < 0 goto final check, else goto no_overflow

    // --- Negative Check: Final Comparison --- (Label: lbl_neg_check_final_cmp)
    generated_instructions.push(oomir::Instruction::Label { name: lbl_neg_check_final_cmp.clone() });
    let tmp_min_minus_a = format!("{}_{}_chk_min_minus_a_{}", dest_base_name, operation, unique_id);
    let tmp_cmp6 = format!("{}_{}_chk_cmp6_{}", dest_base_name, operation, unique_id);
    generated_instructions.push(oomir::Instruction::Sub { dest: tmp_min_minus_a.clone(), op1: oomir::Operand::Constant(const_min), op2: op1_var.clone() });
    generated_instructions.push(oomir::Instruction::Lt { dest: tmp_cmp6.clone(), op1: op2_var.clone(), op2: oomir::Operand::Variable { name: tmp_min_minus_a.clone(), ty: op_ty.clone() } });
    generated_instructions.push(oomir::Instruction::Branch { condition: oomir::Operand::Variable { name: tmp_cmp6.clone(), ty: oomir::Type::Boolean }, true_block: label_overflow.clone(), false_block: label_no_overflow.clone() }); // If b < MIN-a goto overflow, else goto no_overflow

    // --- Overflow Path --- (Label: label_overflow)
    generated_instructions.push(oomir::Instruction::Label { name: label_overflow.clone() });
    generated_instructions.push(oomir::Instruction::Move { dest: tmp_overflow.clone(), src: oomir::Operand::Constant(oomir::Constant::Boolean(true)) });
    // Determine zero value for the type
    let zero_val = match op_ty {
        oomir::Type::I8 => oomir::Constant::I8(0),
        oomir::Type::I16 => oomir::Constant::I16(0),
        oomir::Type::I32 => oomir::Constant::I32(0),
        oomir::Type::I64 => oomir::Constant::I64(0),
        _ => oomir::Constant::I32(0), // Fallback or panic?
    };
    generated_instructions.push(oomir::Instruction::Move { dest: tmp_result.clone(), src: oomir::Operand::Constant(zero_val) });
    generated_instructions.push(oomir::Instruction::Jump { target: label_end.clone() });

    // --- No Overflow Path --- (Label: label_no_overflow)
    generated_instructions.push(oomir::Instruction::Label { name: label_no_overflow.clone() });
    generated_instructions.push(oomir::Instruction::Move { dest: tmp_overflow.clone(), src: oomir::Operand::Constant(oomir::Constant::Boolean(false)) });
    // Perform actual operation
    match operation {
        "add" => generated_instructions.push(oomir::Instruction::Add { dest: tmp_result.clone(), op1: op1_var.clone(), op2: op2_var.clone() }),
        "sub" => generated_instructions.push(oomir::Instruction::Sub { dest: tmp_result.clone(), op1: op1_var.clone(), op2: op2_var.clone() }),
        "mul" => generated_instructions.push(oomir::Instruction::Mul { dest: tmp_result.clone(), op1: op1_var.clone(), op2: op2_var.clone() }),
        _ => panic!("Unsupported checked operation: {}", operation),
    }
    generated_instructions.push(oomir::Instruction::Jump { target: label_end.clone() }); // Or just fall through to end label? Jump is safer.

    // --- End Path --- (Label: label_end)
    generated_instructions.push(oomir::Instruction::Label { name: label_end.clone() });
    // Result is in tmp_result, overflow flag in tmp_overflow. No instruction needed here.

    // Return the generated instructions and the names of the temporary variables
    (generated_instructions, tmp_result, tmp_overflow)
}

fn get_placeholder_operand<'tcx>(
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
