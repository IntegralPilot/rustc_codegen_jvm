//! Intrinsics.
use super::*;

pub(super) fn emit<'tcx>(
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
    external_interfaces: &mut HashSet<String>,
    label: &str,
    mut instructions: &mut Vec<oomir::Instruction>,
    terminator: &rustc_middle::mir::Terminator<'tcx>,
    func_instance: Instance<'tcx>,
    instance_ty: Ty<'tcx>,
    fn_inputs: Vec<Ty<'tcx>>,
    fn_output: Ty<'tcx>,
    oomir_output_type: oomir::Type,
    oomir_operands: Vec<oomir::Operand>,
    effective_dest: Option<String>,
    method_signature: oomir::Signature,
) {
    let called_def_id = func_instance.def_id();
    let intrinsic_name = tcx
        .opt_item_name(called_def_id)
        .map(|name| name.as_str().to_string())
        .unwrap_or_default();
    let is_compiler_intrinsic = matches!(
        func_instance.def,
        InstanceKind::Intrinsic(_) | InstanceKind::LlvmIntrinsic(_)
    ) || (!intrinsic_name.is_empty()
        && tcx.is_intrinsic(called_def_id, Symbol::intern(&intrinsic_name)));
    let is_diagnostic_item = |diagnostic| tcx.is_diagnostic_item(diagnostic, called_def_id);
    let has_diagnostic_item =
        |diagnostic: &str| tcx.is_diagnostic_item(Symbol::intern(diagnostic), called_def_id);
    let is_external_intrinsic = !called_def_id.is_local();
    let is_core_crate_item = tcx.crate_name(called_def_id.krate).as_str() == "core";
    let has_pointer_like_operand = oomir_operands.first().is_some_and(|operand| {
        matches!(
            operand.get_type(),
            Some(oomir::Type::Pointer(_) | oomir::Type::Slice(_) | oomir::Type::Str)
        )
    });
    let is_core_ptr = is_core_crate_item && has_pointer_like_operand;
    let is_size_of = (is_compiler_intrinsic && intrinsic_name.as_str() == "size_of")
        || has_diagnostic_item("mem_size_of");
    let is_align_of = (is_compiler_intrinsic && intrinsic_name.as_str() == "align_of")
        || has_diagnostic_item("mem_align_of");
    let is_size_of_val = (is_compiler_intrinsic && intrinsic_name.as_str() == "size_of_val")
        || has_diagnostic_item("mem_size_of_val");
    let is_align_of_val = is_compiler_intrinsic && intrinsic_name.as_str() == "align_of_val";
    let is_vtable_layout =
        is_compiler_intrinsic && matches!(intrinsic_name.as_str(), "vtable_size" | "vtable_align");
    // The public wrapper has no diagnostic/lang item of its own,
    // so identify it by its defining module rather than its bare name.
    let is_ptr_metadata = (is_compiler_intrinsic && intrinsic_name.as_str() == "ptr_metadata")
        || is_core_ptr_metadata_api(tcx, called_def_id);
    if is_diagnostic_item(sym::needs_drop)
        || (is_compiler_intrinsic && intrinsic_name == "needs_drop")
    {
        let queried_ty = func_instance
            .args
            .types()
            .next()
            .expect("needs_drop has a type argument");
        let needs_drop = queried_ty.needs_drop(tcx, TypingEnv::fully_monomorphized());
        if let Some(dest) = effective_dest.clone() {
            instructions.push(oomir::Instruction::Move {
                dest,
                src: oomir::Operand::Constant(oomir::Constant::Boolean(needs_drop)),
            });
        }
    } else if is_compiler_intrinsic && intrinsic_name == "abort" {
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: None,
            class_name: "org/rustlang/runtime/PanicSupport".to_string(),
            method_name: "abortNow".to_string(),
            method_ty: oomir::Signature {
                params: Vec::new(),
                ret: Box::new(oomir::Type::Void),
                is_static: true,
            },
            args: Vec::new(),
        });
    } else if is_compiler_intrinsic
        && matches!(
            intrinsic_name.as_str(),
            "assert_inhabited" | "assert_zero_valid" | "assert_mem_uninitialized_valid"
        )
    {
        // These are compile-time UB guards. rustc permits them to either
        // panic or do nothing, and reaching the invalid case would make the
        // following unsafe operation UB. Valid monomorphizations therefore
        // require no JVM instruction.
    } else if is_compiler_intrinsic && intrinsic_name == "black_box" && oomir_operands.len() == 1 {
        if let Some(dest) = effective_dest.clone() {
            instructions.push(oomir::Instruction::Move {
                dest,
                src: oomir_operands[0].clone(),
            });
        }
    } else if is_compiler_intrinsic
        && intrinsic_name == "float_to_int_unchecked"
        && oomir_operands.len() == 1
        && let Some(dest) = effective_dest.clone()
    {
        // The intrinsic's caller guarantees that the finite input is in
        // range. On valid inputs its result is therefore identical to the
        // backend's ordinary saturating float-to-int cast.
        instructions.push(oomir::Instruction::Cast {
            op: oomir_operands[0].clone(),
            ty: oomir_output_type.clone(),
            dest,
        });
    } else if is_compiler_intrinsic
        && intrinsic_name == "disjoint_bitor"
        && oomir_operands.len() == 2
        && let Some(dest) = effective_dest.clone()
    {
        instructions.push(oomir::Instruction::Binary {
            op: crate::oomir::BinaryOp::BitOr,
            dest,
            op1: oomir_operands[0].clone(),
            op2: oomir_operands[1].clone(),
        });
    } else if is_compiler_intrinsic
        && matches!(
            intrinsic_name.as_str(),
            "fadd_algebraic"
                | "fsub_algebraic"
                | "fmul_algebraic"
                | "fdiv_algebraic"
                | "frem_algebraic"
                | "fadd_fast"
                | "fsub_fast"
                | "fmul_fast"
                | "fdiv_fast"
                | "frem_fast"
        )
        && oomir_operands.len() == 2
        && let Some(dest) = effective_dest.clone()
    {
        numeric_intrinsics::algebraic_float(
            &mut instructions,
            oomir_operands,
            intrinsic_name,
            dest,
        );
    } else if is_compiler_intrinsic
        && intrinsic_name == "simd_splat"
        && oomir_operands.len() == 1
        && let Some(dest) = effective_dest.clone()
    {
        numeric_intrinsics::simd_splat(
            tcx,
            &mut instructions,
            fn_inputs,
            fn_output,
            oomir_output_type,
            oomir_operands,
            dest,
        );
    } else if is_compiler_intrinsic
        && matches!(intrinsic_name.as_str(), "simd_and" | "simd_or" | "simd_xor")
        && oomir_operands.len() == 2
    {
        numeric_intrinsics::simd_bitwise(
            &mut instructions,
            oomir_output_type,
            oomir_operands,
            effective_dest,
            intrinsic_name,
        );
    } else if is_compiler_intrinsic
        && intrinsic_name == "simd_reduce_all"
        && oomir_operands.len() == 1
    {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: "org/rustlang/runtime/Intrinsics".to_string(),
            method_name: "simdReduceAll".to_string(),
            method_ty: oomir::Signature {
                params: vec![(
                    "vector".to_string(),
                    oomir::Type::Class("java/lang/Object".to_string()),
                )],
                ret: Box::new(oomir::Type::Boolean),
                is_static: true,
            },
            args: oomir_operands.clone(),
            dest: effective_dest.clone(),
        });
    } else if is_compiler_intrinsic
        && matches!(intrinsic_name.as_str(), "simd_eq" | "simd_ne")
        && oomir_operands.len() == 2
        && let Some(dest) = effective_dest.clone()
    {
        numeric_intrinsics::simd_comparison(
            &mut instructions,
            oomir_output_type,
            oomir_operands,
            intrinsic_name,
            dest,
        );
    } else if is_compiler_intrinsic
        && matches!(
            intrinsic_name.as_str(),
            "simd_neg" | "simd_fabs" | "simd_mul"
        )
        && let Some(dest) = effective_dest.clone()
    {
        numeric_intrinsics::simd_unary(
            &mut instructions,
            oomir_output_type,
            oomir_operands,
            intrinsic_name,
            dest,
        );
    } else if is_compiler_intrinsic
        && matches!(intrinsic_name.as_str(), "bswap" | "fabs")
        && oomir_operands.len() == 1
    {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: "org/rustlang/runtime/Intrinsics".to_string(),
            method_name: if intrinsic_name == "bswap" {
                "byteSwap".to_string()
            } else {
                "floatAbs".to_string()
            },
            method_ty: oomir::Signature {
                params: vec![(
                    "value".to_string(),
                    oomir_operands[0]
                        .get_type()
                        .expect("unary intrinsic operand is typed"),
                )],
                ret: Box::new(oomir_output_type.clone()),
                is_static: true,
            },
            args: oomir_operands.clone(),
            dest: effective_dest.clone(),
        });
    } else if is_compiler_intrinsic
        && matches!(
            intrinsic_name.as_str(),
            "rotate_left" | "rotate_right" | "bitreverse"
        )
    {
        numeric_intrinsics::rotate(
            &mut instructions,
            oomir_output_type,
            oomir_operands,
            effective_dest,
            intrinsic_name,
        );
    } else if is_compiler_intrinsic
        && (matches!(
            intrinsic_name.as_str(),
            "sin" | "cos" | "exp" | "exp2" | "log" | "log2" | "log10"
        ) || matches!(
            intrinsic_name.as_str(),
            "copysignf16"
                | "copysignf32"
                | "copysignf64"
                | "copysignf128"
                | "minimumf16"
                | "minimumf32"
                | "minimumf64"
                | "minimumf128"
                | "maximumf16"
                | "maximumf32"
                | "maximumf64"
                | "maximumf128"
                | "minimum_number_nsz_f16"
                | "minimum_number_nsz_f32"
                | "minimum_number_nsz_f64"
                | "minimum_number_nsz_f128"
                | "maximum_number_nsz_f16"
                | "maximum_number_nsz_f32"
                | "maximum_number_nsz_f64"
                | "maximum_number_nsz_f128"
                | "floorf16"
                | "floorf32"
                | "floorf64"
                | "floorf128"
                | "ceilf16"
                | "ceilf32"
                | "ceilf64"
                | "ceilf128"
                | "truncf16"
                | "truncf32"
                | "truncf64"
                | "truncf128"
                | "roundf16"
                | "roundf32"
                | "roundf64"
                | "roundf128"
                | "round_ties_even_f16"
                | "round_ties_even_f32"
                | "round_ties_even_f64"
                | "round_ties_even_f128"
                | "sqrtf16"
                | "sqrtf32"
                | "sqrtf64"
                | "sqrtf128"
                | "expf16"
                | "expf32"
                | "expf64"
                | "expf128"
                | "exp2f16"
                | "exp2f32"
                | "exp2f64"
                | "exp2f128"
                | "logf16"
                | "logf32"
                | "logf64"
                | "logf128"
                | "log2f16"
                | "log2f32"
                | "log2f64"
                | "log2f128"
                | "log10f16"
                | "log10f32"
                | "log10f64"
                | "log10f128"
                | "sinf16"
                | "sinf32"
                | "sinf64"
                | "sinf128"
                | "cosf16"
                | "cosf32"
                | "cosf64"
                | "cosf128"
                | "powf16"
                | "powf32"
                | "powf64"
                | "powf128"
                | "powif16"
                | "powif32"
                | "powif64"
                | "powif128"
                | "fmaf16"
                | "fmaf32"
                | "fmaf64"
                | "fmaf128"
                | "fmuladdf16"
                | "fmuladdf32"
                | "fmuladdf64"
                | "fmuladdf128"
        ))
    {
        numeric_intrinsics::float_math(
            &mut instructions,
            oomir_output_type,
            oomir_operands,
            effective_dest,
            intrinsic_name,
        );
    } else if is_compiler_intrinsic
        && matches!(
            intrinsic_name.as_str(),
            "unchecked_funnel_shl" | "unchecked_funnel_shr"
        )
        && oomir_operands.len() == 3
    {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: "org/rustlang/runtime/Intrinsics".to_string(),
            method_name: if intrinsic_name == "unchecked_funnel_shl" {
                "funnelShiftLeft".to_string()
            } else {
                "funnelShiftRight".to_string()
            },
            method_ty: oomir::Signature {
                params: vec![
                    ("high".to_string(), oomir_output_type.clone()),
                    ("low".to_string(), oomir_output_type.clone()),
                    ("shift".to_string(), oomir::Type::U32),
                ],
                ret: Box::new(oomir_output_type.clone()),
                is_static: true,
            },
            args: oomir_operands.clone(),
            dest: effective_dest.clone(),
        });
    } else if is_compiler_intrinsic
        && intrinsic_name == "carryless_mul"
        && oomir_operands.len() == 2
    {
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: "org/rustlang/runtime/Intrinsics".to_string(),
            method_name: "carrylessMultiply".to_string(),
            method_ty: oomir::Signature {
                params: vec![
                    ("left".to_string(), oomir_output_type.clone()),
                    ("right".to_string(), oomir_output_type.clone()),
                ],
                ret: Box::new(oomir_output_type.clone()),
                is_static: true,
            },
            args: oomir_operands.clone(),
            dest: effective_dest.clone(),
        });
    } else if is_compiler_intrinsic && intrinsic_name == "catch_unwind" && oomir_operands.len() == 3
    {
        unwind::catch_unwind(&mut instructions, oomir_operands, effective_dest);
    } else if is_compiler_intrinsic
        && emit_atomic_intrinsic(
            &intrinsic_name,
            func_instance,
            instance,
            &oomir_operands,
            &oomir_output_type,
            effective_dest.clone(),
            &label,
            tcx,
            data_types,
            &mut instructions,
        )
    {
    } else if is_compiler_intrinsic
        && intrinsic_name == "const_allocate"
        && oomir_operands.len() == 2
    {
        let pointer_size = match fn_output.kind() {
            TyKind::RawPtr(pointee, _) => {
                crate::lower1::types::layout_size_bytes(tcx, *pointee).unwrap_or(1)
            }
            _ => 1,
        };
        instructions.push(oomir::Instruction::InvokeStatic {
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "nullPointer".to_string(),
            method_ty: oomir::Signature {
                params: vec![("view_size".to_string(), oomir::Type::U64)],
                ret: Box::new(oomir_output_type.clone()),
                is_static: true,
            },
            args: vec![oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(pointer_size).expect("Rust allocation pointer layout exceeds u64"),
            ))],
            dest: effective_dest,
        });
    } else if is_compiler_intrinsic && intrinsic_name == "const_deallocate" {
        // Rust's const allocator performs no allocation at
        // runtime, so its matching deallocator is a no-op.
    } else if is_compiler_intrinsic && intrinsic_name == "cold_path" {
        // The JVM has no equivalent branch-prediction hint.
    } else if is_compiler_intrinsic
        && matches!(intrinsic_name.as_str(), "integer_min" | "integer_max")
        && oomir_operands.len() == 2
        && let Some(dest) = effective_dest.clone()
    {
        numeric_intrinsics::integer_min(
            &mut instructions,
            oomir_output_type,
            oomir_operands,
            intrinsic_name,
            dest,
        );
    } else if is_compiler_intrinsic
        && intrinsic_name == "select_unpredictable"
        && oomir_operands.len() == 3
        && let Some(dest) = effective_dest.clone()
    {
        emit_value_selection(
            oomir_operands[0].clone(),
            oomir_operands[1].clone(),
            oomir_operands[2].clone(),
            &oomir_output_type,
            dest,
            &mut instructions,
        );
    } else if is_compiler_intrinsic
        && intrinsic_name == "carrying_mul_add"
        && oomir_operands.len() == 4
        && oomir_operands[0].get_type().is_some_and(|ty| {
            matches!(
                ty,
                oomir::Type::I8 | oomir::Type::I16 | oomir::Type::I32 | oomir::Type::I64
            ) || matches!(
                ty,
                oomir::Type::Class(class_name)
                    if class_name == crate::lower2::I128_CLASS
            )
        })
        && let Some(dest) = effective_dest.clone()
    {
        numeric_intrinsics::carrying_mul_add(
            &label,
            &mut instructions,
            oomir_output_type,
            oomir_operands,
            dest,
        );
    } else if is_compiler_intrinsic
        && intrinsic_name == "ctpop"
        && let Some(dest) = effective_dest.clone()
    {
        numeric_intrinsics::ctpop(&mut instructions, oomir_operands, dest);
    } else if is_compiler_intrinsic
        && matches!(
            intrinsic_name.as_str(),
            "ctlz" | "ctlz_nonzero" | "cttz" | "cttz_nonzero"
        )
        && let Some(dest) = effective_dest.clone()
    {
        numeric_intrinsics::count_zero_bits(
            &mut instructions,
            oomir_operands,
            intrinsic_name,
            dest,
        );
    } else if is_compiler_intrinsic
        && intrinsic_name.as_str() == "is_val_statically_known"
        && let Some(dest) = effective_dest.clone()
    {
        // This intrinsic is explicitly nondeterministic: callers must be
        // correct for either result. Returning false is its conservative
        // runtime implementation when this backend has not proved the
        // operand to be a compile-time constant.
        instructions.push(oomir::Instruction::Move {
            dest,
            src: oomir::Operand::Constant(oomir::Constant::Boolean(false)),
        });
    } else if is_compiler_intrinsic && intrinsic_name == "arith_offset" && oomir_operands.len() == 2
    {
        memory_intrinsics::arith_offset(
            &mut instructions,
            oomir_output_type,
            oomir_operands,
            effective_dest,
        );
    } else if is_compiler_intrinsic
        && matches!(intrinsic_name.as_str(), "saturating_add" | "saturating_sub")
        && let Some(dest) = effective_dest.clone()
    {
        numeric_intrinsics::saturating_arithmetic(
            &mut instructions,
            oomir_output_type,
            oomir_operands,
            intrinsic_name,
            dest,
        );
    } else if is_compiler_intrinsic
        && matches!(
            intrinsic_name.as_str(),
            "exact_div" | "unchecked_div" | "unchecked_rem"
        )
        && let Some(dest) = effective_dest.clone()
    {
        let [left, right] = oomir_operands.as_slice() else {
            panic!("{intrinsic_name} requires two operands")
        };
        if intrinsic_name == "unchecked_rem" {
            instructions.push(oomir::Instruction::Binary {
                op: crate::oomir::BinaryOp::Rem,
                dest,
                op1: left.clone(),
                op2: right.clone(),
            });
        } else {
            instructions.push(oomir::Instruction::Binary {
                op: crate::oomir::BinaryOp::Div,
                dest,
                op1: left.clone(),
                op2: right.clone(),
            });
        }
    } else if is_compiler_intrinsic
        && matches!(
            intrinsic_name.as_str(),
            "ptr_offset_from" | "ptr_offset_from_unsigned"
        )
        && oomir_operands.len() == 2
    {
        pointer_intrinsics::ptr_offset_from(
            &mut instructions,
            oomir_output_type,
            oomir_operands,
            effective_dest,
        );
    } else if is_compiler_intrinsic
        && intrinsic_name == "compare_bytes"
        && oomir_operands.len() == 3
    {
        memory_intrinsics::compare_bytes(&mut instructions, oomir_operands, effective_dest);
    } else if intrinsic_name == "raw_eq"
        && is_compiler_intrinsic
        && let Some(dest) = effective_dest.clone()
    {
        memory_intrinsics::raw_eq(
            tcx,
            instance,
            data_types,
            &label,
            &mut instructions,
            func_instance,
            oomir_operands,
            dest,
        );
    } else if intrinsic_name.as_str() == "caller_location"
        && is_compiler_intrinsic
        && let Some(dest) = effective_dest.clone()
    {
        let location = caller_location_operand(
            terminator.source_info,
            tcx,
            instance,
            mir,
            data_types,
            &mut instructions,
            &format!("{label}_caller_location_intrinsic"),
        );
        instructions.push(oomir::Instruction::Move {
            dest,
            src: location,
        });
    } else if is_vtable_layout && let Some(dest) = effective_dest.clone() {
        let marker_ty = oomir_operands[0]
            .get_type()
            .expect("vtable layout pointer is typed");
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: Some(dest),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: if intrinsic_name == "vtable_size" {
                "vtableSize".to_string()
            } else {
                "vtableAlign".to_string()
            },
            method_ty: oomir::Signature {
                params: vec![("marker".to_string(), marker_ty)],
                ret: Box::new(oomir_output_type.clone()),
                is_static: true,
            },
            args: vec![oomir_operands[0].clone()],
        });
    } else if (is_size_of || is_align_of)
        && let Some(dest) = effective_dest.clone()
    {
        type_intrinsics::static_layout(
            tcx,
            &mut instructions,
            func_instance,
            intrinsic_name,
            is_size_of,
            dest,
        );
    } else if is_size_of_val && let Some(dest) = effective_dest.clone() {
        type_intrinsics::size_of_val(
            tcx,
            &mut instructions,
            func_instance,
            oomir_output_type,
            oomir_operands,
            dest,
        );
    } else if is_align_of_val && let Some(dest) = effective_dest.clone() {
        type_intrinsics::align_of_val(
            tcx,
            &mut instructions,
            func_instance,
            oomir_output_type,
            oomir_operands,
            dest,
        );
    } else if intrinsic_name.as_str() == "type_id_eq"
        && is_compiler_intrinsic
        && oomir_operands.len() == 2
        && let Some(dest) = effective_dest.clone()
    {
        type_intrinsics::type_id_eq(
            tcx,
            data_types,
            &label,
            &mut instructions,
            oomir_operands,
            dest,
        );
    } else if intrinsic_name.as_str() == "forget" && is_external_intrinsic {
        // `mem::forget` intentionally consumes its argument without
        // running drop glue. The value is already represented by the
        // evaluated call operand, so there is no JVM instruction to emit.
    } else if has_diagnostic_item("ptr_drop_in_place") && is_core_ptr {
        pointer_intrinsics::ptr_drop_in_place(
            tcx,
            instance,
            data_types,
            &label,
            &mut instructions,
            func_instance,
            oomir_operands,
        );
    } else if is_diagnostic_item(sym::ptr_from_ref)
        || is_core_ptr_free_function(tcx, called_def_id, Symbol::intern("from_mut"))
    {
        if let Some(dest) = effective_dest.clone() {
            let pointer = crate::lower1::value_repr::adapt_operand_to_rust_type(
                oomir_operands[0].clone(),
                fn_output,
                &format!("{label}_pointer_from_reference"),
                tcx,
                instance,
                data_types,
                &mut instructions,
            );
            instructions.push(oomir::Instruction::Move { dest, src: pointer });
        }
    } else if (has_diagnostic_item("ptr_eq") || intrinsic_name.as_str() == "addr_eq")
        && is_core_ptr
        && oomir_operands.len() == 2
    {
        pointer_intrinsics::ptr_eq(
            &mut instructions,
            oomir_operands,
            effective_dest,
            intrinsic_name,
        );
    } else if matches!(intrinsic_name.as_str(), "dangling" | "dangling_mut")
        && is_external_intrinsic
        && matches!(fn_output.kind(), TyKind::RawPtr(_, _))
    {
        pointer_intrinsics::dangling(
            tcx,
            &mut instructions,
            fn_output,
            effective_dest,
            method_signature,
        );
    } else if (has_diagnostic_item("ptr_slice_from_raw_parts")
        || has_diagnostic_item("ptr_slice_from_raw_parts_mut")
        || matches!(
            intrinsic_name.as_str(),
            "from_raw_parts" | "from_raw_parts_mut"
        ))
        && is_core_ptr
        && matches!(oomir_output_type, oomir::Type::Slice(_) | oomir::Type::Str)
    {
        pointer_intrinsics::ptr_slice_from_raw_parts(
            tcx,
            instance,
            data_types,
            &label,
            &mut instructions,
            fn_output,
            oomir_output_type,
            oomir_operands,
            effective_dest,
        );
    } else if is_ptr_metadata
        && is_core_ptr
        && (matches!(
            oomir_operands[0].get_type(),
            Some(oomir::Type::Slice(_) | oomir::Type::Str)
        ) || fn_inputs.first().is_some_and(|input| {
            matches!(
                input.kind(),
                TyKind::RawPtr(pointee, _)
                    if pointee.is_slice() || pointee.is_str()
            )
        }))
    {
        if let Some(dest) = effective_dest.clone() {
            instructions.push(oomir::Instruction::GetField {
                dest,
                object: oomir_operands[0].clone(),
                field_name: "rustLength".to_string(),
                field_ty: oomir::Type::U64,
                owner_class: oomir::SLICE_VIEW_CLASS.to_string(),
            });
        }
    } else if is_ptr_metadata
        && is_core_ptr
        && matches!(oomir_operands[0].get_type(), Some(oomir::Type::Pointer(_)))
    {
        pointer_intrinsics::metadata(
            tcx,
            data_types,
            &label,
            &mut instructions,
            fn_inputs,
            oomir_output_type,
            oomir_operands,
            effective_dest,
        );
    } else if matches!(
        intrinsic_name.as_str(),
        "from_raw_parts" | "from_raw_parts_mut"
    ) && is_core_ptr
        && matches!(oomir_output_type, oomir::Type::Pointer(_))
    {
        pointer_metadata::from_raw_parts(
            tcx,
            instance,
            data_types,
            &mut instructions,
            fn_output,
            oomir_operands,
            effective_dest,
            method_signature,
        );
    } else if (is_diagnostic_item(sym::ptr_null) || is_diagnostic_item(sym::ptr_null_mut))
        && (is_external_intrinsic || is_core_crate_item)
        && matches!(fn_output.kind(), TyKind::RawPtr(_, _))
    {
        let TyKind::RawPtr(pointee, _) = fn_output.kind() else {
            unreachable!()
        };
        let pointee_size =
            crate::lower1::types::layout_size_bytes(tcx, *pointee).unwrap_or_else(|error| {
                panic!("could not determine null pointer target size: {error}")
            });
        instructions.push(oomir::Instruction::InvokeStatic {
            dest: effective_dest.clone(),
            class_name: oomir::POINTER_CLASS.to_string(),
            method_name: "nullPointer".to_string(),
            method_ty: oomir::Signature {
                params: vec![("view_size".to_string(), oomir::Type::U64)],
                ret: method_signature.ret.clone(),
                is_static: true,
            },
            args: vec![oomir::Operand::Constant(oomir::Constant::U64(
                u64::try_from(pointee_size).expect("Rust null pointer target layout exceeds u64"),
            ))],
        });
    } else if (has_diagnostic_item("ptr_without_provenance")
        || has_diagnostic_item("ptr_without_provenance_mut")
        || matches!(
            intrinsic_name.as_str(),
            "with_exposed_provenance" | "with_exposed_provenance_mut"
        ))
        && is_external_intrinsic
        && matches!(fn_output.kind(), TyKind::RawPtr(_, _))
    {
        pointer_intrinsics::ptr_without_provenance(
            tcx,
            instance,
            data_types,
            &mut instructions,
            fn_output,
            oomir_operands,
            effective_dest,
            method_signature,
            intrinsic_name,
        );
    } else if (is_diagnostic_item(sym::ptr_read)
        || is_diagnostic_item(sym::ptr_read_unaligned)
        || has_diagnostic_item("ptr_read_volatile")
        || (is_compiler_intrinsic
            && matches!(
                intrinsic_name.as_str(),
                "volatile_load" | "unaligned_volatile_load"
            )))
        && is_core_ptr
    {
        pointer_intrinsics::ptr_read(
            &mut instructions,
            oomir_operands,
            effective_dest,
            intrinsic_name,
            is_compiler_intrinsic,
            has_diagnostic_item,
        );
    } else if (is_diagnostic_item(sym::ptr_write)
        || is_diagnostic_item(sym::ptr_write_unaligned)
        || is_diagnostic_item(sym::ptr_write_volatile)
        || (is_compiler_intrinsic
            && matches!(
                intrinsic_name.as_str(),
                "volatile_store" | "unaligned_volatile_store"
            )))
        && is_core_ptr
    {
        pointer_intrinsics::ptr_write(
            &mut instructions,
            oomir_operands,
            intrinsic_name,
            is_compiler_intrinsic,
            is_diagnostic_item,
        );
    } else if is_diagnostic_item(sym::ptr_replace) && is_core_ptr {
        if let Some(oomir::Type::Pointer(pointee)) = oomir_operands[0].get_type() {
            if let Some(dest) = effective_dest.clone() {
                crate::lower1::place::emit_pointer_read(
                    oomir_operands[0].clone(),
                    pointee.as_ref(),
                    &dest,
                    &mut instructions,
                );
            }
            crate::lower1::place::emit_pointer_write(
                oomir_operands[0].clone(),
                pointee.as_ref(),
                oomir_operands[1].clone(),
                &mut instructions,
            );
        }
    // This private `core` helper has no diagnostic item. It is reached by
    // optimized `ptr::swap_nonoverlapping` for dynamically sized byte counts.
    } else if intrinsic_name.as_str() == "swap_nonoverlapping_bytes"
        && is_core_ptr
        && oomir_operands.len() == 3
    {
        pointer_intrinsics::swap_nonoverlapping_bytes(&mut instructions, oomir_operands);
    } else if (is_diagnostic_item(sym::ptr_swap)
        || (is_compiler_intrinsic && intrinsic_name.as_str() == "typed_swap_nonoverlapping"))
        && is_core_ptr
    {
        pointer_intrinsics::ptr_swap(
            tcx,
            &mut instructions,
            func_instance,
            oomir_operands,
            is_diagnostic_item,
        );
    } else if is_diagnostic_item(sym::ptr_swap_nonoverlapping) && is_core_ptr {
        pointer_intrinsics::ptr_swap_nonoverlapping(
            tcx,
            &label,
            &mut instructions,
            func_instance,
            oomir_operands,
        );
    } else if (is_diagnostic_item(sym::ptr_copy)
        || is_diagnostic_item(sym::ptr_copy_nonoverlapping)
        || is_diagnostic_item(sym::ptr_write_bytes)
        || (is_compiler_intrinsic
            && matches!(
                intrinsic_name.as_str(),
                "copy" | "copy_nonoverlapping" | "write_bytes"
            )))
        && is_core_ptr
    {
        pointer_intrinsics::ptr_copy(
            tcx,
            &label,
            &mut instructions,
            func_instance,
            oomir_operands,
            intrinsic_name,
            is_compiler_intrinsic,
            is_diagnostic_item,
        );
    } else {
        callables::ordinary_call(
            tcx,
            data_types,
            external_interfaces,
            &label,
            &mut instructions,
            func_instance,
            instance_ty,
            fn_inputs,
            oomir_operands,
            effective_dest,
            method_signature,
        );
    }
}
