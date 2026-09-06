use super::*;

impl<'tcx> RvalueContext<'_, 'tcx> {
    pub(super) fn lower_arithmetic(
        self,
        rvalue: &Rvalue<'tcx>,
    ) -> (Vec<oomir::Instruction>, oomir::Operand) {
        let Self {
            original_dest_place,
            mir,
            tcx,
            instance,
            data_types,
            ..
        } = self;
        let mut instructions = Vec::new();
        let result_operand;
        let base_temp_name = place_to_string(original_dest_place, tcx);
        match rvalue {
            Rvalue::BinaryOp(bin_op, operands) => {
                let (op1, op2) = operands.as_ref();
                let temp_binop_var = generate_temp_var_name(data_types, &base_temp_name);
                let oomir_op1 =
                    convert_operand(op1, tcx, instance, mir, data_types, &mut instructions);
                let oomir_op2 =
                    convert_operand(op2, tcx, instance, mir, data_types, &mut instructions);
                // Determine result type based on operands or destination hint
                let oomir_result_type =
                    get_place_type(original_dest_place, mir, tcx, instance, data_types);

                match bin_op {
                    BinOp::Offset
                        if matches!(oomir_op1.get_type(), Some(oomir::Type::Pointer(_))) =>
                    {
                        instructions.push(oomir::Instruction::InvokeVirtual {
                            dest: Some(temp_binop_var.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "offset".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("self".to_string(), oomir_op1.get_type().unwrap()),
                                    ("elements".to_string(), oomir::Type::I64),
                                ],
                                ret: Box::new(oomir_result_type.clone()),
                                is_static: false,
                            },
                            args: vec![oomir_op2],
                            operand: oomir_op1,
                        });
                    }
                    BinOp::Eq | BinOp::Ne
                        if matches!(
                            oomir_op1.get_type(),
                            Some(oomir::Type::Slice(_) | oomir::Type::Str)
                        ) =>
                    {
                        instructions.push(oomir::Instruction::InvokeStatic {
                            dest: Some(temp_binop_var.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: "fatPointerEquals".to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    (
                                        "left".to_string(),
                                        oomir::Type::Class("java/lang/Object".to_string()),
                                    ),
                                    (
                                        "right".to_string(),
                                        oomir::Type::Class("java/lang/Object".to_string()),
                                    ),
                                ],
                                ret: Box::new(oomir::Type::Boolean),
                                is_static: true,
                            },
                            args: vec![oomir_op1, oomir_op2],
                        });
                        if matches!(bin_op, BinOp::Ne) {
                            let equality_name = temp_binop_var.clone();
                            let not_name = format!("{temp_binop_var}_not");
                            instructions.push(oomir::Instruction::Not {
                                dest: not_name.clone(),
                                src: oomir::Operand::Variable {
                                    name: equality_name,
                                    ty: oomir::Type::Boolean,
                                },
                            });
                            result_operand = oomir::Operand::Variable {
                                name: not_name,
                                ty: oomir::Type::Boolean,
                            };
                            return (instructions, result_operand);
                        }
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
                        if matches!(oomir_op1.get_type(), Some(oomir::Type::Pointer(_))) =>
                    {
                        let method_name = match bin_op {
                            BinOp::Eq | BinOp::Ne => "samePointer",
                            BinOp::Lt => "lessThan",
                            BinOp::Le => "lessOrEqual",
                            BinOp::Gt => "greaterThan",
                            BinOp::Ge => "greaterOrEqual",
                            _ => unreachable!(),
                        };
                        instructions.push(oomir::Instruction::InvokeVirtual {
                            dest: Some(temp_binop_var.clone()),
                            class_name: oomir::POINTER_CLASS.to_string(),
                            method_name: method_name.to_string(),
                            method_ty: oomir::Signature {
                                params: vec![
                                    ("self".to_string(), oomir_op1.get_type().unwrap()),
                                    ("other".to_string(), oomir_op2.get_type().unwrap()),
                                ],
                                ret: Box::new(oomir::Type::Boolean),
                                is_static: false,
                            },
                            args: vec![oomir_op2],
                            operand: oomir_op1,
                        });
                        if matches!(bin_op, BinOp::Ne) {
                            let equality_name = temp_binop_var.clone();
                            let not_name = format!("{temp_binop_var}_not");
                            instructions.push(oomir::Instruction::Not {
                                dest: not_name.clone(),
                                src: oomir::Operand::Variable {
                                    name: equality_name,
                                    ty: oomir::Type::Boolean,
                                },
                            });
                            result_operand = oomir::Operand::Variable {
                                name: not_name,
                                ty: oomir::Type::Boolean,
                            };
                            return (instructions, result_operand);
                        }
                    }
                    BinOp::Add | BinOp::AddUnchecked => {
                        instructions.push(oomir::Instruction::Binary {
                            op: crate::oomir::BinaryOp::Add,
                            dest: temp_binop_var.clone(),
                            op1: oomir_op1,
                            op2: oomir_op2,
                        })
                    }
                    BinOp::Sub | BinOp::SubUnchecked => {
                        instructions.push(oomir::Instruction::Binary {
                            op: crate::oomir::BinaryOp::Sub,
                            dest: temp_binop_var.clone(),
                            op1: oomir_op1,
                            op2: oomir_op2,
                        })
                    }
                    BinOp::Mul | BinOp::MulUnchecked => {
                        instructions.push(oomir::Instruction::Binary {
                            op: crate::oomir::BinaryOp::Mul,
                            dest: temp_binop_var.clone(),
                            op1: oomir_op1,
                            op2: oomir_op2,
                        })
                    }
                    BinOp::Div => instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::Div,
                        dest: temp_binop_var.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    }),
                    BinOp::Rem => instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::Rem,
                        dest: temp_binop_var.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    }),
                    BinOp::BitAnd => instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::BitAnd,
                        dest: temp_binop_var.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    }),
                    BinOp::BitOr => instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::BitOr,
                        dest: temp_binop_var.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    }),
                    BinOp::BitXor => instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::BitXor,
                        dest: temp_binop_var.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    }),
                    BinOp::Shl | BinOp::ShlUnchecked => {
                        instructions.push(oomir::Instruction::Binary {
                            op: crate::oomir::BinaryOp::Shl,
                            dest: temp_binop_var.clone(),
                            op1: oomir_op1,
                            op2: oomir_op2,
                        })
                    }
                    BinOp::Shr | BinOp::ShrUnchecked => {
                        instructions.push(oomir::Instruction::Binary {
                            op: crate::oomir::BinaryOp::Shr,
                            dest: temp_binop_var.clone(),
                            op1: oomir_op1,
                            op2: oomir_op2,
                        })
                    }
                    BinOp::Eq => instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::Eq,
                        dest: temp_binop_var.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    }),
                    BinOp::Lt => instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::Lt,
                        dest: temp_binop_var.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    }),
                    BinOp::Le => instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::Le,
                        dest: temp_binop_var.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    }),
                    BinOp::Ne => instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::Ne,
                        dest: temp_binop_var.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    }),
                    BinOp::Ge => instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::Ge,
                        dest: temp_binop_var.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    }),
                    BinOp::Gt => instructions.push(oomir::Instruction::Binary {
                        op: crate::oomir::BinaryOp::Gt,
                        dest: temp_binop_var.clone(),
                        op1: oomir_op1,
                        op2: oomir_op2,
                    }),
                    BinOp::Cmp => {
                        let less = format!("{}_less", temp_binop_var);
                        let greater = format!("{}_greater", temp_binop_var);
                        instructions.push(oomir::Instruction::Binary {
                            op: crate::oomir::BinaryOp::Lt,
                            dest: less.clone(),
                            op1: oomir_op1.clone(),
                            op2: oomir_op2.clone(),
                        });
                        instructions.push(oomir::Instruction::Binary {
                            op: crate::oomir::BinaryOp::Gt,
                            dest: greater.clone(),
                            op1: oomir_op1,
                            op2: oomir_op2,
                        });
                        let less_int = format!("{}_less_int", temp_binop_var);
                        let greater_int = format!("{}_greater_int", temp_binop_var);
                        instructions.push(oomir::Instruction::Cast {
                            op: oomir::Operand::Variable {
                                name: less,
                                ty: oomir::Type::Boolean,
                            },
                            ty: oomir::Type::I32,
                            dest: less_int.clone(),
                        });
                        instructions.push(oomir::Instruction::Cast {
                            op: oomir::Operand::Variable {
                                name: greater,
                                ty: oomir::Type::Boolean,
                            },
                            ty: oomir::Type::I32,
                            dest: greater_int.clone(),
                        });
                        instructions.push(oomir::Instruction::Binary {
                            op: crate::oomir::BinaryOp::Sub,
                            dest: temp_binop_var.clone(),
                            op1: oomir::Operand::Variable {
                                name: greater_int,
                                ty: oomir::Type::I32,
                            },
                            op2: oomir::Operand::Variable {
                                name: less_int,
                                ty: oomir::Type::I32,
                            },
                        });
                        result_operand = adapt_simple_enum_operand(
                            oomir::Operand::Variable {
                                name: temp_binop_var,
                                ty: oomir::Type::I32,
                            },
                            &oomir_result_type,
                            &base_temp_name,
                            data_types,
                            &mut instructions,
                        );
                        return (instructions, result_operand);
                    }
                    // Checked ops need special handling as they produce a tuple
                    BinOp::AddWithOverflow | BinOp::SubWithOverflow | BinOp::MulWithOverflow => {
                        // This case needs to return the *tuple* variable, and the instructions
                        // generated inside it are already correct for creating that tuple.

                        // Reuse the logic from the original handle_rvalue for checked ops,
                        // but target the temp_tuple_var instead of the final dest.
                        let (result_mir_ty, _) = {
                            let place_ty = original_dest_place.ty(&mir.local_decls, tcx).ty;
                            if let TyKind::Tuple(elements) = place_ty.kind() {
                                (elements[0], elements[1])
                            } else {
                                panic!("Checked op dest type mismatch")
                            }
                        };
                        let op_oomir_ty =
                            ty_to_oomir_type(result_mir_ty, tcx, data_types, instance);

                        let operation_string = match bin_op {
                            /* ... */ BinOp::AddWithOverflow => "add",
                            BinOp::SubWithOverflow => "sub",
                            BinOp::MulWithOverflow => "mul",
                            _ => unreachable!(),
                        };
                        let tuple_type_name = oomir_result_type
                            .get_class_name()
                            .unwrap_or_else(|| {
                                panic!("checked arithmetic result is not a tuple class")
                            })
                            .to_string();
                        let (
                            checked_instructions,
                            tmp_pair_var,
                            _tmp_result_var,
                            _tmp_overflow_var,
                        ) = emit_checked_arithmetic_oomir_instructions(
                            data_types,
                            &base_temp_name, // Use base temp name for context
                            &oomir_op1,
                            &oomir_op2,
                            &op_oomir_ty,
                            operation_string,
                            instructions.len(),
                            &tuple_type_name,
                        );
                        instructions.extend(checked_instructions);
                        // Return the object as the operand
                        result_operand = oomir::Operand::Variable {
                            name: tmp_pair_var,
                            ty: oomir::Type::Class(tuple_type_name),
                        };
                        return (instructions, result_operand);
                    }
                    _ => {
                        /* Handle Offset, etc. or panic */
                        breadcrumbs::log!(
                            breadcrumbs::LogLevel::Warn,
                            "mir-lowering",
                            format!("Warning: Unhandled binary op {:?}", bin_op)
                        );
                        result_operand = get_placeholder_operand(
                            original_dest_place,
                            mir,
                            tcx,
                            instance,
                            data_types,
                        );
                        // No instruction needed for placeholder
                        return (instructions, result_operand);
                    }
                }
                result_operand = oomir::Operand::Variable {
                    name: temp_binop_var,
                    ty: oomir_result_type, // Use determined result type
                };
            }

            Rvalue::UnaryOp(operation, operand) => {
                let temp_unop_var = generate_temp_var_name(data_types, &base_temp_name);
                let oomir_src_operand =
                    convert_operand(operand, tcx, instance, mir, data_types, &mut instructions);
                let oomir_result_type =
                    get_place_type(original_dest_place, mir, tcx, instance, data_types);
                let mut produced_value = false;

                match operation {
                    UnOp::Not => {
                        instructions.push(oomir::Instruction::Not {
                            dest: temp_unop_var.clone(),
                            src: oomir_src_operand,
                        });
                        produced_value = true;
                    }
                    UnOp::Neg => {
                        instructions.push(oomir::Instruction::Neg {
                            dest: temp_unop_var.clone(),
                            src: oomir_src_operand,
                        });
                        produced_value = true;
                    }
                    UnOp::PtrMetadata => {
                        let operand_ty = operand.ty(&mir.local_decls, tcx);
                        let pointee = match operand_ty.kind() {
                            TyKind::RawPtr(pointee, _) | TyKind::Ref(_, pointee, _) => *pointee,
                            _ => operand_ty,
                        };
                        if pointee.is_slice() || pointee.is_str() {
                            match oomir_src_operand.get_type().unwrap() {
                                oomir::Type::Slice(_) | oomir::Type::Str => {
                                    instructions.push(oomir::Instruction::GetField {
                                        dest: temp_unop_var.clone(),
                                        object: oomir_src_operand,
                                        field_name: "rustLength".into(),
                                        field_ty: oomir::Type::U64,
                                        owner_class: oomir::SLICE_VIEW_CLASS.into(),
                                    });
                                }
                                ty @ oomir::Type::Pointer(_) => {
                                    instructions.push(oomir::Instruction::InvokeVirtual {
                                        dest: Some(temp_unop_var.clone()),
                                        class_name: oomir::POINTER_CLASS.into(),
                                        method_name: "metadata".into(),
                                        method_ty: oomir::Signature {
                                            params: vec![("self".into(), ty)],
                                            ret: Box::new(oomir::Type::U64),
                                            is_static: false,
                                        },
                                        args: vec![],
                                        operand: oomir_src_operand,
                                    });
                                }
                                _ => {
                                    let length_i32 = format!("{temp_unop_var}_i32");
                                    instructions.push(oomir::Instruction::Length {
                                        dest: length_i32.clone(),
                                        array: oomir_src_operand,
                                    });
                                    instructions.push(oomir::Instruction::Cast {
                                        dest: temp_unop_var.clone(),
                                        op: oomir::Operand::Variable {
                                            name: length_i32,
                                            ty: oomir::Type::I32,
                                        },
                                        ty: oomir::Type::U64,
                                    });
                                }
                            }
                            produced_value = true;
                        } else if matches!(pointee.kind(), TyKind::Dynamic(..))
                            && matches!(oomir_src_operand.get_type(), Some(oomir::Type::Pointer(_)))
                        {
                            crate::lower1::control_flow::emit_trait_object_metadata(
                                oomir_src_operand,
                                &oomir_result_type,
                                temp_unop_var.clone(),
                                &format!("{base_temp_name}_trait_metadata"),
                                data_types,
                                &mut instructions,
                            );
                            produced_value = true;
                        } else {
                            let tail = tcx
                                .struct_tail_for_codegen(pointee, TypingEnv::fully_monomorphized());
                            if (tail.is_slice() || tail.is_str())
                                && matches!(
                                    oomir_src_operand.get_type(),
                                    Some(oomir::Type::Pointer(_))
                                )
                            {
                                instructions.push(oomir::Instruction::InvokeVirtual {
                                    dest: Some(temp_unop_var.clone()),
                                    class_name: oomir::POINTER_CLASS.to_string(),
                                    method_name: "metadata".to_string(),
                                    method_ty: oomir::Signature {
                                        params: vec![(
                                            "self".to_string(),
                                            oomir_src_operand
                                                .get_type()
                                                .expect("DST metadata pointer must be typed"),
                                        )],
                                        ret: Box::new(oomir::Type::U64),
                                        is_static: false,
                                    },
                                    args: Vec::new(),
                                    operand: oomir_src_operand,
                                });
                                produced_value = true;
                            }
                        }
                    }
                }

                if produced_value {
                    result_operand = oomir::Operand::Variable {
                        name: temp_unop_var,
                        ty: oomir_result_type,
                    };
                } else if !oomir_result_type.has_jvm_value() {
                    result_operand = oomir::Operand::Constant(oomir::Constant::Unit);
                } else {
                    result_operand = get_placeholder_operand(
                        original_dest_place,
                        mir,
                        tcx,
                        instance,
                        data_types,
                    );
                }
            }

            _ => unreachable!("rvalue routed to arithmetic"),
        }
        (instructions, result_operand)
    }
}
