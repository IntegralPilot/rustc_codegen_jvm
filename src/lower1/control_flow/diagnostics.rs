//! Diagnostics.
use super::*;

pub(in crate::lower1) fn assert_panic_lang_item(
    message: &rustc_middle::mir::AssertMessage<'_>,
) -> Option<LangItem> {
    use rustc_middle::mir::{AssertKind as A, BinOp as B};
    Some(match message {
        A::BoundsCheck { .. } => LangItem::PanicBoundsCheck,
        A::Overflow(op, ..) => match op {
            B::Add => LangItem::PanicAddOverflow,
            B::Sub => LangItem::PanicSubOverflow,
            B::Mul => LangItem::PanicMulOverflow,
            B::Div => LangItem::PanicDivOverflow,
            B::Rem => LangItem::PanicRemOverflow,
            B::Shl => LangItem::PanicShlOverflow,
            B::Shr => LangItem::PanicShrOverflow,
            _ => return None,
        },
        A::OverflowNeg(_) => LangItem::PanicNegOverflow,
        A::DivisionByZero(_) => LangItem::PanicDivZero,
        A::RemainderByZero(_) => LangItem::PanicRemZero,
        _ => return None,
    })
}

pub(in crate::lower1) fn caller_location_operand<'tcx>(
    source_info: SourceInfo,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_prefix: &str,
) -> oomir::Operand {
    let location_ty = tcx.caller_location_ty();
    let location_oomir_ty =
        crate::lower1::types::ty_to_oomir_type(location_ty, tcx, data_types, instance);
    let inherited_location =
        instance
            .def
            .requires_caller_location(tcx)
            .then(|| oomir::Operand::Variable {
                name: oomir::CALLER_LOCATION_PARAM_NAME.to_string(),
                ty: location_oomir_ty,
            });

    mir.caller_location_span(source_info, inherited_location, tcx, |span| {
        // This immutable allocation depends on the resolved source span, not
        // generic arguments. Its carrier was registered above; the first shard
        // also owns any codec declarations produced while decoding it.
        let raw_location = if let Some(value) = data_types.caller_location(span) {
            oomir::Operand::Constant(value)
        } else {
            let value = crate::lower1::operand::handle_const_value(
                None,
                tcx.span_as_caller_location(span),
                &location_ty,
                tcx,
                data_types,
                instance,
            );
            if let oomir::Operand::Constant(value) = &value {
                data_types.remember_caller_location(span, value.clone());
            }
            value
        };
        crate::lower1::value_repr::adapt_operand_to_rust_type(
            raw_location,
            location_ty,
            temp_prefix,
            tcx,
            instance,
            data_types,
            instructions,
        )
    })
}

pub(in crate::lower1) fn emit_panic_lang_item<'tcx>(
    lang_item: LangItem,
    mut args: Vec<oomir::Operand>,
    source_info: SourceInfo,
    tcx: TyCtxt<'tcx>,
    instance: Instance<'tcx>,
    mir: &Body<'tcx>,
    data_types: &mut Definitions<'tcx>,
    instructions: &mut Vec<oomir::Instruction>,
    temp_prefix: &str,
) {
    let panic_def = tcx.require_lang_item(lang_item, source_info.span);
    let panic_instance = Instance::mono(tcx, panic_def);
    args.push(caller_location_operand(
        source_info,
        tcx,
        instance,
        mir,
        data_types,
        instructions,
        temp_prefix,
    ));
    let target = data_types.function_name(tcx, panic_instance);
    let params = args
        .iter()
        .enumerate()
        .map(|(index, arg)| {
            (
                format!("arg{index}"),
                arg.get_type().expect("panic argument must have a JVM type"),
            )
        })
        .collect();
    instructions.push(oomir::Instruction::InvokeStatic {
        dest: None,
        class_name: target
            .class_to_call_on
            .expect("panic lang item must have an owning JVM class"),
        method_name: target.method_name,
        method_ty: oomir::Signature {
            params,
            ret: Box::new(oomir::Type::Void),
            is_static: true,
        },
        args,
    });
    instructions.push(oomir::Instruction::ThrowNewWithMessage {
        exception_class: "java/lang/AssertionError".to_string(),
        message: "Diverging Rust panic call returned unexpectedly".to_string(),
    });
}
