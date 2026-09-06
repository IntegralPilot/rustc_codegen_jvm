use super::*;

#[test]
fn debug_mirrors_keep_dead_values_and_hide_uninitialized_join_bindings() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let wide = types.scalar(ScalarType::I64);
    let boolean = types.scalar(ScalarType::Bool);
    let mut b = Builder::new(&types, int);
    let entry = b.current();
    let condition = b.parameter(entry, boolean);
    let parameter = b.parameter(entry, wide);
    let yes = b.create_block();
    let join = b.create_block();
    let mut debug = DebugInfo {
        locals: vec![DebugLocal::Value(int), DebugLocal::Value(wide)],
        variables: vec![
            DebugVariable {
                name: "conditional".into(),
                local: 0,
            },
            DebugVariable {
                name: "wide".into(),
                local: 1,
            },
        ],
        scopes: vec![vec![0, 1], vec![1]],
        ..Default::default()
    };
    debug.push(
        &b,
        DebugChange::Set {
            local: 1,
            value: parameter,
        },
    );
    debug.push(&b, DebugChange::Scope(0));
    b.branch(condition, yes, join);
    b.switch_to(yes);
    debug.push(&b, DebugChange::Scope(0));
    let seven = b.constant(int, Scalar::integer(ScalarType::I32, 7).unwrap());
    debug.push(
        &b,
        DebugChange::Set {
            local: 0,
            value: seven,
        },
    );
    b.jump(join, vec![]);
    b.switch_to(join);
    debug.push(&b, DebugChange::Scope(0));
    let zero = b.constant(int, Scalar::integer(ScalarType::I32, 0).unwrap());
    b.terminate(Terminator::Return(Some(zero)));
    let body = b.finish().unwrap();
    let ordinary = crate::opt::live(&body, &types);
    let with_debug = crate::opt::live_with_roots(&body, &types, debug.roots(&body));
    assert!(!ordinary.values[seven.index()]);
    assert!(with_debug.values[seven.index()]);
    let mut cp = InternedConstantPool::default();
    let code = compile_with_options(
        &body,
        &types,
        &mut cp,
        Options {
            debug: Some(&debug),
            ..Default::default()
        },
    )
    .unwrap();
    let vars = code
        .attributes
        .iter()
        .find_map(|attribute| {
            if let jvm::attributes::Attribute::LocalVariableTable { variables, .. } = attribute {
                Some(variables)
            } else {
                None
            }
        })
        .unwrap();
    let conditional = vars
        .iter()
        .filter(|v| cp.try_get_utf8(v.name_index).unwrap() == "conditional")
        .collect::<Vec<_>>();
    assert_eq!(conditional.len(), 1);
    let byte_offsets =
        super::super::encoding::instruction_byte_offsets(&code.instructions).unwrap();
    // The conditionally initialized name is visible only in the assigning arm,
    // never in the join reached by the arm that did not initialize it.
    let range = conditional[0];
    assert!(((range.start_pc + range.length) as usize) < byte_offsets[code.instructions.len() - 1]);
    let wide_vars = vars
        .iter()
        .filter(|v| cp.try_get_utf8(v.name_index).unwrap() == "wide")
        .collect::<Vec<_>>();
    assert!(!wide_vars.is_empty());
    assert!(wide_vars.iter().all(|v| v.index + 1 < code.max_locals));
}

#[test]
fn verifier_checks_debug_types_and_definition_points() {
    let mut types = Types::default();
    let int = types.scalar(ScalarType::I32);
    let mut b = Builder::new(&types, int);
    let value = b.constant(int, Scalar::integer(ScalarType::I32, 7).unwrap());
    b.terminate(Terminator::Return(Some(value)));
    let body = b.finish().unwrap();
    let mut debug = DebugInfo {
        locals: vec![DebugLocal::Value(int)],
        events: vec![DebugEvent {
            block: body.entry,
            position: 0,
            change: DebugChange::Set { local: 0, value },
            line: None,
        }],
        ..Default::default()
    };
    assert!(verify_with_debug(&body, &types, Some(&debug)).is_err());
    debug.events[0].position = 1;
    verify_with_debug(&body, &types, Some(&debug)).unwrap();
    debug.events[0].change = DebugChange::Set { local: 4, value };
    assert!(verify_with_debug(&body, &types, Some(&debug)).is_err());
}

#[test]
fn zero_sized_debug_bindings_require_no_jvm_slot() {
    let mut types = Types::default();
    let unit = types.intern(Type::Unit);
    let mut builder = Builder::new(&types, unit);
    builder.terminate(Terminator::Return(None));
    let body = builder.finish().unwrap();
    let debug = DebugInfo {
        locals: vec![DebugLocal::Value(unit)],
        variables: vec![DebugVariable {
            name: "unit".into(),
            local: 0,
        }],
        ..Default::default()
    };
    let code = compile_with_options(
        &body,
        &types,
        &mut Default::default(),
        Options {
            debug: Some(&debug),
            ..Default::default()
        },
    )
    .unwrap();
    assert_eq!(code.max_locals, 0);
    assert!(code.attributes.is_empty());
}
