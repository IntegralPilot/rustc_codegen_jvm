//! Bypass empty traps on undefined paths.
//! Keep preceding instructions because they can throw or unwind.
use crate::ir::*;

fn impossible(body: &Body, edge: EdgeId) -> bool {
    let block = &body.blocks[body.edges[edge.index()].target.index()];
    block.terminator == Some(Terminator::Unreachable)
        && block
            .instructions
            .iter()
            .all(|i| body.instructions[i.index()].op == Op::Nop)
}

pub(super) fn simplify(body: &mut Body, term: Terminator) -> Option<Terminator> {
    match term {
        Terminator::Branch { yes, no, .. } => match (impossible(body, yes), impossible(body, no)) {
            (true, true) => Some(Terminator::Unreachable),
            (true, false) => Some(Terminator::Jump(no)),
            (false, true) => Some(Terminator::Jump(yes)),
            _ => None,
        },
        Terminator::Switch {
            value,
            cases,
            otherwise,
        } => {
            let targets = &body.cases[cases.range()];
            let default = if impossible(body, otherwise) {
                let Some((_, edge)) = targets.iter().find(|(_, edge)| !impossible(body, *edge))
                else {
                    return Some(Terminator::Unreachable);
                };
                *edge
            } else {
                otherwise
            };
            if default == otherwise
                && !targets
                    .iter()
                    .any(|&(_, edge)| edge == default || impossible(body, edge))
            {
                return None;
            }
            let retained = targets
                .iter()
                .copied()
                .filter(|&(_, edge)| edge != default && !impossible(body, edge))
                .collect::<Vec<_>>();
            if retained.is_empty() {
                return Some(Terminator::Jump(default));
            }
            let cases = List {
                start: body.cases.len().try_into().expect("switch pool capacity"),
                len: retained.len().try_into().expect("switch capacity"),
            };
            body.cases.extend(retained);
            Some(Terminator::Switch {
                value,
                cases,
                otherwise: default,
            })
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scalar::{BinaryOp, Scalar, ScalarType};

    #[test]
    fn impossible_edges_disappear_without_losing_the_returned_value() {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let bool_ = types.scalar(ScalarType::Bool);
        for switch in [false, true] {
            let mut b = Builder::new(&types, int);
            let condition = b.parameter(b.current(), if switch { int } else { bool_ });
            let value = b.parameter(b.current(), int);
            let valid = b.create_block();
            let result = b.parameter(valid, int);
            let invalid = b.create_block();
            let yes = b.edge(valid, vec![value]);
            let no = b.edge(invalid, vec![]);
            let cases = List { start: 0, len: 1 };
            b.body
                .cases
                .push((Scalar::integer(ScalarType::I32, 17).unwrap(), yes));
            b.terminate(if switch {
                Terminator::Switch {
                    value: condition,
                    cases,
                    otherwise: no,
                }
            } else {
                Terminator::Branch { condition, yes, no }
            });
            b.switch_to(valid);
            b.terminate(Terminator::Return(Some(result)));
            b.switch_to(invalid);
            b.terminate(Terminator::Unreachable);
            let mut body = b.finish().unwrap();
            super::super::simplify_components(&mut body, &types);
            assert_eq!(
                body.blocks[body.entry.index()].terminator,
                Some(Terminator::Jump(yes))
            );
            assert_eq!(body.resolve(result), body.resolve(value));
            assert!(!body.reachable()[invalid.index()]);
            verify(&body, &types).unwrap();
        }
    }

    #[test]
    fn work_before_unreachable_may_diverge_so_the_edge_must_remain() {
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let bool_ = types.scalar(ScalarType::Bool);
        let mut b = Builder::new(&types, int);
        let condition = b.parameter(b.current(), bool_);
        let value = b.parameter(b.current(), int);
        let divisor = b.parameter(b.current(), int);
        let valid = b.create_block();
        let invalid = b.create_block();
        b.branch(condition, valid, invalid);
        b.switch_to(valid);
        b.terminate(Terminator::Return(Some(value)));
        b.switch_to(invalid);
        b.emit(
            Op::Binary {
                op: BinaryOp::Div,
                left: value,
                right: divisor,
            },
            Some(int),
        );
        b.terminate(Terminator::Unreachable);
        let mut body = b.finish().unwrap();
        super::super::simplify_components(&mut body, &types);
        assert!(matches!(
            body.blocks[body.entry.index()].terminator,
            Some(Terminator::Branch { .. })
        ));
        assert!(body.reachable()[invalid.index()]);
        verify(&body, &types).unwrap();
    }
    #[test]
    fn simplified_switch_preserves_distinct_arguments_on_the_jvm() {
        use crate::classfile::{
            ClassAccessFlags, ClassFile, Method, MethodAccessFlags, Version, attributes::Attribute,
            constant_pool::InternedConstantPool,
        };
        let mut types = Types::default();
        let int = types.scalar(ScalarType::I32);
        let mut b = Builder::new(&types, int);
        let condition = b.parameter(b.current(), int);
        let first = b.parameter(b.current(), int);
        let second = b.parameter(b.current(), int);
        let join = b.create_block();
        let result = b.parameter(join, int);
        let invalid = b.create_block();
        let a = b.edge(join, vec![first]);
        let c = b.edge(join, vec![second]);
        let ub = b.edge(invalid, vec![]);
        let invalid_case = b.edge(invalid, vec![]);
        for (key, edge) in [(17, a), (23, c), (99, invalid_case)] {
            b.body
                .cases
                .push((Scalar::integer(ScalarType::I32, key).unwrap(), edge));
        }
        b.terminate(Terminator::Switch {
            value: condition,
            cases: List { start: 0, len: 3 },
            otherwise: ub,
        });
        b.switch_to(join);
        b.terminate(Terminator::Return(Some(result)));
        b.switch_to(invalid);
        b.terminate(Terminator::Unreachable);
        let mut body = b.finish().unwrap();
        super::super::simplify_components(&mut body, &types);
        let Some(Terminator::Switch {
            cases, otherwise, ..
        }) = body.blocks[body.entry.index()].terminator
        else {
            panic!("lost switch");
        };
        assert_eq!(cases.len, 1);
        assert_eq!(otherwise, a);
        assert_eq!(body.edges[a.index()].args, vec![first]);
        assert_eq!(body.edges[c.index()].args, vec![second]);
        assert!(!body.reachable()[invalid.index()]);
        verify(&body, &types).unwrap();
        let mut cp = InternedConstantPool::default();
        let this_class = cp.add_class("SwitchUB").unwrap();
        let super_class = cp.add_class("java/lang/Object").unwrap();
        let code = crate::jvm::select::compile(&body, &types, &mut cp).unwrap();
        let method = Method {
            access_flags: MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            name_index: cp.add_utf8("eval").unwrap(),
            descriptor_index: cp.add_utf8("(III)I").unwrap(),
            attributes: vec![Attribute::Code {
                name_index: cp.add_utf8("Code").unwrap(),
                max_stack: code.max_stack,
                max_locals: code.max_locals,
                code: code.instructions,
                exception_table: code.exceptions,
                attributes: code.attributes,
            }],
        };
        let class = ClassFile {
            version: Version::Java8 { minor: 0 },
            constant_pool: cp.into_inner(),
            access_flags: ClassAccessFlags::PUBLIC | ClassAccessFlags::SUPER,
            this_class,
            super_class,
            methods: vec![method],
            ..Default::default()
        };
        let mut bytes = Vec::new();
        class.to_bytes(&mut bytes).unwrap();
        let dir =
            std::env::temp_dir().join(format!("rcj-unreachable-switch-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("SwitchUB.class"), bytes).unwrap();
        std::fs::write(
            dir.join("Run.java"),
            r#"
public class Run {
    public static void main(String[] args) {
        for (int i = -100; i <= 100; ++i) {
            if (SwitchUB.eval(17, i, i + 1) != i || SwitchUB.eval(23, i, i + 1) != i + 1)
                throw new AssertionError("switch argument");
        }
    }
}"#,
        )
        .unwrap();
        for (program, args) in [
            ("javac", vec!["-cp", ".", "Run.java"]),
            ("java", vec!["-Xverify:all", "-cp", ".", "Run"]),
        ] {
            let output = std::process::Command::new(program)
                .args(args)
                .current_dir(&dir)
                .output()
                .unwrap();
            assert!(
                output.status.success(),
                "{}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
        std::fs::remove_dir_all(dir).unwrap();
    }
}
