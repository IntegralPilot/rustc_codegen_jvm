use super::*;

#[test]
fn definite_initialization_intersects_branches_and_loop_backedges() {
    let local = Local::from_usize;
    let domain = Domain::new(10_000, [local(1), local(999), local(64)]);
    let flow = MirControlFlow {
        predecessors: [
            vec![],
            vec![0],
            vec![0],
            vec![1, 2, 4, 5],
            vec![3],
            vec![],
            vec![3],
        ]
        .into_iter()
        .map(|p| p.into_iter().map(BasicBlock::from_usize).collect())
        .collect(),
        reachable: vec![true, true, true, true, true, false, true],
    };
    let mut generated = vec![domain.empty(); flow.reachable.len()];
    let mut killed = generated.clone();
    let mut entry = domain.empty();
    entry.insert(domain.slot(local(1)).unwrap());
    for block in [1, 2] {
        generated[block].insert(domain.slot(local(999)).unwrap());
    }
    generated[4].insert(domain.slot(local(64)).unwrap());
    // A storage lifetime ends on one branch and on the loop's backedge.
    killed[2].insert(domain.slot(local(1)).unwrap());
    killed[4].insert(domain.slot(local(999)).unwrap());
    let available = flow.initialized(&domain, &generated, Some(&killed), entry.clone());
    assert_eq!(available[1].locals(&domain).collect::<Vec<_>>(), [local(1)]);
    for block in [3, 4, 5, 6] {
        assert_eq!(available[block].locals(&domain).count(), 0);
    }
    let available = flow.initialized(&domain, &generated, None, entry);
    for block in [3, 4, 6] {
        assert_eq!(
            available[block].locals(&domain).collect::<Vec<_>>(),
            [local(1), local(999)]
        );
    }
}

#[test]
fn sparse_local_numbers_do_not_expand_per_block_state() {
    let locals = (0..65)
        .map(|i| Local::from_usize(9_000 - i * 17))
        .collect::<Vec<_>>();
    let domain = Domain::new(10_000, locals.iter().copied().chain(locals.iter().copied()));
    assert_eq!(domain.all().locals(&domain).collect::<Vec<_>>(), locals);
    assert_eq!(domain.empty().0.len(), 2);
    assert!(domain.slot(Local::from_usize(8)).is_none());
    let domain = Domain::new(10_000, []);
    assert!(domain.slots.is_empty());
    assert!(domain.empty().0.is_empty());
}
