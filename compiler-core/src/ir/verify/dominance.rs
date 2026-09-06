//! Immediate dominators and preorder intervals without recursive graph walks.
use super::*;

/// Iterative immediate dominators and DFS intervals use O(blocks + edges) memory.
pub(super) fn dominance(
    body: &Body,
    predecessors: &[Vec<BlockId>],
    reachable: &[bool],
) -> (Vec<usize>, Vec<usize>) {
    let mut seen = vec![false; body.blocks.len()];
    let mut stack = vec![(body.entry, false)];
    let mut order = Vec::new();
    while let Some((block, exiting)) = stack.pop() {
        if exiting {
            order.push(block);
            continue;
        }
        if std::mem::replace(&mut seen[block.index()], true) {
            continue;
        }
        stack.push((block, true));
        body.blocks[block.index()]
            .terminator
            .unwrap()
            .visit_edges(&body.cases, |edge| {
                stack.push((body.edges[edge.index()].target, false))
            });
    }
    order.reverse();
    let mut ranks = vec![0; body.blocks.len()];
    for (rank, block) in order.iter().enumerate() {
        ranks[block.index()] = rank;
    }
    let mut parent = vec![None; body.blocks.len()];
    parent[body.entry.index()] = Some(body.entry);
    loop {
        let mut changed = false;
        for &block in order.iter().skip(1) {
            let mut incoming = predecessors[block.index()]
                .iter()
                .copied()
                .filter(|p| reachable[p.index()] && parent[p.index()].is_some());
            let Some(mut common) = incoming.next() else {
                continue;
            };
            for mut other in incoming {
                while common != other {
                    while ranks[common.index()] > ranks[other.index()] {
                        common = parent[common.index()].unwrap();
                    }
                    while ranks[other.index()] > ranks[common.index()] {
                        other = parent[other.index()].unwrap();
                    }
                }
            }
            if parent[block.index()] != Some(common) {
                parent[block.index()] = Some(common);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    let mut children = vec![Vec::new(); body.blocks.len()];
    for &block in order.iter().skip(1) {
        children[parent[block.index()].unwrap().index()].push(block);
    }
    let mut pre = vec![0; body.blocks.len()];
    let mut post = pre.clone();
    let mut tick = 0;
    stack.push((body.entry, false));
    while let Some((block, exiting)) = stack.pop() {
        if exiting {
            post[block.index()] = tick;
            continue;
        }
        pre[block.index()] = tick;
        tick += 1;
        stack.push((block, true));
        for &child in &children[block.index()] {
            stack.push((child, false));
        }
    }
    (pre, post)
}
