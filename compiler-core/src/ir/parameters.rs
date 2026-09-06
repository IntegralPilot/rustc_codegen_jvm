use super::*;
use std::collections::VecDeque;

const NONE: u32 = u32::MAX;
struct Parameter {
    block: BlockId,
    index: usize,
    value: ValueId,
}
struct Use {
    user: u32,
    next: u32,
}

/// Only parameters can change identity here. Compact linked lists let us move
/// their dependants to the replacement in O(1), without a Vec per SSA value.
pub(super) fn remove_trivial_parameters(body: &mut Body, predecessors: &[Vec<(BlockId, EdgeId)>]) {
    let params: Vec<_> = body
        .blocks
        .iter()
        .enumerate()
        .filter(|(b, _)| *b != body.entry.index())
        .flat_map(|(b, block)| {
            block
                .params
                .iter()
                .enumerate()
                .map(move |(index, &value)| Parameter {
                    block: BlockId::new(b),
                    index,
                    value,
                })
        })
        .collect();
    if params.is_empty() {
        return;
    }
    let mut numbers = vec![NONE; body.values.len()];
    for (index, param) in params.iter().enumerate() {
        numbers[param.value.index()] = index as u32;
    }
    let mut heads = vec![NONE; params.len()];
    let mut tails = heads.clone();
    let mut uses: Vec<Use> = Vec::new();
    for (index, param) in params.iter().enumerate() {
        for &(_, edge) in &predecessors[param.block.index()] {
            let arg = body.edges[edge.index()].args[param.index];
            let source = numbers[arg.index()];
            if source == NONE || source == index as u32 {
                continue;
            }
            let node = u32::try_from(uses.len()).expect("too many parameter uses");
            assert_ne!(node, NONE);
            uses.push(Use {
                user: index as u32,
                next: NONE,
            });
            if heads[source as usize] == NONE {
                heads[source as usize] = node;
            } else {
                uses[tails[source as usize] as usize].next = node;
            }
            tails[source as usize] = node;
        }
    }
    let mut queue: VecDeque<_> = (0..params.len() as u32).collect();
    let mut queued = vec![true; params.len()];
    while let Some(number) = queue.pop_front() {
        let number = number as usize;
        queued[number] = false;
        let param = &params[number];
        if !matches!(body.values[param.value.index()].def, ValueDef::Param(_)) {
            continue;
        }
        let mut unique = None;
        let mut trivial = true;
        for &(_, edge) in &predecessors[param.block.index()] {
            let arg = body.resolve_mut(body.edges[edge.index()].args[param.index]);
            if arg == param.value {
                continue;
            }
            if unique.is_some_and(|previous| previous != arg) {
                trivial = false;
                break;
            }
            unique = Some(arg);
        }
        if !trivial {
            continue;
        }
        let Some(replacement) = unique else {
            continue;
        };
        body.values[param.value.index()].def = ValueDef::Alias(replacement);
        let head = std::mem::replace(&mut heads[number], NONE);
        let tail = std::mem::replace(&mut tails[number], NONE);
        let mut next = head;
        while next != NONE {
            let node = &uses[next as usize];
            if !std::mem::replace(&mut queued[node.user as usize], true) {
                queue.push_back(node.user);
            }
            next = node.next;
        }
        let target = numbers[replacement.index()];
        if head != NONE && target != NONE {
            let target = target as usize;
            if heads[target] == NONE {
                heads[target] = head;
            } else {
                uses[tails[target] as usize].next = head;
            }
            tails[target] = tail;
        }
    }
    for index in 0..body.values.len() {
        body.resolve_mut(ValueId::new(index));
    }
    // Reuse one mask across blocks and preserve each edge's parameter order.
    let mut keep = Vec::new();
    for (b, incoming) in predecessors.iter().enumerate() {
        keep.clear();
        keep.extend(
            body.blocks[b]
                .params
                .iter()
                .map(|p| matches!(body.values[p.index()].def, ValueDef::Param(_))),
        );
        for &(_, edge) in incoming {
            let mut index = 0;
            body.edges[edge.index()].args.retain(|_| {
                let yes = keep[index];
                index += 1;
                yes
            });
        }
        let mut index = 0;
        body.blocks[b].params.retain(|_| {
            let yes = keep[index];
            index += 1;
            yes
        });
    }
}
