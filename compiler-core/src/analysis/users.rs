//! Store reverse value dependencies in two flat allocations.
//! Representation passes share this graph instead of per-value vectors.
use crate::ir::ValueId;

const END: u32 = u32::MAX;

pub(crate) struct ValueUsers {
    heads: Vec<u32>,
    edges: Vec<(u32, u32)>,
}

impl ValueUsers {
    pub fn new(count: usize) -> Self {
        Self {
            heads: vec![END; count],
            edges: Vec::new(),
        }
    }

    pub fn connect(&mut self, source: ValueId, user: usize) {
        let next = self.heads[source.index()];
        self.heads[source.index()] = u32::try_from(self.edges.len()).expect("too many value uses");
        self.edges
            .push((next, u32::try_from(user).expect("too many values")));
    }

    pub fn users(&self, source: usize) -> impl Iterator<Item = usize> + '_ {
        let mut next = self.heads[source];
        std::iter::from_fn(move || {
            if next == END {
                return None;
            }
            let (following, user) = self.edges[next as usize];
            next = following;
            Some(user as usize)
        })
    }

    /// Reject a candidate if any input is ineligible.
    /// Queue each rejected value once, including values in cycles.
    pub fn close(&self, eligible: &mut [bool]) {
        let mut pending = eligible
            .iter()
            .enumerate()
            .filter_map(|(index, &yes)| (!yes).then_some(index as u32))
            .collect::<Vec<_>>();
        while let Some(index) = pending.pop() {
            for user in self.users(index as usize) {
                if std::mem::replace(&mut eligible[user], false) {
                    pending.push(user as u32);
                }
            }
        }
    }

    /// Propagate proven identity aliases. Joins retain their own components.
    /// They must not reuse the components of one input.
    pub fn propagate<T: Copy>(&self, eligible: &[bool], known: &mut [Option<T>]) {
        let mut pending = known
            .iter()
            .enumerate()
            .filter_map(|(index, value)| value.is_some().then_some(index as u32))
            .collect::<Vec<_>>();
        while let Some(index) = pending.pop() {
            for user in self.users(index as usize) {
                if eligible[user] && known[user].is_none() {
                    known[user] = known[index as usize];
                    pending.push(user as u32);
                }
            }
        }
    }
}
