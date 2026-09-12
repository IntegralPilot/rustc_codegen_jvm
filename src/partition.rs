//! Reuse lowering context across small owners without splitting a JVM class.
use std::collections::BTreeMap;

pub(crate) fn batch_owners<T>(
    owners: BTreeMap<String, Vec<T>>,
    limit: usize,
) -> Vec<(String, Vec<T>)> {
    let mut batches: Vec<(String, Vec<T>)> = Vec::new();
    for (owner, items) in owners {
        if let Some((_, batch)) = batches.last_mut()
            && batch.len() + items.len() <= limit
        {
            batch.extend(items);
        } else {
            batches.push((owner, items));
        }
    }
    batches
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn owners_remain_whole_and_large_owners_stand_alone() {
        let owners = [
            ("a", vec![0, 1]),
            ("b", vec![2]),
            ("c", vec![3, 4, 5, 6]),
            ("d", vec![7]),
        ]
        .map(|(owner, items)| (owner.into(), items))
        .into_iter()
        .collect();
        assert_eq!(
            batch_owners(owners, 3),
            vec![
                ("a".into(), vec![0, 1, 2]),
                ("c".into(), vec![3, 4, 5, 6]),
                ("d".into(), vec![7]),
            ]
        );
    }
}
