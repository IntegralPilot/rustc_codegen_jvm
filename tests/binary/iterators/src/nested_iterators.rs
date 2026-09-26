fn page_iter(page: &[u32]) -> impl DoubleEndedIterator<Item = u32> + '_ {
    page.iter()
        .enumerate()
        .filter(|(_, elem)| **elem != 0)
        .flat_map(|(i, elem)| (0..*elem).map(move |n| i as u32 + n))
}

// Drop lowering must preserve the higher-ranked signature of the filter's
// closure when resolving the nested iterator's IntoIterator::IntoIter type.
fn sum(page: &[u32], partial: bool) -> u32 {
    let first = Some(page)
        .filter(|_| partial)
        .into_iter()
        .flat_map(|page| page_iter(page).map(|n| n + 64));
    first.chain(page_iter(page)).sum()
}

pub fn run() {
    assert_eq!(sum(&[0, 2, 1], false), 5);
    assert_eq!(sum(&[0, 2, 1], true), 202);
    assert_eq!(sum(&[], true), 0);
}
