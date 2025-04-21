/// Binary search for u32 slices.
/// Returns `Some(index)` if `target` is found, or `None` otherwise.
pub fn binary_search(slice: &[u32], target: u32) -> Option<usize> {
    let mut low = 0;
    let mut high = slice.len();

    while low < high {
        // mid = floor((low + high) / 2) without overflow
        let mid = low + (high - low) / 2;
        let v = slice[mid];
        if v < target {
            low = mid + 1;
        } else if v > target {
            high = mid;
        } else {
            return Some(mid);
        }
    }

    None
}

fn main() {
    // demo array (must be sorted!)
    let arr = [1, 2, 3, 5, 8, 13, 21];

    // successful searches
    assert!(binary_search(&arr, 1)  == Some(0));
    assert!(binary_search(&arr, 5)  == Some(3));
    assert!(binary_search(&arr, 21) == Some(6));

    // unsuccessful searches
    assert!(binary_search(&arr, 0).is_none());
    assert!(binary_search(&arr, 4).is_none());
    assert!(binary_search(&arr, 22).is_none());
}