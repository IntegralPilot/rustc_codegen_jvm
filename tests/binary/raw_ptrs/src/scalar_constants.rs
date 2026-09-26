use core::{num::NonZeroUsize, ptr::NonNull};

const DANGLING: NonNull<u8> = NonNull::dangling();
const ALIGNED: NonNull<u64> = NonNull::dangling();
const ADDRESS: NonNull<u8> = NonNull::without_provenance(NonZeroUsize::new(0x1234).unwrap());

pub fn run() {
    // NonNull's scalar field is a pattern type. Its nonzero restriction must
    // not make constant decoding lose the underlying pointer representation.
    assert_eq!(std::hint::black_box(DANGLING).as_ptr().addr(), 1);
    assert_eq!(std::hint::black_box(ALIGNED).as_ptr().addr(), 8);
    assert_eq!(std::hint::black_box(ADDRESS).as_ptr().addr(), 0x1234);
}
