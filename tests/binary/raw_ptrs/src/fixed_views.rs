#[inline(never)]
fn chunk(bytes: &[u8], start: usize) -> u64 {
    let eight: &[u8; 8] = bytes[start..start + 8].try_into().unwrap();
    assert_eq!(
        eight.as_ptr().addr(),
        bytes.as_ptr().wrapping_add(start).addr()
    );
    u64::from_le_bytes(*eight)
}

#[inline(never)]
fn replace_chunk(words: &mut [u32], start: usize) {
    let base = words.as_ptr();
    let four: &mut [u32; 4] = (&mut words[start..start + 4]).try_into().unwrap();
    assert_eq!(four.as_ptr().addr(), base.wrapping_add(start).addr());
    four[1] = 0x1234_5678;
    let back: &mut [u32] = four;
    back[3] = 0xdead_beef;
}

pub fn check() {
    let bytes: Vec<u8> = (0..32).collect();
    for start in 0..24 {
        let expected = (0..8).fold(0, |value, i| value | u64::from(bytes[start + i]) << (i * 8));
        assert_eq!(chunk(&bytes, start), expected);
    }
    let mut words: Vec<u32> = (0..12).collect();
    replace_chunk(&mut words, 3);
    assert_eq!(
        &words[..],
        &[0, 1, 2, 3, 0x1234_5678, 5, 0xdead_beef, 7, 8, 9, 10, 11]
    );
}
