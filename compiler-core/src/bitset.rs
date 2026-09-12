//! Dense block-boundary facts shared by compiler analyses.
#[derive(Debug)]
pub struct BitMatrix {
    pub(crate) words_per_row: usize,
    pub(crate) words: Vec<u64>,
}

impl BitMatrix {
    pub fn new(rows: usize, local_count: usize) -> Self {
        let words_per_row = local_count.div_ceil(u64::BITS as usize);
        Self {
            words_per_row,
            words: vec![
                0;
                rows.checked_mul(words_per_row)
                    .expect("bit matrix capacity overflow")
            ],
        }
    }

    pub fn row(&self, index: usize) -> &[u64] {
        let start = index * self.words_per_row;
        &self.words[start..start + self.words_per_row]
    }

    pub fn row_mut(&mut self, index: usize) -> &mut [u64] {
        let start = index * self.words_per_row;
        &mut self.words[start..start + self.words_per_row]
    }

    pub fn contains(&self, row: usize, local: usize) -> bool {
        self.row(row)
            .get(local / u64::BITS as usize)
            .is_some_and(|word| word & (1 << (local % u64::BITS as usize)) != 0)
    }

    pub fn iter(&self, row: usize) -> BitIter<'_> {
        BitIter {
            words: self.row(row),
            word_index: 0,
            remaining: self.row(row).first().copied().unwrap_or(0),
        }
    }
}

pub struct BitIter<'a> {
    words: &'a [u64],
    word_index: usize,
    remaining: u64,
}

impl Iterator for BitIter<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.remaining != 0 {
                let bit = self.remaining.trailing_zeros() as usize;
                self.remaining &= self.remaining - 1;
                return Some(self.word_index * u64::BITS as usize + bit);
            }
            self.word_index += 1;
            self.remaining = *self.words.get(self.word_index)?;
        }
    }
}
