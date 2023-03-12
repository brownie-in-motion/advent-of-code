pub struct Chunks<I: Iterator> {
    iter: I,
    chunk: usize,
}

impl<I: Iterator> Iterator for Chunks<I> {
    type Item = Vec<I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        (0..self.chunk)
            .map(|_| self.iter.next())
            .into_iter()
            .collect::<Option<Vec<_>>>()
    }
}

pub trait Itertools: Sized + Iterator {
    fn chunked(self, chunk: usize) -> Chunks<Self> {
        Chunks { iter: self, chunk }
    }
}

impl<I> Itertools for I where I: Iterator {}
