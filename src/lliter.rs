pub struct LLIterator<'a, T> {
    head: Option<&'a T>,
    next_fn: fn(&T) -> Option<&T>
}

impl<'a, T> Iterator for LLIterator<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<&'a T> {
        let out = self.head;
        if let Some(val) = out {
            self.head = (self.next_fn)(val);
        }
        out
    }
}

pub fn adapt_ll<T>(head: Option<&T>, next_fn: fn(&T) -> Option<&T>) -> LLIterator<T> {
    LLIterator {
        head,
        next_fn
    }
}