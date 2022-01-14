use core::slice;
use std::ops::Deref;

use memchr::Memchr;

// Check out https://github.com/fflorent/nom_locate for the original, generic version
#[derive(Debug, Clone, Copy)]
pub(crate) struct Span<'a> {
    offset: usize,
    line: u32,
    fragment: &'a str,
}

impl<'a> Deref for Span<'a> {
    type Target = &'a str;

    fn deref(&self) -> &Self::Target {
        &self.fragment
    }
}

impl<'a> Span<'a> {
    pub(crate) fn new(source: &'a str) -> Span {
        Span {
            offset: 0,
            line: 1,
            fragment: source,
        }
    }

    pub(crate) fn slice<I>(&self, index: I) -> Span
    where
        I: core::slice::SliceIndex<str, Output = str>,
    {
        let next_fragment = &self.fragment[index];
        let consumed_len = (next_fragment.as_ptr() as usize) - (self.fragment.as_ptr() as usize);
        if consumed_len == 0 {
            return Span {
                offset: self.offset,
                line: self.line,
                fragment: next_fragment,
            };
        }
        let consumed = &self.fragment[..consumed_len];
        let next_offset = self.offset + consumed_len;
        let consumed_as_bytes = consumed.as_bytes();
        let iter = Memchr::new(b'\n', consumed_as_bytes);
        let number_of_lines = iter.count() as u32;
        let next_line = self.line + number_of_lines;
        Span {
            offset: next_offset,
            line: next_line,
            fragment: next_fragment,
        }
    }

    fn get_unoffsetted_slice(&self) -> &str {
        let ptr = self.fragment.as_ptr();
        unsafe {
            let original_ptr = ptr.offset(-(self.offset as isize));
            std::str::from_utf8_unchecked(slice::from_raw_parts(
                original_ptr,
                self.offset + self.fragment.len(),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use super::Span;

    #[test]
    fn test_unoffsetted_slice() {
        let source = "lorém\t ipsüm\n வணக்கம் உலகம்\n\n\nこんにちは世界";
        let span = Span::new(source);

        for (i, _) in source.char_indices() {
            let new_span = span.slice(i..);
            assert_eq!(*new_span.deref(), &source[i..]);

            let unoffsetted = new_span.get_unoffsetted_slice();
            assert_eq!(unoffsetted, source);
        }
    }
}
