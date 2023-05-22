use nom::{
    lib::std::{
        iter::{Copied, Enumerate},
        ops::{Deref, Index, RangeFrom},
        slice::Iter,
    },
    number::Endianness,
    Compare, FindSubstring, InputIter, InputLength, InputTake, Slice,
};

use crate::binary::FormatVersion;

/// The `ParseState` struct contains the information required to manage the
/// parse at any given position in the input.
///
/// # Caveats
///
/// This struct will be cloned or split repeatedly throughout the parse and so
/// should not contain excess state. Code making use of the parse state should
/// ensure that only a few exist at a time.
#[derive(Clone, Debug, PartialEq)]
pub(super) struct ParseState<'a> {
    /// The current position in the input, represented as a slice beginning at
    /// the next byte to be read and ending at the end of input.
    pub input: &'a [u8],

    /// The endianness of the input.
    endianness: Endianness,

    /// The format version of the input.
    format_version: FormatVersion,

    /// The start of the 16-bit data block if the format version includes one,
    /// represented as a slice beginning at the start of the block.
    data_16_bit_start: Option<&'a [u8]>,

    /// The start of the input body, represented as a slice beginning at the
    /// start of the block.
    body: &'a [u8],
}

impl<'a> ParseState<'a> {
    /// Makes a new parse state with the specified input parameters.
    pub fn new(input: &'a [u8], endianness: Endianness, format_version: FormatVersion) -> Self {
        Self {
            input,
            endianness,
            format_version,
            data_16_bit_start: None,
            body: input,
        }
    }

    /// Gets the endianness of the input being read.
    pub fn endianness(&self) -> Endianness {
        self.endianness
    }

    /// Gets the format version of the input being read.
    pub fn format_version(&self) -> FormatVersion {
        self.format_version
    }

    /// Makes a new parse state with the cursor at the specified offset from
    /// the start of the 16-bit data block.
    pub fn new_from_16_bit_offset(&self, offset: u16) -> Self {
        Self {
            input: &self.data_16_bit_start.unwrap()[offset as usize..],
            ..*self
        }
    }

    /// Makes a new parse state with the cursor at the specified offset from
    /// the start of the input body.
    pub fn new_from_32_bit_offset(&self, offset: u32) -> Self {
        Self {
            input: &self.body[offset as usize..],
            ..*self
        }
    }

    /// Sets the start position of the 16-bit data block if the format version
    /// includes one.
    pub fn set_16_bit_block_start(&self, offset: u32) -> Self {
        Self {
            data_16_bit_start: Some(&self.body[offset as usize..]),
            ..*self
        }
    }

    /// Makes a new parse state with all fields specifiable in order to support
    /// specific test cases.
    #[cfg(test)]
    pub fn new_for_testing(
        input: &'a [u8],
        endianness: Endianness,
        format_version: FormatVersion,
        data_16_bit_start: Option<&'a [u8]>,
        body: &'a [u8],
    ) -> Self {
        Self {
            input,
            endianness,
            format_version,
            data_16_bit_start,
            body,
        }
    }
}

/// # nom Input Adapters
///
/// These trait implementations provide access to the internal input bytes for
/// functions originating in [`nom`].

impl Compare<&[u8]> for ParseState<'_> {
    fn compare(&self, t: &[u8]) -> nom::CompareResult {
        self.input.compare(t)
    }

    fn compare_no_case(&self, t: &[u8]) -> nom::CompareResult {
        self.input.compare_no_case(t)
    }
}

impl Compare<&str> for ParseState<'_> {
    fn compare(&self, t: &str) -> nom::CompareResult {
        self.input.compare(t)
    }

    fn compare_no_case(&self, t: &str) -> nom::CompareResult {
        self.input.compare_no_case(t)
    }
}

impl Deref for ParseState<'_> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.input
    }
}

impl FindSubstring<&[u8]> for ParseState<'_> {
    fn find_substring(&self, substr: &[u8]) -> Option<usize> {
        self.input.find_substring(substr)
    }
}

impl Index<usize> for ParseState<'_> {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        self.input.index(index)
    }
}

impl<'a> InputIter for ParseState<'a> {
    type Item = u8;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Copied<Iter<'a, u8>>;

    fn iter_indices(&self) -> Self::Iter {
        self.input.iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.input.iter_elements()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.input.position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.input.slice_index(count)
    }
}

impl<'a> InputLength for ParseState<'a> {
    fn input_len(&self) -> usize {
        self.input.input_len()
    }
}

impl<'a> InputTake for ParseState<'a> {
    fn take(&self, count: usize) -> Self {
        ParseState {
            input: self.input.take(count),
            ..*self
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (suffix, prefix) = self.input.take_split(count);
        (
            ParseState {
                input: suffix,
                ..*self
            },
            ParseState {
                input: prefix,
                ..*self
            },
        )
    }
}

impl Slice<RangeFrom<usize>> for ParseState<'_> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        Self {
            input: self.input.slice(range),
            ..*self
        }
    }
}
