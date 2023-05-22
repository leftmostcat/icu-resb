pub(self) use nom::{number::Endianness, IResult};

pub(self) use crate::binary::{
    reader::{parse_error::ParseError, parse_state::ParseState},
    FormatVersion,
};

mod explicit_1_word_length {
    use super::*;

    use crate::binary::reader::explicit_1_word_length;

    // Wrap the parser in a prepared parse state.
    fn parser(input: &[u8]) -> IResult<ParseState, u32, ParseError<ParseState>> {
        let parse_state =
            ParseState::new_for_testing(input, Endianness::Little, FormatVersion::V2_0, None, &[]);

        explicit_1_word_length(parse_state)
    }

    #[test]
    fn values_in_bounds() {
        let verify_success = |word: u16, expected| {
            let bytes = word.to_le_bytes();
            let result = parser(&bytes);
            assert!(result.is_ok(), "Unexpected parser failure.");

            let (_, value) = result.unwrap();
            assert_eq!(
                value, expected,
                "Expected parser to return {expected} for {word:04x}; got {value}."
            );
        };

        // Verify the minimum and maximum bounds and their correct values.
        verify_success(0xdc00, 0);
        verify_success(0xdfee, 0x3ee);

        // Verify that values with the least significant of a nibble set
        // aren't truncated.
        verify_success(0xddf2, 0x1f2);
        verify_success(0xdebf, 0x2bf);
    }

    #[test]
    fn values_out_of_bounds() {
        let verify_error = |word: u16| {
            let bytes = word.to_le_bytes();
            let result = parser(&bytes);
            assert!(result.is_err(), "Expected parser not to match.");

            match result {
                Err(nom::Err::Error(_)) => (),
                _ => panic!("Expected non-fatal parse error."),
            }
        };

        // Verify that edge cases do not match.
        verify_error(0xdbff);
        verify_error(0xdfef);

        // Verify that values outside of the surrogate range do not match.
        verify_error(0xe27b);
    }
}
