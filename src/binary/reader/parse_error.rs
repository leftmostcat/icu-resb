use nom::{
    error::{ContextError, ErrorKind, FromExternalError},
    lib::std::{fmt::Debug, fmt::Write, ops::Deref},
    Offset,
};

const BYTES_PER_LINE: usize = 26;

/// Converts an internal parse error type to a more user-readable string.
///
/// This includes pretty-printing of surrounding bytes and offset into the
/// input.
pub(super) fn convert_error<I: Deref<Target = [u8]>>(input: &[u8], error: ParseError<I>) -> String {
    let mut result = String::new();

    for (i, (subinput, kind)) in error.errors.iter().enumerate() {
        if input.is_empty() {
            match kind {
                ParseErrorKind::Context(s) => {
                    write!(&mut result, "{}: in {}, got empty input\n\n", i, s)
                }
                ParseErrorKind::Nom(e) => {
                    write!(&mut result, "{}: in {:?}, got empty input\n\n", i, e)
                }
                ParseErrorKind::External(e, s) => {
                    write!(
                        &mut result,
                        "{}: in {:?}, got empty input and error: {}\n\n",
                        i, e, s
                    )
                }
            }
        } else {
            // Determine the position of the failed input within the complete
            // input.
            let offset = input.offset(subinput);

            // Locate the indicated position within the input and pretty-print
            // the surrounding characters.
            let chunk_containing_offset = offset / BYTES_PER_LINE;
            let chunk = input
                .chunks(BYTES_PER_LINE)
                .nth(chunk_containing_offset)
                .unwrap();
            let column = (offset % BYTES_PER_LINE) * 3 + 1;

            let pretty_chunk = chunk
                .iter()
                .map(|&value| format!("{:02x} ", value))
                .collect::<String>()
                .trim()
                .to_string();

            match kind {
                ParseErrorKind::Context(s) => write!(
                    &mut result,
                    "{input}: at byte 0x{offset:x}, in {context}\n\
                       {caret:>column$}\n\n",
                    input = pretty_chunk,
                    context = s,
                    caret = '^',
                    column = column,
                ),
                ParseErrorKind::Nom(e) => write!(
                    &mut result,
                    "{input}: at byte 0x{offset:x}, in {nom_err:?}\n\
                       {caret:>column$}\n\n",
                    input = pretty_chunk,
                    nom_err = e,
                    caret = '^',
                    column = column,
                ),
                ParseErrorKind::External(e, s) => write!(
                    &mut result,
                    "{input}: {err} at byte 0x{offset:x}, in {nom_err:?}\n\
                       {caret:>column$}\n\n",
                    input = pretty_chunk,
                    nom_err = e,
                    err = s,
                    caret = '^',
                    column = column,
                ),
            }
        }
        .unwrap()
    }

    result
}

#[derive(Debug)]
enum ParseErrorKind {
    Context(&'static str),
    Nom(ErrorKind),
    External(ErrorKind, String),
}

/// The `ParseError` struct aggregates errors which occur in the parse, allowing
/// for later conversion to a user-readable string via [`convert_error`].
#[derive(Debug)]
pub(in crate::binary::reader) struct ParseError<I> {
    errors: Vec<(I, ParseErrorKind)>,
}

impl<I> ContextError<I> for ParseError<I> {
    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push((input, ParseErrorKind::Context(ctx)));
        other
    }
}

impl<I, E: Debug> FromExternalError<I, E> for ParseError<I> {
    fn from_external_error(input: I, kind: ErrorKind, e: E) -> Self {
        Self {
            errors: vec![(input, ParseErrorKind::External(kind, format!("{:?}", e)))],
        }
    }
}

impl<I> nom::error::ParseError<I> for ParseError<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Self {
            errors: vec![(input, ParseErrorKind::Nom(kind))],
        }
    }

    fn append(input: I, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.errors.push((input, ParseErrorKind::Nom(kind)));
        other
    }
}
