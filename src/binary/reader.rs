mod parse_error;
mod parse_state;

mod general_parsers;

use std::borrow::Cow;

use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_until1},
    combinator::{cond, cut, flat_map, map, map_res, peek, rest_len, value, verify},
    error::context,
    multi::{count, length_count, many_m_n},
    number::{
        complete::{u16, u32, u8},
        Endianness,
    },
    sequence::{preceded, terminated, tuple},
    Finish, IResult, Parser,
};

use crate::{Error, Int28, Resource, ResourceBundle, Table};

use self::{
    general_parsers::separated_zip_count,
    parse_error::{convert_error, ParseError},
    parse_state::ParseState,
};

use super::{
    CharsetFamily, FormatVersion, ResDescriptor, ResourceReprType, DATA_FORMAT, DATA_INFO_SIZE,
    DATA_VERSION, MAGIC_WORD, SIZE_OF_STRING_CHAR,
};

/// Reads a key from the input body.
///
/// Keys are null-terminated strings in the core encoding of the input's
/// [`CharsetFamily`].
fn key_string<'a>(
    offset: u32,
) -> impl FnMut(ParseState<'a>) -> IResult<ParseState, &str, ParseError<ParseState<'a>>> {
    move |state| {
        let internal = state.new_from_32_bit_offset(offset);

        let (_, string) = map_res(take_until1(b"\x00" as &[u8]), |bytes: ParseState<'a>| {
            core::str::from_utf8(bytes.input)
        })
        .parse(internal)?;

        Ok((state, string))
    }
}

/// Reads an explicit length for a UTF-16 string of `(40..0x3ef)` characters.
///
/// See [`ResourceReprType::StringV2`] for details on representation.
fn explicit_1_word_length(input: ParseState) -> IResult<ParseState, u32, ParseError<ParseState>> {
    context(
        "explicit_1_word_length",
        map(
            verify(u16(input.endianness()), |value| {
                (0xdc00..0xdfef).contains(value)
            }),
            |value| (value & 0x3ff) as u32,
        ),
    )(input)
}

/// Reads an explicit length for a UTF-16 string of `[0x3ef..0x10_0000)
/// characters.
///
/// See [`ResourceReprType::StringV2`] for details on representation.
fn explicit_2_word_length(input: ParseState) -> IResult<ParseState, u32, ParseError<ParseState>> {
    context(
        "explicit_2_word_length",
        map(
            preceded(
                peek(verify(u16(input.endianness()), |value| {
                    (0xdfef..0xdfff).contains(value)
                })),
                count(u16(input.endianness()), 2),
            ),
            |value| ((value[0] as u32 - 0xdfef) << 16) | value[1] as u32,
        ),
    )(input)
}

/// Reads an explicit length for a UTF-16 string of `[0x10_0000..0x1_0000_0000)`
/// characters.
///
/// As this is already a minimum of 8GB, it is sincerely hoped that there is no
/// call for longer strings.
///
/// See [`ResourceReprType::StringV2`] for details on representation.
fn explicit_3_word_length(input: ParseState) -> IResult<ParseState, u32, ParseError<ParseState>> {
    context(
        "explicit_3_word_length",
        map(
            preceded(
                verify(u16(input.endianness()), |&value| value == 0xdfff),
                count(u16(input.endianness()), 2),
            ),
            |value| ((value[0] as u32) << 16) | value[1] as u32,
        ),
    )(input)
}

/// Reads a UTF-16 string of length greater than 40 characters.
///
/// These strings are stored in the byte stream as UTF-16 low surrogates, which
/// in a well-formed UTF-16 string never occur without a preceding high
/// high surrogate. As such, if we find a low surrogate at the offset given for
/// a UTF-16 string, it must be part of a length indicator.
///
/// See [`ResourceReprType::StringV2`] for more details on the exact
/// representation and meaning of each marker.
fn explicit_length_string(
    input: ParseState,
) -> IResult<ParseState, Vec<u16>, ParseError<ParseState>> {
    context(
        "explicit_length_string",
        preceded(
            peek(verify(u16(input.endianness()), |value| {
                (0xdc00..=0xdfff).contains(value)
            })),
            length_count(
                alt((
                    explicit_1_word_length,
                    explicit_2_word_length,
                    explicit_3_word_length,
                )),
                u16(input.endianness()),
            ),
        ),
    )(input)
}

/// Reads a UTF-16 string without an explicit length marker.
///
/// These must be no more than 40 characters long and null-terminated with
/// U+0000 in UTF-16. See [`ResourceReprType::StringV2`] for more details.
fn implicit_length_string(
    input: ParseState,
) -> IResult<ParseState, Vec<u16>, ParseError<ParseState>> {
    context(
        "implicit_length_string",
        terminated(
            many_m_n(1, 40, verify(u16(input.endianness()), |&value| value != 0)),
            tag(b"\x00\x00" as &[u8]),
        ),
    )(input)
}

/// Reads a UTF-16 string as a series of `u16` words.
///
/// See [`ResourceReprType::StringV2`] for details on binary representation of
/// string resources.
fn string_as_words(input: ParseState) -> IResult<ParseState, Vec<u16>, ParseError<ParseState>> {
    alt((explicit_length_string, implicit_length_string))(input)
}

/// Reads a 16-bit array resource representation from the input body.
///
/// See [`ResourceReprType::Array16`] for details on binary representation.
fn array16_resource<'a>(
    offset: u16,
) -> impl FnMut(ParseState<'a>) -> IResult<ParseState, Resource, ParseError<ParseState<'a>>> {
    move |state| {
        let internal = state.new_from_16_bit_offset(offset * core::mem::size_of::<u16>() as u16);
        let (_, resources) = length_count(
            u16(state.endianness()),
            flat_map(u16(state.endianness()), stringv2_resource),
        )
        .parse(internal)?;

        Ok((state, Resource::Array(resources)))
    }
}

/// Reads a UTF-16 string resource representation from the input body.
///
/// The representation of the string is dependent on length. See
/// [`ResourceReprType::StringV2`] for more details.
fn stringv2_resource<'a>(
    offset: u16,
) -> impl FnMut(ParseState<'a>) -> IResult<ParseState, Resource, ParseError<ParseState<'a>>> {
    move |state| {
        let internal = state.new_from_16_bit_offset(offset * core::mem::size_of::<u16>() as u16);
        let (_, resource) = map(
            map_res(string_as_words, |words| {
                char::decode_utf16(words).collect::<Result<String, _>>()
            }),
            |value| Resource::String(Cow::from(value)),
        )
        .parse(internal)?;

        Ok((state, resource))
    }
}

/// Reads an array resource representation from the input body.
///
/// See [`ResourceReprType::Array`] for details on binary representation.
fn array_resource<'a>(
    offset: u32,
) -> impl FnMut(ParseState<'a>) -> IResult<ParseState, Resource, ParseError<ParseState<'a>>> {
    move |state| {
        let internal = state.new_from_32_bit_offset(offset * core::mem::size_of::<u32>() as u32);
        let (_, resources) = length_count(u32(state.endianness()), resource).parse(internal)?;

        Ok((state, Resource::Array(resources)))
    }
}

/// Reads a binary resource representation from the input body.
///
/// See [`ResourceReprType::Binary`] for details on binary representation.
fn binary_resource<'a>(
    offset: u32,
) -> impl FnMut(ParseState<'a>) -> IResult<ParseState, Resource, ParseError<ParseState<'a>>> {
    move |state| {
        let internal = state.new_from_32_bit_offset(offset * core::mem::size_of::<u32>() as u32);
        let (_, bytes) = flat_map(u32(state.endianness()), take).parse(internal)?;

        Ok((state, Resource::Binary(Cow::from(bytes.input))))
    }
}

/// Reads an integer resource representation from the input body.
///
/// See [`ResourceReprType::Int`] for details on binary representation and
/// the interpretation of 28-bit integers.
fn int_resource<'a>(offset: u32) -> Resource<'a> {
    Resource::Integer(Int28::from(offset))
}

/// Reads an integer vector resource representation from the input body.
///
/// See [`ResourceReprType::IntVector`] for details on binary representation.
fn int_vector_resource<'a>(
    offset: u32,
) -> impl FnMut(ParseState<'a>) -> IResult<ParseState, Resource, ParseError<ParseState<'a>>> {
    move |state| {
        let internal = state.new_from_32_bit_offset(offset * core::mem::size_of::<u32>() as u32);
        let (_, integers) =
            length_count(u32(state.endianness()), u32(state.endianness())).parse(internal)?;

        Ok((state, Resource::IntVector(integers)))
    }
}

/// Reads a table resource representation from the input body.
///
/// See [`ResourceReprType::Table`] for details on binary representation.
fn table_resource<'a>(
    offset: u32,
) -> impl FnMut(ParseState<'a>) -> IResult<ParseState, Resource, ParseError<ParseState<'a>>> {
    move |state| {
        let internal = state.new_from_32_bit_offset(offset * core::mem::size_of::<u32>() as u32);
        let (internal, length) = u16(state.endianness()).parse(internal)?;

        let (_, entries) = separated_zip_count(
            length as usize,
            flat_map(
                map(u16(state.endianness()), |value| value as u32),
                key_string,
            ),
            cond(length & 1 == 0, take(2usize)),
            resource,
        )
        .parse(internal)?;

        let mut table = Table::new();
        for (key, resource) in entries {
            table.insert(key.into(), resource);
        }

        Ok((state, Resource::Table(table)))
    }
}

/// Reads a resource from its corresponding 32-bit descriptor.
fn resource_from_descriptor<'a>(
    descriptor: ResDescriptor,
) -> impl FnMut(ParseState<'a>) -> IResult<ParseState, Resource, ParseError<ParseState<'a>>> {
    move |input| {
        let result = match descriptor.resource_type() {
            ResourceReprType::_String => todo!(),
            ResourceReprType::Binary => binary_resource(descriptor.offset()).parse(input)?,
            ResourceReprType::Table => table_resource(descriptor.offset()).parse(input)?,
            ResourceReprType::_Alias => todo!(),
            ResourceReprType::StringV2 => {
                stringv2_resource(descriptor.offset() as u16).parse(input)?
            }
            ResourceReprType::Int => (input, int_resource(descriptor.offset())),
            ResourceReprType::Array => array_resource(descriptor.offset()).parse(input)?,
            ResourceReprType::Array16 => {
                array16_resource(descriptor.offset() as u16).parse(input)?
            }
            ResourceReprType::IntVector => int_vector_resource(descriptor.offset()).parse(input)?,
            _ => panic!("Unrecognized resource descriptor."),
        };

        Ok(result)
    }
}

/// Reads a `u32` value as a resource descriptor.
///
/// See [`ResDescriptor`] for details on representation of descriptors in the
/// input.
///
/// Failure to read a resource descriptor where one is expected is a fatal parse
/// error.
fn descriptor(state: ParseState) -> IResult<ParseState, ResDescriptor, ParseError<ParseState>> {
    cut(map_res(u32(state.endianness()), |value| {
        ResDescriptor::try_from(value)
    }))(state)
}

/// Reads a resource descriptor and its corresponding resource.
///
/// Failure to read a resource where one is expected is a fatal parse error.
fn resource(state: ParseState) -> IResult<ParseState, Resource, ParseError<ParseState>> {
    let (state, descriptor) = descriptor.parse(state)?;

    let (_, resource) = cut(resource_from_descriptor(descriptor)).parse(state.clone())?;

    Ok((state, resource))
}

/// Reads a resource bundle from the input body.
///
/// See [`BinResBundle`] for details on the representation of the bundle as
/// bytes.
///
/// [`BinResBundle`]: super::BinResBundle
fn bundle<'a>(
    name: &'a str,
) -> impl FnMut(ParseState<'a>) -> IResult<ParseState, ResourceBundle, ParseError<ParseState<'a>>> {
    move |state| {
        // Read the descriptor for the root resource. We presently only support
        // table resources at the root.
        let (state, root_descriptor) = verify(descriptor, |descriptor| {
            descriptor.resource_type() == ResourceReprType::Table
        })(state)?;

        // Parse the index.
        let (state, is_locale_fallback_enabled) = index.parse(state)?;

        // Read the root resource from the descriptor we previously parsed.
        let (_, root) = resource_from_descriptor(root_descriptor).parse(state.clone())?;

        Ok((
            state,
            ResourceBundle::new(Cow::from(name), root, is_locale_fallback_enabled),
        ))
    }
}

/// Reads the offsets and attributes from the index header at the beginning of
/// the body.
///
/// See [`BinIndex`] for details on the representation of the index as
/// bytes.
///
/// [`BinIndex`]: super::BinIndex
fn index(input: ParseState) -> IResult<ParseState, bool, ParseError<ParseState>> {
    if input.format_version() == FormatVersion::V1_0 {
        // Version 1.0 did not specify any index.
        return Ok((input, true));
    }

    // Read the count of 32-bit entries in the index. From version 1.1 on, there
    // should be at least five entries present.
    let (input, index_length) =
        verify(u32(input.endianness()), |&value| value >= 5).parse(input)?;

    // Read the offset from the beginning of the input body at which 16-bit data
    // begins.
    let (input, words_start) = u32(input.endianness()).parse(input)?;
    let input = input.set_16_bit_block_start(words_start * core::mem::size_of::<u32>() as u32);

    // We don't presently use the next three entries. Consume them.
    let (input, _) = take(3usize * core::mem::size_of::<u32>()).parse(input)?;

    // Read the attributes entry. This is a bitfield from which we presently
    // only use the value indicating whether fallback is enabled.
    let (input, is_locale_fallback_enabled) = map(
        cond(
            index_length >= 6,
            map(u32(input.endianness()), |value| value & 1 == 0),
        ),
        |result| result.unwrap_or(true),
    )
    .parse(input)?;

    // Consume any entries we don't presently use.
    let (input, _) = cond(
        index_length >= 7,
        take((index_length - 6) as usize * core::mem::size_of::<u32>()),
    )
    .parse(input)?;

    Ok((input, is_locale_fallback_enabled))
}

/// Reads a format version from its byte representation.
fn format_version<'a>(
    format_version: FormatVersion,
) -> impl FnMut(&'a [u8]) -> IResult<&[u8], FormatVersion, ParseError<&'a [u8]>> {
    move |input| {
        context(
            "format_version",
            value(format_version, tag(<[u8; 4]>::from(format_version))),
        )(input)
    }
}

/// Reads the representation info structure in the header.
///
/// At present, the only information we use from the header is the format
/// version.
///
/// See [`BinReprInfo`] for details on the representation of the bundle as
/// bytes.
///
/// [`BinReprInfo`]: super::BinReprInfo
fn repr_info<'a>(
    endianness: Endianness,
) -> impl FnMut(&'a [u8]) -> IResult<&[u8], FormatVersion, ParseError<&'a [u8]>> {
    move |input: &'a [u8]| {
        let (input, _) = tuple((
            // The first word of the header represents the size of the C++
            // struct. This is 20 in all format versions at the time of writing.
            verify(u16(endianness), |&value| value == DATA_INFO_SIZE),
            // Reserved word.
            u16(endianness),
            // Sanity check; we already parsed the endianness from the header earlier.
            verify(u8, |&value| (value == 1) == (endianness == Endianness::Big)),
            // Verify character set family is ASCII. This value could be 1,
            // indicating EBCDIC, but this is presently unsupported in this
            // crate.
            verify(u8, |&value| value == CharsetFamily::Ascii.into()),
            // Verify the size of string characters. This should always be 2.
            verify(u8, |&value| value == SIZE_OF_STRING_CHAR),
            // Reserved byte.
            u8,
            // Verify the data format. This is `"ResB"` in all format versions
            // at the time of writing.
            tag(DATA_FORMAT),
        ))
        .parse(input)?;

        // The format version is stored as a string of four bytes. At the time of
        // writing, this is a major version, a minor version, and two zero bytes.
        let (input, format_version) = alt((
            format_version(FormatVersion::V1_0),
            format_version(FormatVersion::V1_1),
            format_version(FormatVersion::V1_2),
            format_version(FormatVersion::V1_3),
            format_version(FormatVersion::V2_0),
            format_version(FormatVersion::V3_0),
        ))
        .parse(input)?;

        // Verify data version. This is `[1, 4, 0, 0]` in all known format
        // versions at the time of writing.
        let (input, _) = tag(DATA_VERSION).parse(input)?;

        Ok((input, format_version))
    }
}

/// Reads the endianness of the input.
///
/// Returns [`Endianness::Little`] if the endianness byte is `0` and
/// [`Endianness::Big`] if it is `1`. Otherwise, it does not match.
fn endianness(input: &[u8]) -> IResult<&[u8], Endianness, ParseError<&[u8]>> {
    context(
        "endianness",
        alt((
            value(Endianness::Little, tag(b"\x00")),
            value(Endianness::Big, tag(b"\x01")),
        )),
    )(input)
}

/// Reads ahead from the start of the input to capture the input's endianness.
///
/// The endianness is within the representation info block of the header, but by
/// the time we parse that far, we've already encountered several
/// endian-sensitive values.
fn endianness_lookahead(input: &[u8]) -> IResult<&[u8], Endianness, ParseError<&[u8]>> {
    context(
        "endianness_lookahead",
        peek(preceded(take(8usize), endianness)),
    )(input)
}

/// Reads the magic bytes from the header.
fn header_magic(input: &[u8]) -> IResult<&[u8], &[u8], ParseError<&[u8]>> {
    tag(MAGIC_WORD)(input)
}

/// Reads the size of the representation info in the header.
///
/// This value includes any padding at the end of the representation info block
/// in order to for the length of the header in bytes to be divisible by 16.
fn padded_repr_info_size(
    endianness: Endianness,
) -> impl FnMut(&[u8]) -> IResult<&[u8], u16, ParseError<&[u8]>> {
    move |input| {
        context(
            "aligned_header_size",
            terminated(
                map(
                    verify(u16(endianness), |&value| value >= DATA_INFO_SIZE),
                    |value| {
                        // In reading the header size and the following two magic
                        // bytes, we use up 4 bytes of the total padded header.
                        // What remains is the size of the data info.
                        value - 4
                    },
                ),
                header_magic,
            ),
        )(input)
    }
}

/// Reads the header from the input and builds the parse state needed for
/// parsing the body.
///
/// See [`BinHeader`] for details on the representation of the header as
/// bytes.
///
/// [`BinHeader`]: super::BinHeader
fn header(input: &[u8]) -> IResult<&[u8], ParseState, ParseError<&[u8]>> {
    let (input, endianness) = context(
        "header",
        preceded(
            // Ensure that there is enough data to read the file header.
            verify(rest_len, |&length| length >= DATA_INFO_SIZE as usize),
            // Skip ahead and grab the byte which represents the endianness of
            // the file.
            endianness_lookahead,
        ),
    )
    .parse(input)?;

    // Return to the beginning of the header and parse data info.
    let (input, data_info_bytes) =
        flat_map(padded_repr_info_size(endianness), take).parse(input)?;
    let (_, format_version) = repr_info(endianness).parse(data_info_bytes)?;
    if format_version != FormatVersion::V2_0 {
        panic!("Only format version 2.0 is presently supported.");
    }

    let state = ParseState::new(input, endianness, format_version);

    Ok((input, state))
}

/// The `Reader` struct provides a means of parsing a [`ResourceBundle`] from
/// a sequence of bytes representing the contents of a binary resource bundle
/// file.
pub struct Reader;

impl Reader {
    /// Parses the given byte slice into a resource bundle.
    pub fn read<'a>(name: &'a str, input: &'a [u8]) -> Result<ResourceBundle<'a>, Error> {
        let (_, state) = match header(input).finish() {
            Ok(result) => result,
            Err(err) => {
                return Err(Error::new(format!(
                    "An error occurred during parse:\n{}",
                    convert_error(input, err)
                )))
            }
        };

        let (_, bundle) = match bundle(name)(state).finish() {
            Ok(result) => result,
            Err(err) => {
                return Err(Error::new(format!(
                    "An error occurred during parse:\n{}",
                    convert_error(input, err)
                )))
            }
        };

        Ok(bundle)
    }
}

#[cfg(test)]
mod tests;
