mod reader;
mod writer;

use crate::{Error, MASK_28_BIT};

pub use self::reader::Reader;
pub use self::writer::Writer;

const DATA_FORMAT: &[u8; 4] = b"ResB";
const DATA_VERSION: &[u8; 4] = &[1, 4, 0, 0];
const DATA_INFO_SIZE: u16 = 20;

/// The value of the magic word appearing in the header. As of
/// [`FormatVersion::V3_0`], this value is constant.
const MAGIC_WORD: &[u8; 2] = &[0xda, 0x27];

/// The size of the characters used in representations of string resources. As
/// of [`FormatVersion::V3_0`], this value is always 2.
const SIZE_OF_STRING_CHAR: u8 = 2;

/// The `BinResBundle` struct is designed to mimic the in-memory layout of a
/// binary resource bundle. It is used in constructing byte data for writing.
struct BinResBundle<'a> {
    /// The file header.
    ///
    /// As represented in the byte data, the header is zero-padded such that the
    /// total size of the header in bytes is divisible by 16.
    header: BinHeader,

    /// The resource descriptor for the root resource in the bundle. This is
    /// considered the `0`th 32-bit value in the body for purposes of
    /// resolving offsets.
    root_descriptor: ResDescriptor,

    /// Details of the final layout of the bundle as well as attributes for
    /// resolving resources.
    ///
    /// The index is present from [`FormatVersion::V1_1`] on.
    index: BinIndex,

    /// A block of strings representing keys in table resources.
    ///
    /// Keys are stored as contiguous null-terminated 8-bit strings in the
    /// core encoding of the [`CharsetFamily`] of the bundle.
    keys: &'a [u8],

    /// A block of 16-bit data containing 16-bit resources.
    ///
    /// This block is present from [`FormatVersion::V2_0`] on.
    ///
    /// 16-bit resources consist of UTF-16 string resources and collections
    /// which contain only 16-bit string resources. See
    /// [`ResourceReprType::StringV2`], [`ResourceReprType::Array16`], and
    /// [`ResourceReprType::Table16`] for details on representation.
    data_16_bit: &'a [u8],

    /// A block of resource representations. See [`ResourceReprType`] for
    /// details on the representation of resources.
    resources: &'a [u8],
}

impl From<BinResBundle<'_>> for Vec<u8> {
    fn from(value: BinResBundle) -> Self {
        // Manually build the list of bytes to write to the output. There are
        // crates which provide this functionality, but the writing of the body
        // is sufficiently complex as to make the header the only part where
        // an external crate would be convenient, and that's a matter of 20
        // bytes total, so on balance it's easier to just handle those here.
        let mut bytes = vec![];
        bytes.append(&mut Vec::<u8>::from(value.header));

        // Write the body.
        bytes.extend_from_slice(&u32::from(value.root_descriptor).to_ne_bytes());
        bytes.append(&mut Vec::<u8>::from(value.index));
        bytes.extend_from_slice(value.keys);
        bytes.extend_from_slice(value.data_16_bit);
        bytes.extend_from_slice(value.resources);

        bytes
    }
}

/// The `BinHeader` struct represents the in-memory layout of a binary resource
/// bundle header.
struct BinHeader {
    /// The size of the header in bytes, padded such that it is divisible by 16.
    size: u16,

    /// A magic word. See [`MAGIC_WORD`].
    magic: [u8; 2],

    /// Information on the representation of data in the binary bundle.
    repr_info: BinReprInfo,
}

impl From<BinHeader> for Vec<u8> {
    fn from(value: BinHeader) -> Self {
        let mut bytes = Vec::with_capacity(value.size as usize);

        bytes.extend_from_slice(&value.size.to_ne_bytes());
        bytes.extend_from_slice(&value.magic);

        let mut data_info_bytes = Vec::<u8>::from(value.repr_info);
        bytes.append(&mut data_info_bytes);

        // Pad the header such that its total size is divisible by 16.
        let padded_length = (bytes.len() + 15) & !0xf;
        bytes.resize(padded_length, 0);

        bytes
    }
}

/// The `BinReprInfo` struct represents the in-memory layout of a binary
/// resource bundle's representation specifiers. These data describe the
/// parameters necessary for correctly reading a bundle file.
struct BinReprInfo {
    /// The size of the representation info in bytes.
    size: u16,

    /// Reserved. Always 0.
    reserved_word: u16,

    /// The endianness of values in the file. `0` for little endian and `1` for
    /// big endian.
    endianness: Endianness,

    /// The character set family in which key strings are represented.
    charset_family: CharsetFamily,

    /// The size of a character in a string resource in bytes. May be 1, 2, or
    /// 4.
    size_of_char: u8,

    /// Reserved. Always 0.
    reserved_byte: u8,

    /// The data format identifier. Always `b"ResB"`.
    data_format: [u8; 4],

    /// The format version used for laying out the bundle.
    format_version: FormatVersion,

    /// The version of the data in the file. This is `[1, 4, 0, 0]` in all known
    /// versions of ICU4C's `genrb`.
    data_version: [u8; 4],
}

impl From<BinReprInfo> for Vec<u8> {
    fn from(value: BinReprInfo) -> Self {
        let mut bytes = Vec::with_capacity(value.size as usize);

        bytes.extend_from_slice(&value.size.to_ne_bytes());
        bytes.extend_from_slice(&value.reserved_word.to_ne_bytes());
        bytes.push(value.endianness.into());
        bytes.push(value.charset_family.into());
        bytes.push(value.size_of_char);
        bytes.push(value.reserved_byte);
        bytes.extend_from_slice(&value.data_format);
        bytes.extend_from_slice(&<[u8; 4]>::from(value.format_version));
        bytes.extend_from_slice(&value.data_version);

        bytes
    }
}

/// The `BinIndex` struct represents details of the written bundle.
///
/// The index is present from [`FormatVersion::V1_1`] on.
struct BinIndex {
    /// The number of 32-bit fields written in the index, including the field
    /// count.
    field_count: u32,

    /// The offset of the end of the key block in 32-bit values from the
    /// beginning of the body.
    keys_end: u32,

    /// The offset of the end of the resources block in 32-bit values from the
    /// beginning of the body.
    resources_end: u32,

    /// The offset of the end of the bundle in 32-bit values from the beginning
    /// of the body.
    ///
    /// As of [`FormatVersion::V3_0`], this is always the same as
    /// `resources_end`.
    bundle_end: u32,

    /// The number of entries in the largest table in the bundle.
    largest_table_entry_count: u32,

    /// Attributes describing resolution of external resources.
    ///
    /// Present from [`FormatVersion::V1_2`] on.
    bundle_attributes: u32,

    /// The offset of the end of the 16-bit data block in 32-bit values from the
    /// beginning of the body.
    ///
    /// Present from [`FormatVersion::V2_0`] on.
    data_16_bit_end: u32,

    /// The resource pool bundle checksum.
    ///
    /// Present from [`FormatVersion::V2_0`] on when the bundle either is a pool
    /// bundle or uses a pool bundle for sharing resources.
    pool_checksum: u32,
}

impl From<BinIndex> for Vec<u8> {
    fn from(value: BinIndex) -> Self {
        let mut bytes = Vec::with_capacity(value.field_count as usize * std::mem::size_of::<u32>());

        // Format version 1.0 did not include an index and so no bytes should be
        // written.
        if value.field_count >= 5 {
            bytes.extend_from_slice(&value.field_count.to_ne_bytes());
            bytes.extend_from_slice(&value.keys_end.to_ne_bytes());
            bytes.extend_from_slice(&value.resources_end.to_ne_bytes());
            bytes.extend_from_slice(&value.bundle_end.to_ne_bytes());
            bytes.extend_from_slice(&value.largest_table_entry_count.to_ne_bytes());
        }

        if value.field_count >= 6 {
            bytes.extend_from_slice(&value.bundle_attributes.to_ne_bytes());
        }

        if value.field_count >= 7 {
            bytes.extend_from_slice(&value.data_16_bit_end.to_ne_bytes());
        }

        if value.field_count >= 8 {
            bytes.extend_from_slice(&value.pool_checksum.to_ne_bytes());
        }

        bytes
    }
}

/// Adds convenience properties to an `enum` represented as a primitive type,
/// including conversions to and from the primitive type.
macro_rules! primitive_enum {
    ($type:ty, $(#[$meta:meta])* $vis:vis enum $name:ident {
        $($(#[$variant_meta:meta])* $variant:ident $(= $val:expr)?,)*
    }) => {
        $(#[$meta])*
        #[repr($type)]
        $vis enum $name {
            $($(#[$variant_meta])* $variant $(= $val)?,)*
        }

        impl $name {
            #[cfg(test)]
            const fn _variants() -> &'static [Self] {
                &[
                    $(Self::$variant,)*
                ]
            }
        }

        impl From<$name> for $type {
            fn from(v: $name) -> Self {
                v as $type
            }
        }

        impl TryFrom<$type> for $name {
            type Error = Error;

            fn try_from(v: $type) -> Result<Self, Self::Error> {
                match v {
                    $(x if x == $name::$variant as $type => Ok($name::$variant),)*
                    _ => Err(Error::new(format!("Failed to convert {v} to value of type {}.", stringify!($name)))),
                }
            }
        }
    }
}

primitive_enum!(
    u8,
    /// The endianness used to write a resource bundle.
    enum Endianness {
        Little = 0,
        Big = 1,
    }
);

primitive_enum!(
    u8,
    /// A family of character sets used to represent the characters of key strings.
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    enum CharsetFamily {
        /// The ASCII family of character sets, such as ASCII, latin1, and
        /// UTF-8.
        Ascii = 0,

        /// The EBCDIC family of character sets, such as EBCDIC and UTF-EBCDIC.
        ///
        /// The EBCDIC family is currently unsupported by this crate both for
        /// serialization and deserialization of binary bundles.
        Ebcdic = 1,
    }
);

primitive_enum!(
    u16,
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    /// The type of a resource representation within a binary resource bundle.
    ///
    /// The representation is distinct from the resource type as presented to
    /// consumers. Some resource types may have multiple possible
    /// representations, depending on the [`FormatVersion`] and—in the case of
    /// collections—number and type of their constituent resources.
    enum ResourceReprType {
        /// A string resource. Not yet supported.
        _String = 0,

        /// A raw binary resource.
        ///
        /// Consists of a 32-bit length value `n` followed by `n` arbitrary
        /// bytes.
        Binary = 1,

        /// A table resource for bundles with fewer than `0x1_0000` keys.
        ///
        /// Consists of a 32-bit length value `n` followed by `n` 16-bit key
        /// offsets from the beginning of the key block, 0 or 16 bits of padding
        /// (in order to align the table representation so far to a 32-bit
        /// boundary), and `n` 32-bit resource descriptors. For details on the
        /// representation of resource descriptors, see [`ResDescriptor`].
        ///
        /// The `i`th entry in the resulting table is a a pair of the `i`th key
        /// and the `i`th resource.
        Table = 2,

        /// An alias resource. Not yet supported.
        _Alias = 3,

        /// A table resource for bundles with `0x1_0000` or more keys. Not yet
        /// supported.
        _Table32 = 4,

        /// A 16-bit table resource. Not yet supported.
        Table16 = 5,

        /// A 16-bit string resource for [`FormatVersion::V2_0`] and later.
        ///
        /// Consists of a UTF-16 string with length marked in one of the
        /// following ways:
        ///
        /// - For strings of length `[1..40]` characters, there is no length
        ///   marker and the string must be null-terminated (i.e., by two `0`
        ///   bytes in a row).
        ///
        /// - For strings of length `n` in the range `(40..0x3ef)`, the string
        ///   is preceded by a single UTF-16 low surrogate composed as
        ///   `0xdc00 & n`.
        ///
        /// - For strings of length `n` in the range `[0x3ef..0x10_0000)`, the
        ///   string is preceded by a length marker consisting of a UTF-16 low
        ///   surrogate followed by a 16-bit value, composed as
        ///   `[0xdfef + (n >> 0x10), n & 0xffff]`.
        ///
        /// - For strings of length `n` in the range `[0x10_0000,
        ///   0x1_0000_0000)`, the string is preceded by a length marker
        ///   consisting of a UTF-16 low surrogate followed by two 16-bit length
        ///   values, composed as `[0xdfff, n >> 0x10, n & 0xffff]`.
        ///
        /// Strings of greater length than those described above may not be
        /// stored in binary bundles.
        ///
        /// These length markers can be reliably detected, as UTF-16 low
        /// surrogates may not legally appear without a preceding high surrogate
        /// in a UTF-16 string.
        StringV2 = 6,

        /// A 28-bit integer resource.
        ///
        /// Consists solely of the resource descriptor with the 28 bits of the
        /// integer in place of an offset.
        ///
        /// See [`Int28`] for more details on the storage and behavior of 28-bit
        /// integers. See [`ResDescriptor`] for more details on the
        /// representation of resource descriptors.
        ///
        /// [`Int28`]: crate::Int28
        Int = 7,

        /// A general array resource.
        ///
        /// Consists of a 32-bit length value `n` followed by `n` 32-bit
        /// resource descriptors. For more details on the representation of
        /// resource descriptors, see [`ResDescriptor`].
        Array = 8,

        /// A 16-bit array resource.
        ///
        /// Consists of a 16-bit length value `n` followed by `n` 16-bit offsets
        /// from the beginning of the 16-bit data block.
        ///
        /// As of [`FormatVersion::V3_0`], only `StringV2` representations can
        /// be fully stored in the 16-bit data block. As such, only `StringV2`
        /// resources can appear in an `Array16`.
        Array16 = 9,

        /// An integer array resource.
        ///
        /// Consists of a 32-bit length value `n` followed by `n` 32-bit integer
        /// values.
        ///
        /// Note that these are not integer _resources_, but rather full 32-bit
        /// integers.
        IntVector = 14,
    }
);

/// A `FormatVersion` represents a specific binary file format used for
/// representing resource bundles.
///
/// A partial [specification] of each format version is present in the ICU4C
/// source code.
///
/// [specification]: https://github.com/unicode-org/icu/blob/main/icu4c/source/common/uresdata.h
#[derive(Clone, Copy, Debug, PartialEq)]
enum FormatVersion {
    V1_0,
    V1_1,
    V1_2,
    V1_3,
    V2_0,
    V3_0,
}

impl From<FormatVersion> for [u8; 4] {
    fn from(value: FormatVersion) -> [u8; 4] {
        match value {
            FormatVersion::V1_0 => [1, 0, 0, 0],
            FormatVersion::V1_1 => [1, 1, 0, 0],
            FormatVersion::V1_2 => [1, 2, 0, 0],
            FormatVersion::V1_3 => [1, 3, 0, 0],
            FormatVersion::V2_0 => [2, 0, 0, 0],
            FormatVersion::V3_0 => [3, 0, 0, 0],
        }
    }
}

impl TryFrom<&[u8]> for FormatVersion {
    type Error = Error;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        if value.len() != 4 {
            return Err(Error::new(format!(
                "Invalid format version length {}.",
                value.len()
            )));
        }

        let value = match value {
            [1, 0, 0, 0] => FormatVersion::V1_0,
            [1, 1, 0, 0] => FormatVersion::V1_1,
            [1, 2, 0, 0] => FormatVersion::V1_2,
            [1, 3, 0, 0] => FormatVersion::V1_3,
            [2, 0, 0, 0] => FormatVersion::V2_0,
            [3, 0, 0, 0] => FormatVersion::V3_0,
            value => {
                return Err(Error::new(format!(
                    "No matching format version found for {:?}.",
                    value
                )))
            }
        };

        Ok(value)
    }
}

/// The `ResDescriptor` struct represents a typed pointer to a resource body
/// within a binary resource bundle.
///
/// It is represented within the binary bundle as a 4-bit resource type in the
/// most significant nibble of a 32-bit integer with a 28-bit unsigned offset
/// in the remaining bits. The offset is interpreted as a count of 32-bit
/// values from the start of the body.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ResDescriptor {
    resource_type: ResourceReprType,
    offset: u32,
}

impl ResDescriptor {
    /// Makes a new resource descriptor with the given type and 28-bit offset.
    pub const fn new(resource_type: ResourceReprType, offset: u32) -> Self {
        Self {
            resource_type,
            offset: offset & MASK_28_BIT,
        }
    }

    /// Makes a new resource descriptor with the given type and no body.
    pub const fn empty(resource_type: ResourceReprType) -> Self {
        Self::new(resource_type, 0)
    }

    /// Gets the 28-bit offset to the described resource.
    pub fn offset(&self) -> u32 {
        self.offset
    }

    /// Gets the resource type of the described resource.
    pub fn resource_type(&self) -> ResourceReprType {
        self.resource_type
    }
}

impl TryFrom<u32> for ResDescriptor {
    type Error = Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        let resource_type = ResourceReprType::try_from((value >> 28) as u16)?;

        Ok(Self::new(resource_type, value))
    }
}

impl From<ResDescriptor> for u32 {
    fn from(value: ResDescriptor) -> Self {
        ((value.resource_type as u32) << 28) | value.offset
    }
}

#[cfg(test)]
mod tests {
    use crate::binary::ResourceReprType;

    #[test]
    fn try_from_u32() {
        for &variant in ResourceReprType::_variants() {
            assert_eq!(variant, ResourceReprType::try_from(variant as u16).unwrap());
        }
    }
}
