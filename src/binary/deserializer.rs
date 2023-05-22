use crate::Error;

use super::{
    get_16_bit_offset, get_32_bit_offset, get_subslice, read_u16, read_u32, BinHeader, BinIndex,
    FormatVersion, ResDescriptor, ResourceReprType, SYSTEM_CHARSET_FAMILY,
};

extern crate alloc;
use alloc::{format, string::String};

use nom::InputIter;
use serde::{de, forward_to_deserialize_any, Deserialize};

/// Deserializes an instance of type `T` from bytes representing a binary ICU
/// resource bundle.
pub fn from_bytes<'a, T>(input: &'a [u8]) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    let mut deserializer = ResourceTreeDeserializer::from_bytes(input)?;
    let t = T::deserialize(&mut deserializer)?;

    Ok(t)
}

/// The `ResourceTreeDeserializer` struct processes an ICU binary resource
/// bundle by walking the resource tree (as represented by [`ResDescriptor`]s).
struct ResourceTreeDeserializer<'de> {
    /// The current position in the input, represented as a slice beginning at
    /// the next byte to be read and ending at the end of input.
    ///
    /// As an invariant of the deserializer, `input` should always begin
    /// immediately before a resource descriptor.
    input: &'de [u8],

    /// The format version of the input.
    ///
    /// This is currently unused, but support for other format versions will be
    /// incorporated in later versions.
    _format_version: FormatVersion,

    /// The 16-bit data block, represented as a slice beginning at the start of
    /// the block. This is `None` in format versions below 2.0.
    data_16_bit: Option<&'de [u8]>,

    /// The keys block, represented as a slice beginning at the start of the
    /// block.
    keys: &'de [u8],

    /// The input body, represented as a slice beginning at the start of the
    /// block.
    body: &'de [u8],
}

impl<'de> ResourceTreeDeserializer<'de> {
    /// Creates a new deserializer from the header and index of the resource
    /// bundle.
    fn from_bytes(input: &'de [u8]) -> Result<Self, Error> {
        let header = BinHeader::try_from(input)?;

        // Verify that the representation in the resource bundle is one we're
        // prepared to read.
        if header.repr_info.charset_family != SYSTEM_CHARSET_FAMILY {
            return Err(Error::new(
                "Unable to parse resource bundle from non-matching charset family.",
            ));
        }

        if header.repr_info.size_of_char != 2 {
            return Err(Error::new(
                "Unable to parse resource bundle with characters of size other than 2.",
            ));
        }

        if header.repr_info.format_version != FormatVersion::V2_0 {
            // Support for other versions can be added at a later time, but for
            // now we can only deal with 2.0.
            return Err(Error::new(
                "Unable to parse resource bundle of format version other than 2.0.",
            ));
        }

        let body = get_subslice(input, header.size as usize..)?;

        // Skip the root resource descriptor and get the index area.
        let index = get_subslice(body, get_32_bit_offset(1)..)?;
        let index = BinIndex::try_from(index)?;

        // Keys begin at the start of the body.
        let keys = get_subslice(body, ..get_32_bit_offset(index.keys_end as usize))?;

        let data_16_bit = if header.repr_info.format_version < FormatVersion::V2_0 {
            // The 16-bit data area was not introduced until format version 2.0.
            None
        } else {
            if let Some(data_16_bit_end) = index.data_16_bit_end {
                let data_16_bit = get_subslice(
                    body,
                    get_32_bit_offset(index.keys_end as usize)
                        ..get_32_bit_offset(data_16_bit_end as usize),
                )?;
                Some(data_16_bit)
            } else {
                return Err(Error::new(
                    "Invalid resource bundle. No 16-bit data area end offset specified.",
                ));
            }
        };

        Ok(Self {
            input: body,
            _format_version: header.repr_info.format_version,
            data_16_bit,
            keys,
            body,
        })
    }

    /// Reads the next resource descriptor without updating the input position.
    fn peek_next_resource_descriptor(&self) -> Result<ResDescriptor, Error> {
        let input = get_subslice(self.input, 0..4)?;
        let descriptor = match input.try_into() {
            Ok(value) => value,
            Err(_) => return Err(Error::new("Unable to read resource descriptor bytes.")),
        };
        let descriptor = u32::from_ne_bytes(descriptor);

        ResDescriptor::try_from(descriptor)
    }

    /// Reads the next resource descriptor.
    fn get_next_resource_descriptor(&mut self) -> Result<ResDescriptor, Error> {
        let result = self.peek_next_resource_descriptor();

        // Pop resource descriptor from input.
        self.input = get_subslice(self.input, get_32_bit_offset(1)..)?;

        result
    }

    /// Reads a 28-bit integer resource as a signed value.
    fn parse_signed(&mut self) -> Result<i32, Error> {
        let descriptor = self.get_next_resource_descriptor()?;
        match descriptor.resource_type() {
            // Since integers in the resource bundle are 28-bit, we need to
            // shift left and shift back right in order to get sign extension.
            // Per https://doc.rust-lang.org/reference/expressions/operator-expr.html#arithmetic-and-logical-binary-operators,
            // `>>` is arithmetic right shift on signed ints, so it gives us the
            // desired behavior.
            ResourceReprType::Int => Ok(((descriptor.offset() as i32) << 4) >> 4),
            _ => Err(Error::new("Unexpected resource type where int expected.")),
        }
    }

    /// Reads a 28-bit integer resource as an unsigned value.
    fn parse_unsigned(&mut self) -> Result<u32, Error> {
        let descriptor = self.get_next_resource_descriptor()?;
        match descriptor.resource_type() {
            ResourceReprType::Int => Ok(descriptor.offset()),
            _ => Err(Error::new("Unexpected resource type where int expected.")),
        }
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut ResourceTreeDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let descriptor = self.peek_next_resource_descriptor()?;
        match descriptor.resource_type() {
            ResourceReprType::_String | ResourceReprType::StringV2 => {
                self.deserialize_string(visitor)
            }
            ResourceReprType::Binary => self.deserialize_bytes(visitor),
            ResourceReprType::Table | ResourceReprType::Table16 | ResourceReprType::_Table32 => {
                self.deserialize_map(visitor)
            }
            ResourceReprType::_Alias => todo!(),
            ResourceReprType::Int => self.deserialize_u32(visitor),
            ResourceReprType::Array | ResourceReprType::Array16 | ResourceReprType::IntVector => {
                self.deserialize_seq(visitor)
            }
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let value = self.parse_unsigned()?;
        let value = match value {
            0 => false,
            1 => true,
            _ => return Err(Error::new("Value cannot be represented as Boolean.")),
        };

        visitor.visit_bool(value)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_i32(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_i32(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i32(self.parse_signed()?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i64(self.parse_signed()? as i64)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_u32(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_u32(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u32(self.parse_unsigned()?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u64(self.parse_unsigned()? as u64)
    }

    fn deserialize_f32<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // Resource bundles have no native concept of floating point numbers and
        // no examples of storing them have been encountered.
        unimplemented!()
    }

    fn deserialize_f64<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // Resource bundles have no native concept of floating point numbers and
        // no examples of storing them have been encountered.
        unimplemented!()
    }

    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // Resource bundles have no native concept of single characters and no
        // examples of storing them have been encountered.
        unimplemented!()
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // Strings in resource bundles are stored as UTF-16 and can't be
        // borrowed.
        self.deserialize_string(visitor)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let descriptor = self.get_next_resource_descriptor()?;
        match descriptor.resource_type() {
            ResourceReprType::_String => todo!(),
            ResourceReprType::StringV2 => {
                if let Some(data_16_bit) = self.data_16_bit {
                    if descriptor.is_empty() {
                        return visitor.visit_string(String::from(""));
                    }

                    let input = get_subslice(
                        data_16_bit,
                        get_16_bit_offset(descriptor.offset() as usize)..,
                    )?;
                    let de = Resource16BitDeserializer::new(input);
                    de.deserialize_string(visitor)
                } else {
                    Err(Error::new(
                        "Invalid resource bundle: StringV2 resource without 16-bit data block.",
                    ))
                }
            }
            _ => Err(Error::new(
                "Unexpected resource type where string expected.",
            )),
        }
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let descriptor = self.get_next_resource_descriptor()?;
        let value = match descriptor.resource_type() {
            ResourceReprType::Binary => {
                // Binary resources are, by definition, a sequence of arbitrary
                // bytes and can be borrowed as such.
                if descriptor.is_empty() {
                    // Handle empty descriptors per-type so we don't miss a type
                    // mismatch.
                    return visitor.visit_borrowed_bytes(&[]);
                }

                let input =
                    get_subslice(self.body, get_32_bit_offset(descriptor.offset() as usize)..)?;
                let (length, input) = read_u32(input)?;

                get_subslice(input, 0..length as usize)?
            }
            ResourceReprType::IntVector => {
                // Int vector resources are stored as a sequence of 32-bit
                // integers in the bundle's native endian. For zero-copy, it may
                // be desirable to simply borrow as bytes.
                if descriptor.is_empty() {
                    // Handle empty descriptors per-type so we don't miss a type
                    // mismatch.
                    return visitor.visit_borrowed_bytes(&[]);
                }

                let input =
                    get_subslice(self.body, get_32_bit_offset(descriptor.offset() as usize)..)?;
                let (length, input) = read_u32(input)?;

                get_subslice(input, ..get_32_bit_offset(length as usize))?
            }
            ResourceReprType::StringV2 => {
                // String resources are stored as UTF-16 strings in the bundle's
                // native endian. In situations where treatment as strings may
                // not be needed or performance would benefit from lazy
                // interpretation, allow for zero-copy.
                if let Some(data_16_bit) = self.data_16_bit {
                    if descriptor.is_empty() {
                        // Handle empty descriptors per-type so we don't miss a
                        // type mismatch.
                        return visitor.visit_borrowed_bytes(&[]);
                    }

                    let input = get_subslice(
                        data_16_bit,
                        get_16_bit_offset(descriptor.offset() as usize)..,
                    )?;
                    let (length, input) = get_length_and_start_of_utf16_string(input)?;
                    get_subslice(input, ..get_16_bit_offset(length as usize))?
                } else {
                    return Err(Error::new(
                        "Invalid resource bundle: StringV2 resource without 16-bit data block.",
                    ));
                }
            }
            _ => {
                return Err(Error::new(
                    "Unexpected resource type where binary expected.",
                ))
            }
        };

        visitor.visit_borrowed_bytes(value)
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // There's no concept of `null` or any other unit type in resource
        // bundles.
        unimplemented!()
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // There's no concept of `null` or any other unit type in resource
        // bundles.
        unimplemented!()
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // Resource bundles have no concept of newtypes, so just pass through
        // and let the visitor ask for what it expects.
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let descriptor = self.get_next_resource_descriptor()?;
        match descriptor.resource_type() {
            ResourceReprType::Array => {
                if descriptor.is_empty() {
                    // Handle empty descriptors per-type so we don't miss a type
                    // mismatch.
                    return visitor.visit_seq(EmptySeqAccess);
                }

                let input =
                    get_subslice(self.body, get_32_bit_offset(descriptor.offset() as usize)..)?;
                let (length, offsets) = read_u32(input)?;

                visitor.visit_seq(ArraySeqAccess {
                    de: self,
                    descriptors: offsets,
                    remaining: length as usize,
                })
            }
            ResourceReprType::Array16 => {
                if descriptor.is_empty() {
                    // Handle empty descriptors per-type so we don't miss a type
                    // mismatch.
                    return visitor.visit_seq(EmptySeqAccess);
                }

                if let Some(data_16_bit) = self.data_16_bit {
                    let input = get_subslice(
                        data_16_bit,
                        get_16_bit_offset(descriptor.offset() as usize)..,
                    )?;
                    let (length, offsets) = read_u16(input)?;

                    let result = visitor.visit_seq(Array16SeqAccess {
                        data_16_bit,
                        offsets,
                        remaining: length as usize,
                    });

                    result
                } else {
                    Err(Error::new(
                        "Invalid resource bundle: StringV2 resource with no 16-bit data.",
                    ))
                }
            }
            ResourceReprType::IntVector => {
                if descriptor.is_empty() {
                    // Handle empty descriptors per-type so we don't miss a type
                    // mismatch.
                    return visitor.visit_seq(EmptySeqAccess);
                }

                let input =
                    get_subslice(self.body, get_32_bit_offset(descriptor.offset() as usize)..)?;
                let (length, values) = read_u32(input)?;

                let result = visitor.visit_seq(IntVectorSeqAccess {
                    values,
                    remaining: length as usize,
                });

                result
            }
            _ => Err(Error::new("Unexpected resource type where array expected.")),
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // We copy `serde_json` in handling tuples as sequences.
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // We copy `serde_json` in handling tuples as sequences.
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let descriptor = self.get_next_resource_descriptor()?;
        match descriptor.resource_type() {
            ResourceReprType::Table => {
                if descriptor.is_empty() {
                    // Handle empty descriptors per-type so we don't miss a type
                    // mismatch.
                    return visitor.visit_map(EmptyMapAccess);
                }

                let input =
                    get_subslice(self.body, get_32_bit_offset(descriptor.offset() as usize)..)?;
                let (length, keys) = read_u16(input)?;

                // Most values in the file are 32-bit aligned, so sequences of
                // 16-bit values may be padded.
                let length_with_padding = (length + ((length + 1) % 2)) as usize;

                let values_offset = get_16_bit_offset(length_with_padding);
                let values = get_subslice(keys, values_offset..)?;

                visitor.visit_map(TableMapAccess {
                    de: self,
                    keys,
                    values,
                    remaining: length as usize,
                })
            }
            ResourceReprType::_Table32 => todo!(),
            ResourceReprType::Table16 => todo!(),
            _ => Err(Error::new("Unexpected resource type where table expected.")),
        }
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // Resource bundles have no concept of an enum and it's unclear how to
        // handle untagged heterogeneous values.
        todo!()
    }

    fn deserialize_identifier<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let (_, input) = read_u32(self.input)?;
        self.input = input;

        visitor.visit_none()
    }

    fn is_human_readable(&self) -> bool {
        false
    }
}

/// The `Array16SeqAccess` struct provides deserialization for resources of type
/// `Array16`.
///
/// See [`ResourceReprType`] for more details.
struct Array16SeqAccess<'de> {
    data_16_bit: &'de [u8],
    offsets: &'de [u8],
    remaining: usize,
}

impl<'de> de::SeqAccess<'de> for Array16SeqAccess<'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        if self.remaining == 0 {
            return Ok(None);
        }

        // Elements are stored as a sequence of `u16` offsets. Pop one and
        // deserialize the corresponding resource.
        let (offset, rest) = read_u16(self.offsets)?;
        self.offsets = rest;
        self.remaining -= 1;

        let input = get_subslice(self.data_16_bit, get_16_bit_offset(offset as usize)..)?;
        let de = Resource16BitDeserializer::new(input);
        seed.deserialize(de).map(Some)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.remaining)
    }
}

/// The `ArraySeqAccess` struct provides deserialization for resources of type
/// `Array`.
///
/// See [`ResourceReprType`] for more details.
struct ArraySeqAccess<'a, 'de: 'a> {
    de: &'a mut ResourceTreeDeserializer<'de>,
    descriptors: &'de [u8],
    remaining: usize,
}

impl<'de, 'a> de::SeqAccess<'de> for ArraySeqAccess<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        if self.remaining == 0 {
            return Ok(None);
        }

        // Elements are stored as a sequence of resource descriptors. Pop one
        // and deserialize the corresponding resource.
        let input = self.descriptors;
        self.descriptors = get_subslice(self.descriptors, get_32_bit_offset(1)..)?;
        self.remaining -= 1;

        // Input must always start at a resource descriptor. The rest of the
        // input is immaterial in this case, as we will return to this function
        // for the next descriptor.
        self.de.input = input;
        seed.deserialize(&mut *self.de).map(Some)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.remaining)
    }
}

/// The `IntVectorSeqAccess` struct provides deserialization for resources of
/// type `IntVector`.
///
/// See [`ResourceReprType`] for more details.
struct IntVectorSeqAccess<'de> {
    values: &'de [u8],
    remaining: usize,
}

impl<'de> de::SeqAccess<'de> for IntVectorSeqAccess<'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        if self.remaining == 0 {
            return Ok(None);
        }

        // Elements are stored as a sequence of 32-bit integers. Pop one and
        // feed it into the specialized int vector deserializer.
        let input = self.values;
        self.values = get_subslice(self.values, get_32_bit_offset(1)..)?;
        self.remaining -= 1;

        let de = IntVectorDeserializer::new(input);
        seed.deserialize(de).map(Some)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.remaining)
    }
}

/// The `EmptySeqAccess` struct provides for deserialization of any empty
/// array resource, including `IntVector` and string types.
struct EmptySeqAccess;

impl<'de> de::SeqAccess<'de> for EmptySeqAccess {
    type Error = Error;

    fn next_element_seed<T>(&mut self, _seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        Ok(None)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(0)
    }
}

/// The `EmptyMapAccess` struct provides for deserialization of any empty
/// table resource.
struct EmptyMapAccess;

impl<'de> de::MapAccess<'de> for EmptyMapAccess {
    type Error = Error;

    fn next_key_seed<K>(&mut self, _seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        Ok(None)
    }

    fn next_value_seed<V>(&mut self, _seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        panic!("Unable to process value for empty map. This is likely a `serde` bug.");
    }

    fn size_hint(&self) -> Option<usize> {
        Some(0)
    }
}

/// The `TableMapAccess` struct provides deserialization for resources of type
/// `Table`.
///
/// See [`ResourceReprType`] for more details.
struct TableMapAccess<'de, 'a> {
    de: &'a mut ResourceTreeDeserializer<'de>,
    keys: &'de [u8],
    values: &'de [u8],
    remaining: usize,
}

impl<'de, 'a> de::MapAccess<'de> for TableMapAccess<'de, 'a> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.remaining == 0 {
            return Ok(None);
        }

        // Keys are stored as a sequence of byte offsets into the key block. Pop
        // one and feed it into the specialized key deserializer.
        let (key, keys) = read_u16(self.keys)?;
        self.keys = keys;
        self.remaining -= 1;

        let input = get_subslice(self.de.keys, key as usize..).or(Err(Error::new_from_string(
            format!("Unable to deserialize key at offset {key}."),
        )))?;

        let de = KeyDeserializer::new(input);
        seed.deserialize(de).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        // Values are stored as a sequence of resource descriptors. Pop one and
        // deserialize the corresponding resource.
        let value = self.values;
        self.values = get_subslice(self.values, get_32_bit_offset(1)..)?;

        self.de.input = value;
        seed.deserialize(&mut *self.de)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.remaining)
    }
}

/// The `Resource16BitDeserializer` struct processes resources which are a part
/// of the 16-bit data block of the resource bundle. A resource will be in the
/// 16-bit data block if and only if it is a `StringV2`.
pub struct Resource16BitDeserializer<'de> {
    input: &'de [u8],
}

impl<'de> Resource16BitDeserializer<'de> {
    fn new(input: &'de [u8]) -> Self {
        Self { input }
    }

    /// Reads a UTF-16 string from the 16-bit data block.
    fn read_string_v2(self) -> Result<String, Error> {
        let (length, input) = get_length_and_start_of_utf16_string(self.input)?;

        let byte_slices = input.chunks_exact(2).take(length);
        if byte_slices.len() != length {
            // `take()` will silently return fewer elements than requested if
            // the input is too short, but that's an error during deserialize.
            return Err(Error::new("Unexpected end of input while reading string."));
        }

        let units = byte_slices.map(|bytes| {
            // We can safely unwrap as we guarantee above that this chunk is
            // exactly 2 bytes.
            let bytes = <[u8; 2]>::try_from(bytes).unwrap();
            u16::from_ne_bytes(bytes)
        });

        char::decode_utf16(units)
            .collect::<Result<String, _>>()
            .map_err(|_| Error::new("Resource bundle contains invalid string."))
    }
}

impl<'de> de::Deserializer<'de> for Resource16BitDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // The only type which can be wholly represented in the 16-bit data
        // block is `StringV2`.
        self.deserialize_string(visitor)
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str
        byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // Because `StringV2` is stored as UTF-16, we can't borrow it as a
        // `&str`. If zero-copy is desired, an appropriate data structure such
        // as `ZeroVec` will be needed.
        visitor.visit_string(self.read_string_v2()?)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // `StringV2` is a contiguous sequence of native-endian `u16`, so we can
        // zero-copy deserialize it if the visitor supports it.
        let (length, input) = get_length_and_start_of_utf16_string(self.input)?;
        let bytes = get_subslice(input, 0..get_16_bit_offset(length))?;

        visitor.visit_borrowed_bytes(bytes)
    }

    fn is_human_readable(&self) -> bool {
        false
    }
}

/// The `IntVectorDeserializer` struct processes an `IntVector` resource,
/// consisting of 32-bit integers.
pub struct IntVectorDeserializer<'de> {
    input: &'de [u8],
}

impl<'de> IntVectorDeserializer<'de> {
    fn new(input: &'de [u8]) -> Self {
        Self { input }
    }

    /// Reads a 32-bit integer from the current input as signed.
    fn read_signed(mut self) -> Result<i32, Error> {
        let (value, next) = read_u32(self.input)?;
        self.input = next;

        Ok(value as i32)
    }

    /// Reads a 32-bit integer from the current input as unsigned.
    fn read_unsigned(mut self) -> Result<u32, Error> {
        let (value, next) = read_u32(self.input)?;
        self.input = next;

        Ok(value)
    }
}

impl<'de> de::Deserializer<'de> for IntVectorDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // The contents of `IntVector`s are always 32-bit integers. We can
        // safely generalize to `u32`, as there's no special handling needed
        // for signed integers.
        self.deserialize_u32(visitor)
    }

    forward_to_deserialize_any! {
        bool f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_i32(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_i32(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i32(self.read_signed()?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_i64(self.read_signed()? as i64)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_u32(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_u32(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u32(self.read_unsigned()?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u64(self.read_unsigned()? as u64)
    }
}

pub struct KeyDeserializer<'de> {
    input: &'de [u8],
}

impl<'de> KeyDeserializer<'de> {
    fn new(input: &'de [u8]) -> Self {
        Self { input }
    }

    /// Reads a key from the current input.
    fn read_key(self) -> Result<&'de str, Error> {
        // Keys are stored as null-terminated UTF-8 strings. Locate the
        // terminating byte and return as a borrowed string.
        let terminator_pos = self.input.position(|byte| byte == 0).ok_or(Error::new(
            "Invalid resource bundle: unterminated key offset.",
        ))?;

        let input = get_subslice(self.input, 0..terminator_pos)?;
        core::str::from_utf8(input)
            .map_err(|_| Error::new("Invalid resource bundle: key is not valid UTF-8."))
    }
}

impl<'de> de::Deserializer<'de> for KeyDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.read_key()?)
    }
}

/// Determines the length in units of a serialized UTF-16 string.
///
/// Returns the length of the string and a slice beginning at the first unit.
fn get_length_and_start_of_utf16_string(input: &[u8]) -> Result<(usize, &[u8]), Error> {
    let (first, rest) = read_u16(input)?;

    let (length, rest) = if (0xdc00..0xdfef).contains(&first) {
        // The unit is the entire length marker.
        ((first & 0x03ff) as usize, rest)
    } else if (0xdfef..0xdfff).contains(&first) {
        // The unit is the first of a 2-unit length marker.
        let (second, rest) = read_u16(rest)?;

        (((first as usize - 0xdfef) << 16) | second as usize, rest)
    } else if first == 0xdfff {
        // The unit is the first of a 3-unit length marker.
        let (second, rest) = read_u16(rest)?;
        let (third, rest) = read_u16(rest)?;

        (((second as usize) << 16) | third as usize, rest)
    } else {
        // The string has implicit length. These are strings of at least 1
        // and at most 40 units.
        let length = rest
            .chunks_exact(2)
            .take(40)
            .position(|chunk| chunk == [0, 0])
            .ok_or(Error::new(
                "Invalid resource bundle: unterminated implicit-length string.",
            ))?
            + 1;

        (length, input)
    };

    Ok((length, rest))
}

impl de::StdError for Error {}

impl de::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: core::fmt::Display,
    {
        Error::new_from_string(format!("{msg}"))
    }
}
