use std::{
    char::DecodeUtf16Error,
    collections::HashMap,
    fmt::Debug,
    fs::File,
    io::{BufReader, Read},
    marker::PhantomData,
};

use serde::{
    de::{self, Visitor},
    Deserialize, Serialize,
};

use zerovec::ZeroVec;

#[derive(Deserialize, Serialize)]
#[serde(transparent)]
struct ZeroUTF16String<'a> {
    #[serde(borrow)]
    units: ZeroVec<'a, u16>,
}

impl ZeroUTF16String<'_> {
    /// Gets the count of units in the string.
    ///
    /// This value does not necessarily equal the length of the string in
    /// characters, as characters outside the Basic Multilingual Plane are
    /// represented by 2 units.
    pub fn len(&self) -> usize {
        self.units.len()
    }

    /// Gets an iterator for the units of the string.
    ///
    /// See `len` for details on why this does not correspond to characters.
    pub fn iter(&self) -> impl Iterator<Item = u16> + '_ {
        self.units.iter()
    }
}

impl TryFrom<ZeroUTF16String<'_>> for String {
    type Error = DecodeUtf16Error;

    fn try_from(value: ZeroUTF16String<'_>) -> Result<Self, Self::Error> {
        char::decode_utf16(value.iter()).collect::<Result<String, _>>()
    }
}

impl Debug for ZeroUTF16String<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decoded = char::decode_utf16(self.iter())
            .map(|r| r.unwrap_or(char::REPLACEMENT_CHARACTER))
            .collect::<String>();
        write!(f, "{}", decoded)
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
struct TzDataRuleData<'a> {
    #[serde(borrow)]
    type_offsets: ZeroVec<'a, i32>,
    #[serde(borrow)]
    trans: Option<ZeroVec<'a, i32>>,
    #[serde(borrow)]
    trans_pre32: Option<ZeroVec<'a, i32>>,
    #[serde(borrow)]
    trans_post32: Option<ZeroVec<'a, i32>>,
    type_map: Option<&'a [u8]>,
    #[serde(borrow)]
    final_rule: Option<ZeroUTF16String<'a>>,
    final_raw: Option<i32>,
    final_year: Option<u32>,
    #[serde(borrow)]
    links: Option<ZeroVec<'a, u32>>,
}

#[derive(Debug)]
enum TzDataRule<'a> {
    Table(Box<TzDataRuleData<'a>>),
    Int(u32),
}

impl<'de: 'a, 'a> Deserialize<'de> for TzDataRule<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(TzDataRuleEnumVisitor {
            phantom: PhantomData,
        })
    }
}

struct TzDataRuleEnumVisitor<'a> {
    phantom: PhantomData<TzDataRule<'a>>,
}

impl<'de: 'a, 'a> Visitor<'de> for TzDataRuleEnumVisitor<'a> {
    type Value = TzDataRule<'a>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        todo!()
    }

    fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(TzDataRule::Int(v))
    }

    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let value = TzDataRuleData::deserialize(de::value::MapAccessDeserializer::new(map))?;

        Ok(TzDataRule::Table(Box::new(value)))
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename = "zoneinfo64")]
#[serde(rename_all = "PascalCase")]
struct ZoneInfo64<'a> {
    #[serde(borrow)]
    zones: Vec<TzDataRule<'a>>,
    #[serde(borrow)]
    names: Vec<ZeroUTF16String<'a>>,
    #[serde(borrow)]
    rules: HashMap<&'a str, ZeroVec<'a, i32>>,
    #[serde(borrow)]
    regions: Vec<ZeroUTF16String<'a>>,
}

fn main() {
    let input = File::open("zoneinfo64.res");
    let mut reader = BufReader::new(input.unwrap());

    let mut in_bytes = Vec::new();
    match reader.read_to_end(&mut in_bytes) {
        Ok(_) => (),
        Err(err) => panic!("Unable to read file: {}", err),
    };

    let out = icu_resb::binary::from_bytes::<ZoneInfo64>(&in_bytes).expect("Error processing file");

    println!("{:#?}", out);
}
