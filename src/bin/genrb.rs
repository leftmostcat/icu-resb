use std::{
    fs::File,
    io::{BufReader, BufWriter, Read, Write},
};

use icu_resb::{binary, text};

fn main() {
    let input = File::open("zoneinfo64.txt");
    let mut reader = BufReader::new(input.unwrap());

    let mut in_string = String::new();
    match reader.read_to_string(&mut in_string) {
        Ok(_) => (),
        Err(err) => panic!("Unable to read file: {}", err),
    };

    let (in_bundle, keys_in_discovery_order) = match text::Reader::read(&in_string) {
        Ok(result) => result,
        Err(err) => panic!("Failed to parse text bundle:\n{}", err.message()),
    };

    let file = File::create("zoneinfo64.res");
    let mut writer = BufWriter::new(file.unwrap());

    let bytes = binary::Writer::write(&in_bundle, &keys_in_discovery_order)
        .expect("Failed to generate binary bundle.");

    writer
        .write_all(&bytes)
        .expect("Failed to write binary bundle.");
}
