use std::{
    fs::File,
    io::{BufReader, Read},
};

use icu_resb::{binary, text, Resource, Table};

fn assert_arrays_eq(l: &Vec<Resource>, r: &Vec<Resource>) {
    assert_eq!(l.len(), r.len(), "Array lengths do not match.");

    let mut l_iter = l.iter();
    let mut r_iter = r.iter();

    while let Some((l_resource, r_resource)) = l_iter.next().zip(r_iter.next()) {
        assert_resources_eq(l_resource, r_resource);
    }
}

fn assert_tables_eq(l: &Table, r: &Table) {
    assert_eq!(l.len(), r.len(), "Table lengths do not match.");

    let mut l_resources = l.iter();
    let mut r_resources = r.iter();

    while let Some(((l_key, l_resource), (r_key, r_resource))) =
        l_resources.next().zip(r_resources.next())
    {
        assert_eq!(l_key, r_key, "Keys do not match.");

        assert_resources_eq(l_resource, r_resource);
    }
}

fn assert_resources_eq(l: &Resource, r: &Resource) {
    match (l, r) {
        (Resource::String(l), Resource::String(r)) => assert_eq!(l, r, "Strings do not match."),
        (Resource::Array(l), Resource::Array(r)) => assert_arrays_eq(l, r),
        (Resource::Table(l), Resource::Table(r)) => assert_tables_eq(l, r),
        (Resource::Binary(l), Resource::Binary(r)) => {
            assert_eq!(l, r, "Binary data does not match.")
        }
        (Resource::Integer(l), Resource::Integer(r)) => {
            assert_eq!(l, r, "Integer values do not match.")
        }
        (Resource::IntVector(l), Resource::IntVector(r)) => {
            assert_eq!(l, r, "Int vectors do not match.")
        }
        _ => panic!("Found non-matching resources {:?} and {:?}.", l, r),
    };
}

fn main() {
    let input = File::open("zoneinfo64.res");
    let mut reader = BufReader::new(input.unwrap());

    let mut in_bytes = Vec::new();
    match reader.read_to_end(&mut in_bytes) {
        Ok(_) => (),
        Err(err) => panic!("Unable to read file: {}", err),
    };

    let bin_bundle =
        binary::Reader::read("zoneinfo64", &in_bytes).expect("Failed to read binary bundle.");

    let input = File::open("zoneinfo64.txt");
    let mut reader = BufReader::new(input.unwrap());

    let mut in_string = String::new();
    match reader.read_to_string(&mut in_string) {
        Ok(_) => (),
        Err(err) => panic!("Unable to read file: {}", err),
    };

    let (text_bundle, _) = text::Reader::read(&in_string).expect("Failed to read text bundle.");

    if let (Resource::Table(bin_table), Resource::Table(text_table)) =
        (bin_bundle.root(), text_bundle.root())
    {
        assert_tables_eq(bin_table, text_table);
    }

    assert_resources_eq(bin_bundle.root(), text_bundle.root());

    println!("Success! The binary and text bundles matched.")
}
