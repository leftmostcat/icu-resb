#![cfg_attr(not(feature = "std"), no_std)]

use core::fmt;

extern crate alloc;
use alloc::borrow::Cow;

pub mod binary;

#[cfg(feature = "tools")]
pub mod text;

#[cfg(feature = "std")]
pub mod bundle;

const MASK_28_BIT: u32 = 0x0fff_ffff;

/// The `Error` struct provides basic error handling for serialization and
/// deserialization of the ICU resource bundle formats.
#[derive(Debug)]
pub struct Error {
    message: Cow<'static, str>,
}

impl Error {
    pub fn new(message: &'static str) -> Self {
        Self {
            message: Cow::from(message),
        }
    }

    pub fn new_from_string(message: String) -> Self {
        Self {
            message: Cow::from(message),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
