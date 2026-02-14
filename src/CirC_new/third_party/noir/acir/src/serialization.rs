//! Serialization formats used for the bytecode and the witness stack.
use serde::{Deserialize, Serialize};

/// Serialize a value using `bincode`, based on `serde`.
///
/// This format is compact, but provides no backwards compatibility.
pub(crate) fn bincode_serialize<T: Serialize>(value: &T) -> std::io::Result<Vec<u8>> {
    bincode::serialize(value).map_err(std::io::Error::other)
}

/// Deserialize a value using `bincode`, based on `serde`.
pub(crate) fn bincode_deserialize<T: for<'a> Deserialize<'a>>(buf: &[u8]) -> std::io::Result<T> {
    bincode::deserialize(buf).map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidInput, e))
}
