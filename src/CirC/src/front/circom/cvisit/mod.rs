//! Circom AST visitors and traversal

pub mod cvmut;
pub mod walkfns;
pub mod cstmtwalker;

// Re-export the most commonly used items
pub use cvmut::CircomVisitorMut;

/// Error type for Circom visitors
pub struct CircomVisitorError(pub String);

/// Result type for Circom visitors
pub type CircomResult<T> = Result<T, CircomVisitorError>;

/// Visitor result type for Circom visitors
pub type CircomVisitorResult = CircomResult<()>;

impl From<String> for CircomVisitorError {
    fn from(f: String) -> Self {
        Self(f)
    }
}