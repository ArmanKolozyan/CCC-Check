//! One-way function (OWF) extension operator.
//!
//! Represents an opaque hash/OWF node in the IR. Used by the datan
//! backend to block taint propagation across one-way functions.

use crate::ir::term::ty::TypeErrorReason;
use crate::ir::term::{Sort, Value};

/// Type-check [super::ExtOp::Owf].
pub fn check(arg_sorts: &[&Sort]) -> Result<Sort, TypeErrorReason> {
    if arg_sorts.is_empty() {
        return Err(TypeErrorReason::Custom(
            "Owf requires at least one argument".to_string(),
        ));
    }
    if arg_sorts.len() == 1 {
        Ok((*arg_sorts[0]).clone())
    } else {
        Ok(Sort::Tuple(
            arg_sorts.iter().map(|s| (*s).clone()).collect(),
        ))
    }
}

/// Evaluate [super::ExtOp::Owf].
pub fn eval(_args: &[&Value]) -> Value {
    panic!(
        "Cannot evaluate OWF node concretely. \
         OWF is an analysis abstraction; disable --use-owf for execution."
    )
}
