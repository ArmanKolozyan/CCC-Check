//! AST Walker for circom_pest_ast

use super::super::CircomGen;
use super::cvmut::CircomVisitorMut;
use super::walkfns::*;

use fxhash::{FxHashMap as HashMap, FxHashSet as HashSet};
use circom_pest_ast as ast;
use crate::ir::term::{Term, Op, Sort, leaf_term, term};
use circ_hc::Node;
use circom_pest_ast::{Expression, Number};
use crate::front::circom::term::*;
use crate::front::circom::term::{T, Ty};
use crate::cfg::cfg;

/// Circom has only a few types
#[derive(Debug, Clone, PartialEq)]
pub enum CircomType {
    /// Field element (default in Circom)
    Field,
    /// Array of elements
    Array(Box<CircomType>, usize),
    /// Signal type (input, output, intermediate)
    Signal,
    /// Component type
    Component(String),
}

/// Compile-time values for var variables
/// In Circom, `var` variables can hold either concrete values or symbolic expressions
#[derive(Debug, Clone)]
pub enum CompileTimeValue {
    /// Scalar integer value (using BigInt for proper arithmetic)
    Scalar(rug::Integer),
    /// 1D array of integers
    Array1D(Vec<rug::Integer>),
    /// 2D array of integers
    Array2D(Vec<Vec<rug::Integer>>),
    /// Multi-dimensional array (for 3D+)
    ArrayND(Vec<CompileTimeValue>),
    /// Symbolic expression (IR term) - used when value depends on signals
    /// This enables symbolic execution: var x = func(signal) stores the expression
    Expression(T),
    /// 1D array of expressions - used when array elements depend on signals
    ExprArray1D(Vec<T>),
}

// Custom PartialEq since rug::Integer doesn't derive it automatically
impl PartialEq for CompileTimeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (CompileTimeValue::Scalar(a), CompileTimeValue::Scalar(b)) => a == b,
            (CompileTimeValue::Array1D(a), CompileTimeValue::Array1D(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x == y)
            }
            (CompileTimeValue::Array2D(a), CompileTimeValue::Array2D(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(row_a, row_b)| {
                    row_a.len() == row_b.len() && row_a.iter().zip(row_b.iter()).all(|(x, y)| x == y)
                })
            }
            (CompileTimeValue::ArrayND(a), CompileTimeValue::ArrayND(b)) => a == b,
            // Expressions can't be easily compared for equality - conservative approach
            (CompileTimeValue::Expression(_), CompileTimeValue::Expression(_)) => false,
            (CompileTimeValue::ExprArray1D(_), CompileTimeValue::ExprArray1D(_)) => false,
            _ => false,
        }
    }
}

impl CompileTimeValue {
    /// Create a scalar value
    pub fn scalar(val: i64) -> Self {
        CompileTimeValue::Scalar(rug::Integer::from(val))
    }

    /// Create a scalar value from a BigInt
    pub fn scalar_big(val: rug::Integer) -> Self {
        CompileTimeValue::Scalar(val)
    }

    /// Create a 1D array with given size, initialized to 0
    pub fn array_1d(size: usize) -> Self {
        CompileTimeValue::Array1D(vec![rug::Integer::from(0); size])
    }

    /// Create a 2D array with given dimensions, initialized to 0
    pub fn array_2d(rows: usize, cols: usize) -> Self {
        CompileTimeValue::Array2D(vec![vec![rug::Integer::from(0); cols]; rows])
    }

    /// Get scalar value as i64 if this is a scalar and fits in i64
    pub fn as_scalar(&self) -> Option<i64> {
        match self {
            CompileTimeValue::Scalar(val) => val.to_i64(),
            _ => None,
        }
    }

    /// Get scalar value as Integer reference
    pub fn as_integer(&self) -> Option<&rug::Integer> {
        match self {
            CompileTimeValue::Scalar(val) => Some(val),
            _ => None,
        }
    }

    /// Convert to IR term (field literal or expression)
    pub fn to_term(&self) -> T {
        match self {
            CompileTimeValue::Scalar(val) => {
                // Use field_lit with Integer directly
                // field_lit can handle rug::Integer through the Integer: From<I> trait
                field_lit(val.clone())
            },
            CompileTimeValue::Expression(term) => {
                // Already an IR term - return it directly
                term.clone()
            },
            // Arrays should not be converted directly to terms - only their elements
            _ => panic!("Cannot convert array CompileTimeValue to single term"),
        }
    }

    /// Try to create from a term - returns Expression variant if not a simple constant
    pub fn try_from_term(term: &T) -> Option<Self> {
        use crate::front::circom::term::try_const_value;
        use crate::ir::term::Value;

        // Special handling for array terms: extract element values recursively.
        if let Ty::Array(_, elem_ty) = term.type_() {
            if matches!(term.term.op(), Op::Array(_)) {
                let mut elements: Vec<CompileTimeValue> = Vec::new();
                for child in term.term.cs() {
                    let child_term = T::new((**elem_ty).clone(), child.clone());
                    if let Some(val) = CompileTimeValue::try_from_term(&child_term) {
                        elements.push(val);
                    } else {
                        return Some(CompileTimeValue::Expression(term.clone()));
                    }
                }

                // 1D array of scalars
                if elements.iter().all(|v| matches!(v, CompileTimeValue::Scalar(_))) {
                    let vals = elements
                        .into_iter()
                        .map(|v| match v {
                            CompileTimeValue::Scalar(i) => i,
                            _ => unreachable!(),
                        })
                        .collect();
                    return Some(CompileTimeValue::Array1D(vals));
                }

                // 2D array of scalars (array of Array1D)
                if elements.iter().all(|v| matches!(v, CompileTimeValue::Array1D(_))) {
                    let vals = elements
                        .into_iter()
                        .map(|v| match v {
                            CompileTimeValue::Array1D(row) => row,
                            _ => unreachable!(),
                        })
                        .collect();
                    return Some(CompileTimeValue::Array2D(vals));
                }

                // 1D array of expressions/scalars
                if elements
                    .iter()
                    .all(|v| matches!(v, CompileTimeValue::Scalar(_) | CompileTimeValue::Expression(_)))
                {
                    let elems: Vec<T> = elements.into_iter().map(|v| v.to_term()).collect();
                    return Some(CompileTimeValue::ExprArray1D(elems));
                }

                // Fallback for nested/heterogeneous arrays
                return Some(CompileTimeValue::ArrayND(elements));
            }

            // Array variable that isn't a literal: treat as expression
            return Some(CompileTimeValue::Expression(term.clone()));
        }

        // Try to evaluate as constant
        match try_const_value(&term.term) {
            Some(Value::Field(f)) => {
                let value = f.i().clone();
                Some(CompileTimeValue::Scalar(value))
            }
            _ => Some(CompileTimeValue::Expression(term.clone())),
        }
    }

    /// Convert any compile-time value (including arrays) to an IR term.
    /// Arrays are converted to nested array terms recursively.
    pub fn to_term_any(&self) -> T {
        use crate::front::circom::term::array;

        match self {
            CompileTimeValue::Scalar(val) => field_lit(val.clone()),
            CompileTimeValue::Expression(term) => term.clone(),
            CompileTimeValue::Array1D(arr) => {
                let elems: Vec<T> = arr.iter().map(|v| field_lit(v.clone())).collect();
                array(elems).unwrap()
            }
            CompileTimeValue::Array2D(arr2d) => {
                let rows: Vec<T> = arr2d
                    .iter()
                    .map(|row| {
                        let elems: Vec<T> = row.iter().map(|v| field_lit(v.clone())).collect();
                        array(elems).unwrap()
                    })
                    .collect();
                array(rows).unwrap()
            }
            CompileTimeValue::ExprArray1D(arr) => array(arr.clone()).unwrap(),
            CompileTimeValue::ArrayND(arr) => {
                let elems: Vec<T> = arr.iter().map(|v| v.to_term_any()).collect();
                array(elems).unwrap()
            }
        }
    }

    /// Check if this value is a concrete constant (not an expression)
    pub fn is_concrete(&self) -> bool {
        matches!(self, CompileTimeValue::Scalar(_) |
                      CompileTimeValue::Array1D(_) |
                      CompileTimeValue::Array2D(_) |
                      CompileTimeValue::ArrayND(_))
    }

    /// Check if this value is an expression (depends on signals)
    pub fn is_expression(&self) -> bool {
        matches!(self, CompileTimeValue::Expression(_) |
                      CompileTimeValue::ExprArray1D(_))
    }

    /// Perform arithmetic operation (either compile-time or symbolic)
    pub fn apply_op(&self, op: &str, other: &Self) -> Option<Self> {
        use crate::front::circom::term::{add, sub, mul, div, rem};

        // If either operand is an expression, build an IR expression tree
        if self.is_expression() || other.is_expression() {
            let left_term = self.to_term();
            let right_term = other.to_term();

            let result_term = match op {
                "+" => add(left_term.clone(), right_term.clone()).unwrap_or_else(|e| {
                    panic!(
                        "Addition operation failed during compile-time evaluation\n\
                         \n\
                         Error: {}\n\
                         \n\
                         Left operand: {:?}\n\
                         Right operand: {:?}\n\
                         \n\
                         This indicates an issue with the IR term operations.",
                        e, left_term, right_term
                    )
                }),
                "-" => sub(left_term.clone(), right_term.clone()).unwrap_or_else(|e| {
                    panic!(
                        "Subtraction operation failed during compile-time evaluation\n\
                         \n\
                         Error: {}\n\
                         \n\
                         Left operand: {:?}\n\
                         Right operand: {:?}\n\
                         \n\
                         This indicates an issue with the IR term operations.",
                        e, left_term, right_term
                    )
                }),
                "*" => mul(left_term.clone(), right_term.clone()).unwrap_or_else(|e| {
                    panic!(
                        "Multiplication operation failed during compile-time evaluation\n\
                         \n\
                         Error: {}\n\
                         \n\
                         Left operand: {:?}\n\
                         Right operand: {:?}\n\
                         \n\
                         This indicates an issue with the IR term operations.",
                        e, left_term, right_term
                    )
                }),
                "/" => div(left_term.clone(), right_term.clone()).unwrap_or_else(|e| {
                    panic!(
                        "Division operation failed during compile-time evaluation\n\
                         \n\
                         Error: {}\n\
                         \n\
                         Left operand: {:?}\n\
                         Right operand: {:?}\n\
                         \n\
                         Common causes:\n\
                         1. Division by zero (check if right operand evaluates to 0)\n\
                         2. Invalid operand types\n\
                         \n\
                         Debugging steps:\n\
                         1. Check if divisor can be zero at compile time\n\
                         2. Ensure both operands are valid field elements",
                        e, left_term, right_term
                    )
                }),
                "%" => rem(left_term.clone(), right_term.clone()).unwrap_or_else(|e| {
                    panic!(
                        "Modulo operation failed during compile-time evaluation\n\
                         \n\
                         Error: {}\n\
                         \n\
                         Left operand: {:?}\n\
                         Right operand: {:?}\n\
                         \n\
                         Common causes:\n\
                         1. Modulo by zero (check if right operand evaluates to 0)\n\
                         2. Invalid operand types",
                        e, left_term, right_term
                    )
                }),
                _ => return None, // Bitwise ops not supported on expressions
            };

            return Some(CompileTimeValue::Expression(result_term));
        }

        // Both operands are concrete - do compile-time evaluation
        if let (Some(a), Some(b)) = (self.as_integer(), other.as_integer()) {
            let result = match op {
                "+" => rug::Integer::from(a + b),
                "-" => rug::Integer::from(a - b),
                "*" => rug::Integer::from(a * b),
                "/" => {
                    if b == &rug::Integer::from(0) {
                        panic!(
                            "Division by zero in compile-time constant evaluation\n\
                             \n\
                             Left operand (dividend): {}\n\
                             Right operand (divisor): {}\n\
                             \n\
                             This error occurs when attempting to divide by zero during\n\
                             compile-time constant evaluation.\n\
                             \n\
                             Common causes:\n\
                             1. Divisor variable evaluates to 0\n\
                             2. Expression simplification results in 0 divisor\n\
                             3. Template parameter passed as 0\n\
                             \n\
                             Debugging steps:\n\
                             1. Check the value of the divisor expression\n\
                             2. Ensure template parameters are non-zero\n\
                             3. Add assertions to validate divisor is non-zero\n\
                             4. Review arithmetic expressions for potential zero results",
                            a, b
                        );
                    }
                    rug::Integer::from(a / b)
                }
                "%" => {
                    if b == &rug::Integer::from(0) {
                        panic!(
                            "Modulo by zero in compile-time constant evaluation\n\
                             \n\
                             Left operand: {}\n\
                             Right operand (modulus): {}\n\
                             \n\
                             This error occurs when attempting to compute modulo with\n\
                             a zero divisor during compile-time constant evaluation.\n\
                             \n\
                             Common causes:\n\
                             1. Modulus variable evaluates to 0\n\
                             2. Expression simplification results in 0 modulus\n\
                             3. Template parameter passed as 0\n\
                             \n\
                             Debugging steps:\n\
                             1. Check the value of the modulus expression\n\
                             2. Ensure template parameters are non-zero\n\
                             3. Add assertions to validate modulus is non-zero",
                            a, b
                        );
                    }
                    rug::Integer::from(a % b)
                }
                "&" => rug::Integer::from(a & b),
                "|" => rug::Integer::from(a | b),
                "^" => rug::Integer::from(a ^ b),
                "<<" => {
                    if let Some(shift) = b.to_u32() {
                        rug::Integer::from(a << shift)
                    } else {
                        return None;
                    }
                }
                ">>" => {
                    if let Some(shift) = b.to_u32() {
                        rug::Integer::from(a >> shift)
                    } else {
                        return None;
                    }
                }
                _ => return None,
            };
            Some(CompileTimeValue::Scalar(result))
        } else {
            None // Can't do operations on arrays
        }
    }
}

/// Processes Circom AST and generates IR terms
pub struct CircomStatementWalker<'ast, 'ret> {
    /// Circom code generator reference
    circom_gen: &'ret CircomGen<'ast>,
    /// Current variable types
    vars: HashMap<String, CircomType>,
    /// Variable values (for var declarations) - evaluated at compile-time
    var_values: HashMap<String, CompileTimeValue>,
    /// Component signal variables: component_name -> (signal_name -> variable_name)
    component_signals: HashMap<String, HashMap<String, String>>,
    /// Component signal types: component_name -> (signal_name -> type)
    /// Used to track array sizes for component signals
    component_signal_types: HashMap<String, HashMap<String, CircomType>>,
    /// Generated constraints
    constraints: Vec<Term>,
    /// Output signal names
    output_signals: Vec<String>,
    /// Track which variables are signals (not vars) - used for signal array flattening
    signal_names: HashSet<String>,
    /// Current component context for signal name resolution
    /// When set, unqualified signal names are automatically prefixed with this component name
    current_component: Option<String>,
    /// Public signal names (from main component {public [signal_list]})
    public_signals: HashSet<String>,
    /// Stack of local variable scopes for function inlining (tracks declared var/param names)
    function_scopes: Vec<HashSet<String>>,
    /// Flag indicating a return statement was executed (for nested returns)
    has_returned: bool,
    /// Value from the most recent return statement
    function_return_value: Option<T>,
    /// Signal tags: maps IR variable name -> list of (tag_name, optional_value) pairs
    /// e.g., [("binary", None)] or [("maxbit", Some(8))]
    signal_tags: HashMap<String, Vec<(String, Option<rug::Integer>)>>,
}

impl<'ast, 'ret: 'ast> CircomStatementWalker<'ast, 'ret> {
    /// Create a new walker
    pub fn new(circom_gen: &'ret CircomGen<'ast>, public_signals: HashSet<String>) -> Self {
        Self {
            circom_gen,
            vars: HashMap::default(),
            var_values: HashMap::default(),
            component_signals: HashMap::default(),
            component_signal_types: HashMap::default(),
            constraints: Vec::new(),
            output_signals: Vec::new(),
            signal_names: HashSet::default(),
            current_component: None,
            public_signals,
            function_scopes: Vec::new(),
            has_returned: false,
            function_return_value: None,
            signal_tags: HashMap::default(),
        }
    }

    /// Ensure a variable is registered in the computation metadata
    /// This must be called before any variable reference is used in IR terms
    fn ensure_var_in_metadata(&self, var_name: &str, sort: &Sort) {
        use crate::ir::term::VariableMetadata;

        // Keep the circify borrow alive
        let circ_borrow = self.circom_gen.circ.borrow();
        let cir_ctx = circ_borrow.cir_ctx();
        let mut cs = cir_ctx.cs.borrow_mut();

        // Check if the variable already exists in metadata
        if !cs.metadata.is_input(var_name) {
            // Create metadata entry for this variable
            let metadata = VariableMetadata {
                name: var_name.to_string(),
                sort: sort.clone(),
                vis: Some(0), // Private by default (PROVER_ID = 0)
                round: 0,
                random: false,
                committed: false,
            };

            cs.metadata.new_input_from_meta(metadata);
        }
    }

    /// Declare an input signal, ignoring duplicate-name (Rebind) errors.
    /// Panics on any other error from `declare_input`.
    fn declare_input_ignore_dup(
        &self,
        name: String,
        ty: &Ty,
        visibility: Option<crate::ir::term::PartyId>,
    ) {
        if let Err(e) = self.circom_gen.circ.borrow_mut().declare_input(
            name.clone(), ty, visibility, None, false,
        ) {
            let msg = format!("{}", e);
            if !msg.contains("already declared") {
                panic!("failed to declare input signal '{}': {}", name, e);
            }
        }
    }

    /// Resolve the precompute output name for a signal assignee.
    /// Handles both plain signals (e.g., `out`) and array elements (e.g., `out[i]`).
    /// Returns None for component signal references (dot accesses like `lt.in`).
    fn resolve_signal_precompute_name(
        &mut self,
        assignee: &ast::Assignee<'ast>,
    ) -> Option<String> {
        let base_name = assignee.id.value.clone();

        // Check for dot access (component signal reference) - skip these
        let has_dot = assignee.accesses.iter().any(|a| {
            matches!(a, ast::AssigneeAccess::Dot(_))
        });
        if has_dot {
            return None;
        }

        // Collect array indices
        let mut indices: Vec<i64> = Vec::new();
        for access in &assignee.accesses {
            match access {
                ast::AssigneeAccess::Select(arr) => {
                    if let Some(idx) = self.extract_constant_index_expr(&arr.expression) {
                        indices.push(idx);
                    } else {
                        return None;
                    }
                }
                _ => {}
            }
        }

        // Qualify the base name via component context
        let qualified_base = if let Some(comp_name) = &self.current_component {
            if let Some(signals) = self.component_signals.get(comp_name) {
                if let Some(qualified_name) = signals.get(&base_name) {
                    qualified_name.clone()
                } else {
                    base_name.clone()
                }
            } else {
                base_name.clone()
            }
        } else {
            base_name.clone()
        };

        if indices.is_empty() {
            Some(qualified_base)
        } else {
            // Flatten multi-dimensional index to a single offset
            // Signal arrays are registered as base_flat_idx
            let flat_idx = if indices.len() == 1 {
                indices[0]
            } else {
                // Multi-dimensional: look up dimensions from type info
                if let Some(circom_type) = self.vars.get(&base_name).cloned()
                    .or_else(|| self.vars.get(&qualified_base).cloned())
                {
                    if let Some(dims) = Self::extract_dims_from_circom_type(&circom_type) {
                        Self::calculate_flat_index(&indices, &dims).unwrap_or(indices[0])
                    } else {
                        indices[0]
                    }
                } else {
                    indices[0]
                }
            };
            Some(format!("{}_{}", qualified_base, flat_idx))
        }
    }

    /// Extract array dimensions from an assignee's access list
    /// For example: foo[3][4] returns Some(vec![3, 4])
    /// Supports numeric literals, template parameters, and expressions (e.g., n+1)
    fn extract_array_dimensions(&self, assignee: &ast::Assignee) -> Option<Vec<usize>> {
        // First try to get dimensions from accesses (normal case)
        if !assignee.accesses.is_empty() {
            let mut dims = Vec::new();
            for access in &assignee.accesses {
                if let ast::AssigneeAccess::Select(array_access) = access {
                    // Use extract_constant_value_expr which handles all expression types:
                    // - Numeric literals
                    // - Identifiers (template params, vars)
                    // - Binary expressions (n+1, n*2, etc.)
                    // - Unary expressions (-n, !n, etc.)
                    if let Some(size) = self.extract_constant_value_expr(&array_access.expression) {
                        if size >= 0 {
                            dims.push(size as usize);
                            continue;
                        }
                    }

                    // If we can't evaluate the dimension expression, return None
                    // This happens when processing template definitions where parameters aren't set yet
                    // The dimensions will be resolved later during template instantiation
                    return None;
                }
            }

            if dims.is_empty() {
                None
            } else {
                Some(dims)
            }
        } else {
            // Parser bug workaround: If accesses is empty but the source contains array syntax,
            // try to parse dimensions from the raw span text
            let span_text = assignee.span.as_str();
            self.extract_dimensions_from_span(span_text)
        }
    }

    /// Workaround for parser bug: Extract array dimensions from raw source text
    /// Handles cases like "out[n]" where parser doesn't populate accesses
    fn extract_dimensions_from_span(&self, span_text: &str) -> Option<Vec<usize>> {
        // Look for array dimension syntax: identifier[expr][expr]...
        if !span_text.contains('[') {
            return None;
        }

        let mut dims = Vec::new();
        let mut chars = span_text.chars().peekable();

        // Skip to first '['
        while let Some(ch) = chars.next() {
            if ch == '[' {
                // Extract expression between '[' and ']'
                let mut expr_str = String::new();
                let mut bracket_depth = 1;

                while let Some(ch) = chars.next() {
                    if ch == '[' {
                        bracket_depth += 1;
                        expr_str.push(ch);
                    } else if ch == ']' {
                        bracket_depth -= 1;
                        if bracket_depth == 0 {
                            break;
                        }
                        expr_str.push(ch);
                    } else {
                        expr_str.push(ch);
                    }
                }

                // Try to evaluate the expression
                let expr_str = expr_str.trim();

                // Try simple identifier lookup
                if let Some(CompileTimeValue::Scalar(value)) = self.var_values.get(expr_str) {
                    if let Some(usize_val) = value.to_usize() {
                        dims.push(usize_val);
                        continue;
                    }
                }

                // Try parsing as number
                if let Ok(value) = expr_str.parse::<usize>() {
                    dims.push(value);
                    continue;
                }

                // Array dimensions must be compile-time evaluable
                panic!("Could not evaluate dimension expression '{}'. Array dimensions must be compile-time constant expressions.", expr_str);
            }
        }

        if dims.is_empty() {
            None
        } else {
            Some(dims)
        }
    }

    /// Extract dimensions from a CircomType (inverse of build_circom_array_type)
    fn extract_dims_from_circom_type(ty: &CircomType) -> Option<Vec<usize>> {
        let mut dims = Vec::new();
        let mut current = ty;
        while let CircomType::Array(inner, size) = current {
            dims.push(*size);
            current = inner;
        }
        if dims.is_empty() { None } else { Some(dims) }
    }

    /// Build CircomType from array dimensions
    fn build_circom_array_type(dims: &[usize]) -> CircomType {
        if dims.is_empty() {
            CircomType::Field
        } else {
            // Build from innermost to outermost for CircomType
            let mut result = CircomType::Field;
            for &size in dims.iter().rev() {
                result = CircomType::Array(Box::new(result), size);
            }
            result
        }
    }

    /// Initialize a compile-time array value from CircomType
    fn init_array_value(circom_type: &CircomType) -> Option<CompileTimeValue> {
        match circom_type {
            CircomType::Field | CircomType::Signal | CircomType::Component(_) => None,
            CircomType::Array(_, _) => {
                let dims = Self::extract_dimensions_from_type(circom_type);
                if dims.is_empty() {
                    None
                } else {
                    Some(Self::build_zero_array_value(&dims))
                }
            }
        }
    }

    /// Build a zero-initialized CompileTimeValue for arbitrary dimensions.
    /// Uses Array1D/Array2D for 1D/2D, ArrayND for 3D+.
    fn build_zero_array_value(dims: &[usize]) -> CompileTimeValue {
        match dims.len() {
            0 => CompileTimeValue::Scalar(rug::Integer::from(0)),
            1 => CompileTimeValue::array_1d(dims[0]),
            2 => CompileTimeValue::array_2d(dims[0], dims[1]),
            _ => {
                let mut elems = Vec::with_capacity(dims[0]);
                for _ in 0..dims[0] {
                    elems.push(Self::build_zero_array_value(&dims[1..]));
                }
                CompileTimeValue::ArrayND(elems)
            }
        }
    }

    /// Convert array-like CompileTimeValue into an ArrayND representation.
    /// Returns None if the value is not array-like.
    fn upgrade_array_to_nd(value: CompileTimeValue) -> Option<CompileTimeValue> {
        match value {
            CompileTimeValue::ArrayND(_) => Some(value),
            CompileTimeValue::Array1D(arr) => Some(CompileTimeValue::ArrayND(
                arr.into_iter().map(CompileTimeValue::Scalar).collect(),
            )),
            CompileTimeValue::Array2D(arr2d) => Some(CompileTimeValue::ArrayND(
                arr2d.into_iter().map(CompileTimeValue::Array1D).collect(),
            )),
            CompileTimeValue::ExprArray1D(arr) => Some(CompileTimeValue::ArrayND(
                arr.into_iter().map(CompileTimeValue::Expression).collect(),
            )),
            _ => None,
        }
    }

    /// Update a compile-time array value at the given indices.
    /// Supports nested arrays and upgrades to ArrayND if needed.
    fn update_array_value(
        array_val: &mut CompileTimeValue,
        indices: &[usize],
        rhs: CompileTimeValue,
    ) -> bool {
        if indices.is_empty() {
            *array_val = rhs;
            return true;
        }

        match array_val {
            CompileTimeValue::ArrayND(arr) => {
                let idx = indices[0];
                if idx >= arr.len() {
                    return false;
                }
                if indices.len() == 1 {
                    arr[idx] = rhs;
                    return true;
                }
                return Self::update_array_value(&mut arr[idx], &indices[1..], rhs);
            }
            CompileTimeValue::Array2D(arr2d) => {
                let idx = indices[0];
                if idx >= arr2d.len() {
                    return false;
                }
                if indices.len() == 1 {
                    match rhs {
                        CompileTimeValue::Array1D(row) => {
                            if row.len() != arr2d[idx].len() {
                                return false;
                            }
                            arr2d[idx] = row;
                            return true;
                        }
                        CompileTimeValue::ExprArray1D(_) | CompileTimeValue::ArrayND(_) | CompileTimeValue::Expression(_) => {
                            // Upgrade to ArrayND and retry
                        }
                        _ => return false,
                    }
                } else if indices.len() == 2 {
                    let j = indices[1];
                    if j >= arr2d[idx].len() {
                        return false;
                    }
                    if let Some(val) = rhs.as_integer() {
                        arr2d[idx][j] = val.clone();
                        return true;
                    }
                    // Need to store expression, upgrade to ArrayND
                } else {
                    // Deeper indexing requires ArrayND
                }
            }
            CompileTimeValue::Array1D(arr1d) => {
                let idx = indices[0];
                if idx >= arr1d.len() {
                    return false;
                }
                if indices.len() == 1 {
                    if let Some(val) = rhs.as_integer() {
                        arr1d[idx] = val.clone();
                        return true;
                    }
                    // Need to store expression, upgrade to ArrayND
                } else {
                    // Deeper indexing requires ArrayND
                }
            }
            CompileTimeValue::ExprArray1D(arr) => {
                let idx = indices[0];
                if idx >= arr.len() {
                    return false;
                }
                if indices.len() == 1 {
                    match rhs {
                        CompileTimeValue::Expression(term) => {
                            arr[idx] = term;
                            return true;
                        }
                        CompileTimeValue::Scalar(val) => {
                            arr[idx] = field_lit(val);
                            return true;
                        }
                        _ => {
                            // Upgrade to ArrayND and retry for nested arrays
                        }
                    }
                } else {
                    // Deeper indexing requires ArrayND
                }
            }
            _ => return false,
        }

        // Fallback: upgrade to ArrayND and retry
        let current = std::mem::replace(array_val, CompileTimeValue::Scalar(rug::Integer::from(0)));
        if let Some(mut upgraded) = Self::upgrade_array_to_nd(current.clone()) {
            let ok = Self::update_array_value(&mut upgraded, indices, rhs);
            *array_val = upgraded;
            return ok;
        }
        // Restore original if upgrade failed
        *array_val = current;
        false
    }

    /// Convert CircomType to Ty for IR generation
    fn circom_type_to_ty(ct: &CircomType) -> Ty {
        match ct {
            CircomType::Field => Ty::Field,
            CircomType::Signal => Ty::Field,
            CircomType::Array(elem, size) => {
                let elem_ty = Self::circom_type_to_ty(elem);
                Ty::Array(*size, Box::new(elem_ty))
            }
            CircomType::Component(_) => Ty::Field,
        }
    }

    /// Check if a variable is a signal array (not a var array)
    fn is_signal_array(&self, var_name: &str) -> bool {
        // Check if it's in signal_names set (declared as signal, not var)
        if !self.signal_names.contains(var_name) {
            return false;
        }

        // Check if it's an array type
        if let Some(var_type) = self.vars.get(var_name) {
            matches!(var_type, CircomType::Array(_, _))
        } else {
            false
        }
    }

    /// Extract constant integer from a term (if it's a literal)
    fn extract_constant_from_term(term: &T) -> Option<i64> {
        use crate::front::circom::term::try_const_value;
        use crate::ir::term::Value;

        match try_const_value(&term.term) {
            Some(Value::Field(f)) => f.i().to_i64(),
            _ => None,
        }
    }

    /// Try to extract a compile-time constant index from an expression.
    /// Checks if it's a variable with a compile-time value (e.g., loop variable).
    /// Returns None if not a simple variable or not in var_values.
    fn try_extract_loop_var_value(&self, expr: &ast::Expression) -> Option<i64> {
        // Check if this is a simple variable reference (Identifier or Postfix with no access)
        let var_name = match expr {
            ast::Expression::Identifier(id) => Some(&id.value),
            ast::Expression::Postfix(postfix) => {
                if postfix.access.is_empty() {
                    if let ast::Expression::Identifier(id) = postfix.base.as_ref() {
                        Some(&id.value)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        // Check if this variable has a compile-time value (e.g., loop variable)
        if let Some(name) = var_name {
            if let Some(val) = self.var_values.get(name) {
                if let CompileTimeValue::Scalar(scalar_val) = val {
                    return scalar_val.to_i64();
                }
            }
        }

        None
    }

    /// Get flattened signal name for signal array access with constant index
    /// e.g., arr[0] -> "arr_0", matrix[1] -> "matrix_1"
    fn get_flattened_signal_name(&self, base_name: &str, index: i64) -> String {
        format!("{}_{}", base_name, index)
    }

    /// Build nested array structure from flat list of elements
    /// Given elements [e0, e1, e2, e3, e4, e5] and dimensions [2, 3],
    /// builds [[e0, e1, e2], [e3, e4, e5]]
    fn build_nested_array(&self, elements: Vec<T>, dimensions: &[usize]) -> T {
        use crate::front::circom::term::array;

        if dimensions.is_empty() {
            panic!("build_nested_array: dimensions cannot be empty");
        }

        if dimensions.len() == 1 {
            // Base case: 1D array
            return array(elements).unwrap_or_else(|e| {
                panic!("Failed to create 1D array: {}", e)
            });
        }

        // Recursive case: multi-dimensional array
        let outer_dim = dimensions[0];
        let inner_dims = &dimensions[1..];
        let inner_size: usize = inner_dims.iter().product();

        if elements.len() != outer_dim * inner_size {
            panic!(
                "build_nested_array: element count {} doesn't match dimensions {:?} (expected {})",
                elements.len(),
                dimensions,
                outer_dim * inner_size
            );
        }

        let mut outer_elements = Vec::new();
        for i in 0..outer_dim {
            let start = i * inner_size;
            let end = start + inner_size;
            let inner_elements = elements[start..end].to_vec();
            let inner_array = self.build_nested_array(inner_elements, inner_dims);
            outer_elements.push(inner_array);
        }

        array(outer_elements).unwrap_or_else(|e| {
            panic!("Failed to create outer array: {}", e)
        })
    }

    /// Extract array dimensions from CircomType
    /// Returns the dimensions in order, e.g., for arr[2][3] returns vec![2, 3]
    fn extract_dimensions_from_type(circom_type: &CircomType) -> Vec<usize> {
        let mut dims = Vec::new();
        let mut current = circom_type;

        loop {
            match current {
                CircomType::Array(inner, size) => {
                    dims.push(*size);
                    current = inner;
                }
                _ => break,
            }
        }

        dims
    }

    /// Calculate linear flat index from multi-dimensional indices
    /// For arr[i][j] with dimensions [ops, n], flat_index = i * n + j
    /// For arr[i][j][k] with dimensions [d1, d2, d3], flat_index = i * d2 * d3 + j * d3 + k
    fn calculate_flat_index(indices: &[i64], dimensions: &[usize]) -> Option<i64> {
        if indices.len() != dimensions.len() {
            return None;
        }

        let mut flat_index = 0i64;
        let mut multiplier = 1i64;

        // Process from right to left
        for i in (0..indices.len()).rev() {
            let idx = indices[i];
            let dim = dimensions[i] as i64;

            // Check bounds
            if idx < 0 || idx >= dim {
                return None;
            }

            flat_index += idx * multiplier;
            multiplier *= dim;
        }

        Some(flat_index)
    }

    /// Extract a constant index expression (used for array/component indexing)
    fn extract_constant_index_expr(&mut self, expr: &ast::Expression) -> Option<i64> {
        self.extract_constant_value_expr(expr)
            .or_else(|| self.try_extract_loop_var_value(expr))
    }

    /// Extract constant indices from assignee accesses (component array instantiation)
    fn extract_constant_indices_from_assignee_accesses(
        &mut self,
        accesses: &[ast::AssigneeAccess<'ast>],
    ) -> Option<Vec<i64>> {
        let mut indices = Vec::new();
        for access in accesses {
            match access {
                ast::AssigneeAccess::Select(array_access) => {
                    if let Some(idx) = self.extract_constant_index_expr(&array_access.expression) {
                        indices.push(idx);
                    } else {
                        return None;
                    }
                }
                ast::AssigneeAccess::Dot(_) => {
                    return None;
                }
            }
        }
        Some(indices)
    }

    /// Compute flattened component instance name for component arrays
    fn compute_component_array_instance_name(&self, base_name: &str, indices: &[i64]) -> String {
        let base_type = self.vars.get(base_name).unwrap_or_else(|| {
            panic!(
                "Component array '{}' not found in vars. Components must be declared before use.",
                base_name
            )
        });
        let dims = Self::extract_dimensions_from_type(base_type);
        if dims.is_empty() {
            panic!(
                "Component '{}' is not an array, but was indexed with {:?}",
                base_name, indices
            );
        }
        if indices.len() != dims.len() {
            panic!(
                "Component array '{}' index arity mismatch. Expected {} indices for dimensions {:?}, got {} ({:?}).",
                base_name,
                dims.len(),
                dims,
                indices.len(),
                indices
            );
        }
        let flat = Self::calculate_flat_index(indices, &dims).unwrap_or_else(|| {
            panic!(
                "Component array '{}' index out of bounds. Dimensions {:?}, indices {:?}.",
                base_name, dims, indices
            )
        });
        format!("{}_{}", base_name, flat)
    }

    /// Resolve a component signal access given a qualified component name
    fn resolve_component_signal_access(
        &mut self,
        qualified_comp_name: &str,
        signal_name: &str,
        signal_indices: &[i64],
    ) -> Option<T> {
        let qualified_signal_name = self
            .component_signals
            .get(qualified_comp_name)
            .and_then(|signals| signals.get(signal_name).cloned())
            .unwrap_or_else(|| format!("{}.{}", qualified_comp_name, signal_name));

        let signal_type = self
            .component_signal_types
            .get(qualified_comp_name)
            .and_then(|types| types.get(signal_name).cloned());

        if let Some(sig_ty) = signal_type {
            if matches!(sig_ty, CircomType::Array(_, _)) {
                let dims = Self::extract_dimensions_from_type(&sig_ty);
                if signal_indices.is_empty() {
                    // Return full array
                    let total_size: usize = dims.iter().product();
                    let field_sort = Sort::Field(default_field());
                    let mut elements = Vec::new();
                    for i in 0..total_size {
                        let elem_name = format!("{}_{}", qualified_signal_name, i);
                        self.ensure_var_in_metadata(&elem_name, &field_sort);
                        elements.push(T::new(
                            Ty::Field,
                            leaf_term(Op::new_var(elem_name, field_sort.clone())),
                        ));
                    }
                    let arr = if dims.len() == 1 {
                        array(elements).unwrap()
                    } else {
                        self.build_nested_array(elements, &dims)
                    };
                    return Some(arr);
                }

                let flat = Self::calculate_flat_index(signal_indices, &dims).unwrap_or_else(|| {
                    panic!(
                        "Component signal array {}.{} index out of bounds. Dimensions {:?}, indices {:?}.",
                        qualified_comp_name, signal_name, dims, signal_indices
                    )
                });
                let flattened_name = format!("{}_{}", qualified_signal_name, flat);
                let field_sort = Sort::Field(default_field());
                self.ensure_var_in_metadata(&flattened_name, &field_sort);
                return Some(T::new(
                    Ty::Field,
                    leaf_term(Op::new_var(flattened_name, field_sort)),
                ));
            }
        }

        // Scalar signal
        if !signal_indices.is_empty() {
            panic!(
                "Component signal {}.{} is scalar but indexed with {:?}",
                qualified_comp_name, signal_name, signal_indices
            );
        }
        let field_sort = Sort::Field(default_field());
        self.ensure_var_in_metadata(&qualified_signal_name, &field_sort);
        Some(T::new(
            Ty::Field,
            leaf_term(Op::new_var(qualified_signal_name, field_sort)),
        ))
    }

    /// Extract loop bounds for simple for loops: for(var i = start; i < end; i++)
    /// Returns (start, end, step) if successfully parsed
    fn extract_loop_bounds(&self, for_stmt: &ast::ForStatement) -> (Option<i64>, Option<i64>, Option<i64>) {
        // Extract start value from initialization (use first declaration)
        let start = if let Some(first_decl) = for_stmt.var.declarations.first() {
            if let Some(expr) = &first_decl.value {
                self.extract_constant_value_ternary(expr)
            } else {
                None
            }
        } else {
            None
        };

        // Extract end value from condition (e.g., i < 10)
        let end = match &for_stmt.condition {
            ast::Expression::Binary(bin) => {
                // Support: i < end, i <= end, i > end, i >= end
                match &bin.op {
                    ast::OpBinary::LtOp => {
                        // i < end
                        self.extract_constant_value_expr(&bin.right)
                    }
                    ast::OpBinary::LteOp => {
                        // i <= end, so actual end is end + 1
                        self.extract_constant_value_expr(&bin.right).map(|v| v + 1)
                    }
                    ast::OpBinary::GtOp => {
                        // i > end (counting down)
                        self.extract_constant_value_expr(&bin.right)
                    }
                    ast::OpBinary::GteOp => {
                        // i >= end (counting down), actual end is end - 1
                        self.extract_constant_value_expr(&bin.right).map(|v| v - 1)
                    }
                    ast::OpBinary::NotEqualOp => {
                        // i != end (works for both counting up and down)
                        self.extract_constant_value_expr(&bin.right)
                    }
                    _ => None,
                }
            }
            _ => None,
        };

        // Extract step from increment (e.g., i++ means step=1)
        let step = match &for_stmt.increment {
            ast::Expression::Unary(unary) => {
                match &unary.op {
                    ast::OpUnary::Increment(_) => Some(1), // ++i
                    ast::OpUnary::Decrement(_) => Some(-1), // --i
                    _ => None,
                }
            }
            ast::Expression::Postfix(postfix) => {
                // Support i++ and i-- as postfix
                if let Some(access) = postfix.access.get(0) {
                    match access {
                        ast::Access::Increment(_) => Some(1),
                        ast::Access::Decrement(_) => Some(-1),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            ast::Expression::Binary(bin) => {
                // Support: i += step, i -= step
                match &bin.op {
                    ast::OpBinary::AddOp => {
                        // i + step
                        self.extract_constant_value_expr(&bin.right)
                    }
                    ast::OpBinary::SubOp => {
                        // i - step (negative)
                        self.extract_constant_value_expr(&bin.right).map(|v| -v)
                    }
                    _ => None,
                }
            }
            _ => None,
        };

        (start, end, step)
    }

    /// Extract a constant integer value from a TernaryOrExpression
    fn extract_constant_value_ternary(&self, expr: &ast::TernaryOrExpression) -> Option<i64> {
        match expr {
            ast::TernaryOrExpression::Expression(e) => {
                self.extract_constant_value_expr(e)
            }
            _ => None,
        }
    }

    fn extract_constant_value_expr(&self, expr: &ast::Expression) -> Option<i64> {
        self.extract_constant_value_expr_big(expr).and_then(|v| v.to_i64())
    }

    /// Extract a constant integer value from an expression (arbitrary precision)
    fn extract_constant_value_expr_big(&self, expr: &ast::Expression) -> Option<rug::Integer> {
        use rug::Integer;
        use rug::ops::Pow;

        match expr {
            ast::Expression::Number(num) => {
                match num {
                    ast::Number::Decimal(dec) => {
                        let num_str = dec.span.as_str().trim().replace('_', "");
                        Integer::from_str_radix(&num_str, 10).ok()
                    }
                    ast::Number::Hex(hex) => {
                        let raw = hex.span.as_str().trim();
                        let hex_str = raw
                            .strip_prefix("0x")
                            .or_else(|| raw.strip_prefix("0X"))
                            .unwrap_or(raw)
                            .replace('_', "");
                        Integer::from_str_radix(&hex_str, 16).ok()
                    }
                }
            }
            ast::Expression::Identifier(id) => {
                // Look up identifier in var_values (template params, loop vars, etc.)
                let result = self.var_values.get(&id.value).and_then(|v| {
                    v.as_integer().cloned()
                });

                if result.is_none() && !self.var_values.contains_key(&id.value) {
                }

                result
            }
            ast::Expression::Postfix(postfix) => {
                // Handle array access: arr[idx] or multidimensional arr[i][j]

                // Check if this is a component port access or function call
                let has_dot_access = postfix.access.iter().any(|acc| {
                    matches!(acc, ast::Access::DotAccess(_))
                });
                let has_call_access = postfix.access.iter().any(|acc| {
                    matches!(acc, ast::Access::CallAccess(_))
                });

                if has_dot_access {
                    return None;
                }

                // Handle function calls at compile time
                if has_call_access {
                    if postfix.access.len() == 1 {
                        if let ast::Access::CallAccess(call) = &postfix.access[0] {
                            if let ast::Expression::Identifier(func_id) = postfix.base.as_ref() {
                                let func_name = &func_id.value;
                                // Evaluate all arguments as compile-time constants
                                let mut const_args = Vec::new();
                                for arg in &call.args {
                                    if let Some(val) = self.extract_constant_value_expr_big(arg) {
                                        const_args.push(val);
                                    } else {
                                        return None; // Argument is not a compile-time constant
                                    }
                                }
                                // Look up the function and evaluate it
                                if let Some((_path, func_def)) = self.circom_gen.find_function(func_name) {
                                    let func_def = func_def.clone();
                                    return self.eval_function_constant(&func_def, const_args);
                                }
                            }
                        }
                    }
                    return None;
                }

                // Also check if the base itself is a Postfix with DotAccess or CallAccess (nested case)
                if let ast::Expression::Postfix(inner_postfix) = postfix.base.as_ref() {
                    let inner_has_dot = inner_postfix.access.iter().any(|acc| {
                        matches!(acc, ast::Access::DotAccess(_))
                    });
                    let inner_has_call = inner_postfix.access.iter().any(|acc| {
                        matches!(acc, ast::Access::CallAccess(_))
                    });
                    if inner_has_dot || inner_has_call {
                        return None;
                    }
                }

                // Get the base variable
                if let ast::Expression::Identifier(base_id) = postfix.base.as_ref() {
                    let base_name = &base_id.value;

                    // Signals/components are not compile-time variables
                    if self.vars.contains_key(base_name) && !self.var_values.contains_key(base_name) {
                        return None;
                    }

                    let base_val = self.var_values.get(base_name).unwrap_or_else(|| {
                        panic!(
                            "Variable '{}' not found during array access\n\
                             \n\
                             Attempted to access array variable that doesn't exist.\n\
                             \n\
                             Context:\n\
                             - Variable name: '{}'\n\
                             - Current component: {:?}",
                            base_name,
                            base_name,
                            self.current_component
                        )
                    });

                    // Process each access (for multidimensional arrays)
                    let mut current_val = base_val.clone();
                    for access in &postfix.access {
                        if let ast::Access::ArrayAccess(array_access) = access {
                            // Evaluate the index expression (must be non-negative and fit usize)
                            let index_opt = self.extract_constant_value_expr_big(&array_access.expression);
                            let index = match index_opt.and_then(|i| i.to_i64()) {
                                Some(i) if i >= 0 => i as usize,
                                Some(i) => {
                                    panic!(
                                        "Negative array index: {}\n\
                                         \n\
                                         Array indices must be non-negative integers.\n\
                                         \n\
                                         Context:\n\
                                         - Array: '{}'\n\
                                         - Index expression: {:?}\n\
                                         - Evaluated index: {}\n\
                                         - Current component: {:?}",
                                        i,
                                        base_name,
                                        array_access.expression,
                                        i,
                                        self.current_component
                                    )
                                }
                                None => {
                                    panic!(
                                        "Cannot evaluate array index as compile-time constant\n\
                                         \n\
                                         Array indices must be compile-time constants.\n\
                                         \n\
                                         Context:\n\
                                         - Array: '{}'\n\
                                         - Index expression: {:?}\n\
                                         - Current component: {:?}",
                                        base_name,
                                        array_access.expression,
                                        self.current_component
                                    )
                                }
                            };

                            current_val = match &current_val {
                                CompileTimeValue::Array1D(arr) => {
                                    if index < arr.len() {
                                        CompileTimeValue::Scalar(arr[index].clone())
                                    } else {
                                        panic!(
                                            "Array index out of bounds\n\
                                             \n\
                                             Attempted to access index {} but array length is {}.\n\
                                             \n\
                                             Context:\n\
                                             - Array: '{}'\n\
                                             - Index: {}\n\
                                             - Array length: {}\n\
                                             - Current component: {:?}",
                                            index,
                                            arr.len(),
                                            base_name,
                                            index,
                                            arr.len(),
                                            self.current_component
                                        )
                                    }
                                }
                                CompileTimeValue::Array2D(arr) => {
                                    if index < arr.len() {
                                        CompileTimeValue::Array1D(arr[index].clone())
                                    } else {
                                        panic!(
                                            "Array index out of bounds (2D array)\n\
                                             \n\
                                             Attempted to access index {} but first dimension length is {}.\n\
                                             \n\
                                             Context:\n\
                                             - Array: '{}'\n\
                                             - Index: {}\n\
                                             - First dimension length: {}\n\
                                             - Current component: {:?}",
                                            index,
                                            arr.len(),
                                            base_name,
                                            index,
                                            arr.len(),
                                            self.current_component
                                        )
                                    }
                                }
                                CompileTimeValue::ArrayND(arr) => {
                                    if index < arr.len() {
                                        arr[index].clone()
                                    } else {
                                        panic!(
                                            "Array index out of bounds (multidimensional array)\n\
                                             \n\
                                             Attempted to access index {} but dimension length is {}.\n\
                                             \n\
                                             Context:\n\
                                             - Array: '{}'\n\
                                             - Index: {}\n\
                                             - Dimension length: {}\n\
                                             - Current component: {:?}",
                                            index,
                                            arr.len(),
                                            base_name,
                                            index,
                                            arr.len(),
                                            self.current_component
                                        )
                                    }
                                }
                                CompileTimeValue::Scalar(_) => {
                                    panic!(
                                        "Cannot index into scalar value\n\
                                         \n\
                                         Attempted to use array indexing on a scalar (non-array) variable.\n\
                                         \n\
                                         Context:\n\
                                         - Variable: '{}'\n\
                                         - Index expression: {:?}\n\
                                         - Current component: {:?}",
                                        base_name,
                                        array_access.expression,
                                        self.current_component
                                    )
                                }
                                CompileTimeValue::Expression(_) | CompileTimeValue::ExprArray1D(_) => {
                                    return None;
                                }
                            };
                        } else {
                            return None;
                        }
                    }

                    current_val.as_integer().cloned()
                } else {
                    None
                }
            }
            ast::Expression::Binary(bin) => {
                let left = self.extract_constant_value_expr_big(&bin.left)?;
                let right = self.extract_constant_value_expr_big(&bin.right)?;

                match bin.op {
                    ast::OpBinary::AddOp => Some(left + right),
                    ast::OpBinary::SubOp => Some(left - right),
                    ast::OpBinary::MulOp => Some(left * right),
                    ast::OpBinary::DivOp | ast::OpBinary::IDivOp => {
                        if right == 0 {
                            None
                        } else {
                            Some(left / right)
                        }
                    }
                    ast::OpBinary::ModOp => {
                        if right == 0 {
                            None
                        } else {
                            Some(left % right)
                        }
                    }
                    ast::OpBinary::LeftShiftOp => {
                        if right < 0 {
                            None
                        } else if let Some(shift) = right.to_u32() {
                            Some(left << shift)
                        } else {
                            None
                        }
                    }
                    ast::OpBinary::RightShiftOp => {
                        if right < 0 {
                            None
                        } else if let Some(shift) = right.to_u32() {
                            Some(left >> shift)
                        } else {
                            None
                        }
                    }
                    ast::OpBinary::BitAndOp => Some(left & right),
                    ast::OpBinary::BitOrOp => Some(left | right),
                    ast::OpBinary::BitXorOp => Some(left ^ right),
                    ast::OpBinary::PowOp => {
                        if right < 0 {
                            None
                        } else if let Some(exp) = right.to_u32() {
                            Some(left.pow(exp))
                        } else {
                            None
                        }
                    }
                    ast::OpBinary::EqualOp => Some(Integer::from(if left == right { 1 } else { 0 })),
                    ast::OpBinary::NotEqualOp => Some(Integer::from(if left != right { 1 } else { 0 })),
                    ast::OpBinary::LtOp => Some(Integer::from(if left < right { 1 } else { 0 })),
                    ast::OpBinary::GtOp => Some(Integer::from(if left > right { 1 } else { 0 })),
                    ast::OpBinary::LteOp => Some(Integer::from(if left <= right { 1 } else { 0 })),
                    ast::OpBinary::GteOp => Some(Integer::from(if left >= right { 1 } else { 0 })),
                    ast::OpBinary::AndOp => Some(Integer::from(if left != 0 && right != 0 { 1 } else { 0 })),
                    ast::OpBinary::OrOp => Some(Integer::from(if left != 0 || right != 0 { 1 } else { 0 })),
                    _ => None,
                }
            }
            ast::Expression::Unary(un) => {
                let operand = self.extract_constant_value_expr_big(&un.expression)?;

                match un.op {
                    ast::OpUnary::Neg(_) => Some(-operand),
                    ast::OpUnary::Not(_) => Some(Integer::from(if operand == 0 { 1 } else { 0 })),
                    ast::OpUnary::Increment(_) => Some(operand + 1),
                    ast::OpUnary::Decrement(_) => Some(operand - 1),
                }
            }
            _ => None,
        }
    }

    /// Evaluate a Circom function at compile time with constant arguments.
    /// Returns the return value as an Integer, or None if evaluation fails.
    /// Supports: var declarations/assignments, for loops, if/else, return statements.
    fn eval_function_constant(
        &self,
        func_def: &circom_pest_ast::FunctionDefinition<'ast>,
        args: Vec<rug::Integer>,
    ) -> Option<rug::Integer> {
        use rug::Integer;

        if func_def.params.len() != args.len() {
            return None;
        }

        // Local variable environment for this function evaluation
        let mut env: HashMap<String, Integer> = HashMap::default();
        for (param, val) in func_def.params.iter().zip(args.iter()) {
            env.insert(param.value.clone(), val.clone());
        }

        /// Result of evaluating statements: either a return value was hit, or execution continues.
        enum StmtResult {
            Continue,
            Return(rug::Integer),
        }

        /// Evaluate a list of statements. Returns None on failure.
        fn eval_stmts(
            stmts: &[circom_pest_ast::Statement],
            env: &mut HashMap<String, rug::Integer>,
            depth: usize,
        ) -> Option<StmtResult> {
            if depth > 1000 { return None; }
            for stmt in stmts {
                if let Some(r) = eval_stmt(stmt, env, depth)? {
                    return Some(StmtResult::Return(r));
                }
            }
            Some(StmtResult::Continue)
        }

        /// Evaluate a single statement. Returns Some(val) if a return was hit, None-in-Option otherwise.
        /// Outer Option is for failure (None = can't evaluate).
        fn eval_stmt(
            stmt: &circom_pest_ast::Statement,
            env: &mut HashMap<String, rug::Integer>,
            depth: usize,
        ) -> Option<Option<rug::Integer>> {
            match stmt {
                circom_pest_ast::Statement::Variable(var_stmt) => {
                    for decl in &var_stmt.declarations {
                        let var_name = decl.assignee.id.value.clone();
                        if let Some(val_expr) = &decl.value {
                            if let circom_pest_ast::TernaryOrExpression::Expression(rhs) = val_expr {
                                let val = eval_expr(rhs, env)?;
                                // Handle assignment operators (=, +=, etc.)
                                let final_val = if let Some(op) = &decl.op {
                                    apply_assign_op(op, env.get(&var_name).cloned().unwrap_or(rug::Integer::from(0)), val)?
                                } else {
                                    val
                                };
                                env.insert(var_name, final_val);
                            } else {
                                return None; // ternary not supported
                            }
                        } else {
                            // Declaration without value: var x;
                            env.insert(var_name, rug::Integer::from(0));
                        }
                    }
                    Some(None)
                }
                circom_pest_ast::Statement::Return(ret) => {
                    if let Some(ref expr) = ret.expression {
                        let val = eval_expr(expr, env)?;
                        Some(Some(val))
                    } else {
                        Some(Some(rug::Integer::from(0)))
                    }
                }
                circom_pest_ast::Statement::If(if_stmt) => {
                    let cond = eval_expr(&if_stmt.condition, env)?;
                    if cond != 0 {
                        match eval_stmts(&if_stmt.then_statements, env, depth + 1)? {
                            StmtResult::Return(v) => return Some(Some(v)),
                            StmtResult::Continue => {}
                        }
                    } else {
                        // Try else-if branches
                        let mut handled = false;
                        for else_if in &if_stmt.else_if_branches {
                            let c = eval_expr(&else_if.condition, env)?;
                            if c != 0 {
                                match eval_stmts(&else_if.statements, env, depth + 1)? {
                                    StmtResult::Return(v) => return Some(Some(v)),
                                    StmtResult::Continue => {}
                                }
                                handled = true;
                                break;
                            }
                        }
                        if !handled {
                            if let Some(else_branch) = &if_stmt.else_branch {
                                match eval_stmts(&else_branch.statements, env, depth + 1)? {
                                    StmtResult::Return(v) => return Some(Some(v)),
                                    StmtResult::Continue => {}
                                }
                            }
                        }
                    }
                    Some(None)
                }
                circom_pest_ast::Statement::For(for_stmt) => {
                    // Init: for_stmt.var is a VariableStatement
                    let init_stmt = circom_pest_ast::Statement::Variable(for_stmt.var.clone());
                    if let Some(v) = eval_stmt(&init_stmt, env, depth + 1)? {
                        return Some(Some(v));
                    }
                    let mut iterations = 0;
                    loop {
                        if iterations > 100_000 { return None; }
                        iterations += 1;
                        let cond = eval_expr(&for_stmt.condition, env)?;
                        if cond == 0 { break; }
                        // Body
                        match eval_stmts(&for_stmt.statements, env, depth + 1)? {
                            StmtResult::Return(v) => return Some(Some(v)),
                            StmtResult::Continue => {}
                        }
                        // Increment (it's an Expression)
                        eval_increment_expr(&for_stmt.increment, env)?;
                    }
                    Some(None)
                }
                circom_pest_ast::Statement::Expression(expr) => {
                    // Expression statements in functions — handle assignments and increments
                    eval_increment_expr(expr, env)?;
                    Some(None)
                }
                circom_pest_ast::Statement::Log(_) | circom_pest_ast::Statement::Assert(_) => {
                    Some(None) // Skip log/assert during constant evaluation
                }
                _ => None, // Unsupported statement type
            }
        }

        /// Handle increment/decrement and assignment expressions used as statements
        fn eval_increment_expr(
            expr: &circom_pest_ast::Expression,
            env: &mut HashMap<String, rug::Integer>,
        ) -> Option<()> {
            match expr {
                circom_pest_ast::Expression::Postfix(postfix) => {
                    if let circom_pest_ast::Expression::Identifier(id) = postfix.base.as_ref() {
                        if let Some(access) = postfix.access.first() {
                            match access {
                                circom_pest_ast::Access::Increment(_) => {
                                    let val = env.get(&id.value)?.clone();
                                    env.insert(id.value.clone(), val + 1);
                                    return Some(());
                                }
                                circom_pest_ast::Access::Decrement(_) => {
                                    let val = env.get(&id.value)?.clone();
                                    env.insert(id.value.clone(), val - 1);
                                    return Some(());
                                }
                                _ => {}
                            }
                        }
                    }
                    // Other postfix — just evaluate for side effects
                    eval_expr(expr, env)?;
                    Some(())
                }
                circom_pest_ast::Expression::Unary(un) => {
                    if let circom_pest_ast::Expression::Identifier(id) = &*un.expression {
                        match un.op {
                            circom_pest_ast::OpUnary::Increment(_) => {
                                let val = env.get(&id.value)?.clone();
                                env.insert(id.value.clone(), val + 1);
                                return Some(());
                            }
                            circom_pest_ast::OpUnary::Decrement(_) => {
                                let val = env.get(&id.value)?.clone();
                                env.insert(id.value.clone(), val - 1);
                                return Some(());
                            }
                            _ => {}
                        }
                    }
                    eval_expr(expr, env)?;
                    Some(())
                }
                _ => {
                    eval_expr(expr, env)?;
                    Some(())
                }
            }
        }

        /// Apply a compound assignment operator
        fn apply_assign_op(
            op: &circom_pest_ast::VarAssignmentOp,
            lhs: rug::Integer,
            rhs: rug::Integer,
        ) -> Option<rug::Integer> {
            use rug::ops::Pow;
            match op {
                circom_pest_ast::VarAssignmentOp::Assign(_) => Some(rhs),
                circom_pest_ast::VarAssignmentOp::AddAssign(_) => Some(lhs + rhs),
                circom_pest_ast::VarAssignmentOp::SubAssign(_) => Some(lhs - rhs),
                circom_pest_ast::VarAssignmentOp::MulAssign(_) => Some(lhs * rhs),
                circom_pest_ast::VarAssignmentOp::DivAssign(_) => {
                    if rhs == 0 { None } else { Some(lhs / rhs) }
                }
                circom_pest_ast::VarAssignmentOp::ModAssign(_) => {
                    if rhs == 0 { None } else { Some(lhs % rhs) }
                }
                circom_pest_ast::VarAssignmentOp::PowAssign(_) => {
                    rhs.to_u32().map(|e| lhs.pow(e))
                }
                circom_pest_ast::VarAssignmentOp::LeftShiftAssign(_) => {
                    rhs.to_u32().map(|s| lhs << s)
                }
                circom_pest_ast::VarAssignmentOp::RightShiftAssign(_) => {
                    rhs.to_u32().map(|s| lhs >> s)
                }
                circom_pest_ast::VarAssignmentOp::BitAndAssign(_) => Some(lhs & rhs),
                circom_pest_ast::VarAssignmentOp::BitOrAssign(_) => Some(lhs | rhs),
                circom_pest_ast::VarAssignmentOp::BitXorAssign(_) => Some(lhs ^ rhs),
                circom_pest_ast::VarAssignmentOp::BitNotAssign(_) => Some(!rhs),
            }
        }

        fn eval_expr(
            expr: &circom_pest_ast::Expression,
            env: &HashMap<String, rug::Integer>,
        ) -> Option<rug::Integer> {
            use rug::Integer;
            use rug::ops::Pow;
            match expr {
                circom_pest_ast::Expression::Number(num) => {
                    match num {
                        circom_pest_ast::Number::Decimal(dec) => {
                            let num_str = dec.span.as_str().trim().replace('_', "");
                            Integer::from_str_radix(&num_str, 10).ok()
                        }
                        circom_pest_ast::Number::Hex(hex) => {
                            let raw = hex.span.as_str().trim();
                            let hex_str = raw
                                .strip_prefix("0x")
                                .or_else(|| raw.strip_prefix("0X"))
                                .unwrap_or(raw)
                                .replace('_', "");
                            Integer::from_str_radix(&hex_str, 16).ok()
                        }
                    }
                }
                circom_pest_ast::Expression::Identifier(id) => {
                    env.get(&id.value).cloned()
                }
                circom_pest_ast::Expression::Binary(bin) => {
                    let left = eval_expr(&bin.left, env)?;
                    let right = eval_expr(&bin.right, env)?;
                    match bin.op {
                        circom_pest_ast::OpBinary::AddOp => Some(left + right),
                        circom_pest_ast::OpBinary::SubOp => Some(left - right),
                        circom_pest_ast::OpBinary::MulOp => Some(left * right),
                        circom_pest_ast::OpBinary::DivOp | circom_pest_ast::OpBinary::IDivOp => {
                            if right == 0 { None } else { Some(left / right) }
                        }
                        circom_pest_ast::OpBinary::ModOp => {
                            if right == 0 { None } else { Some(left % right) }
                        }
                        circom_pest_ast::OpBinary::LeftShiftOp => {
                            right.to_u32().map(|s| left << s)
                        }
                        circom_pest_ast::OpBinary::RightShiftOp => {
                            right.to_u32().map(|s| left >> s)
                        }
                        circom_pest_ast::OpBinary::PowOp => {
                            right.to_u32().map(|e| left.pow(e))
                        }
                        circom_pest_ast::OpBinary::BitAndOp => Some(left & right),
                        circom_pest_ast::OpBinary::BitOrOp => Some(left | right),
                        circom_pest_ast::OpBinary::BitXorOp => Some(left ^ right),
                        circom_pest_ast::OpBinary::EqualOp => Some(Integer::from(if left == right { 1 } else { 0 })),
                        circom_pest_ast::OpBinary::NotEqualOp => Some(Integer::from(if left != right { 1 } else { 0 })),
                        circom_pest_ast::OpBinary::LtOp => Some(Integer::from(if left < right { 1 } else { 0 })),
                        circom_pest_ast::OpBinary::GtOp => Some(Integer::from(if left > right { 1 } else { 0 })),
                        circom_pest_ast::OpBinary::LteOp => Some(Integer::from(if left <= right { 1 } else { 0 })),
                        circom_pest_ast::OpBinary::GteOp => Some(Integer::from(if left >= right { 1 } else { 0 })),
                        circom_pest_ast::OpBinary::AndOp => Some(Integer::from(if left != 0 && right != 0 { 1 } else { 0 })),
                        circom_pest_ast::OpBinary::OrOp => Some(Integer::from(if left != 0 || right != 0 { 1 } else { 0 })),
                        _ => None,
                    }
                }
                circom_pest_ast::Expression::Unary(un) => {
                    let operand = eval_expr(&un.expression, env)?;
                    match un.op {
                        circom_pest_ast::OpUnary::Neg(_) => Some(-operand),
                        circom_pest_ast::OpUnary::Not(_) => Some(Integer::from(if operand == 0 { 1 } else { 0 })),
                        circom_pest_ast::OpUnary::Increment(_) => Some(operand + 1),
                        circom_pest_ast::OpUnary::Decrement(_) => Some(operand - 1),
                    }
                }
                _ => None,
            }
        }

        // Run the function body
        match eval_stmts(&func_def.statements, &mut env, 0)? {
            StmtResult::Return(val) => Some(val),
            StmtResult::Continue => None,
        }
    }

    /// Extract a template parameter value (can be scalar or array)
    /// Returns CompileTimeValue instead of just i64
    /// This is used when extracting template instantiation arguments
    fn extract_parameter_value(&self, expr: &ast::Expression) -> Option<CompileTimeValue> {
        match expr {
            // Scalar values (numbers, binary expressions, unary expressions)
            ast::Expression::Number(_) | ast::Expression::Binary(_) | ast::Expression::Unary(_) => {
                // Try to extract as scalar constant
                self.extract_constant_value_expr_big(expr).map(CompileTimeValue::scalar_big)
            }

            // Identifiers - could be scalar or array references
            ast::Expression::Identifier(id) => {
                let var_name = &id.value;


                // First check var_values for compile-time values
                if let Some(value) = self.var_values.get(var_name) {
                    return Some(value.clone());  // Return entire value (scalar or array)
                }

                // If not in var_values, return None (can't determine value)
                None
            }

            // Postfix expressions: array access (e.g., arr[0], arr2d[i]) or function calls
            ast::Expression::Postfix(postfix) => {
                // First, try to handle array indexing that returns a sub-array
                // e.g., PBASE[i] where PBASE is a 2D array returns a 1D array row
                if let ast::Expression::Identifier(base_id) = postfix.base.as_ref() {
                    let base_name = &base_id.value;
                    if let Some(base_val) = self.var_values.get(base_name) {
                        // Single array access on a multi-dimensional array
                        if postfix.access.len() == 1 {
                            if let ast::Access::ArrayAccess(array_access) = &postfix.access[0] {
                                if let Some(idx) = self.extract_constant_value_expr_big(&array_access.expression)
                                    .and_then(|v| v.to_usize()) {
                                    match base_val {
                                        CompileTimeValue::Array2D(arr2d) => {
                                            if idx < arr2d.len() {
                                                return Some(CompileTimeValue::Array1D(arr2d[idx].clone()));
                                            }
                                        }
                                        CompileTimeValue::ArrayND(inner) => {
                                            if idx < inner.len() {
                                                return Some(inner[idx].clone());
                                            }
                                        }
                                        CompileTimeValue::Array1D(arr) => {
                                            if idx < arr.len() {
                                                return Some(CompileTimeValue::scalar_big(arr[idx].clone()));
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                }
                // Fall back to scalar extraction
                self.extract_constant_value_expr_big(expr).map(CompileTimeValue::scalar_big)
            }

            // Array literals (if Circom supports inline array parameters)
            ast::Expression::Array(arr) => {
                let elements: Option<Vec<rug::Integer>> = arr.elements.iter()
                    .map(|e| self.extract_constant_value_expr_big(e))
                    .collect();
                elements.map(CompileTimeValue::Array1D)
            }
        }
    }

    /// Handle binary operation on arrays element-wise
    /// Called when at least one operand is an array type
    fn handle_array_binary_op(
        &mut self,
        lhs: T,
        rhs: T,
        op: &ast::OpBinary,
    ) -> T {
        use crate::front::circom::term::{add, sub, mul, div, idiv, rem, pow};

        let lhs_ty = lhs.type_();
        let rhs_ty = rhs.type_();

        // Determine which operand is the array and which is the scalar
        let (arr_term, scalar_term, is_left_array) = if matches!(lhs_ty, Ty::Array(_, _)) {
            (lhs, rhs, true)
        } else if matches!(rhs_ty, Ty::Array(_, _)) {
            (rhs, lhs, false)
        } else {
            panic!("handle_array_binary_op called with non-array operands");
        };

        // Get array size
        let arr_size = if let Ty::Array(size, _) = arr_term.type_() {
            *size
        } else {
            panic!("Expected array type");
        };


        // Perform element-wise operation
        let mut result_elements = Vec::new();
        for i in 0..arr_size {
            // Access array element using array_select
            let index_term = field_lit(rug::Integer::from(i));
            let elem_term = array_select(arr_term.clone(), index_term)
                .unwrap_or_else(|e| panic!("Failed to select array element {}: {}", i, e));

            // Apply operation (respect operand order)
            let result_elem = if is_left_array {
                // array op scalar
                match op {
                    ast::OpBinary::AddOp => add(elem_term, scalar_term.clone()),
                    ast::OpBinary::SubOp => sub(elem_term, scalar_term.clone()),
                    ast::OpBinary::MulOp => mul(elem_term, scalar_term.clone()),
                    ast::OpBinary::DivOp => div(elem_term, scalar_term.clone()),
                    ast::OpBinary::IDivOp => idiv(elem_term, scalar_term.clone()),
                    ast::OpBinary::ModOp => rem(elem_term, scalar_term.clone()),
                    ast::OpBinary::PowOp => pow(elem_term, scalar_term.clone()),
                    _ => panic!("Unsupported array operation: {:?}", op),
                }
            } else {
                // scalar op array
                match op {
                    ast::OpBinary::AddOp => add(scalar_term.clone(), elem_term),
                    ast::OpBinary::SubOp => sub(scalar_term.clone(), elem_term),
                    ast::OpBinary::MulOp => mul(scalar_term.clone(), elem_term),
                    ast::OpBinary::DivOp => div(scalar_term.clone(), elem_term),
                    ast::OpBinary::IDivOp => idiv(scalar_term.clone(), elem_term),
                    ast::OpBinary::ModOp => rem(scalar_term.clone(), elem_term),
                    ast::OpBinary::PowOp => pow(scalar_term.clone(), elem_term),
                    _ => panic!("Unsupported array operation: {:?}", op),
                }
            }.unwrap_or_else(|e| panic!("Array element operation failed: {}", e));

            result_elements.push(result_elem);
        }


        // Return the array term
        array(result_elements).unwrap()
    }

    /// Convert expression to IR term
    fn expr_to_term(&mut self, expr: &Expression<'ast>) -> T {
        match expr {
            Expression::Binary(bin) => {
                let lhs = self.expr_to_term(&bin.left);
                let rhs = self.expr_to_term(&bin.right);
                let lhs_ty = lhs.type_().clone();
                let rhs_ty = rhs.type_().clone();

                // Check if either operand is an array - if so, handle element-wise
                let is_array_op = matches!(lhs_ty, Ty::Array(_, _)) || matches!(rhs_ty, Ty::Array(_, _));

                if is_array_op {

                    // Handle element-wise array operations
                    return self.handle_array_binary_op(lhs, rhs, &bin.op);
                }

                match bin.op {
                    ast::OpBinary::AddOp => add(lhs, rhs).unwrap_or_else(|e| panic!("Add operation failed: {}. Left type: {:?}, Right type: {:?}", e, lhs_ty, rhs_ty)),
                    ast::OpBinary::SubOp => sub(lhs, rhs).unwrap_or_else(|e| panic!("Sub operation failed: {}. Left type: {:?}, Right type: {:?}", e, lhs_ty, rhs_ty)),
                    ast::OpBinary::MulOp => mul(lhs, rhs).unwrap_or_else(|e| panic!("Mul operation failed: {}. Left type: {:?}, Right type: {:?}", e, lhs_ty, rhs_ty)),
                    ast::OpBinary::DivOp => div(lhs, rhs).unwrap_or_else(|e| panic!("Div operation failed: {}. Left type: {:?}, Right type: {:?}", e, lhs_ty, rhs_ty)),
                    ast::OpBinary::IDivOp => idiv(lhs, rhs).unwrap_or_else(|e| panic!("IDiv operation failed: {}. Left type: {:?}, Right type: {:?}", e, lhs_ty, rhs_ty)),
                    ast::OpBinary::ModOp => rem(lhs, rhs).unwrap_or_else(|e| panic!("Mod operation failed: {}. Left type: {:?}, Right type: {:?}. This may occur when operating on arrays - arrays in Circom should be processed element-wise at compile-time, not as runtime IR operations.", e, lhs_ty, rhs_ty)),
                    ast::OpBinary::PowOp => pow(lhs, rhs).unwrap_or_else(|e| panic!("Pow operation failed: {}. Left type: {:?}, Right type: {:?}", e, lhs_ty, rhs_ty)),
                    ast::OpBinary::EqualOp => eq(lhs, rhs).unwrap(),
                    ast::OpBinary::NotEqualOp => neq(lhs, rhs).unwrap(),
                    ast::OpBinary::LtOp => lt(lhs, rhs).unwrap(),
                    ast::OpBinary::LteOp => lte(lhs, rhs).unwrap(),
                    ast::OpBinary::GtOp => gt(lhs, rhs).unwrap(),
                    ast::OpBinary::GteOp => gte(lhs, rhs).unwrap(),
                    ast::OpBinary::AndOp => and(lhs, rhs).unwrap(),
                    ast::OpBinary::OrOp => or(lhs, rhs).unwrap(),
                    ast::OpBinary::BitAndOp => bit_and(lhs, rhs).unwrap(),
                    ast::OpBinary::BitOrOp => bit_or(lhs, rhs).unwrap(),
                    ast::OpBinary::BitXorOp => bit_xor(lhs, rhs).unwrap(),
                    ast::OpBinary::LeftShiftOp => left_shift(lhs, rhs).unwrap(),
                    ast::OpBinary::RightShiftOp => right_shift(lhs, rhs).unwrap(),
                    _ => unimplemented!("Binary operator not supported: {:?}", bin.op),
                }
            }
            Expression::Unary(un) => {
                let operand = self.expr_to_term(&un.expression);
                match un.op {
                    ast::OpUnary::Neg(_) => neg(operand).unwrap(),
                    ast::OpUnary::Not(_) => logical_not(operand).unwrap(),
                    ast::OpUnary::Increment(_) => increment(operand).unwrap(),
                    ast::OpUnary::Decrement(_) => decrement(operand).unwrap(),
                }
            }
            Expression::Identifier(id) => {
                // Get the correct sort for the variable
                let var_name = id.value.clone();

                // Check if this is a variable with a stored compile-time value (var x = ...)
                // Handle both compile-time scalars and runtime expressions
                if let Some(value) = self.var_values.get(&var_name) {
                    match value {
                        CompileTimeValue::Scalar(_) => {
                            return value.to_term();
                        }
                        CompileTimeValue::Expression(term) => {
                            // Variable contains a runtime expression (e.g., depends on signals)
                            // Return the stored IR term directly
                            return term.clone();
                        }
                        _ => {
                            // For arrays, fall through to create an IR array variable
                            // The actual element access will be handled by Postfix ArrayAccess
                        }
                    }
                }

                // Check if variable has array structure in var_values (even if not in vars yet)
                // This handles cases where array variables are computed from functions
                if let Some(value) = self.var_values.get(&var_name) {
                    if var_name == "C" {
                    }
                    match value {
                        CompileTimeValue::Array1D(arr) => {
                            if var_name == "C" {
                            }
                            // Create an array term from the compile-time values
                            let elem_terms: Vec<T> = arr.iter()
                                .map(|val| field_lit(val.clone()))
                                .collect();
                            return array(elem_terms).unwrap();
                        }
                        CompileTimeValue::Array2D(arr2d) => {
                            // Create a 2D array term
                            let row_terms: Vec<T> = arr2d.iter()
                                .map(|row| {
                                    let elem_terms: Vec<T> = row.iter()
                                        .map(|val| field_lit(val.clone()))
                                        .collect();
                                    array(elem_terms).unwrap()
                                })
                                .collect();
                            return array(row_terms).unwrap();
                        }
                        CompileTimeValue::ExprArray1D(expr_arr) => {
                            // Array of runtime expressions - return as array term
                            return array(expr_arr.clone()).unwrap();
                        }
                        CompileTimeValue::ArrayND(_) => {
                            // Nested arrays (3D+) - build term recursively
                            return value.to_term_any();
                        }
                        _ => {}  // Fall through for scalars and expressions (already handled above)
                    }
                }

                // Otherwise, lookup in vars (for signals and other variables)
                if let Some(var_type) = self.vars.get(&var_name) {
                    if var_name == "C" {
                    }
                    match var_type {
                        CircomType::Field => {
                            if var_name == "C" {
                            }
                            // Ensure variable is registered in metadata before creating IR term
                            let field_sort = Sort::Field(cfg().field().clone());
                            self.ensure_var_in_metadata(&var_name, &field_sort);
                            // Create a new field variable
                            T::new(Ty::Field, leaf_term(Op::new_var(var_name, field_sort)))
                        }
                        CircomType::Array(_elem_ty, size) => {
                            let array_size = *size;

                            // Check if this is a signal array (elements are flattened)
                            if self.signal_names.contains(&var_name) {

                                // Construct array term from flattened signal elements
                                let actual_name_prefix = if let Some(comp_name) = &self.current_component {
                                    format!("{}.{}", comp_name, var_name)
                                } else {
                                    var_name.clone()
                                };

                                let mut elements = Vec::new();
                                for i in 0..array_size {
                                    let elem_name = format!("{}_{}", actual_name_prefix, i);
                                    let field_sort = Sort::Field(cfg().field().clone());
                                    self.ensure_var_in_metadata(&elem_name, &field_sort);
                                    elements.push(T::new(Ty::Field, leaf_term(Op::new_var(elem_name, field_sort))));
                                }

                                return array(elements).unwrap();
                            }

                            // Regular (non-signal) array variable
                            let ir_type = Self::circom_type_to_ty(var_type);
                            let array_sort = ir_type.sort();

                            // Ensure variable is registered in metadata before creating IR term
                            self.ensure_var_in_metadata(&var_name, &array_sort);

                            // Create variable with proper array type
                            T::new(
                                ir_type.clone(),
                                leaf_term(Op::new_var(
                                    var_name,
                                    array_sort
                                ))
                            )
                        }
                        CircomType::Component(template_name) => {
                            // Look up the template definition
                            if let Some((_template_path, template)) = self.circom_gen.find_template(template_name) {
                                // Process template parameters and generate constraints
                                // For internal components, no signals are public
                                let mut walker = CircomStatementWalker::new(self.circom_gen, HashSet::default());
                                walker.visit_template(&mut template.clone());
                                // Ensure variable is registered in metadata
                                let field_sort = Sort::Field(cfg().field().clone());
                                self.ensure_var_in_metadata(&var_name, &field_sort);
                                // Return the component as a field element
                                T::new(Ty::Field, leaf_term(Op::new_var(var_name, field_sort)))
                            } else {
                                // Collect available template names for helpful error message
                                let mut available_templates: Vec<String> = self.circom_gen.templates
                                    .values()
                                    .flat_map(|temps| temps.keys())
                                    .map(|s| s.to_string())
                                    .collect();
                                available_templates.sort();
                                available_templates.dedup();

                                let available_list = if available_templates.is_empty() {
                                    "  (no templates defined yet)".to_string()
                                } else {
                                    available_templates.iter()
                                        .map(|t| format!("  - {}", t))
                                        .collect::<Vec<_>>()
                                        .join("\n")
                                };

                                panic!(
                                    "Template not found: '{}'\n\
                                     \n\
                                     Cannot instantiate undefined template.\n\
                                     \n\
                                     Available templates:\n\
                                     {}\n\
                                     \n\
                                     Common causes:\n\
                                     1. Template name typo or misspelling\n\
                                     2. Template defined in different file not included\n\
                                     3. Template defined after this instantiation (move definition before use)\n\
                                     4. Using 'function' keyword instead of 'template'\n\
                                     \n\
                                     Debugging steps:\n\
                                     1. Check spelling of template name '{}'\n\
                                     2. Ensure template file is included with 'include' directive\n\
                                     3. Verify template is defined before this instantiation\n\
                                     4. Check that template uses 'template' keyword, not 'function'",
                                    template_name, available_list, template_name
                                );
                            }
                        }
                        CircomType::Signal => {
                            // If we're in a component context, use qualified signal name
                            let actual_name = if let Some(comp_name) = &self.current_component {
                                // Check if this signal belongs to the current component
                                if let Some(signals) = self.component_signals.get(comp_name) {
                                    if let Some(qualified_name) = signals.get(&var_name) {
                                        qualified_name.clone()
                                    } else {
                                        var_name.clone()
                                    }
                                } else {
                                    var_name.clone()
                                }
                            } else {
                                var_name.clone()
                            };
                            // Ensure variable is registered in metadata
                            let field_sort = Sort::Field(cfg().field().clone());
                            self.ensure_var_in_metadata(&actual_name, &field_sort);
                            // Signals are field elements
                            T::new(Ty::Field, leaf_term(Op::new_var(actual_name, field_sort)))
                        }
                    }
                } else {
                    // Variable not found - might be a signal that needs special handling
                    // Try to construct a variable reference term
                    let actual_name = if let Some(comp_name) = &self.current_component {
                        format!("{}.{}", comp_name, var_name)
                    } else {
                        var_name.clone()
                    };

                    // Check if the actual_name (qualified name) exists in vars with a type
                    let var_type = self.vars.get(&actual_name).cloned();

                    // If not found, try to look it up as an unregistered signal from the current template
                    let var_type = if var_type.is_none() && self.current_component.is_some() {
                        self.lookup_unregistered_signal(&var_name)
                    } else {
                        var_type
                    };

                    if let Some(var_type) = var_type {

                        // Use the type from vars to create the proper term
                        match var_type {
                            CircomType::Array(ref _elem_ty, size) => {
                                let array_size = size;

                                // This is a signal array - construct from flattened elements
                                if self.signal_names.contains(&actual_name) {
                                    let mut elements = Vec::new();
                                    for i in 0..array_size {
                                        let elem_name = format!("{}_{}", actual_name, i);
                                        let field_sort = Sort::Field(cfg().field().clone());
                                        self.ensure_var_in_metadata(&elem_name, &field_sort);
                                        elements.push(T::new(Ty::Field, leaf_term(Op::new_var(elem_name, field_sort))));
                                    }

                                    return array(elements).unwrap();
                                } else {
                                    // Regular array variable (not a signal)
                                    // Create a single array term reference with proper nested type
                                    let ir_type = Self::circom_type_to_ty(&var_type);
                                    let array_sort = ir_type.sort();
                                    self.ensure_var_in_metadata(&actual_name, &array_sort);
                                    T::new(ir_type, leaf_term(Op::new_var(actual_name, array_sort)))
                                }
                            }
                            _ => {
                                // Non-array type - create field term
                                let field_sort = Sort::Field(cfg().field().clone());
                                self.ensure_var_in_metadata(&actual_name, &field_sort);
                                T::new(Ty::Field, leaf_term(Op::new_var(actual_name, field_sort)))
                            }
                        }
                    } else {
                        panic!(
                            "Unknown identifier '{}'. \
                             Variable/signal must be declared before use.",
                            actual_name
                        );
                    }
                }
            }
            Expression::Number(num) => {
                match num {
                    Number::Decimal(dec) => {
                        let num_str = dec.span.as_str();
                        // Remove underscores and trim whitespace (Circom allows 1_000_000)
                        let cleaned = num_str.replace('_', "").trim().to_string();

                        // Try parsing as u64 first for efficiency with small numbers
                        if let Ok(value) = cleaned.parse::<u64>() {
                            field_lit(value)
                        } else {
                            // Fall back to arbitrary-precision Integer for large field elements
                            use rug::Integer;
                            let value = Integer::from_str_radix(&cleaned, 10)
                                .unwrap_or_else(|e| panic!("Failed to parse number '{}': {}", num_str, e));
                            field_lit(value)
                        }
                    }
                    Number::Hex(hex) => {
                        let hex_str = hex.span.as_str().trim();
                        // Try parsing as u64 first for efficiency
                        if let Ok(value) = u64::from_str_radix(&hex_str[2..], 16) {
                            field_lit(value)
                        } else {
                            // Fall back to arbitrary-precision Integer for large hex values
                            use rug::Integer;
                            let value = Integer::from_str_radix(&hex_str[2..], 16)
                                .unwrap_or_else(|e| panic!("Failed to parse hex '{}': {}", hex_str, e));
                            field_lit(value)
                        }
                    }
                }
            }
            Expression::Array(arr) => {
                let elements: Vec<T> = arr.elements.iter()
                    .map(|e| self.expr_to_term(e))
                    .collect();
                array(elements).unwrap()
            }
            Expression::Postfix(postfix) => {
                // Track the base identifier for component signal access and signal array flattening
                let base_id = self.get_base_identifier(&postfix.base);


                // Special case: function calls - handle before evaluating base
                // to avoid treating function name as a variable
                if postfix.access.len() == 1 {
                    if let ast::Access::CallAccess(call) = &postfix.access[0] {
                        // This is a function call
                        let func_name = base_id.clone();

                        // Keep original argument expressions for array parameter binding
                        let arg_exprs: Vec<&ast::Expression> = call.args.iter().collect();

                        // Convert arguments to terms
                        let args: Vec<T> = call.args.iter()
                            .map(|arg| self.expr_to_term(arg))
                            .collect();

                        if let Some(name) = func_name {
                            // Check if this is a built-in function
                            match name.as_str() {
                                "assert" => {
                                    // Try to extract tag values from assertion patterns
                                    if !arg_exprs.is_empty() {
                                        self.try_extract_tag_value_from_assert(arg_exprs[0]);
                                    }
                                    // assert(condition) - add as constraint
                                    if !args.is_empty() {
                                        self.circom_gen.assert_constraint(args[0].term.clone());
                                    }
                                    return T::new_field(0); // assert returns nothing
                                }
                                _ => {
                                    // Look up user-defined function
                                    if let Some((_func_path, func_def)) = self.circom_gen.find_function(&name) {
                                        // Inline the function with both terms and original expressions
                                        // With symbolic execution, this should now handle expression arguments
                                        match self.inline_function(&func_def, args, arg_exprs) {
                                            Ok(value) => return value,
                                            Err(e) => {
                                                panic!("Error inlining function '{}': {}.", name, e);
                                            }
                                        }
                                    } else if self.circom_gen.find_template(&name).is_some() {
                                        // This is a template, not a function - this is component instantiation
                                        // Templates cannot be used in expression context
                                        panic!("Template '{}' used in expression context. Templates can only be instantiated in component statements, not used as expressions.", name);
                                    } else {
                                        // Collect available function names for helpful error message
                                        let mut available_functions: Vec<String> = self.circom_gen.functions
                                            .values()
                                            .flat_map(|funcs| funcs.keys())
                                            .map(|s| s.to_string())
                                            .collect();
                                        available_functions.sort();
                                        available_functions.dedup();

                                        let available_list = if available_functions.is_empty() {
                                            "  (no functions defined yet)".to_string()
                                        } else {
                                            available_functions.iter()
                                                .map(|f| format!("  - {}", f))
                                                .collect::<Vec<_>>()
                                                .join("\n")
                                        };

                                        panic!(
                                            "Unknown function: '{}'\n\
                                             \n\
                                             Function must be defined before being called.\n\
                                             \n\
                                             Available functions:\n\
                                             {}\n\
                                             \n\
                                             Common causes:\n\
                                             1. Function name typo or misspelling\n\
                                             2. Function defined in different file not included\n\
                                             3. Function defined after this call (move definition before use)\n\
                                             4. Missing 'function' keyword in function definition\n\
                                             \n\
                                             Debugging steps:\n\
                                             1. Check spelling of function name '{}'\n\
                                             2. Ensure function file is included with 'include' directive\n\
                                             3. Verify function is defined before this call\n\
                                             4. Check that function uses 'function' keyword, not 'template'",
                                            name, available_list, name
                                        );
                                    }
                                }
                            }
                        } else {
                            panic!(
                                "Cannot determine function name in call expression\n\
                                 \n\
                                 Failed to extract function name from function call.\n\
                                 \n\
                                 Context:\n\
                                 - Current component: {:?}\n\
                                 \n\
                                 This usually indicates:\n\
                                 1. Complex expression used where simple function name expected\n\
                                 2. Malformed function call syntax\n\
                                 3. Internal compiler error in name extraction\n\
                                 \n\
                                 Debugging steps:\n\
                                 1. Ensure function calls use simple names (e.g., 'myFunc(...)' not 'obj.func(...)')\n\
                                 2. Check function call syntax matches Circom specification\n\
                                 3. Simplify complex expressions into separate statements",
                                self.current_component
                            );
                        }
                    }
                }

                // Special case: component signal access (supports component arrays + signal arrays)
                // Handle BEFORE evaluating base to avoid creating component variable reference
                if let Some(dot_pos) = postfix.access.iter().position(|acc| matches!(acc, ast::Access::DotAccess(_))) {
                    if let Some(base_name) = &base_id {
                        // Accesses before dot are component array indices (if any)
                        let prefix = &postfix.access[..dot_pos];
                        if !prefix.iter().all(|acc| matches!(acc, ast::Access::ArrayAccess(_))) {
                            panic!(
                                "Unsupported component access pattern for '{}'. Only array indices are supported before dot access.",
                                base_name
                            );
                        }

                        let mut comp_indices = Vec::new();
                        for access in prefix {
                            if let ast::Access::ArrayAccess(array_access) = access {
                                if let Some(idx) = self.extract_constant_index_expr(&array_access.expression) {
                                    comp_indices.push(idx);
                                } else {
                                    panic!(
                                        "Component array '{}' requires constant indices before dot access. Array indices must be compile-time constants.",
                                        base_name
                                    );
                                }
                            }
                        }

                        let comp_instance_name = if comp_indices.is_empty() {
                            base_name.clone()
                        } else {
                            self.compute_component_array_instance_name(base_name, &comp_indices)
                        };

                        // Qualify the component name with current context
                        let qualified_comp_name = if let Some(parent) = &self.current_component {
                            format!("{}.{}", parent, comp_instance_name)
                        } else {
                            comp_instance_name.clone()
                        };

                        if self.component_signals.contains_key(&qualified_comp_name) {
                            let dot_access = match &postfix.access[dot_pos] {
                                ast::Access::DotAccess(dot_access) => dot_access,
                                _ => unreachable!(),
                            };
                            let signal_name = &dot_access.inner.value;

                            let mut signal_indices = Vec::new();
                            if let Some(array_access) = &dot_access.array_access {
                                if let Some(idx) = self.extract_constant_index_expr(&array_access.expression) {
                                    signal_indices.push(idx);
                                } else {
                                    panic!(
                                        "Component signal array {}.{} requires constant indices. Array indices must be compile-time constants.",
                                        qualified_comp_name, signal_name
                                    );
                                }
                            }

                            if dot_pos + 1 < postfix.access.len() {
                                let suffix = &postfix.access[dot_pos + 1..];
                                if !suffix.iter().all(|acc| matches!(acc, ast::Access::ArrayAccess(_))) {
                                    panic!(
                                        "Unsupported component signal access pattern for '{}.{}'. Only array indices are supported after dot access.",
                                        qualified_comp_name, signal_name
                                    );
                                }
                                for access in suffix {
                                    if let ast::Access::ArrayAccess(array_access) = access {
                                        if let Some(idx) = self.extract_constant_index_expr(&array_access.expression) {
                                            signal_indices.push(idx);
                                        } else {
                                            panic!(
                                                "Component signal array {}.{} requires constant indices. Array indices must be compile-time constants.",
                                                qualified_comp_name, signal_name
                                            );
                                        }
                                    }
                                }
                            }

                            if let Some(term) = self.resolve_component_signal_access(
                                &qualified_comp_name,
                                signal_name,
                                &signal_indices,
                            ) {
                                return term;
                            }
                        }
                    }
                }

                // Special case: signal arrays with constant indices
                // Check BEFORE evaluating base to avoid creating undefined variable reference
                if let Some(base_name) = &base_id {
                    // Only treat as signal array if this name isn't shadowed by a local var/parameter
                    if self.is_signal_array(base_name) && !self.var_values.contains_key(base_name) {
                        // Check if all accesses are array accesses
                        let all_array_access = postfix.access.iter().all(|acc| {
                            matches!(acc, ast::Access::ArrayAccess(_))
                        });

                        if all_array_access && !postfix.access.is_empty() {
                            // Try to extract all indices as compile-time constants
                            let mut indices = Vec::new();
                            let mut all_constant = true;

                            for access in &postfix.access {
                                if let ast::Access::ArrayAccess(array_access) = access {
                                    // Try multiple approaches to extract constant index:
                                    // 1. Direct constant expression evaluation (handles i-r, i+1, etc.)
                                    // 2. Loop variable lookup (simple identifier)
                                    // 3. Term evaluation and extraction
                                    let idx = if let Some(val) = self.extract_constant_value_expr(&array_access.expression) {
                                        Some(val)
                                    } else if let Some(val) = self.try_extract_loop_var_value(&array_access.expression) {
                                        Some(val)
                                    } else {
                                        let index_term = self.expr_to_term(&array_access.expression);
                                        Self::extract_constant_from_term(&index_term)
                                    };

                                    if let Some(idx) = idx {
                                        indices.push(idx);
                                    } else {
                                        all_constant = false;
                                        break;
                                    }
                                }
                            }

                            if all_constant {
                                // Qualify the name if we're inside a component context
                                let qualified_base = if let Some(comp_name) = &self.current_component {
                                    // Check if this signal belongs to the component
                                    if let Some(signals) = self.component_signals.get(comp_name) {
                                        if let Some(qualified_name) = signals.get(base_name) {
                                            qualified_name.clone()
                                        } else {
                                            base_name.clone()
                                        }
                                    } else {
                                        base_name.clone()
                                    }
                                } else {
                                    base_name.clone()
                                };

                                // Calculate flat index based on array dimensions
                                let flat_idx = if indices.len() == 1 {
                                    // 1D array - index is already flat
                                    indices[0]
                                } else {
                                    // Multi-dimensional array - need to calculate flat index
                                    // Get the signal type to extract dimensions
                                    if let Some(signal_type) = self.vars.get(base_name) {
                                        let dimensions = Self::extract_dimensions_from_type(signal_type);
                                        if let Some(flat) = Self::calculate_flat_index(&indices, &dimensions) {
                                            flat
                                        } else {
                                            panic!("Signal array '{}' index out of bounds or dimension mismatch. Check array dimensions and access patterns.", base_name);
                                        }
                                    } else {
                                        panic!("Signal array '{}' type not found. Array must be properly declared before use.", base_name);
                                    }
                                };

                                let flattened_name = self.get_flattened_signal_name(&qualified_base, flat_idx);
                                // Ensure variable is registered in metadata
                                let field_sort = Sort::Field(default_field());
                                self.ensure_var_in_metadata(&flattened_name, &field_sort);
                                // Return the flattened signal directly without evaluating base
                                return T::new(
                                    Ty::Field,
                                    leaf_term(Op::new_var(flattened_name, field_sort))
                                );
                            } else {
                                // PARTIAL-CONSTANT MULTI-DIMENSIONAL ACCESS FIX
                                // Handle cases like P[2][i] where some indices are constant and others are runtime
                                // We reconstruct an array slice from the flattened elements
                                // Extract partial constant indices
                                let mut partial_indices = Vec::new();
                                let mut first_runtime_dim = None;

                                for (dim_idx, access) in postfix.access.iter().enumerate() {
                                    if let ast::Access::ArrayAccess(array_access) = access {
                                        if let Some(val) = self.extract_constant_value_expr(&array_access.expression)
                                            .or_else(|| self.try_extract_loop_var_value(&array_access.expression))
                                            .or_else(|| {
                                                let index_term = self.expr_to_term(&array_access.expression);
                                                Self::extract_constant_from_term(&index_term)
                                            })
                                        {
                                            partial_indices.push(Some(val));
                                        } else {
                                            partial_indices.push(None);
                                            if first_runtime_dim.is_none() {
                                                first_runtime_dim = Some(dim_idx);
                                            }
                                        }
                                    }
                                }

                                if let Some(first_runtime) = first_runtime_dim {
                                    // Get the full dimensions from the signal type
                                    if let Some(signal_type) = self.vars.get(base_name) {
                                        let dimensions = Self::extract_dimensions_from_type(signal_type);

                                        // Check that all dimensions after the first runtime are also runtime
                                        // (We don't support mixed constant/runtime like P[i][2])
                                        let all_following_runtime = partial_indices.iter().skip(first_runtime + 1).all(|idx| idx.is_none());

                                        if !all_following_runtime {
                                            panic!(
                                                "Signal array '{}' has unsupported access pattern. \n\
                                                 Only patterns like P[const][runtime] are supported, not P[runtime][const].\n\
                                                 All dimensions after the first runtime dimension must also be runtime.",
                                                base_name
                                            );
                                        }

                                        // Qualify the base name
                                        let qualified_base = if let Some(comp_name) = &self.current_component {
                                            if let Some(signals) = self.component_signals.get(comp_name) {
                                                signals.get(base_name).cloned().unwrap_or_else(|| base_name.clone())
                                            } else {
                                                base_name.clone()
                                            }
                                        } else {
                                            base_name.clone()
                                        };

                                        // Calculate the base offset using only the constant dimensions
                                        let const_indices: Vec<i64> = partial_indices.iter()
                                            .take(first_runtime)
                                            .filter_map(|&idx| idx)
                                            .collect();

                                        let const_dimensions: Vec<usize> = dimensions.iter()
                                            .take(first_runtime)
                                            .cloned()
                                            .collect();

                                        let base_offset = if !const_indices.is_empty() {
                                            Self::calculate_flat_index(&const_indices, &const_dimensions).unwrap_or(0)
                                        } else {
                                            0
                                        };

                                        // Calculate the size of the remaining dimensions
                                        let remaining_dims: Vec<usize> = dimensions.iter()
                                            .skip(first_runtime)
                                            .cloned()
                                            .collect();
                                        let slice_size: usize = remaining_dims.iter().product();

                                        // Reconstruct array from flattened elements
                                        let field_sort = Sort::Field(default_field());
                                        let mut array_elements = Vec::new();

                                        for i in 0..slice_size {
                                            let flat_idx = base_offset + (i as i64);
                                            let flattened_name = self.get_flattened_signal_name(&qualified_base, flat_idx);
                                            self.ensure_var_in_metadata(&flattened_name, &field_sort);
                                            array_elements.push(
                                                T::new(
                                                    Ty::Field,
                                                    leaf_term(Op::new_var(flattened_name, field_sort.clone()))
                                                )
                                            );
                                        }

                                        // Build the array structure for remaining dimensions
                                        let array_result = self.build_nested_array(array_elements, &remaining_dims);

                                        // Now apply the remaining runtime array accesses
                                        let mut result = array_result;
                                        for access in postfix.access.iter().skip(first_runtime) {
                                            if let ast::Access::ArrayAccess(array_access) = access {
                                                let index_term = self.expr_to_term(&array_access.expression);
                                                result = array_select(result, index_term)
                                                    .unwrap_or_else(|e| panic!("Array select failed: {}", e));
                                            }
                                        }

                                        return result;
                                    } else {
                                        panic!("Signal array '{}' type not found for partial-constant access.", base_name);
                                    }
                                }
                            }
                        }
                        // Debug: Try to understand why constant extraction failed
                        let debug_info = if postfix.access.len() == 1 {
                            if let ast::Access::ArrayAccess(aa) = &postfix.access[0] {
                                format!("Index expression: {:?}. Available vars in var_values: {:?}",
                                    aa.expression,
                                    self.var_values.keys().collect::<Vec<_>>())
                            } else {
                                "Not a simple array access".to_string()
                            }
                        } else {
                            format!("{} accesses", postfix.access.len())
                        };
                        panic!("Signal array '{}' requires constant index access. All array indices must be compile-time constants in Circom. Debug: {}", base_name, debug_info);
                    }
                }

                // Normal case: evaluate base for non-signal arrays, non-function calls, and non-component access
                let base = self.expr_to_term(&postfix.base);
                let mut result = base;

                for access in &postfix.access {
                    match access {
                        ast::Access::ArrayAccess(array_access) => {
                            // First, try to extract the index as a compile-time constant
                            // to check for negative values before creating IR terms
                            if let Some(idx_val) = self.extract_constant_value_expr(&array_access.expression) {
                                if idx_val < 0 {
                                    let base_name_str = base_id.as_deref().unwrap_or("unknown");
                                    panic!("Negative array index {} for array '{}'. Array indices must be non-negative integers. \
                                           This often happens from expressions like 'i-1' when i=0 in loops. \
                                           Index expression: {:?}",
                                           idx_val, base_name_str, array_access.expression);
                                }
                            }

                            let index = self.expr_to_term(&array_access.expression);

                            // Double-check after term conversion
                            if let Some(const_idx) = Self::extract_constant_from_term(&index) {
                                if const_idx < 0 {
                                    let base_name_str = base_id.as_deref().unwrap_or("unknown");
                                    panic!("Negative array index {} for array '{}' after term conversion. Array indices must be non-negative integers. \
                                           This often happens from expressions like 'i-1' when i=0 in loops.",
                                           const_idx, base_name_str);
                                }
                            }

                            // Check if this is a var array with compile-time value
                            if let Some(base_name) = &base_id {

                                if let Some(const_idx) = Self::extract_constant_from_term(&index) {
                                    if let Some(array_val) = self.var_values.get(base_name) {
                                        // This is a var array - try to extract the element value
                                        let elem_val = match array_val {
                                            CompileTimeValue::Array1D(arr) => {
                                                let idx = const_idx as usize;
                                                if idx < arr.len() {
                                                    Some(arr[idx].clone())
                                                } else {
                                                    None
                                                }
                                            }
                                            CompileTimeValue::Array2D(_) => {
                                                // For 2D arrays, we need the next access too
                                                // This is handled below by chaining accesses
                                                None
                                            }
                                            CompileTimeValue::Expression(arr_term) => {
                                                // Array stored as expression - use array_select to get element
                                                // Override result to be the array term before selection
                                                result = arr_term.clone();
                                                None // Fall through to array_select below
                                            }
                                            CompileTimeValue::ExprArray1D(arr) => {
                                                // Array of expressions - return the element expression as a term
                                                let idx = const_idx as usize;
                                                if idx < arr.len() {
                                                    // The element is already a term, return it directly by overriding result
                                                    result = arr[idx].clone();
                                                    // Return a dummy value to skip array_select
                                                    Some(rug::Integer::from(0)) // Will not be used since result is already set
                                                } else {
                                                    None
                                                }
                                            }
                                            _ => None,
                                        };

                                        if let Some(val) = elem_val {
                                            // Only set result if elem_val is a concrete value
                                            if !matches!(array_val, CompileTimeValue::ExprArray1D(_)) {
                                                result = field_lit(val);
                                            }
                                            continue;
                                        }

                                        if let Some(val) = elem_val {
                                            result = field_lit(val);
                                            continue;
                                        }
                                    } else {
                                    }
                                } else {
                                }
                            }

                            // Default: use array selection for non-var arrays or runtime indices
                            match array_select(result.clone(), index.clone()) {
                                Ok(selected) => result = selected,
                                Err(e) => {
                                    let base_name_str = base_id.as_deref().unwrap_or("unknown");
                                    // Debug: show var_values state for this variable
                                    let var_info = base_id.as_ref().and_then(|name| {
                                        self.var_values.get(name.as_str()).map(|v| {
                                            match v {
                                                CompileTimeValue::Scalar(i) => format!("Scalar({})", i),
                                                CompileTimeValue::Array1D(arr) => format!("Array1D(len={})", arr.len()),
                                                CompileTimeValue::Array2D(arr) => format!("Array2D({}x{})", arr.len(), arr.first().map_or(0, |r| r.len())),
                                                CompileTimeValue::Expression(_) => "Expression".to_string(),
                                                CompileTimeValue::ExprArray1D(arr) => format!("ExprArray1D(len={})", arr.len()),
                                                CompileTimeValue::ArrayND(_) => "ArrayND".to_string(),
                                            }
                                        })
                                    });
                                    panic!("Array selection failed for '{}': {}. Base type: {:?}, Index type: {:?}. var_values entry: {:?}. This indicates that '{}' is not properly typed as an array.",
                                           base_name_str, e, result.type_(), index.type_(), var_info, base_name_str);
                                }
                            }
                        }
                        ast::Access::DotAccess(dot_access) => {
                            let signal_name = &dot_access.inner.value;

                            // Look up component signal
                            if let Some(base_name) = &base_id {
                                // Qualify the component name with current context
                                let qualified_comp_name = if let Some(parent) = &self.current_component {
                                    format!("{}.{}", parent, base_name)
                                } else {
                                    base_name.clone()
                                };
                                if let Some(signals) = self.component_signals.get(&qualified_comp_name) {
                                    if let Some(var_name) = signals.get(signal_name) {
                                        // Look up the variable
                                        if let Some(_var_type) = self.vars.get(var_name) {
                                            // Ensure variable is registered in metadata
                                            let field_sort = Sort::Field(default_field());
                                            self.ensure_var_in_metadata(var_name, &field_sort);
                                            result = T::new(
                                                Ty::Field,
                                                leaf_term(Op::new_var(var_name.clone(), field_sort))
                                            );
                                        }
                                    }
                                }
                            }
                        }
                        ast::Access::Increment(_) => {
                            result = increment(result).unwrap();
                        }
                        ast::Access::Decrement(_) => {
                            result = decrement(result).unwrap();
                        }
                        ast::Access::CallAccess(call) => {
                            // Function/template call
                            // Get the function name from the base
                            let func_name = self.get_base_identifier(&postfix.base);

                            // Keep original argument expressions for array parameter binding
                            let arg_exprs: Vec<&ast::Expression> = call.args.iter().collect();

                            // Convert arguments to terms
                            let args: Vec<T> = call.args.iter()
                                .map(|arg| self.expr_to_term(arg))
                                .collect();
                            let args_len = args.len();

                            if let Some(name) = func_name {
                                // Check if this is a built-in function
                                match name.as_str() {
                                    // Built-in functions that can be directly translated
                                    "assert" => {
                                        // Try to extract tag values from assertion patterns
                                        if !arg_exprs.is_empty() {
                                            self.try_extract_tag_value_from_assert(arg_exprs[0]);
                                        }
                                        // assert(condition) - add as constraint
                                        if !args.is_empty() {
                                            self.circom_gen.assert_constraint(args[0].term.clone());
                                        }
                                        result = T::new_field(0); // assert returns nothing
                                    }
                                    _ => {
                                        // Look up user-defined function
                                        if let Some((_func_path, func_def)) = self.circom_gen.find_function(&name) {
                                            // Inline the function
                                            match self.inline_function(&func_def, args, arg_exprs) {
                                                Ok(value) => result = value,
                                                Err(e) => {
                                                    panic!(
                                                        "Function call evaluation failed: {}\n\
                                                         \n\
                                                         Context:\n\
                                                         - Function name: '{}'\n\
                                                         - Number of arguments: {}\n\
                                                         - Current component: {:?}\n\
                                                         \n\
                                                         This error occurs when a function cannot be properly evaluated\n\
                                                         at compile-time during template instantiation.\n\
                                                         \n\
                                                         Common causes:\n\
                                                         1. Function contains signal operations that can't be evaluated at compile time\n\
                                                         2. Function uses variables that haven't been initialized\n\
                                                         3. Function contains unsupported operations for compile-time evaluation\n\
                                                         4. Recursive function calls without proper base case\n\
                                                         5. Function accesses undefined variables or arrays\n\
                                                         \n\
                                                         Debugging steps:\n\
                                                         1. Review function '{}' implementation for signal dependencies\n\
                                                         2. Ensure all function arguments are compile-time constants\n\
                                                         3. Check that function doesn't depend on signals\n\
                                                         4. Verify all variables used in function are properly initialized\n\
                                                         5. Use CIRCOM_DEBUG=1 to see detailed evaluation trace",
                                                        e,
                                                        name,
                                                        args_len,
                                                        self.current_component,
                                                        name
                                                    );
                                                }
                                            }
                                        } else {
                                            // Collect available function names for helpful error message
                                            let mut available_functions: Vec<String> = self.circom_gen.functions
                                                .values()
                                                .flat_map(|funcs| funcs.keys())
                                                .map(|s| s.to_string())
                                                .collect();
                                            available_functions.sort();
                                            available_functions.dedup();

                                            let available_list = if available_functions.is_empty() {
                                                "  (no functions defined yet)".to_string()
                                            } else {
                                                available_functions.iter()
                                                    .map(|f| format!("  - {}", f))
                                                    .collect::<Vec<_>>()
                                                    .join("\n")
                                            };

                                            panic!(
                                                "Unknown function: '{}'\n\
                                                 \n\
                                                 Function must be defined before being called.\n\
                                                 \n\
                                                 Available functions:\n\
                                                 {}\n\
                                                 \n\
                                                 Common causes:\n\
                                                 1. Function name typo or misspelling\n\
                                                 2. Function defined in different file not included\n\
                                                 3. Function defined after this call (move definition before use)\n\
                                                 4. Missing 'function' keyword in function definition\n\
                                                 \n\
                                                 Debugging steps:\n\
                                                 1. Check spelling of function name '{}'\n\
                                                 2. Ensure function file is included with 'include' directive\n\
                                                 3. Verify function is defined before this call\n\
                                                 4. Check that function uses 'function' keyword, not 'template'",
                                                name, available_list, name
                                            );
                                        }
                                    }
                                }
                            } else {
                                panic!("Cannot determine function name in call expression");
                            }
                        }
                    }
                }
                result
            }
        }
    }

    /// Inline a function call by executing its body with argument bindings
    fn inline_function(
        &mut self,
        func_def: &circom_pest_ast::FunctionDefinition<'ast>,
        args: Vec<T>,
        arg_exprs: Vec<&ast::Expression<'ast>>,
    ) -> Result<T, String> {

        // Detect if runtime expansion is needed
        let needs_runtime_expansion = self.check_needs_runtime_expansion(&args, &arg_exprs);

        if needs_runtime_expansion {
            // Runtime expansion not yet fully implemented for all AST versions
            // Fall back to compile-time inlining for now
            self.inline_function_compile_time(func_def, args, arg_exprs)
        } else {
            self.inline_function_compile_time(func_def, args, arg_exprs)
        }
    }

    /// Check if a function call needs runtime expansion
    /// Returns true if any argument is runtime-dependent (not a compile-time constant)
    fn check_needs_runtime_expansion(
        &self,
        args: &[T],
        arg_exprs: &[&ast::Expression<'ast>],
    ) -> bool {
        for (_i, (arg, arg_expr)) in args.iter().zip(arg_exprs.iter()).enumerate() {
            // Check 1: Can we extract compile-time constant from AST?
            if self.extract_constant_value_expr_big(arg_expr).is_some() {
                continue;
            }

            // Check 2: Is it an identifier in var_values (compile-time array)?
            if let ast::Expression::Identifier(id) = arg_expr {
                if self.var_values.contains_key(&id.value) {
                    continue;
                }
            }

            // Check 3: Is the IR term a constant?
            if Self::extract_constant_from_term(arg).is_some() {
                continue;
            }

            // This argument is runtime-dependent
            return true;
        }
        false
    }

    /// Inline a function call at compile-time by executing its body with argument bindings
    fn inline_function_compile_time(
        &mut self,
        func_def: &circom_pest_ast::FunctionDefinition<'ast>,
        args: Vec<T>,
        arg_exprs: Vec<&ast::Expression<'ast>>,
    ) -> Result<T, String> {

        // Validate argument count
        if func_def.params.len() != args.len() {
            return Err(format!(
                "Function {} expects {} arguments, got {}",
                func_def.id.value, func_def.params.len(), args.len()
            ));
        }

        // Save current variable state (to restore after function returns)
        let saved_var_values = self.var_values.clone();
        let saved_vars = self.vars.clone();
        let saved_scope_len = self.function_scopes.len();
        // New local scope for this function inlining
        self.function_scopes.push(HashSet::default());

        // Bind parameters to argument values
        for ((param, arg), arg_expr) in func_def.params.iter().zip(args.iter()).zip(arg_exprs.iter()) {
            let param_name = param.value.clone();
            // Track parameter as a local name in this function scope
            if let Some(scope) = self.function_scopes.last_mut() {
                scope.insert(param_name.clone());
            }


            // Check if the argument expression is a simple identifier referencing an array
            let mut bound_as_array = false;


            if let ast::Expression::Identifier(id) = arg_expr {
                let var_name = &id.value;

                // Look up this variable in SAVED var_values (before binding current function's parameters)
                // This prevents parameters from shadowing arguments that reference outer scope variables
                if let Some(var_value) = saved_var_values.get(var_name).cloned() {
                    match &var_value {
                        CompileTimeValue::Array1D(_) | CompileTimeValue::Array2D(_) | CompileTimeValue::ArrayND(_) |
                        CompileTimeValue::ExprArray1D(_) => {
                            // This parameter should be bound as an array

                            // Compute array type before mutable borrow
                            let array_type = match &var_value {
                                CompileTimeValue::Array1D(arr) => {
                                    Self::build_circom_array_type(&vec![arr.len()])
                                }
                                CompileTimeValue::Array2D(arr) => {
                                    let dim2 = arr.get(0).map(|r| r.len()).unwrap_or(0);
                                    Self::build_circom_array_type(&vec![arr.len(), dim2])
                                }
                                _ => CircomType::Field, // fallback
                            };


                            // Now insert with mutable borrows
                            self.var_values.insert(param_name.clone(), var_value);
                            self.vars.insert(param_name.clone(), array_type.clone());

                            // Verify insertion

                            bound_as_array = true;
                        }
                        CompileTimeValue::Scalar(_) => {
                            // Bind scalar parameter from identifier
                            self.var_values.insert(param_name.clone(), var_value);
                            self.vars.insert(param_name.clone(), CircomType::Field);
                            bound_as_array = true; // Set to skip the fallback binding below
                        }
                        CompileTimeValue::Expression(term) => {
                            // Bind expression parameter - infer type from the term
                            let param_type = Self::term_to_circom_type(term);
                            self.var_values.insert(param_name.clone(), var_value);
                            self.vars.insert(param_name.clone(), param_type);
                            bound_as_array = true; // Set to skip the fallback binding below
                        }
                    }
                } else {
                }
            } else {
            }

            if !bound_as_array {
                // Try to extract compile-time constant from AST expression first (more powerful)
                let const_val_opt = self.extract_constant_value_expr_big(arg_expr);

                if let Some(const_val) = const_val_opt {
                    self.var_values.insert(param_name.clone(), CompileTimeValue::scalar_big(const_val));
                    self.vars.insert(param_name, CircomType::Field);
                } else if let Some(const_val) = Self::extract_constant_from_term(arg) {
                    // Fallback: try to extract from IR term
                    self.var_values.insert(param_name.clone(), CompileTimeValue::scalar(const_val));
                    self.vars.insert(param_name, CircomType::Field);
                } else if let Some(array_val) = CompileTimeValue::try_from_term(arg) {
                    // Handle array-valued arguments (including expression arrays)
                    if matches!(array_val,
                        CompileTimeValue::Array1D(_) |
                        CompileTimeValue::Array2D(_) |
                        CompileTimeValue::ArrayND(_) |
                        CompileTimeValue::ExprArray1D(_)
                    ) {
                        let param_type = Self::ty_to_circom_type(arg.type_());
                        self.var_values.insert(param_name.clone(), array_val);
                        self.vars.insert(param_name, param_type);
                    } else if matches!(array_val, CompileTimeValue::Expression(_)) && matches!(arg.type_(), Ty::Array(_, _)) {
                        let param_type = Self::ty_to_circom_type(arg.type_());
                        self.var_values.insert(param_name.clone(), CompileTimeValue::Expression(arg.clone()));
                        self.vars.insert(param_name, param_type);
                    } else {
                        // Fall through to generic expression handling below
                        // Argument is not a compile-time constant - store as expression

                        // Check if this is an array type
                        match arg.type_() {
                            Ty::Array(_, _) => {
                                // Array type - store expression and preserve type
                                self.var_values.insert(param_name.clone(), CompileTimeValue::Expression(arg.clone()));
                                let param_type = Self::ty_to_circom_type(arg.type_());
                                self.vars.insert(param_name, param_type);
                            }
                            _ => {
                                // Scalar expression
                                self.var_values.insert(param_name.clone(), CompileTimeValue::Expression(arg.clone()));
                                self.vars.insert(param_name, CircomType::Field);
                            }
                        }
                    }
                } else {
                    // Argument is not a compile-time constant - store as expression
                    // This enables symbolic execution: the function can work with expressions

                    // Check if this is an array type
                    match arg.type_() {
                        Ty::Array(_, _) => {
                            // Array type - we need special handling
                            // For now, store as Expression and handle element access specially
                            self.var_values.insert(param_name.clone(), CompileTimeValue::Expression(arg.clone()));
                            let param_type = Self::ty_to_circom_type(arg.type_());
                            self.vars.insert(param_name, param_type);
                        }
                        _ => {
                            // Scalar expression
                            self.var_values.insert(param_name.clone(), CompileTimeValue::Expression(arg.clone()));
                            self.vars.insert(param_name, CircomType::Field);
                        }
                    }
                }
            }
        }

        // Execute function body, looking for return statement (including nested)
        self.has_returned = false;
        self.function_return_value = None;

        for stmt in &func_def.statements {
            let mut stmt_clone = stmt.clone();
            self.visit_statement(&mut stmt_clone);
            if self.has_returned {
                break;
            }
        }

        let return_value = self.function_return_value.take().unwrap_or_else(T::new_field_zero);
        self.has_returned = false;

        // Restore variable state
        self.var_values = saved_var_values;
        self.vars = saved_vars;
        // Restore function scope stack
        while self.function_scopes.len() > saved_scope_len {
            self.function_scopes.pop();
        }

        Ok(return_value)
    }

    /// Expand a function call at runtime by generating IR constraints
    /// This is used when function parameters are signal-derived values
    /// NOTE: Currently disabled due to AST compatibility issues
    #[allow(dead_code)]
    #[cfg(any())]  // Disabled - not compatible with circom feature
    fn expand_function_runtime(
        &mut self,
        func_def: &circom_pest_ast::FunctionDefinition<'ast>,
        args: Vec<T>,
        arg_exprs: Vec<&ast::Expression<'ast>>,
    ) -> Result<T, String> {

        // Validate argument count
        if func_def.params.len() != args.len() {
            return Err(format!(
                "Function {} expects {} arguments, got {}",
                func_def.id.value, func_def.params.len(), args.len()
            ));
        }

        // Validate that this function can be expanded at runtime
        self.validate_runtime_expansion_support(func_def)?;

        // Save current scope
        let saved_var_values = self.var_values.clone();
        let saved_vars = self.vars.clone();

        // Create runtime parameter bindings
        // Store mapping: param_name -> arg_term for use in expression evaluation
        let mut runtime_params: HashMap<String, T> = HashMap::default();

        for ((param, arg), arg_expr) in func_def.params.iter().zip(args.iter()).zip(arg_exprs.iter()) {
            let param_name = param.value.clone();

            // Check if this is a compile-time constant we can bind
            if let ast::Expression::Identifier(id) = arg_expr {
                let var_name = &id.value;
                if let Some(var_value) = saved_var_values.get(var_name).cloned() {
                    // This is a compile-time array/value, bind it
                    self.var_values.insert(param_name.clone(), var_value.clone());

                    let var_type = match &var_value {
                        CompileTimeValue::Array1D(arr) => {
                            Self::build_circom_array_type(&vec![arr.len()])
                        }
                        CompileTimeValue::Array2D(arr) => {
                            let dim2 = arr.get(0).map(|r| r.len()).unwrap_or(0);
                            Self::build_circom_array_type(&vec![arr.len(), dim2])
                        }
                        CompileTimeValue::Scalar(_) => CircomType::Field,
                        CompileTimeValue::Expression(term) => Self::term_to_circom_type(term),
                        CompileTimeValue::ExprArray1D(arr) => {
                            if !arr.is_empty() {
                                CircomType::Array(Box::new(CircomType::Field), arr.len())
                            } else {
                                CircomType::Field
                            }
                        }
                        _ => CircomType::Field,
                    };
                    self.vars.insert(param_name.clone(), var_type);
                    continue;
                }
            }

            // Check if arg can be evaluated as compile-time constant
            if let Some(const_val) = self.extract_constant_value_expr(arg_expr) {
                self.var_values.insert(param_name.clone(), CompileTimeValue::Scalar(const_val));
                self.vars.insert(param_name.clone(), CircomType::Field);
                continue;
            }

            if let Some(const_val) = Self::extract_constant_from_term(arg) {
                self.var_values.insert(param_name.clone(), CompileTimeValue::Scalar(const_val));
                self.vars.insert(param_name.clone(), CircomType::Field);
                continue;
            }

            // Store the IR term for runtime parameters
            runtime_params.insert(param_name.clone(), arg.clone());

            // Register type in vars
            let param_type = Self::term_to_circom_type(arg);
            self.vars.insert(param_name, param_type);
        }

        // Process function statements in runtime context
        let mut return_term: Option<T> = None;

        for stmt in &func_def.statements {
            match stmt {
                ast::Statement::Return(ret) => {
                    if let Some(expr) = &ret.expression {
                        // Convert return expression to IR term using runtime parameter bindings
                        return_term = Some(self.expr_to_term_with_runtime_params(expr, &runtime_params));
                    }
                    break;
                }
                ast::Statement::Variable(var_stmt) => {
                    self.process_variable_runtime(var_stmt, &runtime_params)?;
                }
                ast::Statement::For(for_stmt) => {
                    self.process_for_runtime(for_stmt, &runtime_params)?;
                }
                ast::Statement::While(_) => {
                    return Err(format!(
                        "While loops not supported in runtime function expansion for '{}'",
                        func_def.id.value
                    ));
                }
                ast::Statement::Substitution(subst) => {
                    self.process_substitution_runtime(subst, &runtime_params)?;
                }
                _ => {
                    return Err(format!(
                        "Statement type not supported in runtime function expansion for '{}': {:?}",
                        func_def.id.value, stmt
                    ));
                }
            }
        }

        // Restore scope
        self.var_values = saved_var_values;
        self.vars = saved_vars;

        // Return the runtime term
        return_term.ok_or_else(||
            format!("Function {} has no return value in runtime expansion", func_def.id.value))
    }

    /// Validate that a function can be expanded at runtime
    #[allow(dead_code)]
    #[cfg(any())]
    fn validate_runtime_expansion_support(
        &self,
        func_def: &circom_pest_ast::FunctionDefinition<'ast>,
    ) -> Result<(), String> {
        // Check for while loops
        for stmt in &func_def.statements {
            if matches!(stmt, ast::Statement::While(_)) {
                return Err(format!(
                    "Function '{}' contains while loops, which are not supported in runtime expansion",
                    func_def.id.value
                ));
            }
        }
        Ok(())
    }

    /// Convert expression to term with runtime parameter bindings
    #[allow(dead_code)]
    #[cfg(any())]
    fn expr_to_term_with_runtime_params(
        &mut self,
        expr: &ast::Expression<'ast>,
        runtime_params: &HashMap<String, T>,
    ) -> T {
        // When expr is an identifier, check runtime_params first
        if let ast::Expression::Identifier(id) = expr {
            if let Some(term) = runtime_params.get(&id.value) {
                return term.clone();
            }
        }

        // For postfix expressions (array access), we need special handling
        if let ast::Expression::Postfix(postfix) = expr {
            if let ast::Expression::Identifier(base_id) = &*postfix.base {
                // Check if base is a runtime parameter
                if let Some(base_term) = runtime_params.get(&base_id.value) {

                    // Process accesses on the runtime parameter
                    let mut result = base_term.clone();
                    for access in &postfix.accesses {
                        if let ast::Access::ArrayAccess(arr_access) = access {
                            let index = self.expr_to_term_with_runtime_params(&arr_access.access, runtime_params);
                            result = array_select(result, index).unwrap();
                        }
                    }
                    return result;
                }
            }
        }

        // Otherwise use normal expr_to_term
        self.expr_to_term(expr)
    }

    /// Process variable declaration in runtime context
    #[allow(dead_code)]
    #[cfg(any())]
    fn process_variable_runtime(
        &mut self,
        var_stmt: &ast::VariableStatement<'ast>,
        runtime_params: &HashMap<String, T>,
    ) -> Result<(), String> {
        for assignee in &var_stmt.assignees {
            let var_name = assignee.id.value.clone();

            if let Some(init) = &assignee.initialization {
                // Convert to IR term (don't evaluate to constant)
                let init_term = self.expr_to_term_with_runtime_params(init, runtime_params);

                // Try to extract compile-time value
                if let Some(const_val) = CompileTimeValue::try_from_term(&init_term) {
                    self.var_values.insert(var_name.clone(), const_val);
                } else {
                    // Don't store in var_values since it's runtime-dependent
                }

                // Register type
                let var_type = Self::term_to_circom_type(&init_term);
                self.vars.insert(var_name.clone(), var_type);
            } else {
                // Uninitialized array - determine dimensions
                if !assignee.dimensions.is_empty() {
                    let dims: Vec<usize> = assignee.dimensions.iter()
                        .map(|dim_expr| {
                            let dim_term = self.expr_to_term(dim_expr);
                            Self::extract_constant_from_term(&dim_term)
                                .ok_or_else(|| format!("Array dimension must be compile-time constant"))
                                .map(|v| v as usize)
                        })
                        .collect::<Result<Vec<_>, String>>()?;

                    let array_type = Self::build_circom_array_type(&dims);
                    self.vars.insert(var_name.clone(), array_type);

                    // Initialize compile-time array
                    let init_val = match dims.len() {
                        1 => CompileTimeValue::Array1D(vec![0; dims[0]]),
                        2 => CompileTimeValue::Array2D(vec![vec![0; dims[1]]; dims[0]]),
                        _ => return Err(format!("Arrays with {} dimensions not yet supported in runtime expansion", dims.len())),
                    };
                    self.var_values.insert(var_name, init_val);
                } else {
                    self.vars.insert(var_name.clone(), CircomType::Field);
                }
            }
        }
        Ok(())
    }

    /// Process for loop in runtime context
    #[allow(dead_code)]
    #[cfg(any())]
    fn process_for_runtime(
        &mut self,
        for_stmt: &ast::ForStatement<'ast>,
        runtime_params: &HashMap<String, T>,
    ) -> Result<(), String> {
        // Loop bounds MUST be compile-time constant
        let start = self.extract_constant_value_expr(&for_stmt.start)
            .ok_or_else(|| format!(
                "For loop start must be compile-time constant in runtime function"
            ))?;

        let end = self.extract_constant_value_expr(&for_stmt.end)
            .ok_or_else(|| format!(
                "For loop end must be compile-time constant in runtime function"
            ))?;


        // Unroll loop at compile-time, but statements may generate IR
        for i in start..end {
            // Set loop variable as compile-time constant
            self.var_values.insert(
                for_stmt.variable.value.clone(),
                CompileTimeValue::Scalar(i)
            );

            // Process body statements (may use runtime_params)
            for stmt in &for_stmt.statements {
                match stmt {
                    ast::Statement::Variable(var_stmt) => {
                        self.process_variable_runtime(var_stmt, runtime_params)?;
                    }
                    ast::Statement::Substitution(subst) => {
                        self.process_substitution_runtime(subst, runtime_params)?;
                    }
                    ast::Statement::For(nested_for) => {
                        self.process_for_runtime(nested_for, runtime_params)?;
                    }
                    _ => {
                        return Err(format!("Unsupported statement type in for loop body: {:?}", stmt));
                    }
                }
            }
        }

        Ok(())
    }

    /// Process substitution (assignment) in runtime context
    #[allow(dead_code)]
    #[cfg(any())]
    fn process_substitution_runtime(
        &mut self,
        subst: &ast::SubstitutionStatement<'ast>,
        runtime_params: &HashMap<String, T>,
    ) -> Result<(), String> {
        // Evaluate the RHS expression
        let rhs_term = self.expr_to_term_with_runtime_params(&subst.rhs, runtime_params);

        // Handle LHS assignment
        match &subst.target {
            ast::AssigneeTarget::Single(assignee) => {
                let var_name = assignee.id.value.clone();

                // Check if this is an array element assignment
                if !assignee.accesses.is_empty() {

                    // Get indices
                    let mut indices = Vec::new();
                    for access in &assignee.accesses {
                        if let ast::Access::ArrayAccess(arr_access) = access {
                            let index_term = self.expr_to_term_with_runtime_params(&arr_access.access, runtime_params);
                            if let Some(idx) = Self::extract_constant_from_term(&index_term) {
                                indices.push(idx as usize);
                            } else {
                                return Err(format!("Array index must be compile-time constant in runtime function"));
                            }
                        }
                    }

                    // Try to update compile-time array
                    if let Some(array_val) = self.var_values.get_mut(&var_name) {
                        if let Some(const_val) = Self::extract_constant_from_term(&rhs_term) {
                            match array_val {
                                CompileTimeValue::Array1D(arr) => {
                                    if indices.len() == 1 && indices[0] < arr.len() {
                                        arr[indices[0]] = const_val;
                                    }
                                }
                                CompileTimeValue::Array2D(arr) => {
                                    if indices.len() == 2 && indices[0] < arr.len() && indices[1] < arr[indices[0]].len() {
                                        arr[indices[0]][indices[1]] = const_val;
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                } else {

                    // Try to extract compile-time value
                    if let Some(const_val) = CompileTimeValue::try_from_term(&rhs_term) {
                        self.var_values.insert(var_name.clone(), const_val);
                    }
                    // If not compile-time, just update the type
                    let var_type = Self::term_to_circom_type(&rhs_term);
                    self.vars.insert(var_name, var_type);
                }
            }
            _ => {
                return Err("Tuple assignments not supported in runtime function expansion".to_string());
            }
        }

        Ok(())
    }

    /// Infer CircomType from IR term
    #[allow(dead_code)]
    fn term_to_circom_type(term: &T) -> CircomType {
        Self::ty_to_circom_type(term.type_())
    }

    /// Convert IR Ty to CircomType (preserves nested arrays)
    fn ty_to_circom_type(ty: &Ty) -> CircomType {
        match ty {
            Ty::Field => CircomType::Field,
            Ty::Array(size, elem_ty) => {
                let elem_type = Self::ty_to_circom_type(elem_ty);
                CircomType::Array(Box::new(elem_type), *size)
            }
        }
    }

    /// Look up a signal that hasn't been registered yet by scanning the current template's statements
    /// This is a placeholder - currently returns None
    fn lookup_unregistered_signal(&self, _signal_name: &str) -> Option<CircomType> {
        None
    }

    /// Execute a while loop at compile-time by evaluating the condition and running the body
    fn execute_while_loop(&mut self, while_stmt: &mut ast::WhileStatement<'ast>) {

        // Safety limit to prevent infinite loops
        let max_iterations = 10000;
        let mut iterations = 0;

        loop {
            // Check iteration limit
            if iterations >= max_iterations {
                panic!(
                    "While loop exceeded maximum iterations ({})\n\
                     \n\
                     This indicates an infinite loop or incorrect loop condition.\n\
                     \n\
                     Context:\n\
                     - Condition: {:?}\n\
                     - Current component: {:?}\n\
                     - Iterations completed: {}\n\
                     \n\
                     Common causes:\n\
                     1. Loop condition never becomes false\n\
                     2. Loop variable not properly updated in loop body\n\
                     3. Loop condition depends on uninitialized variable\n\
                     4. Off-by-one error in loop logic\n\
                     \n\
                     Debugging steps:\n\
                     1. Check that loop variables are properly initialized\n\
                     2. Verify loop variables are updated in the loop body\n\
                     3. Add debug output to track loop variable values\n\
                     4. Consider using a for loop with explicit bounds instead",
                    max_iterations,
                    while_stmt.condition,
                    self.current_component,
                    iterations
                );
            }
            iterations += 1;

            // Evaluate condition
            let condition = self.expr_to_term(&while_stmt.condition);
            let condition_value = Self::extract_constant_from_term(&condition);


            // Check if condition is true (non-zero)
            if let Some(val) = condition_value {
                if val == 0 {
                    break;  // Condition is false, exit loop
                }
            } else {
                // Cannot evaluate condition at compile-time
                panic!(
                    "While loop condition cannot be evaluated at compile-time\n\
                     \n\
                     All loop conditions in functions must be compile-time constants.\n\
                     \n\
                     Context:\n\
                     - Condition: {:?}\n\
                     - Current component: {:?}\n\
                     - Iteration: {}\n\
                     \n\
                     Common causes:\n\
                     1. Condition depends on signals (signals are not known at compile time)\n\
                     2. Condition uses variables that haven't been evaluated\n\
                     3. Condition contains function calls that failed to evaluate\n\
                     4. Condition uses runtime expressions\n\
                     \n\
                     Debugging steps:\n\
                     1. Ensure all variables in condition are assigned compile-time constants\n\
                     2. Check that functions used in condition are properly defined and evaluated\n\
                     3. Verify variables are initialized before the while loop\n\
                     4. Consider using a for loop with constant bounds if possible",
                    while_stmt.condition,
                    self.current_component,
                    iterations
                );
            }

            // Execute loop body
            for s in &mut while_stmt.statements.clone() {
                self.visit_statement(s);
            }
        }

    }

    /// Get the base identifier from an expression (for component signal access)
    fn get_base_identifier(&self, expr: &Expression) -> Option<String> {
        match expr {
            Expression::Identifier(id) => Some(id.value.clone()),
            Expression::Postfix(postfix) => {
                // Recursively extract base from postfix expressions
                // For intermediate[0], base is intermediate (an Identifier)
                self.get_base_identifier(&postfix.base)
            }
            _ => None,
        }
    }

    /// Convert an AssigneeTarget to a term
    fn assignee_target_to_term(&mut self, target: &ast::AssigneeTarget<'ast>) -> T {
        // AssigneeTarget can be Single or Tuple
        match target {
            ast::AssigneeTarget::Single(assignee) => {
                // Get the identifier and handle any array accesses
                let var_name = assignee.id.value.clone();

                // Check if this has accesses (e.g., arr[0] or component.signal)
                if !assignee.accesses.is_empty() {
                    // Special case: component signal access (supports component arrays)
                    // Handle BEFORE other access types
                    if let Some(dot_pos) = assignee.accesses.iter().position(|acc| matches!(acc, ast::AssigneeAccess::Dot(_))) {
                        let prefix = &assignee.accesses[..dot_pos];
                        if !prefix.iter().all(|acc| matches!(acc, ast::AssigneeAccess::Select(_))) {
                            panic!(
                                "Unsupported component access pattern for '{}'. Only array indices are supported before dot access.",
                                var_name
                            );
                        }

                        let mut comp_indices = Vec::new();
                        for access in prefix {
                            if let ast::AssigneeAccess::Select(array_access) = access {
                                if let Some(idx) = self.extract_constant_index_expr(&array_access.expression) {
                                    comp_indices.push(idx);
                                } else {
                                    panic!(
                                        "Component array '{}' requires constant indices before dot access. Array indices must be compile-time constants.",
                                        var_name
                                    );
                                }
                            }
                        }

                        let comp_instance_name = if comp_indices.is_empty() {
                            var_name.clone()
                        } else {
                            self.compute_component_array_instance_name(&var_name, &comp_indices)
                        };

                        // Qualify the component name with current context
                        let qualified_comp_name = if let Some(parent) = &self.current_component {
                            format!("{}.{}", parent, comp_instance_name)
                        } else {
                            comp_instance_name.clone()
                        };

                        if self.component_signals.contains_key(&qualified_comp_name) {
                            let dot_access = match &assignee.accesses[dot_pos] {
                                ast::AssigneeAccess::Dot(dot_access) => dot_access,
                                _ => unreachable!(),
                            };
                            let signal_name = &dot_access.inner.value;

                            let has_signal = self
                                .component_signals
                                .get(&qualified_comp_name)
                                .and_then(|signals| signals.get(signal_name))
                                .is_some();
                            if !has_signal {
                                panic!(
                                    "Component signal '{}.{}' not found. Qualified component '{}' does not have signal '{}'.",
                                    var_name, signal_name, qualified_comp_name, signal_name
                                );
                            }

                            let mut signal_indices = Vec::new();
                            if let Some(array_access) = &dot_access.array_access {
                                if let Some(idx) = self.extract_constant_index_expr(&array_access.expression) {
                                    signal_indices.push(idx);
                                } else {
                                    panic!(
                                        "Component signal array '{}.{}' requires constant indices. Array indices must be compile-time constants.",
                                        qualified_comp_name, signal_name
                                    );
                                }
                            }

                            for access in assignee.accesses.iter().skip(dot_pos + 1) {
                                if let ast::AssigneeAccess::Select(array_access) = access {
                                    if let Some(idx) = self.extract_constant_index_expr(&array_access.expression) {
                                        signal_indices.push(idx);
                                    } else {
                                        panic!(
                                            "Component signal array '{}.{}' requires constant indices. Array indices must be compile-time constants.",
                                            qualified_comp_name, signal_name
                                        );
                                    }
                                } else {
                                    panic!(
                                        "Unsupported component signal access pattern for '{}.{}'. Only array indices are supported after dot access.",
                                        qualified_comp_name, signal_name
                                    );
                                }
                            }

                            if let Some(term) = self.resolve_component_signal_access(
                                &qualified_comp_name,
                                signal_name,
                                &signal_indices,
                            ) {
                                return term;
                            }
                        }
                    }

                    // Special case: signal arrays with constant indices use flattened naming
                    // Check this BEFORE creating base variable term
                    if self.is_signal_array(&var_name) {
                        // Support multi-dimensional signal arrays with constant indices
                        let mut indices: Vec<i64> = Vec::new();
                        for access in &assignee.accesses {
                            if let ast::AssigneeAccess::Select(array_access) = access {
                                let idx = self
                                    .extract_constant_value_expr(&array_access.expression)
                                    .or_else(|| self.try_extract_loop_var_value(&array_access.expression))
                                    .or_else(|| {
                                        let index_term = self.expr_to_term(&array_access.expression);
                                        Self::extract_constant_from_term(&index_term)
                                    })
                                    .unwrap_or_else(|| {
                                        panic!(
                                            "Signal array '{}' requires constant indices. Array indices must be compile-time constants.",
                                            var_name
                                        )
                                    });
                                if idx < 0 {
                                    panic!(
                                        "Signal array '{}' index {} is negative. Array indices must be non-negative.",
                                        var_name, idx
                                    );
                                }
                                indices.push(idx);
                            } else {
                                panic!(
                                    "Unsupported signal array access pattern for '{}'. Signal arrays must use constant indices.",
                                    var_name
                                );
                            }
                        }

                        if indices.is_empty() {
                            panic!(
                                "Unsupported signal array access pattern for '{}'. Signal arrays must use constant indices.",
                                var_name
                            );
                        }

                        // Qualify the base name if we're inside a component context
                        let qualified_base = if let Some(comp_name) = &self.current_component {
                            if let Some(signals) = self.component_signals.get(comp_name) {
                                if let Some(qualified_name) = signals.get(&var_name) {
                                    qualified_name.clone()
                                } else {
                                    var_name.clone()
                                }
                            } else {
                                var_name.clone()
                            }
                        } else {
                            var_name.clone()
                        };

                        let flat_idx = if indices.len() == 1 {
                            indices[0]
                        } else {
                            let signal_type = self
                                .vars
                                .get(&var_name)
                                .cloned()
                                .unwrap_or(CircomType::Signal);
                            let dimensions = Self::extract_dimensions_from_type(&signal_type);
                            Self::calculate_flat_index(&indices, &dimensions).unwrap_or_else(|| {
                                panic!(
                                    "Signal array '{}' index out of bounds or dimension mismatch. \
                                     Indices: {:?}, dimensions: {:?}",
                                    var_name, indices, dimensions
                                )
                            })
                        };

                        let flattened_name = self.get_flattened_signal_name(&qualified_base, flat_idx);

                        // Declare the flattened signal if not already declared
                        if !self.vars.contains_key(&flattened_name) {
                            self.vars.insert(flattened_name.clone(), CircomType::Signal);
                            self.signal_names.insert(flattened_name.clone());

                            // Declare in IR
                            self.declare_input_ignore_dup(
                                flattened_name.clone(), &Ty::Field, None,
                            );
                        }

                        // Ensure variable is registered in metadata (in case declare_input wasn't called)
                        let field_sort = Sort::Field(default_field());
                        self.ensure_var_in_metadata(&flattened_name, &field_sort);
                        // Return the flattened signal directly
                        return T::new(
                            Ty::Field,
                            leaf_term(Op::new_var(flattened_name, field_sort))
                        );
                    }

                    // Normal case: non-signal arrays use Select operations
                    // Start with the base variable
                    if let Some(var_type) = self.vars.get(&var_name) {
                        let mut current_term = match var_type {
                            CircomType::Array(_elem_ty, _size) => {
                                // Build proper IR type from CircomType
                                let ir_type = Self::circom_type_to_ty(var_type);
                                let array_sort = ir_type.sort();
                                // Ensure variable is registered in metadata
                                self.ensure_var_in_metadata(&var_name, &array_sort);
                                T::new(
                                    ir_type.clone(),
                                    leaf_term(Op::new_var(var_name.clone(), array_sort))
                                )
                            }
                            _ => {
                                let field_sort = Sort::Field(default_field());
                                // Ensure variable is registered in metadata
                                self.ensure_var_in_metadata(&var_name, &field_sort);
                                T::new(
                                    Ty::Field,
                                    leaf_term(Op::new_var(var_name.clone(), field_sort))
                                )
                            }
                        };

                        // Apply each access (array indexing)
                        for access in &assignee.accesses {
                            if let ast::AssigneeAccess::Select(array_access) = access {
                                let index_term = self.expr_to_term(&array_access.expression);

                                // Array select operation for non-signal arrays
                                if let Ty::Array(_, elem_ty) = &current_term.ty {
                                    current_term = T::new(
                                        *elem_ty.clone(),
                                        term![Op::Select; current_term.term, index_term.term]
                                    );
                                } else {
                                    panic!("Type error: Trying to index non-array type for variable '{}'", var_name);
                                }
                            }
                        }

                        current_term
                    } else {
                        panic!("Variable '{}' not found. Variables must be declared before use.", var_name);
                    }
                } else {
                    // No array access, just return the variable
                    if let Some(_var_type) = self.vars.get(&var_name) {
                        let field_sort = Sort::Field(default_field());
                        // Ensure variable is registered in metadata
                        self.ensure_var_in_metadata(&var_name, &field_sort);
                        T::new(
                            Ty::Field,
                            leaf_term(Op::new_var(var_name, field_sort))
                        )
                    } else {
                        panic!("Variable '{}' not found. Variables must be declared before use.", var_name);
                    }
                }
            },
            ast::AssigneeTarget::Tuple(tuple) => {
                // Tuple assignments like (a, b) <== ... are not yet supported.
                // Only the first element was being processed, silently dropping
                // constraints for the rest. Panic instead of producing wrong results.
                panic!(
                    "Tuple assignment targets are not supported. \
                     Found tuple with {} assignees: ({}).",
                    tuple.assignees.len(),
                    tuple.assignees.iter()
                        .map(|a| a.id.value.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            },
        }
    }

    /// Instantiate a component with given name and template
    pub fn instantiate_component(&mut self, comp_name: &str, template_name: &str, param_values: &[CompileTimeValue], template: &circom_pest_ast::TemplateDefinition<'ast>) {

        // Register the component with its type
        let comp_type = CircomType::Component(template_name.to_string());
        self.vars.insert(comp_name.to_string(), comp_type);

        // Save current context before entering template
        let saved_vars = std::mem::take(&mut self.vars);
        let saved_var_values = std::mem::take(&mut self.var_values);
        let saved_component = self.current_component.clone();

        // Start with inherited variable scope for the new component
        // Important fix: Preserve parent scope var_values (like loop variables) so they're accessible
        // in nested component instantiations. Template parameters will shadow parent variables.
        // This fixes the issue where loop variables are lost during deep template nesting
        self.vars = HashMap::default();
        self.var_values = saved_var_values.clone();  // inherit parent scope variables

        // Set component context with hierarchical qualification for nested/recursive calls
        // This ensures unique names like "parent.child" for components in recursive templates
        let new_component_name = if let Some(parent) = &saved_component {
            format!("{}.{}", parent, comp_name)
        } else {
            comp_name.to_string()
        };
        self.current_component = Some(new_component_name);

        // Set up template parameters (these will shadow parent scope variables with same names)
        self.circom_gen.enter_template();
        for (param, value) in template.params.iter().zip(param_values.iter()) {
            self.var_values.insert(param.value.clone(), value.clone());
        }

        // Extract signal declarations from template and create qualified names
        let mut signals = HashMap::default();

        // CRITICAL: Process component, var, and signal declarations IN ORDER
        // This ensures that:
        // 1. Components declared (e.g., `component pX;`) are registered in vars
        // 2. Vars used in signal array dimensions (e.g., `signal output X[m]`) are available
        // 3. Signals used in var declarations (e.g., `var Xvar = f(..., in, ...)`) are available
        // We process them in the order they appear in the template source.
        for stmt in &template.statements {
            // Process component declarations (for component variables used later)
            if let circom_pest_ast::Statement::Component(comp_stmt) = stmt {
                // Only process declarations without instantiation (component pX;)
                // Instantiations (component pX = Template();) are handled in the main statement pass
                if comp_stmt.value.is_none() {
                    let comp_var_name = comp_stmt.assignee.id.value.clone();

                    if let Some(dims) = self.extract_array_dimensions(&comp_stmt.assignee) {
                        // Component array declaration
                        let array_type = Self::build_circom_array_type(&dims);
                        self.vars.insert(comp_var_name, array_type);
                    } else {
                        // Single component declaration
                        self.vars.insert(comp_var_name, CircomType::Component("unknown".to_string()));
                    }
                }
            }

            // Process var declarations (for signal array dimensions)
            if let circom_pest_ast::Statement::Variable(var) = stmt {
                self.process_variable_statement(var, false);  // declarations_only=false - EVALUATE VALUES
            }

            // Process signal declarations (so they're available for subsequent var declarations)
            if let circom_pest_ast::Statement::Signal(signal_stmt) = stmt {
                if let circom_pest_ast::SignalStatement::SignalDecl(decl) = signal_stmt {
                    for assignee in &decl.assignees {
                        let signal_name = assignee.id.value.clone();

                        // Extract array dimensions if present
                        let array_dims = self.extract_array_dimensions(assignee);

                        // Use the hierarchically qualified component name for signal registration
                        let qualified_comp = self.current_component.as_ref().unwrap();

                        if let Some(dims) = &array_dims {
                            // For array signals, flatten immediately
                            let total_size = dims.iter().product::<usize>();
                            for flat_idx in 0..total_size {
                                let flattened_name = format!("{}.{}_{}", qualified_comp, signal_name, flat_idx);

                                // Register each flattened element
                                self.vars.insert(flattened_name.clone(), CircomType::Signal);
                                self.signal_names.insert(flattened_name.clone());

                                // Declare in IR for input/output signals with correct visibility
                                // Sub-component signals are always private (only main
                                // component outputs are public, handled elsewhere)
                                if decl.signal_type.is_some() {
                                    self.declare_input_ignore_dup(
                                        flattened_name, &Ty::Field, Some(0),
                                    );
                                }
                            }

                            // Store the base name mapping
                            let qualified_base = format!("{}.{}", qualified_comp, signal_name);
                            signals.insert(signal_name.clone(), qualified_base.clone());

                            // Register the qualified base name with its array type
                            let array_type = Self::build_circom_array_type(dims);
                            self.vars.insert(qualified_base.clone(), array_type.clone());

                            // ALSO register the unqualified name immediately (for use in var declarations that follow)
                            self.vars.insert(signal_name.clone(), array_type);
                            // Add to signal_names so is_signal_array() works for template body constraints
                            self.signal_names.insert(signal_name.clone());

                            // Extract tags for array signals in component
                            if let Some(ref tags) = decl.tags {
                                let tag_pairs: Vec<(String, Option<rug::Integer>)> = tags.tags.iter()
                                    .map(|id| (id.value.clone(), None))
                                    .collect();
                                let total_size = dims.iter().product::<usize>();
                                for flat_idx in 0..total_size {
                                    let flattened_name = format!("{}.{}_{}", qualified_comp, signal_name, flat_idx);
                                    self.signal_tags.insert(flattened_name, tag_pairs.clone());
                                }
                            }
                        } else {
                            // Non-array signal - register normally
                            let qualified_name = format!("{}.{}", qualified_comp, signal_name);
                            self.vars.insert(qualified_name.clone(), CircomType::Signal);
                            self.signal_names.insert(qualified_name.clone());
                            signals.insert(signal_name.clone(), qualified_name.clone());

                            // ALSO register the unqualified name immediately
                            self.vars.insert(signal_name.clone(), CircomType::Signal);
                            // Add to signal_names so is_signal_array() works for template body constraints
                            self.signal_names.insert(signal_name.clone());

                            // Declare in IR for input/output signals with correct visibility
                            // Sub-component signals are always private
                            if decl.signal_type.is_some() {
                                self.declare_input_ignore_dup(
                                    qualified_name.clone(), &Ty::Field, Some(0),
                                );
                            }

                            // Extract tags for scalar signals in component
                            if let Some(ref tags) = decl.tags {
                                let tag_pairs: Vec<(String, Option<rug::Integer>)> = tags.tags.iter()
                                    .map(|id| (id.value.clone(), None))
                                    .collect();
                                self.signal_tags.insert(qualified_name, tag_pairs);
                            }
                        }
                    }
                }
            }
        }

        // Store signal mapping for component.signal access
        // Use the hierarchically qualified name (from current_component) to match lookups
        let qualified_comp_name = self.current_component.as_ref().unwrap().clone();
        self.component_signals.insert(qualified_comp_name.clone(), signals.clone());

        // Store signal types for component.signal access
        let mut signal_types = HashMap::default();
        for (template_signal, qualified_name) in &signals {
            if let Some(signal_type) = self.vars.get(qualified_name) {
                signal_types.insert(template_signal.clone(), signal_type.clone());
            } else {
                signal_types.insert(template_signal.clone(), CircomType::Signal);
            }
        }
        self.component_signal_types.insert(qualified_comp_name, signal_types);

        // Add unqualified signal names to vars and signal_names
        for (template_signal, qualified_name) in &signals {
            // Preserve the array type from the qualified name
            if let Some(signal_type) = self.vars.get(qualified_name) {
                self.vars.insert(template_signal.clone(), signal_type.clone());
            } else {
                self.vars.insert(template_signal.clone(), CircomType::Signal);
            }
            // Also add to signal_names for is_signal_array() to work in template body
            self.signal_names.insert(template_signal.clone());
        }

        // Process template statements
        for stmt in &template.statements {
            // NOTE: Variable declarations were processed in first pass, but reassignments
            // (e.g., lout = lout + aux) need to be processed here now that signals are registered.
            // visit_statement will handle them appropriately with declarations_only=false.

            // Skip ONLY signal declarations (not constraint statements!)
            // SignalStatement includes:
            //   - SignalDecl (declarations like "signal input x;") - SKIP THESE
            //   - SignalAssignmentConstraintStatement (constraints like "out <== in * in") - PROCESS THESE
            //   - SignalAssignmentStatement (witness assignments like "out <-- value") - PROCESS THESE
            //   - ConstraintStatement (equality constraints like "a === b") - PROCESS THESE
            if let circom_pest_ast::Statement::Signal(signal_stmt) = stmt {
                if matches!(signal_stmt, circom_pest_ast::SignalStatement::SignalDecl(_)) {
                    continue;
                }
            }

            let mut stmt_clone = stmt.clone();
            self.visit_statement(&mut stmt_clone);
        }

        // Restore context
        self.vars = saved_vars;
        self.var_values = saved_var_values;
        self.current_component = saved_component;
        self.circom_gen.exit_template();
    }

    /// Process a component instantiation statement
    fn process_component_statement(&mut self, comp: &ast::ComponentStatement<'ast>) {
        let comp_name = comp.assignee.id.value.clone();

        // Handle component array declarations without instantiation
        // e.g., component foo[10]; or component bar[n][m];
        if comp.value.is_none() {
            // Extract array dimensions if present
            if let Some(dims) = self.extract_array_dimensions(&comp.assignee) {
                // This is a component array declaration
                let array_type = Self::build_circom_array_type(&dims);
                self.vars.insert(comp_name.clone(), array_type);
                // Note: We don't know the template type yet, so we just mark it as an array
                // The actual instantiation will happen later (e.g., in a loop)
            } else {
                // Single component declaration without instantiation
                self.vars.insert(comp_name.clone(), CircomType::Component("unknown".to_string()));
            }
            return;
        }

        // Handle component instantiation
        if let Some(inst) = &comp.value {
            // Get the template name
            let template_name = &inst.id.value;

            // Look up the template definition
            if let Some((_template_path, template)) = self.circom_gen.find_template(template_name) {

                // Parse template parameter values from instantiation (can be scalars or arrays)
                let param_values: Vec<CompileTimeValue> = inst.args.iter()
                    .map(|expr| {
                        self.extract_parameter_value(expr).unwrap_or_else(|| {
                            panic!(
                                "Cannot evaluate template parameter for '{}'. \
                                 All template parameters must be compile-time constants.",
                                template_name
                            )
                        })
                    })
                    .collect();


                // Check if this is an indexed assignment (e.g., ark[i] = Ark(...))
                let has_array_access = !comp.assignee.accesses.is_empty();

                if has_array_access {
                    // Indexed component instantiation: comp_array[idx] = Template(...)
                    // Extract all indices and evaluate them as compile-time constants
                    if let Some(indices) = self.extract_constant_indices_from_assignee_accesses(&comp.assignee.accesses) {
                        let qualified_name = self.compute_component_array_instance_name(&comp_name, &indices);
                        self.instantiate_component(&qualified_name, template_name, &param_values, template);
                    } else {
                        // Non-constant index - this shouldn't happen in properly unrolled loops
                        panic!(
                            "Component array instantiation with non-constant index: {}[..]. All component instantiations must use compile-time constant indices.",
                            comp_name
                        );
                    }
                } else {
                    // Non-indexed instantiation: component foo = Template(...)
                    self.instantiate_component(&comp_name, template_name, &param_values, template);
                }
            }
        }
    }

    /// Process a variable declaration
    /// If declarations_only is true, skip reassignments to existing variables (for first pass)
    fn process_variable_statement(&mut self, var: &ast::VariableStatement<'ast>, declarations_only: bool) {
        // Process each declaration in the statement (supports comma-separated variables)
        for decl in &var.declarations {
            self.process_single_variable_declaration(decl, declarations_only);
        }
    }

    /// Process a single variable declaration
    /// If declarations_only is true, skip reassignments to existing variables (for first pass)
    fn process_single_variable_declaration(&mut self, decl: &ast::VariableDeclaration<'ast>, declarations_only: bool) {
        let var_name = decl.assignee.id.value.clone();

        // Extract array dimensions if present
        let circom_type = if let Some(dims) = self.extract_array_dimensions(&decl.assignee) {
            Self::build_circom_array_type(&dims)
        } else {
            CircomType::Field
        };



        // Check if variable already exists (to distinguish declaration from indexed assignment)
        // In function scope, allow shadowing of outer variables/signals by treating only
        // names declared in the current function scope as "existing".
        let in_function_scope = !self.function_scopes.is_empty();
        let local_declared = in_function_scope
            && self
                .function_scopes
                .last()
                .map(|scope| scope.contains(&var_name))
                .unwrap_or(false);
        let is_existing_var = if in_function_scope {
            local_declared
        } else {
            self.vars.contains_key(&var_name)
        };

        // Check if this is an indexed assignment to existing array (e.g., arr[0] = 1)
        // vs a new array declaration (e.g., var arr[10] = in)
        let has_array_access = !decl.assignee.accesses.is_empty();
        let is_indexed_assignment = has_array_access && is_existing_var;

        // Check if this is a compound assignment (not Assign or None)
        let is_compound = match &decl.op {
            Some(ast::VarAssignmentOp::Assign(_)) | None => false,
            Some(_) => true,
        };

        // Check if this is a tag value assignment: out.maxbit = n;
        // A dot access on a signal with an assignment -> tag value setting
        if has_array_access && is_existing_var {
            let has_dot_access = decl.assignee.accesses.iter().any(|a| {
                matches!(a, ast::AssigneeAccess::Dot(_))
            });
            if has_dot_access && self.signal_names.contains(&var_name) {
                if let Some(ast::AssigneeAccess::Dot(dot)) = decl.assignee.accesses.first() {
                    let tag_name = dot.inner.value.clone();
                    if let Some(expr) = &decl.value {
                        if let ast::TernaryOrExpression::Expression(rhs_expr) = expr {
                            if let Some(val) = self.extract_constant_value_expr_big(rhs_expr) {
                                self.set_tag_value(&var_name, &tag_name, val.clone());
                                if let Some(ref comp) = self.current_component {
                                    let qualified = format!("{}.{}", comp, var_name);
                                    self.set_tag_value(&qualified, &tag_name, val);
                                }
                                return;
                            }
                        }
                    }
                    panic!(
                        "Tag value assignment `{}.{} = ...` could not resolve RHS to compile-time value",
                        var_name, tag_name
                    );
                }
            }
        }

        // During first pass (declarations_only=true), skip reassignments to existing variables
        // These will reference signals that haven't been registered yet
        if declarations_only && is_existing_var && !is_indexed_assignment && !is_compound {
            return;
        }

        // Register the variable type EARLY (before evaluating RHS) to ensure it's available
        // for self-referential expressions and array accesses within the same template
        // Don't overwrite existing variables (e.g., function parameters)
        if !is_indexed_assignment && !is_compound && !is_existing_var {
            self.vars.insert(var_name.clone(), circom_type.clone());
            if in_function_scope {
                if let Some(scope) = self.function_scopes.last_mut() {
                    scope.insert(var_name.clone());
                }
            }
        } else if !is_indexed_assignment && !is_compound && is_existing_var {
        }

        // Process assignment or compound assignment
        if let Some(expr) = &decl.value {
            // Special case: Check for component instantiation BEFORE evaluating RHS
            // This handles both: h0 = K(8); and hashers[i] = Hash();
            if let ast::TernaryOrExpression::Expression(rhs_expr) = expr {
                if let ast::Expression::Postfix(postfix) = rhs_expr {
                    if let ast::Expression::Identifier(template_id) = postfix.base.as_ref() {
                        let template_name = &template_id.value;

                        // Check if this is a template with a call access (component instantiation)
                        if let Some((_template_path, template_def)) = self.circom_gen.find_template(template_name) {
                            if let Some(ast::Access::CallAccess(call)) = postfix.access.first() {
                                // Parse template arguments (can be scalars or arrays)
                                let arg_values: Vec<CompileTimeValue> = call.args.iter()
                                    .map(|arg| {
                                        self.extract_parameter_value(arg).unwrap_or_else(|| {
                                            panic!(
                                                "Cannot evaluate template parameter for '{}'. \
                                                 All template parameters must be compile-time constants.",
                                                template_name
                                            )
                                        })
                                    })
                                    .collect();

                                // Check if this is array indexed assignment: hashers[i] = Hash();
                                if is_indexed_assignment && has_array_access {
                                    // Component array instantiation (supports multi-dimensional arrays)
                                    if let Some(indices) = self.extract_constant_indices_from_assignee_accesses(&decl.assignee.accesses) {
                                        let comp_name = self.compute_component_array_instance_name(&var_name, &indices);
                                        self.instantiate_component(&comp_name, template_name, &arg_values, template_def);
                                        return; // Done
                                    }
                                    panic!(
                                        "Component array instantiation with non-constant index: {}[..]. All component instantiations must use compile-time constant indices.",
                                        var_name
                                    );
                                } else if is_existing_var && !is_compound {
                                    // Non-array component assignment: h0 = K(8);
                                    // This is an assignment to an existing component variable
                                    self.instantiate_component(&var_name, template_name, &arg_values, template_def);
                                    return; // Done
                                } else {
                                }
                            }
                        }
                    }
                }
            }

            // Not a component instantiation - evaluate RHS normally
            // Try compile-time evaluation first (for function inlining contexts)
            // This allows variables computed from function parameters to be stored in var_values
            let compile_time_val = if let ast::TernaryOrExpression::Expression(e) = expr {
                self.extract_constant_value_expr_big(e)
            } else {
                None
            };

            // If we got a compile-time scalar value for a non-array variable, store it early
            // This makes it available for subsequent function calls that need compile-time parameters
            // 
            // Limitation: eval_function_constant only handles scalar returns (Option<Integer>),
            // not arrays. So we must skip storing the value when:
            // 1. Variable is or was declared as an array type
            // 2. This is an indexed assignment (arr[i] = value)
            // 3. This is a compound assignment (n *= 2 should compute n*2, not store 2)
            if let Some(val) = compile_time_val {
                let is_array_var = matches!(circom_type, CircomType::Array(_, _))
                    || self.var_values.get(&var_name).map_or(false, |v| {
                        matches!(v, CompileTimeValue::Array1D(_) | CompileTimeValue::Array2D(_) | CompileTimeValue::ArrayND(_))
                    });
                
                if !is_indexed_assignment && !is_compound && !is_array_var {
                    self.var_values.insert(var_name.clone(), CompileTimeValue::scalar_big(val));
                }
            }

            // Always generate IR term for constraint generation (even if we did compile-time eval)
            let rhs_term = self.expr_to_term_from_ternary(expr);

            // For indexed assignments to existing arrays (arr[i] = value), handle compile-time array updates
            if is_indexed_assignment {
                // First, evaluate all indices (this may borrow self)
                let mut indices = Vec::new();
                for access in &decl.assignee.accesses {
                    if let ast::AssigneeAccess::Select(array_access) = access {
                        let index_term = self.expr_to_term(&array_access.expression);
                        if let Some(const_idx) = Self::extract_constant_from_term(&index_term) {
                            if const_idx < 0 {
                                panic!(
                                    "Negative array index: {} for variable '{}'. Array indices must be non-negative.",
                                    const_idx, var_name
                                );
                            }
                            indices.push(const_idx as usize);
                        } else {
                            // Non-constant index - can't do compile-time evaluation
                            return;
                        }
                    }
                }

                // Now check if this is a var array and update it
                if let Some(array_val) = self.var_values.get_mut(&var_name) {
                    if let Some(rhs_val) = CompileTimeValue::try_from_term(&rhs_term) {
                        if Self::update_array_value(array_val, &indices, rhs_val) {
                            return;
                        }
                    }
                }
                // If we get here, it's either a signal array or non-constant - skip
                return;
            }


            if is_compound {
                // Compound assignment: variable must already exist
                if let Some(_var_type) = self.vars.get(&var_name).cloned() {
                    let current_val_opt = self.var_values.get(&var_name).cloned();
                    let rhs_val_opt = CompileTimeValue::try_from_term(&rhs_term);


                    // Try compile-time evaluation for vars
                    if let (Some(current_val), Some(rhs_val)) = (current_val_opt, rhs_val_opt) {
                        // Check if both are concrete (not expressions)
                        if !current_val.is_concrete() {
                            return;
                        }
                        if !rhs_val.is_concrete() {
                            return;
                        }

                        // Both are compile-time values - do compile-time arithmetic
                        let op_str = match decl.op.as_ref().unwrap() {
                            ast::VarAssignmentOp::AddAssign(_) => "+",
                            ast::VarAssignmentOp::SubAssign(_) => "-",
                            ast::VarAssignmentOp::MulAssign(_) => "*",
                            ast::VarAssignmentOp::DivAssign(_) => "/",
                            ast::VarAssignmentOp::ModAssign(_) => "%",
                            ast::VarAssignmentOp::BitAndAssign(_) => "&",
                            ast::VarAssignmentOp::BitOrAssign(_) => "|",
                            ast::VarAssignmentOp::BitXorAssign(_) => "^",
                            ast::VarAssignmentOp::LeftShiftAssign(_) => "<<",
                            ast::VarAssignmentOp::RightShiftAssign(_) => ">>",
                            ast::VarAssignmentOp::PowAssign(_) => {
                                // Power needs special handling
                                if let (Some(base), Some(exp)) = (current_val.as_scalar(), rhs_val.as_scalar()) {
                                    if exp >= 0 && exp < 64 {
                                        let result = base.pow(exp as u32);
                                        self.var_values.insert(var_name, CompileTimeValue::scalar(result));
                                        return;
                                    }
                                }
                                panic!("Power operation failed in compile-time evaluation");
                            }
                            ast::VarAssignmentOp::BitNotAssign(_) => {
                                // Bitwise NOT: just NOT the current value
                                if let Some(val) = current_val.as_scalar() {
                                    self.var_values.insert(var_name, CompileTimeValue::scalar(!val));
                                    return;
                                }
                                panic!("Bitwise NOT failed in compile-time evaluation");
                            }
                            ast::VarAssignmentOp::Assign(_) => unreachable!(),
                        };

                        if let Some(result) = current_val.apply_op(op_str, &rhs_val) {
                            self.var_values.insert(var_name.clone(), result.clone());
                        } else {
                            eprintln!("ERROR: Compile-time operation {} failed for variable '{}'", op_str, var_name);
                            eprintln!("       Current value: {:?}", current_val);
                            eprintln!("       RHS value: {:?}", rhs_val);
                            panic!("Compile-time operation {} failed", op_str);
                        }
                    } else {
                        // Fall back to IR term operations for signals
                        let current_term = if let Some(stored_value) = self.var_values.get(&var_name) {
                            stored_value.to_term()
                        } else {
                            let field_sort = Sort::Field(cfg().field().clone());
                            // Ensure variable is registered in metadata
                            self.ensure_var_in_metadata(&var_name, &field_sort);
                            T::new(
                                Ty::Field,
                                leaf_term(Op::new_var(var_name.clone(), field_sort))
                            )
                        };

                        let new_term = match decl.op.as_ref().unwrap() {
                            ast::VarAssignmentOp::AddAssign(_) => add(current_term, rhs_term).unwrap(),
                            ast::VarAssignmentOp::SubAssign(_) => sub(current_term, rhs_term).unwrap(),
                            ast::VarAssignmentOp::MulAssign(_) => mul(current_term, rhs_term).unwrap(),
                            ast::VarAssignmentOp::DivAssign(_) => div(current_term, rhs_term).unwrap(),
                            ast::VarAssignmentOp::ModAssign(_) => rem(current_term, rhs_term).unwrap(),
                            ast::VarAssignmentOp::PowAssign(_) => pow(current_term, rhs_term).unwrap(),
                            ast::VarAssignmentOp::BitAndAssign(_) => bit_and(current_term, rhs_term).unwrap(),
                            ast::VarAssignmentOp::BitOrAssign(_) => bit_or(current_term, rhs_term).unwrap(),
                            ast::VarAssignmentOp::BitXorAssign(_) => bit_xor(current_term, rhs_term).unwrap(),
                            ast::VarAssignmentOp::LeftShiftAssign(_) => left_shift(current_term, rhs_term).unwrap(),
                            ast::VarAssignmentOp::RightShiftAssign(_) => right_shift(current_term, rhs_term).unwrap(),
                            ast::VarAssignmentOp::BitNotAssign(_) => bit_not(current_term).unwrap(),
                            ast::VarAssignmentOp::Assign(_) => unreachable!(),
                        };

                        // Store as compile-time value if it's a constant
                        if let Some(const_val) = CompileTimeValue::try_from_term(&new_term) {
                            self.var_values.insert(var_name, const_val);
                        }
                        // If not constant, it's a signal expression - don't store in var_values
                    }
                } else {
                    panic!("Compound assignment to undeclared variable: {}", var_name);
                }
            } else {
                // Regular declaration/assignment - register the variable type
                // 
                // Limitation: Type inference from RHS doesn't recognize array-returning functions
                // (because constant evaluation only handles scalars). Preserve array types across
                // reassignments to prevent losing type information: `var dbl[2] = ...; dbl = pointAdd(...)`
                if is_existing_var {
                    if let Some(existing_type) = self.vars.get(&var_name) {
                        if matches!(existing_type, CircomType::Array(_, _)) && matches!(circom_type, CircomType::Field) {
                            // Preserve existing array type when RHS type inference returns Field
                            // This happens with array-returning functions that can't be const-evaluated
                        } else {
                            self.vars.insert(var_name.clone(), circom_type.clone());
                        }
                    } else {
                        self.vars.insert(var_name.clone(), circom_type.clone());
                    }
                } else {
                    self.vars.insert(var_name.clone(), circom_type.clone());
                }

                // Special handling for array-to-array initialization (e.g., var arr[n] = other_arr)
                // Check if RHS is an identifier that refers to an array
                if let Some(expr_ternary) = &decl.value {
                    if let ast::TernaryOrExpression::Expression(expr) = expr_ternary {
                        if let ast::Expression::Identifier(id) = expr {
                            let rhs_name = &id.value;
                            // Check if RHS is an array variable
                            if let Some(rhs_val) = self.var_values.get(rhs_name).cloned() {
                                // Copy the array value to the new variable
                                self.var_values.insert(var_name.clone(), rhs_val);
                                return; // Done with this declaration
                            }
                            // If RHS is a signal array or not in var_values, initialize with zeros
                            // (we can't use signal values at compile time)
                            if let Some(array_val) = Self::init_array_value(&circom_type) {
                                self.var_values.insert(var_name.clone(), array_val);
                                return; // Done with this declaration
                            }
                        }
                    }
                }

                // For array variables, try to evaluate RHS as array literal first
                // Only fall back to zero initialization if RHS can't be evaluated
                let array_val_from_rhs = CompileTimeValue::try_from_term(&rhs_term);
                let is_array_from_rhs = array_val_from_rhs.as_ref()
                    .map(|v| matches!(v,
                        CompileTimeValue::Array1D(_) |
                        CompileTimeValue::Array2D(_) |
                        CompileTimeValue::ArrayND(_) |
                        CompileTimeValue::ExprArray1D(_)
                    ))
                    .unwrap_or(false);

                if is_array_from_rhs {
                    // Use the array literal from RHS
                    let val = array_val_from_rhs.unwrap();
                    self.var_values.insert(var_name, val);
                } else if matches!(rhs_term.type_(), Ty::Array(_, _)) {
                    // RHS is an array expression (non-literal). Preserve as expression instead of zeroing.
                    self.var_values.insert(var_name, CompileTimeValue::Expression(rhs_term.clone()));
                } else if let Some(array_val) = Self::init_array_value(&circom_type) {
                    // Fall back to zero initialization if RHS is not an array literal
                    // This handles cases like `var C[n] = POSEIDON_C(t)` where the function can't be evaluated
                    self.var_values.insert(var_name, array_val);
                } else {
                    // For non-array variables, store the compile-time constant value
                    if let Some(const_val) = CompileTimeValue::try_from_term(&rhs_term) {
                        // Check if we're overwriting an existing array with a scalar (BUG!)
                        if let Some(existing) = self.var_values.get(&var_name) {
                            match existing {
                                CompileTimeValue::Array1D(_) | CompileTimeValue::Array2D(_) | CompileTimeValue::ArrayND(_) => {
                                    return; // Don't overwrite array with scalar
                                }
                                _ => {}
                            }
                        }

                        // VALIDATION: Check for non-concrete values being stored
                        // This catches cases where try_from_term returns an Expression
                        if const_val.is_expression() {
                        }

                        self.var_values.insert(var_name, const_val);
                    } else {
                    }
                }
                // If not constant and not an array, it's a signal expression - don't store in var_values
            }
        } else {
            // Declaration without assignment (e.g., var x; or var arr[3];)
            self.vars.insert(var_name.clone(), circom_type.clone());

            // For array declarations, initialize with zeros
            if let Some(array_val) = Self::init_array_value(&circom_type) {
                self.var_values.insert(var_name, array_val);
            }
        }
    }

    /// Helper to convert TernaryOrExpression to term
    fn expr_to_term_from_ternary(&mut self, expr: &ast::TernaryOrExpression<'ast>) -> T {
        match expr {
            ast::TernaryOrExpression::Expression(e) => {
                self.expr_to_term(e)
            }
            ast::TernaryOrExpression::Ternary(ternary) => {
                // Implement ternary expression: condition ? then : else
                // If condition is compile-time constant, only evaluate the selected branch.
                if let Some(cond_val) = self.extract_constant_value_expr(&ternary.condition) {
                    if cond_val != 0 {
                        return self.expr_to_term(&ternary.consequence);
                    }
                    return self.expr_to_term(&ternary.alternative);
                }

                let cond_term = self.expr_to_term(&ternary.condition);
                let then_term = self.expr_to_term(&ternary.consequence);
                let else_term = self.expr_to_term(&ternary.alternative);

                // Use cond() which handles field-to-bool conversion
                crate::front::circom::term::cond(cond_term, then_term, else_term)
                    .expect("Failed to create ternary expression")
            }
        }
    }
    
    /// Get the constraints generated
    pub fn get_constraints(&self) -> &[Term] {
        &self.constraints
    }

    /// Get the output signal names
    pub fn get_output_signals(&self) -> &[String] {
        &self.output_signals
    }

    /// Get the signal tags map
    pub fn get_signal_tags(&self) -> &HashMap<String, Vec<(String, Option<rug::Integer>)>> {
        &self.signal_tags
    }

    /// Update the value of an existing tag on a signal.
    /// Searches for the tag by name across exact match and array-flattened names.
    fn set_tag_value(&mut self, signal_name: &str, tag_name: &str, value: rug::Integer) {
        // Update tag value for exact signal name
        if let Some(tags) = self.signal_tags.get_mut(signal_name) {
            for (name, val) in tags.iter_mut() {
                if name == tag_name {
                    *val = Some(value.clone());
                    return;
                }
            }
        }
        // Also check array-flattened names (signal_0, signal_1, ...)
        let prefix = format!("{}_", signal_name);
        let matching_keys: Vec<String> = self.signal_tags.keys()
            .filter(|k| k.starts_with(&prefix))
            .cloned()
            .collect();
        for key in matching_keys {
            if let Some(tags) = self.signal_tags.get_mut(&key) {
                for (name, val) in tags.iter_mut() {
                    if name == tag_name {
                        *val = Some(value.clone());
                    }
                }
            }
        }
    }

    /// Like set_tag_value, but only sets the value if no value exists yet or the new
    /// value is smaller (tighter bound). Used for assert-derived bounds that should not
    /// override explicit tag assignments.
    fn set_tag_value_upper_bound(&mut self, signal_name: &str, tag_name: &str, value: rug::Integer) {
        // Helper: check if any tag entry for a signal already has a tighter bound
        let has_tighter = |tags: &[(String, Option<rug::Integer>)]| -> bool {
            tags.iter().any(|(name, val)| {
                name == tag_name && val.as_ref().map_or(false, |v| *v <= value)
            })
        };

        // Check exact signal name
        if let Some(tags) = self.signal_tags.get(signal_name) {
            if has_tighter(tags) {
                return;
            }
        }

        // Also check array-flattened names (signal_0, signal_1, ...)
        let prefix = format!("{}_", signal_name);
        let any_tighter = self.signal_tags.iter().any(|(key, tags)| {
            key.starts_with(&prefix) && has_tighter(tags)
        });
        if any_tighter {
            return;
        }

        self.set_tag_value(signal_name, tag_name, value);
    }

    /// Try to extract tag value from assert expressions like:
    ///   assert(signal.tagname <= value)
    ///   assert(value >= signal.tagname)
    fn try_extract_tag_value_from_assert(&mut self, expr: &ast::Expression<'ast>) {
        use ast::Expression;

        if let Expression::Binary(bin) = expr {
            let is_lte = matches!(bin.op, ast::OpBinary::LteOp);
            let is_gte = matches!(bin.op, ast::OpBinary::GteOp);

            if !is_lte && !is_gte {
                return;
            }

            // Pattern: signal.tag <= value
            if let Expression::Postfix(postfix) = &*bin.left {
                if let Expression::Identifier(base_id) = postfix.base.as_ref() {
                    let signal_name = base_id.value.clone();
                    if let Some(ast::Access::DotAccess(dot)) = postfix.access.first() {
                        let tag_name = dot.inner.value.clone();
                        let is_direct = self.signal_names.contains(&signal_name);
                        let array_prefix = format!("{}_", signal_name);
                        let is_array = !is_direct && self.signal_names.iter().any(|s| s.starts_with(&array_prefix));
                        if (is_direct || is_array) && is_lte {
                            if let Some(val) = self.extract_constant_value_expr_big(&*bin.right) {
                                if is_direct {
                                    self.set_tag_value_upper_bound(&signal_name, &tag_name, val.clone());
                                    if let Some(ref comp) = self.current_component {
                                        let qualified = format!("{}.{}", comp, signal_name);
                                        self.set_tag_value_upper_bound(&qualified, &tag_name, val);
                                    }
                                } else {
                                    for sig in self.signal_names.clone().iter() {
                                        if sig.starts_with(&array_prefix) {
                                            self.set_tag_value_upper_bound(sig, &tag_name, val.clone());
                                            if let Some(ref comp) = self.current_component {
                                                let qualified = format!("{}.{}", comp, sig);
                                                self.set_tag_value_upper_bound(&qualified, &tag_name, val.clone());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Reverse pattern: value >= signal.tag
            if let Expression::Postfix(postfix) = &*bin.right {
                if let Expression::Identifier(base_id) = postfix.base.as_ref() {
                    let signal_name = base_id.value.clone();
                    if let Some(ast::Access::DotAccess(dot)) = postfix.access.first() {
                        let tag_name = dot.inner.value.clone();
                        let is_direct = self.signal_names.contains(&signal_name);
                        let array_prefix = format!("{}_", signal_name);
                        let is_array = !is_direct && self.signal_names.iter().any(|s| s.starts_with(&array_prefix));
                        if (is_direct || is_array) && is_gte {
                            if let Some(val) = self.extract_constant_value_expr_big(&*bin.left) {
                                if is_direct {
                                    self.set_tag_value_upper_bound(&signal_name, &tag_name, val.clone());
                                    if let Some(ref comp) = self.current_component {
                                        let qualified = format!("{}.{}", comp, signal_name);
                                        self.set_tag_value_upper_bound(&qualified, &tag_name, val);
                                    }
                                } else {
                                    for sig in self.signal_names.clone().iter() {
                                        if sig.starts_with(&array_prefix) {
                                            self.set_tag_value_upper_bound(sig, &tag_name, val.clone());
                                            if let Some(ref comp) = self.current_component {
                                                let qualified = format!("{}.{}", comp, sig);
                                                self.set_tag_value_upper_bound(&qualified, &tag_name, val.clone());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Add a constraint
    fn _add_constraint(&mut self, constraint: Term) {
        self.constraints.push(constraint);
    }

    /// Set template parameter values from component instantiation
    /// This should be called before visit_template for the main component
    pub fn set_template_params(&mut self, template: &ast::TemplateDefinition<'ast>, param_values: &[CompileTimeValue]) {
        // Store template parameters as compile-time values (scalars or arrays)
        for (param, value) in template.params.iter().zip(param_values.iter()) {
            let param_name = param.value.clone();
            self.var_values.insert(param_name, value.clone());
        }
    }

    /// Set the current component context
    pub fn set_current_component(&mut self, component: Option<String>) {
        self.current_component = component;
    }
}

impl<'ast, 'ret: 'ast> CircomVisitorMut<'ast> for CircomStatementWalker<'ast, 'ret> {
    fn visit_file(&mut self, file: &mut ast::File<'ast>) {
        // Process each declaration in the file
        for decl in &mut file.declarations {
            self.visit_symbol_declaration(decl);
        }
    }
    
    fn visit_symbol_declaration(&mut self, decl: &mut ast::SymbolDeclaration<'ast>) {
        match decl {
            ast::SymbolDeclaration::Template(template) => self.visit_template(template),
            ast::SymbolDeclaration::Function(function) => self.visit_function(function),
            ast::SymbolDeclaration::MainComponent(main) => {
                // Process the main component
                let template_name = &main.component_instantiation.id.value;

                // Store public signals
                for signal in &main.public_signals {
                    self.public_signals.insert(signal.value.clone());
                }

                // Look up the template definition
                if let Some((_template_path, template)) = self.circom_gen.find_template(template_name) {
                    // Parse template arguments
                    let param_values: Vec<CompileTimeValue> = main.component_instantiation.args.iter()
                        .filter_map(|expr| self.extract_parameter_value(expr))
                        .collect();

                    // Instantiate the main component
                    self.instantiate_component("main", template_name, &param_values, template);
                } else {
                    panic!("Main component template '{}' not found", template_name);
                }
            },
            ast::SymbolDeclaration::Include(_) => {
                // Includes are handled separately by CircomGen
            },
        }
    }

    fn visit_template(&mut self, template: &mut ast::TemplateDefinition<'ast>) {

        // Check if this is template definition processing (params not set) vs instantiation
        // During definition, template parameters won't be in var_values
        // IMPORTANT: For zero-parameter templates, we can't distinguish definition from instantiation
        // based on parameters, so we check if we're currently inside a component context.
        // If current_component is None, we're at the top level processing template definitions.
        let is_definition_mode = (!template.params.is_empty() &&
                                  !template.params.iter().any(|p| self.var_values.contains_key(&p.value))) ||
                                 (template.params.is_empty() && self.current_component.is_none());

        if is_definition_mode {
            // Don't process template bodies during definition - only during instantiation
            // Template instantiation happens in instantiate_component which sets parameters first
            return;
        }

        // Template parameters are compile-time constants stored in var_values.
        // They should NOT be registered in vars because:
        // 1. They can be arrays, not just Fields
        // 2. Their types are determined by the calling context (param_values), not the template definition
        // 3. Registering them as Field would cause "Cannot index field using field" errors for array parameters

        // Only register scalar template parameters in vars (for backward compatibility)
        for param in &template.params {

            // Only register if:
            // 1. Not already in vars (avoids overwriting outer scope)
            // 2. The parameter value is a scalar (not an array)
            if !self.vars.contains_key(&param.value) {
                if let Some(value) = self.var_values.get(&param.value) {
                    match value {
                        CompileTimeValue::Scalar(_) | CompileTimeValue::Expression(_) => {
                            // Register scalars and expressions as Field type
                            self.vars.insert(param.value.clone(), CircomType::Field);
                        }
                        // Arrays should not be registered in vars - they're accessed via var_values
                        CompileTimeValue::Array1D(_) | CompileTimeValue::Array2D(_) | CompileTimeValue::ArrayND(_) |
                        CompileTimeValue::ExprArray1D(_) => {
                        }
                    }
                } else {
                    // Parameter value not yet set (might be set later during instantiation)
                    // Register as Field for now (backward compatibility)
                    self.vars.insert(param.value.clone(), CircomType::Field);
                }
            }
        }

        // Two-pass processing to handle var declarations that are used in signal array dimensions:

        // PASS 1a: Process component declarations (without instantiation) to register component names
        // This ensures that later variable assignments like `h0 = K(8);` can find the component
        for stmt in &template.statements {
            if let ast::Statement::Component(comp) = stmt {
                if comp.value.is_none() {
                    // Only process declarations without instantiation here
                    self.process_component_statement(comp);
                }
            }
        }

        // PASS 1b: Process var DECLARATIONS (with full evaluation) to populate var_values
        // This ensures that expressions like `signal output out[nout]` can resolve `nout`
        // even when nout is computed from a function call like `var nout = nbits(...)`
        // We use declarations_only=false to ensure var values are evaluated and stored.
        for stmt in &template.statements {
            if let ast::Statement::Variable(var) = stmt {
                self.process_variable_statement(var, false);  // declarations_only=false - EVALUATE VALUES
            }
        }

        // PASS 2: Process all statements including reassignments and component instantiations
        // Variable declarations were handled in PASS 1, but reassignments and component
        // instantiation assignments (e.g., h0 = K(8);) need to be processed here
        for stmt in &mut template.statements {
            match stmt {
                ast::Statement::Variable(var) => {
                    // Process with full mode (declarations_only=false) to handle reassignments
                    // and component instantiation assignments
                    self.process_variable_statement(var, false);
                }
                _ => {
                    self.visit_statement(stmt);
                }
            }
        }
    }
    
    fn visit_function(&mut self, function: &mut ast::FunctionDefinition<'ast>) {
        // Process function parameters
        for param in &function.params {
            self.vars.insert(param.value.clone(), CircomType::Field);
        }
        
        // Visit function statements
        for stmt in &mut function.statements {
            self.visit_statement(stmt);
        }
    }

    fn visit_statement(&mut self, stmt: &mut ast::Statement<'ast>) {
        match stmt {
            ast::Statement::Signal(signal) => {
                match signal {
                    ast::SignalStatement::SignalDecl(decl) => {
                        // Register each signal with appropriate type
                        for assignee in &decl.assignees {
                            let signal_name = assignee.id.value.clone();

                            // Extract array dimensions if present
                            let array_dims = self.extract_array_dimensions(assignee);

                            if let Some(ref dims) = array_dims {
                                // Array signal - register the array type
                                let circom_type = Self::build_circom_array_type(dims);
                                self.vars.insert(signal_name.clone(), circom_type);

                                // Track that this is a signal (not var) for array flattening
                                self.signal_names.insert(signal_name.clone());

                                // For input/output arrays, declare each element separately
                                // This flattens the array into individual signals
                                if decl.signal_type.is_some() {
                                    // Determine party based on whether signal is in public list
                                    // Public signals: party = None (verifier knows value)
                                    // Private signals: party = Some(0) (only prover knows value)
                                    let is_output = matches!(
                                        &decl.signal_type,
                                        Some(ast::SignalType::Output(_))
                                    );
                                    let is_public =
                                        is_output || self.public_signals.contains(&signal_name);
                                    let party = if is_public { None } else { Some(0) };

                                    // Declare each array element as a separate signal
                                    let total_elements = dims.iter().product();
                                    for i in 0..total_elements {
                                        let element_name = format!("{}_{}", signal_name, i);
                                        self.declare_input_ignore_dup(
                                            element_name.clone(), &Ty::Field, party,
                                        );
                                    }

                                    // Track output signals
                                    if let Some(ast::SignalType::Output(_)) = &decl.signal_type {
                                        self.output_signals.push(signal_name.clone());
                                    }
                                }

                                // Extract tags for array signals
                                if let Some(ref tags) = decl.tags {
                                    let tag_pairs: Vec<(String, Option<rug::Integer>)> = tags.tags.iter()
                                        .map(|id| (id.value.clone(), None))
                                        .collect();
                                    let total_elements: usize = dims.iter().product();
                                    for i in 0..total_elements {
                                        let element_name = format!("{}_{}", signal_name, i);
                                        self.signal_tags.insert(element_name, tag_pairs.clone());
                                    }
                                }
                                // Intermediate array signals - elements created on assignment
                            } else {
                                // Non-array signal OR array with unresolved dimensions
                                // Register as simple signal for now (will be properly typed during instantiation)
                                if !assignee.accesses.is_empty() {
                                }
                                self.vars.insert(signal_name.clone(), CircomType::Signal);

                                // Track that this is a signal (not var)
                                self.signal_names.insert(signal_name.clone());

                                if decl.signal_type.is_some() {
                                    // Determine party based on whether signal is in public list
                                    // Public signals: party = None (verifier knows value)
                                    // Private signals: party = Some(0) (only prover knows value)
                                    let is_output = matches!(
                                        &decl.signal_type,
                                        Some(ast::SignalType::Output(_))
                                    );
                                    let is_public =
                                        is_output || self.public_signals.contains(&signal_name);
                                    let party = if is_public { None } else { Some(0) };

                                    self.declare_input_ignore_dup(
                                        signal_name.clone(), &Ty::Field, party,
                                    );

                                    if let Some(ast::SignalType::Output(_)) = &decl.signal_type {
                                        self.output_signals.push(signal_name.clone());
                                    }
                                }

                                // Extract tags for scalar signals
                                if let Some(ref tags) = decl.tags {
                                    let tag_pairs: Vec<(String, Option<rug::Integer>)> = tags.tags.iter()
                                        .map(|id| (id.value.clone(), None))
                                        .collect();
                                    self.signal_tags.insert(signal_name, tag_pairs);
                                }
                            }
                        }
                    },
                    ast::SignalStatement::SignalAssignmentConstraintStatement(constraint) => {
                        // Handle <== and ==> constraint assignments
                        match constraint {
                            ast::SignalAssignmentConstraintStatement::LeftArrow(left) => {
                                // target <== value
                                // Per Circom docs, this is equivalent to:
                                //   target <-- value  (witness generation: assign value)
                                //   target === value  (constraint: prove equality)

                                // If this is an intermediate signal (not input/output), declare it now
                                if let ast::AssigneeTarget::Single(assignee) = &left.target {
                                    let target_name = assignee.id.value.clone();
                                    // Check if it's a signal and not already declared as input/output
                                    if self.signal_names.contains(&target_name) &&
                                       !self.public_signals.contains(&target_name) &&
                                       !self.output_signals.contains(&target_name) {
                                        // Declare as intermediate signal (private witness variable)
                                        self.declare_input_ignore_dup(
                                            target_name.clone(), &Ty::Field, Some(0),
                                        );
                                    }
                                }

                                let value_term = self.expr_to_term_from_ternary(&left.value);
                                let target_term = self.assignee_target_to_term(&left.target);

                                // 1. Add witness computation to precomputes
                                // This tells the witness generator how to compute the target signal
                                if let ast::AssigneeTarget::Single(assignee) = &left.target {
                                    if let Some(actual_name) = self.resolve_signal_precompute_name(assignee) {
                                        self.circom_gen.circ.borrow().cir_ctx().cs.borrow_mut()
                                            .precomputes.add_output(actual_name, value_term.term.clone());
                                    }
                                }

                                // 2. Add constraint to prove equality
                                let constraint_term = term(Op::Eq, vec![target_term.term, value_term.term]);
                                self.circom_gen.assert_constraint(constraint_term);
                            },
                            ast::SignalAssignmentConstraintStatement::RightArrow(right) => {
                                // value ==> target
                                // Same as target <== value

                                // If this is an intermediate signal (not input/output), declare it now
                                if let ast::AssigneeTarget::Single(assignee) = &right.target {
                                    let target_name = assignee.id.value.clone();
                                    // Check if it's a signal and not already declared as input/output
                                    if self.signal_names.contains(&target_name) &&
                                       !self.public_signals.contains(&target_name) &&
                                       !self.output_signals.contains(&target_name) {
                                        // Declare as intermediate signal (private witness variable)
                                        self.declare_input_ignore_dup(
                                            target_name.clone(), &Ty::Field, Some(0),
                                        );
                                    }
                                }

                                let value_term = self.expr_to_term_from_ternary(&right.value);
                                let target_term = self.assignee_target_to_term(&right.target);

                                // 1. Add witness computation
                                if let ast::AssigneeTarget::Single(assignee) = &right.target {
                                    if let Some(actual_name) = self.resolve_signal_precompute_name(assignee) {
                                        self.circom_gen.circ.borrow().cir_ctx().cs.borrow_mut()
                                            .precomputes.add_output(actual_name, value_term.term.clone());
                                    }
                                }

                                // 2. Add constraint
                                let constraint_term = term(Op::Eq, vec![target_term.term, value_term.term]);
                                self.circom_gen.assert_constraint(constraint_term);
                            },
                        }
                    },
                    ast::SignalStatement::ConstraintStatement(stmt) => {
                        // Handle === equality constraints
                        let lhs = self.expr_to_term(&stmt.lhs);
                        let rhs = self.expr_to_term(&stmt.rhs);
                        // Generate boolean equality constraint
                        let constraint_term = term(Op::Eq, vec![lhs.term, rhs.term]);
                        // Assert the constraint to the computation
                        self.circom_gen.assert_constraint(constraint_term);
                    },
                    ast::SignalStatement::SignalAssignmentStatement(assign) => {
                        // Handle <-- witness assignment (without constraint)
                        // These are used for intermediate signals
                        match assign {
                            ast::SignalAssignmentStatement::LeftArrow(left) => {
                                // target <-- value
                                // If this is an intermediate signal, declare it now
                                if let ast::AssigneeTarget::Single(assignee) = &left.target {
                                    let target_name = assignee.id.value.clone();
                                    // Check if it's a signal and not already declared
                                    if self.signal_names.contains(&target_name) &&
                                       !self.public_signals.contains(&target_name) &&
                                       !self.output_signals.contains(&target_name) {
                                        // Check if in component context and need to qualify
                                        let actual_name = if let Some(comp_name) = &self.current_component {
                                            if let Some(signals) = self.component_signals.get(comp_name) {
                                                if let Some(qualified_name) = signals.get(&target_name) {
                                                    qualified_name.clone()
                                                } else {
                                                    target_name.clone()
                                                }
                                            } else {
                                                target_name.clone()
                                            }
                                        } else {
                                            target_name.clone()
                                        };

                                        // Declare as intermediate signal (private witness variable)
                                        self.declare_input_ignore_dup(
                                            actual_name.clone(), &Ty::Field, Some(0),
                                        );
                                    }
                                }

                                // Evaluate the value expression for precomputes
                                let value_term = self.expr_to_term_from_ternary(&left.value);

                                // Add to precomputes for witness generation
                                // (but don't add constraint - that's what makes this different from <==)
                                if let ast::AssigneeTarget::Single(assignee) = &left.target {
                                    if let Some(actual_name) = self.resolve_signal_precompute_name(assignee) {
                                        self.circom_gen.circ.borrow().cir_ctx().cs.borrow_mut()
                                            .precomputes.add_output(actual_name, value_term.term.clone());
                                    }
                                }
                            },
                            ast::SignalAssignmentStatement::RightArrow(right) => {
                                // value --> target (same semantics as target <-- value)
                                if let ast::AssigneeTarget::Single(assignee) = &right.target {
                                    let target_name = assignee.id.value.clone();
                                    if self.signal_names.contains(&target_name) &&
                                       !self.public_signals.contains(&target_name) &&
                                       !self.output_signals.contains(&target_name) {
                                        let actual_name = if let Some(comp_name) = &self.current_component {
                                            if let Some(signals) = self.component_signals.get(comp_name) {
                                                if let Some(qualified_name) = signals.get(&target_name) {
                                                    qualified_name.clone()
                                                } else {
                                                    target_name.clone()
                                                }
                                            } else {
                                                target_name.clone()
                                            }
                                        } else {
                                            target_name.clone()
                                        };
                                        self.declare_input_ignore_dup(
                                            actual_name.clone(), &Ty::Field, Some(0),
                                        );
                                    }
                                }

                                let value_term = self.expr_to_term_from_ternary(&right.value);

                                if let ast::AssigneeTarget::Single(assignee) = &right.target {
                                    if let Some(actual_name) = self.resolve_signal_precompute_name(assignee) {
                                        self.circom_gen.circ.borrow().cir_ctx().cs.borrow_mut()
                                            .precomputes.add_output(actual_name, value_term.term.clone());
                                    }
                                }
                            },
                        }
                    },
                }
            },
            ast::Statement::Component(comp) => {
                self.process_component_statement(comp);
            },
            ast::Statement::Variable(var) => {
                self.process_variable_statement(var, false);  // declarations_only=false (full processing)
            },
            ast::Statement::If(if_stmt) => {
                // Try to evaluate the condition as a compile-time constant
                if let Some(cond_val) = self.extract_constant_value_expr_big(&if_stmt.condition) {
                    // Condition is compile-time constant - execute only the appropriate branch
                    if cond_val != 0 {
                        // Condition is true - execute then branch
                        for s in &mut if_stmt.then_statements {
                            self.visit_statement(s);
                            if self.has_returned { break; }
                        }
                    } else {
                        // Condition is false - check else-if branches
                        let mut executed = false;
                        for branch in &mut if_stmt.else_if_branches {
                            if let Some(branch_cond_val) = self.extract_constant_value_expr_big(&branch.condition) {
                                if branch_cond_val != 0 {
                                    // This else-if condition is true
                                    for s in &mut branch.statements {
                                        self.visit_statement(s);
                                        if self.has_returned { break; }
                                    }
                                    executed = true;
                                    break;
                                }
                            } else {
                                // Can't evaluate else-if condition at compile-time
                                // Process this and ALL remaining branches since
                                // any of them may contribute constraints
                                self.visit_expression(&mut branch.condition);
                                for s in &mut branch.statements {
                                    self.visit_statement(s);
                                    if self.has_returned { break; }
                                }
                                // Don't break - continue to process remaining
                                // else-if branches and the else branch below
                            }
                        }
                        // If no else-if branch executed, execute else branch
                        if !executed && !self.has_returned {
                            if let Some(else_branch) = &mut if_stmt.else_branch {
                                for s in &mut else_branch.statements {
                                    self.visit_statement(s);
                                    if self.has_returned { break; }
                                }
                            }
                        }
                    }
                } else {
                    // Condition is not compile-time constant (depends on signals)
                    // In this case, we need to visit all branches to generate constraints
                    self.visit_expression(&mut if_stmt.condition);
                    for s in &mut if_stmt.then_statements {
                        self.visit_statement(s);
                    }
                    for branch in &mut if_stmt.else_if_branches {
                        self.visit_expression(&mut branch.condition);
                        for s in &mut branch.statements {
                            self.visit_statement(s);
                        }
                    }
                    if let Some(else_branch) = &mut if_stmt.else_branch {
                        for s in &mut else_branch.statements {
                            self.visit_statement(s);
                        }
                    }
                }
            },
            ast::Statement::For(for_stmt) => {
                // Process initialization - register the loop variable
                self.process_variable_statement(&for_stmt.var, false);  // declarations_only=false
                // For loops should have exactly one declaration
                let loop_var = for_stmt.var.declarations.first()
                    .expect("For loop must have at least one variable declaration")
                    .assignee.id.value.clone();

                // Try to extract loop bounds for unrolling
                // Support simple patterns: for(var i = start; i < end; i++)
                let (start, end, step) = self.extract_loop_bounds(&for_stmt);

                if let (Some(start_val), Some(end_val), Some(step_val)) = (start, end, step) {
                    // VALIDATION: Check for suspicious loop bounds
                    if end_val > 10000 || end_val < -10000 || start_val.abs() > 10000 {
                        eprintln!("ERROR: Loop bound appears suspicious: start={}, end={}, step={}", start_val, end_val, step_val);
                        eprintln!("       Loop variable: {}", loop_var);
                        eprintln!("       This may indicate a failed compile-time evaluation.");
                        eprintln!("       If this is unexpected, check that:");
                        eprintln!("         1. All loop bounds are compile-time constants");
                        eprintln!("         2. Functions used in bounds are properly evaluated");
                        eprintln!("         3. Template parameters are correctly passed");
                        panic!("Aborting due to suspicious loop bounds to prevent massive constraint generation.");
                    }


                    // Calculate expected iterations for validation
                    let expected_iterations = if step_val > 0 && end_val >= start_val {
                        ((end_val - start_val) / step_val) as usize
                    } else if step_val < 0 && end_val <= start_val {
                        ((start_val - end_val) / step_val.abs()) as usize
                    } else {
                        0
                    };

                    if expected_iterations > 1000
                        && std::env::var("CIRC_WARN_LOOPS").is_ok()
                    {
                        eprintln!("WARNING: Loop will execute {} iterations. This may generate many constraints.", expected_iterations);
                        eprintln!("         Loop: for (var {} = {}; {} < {}; {} += {})", loop_var, start_val, loop_var, end_val, loop_var, step_val);
                    }

                    // Unroll the loop
                    let mut i = start_val;
                    while (step_val > 0 && i < end_val) || (step_val < 0 && i > end_val) {
                        // Update loop variable value (compile-time)
                        // Check if we're about to overwrite an array parameter (should not happen in valid Circom)
                        if let Some(existing) = self.var_values.get(&loop_var) {
                            if matches!(existing, CompileTimeValue::Array1D(_) | CompileTimeValue::Array2D(_) | CompileTimeValue::ArrayND(_)) {
                                panic!("For loop variable '{}' conflicts with an array parameter/variable. This is invalid in Circom.", loop_var);
                            }
                        }
                        self.var_values.insert(loop_var.clone(), CompileTimeValue::scalar(i));

                        // Execute loop body
                        for s in &mut for_stmt.statements.clone() {
                            self.visit_statement(s);
                        }

                        i += step_val;

                        // Safety limit - now with better error message
                        if i.abs() > 10000 {
                            eprintln!("ERROR: Loop exceeded safety limit of 10000 iterations!");
                            eprintln!("       Loop variable: {}, current value: {}", loop_var, i);
                            eprintln!("       This indicates either:");
                            eprintln!("         1. An infinite loop bug in the circuit");
                            eprintln!("         2. Incorrect loop bound calculation");
                            eprintln!("         3. A failed compile-time evaluation");
                            panic!("Aborting to prevent runaway constraint generation.");
                        }
                    }

                } else {
                    // Improved error message with more context
                    eprintln!("ERROR: Cannot unroll loop - bounds are not compile-time constants");
                    eprintln!("       Loop variable: {}", loop_var);
                    eprintln!("       Start: {:?}, End: {:?}, Step: {:?}", start, end, step);
                    eprintln!("       In Circom, all loop bounds must be known at compile-time.");
                    eprintln!("       Common causes:");
                    eprintln!("         1. Loop bound depends on a signal (runtime value)");
                    eprintln!("         2. Loop bound from function call that wasn't evaluated");
                    eprintln!("         3. Template parameter not properly passed");

                    // Try to give more specific error information
                    if start.is_none() {
                        eprintln!("       Start value could not be determined");
                    }
                    if end.is_none() {
                        eprintln!("       End value could not be determined from condition: {:?}", for_stmt.condition);
                        // Try to extract more info about the end condition
                        if let ast::Expression::Binary(bin) = &for_stmt.condition {
                            if let ast::Expression::Identifier(id) = bin.right.as_ref() {
                                eprintln!("       End bound identifier '{}' - check if it's defined and has a compile-time value", id.value);
                                if let Some(val) = self.var_values.get(&id.value) {
                                    eprintln!("       Found in var_values: {:?}", val);
                                    if val.is_expression() {
                                        eprintln!("       ERROR: Loop bound '{}' is an Expression (runtime), not a compile-time constant!", id.value);
                                    }
                                } else {
                                    eprintln!("       Not found in var_values - may be undefined or out of scope");
                                }
                            }
                        }
                    }
                    if step.is_none() {
                        eprintln!("       Step value could not be determined from: {:?}", for_stmt.increment);
                    }

                    panic!("Cannot compile circuit with non-constant loop bounds.");
                }
            },
            ast::Statement::While(while_stmt) => {
                // Unroll while loops at compile-time (Circom requires constant loop conditions)
                self.execute_while_loop(while_stmt);
            },
            ast::Statement::Return(return_stmt) => {
                // Evaluate return expression and set the return flag
                if let Some(expr) = &return_stmt.expression {
                    let val = self.expr_to_term(expr);
                    self.function_return_value = Some(val);
                }
                self.has_returned = true;
            },
            ast::Statement::Log(log_stmt) => {
                // Visit log arguments
                self.visit_expression(&mut log_stmt.expression);
            },
            ast::Statement::Assert(assert_stmt) => {
                // Try to extract tag values from assertion patterns before visiting
                self.try_extract_tag_value_from_assert(&assert_stmt.expression);
                // Visit assert condition
                self.visit_expression(&mut assert_stmt.expression);
            },
            ast::Statement::Expression(expr) => {
                // Special case: handle increment/decrement as statement (e.g., r++; or ++r;)
                match expr {
                    ast::Expression::Postfix(postfix) => {
                        // Check if this is an increment/decrement expression
                        if let Some(ast::Access::Increment(_)) = postfix.access.get(0) {
                            // Postfix increment: r++
                            if let ast::Expression::Identifier(id) = postfix.base.as_ref() {
                                let var_name = &id.value;
                                // Only update if it's already a scalar (don't overwrite arrays!)
                                if let Some(CompileTimeValue::Scalar(val)) = self.var_values.get(var_name).cloned() {
                                    let new_val = val.clone() + 1;
                                    self.var_values.insert(var_name.clone(), CompileTimeValue::Scalar(new_val));
                                } else {
                                }
                            }
                        } else if let Some(ast::Access::Decrement(_)) = postfix.access.get(0) {
                            // Postfix decrement: r--
                            if let ast::Expression::Identifier(id) = postfix.base.as_ref() {
                                let var_name = &id.value;
                                if let Some(CompileTimeValue::Scalar(val)) = self.var_values.get(var_name).cloned() {
                                    let new_val = val.clone() - 1;
                                    self.var_values.insert(var_name.clone(), CompileTimeValue::Scalar(new_val));
                                } else {
                                }
                            }
                        } else {
                            // Not increment/decrement - just visit
                            self.visit_expression(expr);
                        }
                    }
                    ast::Expression::Unary(unary) => {
                        // Check for prefix increment/decrement: ++r or --r
                        match &unary.op {
                            ast::OpUnary::Increment(_) => {
                                if let ast::Expression::Identifier(id) = unary.expression.as_ref() {
                                    let var_name = &id.value;
                                    if let Some(CompileTimeValue::Scalar(val)) = self.var_values.get(var_name).cloned() {
                                        let new_val = val.clone() + 1;
                                        self.var_values.insert(var_name.clone(), CompileTimeValue::Scalar(new_val));
                                    } else {
                                    }
                                }
                            }
                            ast::OpUnary::Decrement(_) => {
                                if let ast::Expression::Identifier(id) = unary.expression.as_ref() {
                                    let var_name = &id.value;
                                    if let Some(CompileTimeValue::Scalar(val)) = self.var_values.get(var_name).cloned() {
                                        let new_val = val.clone() - 1;
                                        self.var_values.insert(var_name.clone(), CompileTimeValue::Scalar(new_val));
                                    } else {
                                    }
                                }
                            }
                            _ => {
                                // Other unary ops - just visit
                                self.visit_expression(expr);
                            }
                        }
                    }
                    _ => {
                        // Other expressions - just visit
                        self.visit_expression(expr);
                    }
                }
            },
        }
    }
    
    fn visit_expression(&mut self, expr: &mut ast::Expression<'ast>) {
        // Use the default walking implementation
        walk_expression(self, expr);
    }
}
