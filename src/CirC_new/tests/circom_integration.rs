// Integration tests for Circom frontend

use std::path::PathBuf;
use std::sync::Once;
use circ::front::circom::{CircomFE, Inputs};
use circ::front::FrontEnd;
use circ::ir::term::Computations;
use circ::cfg::{self, CircOpt};

static INIT: Once = Once::new();

/// Initialize CirC configuration once for all tests
fn init_config() {
    INIT.call_once(|| {
        let opts = CircOpt::default();
        cfg::set(&opts);
    });
}

/// Helper function to compile a Circom circuit
fn compile_circom(file_path: &str) -> Computations {
    init_config();
    let path = PathBuf::from(file_path);
    CircomFE::gen(Inputs { file: path })
}

/// Helper function to count constraints in a computation
fn count_constraints(comp: &Computations) -> usize {
    comp.comps
        .get("main")
        .map(|cs| cs.outputs.len())
        .unwrap_or(0)
}

#[test]
fn test_simple_constraint() {
    let comp = compile_circom("examples/Circom/pf/simple_constraint.circom");
    let constraints = count_constraints(&comp);
    assert_eq!(constraints, 1, "Expected 1 constraint for simple_constraint");
}

#[test]
fn test_arithmetic_ops() {
    let comp = compile_circom("examples/Circom/pf/arithmetic.circom");
    // Arithmetic circuit should compile successfully
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_compound_assign() {
    let comp = compile_circom("examples/Circom/pf/compound_assign_simple.circom");
    let constraints = count_constraints(&comp);
    assert_eq!(constraints, 1, "Expected 1 constraint for compound_assign_simple");
}

#[test]
fn test_bitwise_ops() {
    let comp = compile_circom("examples/Circom/pf/bitwise_ops.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_shifts() {
    let comp = compile_circom("examples/Circom/pf/shifts.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_power_mod_div() {
    let comp = compile_circom("examples/Circom/pf/power_mod_div.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_comparison_ops() {
    let comp = compile_circom("examples/Circom/pf/comparison_ops.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_logical_ops() {
    let comp = compile_circom("examples/Circom/pf/logical_ops.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_ternary_simple() {
    let comp = compile_circom("examples/Circom/pf/ternary_simple.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_ternary() {
    let comp = compile_circom("examples/Circom/pf/ternary.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_increment_decrement() {
    let comp = compile_circom("examples/Circom/pf/increment_decrement.circom");
    let constraints = count_constraints(&comp);
    assert_eq!(constraints, 1, "Expected 1 constraint for increment_decrement");
}

#[test]
fn test_mixed_operations() {
    let comp = compile_circom("examples/Circom/pf/mixed_operations.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_mixed_constraints() {
    let comp = compile_circom("examples/Circom/pf/mixed_constraints.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
    let constraints = count_constraints(&comp);
    assert!(constraints >= 2, "Expected at least 2 constraints for mixed_constraints");
}

#[test]
fn test_array_basic() {
    let comp = compile_circom("examples/Circom/pf/array_basic.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_array_sum() {
    let comp = compile_circom("examples/Circom/pf/array_sum.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_loop() {
    let comp = compile_circom("examples/Circom/pf/loop_test.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_signal_array() {
    let comp = compile_circom("examples/Circom/pf/signal_array.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_precedence() {
    let comp = compile_circom("examples/Circom/pf/precedence_test.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

// Additional expression tests
#[test]
fn test_nested_ternary() {
    let comp = compile_circom("examples/Circom/pf/nested_ternary.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_complex_expressions() {
    let comp = compile_circom("examples/Circom/pf/complex_expressions.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_unary_ops() {
    let comp = compile_circom("examples/Circom/pf/unary_ops.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_bitwise_not() {
    let comp = compile_circom("examples/Circom/pf/bitwise_not.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_bitwise_compound() {
    let comp = compile_circom("examples/Circom/pf/bitwise_compound.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_postfix_ops() {
    let comp = compile_circom("examples/Circom/pf/postfix_ops.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

// Template modifier tests
#[test]
fn test_custom_template() {
    let comp = compile_circom("examples/Circom/pf/custom_template.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_parallel_template() {
    let comp = compile_circom("examples/Circom/pf/parallel_template.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_signal_tags() {
    let comp = compile_circom("examples/Circom/pf/signal_tags_test.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

// Simple operation tests
#[test]
fn test_add() {
    let comp = compile_circom("examples/Circom/pf/add.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_mult() {
    let comp = compile_circom("examples/Circom/pf/mult.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_greater_than() {
    let comp = compile_circom("examples/Circom/pf/greater_than.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_assert() {
    let comp = compile_circom("examples/Circom/pf/assert.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

// Advanced operation tests
#[test]
fn test_compound_assign_advanced() {
    let comp = compile_circom("examples/Circom/pf/compound_assign.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_if_else_constraints() {
    let comp = compile_circom("examples/Circom/pf/if_else_constraints.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_signal_assignment_types() {
    let comp = compile_circom("examples/Circom/pf/signal_assignment_types.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

// Array tests
#[test]
fn test_array_iteration() {
    let comp = compile_circom("examples/Circom/pf/array_iteration.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

#[test]
fn test_multidim_array() {
    let comp = compile_circom("examples/Circom/pf/multidim_array.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
}

// Template parameter tests
#[test]
fn test_template_param_basic() {
    let comp = compile_circom("examples/Circom/pf/template_param_basic.circom");
    let constraints = count_constraints(&comp);
    assert_eq!(constraints, 1, "Expected 1 constraint for basic template parameter");
}

#[test]
fn test_template_param_array() {
    let comp = compile_circom("examples/Circom/pf/template_param_array.circom");
    let constraints = count_constraints(&comp);
    assert_eq!(constraints, 1, "Expected 1 constraint for template parameter array");
}

#[test]
fn test_template_param_matrix() {
    let comp = compile_circom("examples/Circom/pf/template_param_matrix.circom");
    let constraints = count_constraints(&comp);
    assert_eq!(constraints, 1, "Expected 1 constraint for template parameter matrix");
}

// Note: Some advanced tests are commented out because they require
// features that are not yet fully implemented (e.g., full component instantiation)

#[test]
fn test_component_basic() {
    let comp = compile_circom("examples/Circom/pf/component_basic.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
    let constraints = count_constraints(&comp);
    assert_eq!(constraints, 3, "Expected 3 constraints for component_basic");
}

#[test]
fn test_function_basic() {
    let comp = compile_circom("examples/Circom/pf/function_basic.circom");
    assert!(comp.comps.contains_key("main"), "Main computation should exist");
    let constraints = count_constraints(&comp);
    assert_eq!(constraints, 1, "Expected 1 constraint for function_basic");
}
