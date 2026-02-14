use crate::target::datan::Datan;

/// Serializes a Datan instance into a string representation of Datalog facts.
pub fn serialize_datan(datan: &Datan) -> String {
    let mut output = String::new();

    output.push_str("// Private Inputs:\n");
    for input in &datan.private_inputs {
        output.push_str(&input.to_string());
        output.push('\n');
    }

    output.push_str("// Public Inputs:\n");
    for input in &datan.public_inputs {
        output.push_str(&input.to_string());
        output.push('\n');
    }

    output.push_str("// Identifiers:\n");
    for identifier in &datan.identifiers {
        output.push_str(&identifier.to_string());
        output.push('\n');
    }

    // we don't need to output expressions
    // output.push_str("// Expressions:\n");
    // for expr in &datan.exprs {
    //     output.push_str(&expr.to_string());
    //     output.push('\n');
    // }

    output.push_str("// Field Constants:\n");
    for constant in &datan.field_constants {
        output.push_str(&format!("FieldConstant(\"{}\").", constant));
        output.push('\n');
    }

    output.push_str("// BV Constants:\n");
    for constant in &datan.bv_constants {
        output.push_str(&format!("BVConstant(\"{}\").", constant));
        output.push('\n');
    }

    output.push_str("// Boolean Constants:\n");
    for constant in &datan.boolean_constants {
        output.push_str(&format!("BooleanConstant(\"{}\").", constant));
        output.push('\n');
    }

    output.push_str("// Binary Expressions:\n");
    for binary_expr in &datan.binary_exprs {
        output.push_str(&binary_expr.to_string());
        output.push('\n');
    }

    output.push_str("// Assignments:\n");
    for assign in &datan.assigns {
        output.push_str(&assign.to_string());
        output.push('\n');
    }

    output.push_str("// Assertions:\n");
    for assertion in &datan.asserts {
        output.push_str(&assertion.to_string());
        output.push('\n');
    }

    output.push_str("// Not:\n");
    for not in &datan.nots {
        output.push_str(&not.to_string());
        output.push('\n');
    }

    output.push_str("// Ite:\n");
    for ite in &datan.ites {
        output.push_str(&ite.to_string());
        output.push('\n');
    }

    output.push_str("// Transforms:\n");
    for transform in &datan.transforms {
        output.push_str(&transform.to_string());
        output.push('\n');
    }

    output.push_str("// ConcatX:\n");
    for concatx in &datan.concatxs {
        output.push_str(&concatx.to_string());
        output.push('\n');
    }

    output.push_str("// BoolMaj:\n");
    for boolmaj in &datan.boolmjs {
        output.push_str(&boolmaj.to_string());
        output.push('\n');
    }

    output
}