use std::embedded_curve_ops::{EmbeddedCurvePoint, EmbeddedCurveScalar, multi_scalar_mul};

// Compute scalar * G where G is the Grumpkin generator and scalar = 2
// (i.e., 2*G which is point doubling of the generator)
fn main(
    point_x: pub Field,
    point_y: pub Field,
    scalar_lo: pub Field,
    scalar_hi: pub Field,
    expected_x: Field,
    expected_y: Field,
) {
    let point = EmbeddedCurvePoint { x: point_x, y: point_y, is_infinite: false };
    let scalar = EmbeddedCurveScalar { lo: scalar_lo, hi: scalar_hi };
    let result = multi_scalar_mul([point], [scalar]);
    assert(result.x == expected_x);
    assert(result.y == expected_y);
}
