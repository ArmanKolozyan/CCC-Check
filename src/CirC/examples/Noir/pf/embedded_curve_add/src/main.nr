use std::embedded_curve_ops::EmbeddedCurvePoint;
use std::embedded_curve_ops::embedded_curve_add;

fn main(x1: pub Field, y1: pub Field, expected_x: Field, expected_y: Field) {
    let point = EmbeddedCurvePoint { x: x1, y: y1, is_infinite: false };
    let result = embedded_curve_add(point, point);
    assert(result.x == expected_x);
    assert(result.y == expected_y);
}
