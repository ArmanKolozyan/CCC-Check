//! Embedded curve (Grumpkin) operations for the Noir frontend.
//!
//! Decomposes elliptic curve point addition and scalar multiplication on the
//! Grumpkin curve (y² = x³ - 17, a=0) into CirC IR field arithmetic primitives.

use crate::ir::term::*;
use circ_fields::FieldT;

/// Field subtraction: a - b = a + (-b).
fn pf_sub(a: Term, b: Term) -> Term {
    let neg_b = term![Op::PfUnOp(PfUnOp::Neg); b];
    term![Op::PfNaryOp(PfNaryOp::Add); a, neg_b]
}

/// Field multiplication.
fn pf_mul(a: Term, b: Term) -> Term {
    term![Op::PfNaryOp(PfNaryOp::Mul); a, b]
}

/// Field addition.
#[allow(dead_code)]
fn pf_add(a: Term, b: Term) -> Term {
    term![Op::PfNaryOp(PfNaryOp::Add); a, b]
}

/// Field inversion: 1/a.
fn pf_inv(a: Term) -> Term {
    term![Op::PfUnOp(PfUnOp::Recip); a]
}

/// If-then-else for field elements.
fn pf_ite(cond: Term, then_val: Term, else_val: Term) -> Term {
    term![Op::Ite; cond, then_val, else_val]
}

/// Point doubling on Grumpkin (a=0) with safe denominator.
/// Given P=(x,y):
///   λ = 3x² / (2y)
///   x3 = λ² - 2x
///   y3 = λ*(x-x3) - y
/// Uses safe denominator: when y=0, replaces 2y with 1 to avoid division by zero.
/// The result is garbage when y=0, but callers guard with ITE.
fn point_double(field: &FieldT, x: &Term, y: &Term) -> (Term, Term) {
    let zero = pf_lit(field.zero());
    let one = pf_lit(field.new_v(1u64));
    let three = pf_lit(field.new_v(3u64));
    let two = pf_lit(field.new_v(2u64));
    let x_sq = pf_mul(x.clone(), x.clone());
    let y_is_zero = term![Op::Eq; y.clone(), zero];
    let denom = pf_mul(two.clone(), y.clone());
    let safe_denom = pf_ite(y_is_zero, one, denom);
    let lambda = pf_mul(pf_mul(three, x_sq), pf_inv(safe_denom));
    let lambda_sq = pf_mul(lambda.clone(), lambda.clone());
    let x3 = pf_sub(lambda_sq, pf_mul(two, x.clone()));
    let y3 = pf_sub(pf_mul(lambda.clone(), pf_sub(x.clone(), x3.clone())), y.clone());
    (x3, y3)
}

/// Embedded curve point addition with full edge-case handling.
///
/// Handles: infinity inputs, doubling (x1==x2, y1==y2), inverse (x1==x2, y1!=y2),
/// and the predicate flag.
///
/// Both addition and doubling formulas are always computed, but with safe
/// denominators to avoid division by zero. The `Ite` selects the correct result.
///
/// Returns (x3, y3, is_infinite3) as Terms.
pub fn embedded_curve_add(
    field: &FieldT,
    input1: &[Term; 3], // [x1, y1, is_inf1]
    input2: &[Term; 3], // [x2, y2, is_inf2]
    predicate: &Term,
) -> (Term, Term, Term) {
    let x1 = &input1[0];
    let y1 = &input1[1];
    let inf1 = &input1[2];
    let x2 = &input2[0];
    let y2 = &input2[1];
    let inf2 = &input2[2];

    let zero = pf_lit(field.zero());
    let one = pf_lit(field.new_v(1u64));

    // Check if x1 == x2
    let x_eq = term![Op::Eq; x1.clone(), x2.clone()];
    // Check if y1 == y2
    let y_eq = term![Op::Eq; y1.clone(), y2.clone()];
    let same_point = term![Op::BoolNaryOp(BoolNaryOp::And); x_eq.clone(), y_eq];

    // Safe addition formula: use (x2-x1) as denominator, but replace with 1 when x_eq
    // to avoid division by zero. The result is discarded when x_eq anyway.
    let dx = pf_sub(x2.clone(), x1.clone());
    let safe_dx = pf_ite(x_eq.clone(), one.clone(), dx);
    let lambda_add = pf_mul(pf_sub(y2.clone(), y1.clone()), pf_inv(safe_dx));
    let lambda_add_sq = pf_mul(lambda_add.clone(), lambda_add.clone());
    let add_x = pf_sub(pf_sub(lambda_add_sq, x1.clone()), x2.clone());
    let add_y = pf_sub(
        pf_mul(lambda_add, pf_sub(x1.clone(), add_x.clone())),
        y1.clone(),
    );

    // Doubling formula: λ = 3x1²/(2y1)
    let (dbl_x, dbl_y) = point_double(field, x1, y1);

    // Select result based on point relationship:
    // same_point → doubling
    // x_eq && !same_point → inverse points → infinity
    // !x_eq → standard addition
    let result_x = pf_ite(
        same_point.clone(),
        dbl_x,
        pf_ite(x_eq.clone(), zero.clone(), add_x),
    );
    let result_y = pf_ite(
        same_point.clone(),
        dbl_y,
        pf_ite(x_eq.clone(), zero.clone(), add_y),
    );
    let result_inf = pf_ite(
        same_point,
        zero.clone(),
        pf_ite(x_eq, one.clone(), zero.clone()),
    );

    // Handle infinity inputs:
    // If inf1 == 1 → return input2
    // If inf2 == 1 → return input1
    let inf1_flag = term![Op::Eq; inf1.clone(), one.clone()];
    let inf2_flag = term![Op::Eq; inf2.clone(), one.clone()];

    let result_x = pf_ite(
        inf1_flag.clone(),
        x2.clone(),
        pf_ite(inf2_flag.clone(), x1.clone(), result_x),
    );
    let result_y = pf_ite(
        inf1_flag.clone(),
        y2.clone(),
        pf_ite(inf2_flag.clone(), y1.clone(), result_y),
    );
    let result_inf = pf_ite(
        inf1_flag,
        inf2.clone(),
        pf_ite(inf2_flag, inf1.clone(), result_inf),
    );

    // Handle predicate: if predicate == 0, return point at infinity
    let pred_false = term![Op::Eq; predicate.clone(), zero.clone()];
    let result_x = pf_ite(pred_false.clone(), zero.clone(), result_x);
    let result_y = pf_ite(pred_false.clone(), zero.clone(), result_y);
    let result_inf = pf_ite(pred_false, one, result_inf);

    (result_x, result_y, result_inf)
}

/// Point addition with safe denominator.
/// When x1==x2, replaces (x2-x1) with 1 to avoid division by zero.
/// The result is garbage when x1==x2, but callers guard with ITE.
fn point_add(field: &FieldT, x1: &Term, y1: &Term, x2: &Term, y2: &Term) -> (Term, Term) {
    let one = pf_lit(field.new_v(1u64));
    let dx = pf_sub(x2.clone(), x1.clone());
    let x_eq = term![Op::Eq; x1.clone(), x2.clone()];
    let safe_dx = pf_ite(x_eq, one, dx);
    let lambda = pf_mul(pf_sub(y2.clone(), y1.clone()), pf_inv(safe_dx));
    let lambda_sq = pf_mul(lambda.clone(), lambda.clone());
    let x3 = pf_sub(pf_sub(lambda_sq, x1.clone()), x2.clone());
    let y3 = pf_sub(pf_mul(lambda, pf_sub(x1.clone(), x3.clone())), y1.clone());
    (x3, y3)
}

/// Scalar multiplication using double-and-add.
///
/// Computes scalar * point where scalar = scalar_lo + scalar_hi * 2^128.
/// Returns (x, y, is_infinite).
pub fn scalar_mul(
    field: &FieldT,
    point: &[Term; 3], // [x, y, is_inf]
    scalar_lo: &Term,
    scalar_hi: &Term,
) -> (Term, Term, Term) {
    let zero = pf_lit(field.zero());
    let one = pf_lit(field.new_v(1u64));

    // Decompose scalar_lo and scalar_hi to 128-bit bitvectors
    let lo_bv = term![Op::PfToBv(128); scalar_lo.clone()];
    let hi_bv = term![Op::PfToBv(128); scalar_hi.clone()];

    // Start with point at infinity
    let mut acc_x = zero.clone();
    let mut acc_y = zero.clone();
    let mut acc_inf = one.clone();

    // Process high bits first (bits 255 down to 128)
    for bit_idx in (0..128).rev() {
        // Double the accumulator
        let pred_not_inf = term![Op::Eq; acc_inf.clone(), zero.clone()];
        let (dbl_x, dbl_y) = point_double(field, &acc_x, &acc_y);
        // If acc is infinity, doubling gives infinity; otherwise use doubled point
        let new_acc_x = pf_ite(pred_not_inf.clone(), dbl_x, zero.clone());
        let new_acc_y = pf_ite(pred_not_inf.clone(), dbl_y, zero.clone());
        // After doubling, still infinity if was infinity
        acc_x = new_acc_x;
        acc_y = new_acc_y;
        // acc_inf stays the same after doubling (infinity doubled is infinity)

        // Extract the current bit
        let bit = term![Op::BvBit(bit_idx); hi_bv.clone()];

        // If bit is 1, add the point
        // Compute addition of acc + point
        let (add_x, add_y) = point_add(field, &acc_x, &acc_y, &point[0], &point[1]);

        // If acc is infinity and bit is 1, result is just the point
        // If acc is not infinity and bit is 1, result is add
        // If bit is 0, keep acc
        let acc_is_inf = term![Op::Eq; acc_inf.clone(), one.clone()];
        let add_result_x = pf_ite(acc_is_inf.clone(), point[0].clone(), add_x);
        let add_result_y = pf_ite(acc_is_inf.clone(), point[1].clone(), add_y);
        let add_result_inf = pf_ite(acc_is_inf, zero.clone(), zero.clone());

        acc_x = pf_ite(bit.clone(), add_result_x, acc_x);
        acc_y = pf_ite(bit.clone(), add_result_y, acc_y);
        acc_inf = pf_ite(bit, add_result_inf, acc_inf);
    }

    // Process low bits (bits 127 down to 0)
    for bit_idx in (0..128).rev() {
        // Double
        let pred_not_inf = term![Op::Eq; acc_inf.clone(), zero.clone()];
        let (dbl_x, dbl_y) = point_double(field, &acc_x, &acc_y);
        let new_acc_x = pf_ite(pred_not_inf.clone(), dbl_x, zero.clone());
        let new_acc_y = pf_ite(pred_not_inf.clone(), dbl_y, zero.clone());
        acc_x = new_acc_x;
        acc_y = new_acc_y;

        // Extract bit
        let bit = term![Op::BvBit(bit_idx); lo_bv.clone()];

        // If bit is 1, add
        let (add_x, add_y) = point_add(field, &acc_x, &acc_y, &point[0], &point[1]);
        let acc_is_inf = term![Op::Eq; acc_inf.clone(), one.clone()];
        let add_result_x = pf_ite(acc_is_inf.clone(), point[0].clone(), add_x);
        let add_result_y = pf_ite(acc_is_inf.clone(), point[1].clone(), add_y);
        let add_result_inf = pf_ite(acc_is_inf, zero.clone(), zero.clone());

        acc_x = pf_ite(bit.clone(), add_result_x, acc_x);
        acc_y = pf_ite(bit.clone(), add_result_y, acc_y);
        acc_inf = pf_ite(bit, add_result_inf, acc_inf);
    }

    (acc_x, acc_y, acc_inf)
}

/// Multi-scalar multiplication: result = Σ(scalar_i * point_i).
///
/// Points are given as flat [x1,y1,inf1, x2,y2,inf2, ...].
/// Scalars are given as flat [lo1,hi1, lo2,hi2, ...].
pub fn multi_scalar_mul(
    field: &FieldT,
    points: &[Term],
    scalars: &[Term],
) -> (Term, Term, Term) {
    let n = points.len() / 3;
    assert_eq!(points.len(), 3 * n);
    assert_eq!(scalars.len(), 2 * n);

    let zero = pf_lit(field.zero());
    let one = pf_lit(field.new_v(1u64));

    // Start with infinity
    let mut acc_x = zero.clone();
    let mut acc_y = zero.clone();
    let mut acc_inf = one.clone();

    for i in 0..n {
        let point = [
            points[3 * i].clone(),
            points[3 * i + 1].clone(),
            points[3 * i + 2].clone(),
        ];
        let scalar_lo = &scalars[2 * i];
        let scalar_hi = &scalars[2 * i + 1];

        // Compute scalar * point
        let (sm_x, sm_y, sm_inf) = scalar_mul(field, &point, scalar_lo, scalar_hi);

        // Accumulate: acc = acc + (scalar * point)
        let pred = pf_lit(field.new_v(1u64));
        let acc_point = [acc_x, acc_y, acc_inf];
        let sm_point = [sm_x, sm_y, sm_inf];
        let (new_x, new_y, new_inf) = embedded_curve_add(field, &acc_point, &sm_point, &pred);
        acc_x = new_x;
        acc_y = new_y;
        acc_inf = new_inf;
    }

    (acc_x, acc_y, acc_inf)
}
