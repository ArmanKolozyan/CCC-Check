pragma circom 2.1.9;


// Fully flattened (i.e., everything in a single template) 
// version of using the LessThan template correctly 
// with input variables that are constrained 
// to be representable in 2 bits.
template FlattenedLessThan2() {
    signal input a;
    signal input b;
    signal output out;

    //
    // 1) We first constrain each of a and b to be representable in 2 bits.
    //    (b0*(b0-1)=0, b1*(b1-1)=0, and we reconstruct 'a' and 'b' from these bits.
    //

    // For a:
    signal a_b0 <-- (a & 1);          // least significant bit
    signal a_b1 <-- ((a >> 1) & 1);   // next bit
    a_b0 * (a_b0 - 1) === 0;           // forces a_b0 to be 0 or 1
    a_b1 * (a_b1 - 1) === 0;           // forces a_b1 to be 0 or 1
    a_b0 + 2 * a_b1 === a;         // reconstructing a from bits

    // Same for b:
    signal b_b0 <-- (b & 1);
    signal b_b1 <-- ((b >> 1) & 1);
    b_b0 * (b_b0 - 1) === 0;
    b_b1 * (b_b1 - 1) === 0;
    b_b0 + 2 * b_b1 === b;

    //
    // 2) We then flatten out what LessThan(2) would do:
    //    out = 1 - (the highest bit of (a + 2^2 - b)).
    //

    // We compute diff = a + 4 - b.
    signal diff;
    diff <== a + (1 << 2) - b;  // 1<<2 = 4

    // 3) We flatten out the Num2Bits(3) call on 'diff':
    //    We extract 3 bits (diff_b0, diff_b1, diff_b2),
    //    force each to be 0 or 1, and reconstruct diff from them.
    signal diff_b0 <-- (diff & 1);
    signal diff_b1 <-- ((diff >> 1) & 1);
    signal diff_b2 <-- ((diff >> 2) & 1);

    // each diff_bi must be 0 or 1
    diff_b0 * (diff_b0 - 1) === 0;  
    diff_b1 * (diff_b1 - 1) === 0;
    diff_b2 * (diff_b2 - 1) === 0;

    diff_b0 + 2 * diff_b1 + 4 * diff_b2 === diff;

    // 4) The original LessThan(n) uses out = 1 - diff_bN for N bits.
    //    Here, N=2 => diff_b2 is the 3rd bit. So:
    out <== 1 - diff_b2;
}

component main = FlattenedLessThan2();
