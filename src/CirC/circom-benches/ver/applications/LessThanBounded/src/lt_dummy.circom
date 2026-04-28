pragma circom 2.0.0;

include "../../../libs/circomlib/circuits/bitify.circom";

template LessThanPowerDummy(base) {
  signal input in;
  signal output out;

  // DUMMY BEGINS
  component dummy_comp = Multiplier();
  dummy_comp.inp <-- in;
  signal dummy;
  dummy <-- dummy_comp.out - 2*in;
  // DUMMY ENDS

  //Signal "out" can be 0 or 1 without any bounding, missing <== assignment
  //Signal "in" is inherently unbounded because (in >> base) can be greater than 0 in many in values
  out <-- (in >> base) > 0 ? 0 : 1;
  out * (out - 1) === 0;
}

template LessThanPower(base) {
  signal input in;
  signal output out;

  out <-- (in >> base) > 0 ? 0 : 1;
  out * (out - 1) === 0;
}

template LessThanBounded(base) {
  signal input in[2];
  signal output out;

  component lt1 = LessThanPower(base);
  lt1.in <== in[0];

  component lt2 = LessThanPower(base);
  lt2.in <== in[1];

  // DUMMY BEGINS
  component dummy_comp = Multiplier();
  dummy_comp.inp <-- in[0];
  signal dummy;
  dummy <-- dummy_comp.out - 2*in[0];
  // DUMMY ENDS

  //Signal "out" can be 0 or 1 without any bounding, missing <== assignment
  out <-- in[0] < in[1] ? 1 : 0;
  out * (out - 1) === 0;
}

// DUMMY BEGINS
template Multiplier() {
    signal input  inp;
    signal output out;
    // Ignore the computation here.
    out <-- 2*inp;
}
// DUMMY ENDS