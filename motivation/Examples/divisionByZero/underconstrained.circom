pragma circom 2.1.9;

// https://docs.veridise.com/zkvanguard/detectors/zk-divide-by-zero/
// fake proof with inputs dividend = 0, divisor = 0
// and quotient = 5
// 5 * 0 === 0
template Divide() {
  signal input dividend;
  signal input divisor;
  signal output quotient;
  
  quotient <-- dividend / divisor;
  quotient * divisor === dividend;
}

component main = Divide();