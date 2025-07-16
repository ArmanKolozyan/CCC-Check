pragma circom 2.0.2;

include "bigint.circom";

template Aux1(){
   signal input a[4];
   signal input b[4];
   signal input c[4];
   
   signal output out1[7];
   
   signal {maxbit} aux1[4];
   aux1.maxbit = 64;
   aux1 <== a;
   
   signal {maxbit} aux2[4];
   aux2.maxbit = 64;
   aux2 <== b;
   
   signal {maxbit} aux3[4];
   aux3.maxbit = 64;
   aux3 <== c;
   
   
   signal output out2[10];
   out2 <== A3NoCarry()(aux1);
   
   signal output out3[10];
   out3 <== A2B1NoCarry()(aux1, aux2);
   
   signal output out4[10];
   out4 <== A1B1C1NoCarry()(aux1, aux2, aux3);
   
}


template A2NoCarry() {
    signal input {maxbit} a[4]; // represents number of 
    
    assert(a.maxbit <= 64);

    // these representations have overflowed, nonnegative registers
    signal output {maxbit} a2[7];
    
    component a2Comp = BigMultNoCarry(64, 64, 64, 4, 4);
    for (var i = 0; i < 4; i++) {
        a2Comp.a[i] <== a[i];
        a2Comp.b[i] <== a[i];
    }
    for (var i = 0; i < 7; i++) {
        a2[i] <== a2Comp.out[i]; // 130 bits
    }
    log(a2.maxbit);
}

template A3NoCarry() {
    signal input {maxbit} a[4];
    
    assert(a.maxbit <= 64);

    // these representations have overflowed, nonnegative registers
    signal {maxbit} a2[7];
    component a2Comp = BigMultNoCarry(64, 64, 64, 4, 4);
    for (var i = 0; i < 4; i++) {
        a2Comp.a[i] <== a[i];
        a2Comp.b[i] <== a[i];
    }
    for (var i = 0; i < 7; i++) {
        a2[i] <== a2Comp.out[i]; // 130 bits
    }
    signal output {maxbit} a3[10];
    component a3Comp = BigMultNoCarry(64, 130, 64, 7, 4);
    for (var i = 0; i < 7; i++) {
        a3Comp.a[i] <== a2[i];
    }
    for (var i = 0; i < 4; i++) {
        a3Comp.b[i] <== a[i];
    }
    for (var i = 0; i < 10; i++) {
        a3[i] <== a3Comp.out[i]; // 197 bits ----> TODO: check that we only need 196
    }
    log(a3.maxbit);
}

template A2B1NoCarry() {
    signal input {maxbit} a[4];
    signal input {maxbit} b[4];
    
    assert(a.maxbit <= 64);
    assert(b.maxbit <= 64);

    // these representations have overflowed, nonnegative registers
    signal {maxbit} a2[7];
    component a2Comp = BigMultNoCarry(64, 64, 64, 4, 4);
    for (var i = 0; i < 4; i++) {
        a2Comp.a[i] <== a[i];
        a2Comp.b[i] <== a[i];
    }
    for (var i = 0; i < 7; i++) {
        a2[i] <== a2Comp.out[i]; // 130 bits
    }

    signal output {maxbit} a2b1[10];
    component a2b1Comp = BigMultNoCarry(64, 130, 64, 7, 4);
    for (var i = 0; i < 7; i++) {
        a2b1Comp.a[i] <== a2[i];
    }
    for (var i = 0; i < 4; i++) {
        a2b1Comp.b[i] <== b[i];
    }
    for (var i = 0; i < 10; i++) {
        a2b1[i] <== a2b1Comp.out[i]; // 197 bits ---> TODO: check that we only need 196
    }
    log(a2b1.maxbit);
}

template A1B1C1NoCarry() {
    signal input {maxbit} a[4];
    signal input {maxbit} b[4];
    signal input {maxbit} c[4];
    
    assert(a.maxbit <= 64);
    assert(b.maxbit <= 64);
    assert(c.maxbit <= 64);

    // these representations have overflowed, nonnegative registers
    signal {maxbit} a1b1[7];
    component a1b1Comp = BigMultNoCarry(64, 64, 64, 4, 4);
    for (var i = 0; i < 4; i++) {
        a1b1Comp.a[i] <== a[i];
        a1b1Comp.b[i] <== b[i];
    }
    for (var i = 0; i < 7; i++) {
        a1b1[i] <== a1b1Comp.out[i]; // 130 bits
    }

    signal output {maxbit} a1b1c1[10];
    component a1b1c1Comp = BigMultNoCarry(64, 130, 64, 7, 4);
    for (var i = 0; i < 7; i++) {
        a1b1c1Comp.a[i] <== a1b1[i];
    }
    for (var i = 0; i < 4; i++) {
        a1b1c1Comp.b[i] <== c[i];
    }
    for (var i = 0; i < 10; i++) {
        a1b1c1[i] <== a1b1c1Comp.out[i]; // 197 bits
    }
    log(a1b1c1.maxbit);
}


component main = Aux1();
