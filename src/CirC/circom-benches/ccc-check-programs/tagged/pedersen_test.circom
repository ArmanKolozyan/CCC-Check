pragma circom 2.0.0;

include "mux4.circom";
include "num2bits.circom";

function pointAdd(x1,y1,x2,y2) {
    var a = 168700;
    var d = 168696;

    var res[2];
    res[0] = (x1*y2 + y1*x2) / (1 + d*x1*x2*y1*y2);
    res[1] = (y1*y2 - a*x1*x2) / (1 - d*x1*x2*y1*y2);
    return res;
}

function EscalarMulW4Table(base, k) {
    var out[16][2];

    var i;
    var p[2];

    var dbl[2] = base;

    for (i=0; i<k*4; i++) {
        dbl = pointAdd(dbl[0], dbl[1], dbl[0], dbl[1]);
    }

    out[0][0] = 0;
    out[0][1] = 1;
    for (i=1; i<16; i++) {
        p = pointAdd(out[i-1][0], out[i-1][1], dbl[0], dbl[1]);
        out[i][0] = p[0];
        out[i][1] = p[1];
    }

    return out;
}

template BabyAdd() {
    signal input x1;
    signal input y1;
    signal input x2;
    signal input y2;
    signal output xout;
    signal output yout;

    signal beta;
    signal gamma;
    signal delta;
    signal tau;

    var a = 168700;
    var d = 168696;

    beta <== x1*y2;
    gamma <== y1*x2;
    delta <== (-a*x1+y1)*(x2 + y2);
    tau <== beta * gamma;

    xout <-- (beta + gamma) / (1+ d*tau);
    (1+ d*tau) * xout === (beta + gamma);

    yout <-- (delta + a*beta - gamma) / (1-d*tau);
    (1-d*tau)*yout === (delta + a*beta - gamma);
}

template EscalarMulWindow(base, k) {

    signal input in[2];
    signal input {binary} sel[4];
    signal output out[2];

    var table[16][2];
    component mux;
    component adder;

    var i;

    table = EscalarMulW4Table(base, k);
    mux = MultiMux4(2);
    adder = BabyAdd();

    for (i=0; i<4; i++) {
        sel[i] ==> mux.s[i];
    }

    for (i=0; i<16; i++) {
        mux.c[0][i] <== table[i][0];
        mux.c[1][i] <== table[i][1];
    }

    in[0] ==> adder.x1;
    in[1] ==> adder.y1;

    mux.out[0] ==> adder.x2;
    mux.out[1] ==> adder.y2;

    adder.xout ==> out[0];
    adder.yout ==> out[1];
}

template EscalarMul(n, base) {
    signal input {binary} in[n];
    signal input inp[2];   // Point input to be added
    signal output out[2];

    var nBlocks = ((n-1)>>2)+1;
    var i;
    var j;

    signal {binary} aux_0 <== 0;
    component windows[nBlocks];

    // Construct the windows
    for (i=0; i<nBlocks; i++) {
      windows[i] = EscalarMulWindow(base, i);
    }

    // Connect the selectors
    for (i=0; i<nBlocks; i++) {
        for (j=0; j<4; j++) {
            if (i*4+j >= n) {
                windows[i].sel[j] <== aux_0;
            } else {
                windows[i].sel[j] <== in[i*4+j];
            }
        }
    }

    // Start with generator
    windows[0].in[0] <== inp[0];
    windows[0].in[1] <== inp[1];

    for(i=0; i<nBlocks-1; i++) {
        windows[i].out[0] ==> windows[i+1].in[0];
        windows[i].out[1] ==> windows[i+1].in[1];
    }

    windows[nBlocks-1].out[0] ==> out[0];
    windows[nBlocks-1].out[1] ==> out[1];
}

// Pedersen template matching the structure from pedersen_old.circom
template Pedersen(n) {
    signal input {binary} in[n];
    signal output out[2];

    var nexps = ((n-1) \ 250) + 1;
    var nlastbits = n - (nexps-1)*250;

    component escalarMuls[nexps];

    var PBASE[10][2] = [
        [10457101036533406547632367118273992217979173478358440826365724437999023779287,19824078218392094440610104313265183977899662750282163392862422243483260492317],
        [2671756056509184035029146175565761955751135805354291559563293617232983272177,2663205510731142763556352975002641716101654201788071096152948830924149045094],
        [5802099305472655231388284418920769829666717045250560929368476121199858275951,5980429700218124965372158798884772646841287887664001482443826541541529227896],
        [7107336197374528537877327281242680114152313102022415488494307685842428166594,2857869773864086953506483169737724679646433914307247183624878062391496185654],
        [20265828622013100949498132415626198973119240347465898028410217039057588424236,1160461593266035632937973507065134938065359936056410650153315956301179689506],
        [1487999857809287756929114517587739322941449154962237464737694709326309567994,14017256862867289575056460215526364897734808720610101650676790868051368668003],
        [14618644331049802168996997831720384953259095788558646464435263343433563860015,13115243279999696210147231297848654998887864576952244320558158620692603342236],
        [6814338563135591367010655964669793483652536871717891893032616415581401894627,13660303521961041205824633772157003587453809761793065294055279768121314853695],
        [3571615583211663069428808372184817973703476260057504149923239576077102575715,11981351099832644138306422070127357074117642951423551606012551622164230222506],
        [18597552580465440374022635246985743886550544261632147935254624835147509493269,6753322320275422086923032033899357299485124665258735666995435957890214041481]
    ];

    var i;
    var j;
    var nexpbits;
    for (i=0; i<nexps; i++) {
        nexpbits = (i == nexps-1) ? nlastbits : 250;
        escalarMuls[i] = EscalarMul(nexpbits, PBASE[i]);

        for (j=0; j<nexpbits; j++) {
            escalarMuls[i].in[j] <== in[250*i + j];
        }

        if (i==0) {
            escalarMuls[i].inp[0] <== 0;
            escalarMuls[i].inp[1] <== 1;
        } else {
            escalarMuls[i].inp[0] <== escalarMuls[i-1].out[0];
            escalarMuls[i].inp[1] <== escalarMuls[i-1].out[1];
        }
    }

    escalarMuls[nexps-1].out[0] ==> out[0];
    escalarMuls[nexps-1].out[1] ==> out[1];
}

template Main() {
    signal input in[2];
    signal output out[2];

    component pedersen = Pedersen(250*2);

    component n2b[2];
    n2b[0] = Num2Bits(250);
    n2b[1] = Num2Bits(250);

    var i;

    in[0] ==> n2b[0].in;
    in[1] ==> n2b[1].in;

    for (i=0; i<250; i++) {
        n2b[0].out[i] ==> pedersen.in[i];
        n2b[1].out[i] ==> pedersen.in[250+i];
    }

    pedersen.out[0] ==> out[0];
    pedersen.out[1] ==> out[1];
}

component main = Main();
