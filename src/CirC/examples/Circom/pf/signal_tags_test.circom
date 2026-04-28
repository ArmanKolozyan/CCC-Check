pragma circom 2.0.0;

// Test signal tags
template SignalTagsTest() {
    signal {tag1, tag2} input a;
    signal {output_tag} output b;

    b <== a + 5;
}

component main {public [a]} = SignalTagsTest();
