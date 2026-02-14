pragma custom_templates;

// Test pragma custom_templates directive
template SimpleTemplate() {
    signal input x;
    signal output y;

    y <== x * 3;
}

component main {public [x]} = SimpleTemplate();
