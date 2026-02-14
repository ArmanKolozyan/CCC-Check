pragma circom 2.0.0;

// This should fail with: Template not found (with list of available templates)
template Main() {
    signal output y;
    component c = NonexistentTemplate();  // ERROR: template not defined
    y <== 1;
}

component main = Main();
