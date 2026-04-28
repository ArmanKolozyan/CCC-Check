fn square(x: Field) -> Field {
    x * x
}

fn main(x: pub Field) {
    let y = square(x);
    assert(y == 9);
}
