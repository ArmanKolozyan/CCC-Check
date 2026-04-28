fn main(x: pub Field, y: Field) {
    let result = if x == 1 { y + 1 } else { y };
    assert(result == 5);
}
