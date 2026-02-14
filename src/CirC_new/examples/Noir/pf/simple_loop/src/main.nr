fn main(x: pub Field) {
    let mut sum: Field = 0;
    for _i in 0..5 {
        sum = sum + x;
    }
    assert(sum == 15);
}
