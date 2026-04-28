fn main(n: pub u32) {
    let mut a: u32 = 0;
    let mut b: u32 = 1;
    for _i in 0..10 {
        let temp = a + b;
        a = b;
        b = temp;
    }
    assert(b == 89);
}
