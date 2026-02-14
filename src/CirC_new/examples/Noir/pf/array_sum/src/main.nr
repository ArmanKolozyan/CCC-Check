fn main(arr: pub [Field; 4]) {
    let mut sum: Field = 0;
    for i in 0..4 {
        sum = sum + arr[i];
    }
    assert(sum == 10);
}
