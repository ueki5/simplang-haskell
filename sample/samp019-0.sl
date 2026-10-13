fn main() -> i32 {
    let a: [i32; 5] = [3, 1, 4, 1, 5];
    let sum: i32 = 0;
    let i: usize = 0;
    while i < 5 {
        sum = sum + a[i];
        i = i + 1;
    }
    sum
}
