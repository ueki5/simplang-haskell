fn sum(p: &[i64; 4]) -> i64 {
    let s = 0;
    let i: usize = 0;
    while i < 4 {
        s = s + p[i];
        i = i + 1;
    }
    s
}

fn main() -> i64 {
    let a = [10, 20, 30, 40];
    sum(&a)
}
