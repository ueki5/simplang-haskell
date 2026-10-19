fn sum(s: &[i64]) -> i64 {
    let t = 0;
    let i = 0usize;
    while i < s.len() {
        t = t + s[i];
        i = i + 1;
    }
    t
}

fn longer(a: &[i64], b: &[i64]) -> &[i64] {
    if a.len() >= b.len() {
        return a;
    }
    b
}

fn main() -> i64 {
    let x = [1, 2, 3];
    let y = [10, 20, 30, 40, 50];
    let s = longer(&x, &y);
    sum(&x) + sum(s) + s.len() as i64
}
