static CALLS: i64 = 0;

fn digits(a: i64, b: i64, c: i64, d: i64, e: i64, f: i64, g: i64, h: i64) -> i64 {
    CALLS = CALLS + 1;
    (((((((a * 10 + b) * 10 + c) * 10 + d) * 10 + e) * 10 + f) * 10 + g) * 10 + h)
}

fn pick(a: i64, b: i64, c: i64, d: i64, e: i64, f: i64, flag: bool, p: &[i64; 3], i: usize) -> i64 {
    if flag {
        return p[i];
    }
    a + b + c + d + e + f
}

fn main() -> i64 {
    let t = [100, 200, 300];
    digits(1, 2, 3, 4, 5, 6, 7, 8) + pick(1, 2, 3, 4, 5, 6, true, &t, 2) + CALLS
}
