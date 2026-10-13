fn set_positive(p: &i64, v: i64) -> () {
    if v < 0 {
        return;
    }
    *p = v;
}

fn main() -> i64 {
    let x = 1;
    set_positive(&x, 5);
    set_positive(&x, -1);
    x
}
