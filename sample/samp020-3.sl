fn at(p: &[i64; 3], i: usize) -> &i64 {
    &p[i]
}

fn main() {
    let a = [1, 2, 3];
    let q = at(&a, 1);
    *q = 100;
    a[0] + a[1] + a[2]
}
