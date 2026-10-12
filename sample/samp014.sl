fn main() {
    let a = 5;
    let b = 3i32;
    let c: i32 = 2;
    let p = &c;
    a + (b + c) as i64 + *p as i64
}
