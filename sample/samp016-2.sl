fn main() -> i32 {
    let a = 1i64;
    let b = &a;
    let c:i32 = *b as i32;
    c
}
