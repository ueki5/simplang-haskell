fn f(x: &i64) -> &i64 { x }

fn main() -> &i64 {
    let a:i64 = 4;
    f(&a)
}
