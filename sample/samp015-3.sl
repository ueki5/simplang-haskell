fn g(x: i64) -> i64 { x }

fn f(a: i64, b: i64) -> i64 { a + b }

fn main() -> i64 {
    let y: i32 = 3;
    f(g(y as i64), 10)
}
