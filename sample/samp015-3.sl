fn g(x) { x }
fn f(a, b) { a + b }
let y: i32 = 3;
f(g(y as i64), 10)
