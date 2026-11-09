fn sign(x: i64) -> i64 {
    if x < 0 { -1 } else if x == 0 { 0 } else { 1 }
}

fn count(n: i64) {
    let i = 0;
    while i < n { i = i + 1; }
}

fn main() -> i64 {
    let a = if sign(-5) < 0 { 10 } else { 20 };
    let i = 0;
    while i < 3 { i = i + 1; }
    if i == 3 { a = a + 1; }
    let u: () = while false {};
    count(i);
    a + i
}
