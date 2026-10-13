fn add(a: i64, b: i64) -> i64 {
a + b
}

fn fact(n: i64) -> i64 {
if n <= 1 {
return 1;
}
n * fact(n - 1)
}

fn id(x: i64) -> i64 {
x
}

fn main() -> i64 {
    add(fact(5i64), id(3i64))
}
