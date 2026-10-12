fn add(a, b) {
a + b
}

fn fact(n) {
if n <= 1 {
return 1;
}
n * fact(n - 1)
}

fn id(x) {
x
}

fn main() {
    add(fact(5i64), id(3i64))
}
