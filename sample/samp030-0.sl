import "lib/samp030lib.sl";

fn helper() -> i64 {
    100
}

fn main() -> i64 {
    let a = add(1) + add(2);
    a + LIMIT + *LAST + helper()
}
