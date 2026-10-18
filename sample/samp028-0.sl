const N: usize = 5;
const LIMIT: i64 = N as i64 * 10 + 1;
static COUNT: i64 = 0;
static TABLE: [i64; 5] = [1, 2, 3, 4, 5];

fn bump() {
    COUNT = COUNT + 1;
}

fn main() -> i64 {
    let i = 0usize;
    while i < N {
        TABLE[i] = TABLE[i] * 2;
        bump();
        i = i + 1;
    }
    TABLE[4] + COUNT + LIMIT
}
