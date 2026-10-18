const LEN: usize = 4;
const UNSET: i32 = -1;
static MEMO: [i32; 4] = [UNSET; 4];
static CALLS: u32 = 0;

fn fib(n: usize) -> i32 {
    CALLS = CALLS + 1u32;
    if n < 2 {
        return n as i32;
    }
    if n < LEN {
        if MEMO[n] != UNSET {
            return MEMO[n];
        }
    }
    let v = fib(n - 1) + fib(n - 2);
    if n < LEN {
        MEMO[n] = v;
    }
    v
}

fn reset(p: &[i32; 4]) {
    let i = 0usize;
    while i < LEN {
        p[i] = UNSET;
        i = i + 1;
    }
}

fn main() -> u32 {
    let a = fib(10);
    reset(&MEMO);
    CALLS = 0u32;
    let b = fib(3);
    CALLS + (a + b) as u32
}
