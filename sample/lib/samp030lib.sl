const BASE: i64 = 10;
pub const LIMIT: i64 = BASE * 2;
static CALLS: i64 = 0;
pub static LAST: &i64 = &CALLS;

fn helper(x: i64) -> i64 {
    CALLS = CALLS + 1;
    x + BASE
}

pub fn add(x: i64) -> i64 {
    helper(x)
}
