import "samp024math.sl";

pub fn sum(p: &[i64; 5]) -> i64 {
    let total = 0;
    let i: usize = 0;
    while i < 5 {
        total = total + p[i];
        i = i + 1;
    }
    total
}

pub fn sq(x: i64) -> i64 { mul(x, x) }
