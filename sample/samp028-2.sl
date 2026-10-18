import "lib/samp028lib.sl";

const TIMES: i64 = 4;

fn main() -> i64 {
    let i = 0;
    while i < TIMES {
        add_step();
        i = i + 1;
    }
    TOTAL = TOTAL * STEP;
    TOTAL
}
