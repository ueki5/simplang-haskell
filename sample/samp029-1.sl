const LEN: usize = 3;
static BUF_A: [i32; 3] = [0; 3];
static BUF_B: [i32; 3] = [0; 3];
static FRONT: &[i32; 3] = &BUF_A;
static BACK: &[i32; 3] = &BUF_B;

fn swap() {
    let t = FRONT;
    FRONT = BACK;
    BACK = t;
}

fn step() {
    let i = 0usize;
    while i < LEN {
        BACK[i] = FRONT[i] * 2 + i as i32;
        i = i + 1;
    }
    swap();
}

fn main() -> i32 {
    FRONT[0] = 1;
    FRONT[1] = 2;
    FRONT[2] = 3;
    step();
    step();
    FRONT[0] * 100 + FRONT[1] * 10 + FRONT[2]
}
