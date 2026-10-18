static COUNT: i64 = 0;
static TABLE: [i64; 4] = [1, 2, 3, 4];
static P: &i64 = &COUNT;
static Q: &[i64; 4] = &TABLE;
static R: &i64 = &TABLE[2];
static PS: [&i64; 2] = [&COUNT, &TABLE[0]];
static PP: &&i64 = &P;

fn main() -> i64 {
    *P = 5;
    Q[1] + *R + **PP
}
