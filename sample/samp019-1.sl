fn main() {
    let sq = [0u64; 5];
    let i: usize = 0;
    while i < 5 {
        let v = i as u64;
        sq[i] = v * v;
        i = i + 1;
    }
    sq[4]
}
