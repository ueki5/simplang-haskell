fn fill_squares(p: &[u64; 5]) -> u64 {
    let i: usize = 0;
    while i < 5 {
        let v = i as u64;
        p[i] = v * v;
        i = i + 1;
    }
    0u64
}
let sq = [0u64; 5];
let d = fill_squares(&sq);
sq[3] + sq[4]
