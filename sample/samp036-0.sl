fn main() -> i64 {
    let x = 3;
    let y = {
        let t = x + 1;
        t * t
    };
    y + { let z = 2; z * 10 }
}
