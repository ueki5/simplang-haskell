fn larger(a: &[i32; 3], b: &[i32; 3]) -> &[i32; 3] {
    if a[0] + a[1] + a[2] > b[0] + b[1] + b[2] {
        return a;
    }
    b
}

fn main() {
    let x = [1i32, 2i32, 3i32];
    let y = [4i32, 0i32, 1i32];
    larger(&x, &y)[2]
}
