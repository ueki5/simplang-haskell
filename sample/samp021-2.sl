fn noop() {}

fn main() {
    let u = noop();
    let w: () = ();
    let p = &w;
    u == *p
}
