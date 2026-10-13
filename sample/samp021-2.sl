fn noop() {}

fn main() -> bool {
    let u = noop();
    let w: () = ();
    let p = &w;
    u == *p
}
