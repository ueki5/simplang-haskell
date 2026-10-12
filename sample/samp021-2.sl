fn noop() {}
let u = noop();
let w: () = ();
let p = &w;
u == *p
