let x: i64 = 10;
let y: i64 = 20;
let ps: [&i64; 2] = [&x, &y];
let flags = [true, false];
y = 30;
if flags[0] {
    x = *ps[0] + *ps[1];
}
x
