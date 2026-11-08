fn sum(ptr: &[i64; 3]) -> i64 {
  let i: i64 = 0;
  let sum: i64 = 0;
  while i as usize < ptr.len() {
    sum = sum + ptr[i as usize];
    i = i + 1;
  }
  sum
}
fn main() -> i64 {
  let ary: [i64; 3] = [0, 1, 2];
  let ret = sum(&ary);
  ret
}
