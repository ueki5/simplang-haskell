let a:i64=0;
{
  let b:i64=1;
  a=b;
  {
    let c:i64=2;
    a=c;
  }
}
a
