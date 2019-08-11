use io
use conv

main(args : int[][]) {
  res:int = foo(100, 200);
  println(unparseInt(res))
}

foo (a: int, b: int) : int {
  println("a = " + unparseInt(a));
  println("b = " + unparseInt(b));
  return a + b;
}