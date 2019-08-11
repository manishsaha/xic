use io
use conv

foo():int {
  return 2;
}

main(args:int[][]) {
  x:int = 5;
  y:int = foo() * x;
  println(unparseInt(y));
}
