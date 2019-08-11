use io
use conv

main(args:int[][]) {
  x:int = 5;
  y:int = 6;
  z:int = 7;
  z = x
  while (z < 25) z = z + 1
  test:int = z + 5
  println(unparseInt(test))
}