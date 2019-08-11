use io
use conv

fact(n : int) : int {
  if (n <= 1) {
    return 1
  } else {
    return n * fact(n-1)
  }
}

main(args: int[][]) {
  res:int = fact(10);
  println(unparseInt(res));
}