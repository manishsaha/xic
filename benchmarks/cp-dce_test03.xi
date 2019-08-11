use io
use conv

main(args: int[][]) {
  i: int = 0;
  len: int = length(args) + 1;
  ITERATIONS: int = 20000000;

  while (i < ITERATIONS) {
    i = i + 1;

    x: int = len;
    y: int = x * 500;
    z: int = len / (1 + y) * x * y * len;

    a1: int = len / (1 + y) * x * y * len;
    a2: int = a1 * x * a1 - 1000;
    a3: int = a2 * z * y;
    a4: int = x * y * z * a1 * a2 * a3;
  }
  println(unparseInt(i));
}
