use io
use conv

main(args: int[][]) {
  min_int:int = 9223372036854775807 + 1;
  max_int:int = -9223372036854775808 - 1;

  min_int':int = max_int + 1;
  max_int':int = min_int - 1;

  println(unparseInt(min_int));
  println(unparseInt(min_int'));
  println(unparseInt(max_int));
  println(unparseInt(max_int'));
}