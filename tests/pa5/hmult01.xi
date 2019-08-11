use io
use conv

main(args: int[][]) {
  magic:int = 1234567890123456789;
  max_int : int = 9223372036854775807;

  // Should print 617283945061728394
  println(unparseInt(max_int *>> magic));
}