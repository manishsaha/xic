use io
use conv

main(args:int[][]) {
  x:int[] = {1,2,3,4,5,6}
  i:int = 0;
  while (i < length(x)) {
    println(unparseInt(x[i]))
    i = i + 1
  }
}