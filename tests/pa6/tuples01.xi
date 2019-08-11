use io
use conv

helper(x:int,y:int):int {
  return x + y
}

main(args:int[][]) {
  println(unparseInt(helper(5,2)))
}
