use io
use conv

test(x:int[]) {
  y:int[] = "hello"
  y = x;
  println(y)
}

main(args:int[][]) {
    test("goodbye")
}

