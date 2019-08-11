use io

foo():int[] {
  return "hello";
}

main(args:int[][]) {
  x:int[] = {97, 98}
  y:int[] = foo() + x;
  println(y)
}
