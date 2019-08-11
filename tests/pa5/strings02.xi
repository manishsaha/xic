use io

helper(x:int[]):int[] {
  return x + {97,100};
}

main(args:int[][]) {
  x:int[] = "";
  y:int = 0;
  while (y < 5) {
    x = helper(x + "test")
    y = y + 1
  }
  println(x)
}