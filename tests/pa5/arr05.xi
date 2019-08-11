use io

main(args:int[][]) {
  x:int[][] = {{50,51},{52,53},{54,55,56}};
  i:int = 0;
  while (i < length(x)) {
    j:int = 0;
    while j < length(x[i]) {
      x[i][j] = x[i][j] * 2
      j = j + 1
    }
    i = i + 1
  }
  k:int = 0;
  while (k < length(x)) {
    println(x[k])
    k = k + 1
  }
}