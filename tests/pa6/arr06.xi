use io
use conv

helper():int {
  return 5;
}

main(args:int[][]) {
  x:int = helper()
  arr:int[x][x][x];
  i:int = 0;
  while (i < length(arr)) {
    j:int = 0;
    while (j < length(arr[i])) {
      k:int = 0;
      while (k < length(arr[i][j])) {
        print(unparseInt(arr[i][j][k]));
        k = k + 1;
      }
      j = j + 1;
    }
    i = i + 1;
  }
}