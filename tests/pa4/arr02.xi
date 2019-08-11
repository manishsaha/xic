use io

updateArr(arr:int[]) {
  x:int = 0
  while (x < length(arr)) {
    arr[x] = arr[x] + 2
    x = x + 1
  }
}

main(args:int[][]) {
  x:int[] = {97,98,99};
  updateArr(x);
  println({97,98,99});
  println(x);
}