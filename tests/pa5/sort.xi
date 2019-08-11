use io
use conv

sort(a: int[]) {
  i:int = 0
  n:int = length(a)
  while i < n {
    j:int = i
    while j > 0 {
      if a[j-1] > a[j] {
        swap:int = a[j]
        a[j] = a[j-1]
        a[j-1] = swap
      }
      j = j-1
    }
    i = i+1
  }
}

main(args: int[][]) {
  arr:int[] = {1, 6, 24, -2, 40, -234, 0};
  sort(arr);

  i:int = 0;
  delim:int[] = "";
  while (i < length(arr)) {
   print(delim + unparseInt(arr[i]));
   delim = ",";
   i = i + 1;
  }
}