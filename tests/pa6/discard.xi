use io
use conv

tupler(n : int) : int, int {
  if (n % 2 == 0) {
    return n, n-1
  } else {
    return -1, 1
  }
}

difftupler(arr: int[]) : int, int[] {
  return arr[0], arr + " worl\x64!"
} 

main(args: int[][]) {
  _, a:int = tupler(7)
  b:int, c:int = tupler(20)
  _, _ = tupler(9000)
  _, e:int[] = difftupler("\x44isney")
  println(unparseInt(a))
  println(unparseInt(b))
  println(unparseInt(c))
  println(e)
}