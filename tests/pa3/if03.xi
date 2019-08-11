main(b:int) : bool {//return inside if block
  a:int[] = {4,5,6}
  if (a[0] == a[1]) {
    return a[1] < a[2]
  }
  return b == a[2]
}