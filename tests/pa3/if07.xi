main(b:int) : int {
  a:int[] = {4,5,6}
  if a[0] == a[1]
    c:int = 5
  else if a[1] == a[2]
    c:int = 7
  else
    c:int = 9
  return c //should fail to find
}