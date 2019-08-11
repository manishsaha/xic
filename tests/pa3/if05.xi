main(b:int) : int {//remove guard parens
  a:int[] = {4,5,6}
  if a[0] == a[1]
    a[2] = 7
  else if a[1] == a[2]
    a[1] = 8
  else
    a[0] = 9
  return a[b]
}
