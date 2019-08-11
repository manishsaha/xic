use io
use conv

main(args:int[][]) {
  s:int[] = "Hello world"
  i:int = 0
  c:int = 'B'
  while (i < length(s)) {
    s[i] = s[i] + 1
    i = i + 1
  }
  d:int = c / 2
  println(unparseInt(length(s)))
  println(s)
  println({d})
}
