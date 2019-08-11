foo(a:int): int {
  b:int = 10
  while a != b {
    c:int = 4
    while (b != c) {
      d:int = 7
      while c != d | a == b
        b = a
    }
  }
  return a
}