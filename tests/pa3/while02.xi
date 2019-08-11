c(a:int): int {
  while a < 10
    a = a + 1
  return a
}

d(): bool {
  b:int = 9
  while (b > 1) {
    b = b - c(b)
  }
  return true
}