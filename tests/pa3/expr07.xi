f(): int, int {
  return 1, 2
}

g(): int, int {
  return 0, 1
}

h() {
  b: bool = f() == g()
}
