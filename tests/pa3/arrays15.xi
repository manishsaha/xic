f(): int, int {
  return 1, 2;
}

g() {
  x: bool = {f()} == {5};
}
