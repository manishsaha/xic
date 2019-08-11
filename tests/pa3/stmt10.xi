foo() : int, int {
  return 0, 0;
}

bar() {
  x:int, y:int = foo();
  y = 5;
  x = 5;
}
