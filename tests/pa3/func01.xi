proc1() {
  return 
}

proc2() {

}

func1(): int {
  return func1()
}

func2(x: int): bool, bool {
  return x == 5, true;
}

main() {
  y: bool, _ = func2(func1()) 
}
