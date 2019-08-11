use io

callee(x:int):int {
  if (x % 2 == 0) {
    return x
  }
  else {
    return x + 1
  }
}

main(args:int[][]) {
  x:int = 0;
  while (x < 10) {
    if (callee(x) % 2 == 0)
      print("even number\n")
    else print("odd number\n")
    x = x + 1;
  }
}