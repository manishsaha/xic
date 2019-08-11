use io
use conv

foo() : int[] {
  while (false) {
    return "1";
  }

  while (0 > 1) {
    return "2";
  }

  if (1 + 2 + 3 + 4 == 0) {
    return "3";
  }

  if (4*10 + 2 == 42) {
    if (false) {
      if (false) {
        if (false) {
          return "4";
        }
        return "5";
      }
      return "6";
    }
    return "7";
  }

  return "-1";
}

main(args: int[][]) {
  println(foo());
}