use io
use conv

main(args : int[][]) {
  eqb_test1:int[] = eqb(100, -100)
  eqb_test2:int[] = eqb(-100, -100)
  println(eqb_test1);
  println(eqb_test2);
}

eqb (a: int, b: int) : int[] {
  a_str:int[] = unparseInt(a);
  b_str:int[] = unparseInt(b);
  return a_str + " =? " + b_str + " = " + (string_of_bool (a == b));
}

string_of_bool (b : bool) : int[] {
  if b {
    return "true"
  } else {
    return "false"
  }
}