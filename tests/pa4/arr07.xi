use io
use conv

string_of_bool(b : bool) : int[] {
  if b { return "true" } else { return "false" }
}

main(args : int[][]) {
  x:int[] = {1, 2, 3};
  y:int[] = x;
  z:int[] = {1, 2, 3};
  println(string_of_bool(x == y));
  println(string_of_bool(x == z));
  println(string_of_bool(y == z));
  println(string_of_bool(x != y));
  println(string_of_bool(x != z));
  println(string_of_bool(y != z));
}