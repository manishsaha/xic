use io
use conv

is_even(n : int) : bool {
  if n == 0 {
    return true
  } else if n > 0 {
    return is_odd(n - 1)
  } else {
    return is_odd(n + 1)
  }
}

is_odd(n : int) : bool {
  if n == 0 {
    return false
  } else if n > 0 {
    return is_even(n - 1)
  } else {
    return is_even(n + 1)
  }
}

string_of_bool (b : bool) : int[] {
  if b {
    return "true"
  } else {
    return "false"
  }
}

main(args: int[][]) {
  b1:bool = is_even(20);
  b2:bool = is_even(21);
  b3:bool = is_odd(41);
  b4:bool = is_odd(40);

  println(string_of_bool(b1));
  println(string_of_bool(b2));
  println(string_of_bool(b3));
  println(string_of_bool(b4));
}