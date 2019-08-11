// Mutually recursive functions to compute if a number is odd or even

is_even(n : int) : bool {
  if (n == 0) {
    return true;
  } else if (n < 0) {
    return !is_odd(n+1);
  } else {
    return !is_odd(n-1);
  }
}

is_odd(n : int) : bool {
  if (n == 0) {
    return false;
  } else if (n < 0) {
    return !is_even(n+1);
  } else {
    return !is_even(n-1);
  }
}
