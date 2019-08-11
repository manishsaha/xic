use io
use conv

f() : int {
  return 1;
}

g() : int {
  return 2;
}

string_of_bool (b : bool) : int[] {
  if b {
    return "true";
  } else {
    return "false";
  }
}

main(args: int[][]) {
  b1:bool = true;
  b2:bool = false;
  b3:bool = b1 & b2;
  b4:bool = b1 | b2;
  b5:bool = b1 | b2 & b3;

  x:int = 1;
  y:int = 2;
  b6:bool = 1 == 2;
  b7:bool = 1 != 2;
  b8:bool = f() == f();
  b9:bool = f() == g();
  b10:bool = f() != f();
  b11:bool = f() != g();
  b12:bool = f() <  f();
  b13:bool = f() <  g();
  b14:bool = f() <= f();
  b15:bool = f() <= g();
  b16:bool = f() > f();
  b17:bool = f() >  g();
  b18:bool = f() >= f();
  b19:bool = f() >=  g();
  z:int = (f() + g()) - (f() * x + (1 / g()));

  println(string_of_bool(b1));
  println(string_of_bool(b2));
  println(string_of_bool(b3));
  println(string_of_bool(b4));
  println(string_of_bool(b5));
  println(string_of_bool(b6));
  println(string_of_bool(b7));
  println(string_of_bool(b8));
  println(string_of_bool(b9));
  println(string_of_bool(b10));
  println(string_of_bool(b11));
  println(string_of_bool(b12));
  println(string_of_bool(b13));
  println(string_of_bool(b14));
  println(string_of_bool(b15));
  println(string_of_bool(b16));
  println(string_of_bool(b17));
  println(string_of_bool(b18));
  println(string_of_bool(b19));

  println(unparseInt(x));
  println(unparseInt(y));
  println(unparseInt(z));
}