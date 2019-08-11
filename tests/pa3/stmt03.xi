foo(x:int) : int, int[], bool {
  return x, {x}, false
}

bar() {
  _,      _,        _       = foo(0);
  x1:int, _,        _       = foo(0);
  _,      y1:int[], _       = foo(0);
  _,      _,        z1:bool = foo(0);
  x2:int, y2:int[], _       = foo(0);
  x3:int, _,        z2:bool = foo(0);
  _,      y3:int[], z3:bool = foo(0);
  x4:int, y4:int[], z4:bool = foo(0);
}
