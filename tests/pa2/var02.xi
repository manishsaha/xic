bar() {
  _ = 5;
  _, _ = ratadd(x, y);
  x:int,  _,        y:int  = foo(baz);
  x:int,  y:bool[], _      = foo(baz);
  x:bool, y:int,    z:bool = foo(baz);
  _,      _,        _      = foo(baz);
}
