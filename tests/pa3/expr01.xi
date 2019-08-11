f() {
  x:int;
  y:bool;

  x = 5;
  x = x;
  x = x + x;
  x = 5 * 3;
  x = -x;
  x = 4 / 1;
  x = 0 % 10;
  x = length({0, 1});
  x = {0, 1}[0];
  x = {0, -5, length({true, false})}[1 * 3 % 2]
  
  y = true;
  y = y;
  y = x == 3;
  y = x != 4;
  y = x < 5;
  y = x <= 6;
  y = x > 2;
  y = x >= 1;
  y = true == false;
  y = false != true;
  y = y & y;
  y = y | y;
  y = {1, 2, 3} == {4, 5};
  y = {6, 7} != {6, 7};
  y = !!false;
  y = !(x > 3 | (y == true & x != {1, 2}[0]));
}
