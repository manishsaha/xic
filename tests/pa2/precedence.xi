foo() {
  x:int = -2 * 3 + 4;
  y:bool = -2 * 3 + 4 < 10 == true;
  z:bool = true == 10 > 4 + 3 * -2;

  // Be careful about how this parses
  a:bool = false & true == false & 10 > 4 + 3 * -2 | true;

  b:bool = 3 - -4 * 5 + 2 / 3 * 7
}