class Point {
  x: int
  init(x0: int) : Point {
    x = x0;
    return this;
  }
  eq(p1: Point, p2: Point) : bool {
    return p1 == p2;
  }
}
