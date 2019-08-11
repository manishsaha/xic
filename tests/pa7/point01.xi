
use io
use conv

class Point { // a mutable point
    x,y: int

    move(dx: int, dy: int) {
      x = x + dx
      y = y + dy
    }
    coords() : int, int {
      return x, y
    }
    add(p: Point) : Point {
      return createPoint(x + p.x, y + p.y)
    }
    initPoint(x0: int, y0: int): Point {
        x = x0
        y = y0
        return this
    }
    clone(): Point { return createPoint(x, y) }
    equals(p: Point): bool { return this == p }
    toString(): int[] {
        str:int[] = "Point at x = " + unparseInt(x) + " and y = " + unparseInt(y)
        return str
    }
}

createPoint(x: int, y: int): Point {
    return new Point.initPoint(x, y)
}

main(args: int[][]) {
    println("Starting point/coloredpoint example");

    p1: Point = createPoint(100, 200);
    println(p1.toString());
    println("Should have printed \"Point at x = 100 and y = 200\"");

    p2: Point = p1.clone();
    p2.move(5, 5);
    println(p1.toString());
    println("Should have printed \"Point at x = 100 and y = 200\"");
    println(p2.toString());
    println("Should have printed \"Point at x = 105 and y = 205\"");
}
