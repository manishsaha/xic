use io
use conv

class Point { // a mutable point
    x,y: int
    initPoint(x0: int, y0: int): Point {
        x = x0
        y = y0
        return this
    }
}

createPoint(x: int, y: int): Point {
    return new Point.initPoint(x, y)
}

main(args: int[][]) {
    p1: Point = createPoint(100, 200);
}
