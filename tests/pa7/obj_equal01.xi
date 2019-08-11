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

class NotAPoint {
    r:int
    y:Point
    z:Point
    initNotAPoint(r0: int): NotAPoint {
        r = r0;
        y = new Point.initPoint(r0, r0);
        z = new Point.initPoint(0, 0);
        return this
    }
    failMethod(): bool {
        return y == z;
    }
}

createPoint(x: int, y: int): Point {
    return new Point.initPoint(x, y)
}

main(args: int[][]) {
    p1: Point = createPoint(0, 0);
}
