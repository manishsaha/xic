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
    initNotAPoint(r0: int): NotAPoint {
        r = r0;
        return this
    }
}

createObjs(x: int, y: int): Point, NotAPoint {
    return new Point.initPoint(x, y), new NotAPoint.initNotAPoint(x)
}

main(args: int[][]) {
    p1: Point, p2: NotAPoint = createObjs(0, 0);
    valid:bool = p1 == p2;
}
