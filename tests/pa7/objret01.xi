use io
use conv

f() {
    println("Hello world");
}

class Point {
    x: int;
    init(x0: int) : Point {
        x = x0;
        f();
        return this;
    }
}

main(args: int[][]) {
    p: Point = new Point.init(1);
    println(unparseInt(p.x));
}
