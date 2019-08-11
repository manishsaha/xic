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

class Color {
    r,g,b: int
    initColor(r0: int, g0: int, b0: int) : Color {
        r = r0;
        b = b0;
        g = g0;
        return this;
    }
    getRGB() : int, int, int {
        return r, g, b;
    }
    toString(): int[] {
        str:int[] = "Color with r = " + unparseInt(r) + " and g = " + unparseInt(g) + " and b = " + unparseInt(b)
        return str
    }
}

class ColoredPoint extends Point {
    col: Color
    color(): Color { return col }

    initColoredPoint(x0: int, y0: int, c: Color): ColoredPoint {
        col = c
        _ = initPoint(x0, y0)
        return this
    }

    clone(): Point {
        return cloneColoredPoint();
    }

    cloneColoredPoint(): ColoredPoint {
        x0: int, y0:int = coords();
        r:int, g:int, b:int = col.getRGB();
        return createColoredPoint(x0, y0, r, g, b);
    }

    toString(): int[] {
        x0: int, y0:int = coords();
        str:int[] = "ColoredPoint at x = " + unparseInt(x0) + " and y = " + unparseInt(y0) + " and " + col.toString()
        return str
    }
}

createPoint(x: int, y: int): Point {
    return new Point.initPoint(x, y)
}

createColoredPoint(x: int, y: int, r: int, g: int, b: int): ColoredPoint {
    c: Color = new Color.initColor(r, g, b)
    return new ColoredPoint.initColoredPoint(x, y, c)
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

    cp1: ColoredPoint = createColoredPoint(1000, 1000, 255, 128, 0);
    println(cp1.toString());
    println("Should have printed \"ColoredPoint at x = 1000 and y = 2000 and Color with r = 255 and g = 128 and b = 0\"");
    p3: Point = cp1.clone();
    p3.move(5, 5);
    println(cp1.toString());
    println("Should have printed \"ColoredPoint at x = 1000 and y = 2000 and Color with r = 255 and g = 128 and b = 0\"");
    println(p3.toString());
    println("Should have printed \"ColoredPoint at x = 1005 and y = 2005 and Color with r = 255 and g = 128 and b = 0\"");

    p4: Point = p3.add(p2);
    println(p4.toString());
    println("Should have printed \"Point at x = 1110 and y = 2210\"");

    p5: Point = p2.add(cp1);
    println(p5.toString());
    println("Should have printed \"Point at x = 1105 and y = 1205\"");
}
