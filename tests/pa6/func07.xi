use io
use conv

main(args: int[][]) {
    a: int = 10;
    b: int = a - 1; // 9
    c: int = b - 1; // 8
    d: int = c - 1; // 7
    e: int = d - 1; // 6
    f: int = e - 1; // 5
    g: int = f - 1; // 4
    h: int = g - 1; // 3
    i: int = h - 1; // 2
    j: int = i - 1; // 1
    res: int = fn(a, b, c, d, e, f, g, h, i, j);
    println(unparseInt(res));
}

fn(a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int, i:int, j:int) : int {
    return a - b - c - d - e - f - g - h - i - j;
}
