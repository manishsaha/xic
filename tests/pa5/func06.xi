use io
use conv

main(args: int[][]) {
    a:int, b:int, c:int, d:int, e:int, f:int, g:int, h:int, i:int = fn();

    println(unparseInt(a));
    println(unparseInt(b));
    println(unparseInt(c));
    println(unparseInt(d));
    println(unparseInt(e));
    println(unparseInt(f));
    println(unparseInt(g));
    println(unparseInt(h));
    println(unparseInt(i));
}

fn() : int, int, int, int, int, int, int, int, int {
    return 1, 2, 3, 4, 5, 6, 7, 8, 9;
}
