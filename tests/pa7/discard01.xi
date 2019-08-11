use io
use conv

f(x: int, y: int) : int, int {
    return x+1, y+1;
}

main(args: int[][]) {
    a1: int, _ = f(100, 200);
    a2: int, _ = f(100, 200);
    a3: int, _ = f(100, 200);
    a4: int, _ = f(100, 200);
    a5: int, _ = f(100, 200);
    // a6: int, _ = f(100, 200);
    // a7: int, _ = f(100, 200);
    // a8: int, _ = f(100, 200);
    // a9: int, _ = f(100, 200);
    println(unparseInt(a1));
    println(unparseInt(a2));
    println(unparseInt(a3));
    println(unparseInt(a4));
    println(unparseInt(a5));
    // println(unparseInt(a6));
    // println(unparseInt(a7));
    // println(unparseInt(a8));
    // println(unparseInt(a9));
}
