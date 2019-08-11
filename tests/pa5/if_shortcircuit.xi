use io
use conv

main(args: int[][]) {
    x: int = 0
    if (lt(0, 0) | lt(0, 0)) x = x + 1
    b: bool = lt(0, -1);
}

lt(x: int, y: int): bool {
    print("lt() called: ")
    print(unparseInt(x))
    print(" < ")
    println(unparseInt(y))
    return x < y
}
