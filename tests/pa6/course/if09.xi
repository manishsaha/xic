use io
use conv

main(args: int[][]) {
    if (lt(0, 0) | lt(0, 0)) println("true")
    if (lt(0, 0) | lt(0, -1)) println("true")
    if (lt(0, 0) | lt(0, 1)) println("true")
    if (lt(0, -1) | lt(0, 0)) println("true")
    if (lt(0, -1) | lt(0, -1)) println("true")
    if (lt(0, -1) | lt(0, 1)) println("true")
    if (lt(0, 1) | lt(0, 0)) println("true")
    if (lt(0, 1) | lt(0, -1)) println("true")
    if (lt(0, 1) | lt(0, 1)) println("true")
    println("printed")
}

lt(x: int, y: int): bool {
    print("lt() called: ")
    print(unparseInt(x))
    print(" < ")
    println(unparseInt(y))
    return x < y
}
