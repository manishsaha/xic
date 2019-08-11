use io
use conv

add_bad(x:int, y:int) : int {
    z:int
    return x + z
}

main(args: int[][]) {
    a:int = 1
    b:int
    println(unparseInt(add_bad(a,b)))
}
