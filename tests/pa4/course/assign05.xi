use io
use conv

main(args: int[][]) {
    a: int[3]
    a[f(0)] = 1
    a[f(1)] = 2
    a[f(2)] = 3
    println(unparseInt(length(a)))
    println(unparseInt(a[1]))
}

f(x: int): int {
    print("f() called: ")
    println(unparseInt(x))
    return x
}
