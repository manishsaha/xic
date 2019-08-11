use io
use conv

main(args: int[][]) {
    println(b2s(f(true) == f(true)))
    println(b2s(f(true) == f(false)))
    println(b2s(f(false) == f(true)))
    println(b2s(f(false) == f(false)))
    println(b2s(f(true) != f(true)))
    println(b2s(f(true) != f(false)))
    println(b2s(f(false) != f(true)))
    println(b2s(f(false) != f(false)))
    println(b2s(f(true) & f(true)))
    println(b2s(f(true) & f(false)))
    println(b2s(f(false) & f(true)))
    println(b2s(f(false) & f(false)))
    println(b2s(f(true) | f(true)))
    println(b2s(f(true) | f(false)))
    println(b2s(f(false) | f(true)))
    println(b2s(f(false) | f(false)))
}

f(x: bool): bool {
    print("f() called: ")
    println(b2s(x))
    return x
}

b2s(b: bool): int[] {
    if (b) { return unparseInt(1) } else { return unparseInt(0) }
}
