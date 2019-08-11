use io
use conv

main(args: int[][]) {
    x: bool[] = {true, false}
    println(b2s(f(x, true) == f(x, true)))
    println(b2s(f(x, true) == f(x, false)))
    println(b2s(f(x, false) == f(x, true)))
    println(b2s(f(x, false) == f(x, false)))
    println(b2s(f(x, true) != f(x, true)))
    println(b2s(f(x, true) != f(x, false)))
    println(b2s(f(x, false) != f(x, true)))
    println(b2s(f(x, false) != f(x, false)))
    println(b2s(f(x, true) & f(x, true)))
    println(b2s(f(x, true) & f(x, false)))
    println(b2s(f(x, false) & f(x, true)))
    println(b2s(f(x, false) & f(x, false)))
    println(b2s(f(x, true) | f(x, true)))
    println(b2s(f(x, true) | f(x, false)))
    println(b2s(f(x, false) | f(x, true)))
    println(b2s(f(x, false) | f(x, false)))
}

f(x: bool[], y: bool): bool {
    print("f() called: ")
    println(b2s(x[0]))
    println(b2s(y))
    x[0] = !x[0]
    return x[0]
}

b2s(b: bool): int[] {
    if (b) { return unparseInt(1) } else { return unparseInt(0) }
}
