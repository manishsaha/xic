use io
use conv

main(args: int[][]) {
    a: int[] = {1111, 2222, 3333}
    x: int = a[f(0)]
}

f(i: int): int {
    print("Index: ")
    println(unparseInt(i))
    return i
}
