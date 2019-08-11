use io
use conv

main(args: int[][]) {
    a: int[5]
    i: int = 0
    while (i < length(a)) {
        a[i] = i
        i=i+ 1
    }
    i = 0
    while (i < length(a)) {
        println(unparseInt(a[f(i)]))
        i=i+ 1
    }
}

f(i: int): int {
    print("Index: ")
    println(unparseInt(i))
    return i
}
