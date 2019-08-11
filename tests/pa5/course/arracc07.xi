use io
use conv

main(args: int[][]) {
    a: int[5]
    i: int = 0
    while (i < length(a)) {
        a[i] = i - 1
        i = i + 1
    }
    i = 0
    while (i < length(a)) {
        println(unparseInt(a[a[-(f(a, i) - f(a, i))]]))
        j: int = 0
        while (j < length(a)) {
            println(unparseInt(a[j]))
            j = j + 1
        }
        i = i + 1
    }
}

f(a: int[], i: int): int {
    print("Index: ")
    println(unparseInt(i))
    a[i] = a[i] + 1
    return a[i]
}
