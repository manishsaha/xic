use io
use conv

main(args: int[][]) {
    x: int[] = {1, 2, 3, 4}
    a: int[5][4]
    {
        i: int = 0
        while (i < length(a)) {
            j: int = 0
            while (j < length(a[i])) {
                a[i][j] = i + j
                j = j + 1
            }
            i = i + 1
        }
    }
    a[f(x,3)-f(x,2)][f(x,2)-f(x,1)] = 47
    {
        i: int = 0
        while (i < length(a)) {
            j: int = 0
            while (j < length(a[i])) {
                println(unparseInt(a[i][j]))
                j = j + 1
            }
            i = i + 1
        }
    }
}

f(a: int[], i: int): int {
    print("Index: ")
    println(unparseInt(i))
    a[i] = a[i] + 1
    return a[i]
}
