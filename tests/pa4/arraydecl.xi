use io
use conv

main(args: int[][]) {
    a: int[] = {1, 2, 3, 4}
    x: int[f(a,3)-f(a,2)][f(a,2)-f(a,1)][f(a,1)-f(a,0)][][]
}

f(a: int[], i: int): int {
    print("Index: ")
    println(unparseInt(i))
    a[i] = a[i] + 1
    return a[i]
}
