use io
use conv

main(args: int[][]) {
    arr: int[] = f();
    i: int = 0;
    while (i < length(arr)) {
        println(unparseInt(arr[i]));
        i = i + 1;
    }
}

f() : int[] {
    return {1, 2, 3};
}
