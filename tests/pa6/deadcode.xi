f(arr: int[]) : int {
    arr[0] = 1111;
    return 9999;
}

main(args:int[][]) {
    a:int = 15
    b:int = a + 1;
    c:int[] = {b};
    d:int = f(c)

    if (a == 1) {
        b = 1
    } else {
        b = 0
    }
}
