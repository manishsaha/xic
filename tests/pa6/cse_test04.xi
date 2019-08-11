use io
use conv

main(args:int[][]) {
    c:int = 8
    a:int = 99 / c
    b:int = (99 / c) + (99 / c) + (99 / c) + a * a * a * a
    i:int = 1
    while (i < 10000000) {
        b = b + i + (9 / c) + (9 / c) + (9 / c) + a * a * a * a
        d:int = ((9 / c) + (9 / c) + (9 / c)) * ((9 / c) + (9 / c) + (9 / c))
        i = i + 1
    }
    println(unparseInt(b))

}