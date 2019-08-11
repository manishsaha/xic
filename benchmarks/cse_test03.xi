use io
use conv

main(args:int[][]) {
    c:int = 8
    a:int = 99 / c
    b:int = (99 / c) + (99 / c) + (99 / c) + a * a * a * a
    i:int = 0
    while (i < 10000000) {
        b = (99 / c) + (99 / c) + (99 / c) + a * a * a * a
        d:int = (99 / c) + (99 / c) + (99 / c) + a * a * a * a
        g:int = (99 / c) + (99 / c) + (99 / c) + a * a * a * a
        f:int = (99 / c) + (99 / c) + (99 / c) + a * a * a * a
        xx:int = (99 / c) + (99 / c) + (99 / c) + a * a * a * a
        i = i + 1
    }
    println(unparseInt(i))
    
} 