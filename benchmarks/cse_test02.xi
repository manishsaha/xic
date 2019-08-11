use io
use conv

main(args:int[][]) {
    c:int = 8
    a:int = 99 / c
    b:int = (a / c) + (a / c) + (a / c) + a * a * a * a
    i:int = 1
    while (i < 20000000) {
        b = (a / c) + (a / c) + (a / c)
        d:int = (a / c) + (a / c) + (a / c)
        e:int = (a / c) + (a / c) + (a / c)
        i = i + 2
    }
    println(unparseInt(i))
    
}