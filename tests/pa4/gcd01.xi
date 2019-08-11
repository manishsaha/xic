use io
use conv

gcd(a:int, b:int):int {
    while (a != 0) {
        if (a < b) {
            b = b - a
        } else {
            a = a - b
        }
    }
    return b
}

main(args:int[][]) {
    println(unparseInt(gcd(21, 14)));
}
