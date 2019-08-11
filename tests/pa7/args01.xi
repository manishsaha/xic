use qt
use io

main(args: int[][]) {
    a: int = 0
    while (a < length(args)) {
        println(args[a])
        a =  a + 1
    }
}
