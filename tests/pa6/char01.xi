use io
use conv

main(args:int[][]) {
    s:int = '\x025'
    b:int = '\n'
    c:int = '\x02A'
    d:int = '\t'
    e:int = '\\'
    f:int = '\''
    g:int = '\x0A' // same as '\n'
    h:int = '\xAAAA'
    println(unparseInt(s))
    println(unparseInt(b))
    println(unparseInt(c))
    println(unparseInt(d))
    println(unparseInt(e))
    println(unparseInt(f))
    println(unparseInt(g))
    println(unparseInt(h))
}
