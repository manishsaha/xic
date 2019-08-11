use io
use conv

main(args:int[][]) {
    x:int = 5;
    y:int = 6;
    z:int = 10;
    a:int = 15
    z = a;
    y = z;
    x = y;
    ret:int = x + 35
    println(unparseInt(ret))
}
