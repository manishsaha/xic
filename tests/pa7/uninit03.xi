use io
use conv

main(args: int[][]) {
    x: int;
    y: int = 5;
    z: int;
    if (y < length(args)) {
        x = y + 1;
    } else {
        y = 1 + z + x;
    }
    println(unparseInt(x));
}
