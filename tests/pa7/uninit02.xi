use io
use conv

main(args: int[][]) {
    x: int;
    y: int = 5;
    if (y < length(args)) {
        x = y + 1;
    } else {
        y = 1;
    }
    println(unparseInt(x));
}
