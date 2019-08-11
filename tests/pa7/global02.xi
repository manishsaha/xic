use io
use conv

C: int = inc(5);
D: int = inc(C);

inc(x: int) : int {
    return x + 1;
}

main(args: int[][]) {
    println(unparseInt(C));
    println(unparseInt(D));
}
