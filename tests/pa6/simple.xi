use conv
use io

main(args: int[][]) {
    a: int = 1;
    b: int = a + a;
    if (a < b) {
        c: int = b + 1;
        a = c;
    } else {
        b = b * a + 4;
    }
    d: int = b + 5
    c: int = d;
}
