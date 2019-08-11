use io
use conv
use assert

WINSIZE: int = 256;
plot: int[2][][];

mkMatrix(d: int): int[][] {
    arr: int[d][]
    c: int = 0
    while (c < d) {
        row: int[d]
        arr[c] = row
        c = c + 1
    }
    return arr
}

main(args: int[][]) {
    i: int = 0;
    while (i < 2) {
        buf: int[][] = mkMatrix(WINSIZE+1)
        // buf: int[WINSIZE+1][WINSIZE+1]
        plot[i] = buf
        i = i + 1
    }

    x: int = 0;
    while (x < 2) {
        y: int = 0;
        while (y < WINSIZE+1) {
            z: int = 0;
            while (z < WINSIZE+1) {
                assert(plot[x][y][z] == 0);
                z = z + 1;
            }
            y = y + 1;
        }
        x = x + 1;
    }
}
