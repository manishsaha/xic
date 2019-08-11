use io
use conv
use assert

LEN: int = 256;
colors: Color[LEN];

class Color {
    r,g,b: int
    init(r0: int, g0: int, b0: int) : Color {
        r = r0; g = g0; b = b0;
        return this
    }

    printColor() {
        print("(");
        print(unparseInt(r)); print(", ");
        print(unparseInt(g)); print(", ");
        print(unparseInt(b));
        println(")");
    }
}

main(args: int[][]) {
    i: int = 0;
    while (i < LEN) {
        colors[i] = new Color.init(i, i, i);
        i = i + 1;
    }

    i = 0;
    while (i < LEN) {
        // colors[i].printColor();
        assert(colors[i].r == i);
        assert(colors[i].g == i);
        assert(colors[i].b == i);
        i = i + 1;
    }
}
