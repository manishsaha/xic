use io
use conv

main(args: int[][]) {
    i: int = 0;
    MAX: int = 60000000;
    
    while (i < MAX) {
        i = i + 2;
        
        x: int = 5000;
        y: int = x * 500;
        if (x < y) {
            x = y + i;
        }
        z: int = 20 / (1 + y) * x * y * 20;
        h: bool = (x * y * z) > 0
        f: int = z + x / 3000 / i;
        z = z + f / (1 + i) * (f + f)
    }
    println(unparseInt(i));
}