use io
use conv

main(args: int[][]) {
    i: int = 0;
    g: int = 30000000;
    
    while (i < g) {
        i = i + 1;
        
        x: int = 5000;
        y: int = x * 500;
        z: int = 20 / (1 + y) * x * y * 20;
        h: bool = (x * y * z) > 0
        f: int = z + x / 3000 / i;
    }
    println(unparseInt(i));
}