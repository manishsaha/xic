//This is a program that tests the whole shebang.

use io;
use pigs

return_multiple(): bool, int {
    
    return 1, 2
    
}

return_an_array(): int[][] {
    return {{1, 3}, {2, 4}, {3, 7}};
}

multiple_args(x:int, y:int[][][], z:bool) {

    print("I don't actually do anything!")

}

main(args: int[][]) {

    x:int = 5
    y:bool = false | (true & true);
    
    if (x < 12)
        if (y)
            print("Hello, worl\x64!")
        else if (!y)
            print("Goodbye, cruel world!")
        else
            print("How did I get here?")
    
    while (y) {
        x = x + '1';
        y = x == 12
    }
    
    {
        print("I'm inside a scope!")
        
        z:int[][] = {{2}, {1,}}
        
        q:int[1][z[0][1]][]
        
        return_an_array()[0] = return_an_array()[1];
        
    } 
    
    z:int, _ = return_multiple()
    
    an_array:bool[7]
    
    print(length((an_array)))
    
    x = length(return_an_array()[0])

}

