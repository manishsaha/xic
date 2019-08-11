use LetsTestSomeIfs

test(a:bool, b:bool, aa:bool) : int {
    count: int = 0;
    if (a & b) count = 1 else if (aa & b) count = 2 else count = 3
    return count
}