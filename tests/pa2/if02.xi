use LetsTestMoreIfs
// dangling else
test(a:bool, b:bool) : int {
    count: int = 0;
    if (a) if (b) count = 1 else count = 2
    return count
}
