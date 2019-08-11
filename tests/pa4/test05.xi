use io
use conv

a (i: int, j:int):int,int {
  return i, 2*j
}

b(i: int, j:int):int {
  x:int,y:int = a(i,j);
  return x + 5*y
}

main(args:int[][]) {
  x:int,y:int = a(2,2);
  z:int = b(2,2);
  println(unparseInt(x) + " " + unparseInt(y) + " " + unparseInt(z))
}
