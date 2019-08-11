class Object {
}

class A extends Object {
}

class B extends A {
}

class C extends Object {
}

main(args: int[][]) {
    arr: Object[] = {new A, new B, new C};
}