use qt
use io
use conv

main(origArgs: int[][]) {
    app: QApplication, args: int[][] = qapplication(origArgs)
    // a: int = 0

    // println(unparseInt(length(args)));
    // while (a < length(args)) {
    //     println(args[a])
    //     a =  a + 1
    // }

    b: QPushButton = qpushbutton (qs("hi"))
    b.show()
    app.exec()
}
