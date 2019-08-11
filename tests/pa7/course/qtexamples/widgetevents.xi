use qt;
use io;
use conv;

qapp: QApplication;

class MyButton extends QPushButton {
    keyPressEvent(e: QKeyEvent) {
        println(e.text().XiString());
        defaultEvent(e);
    }

    mouseMoveEvent(e: QMouseEvent) {
        p: QPoint = e.pos();
        print(unparseInt(p.x()));
        print(",");
        println(unparseInt(p.y()));
    }

    clicked() {
        res: int = qmessageBoxQuestion(NO_WIDGET(), qs("Quit?"), qs("Should we quit?"),
                                       ButtonYes() + ButtonNo());
        if (res == ButtonYes())
            qapp.quit();
    }
}

main(args: int[][]) {
    app: QApplication, _ = qapplication(args);
    qapp = app;

    w: QPushButton = new MyButton;
    w.setIcon(qiconStandard(DialogOkButton()));
    w.setFocusPolicy(ClickFocus());
    w.setMouseTracking(true);
    w.show();
    qapp.exec();
}

