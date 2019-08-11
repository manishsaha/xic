// A fancier bouncing ball demo, with menus and such
use qt;

// Dimension of the arena, and radius of the object.
DIM:int = 556;
R:int   = 16;

// position of the object.
x:int = 100;
y:int = 50;
// directions
dx:int = 2;
dy:int = 1;

mainWidget: QWidget;

// we use a pixmap for double-buffering. Actually, Qt will do it
// for us, but I test more things this way!
backBuffer: QPixmap;

ballRect(): QRect {
    return qrect(x - R, y - R, R*2, R*2);
}

class BallWidget extends QWidget {
    paintEvent(pe: QPaintEvent) {
        // draw scene on backBuffer
        backBuffer.fill(qcolor(255, 255, 192));

        p: QPainter = qpainter(backBuffer);
        p.setHighQuality(true);

        pen: QPen = qpen (qcolor(0, 0, 255));
        pen.setWidth(5);
        p.setPen(pen);
        p.setBrush (qbrush (qcolor (255, 0, 0)));
        p.drawEllipse (ballRect());
        p.end();

        // paint the backbuffer
        dirty: QRect = pe.rect();
        pw: QPainter = qpainter(this);
        pw.drawPixmapPortion(dirty.topLeft(), backBuffer, dirty);
        pw.end();
    }
}

class AnimationTimer extends TimerListener {
    timeout(timerObj: QTimer) {
        oldRect: QRect = ballRect();

        x = x + dx;
        y = y + dy;

        if (x + R >= DIM | (x - R)<= 0)
            dx = -dx;

        if (y + R >= DIM | (y - R) <= 0)
            dy = -dy;

        newRect: QRect = ballRect();
        both:    QRect = newRect.united(oldRect);

        mainWidget.repaint(both.adjusted(-5, -5, 5, 5));
    }
}

timer: QTimer;
play:  QAction;
stop:  QAction;

class PlaybackController extends ActionListener {
    triggered(a: QAction) {
        if (a.text().equals(play.text())) {
            play.setEnabled(false);
            stop.setEnabled(true);
            timer.start();
        } else {
            play.setEnabled(true);
            stop.setEnabled(false);
            timer.stop();
        }
    }
}

main(args: int[][]) {
    qapp: QApplication, _ = qapplication(args);

    // Create the main window.
    mw: QMainWindow = qmainwindow();

    // Create toolbar
    tb: QToolBar = qtoolbar();
    tb.setToolButtonStyle(ToolButtonTextBesideIcon());
    mw.addToolBar(tb);

    // Create menu, add it.
    menu: QMenu = qmenu(qs("Playback"));
    mw.menuBar().addMenu(menu);

    // Actions for controlling animation
    play = qaction (qs("Play"));
    play.setIcon(qiconStandard(MediaPlay()));
    play.setEnabled(false);

    stop = qaction (qs("Stop"));
    stop.setIcon(qiconStandard (MediaStop()));

    // Hook them up to our listener
    al: PlaybackController = new PlaybackController;
    play.addActionListener(al);
    stop.addActionListener(al);

    // Add actions to toolbar, menu
    tb.addAction(play);
    tb.addAction(stop);
    menu.addAction(play);
    menu.addAction(stop);

    mainWidget = new BallWidget;
    mainWidget.setFixedSize(qsize(DIM, DIM));
    backBuffer = qpixmap(DIM, DIM);

    // create animation timer.
    timer = qtimer();

    // make our listener.
    timer.addTimerListener(new AnimationTimer);

    // start the timer..
    timer.setSingleShot(false);
    timer.setInterval(40);
    timer.start();

    mw.setCentralWidget(mainWidget);
    mw.show();
    qapp.exec();
}

