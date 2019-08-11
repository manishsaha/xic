use qt

bg: QButtonGroup // global to prevent GC

main(args: int[][]) {
    app: QApplication, _ = qapplication(args)

    // We use it modeless here
    d: QDialog = qdialog()
    v: QVBoxLayout = qvboxLayout()
    d.setLayout(v)

    // Add a slider and a buddy label
    l1: QLabel  = qlabel (qs("Value"))
    sl: QSlider = qslider()
    l1.setBuddy(sl)
    sl.setOrientation(Horizontal())
    sl.setRange(0, 100)
    sl.setTickInterval(10)
    sl.setTickPosition(TicksAbove())

    hrow1:QHBoxLayout = qhboxLayout()
    hrow1.addWidget(l1)
    hrow1.addWidget(sl)
    v.addLayout(hrow1)

    // A a checkbox.
    cb: QCheckBox = qcheckbox (qs("Checkbox"))
    v.addWidget(cb)

    // Some radios... We add them to a QButtonGroup, though we don't have to -
    // they'd be grouped automatically due to same parent.
    aStr: QString = qs("A")
    bg = qbuttongroup()

    i: int = 0
    while (i < 5) {
        label: QString = aStr.plus (qsNum (i + 1))
        r: QRadioButton = qradio(label)
        v.addWidget(r)
        bg.addButton(r)
        i = i + 1
    }

    // Some more, inside a QGroupBox
    group: QGroupBox = qgroupbox(qs("Group"))
    v.addWidget(group)

	vg: QVBoxLayout = qvboxLayout()
    group.setLayout(vg)
    group.setFlat(true)

    bStr: QString = qs("B")
    i = 0
    while (i < 5) {
        label: QString = bStr.plus (qsNum (i + 1))
        r: QRadioButton = qradio(label)
        vg.addWidget(r)
        i = i + 1
    }

    // QLineEdit + a buddy.
    l2: QLabel    = qlabel (qs("Value"))
    le: QLineEdit = qlineedit()
    l2.setBuddy(le)

    hrow2:QHBoxLayout = qhboxLayout()
    hrow2.addWidget(l2)
    hrow2.addWidget(le)
    v.addLayout(hrow2)

    // A QTextEdit
    t: QTextEdit = qtextedit()
    v.addWidget(t)

    // A couple of buttons, right-aligned
    hrow3: QHBoxLayout = qhboxLayout()
    hrow3.addStretch()

    ok: QPushButton = qpushbutton(qs("Ok"))
    ok.setIcon(qiconStandard (DialogOkButton()))
    hrow3.addWidget(ok)

    cancel: QPushButton = qpushbutton(qs("Cancel"))
    cancel.setIcon(qiconStandard (DialogCancelButton()))
    hrow3.addWidget(cancel)

    v.addLayout(hrow3)

    d.show()
    app.exec()
}

