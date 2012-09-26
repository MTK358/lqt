Overview
========

A (currently partially working) rewrite of the generator code for lqt.

Building lqt
------------

Run these commands:

    mkdir build
    cd build
    cmake ..
    make

After running these commands, the finished modules should be available in `build/lib`.

Usage
-----

    local Qt = require 'QtCore'

    local app = Qt.QCoreApplication(arg)
    
    local str = Qt.QString.number(15)
    print(str:toLuaUtf8())
    
    local p = Qt.QPoint()
    p:setX(5)
    print(p:x(), p:y())

    local t = Qt.QTimer()
    t:setSingleShot(true)
    t:setInterval(10000)
    t:connect('2timeout()', function () print 'timeout' end)
    t:start()

    app.exec()

