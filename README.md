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

    local QtCore = require 'QtCore'
    
    local str = QtCore.QString.number(15)
    print(str:toLuaUtf8())
    
    local p = QPoint()
    p:setX(5)
    print(p:x(), p:y())
