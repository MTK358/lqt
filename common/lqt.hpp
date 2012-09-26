
extern "C" {
#include <lua.h>
#include <lauxlib.h>
}

#include <QtCore>

class LqtLuaFunctionSlotHandler : public QObject {
    Q_OBJECT
public:
    LqtLuaFunctionSlotHandler(QObject *sender, const char *signal, lua_State *L, int funcindex);
    ~LqtLuaFunctionSlotHandler();

private slots:
    void theslot();

private:
    lua_State *mL;
    int mFuncRef;
};

