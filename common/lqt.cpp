
#include "lqt.hpp"

LqtLuaFunctionSlotHandler::LqtLuaFunctionSlotHandler(QObject *sender, const char *signal, lua_State *L, int funcindex) {
    setParent(sender); // destroy this object when the sender is destroyed
    connect(sender, signal, this, SLOT(theslot()));
    mL = L;
    lua_pushvalue(L, funcindex);
    mFuncRef = luaL_ref(L, LUA_REGISTRYINDEX);
}

LqtLuaFunctionSlotHandler::~LqtLuaFunctionSlotHandler() {
    luaL_unref(mL, LUA_REGISTRYINDEX, mFuncRef);
}

void LqtLuaFunctionSlotHandler::theslot() {
    lua_rawgeti(mL, LUA_REGISTRYINDEX, mFuncRef);
    int result = lua_pcall(mL, 0, 0, 0);
    if (result != 0) {
        fprintf(stderr, "error in slot handler: %s\n", lua_tostring(mL, -1));
        lua_pop(mL, 1);
    }
}

