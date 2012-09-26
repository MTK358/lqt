
return {
    ['QEventLoop'] = {
        nocopy = true,
    },
    ['QCoreApplication'] = {
        ctor = {code=[=[
            luaL_argcheck(L, lua_type(L, 2) == LUA_TTABLE, 1, "expected table");
            lua_rawgeti(L, 2, 0);
            luaL_argcheck(L, lua_type(L, -1) != LUA_TNIL, 1, "table must have a 0 item containing the program name");
            int argc = lua_objlen(L, 1);
            char **argv = (char**) malloc((argc+1) * sizeof(char));
            for (size_t i=0; i<=argc; ++i) {
                lua_rawgeti(L, 2, i);
                const char *str = lua_tostring(L, -1);
                argv[i] = strdup(str);
                lua_pop(L, 1);
            }
            int newargc = argc;
            @push QCoreApplication*(L, new QCoreApplication(newargc, argv), true)@;
            for (size_t i=0; i<=argc; ++i) {
                free(argv[i]);
            }
            free(argv);
            return 1;
        ]=]},
    },
    ['QString'] = {
        {
            [0] = 'wrappedfunction',
            name = 'toLuaUtf8',
            code = [=[
                QString const& str = @check QString const&(L, 1)@;
                QByteArray ba = str.toUtf8();
                lua_pushlstring(L, ba.constData(), ba.size());
                return 1;
            ]=],
        },
        {
            [0] = 'wrappedfunction',
            name = 'toLuaLocal8Bit',
            code = [=[
                QString const& str = @check QString const&(L, 1)@;
                QByteArray ba = str.toLocal8Bit();
                lua_pushlstring(L, ba.constData(), ba.size());
                return 1;
            ]=],
        },
        {
            [0] = 'wrappedfunction',
            name = 'toLuaAscii',
            code = [=[
                QString const& str = @check QString const&(L, 1)@;
                QByteArray ba = str.toAscii();
                lua_pushlstring(L, ba.constData(), ba.size());
                return 1;
            ]=],
        },
    },
    ['QByteArray'] = {
        {
            [0] = 'wrappedfunction',
            name = 'toLua',
            code = [=[
                QByteArray const& ba = @check QByteArray const&(L, 1)@;
                lua_pushlstring(L, ba.constData(), ba.size());
                return 1;
            ]=],
        },
    },
    ['QObject'] = {
        {
            [0] = 'wrappedfunction',
            name = 'connect',
            code = [=[
                QObject *sender = @check QObject*(L, 1)@;
                const char *signal = @check char const*(L, 2)@;
                if (lua_type(L, 3) == LUA_TFUNCTION) {
                    LqtLuaFunctionSlotHandler *handler = new LqtLuaFunctionSlotHandler(sender, signal, L, 3);
                } else {
                    QObject *receiver = @check QObject*(L, 3)@;
                    const char *slot = @check char const*(L, 4)@;
                    QObject::connect(sender, signal, receiver, slot);
                }
                return 0;
            ]=],
        },
    },
}

