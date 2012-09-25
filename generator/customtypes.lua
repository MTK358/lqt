
return {
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
}

