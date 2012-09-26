#!/usr/bin/env lua

--[[

Copyright (c) 2012 Michael Kirsch

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

--]]

--[=[
Generates a module that lets you use a C++ API from Lua based on a spec file.

The spec file is a Lua script that returns a table with the following values:

 * "module": The name of the module.
 * "cppcode_pre": C++ code to put before everything.
 * "cppcode_post": C++ code to put after everything.

And the table can also contain more tables as array entries that describe
things to bind. The [0] item of these tables should be a string describing
the type of binding. These are the possible choices:

 * "namespace": Put child bindings inside a named table.
   * "name": The Lua name of the namespace table.
 * "enum": Bind an enum to Lua. The enum's entries will be exposed to Lua as
           string keys containing number values.
   * "cppname": The full C++ name of the enum.
   * "values": A newline-separated list of "name = value" pairs.
 * "class": Bind a class.
   * "name": The Lua name of the class.
   * "cppname": The full C++ name of the class.
   * "base" (optional): The full C++ name of the base of the class.
   * "ctors" (optional): A table of comma-separated arg lists of the class's
                         constructors. Set this to false instead of a table (
                         nil won't work) for abstract classes.
   * "nocopy" (optional): Do not create code that copies the object. This
                          means that you can't bind functions that pass this
                          class by copy.
   * "methods" (optional): A newline-separated list of the signatures of the
                           class's methods. Overloads must be on the same line
                           and separated with "|" characters. The return type
                           and name ore optional in all but the first in an
                           overload list. For a custom Lua name, prepend the
                           line with "@myluaname". For static methods, do not
                           add them to this list, instead add child "function"
                           bindings.
 * "function": Bind a function (or static method).
   * "name" (optional): Specify a Lua name different from the one in the
                        signature.
   * "sig": The C++ signature of the function, including name and return type.
            For overloaded functions, separate the signatures with a "|"
            character. The return types and names are optional for all but the
            first overload in the list.
 * "wrappedfunction": Bind a function with custom C++ code that runs when it's
                      called.
   * "name": The Lua name of the function.
   * "code": The C++ code to run when the function is called.
 * "template": Substitute template paremeters is child specs.
   * "substitutions": A list containing tables of name->substitution pairs.
                      For every item in this table, the child spec is run
                      once, with the specified substitutions done to its
                      template args. In Lua, the "<", ",", and ">" characters
                      in typenames are replaced with "_".
]=]

local function printdebug(...)
    --print(...)
end

local infile = assert(arg[1], 'input file not specified')
local outsrc = assert(arg[2], 'output file not specified')

local operatornames = {--{{{
    ['+ 2'] = '!__add', -- values starting with "!" are put in the instance
    ['- 2'] = '!__sub', -- metatable instead of the class table
    ['* 2'] = '!__mul',
    ['/ 2'] = '!__div',
    ['- 1'] = '!__unm',
    ['++ 0'] = 'op_incr',
    ['-- 0'] = 'op_decr',
    ['<< 2'] = 'op_lshift',
    ['>> 2'] = 'op_lshift',
    ['| 2'] = 'op_bitor',
    ['& 2'] = 'op_bitand',
    -- lua has a ^ operator, but with a completely different meaning, so using
    -- it here might be confusing
    ['^ 2'] = 'op_bitxor',
    ['~ 1'] = 'op_bitnot',
    -- the lua [] operator can't be used since it wouldn't work if there's a
    -- field/method with the same name as the key
    ['[] 2'] = 'op_index',
}--}}}

local spec = assert(dofile(infile))

-- preheader and header go before the main function, bodyheader and body go in
-- the main function, and footer goes after it
local srcpreheader, srcheader, srcbodyheader, srcbody, srcfooter = '', '', '', '', ''

-- writing functions
local function writesrcpreheader(str)--{{{
    srcpreheader = srcpreheader..str
end

local function writesrcheader(str)
    srcheader = srcheader..str
end

local function writesrcbodyheader(str)
    srcbodyheader = srcbodyheader..str
end

local function writesrcbody(str)
    srcbody = srcbody..str
end

local function writesrcfooter(str)
    srcfooter = srcfooter..str
end--}}}

local nextid = 0
-- get a uinique integer ID number every time
local function uniqueid()--{{{
    local id = nextid
    nextid = nextid + 1
    return id
end--}}}

-- table of type names and is/to/check/push code generators
local types = {}

-- fill type table/create type adding functions
local function addnumtype(cppname, preference)--{{{
    types[cppname] = {
        is = function (state, index) return ('(lua_type(%s,%s)==LUA_TNUMBER)'):format(state, index) end,
        to = function (state, index) return ('((%s)lua_tonumber(%s,%s))'):format(cppname, state, index) end,
        check = function (state, index) return ('((%s)luaL_checknumber(%s,%s))'):format(cppname, state, index) end,
        push = function (state, value) return ('lua_pushnumber(%s, %s);'):format(state, value) end,
        preference = preference,
        luatype = 'number',
    }
    types[cppname..' const&'] = types[cppname]
    types[cppname..'&'] = types[cppname]
end

local function addtypealias(alias, typename)
    local additions = {}
    for k, v in pairs(types) do
        if k:sub(1, #typename) == typename then
            additions[alias..k:sub(#typename+1)] = v
        end
    end
    for k, v in pairs(additions) do
        types[k] = v
    end
end

addnumtype('double', 0)
addnumtype('float', -1)
addnumtype('long long', -2)
addnumtype('unsigned long long', -3)
addnumtype('long', -4)
addnumtype('unsigned long', -5)
addnumtype('int', -6)
addnumtype('unsigned int', -7)
addnumtype('short', -8)
addnumtype('unsigned short', -9)
addtypealias('unsigned', 'unsigned int')
addtypealias('short int', 'short')
addtypealias('unsigned short int', 'unsigned short')
addtypealias('long int', 'long')
addtypealias('unsigned long int', 'unsigned long')
addtypealias('long long int', 'long long')
addtypealias('unsigned long long int', 'unsigned long long')

types['bool'] = {
    is = function (state, index) return ('(true)') end,
    to = function (state, index) return ('((bool) lua_toboolean(%s,%s))'):format(state, index) end,
    check = function (state, index) return ('((bool) luaL_checkboolean(%s,%s))'):format(state, index) end,
    push = function (state, value) return ('lua_pushboolean(%s, %s);'):format(state, value) end,
    luatype = 'bool',
}
types['char const*'] = {
    is = function (state, index) return ('(lua_type(%s,%s)==LUA_TSTRING)'):format(state, index) end,
    to = function (state, index) return ('(lua_tostring(%s,%s))'):format(state, index) end,
    check = function (state, index) return ('(luaL_checkstring(%s,%s))'):format(state, index) end,
    push = function (state, value) return ('lua_pushstring(%s, %s);'):format(state, value) end,
    luatype = 'string',
}
types['char'] = {
    is = function (state, index) return ('(lua_type(%s,%s)==LUA_TSTRING)'):format(state, index, state, index) end,
    to = function (state, index) return ('(lua_tostring(%s,%s)[0])'):format(state, index) end,
    check = function (state, index) return ('(luaL_checkstring(%s,%s)[0])'):format(state, index) end,
    push = function (state, value) return ('{char c=(%s);lua_pushlstring(%s, &c, 1);};'):format(value, state) end,
    luatype = 'string',
}
addtypealias('signed char', 'char')
addtypealias('unsigned char', 'char')
            
local bases = {}
local classids = {}
local function addclasstype(cppname, nocopy, base)
    if base and not classids[base] then error(('base class of %s (%s) not registered'):format(cppname, base)) end
    bases[cppname] = base
    local id = uniqueid()
    classids[cppname] = id
    local fixedcppname = cppname:gsub('>>', '> >'):gsub('>>', '> >')
    writesrcpreheader('static int _bindgen_classtable'..id..';\n')
    writesrcpreheader('static int _bindgen_classmt'..id..';\n')
    writesrcpreheader('static int _bindgen_instancemt'..id..';\n')
    if not nocopy then
        writesrcpreheader([[
static bool _bindgen_is]]..id..[[(lua_State *L, int index) {
    int type = lua_type(L, index);
    if (type != LUA_TUSERDATA) return false;
    lua_rawgeti(L, LUA_REGISTRYINDEX, _bindgen_instancemt]]..id..[[);
    lua_getmetatable(L, index>0 ? index : index-1);
    if (! lua_rawequal(L, -1, -2)) {
        lua_pop(L, 2);
        return false;
    }
    lua_pop(L, 2);
    return true;
}
static ]]..fixedcppname..[[ _bindgen_to]]..id..[[(lua_State *L, int index) {
    _BindgenUserdata<]]..fixedcppname..[[ > *ud = (_BindgenUserdata<]]..fixedcppname..[[ >*) lua_touserdata(L, index);
    return *ud->instance;
}
static ]]..fixedcppname..[[ _bindgen_check]]..id..[[(lua_State *L, int index) {
    if (! _bindgen_is]]..id..[[(L, index)) {
        lua_pushfstring(L, "expected ]]..fixedcppname..[[, got %s", lua_typename(L, index));
        luaL_argerror(L, index, lua_tostring(L, -1));
    }
    return _bindgen_to]]..id..[[(L, index);
}
static void _bindgen_push]]..id..[[(lua_State *L, const ]]..fixedcppname..[[ &value) {
    lua_rawgeti(L, LUA_REGISTRYINDEX, _bindgen_instancemt]]..id..[[);
    _BindgenUserdata<]]..fixedcppname..[[ > *ud = (_BindgenUserdata<]]..fixedcppname..[[ >*) lua_newuserdata(L, sizeof(_BindgenUserdata<]]..fixedcppname..[[ >));
    ud->instance = new ]]..fixedcppname..[[(value);
    ud->ownedbylua = true;
    lua_pushvalue(L, -2);
    lua_setmetatable(L, -2);
    lua_remove(L, -2);
}]])
    end
    writesrcpreheader([[
static bool _bindgen_isref]]..id..[[(lua_State *L, int index) {
    int type = lua_type(L, index);
    if (type != LUA_TUSERDATA) return false;
    lua_rawgeti(L, LUA_REGISTRYINDEX, _bindgen_instancemt]]..id..[[);
    lua_getmetatable(L, index>0 ? index : index-1);
    if (lua_rawequal(L, -1, -2)) {
        lua_pop(L, 2);
        return true;
    }
    while (true) {
        lua_rawgeti(L, -1, 1);
        if (lua_type(L, -1) == LUA_TNIL) {
            lua_pop(L, 3);
            return false;
        }
        lua_remove(L, -2);
        if (lua_rawequal(L, -1, -2)) {
            lua_pop(L, 2);
            return true;
        }
    }
}
static ]]..fixedcppname..[[* _bindgen_toref]]..id..[[(lua_State *L, int index) {
    _BindgenUserdata<]]..fixedcppname..[[ > *ud = (_BindgenUserdata<]]..fixedcppname..[[ >*) lua_touserdata(L, index);
    return ud->instance;
}
static ]]..fixedcppname..[[* _bindgen_checkref]]..id..[[(lua_State *L, int index) {
    if (! _bindgen_isref]]..id..[[(L, index)) {
        lua_pushfstring(L, "expected ]]..fixedcppname..[[, got %s", lua_typename(L, index));
        luaL_argerror(L, index, lua_tostring(L, -1));
    }
    return _bindgen_toref]]..id..[[(L, index);
}
static void _bindgen_pushref]]..id..[[(lua_State *L, ]]..fixedcppname..[[ *value, bool ownedbylua) {
    lua_rawgeti(L, LUA_REGISTRYINDEX, _bindgen_instancemt]]..id..[[);
    _BindgenUserdata<]]..fixedcppname..[[ > *ud = (_BindgenUserdata<]]..fixedcppname..[[ >*) lua_newuserdata(L, sizeof(_BindgenUserdata<]]..fixedcppname..[[ >));
    ud->instance = value;
    ud->ownedbylua = ownedbylua;
    lua_pushvalue(L, -2);
    lua_setmetatable(L, -2);
    lua_remove(L, -2);
}

]])
    if base then
        writesrcbodyheader([[
    lua_newtable(L);                                                   // ct
    lua_pushvalue(L, -1);                                              // ct ct
    _bindgen_classtable]]..id..[[ = luaL_ref(L, LUA_REGISTRYINDEX);    // ct
    lua_newtable(L);                                                   // ct imt
    lua_pushvalue(L, -1);                                              // ct imt imt
    _bindgen_instancemt]]..id..[[ = luaL_ref(L, LUA_REGISTRYINDEX);    // ct imt
    lua_rawgeti(L, LUA_REGISTRYINDEX, _bindgen_instancemt]]..classids[base]..[[); // ct imt baseimt
    lua_rawseti(L, -2, 1);                                             // ct imt
    lua_newtable(L);                                                   // ct imt cmt
    lua_pushvalue(L, -1);                                              // ct imt cmt cmt
    _bindgen_classmt]]..id..[[ = luaL_ref(L, LUA_REGISTRYINDEX);       // ct imt cmt
    lua_rawgeti(L, LUA_REGISTRYINDEX, _bindgen_classtable]]..classids[base]..[[); // ct imt cmt basect
    lua_rawsetf(L, -2, "__index");                                     // ct imt cmt
    lua_setmetatable(L, -3);                                           // ct imt
    lua_pushvalue(L, -2);                                              // ct imt ct
    lua_rawsetf(L, -2, "__index");                                     // ct imt
    lua_pushcfunction(L, (&_bindgen_instance_metatable___gc<]]..fixedcppname..[[ >));      // ct imt gc
    lua_rawsetf(L, -2, "__gc");                                        // ct imt
    lua_pop(L, 2);                                                     // (none)

]])
    else
        writesrcbodyheader([[
    lua_newtable(L);                                                   // ct
    lua_pushvalue(L, -1);                                              // ct ct
    _bindgen_classtable]]..id..[[ = luaL_ref(L, LUA_REGISTRYINDEX);    // ct
    lua_newtable(L);                                                   // ct imt
    lua_pushvalue(L, -1);                                              // ct imt imt
    _bindgen_instancemt]]..id..[[ = luaL_ref(L, LUA_REGISTRYINDEX);    // ct imt
    lua_newtable(L);                                                   // ct imt cmt
    lua_pushvalue(L, -1);                                              // ct imt cmt cmt
    _bindgen_classmt]]..id..[[ = luaL_ref(L, LUA_REGISTRYINDEX);       // ct imt cmt
    lua_setmetatable(L, -3);                                           // ct imt
    lua_pushvalue(L, -2);                                              // ct imt ct
    lua_rawsetf(L, -2, "__index");                                     // ct imt
    lua_pushcfunction(L, (&_bindgen_instance_metatable___gc<]]..fixedcppname..[[ >));      // ct imt gc
    lua_rawsetf(L, -2, "__gc");                                        // ct imt
    lua_pop(L, 2);                                                     // (none)

]])
    end
    if not nocopy then
        types[cppname] = {
            is = function (state, index) return ('(_bindgen_is'..id..'(%s,%s))'):format(state, index) end,
            to = function (state, index) return ('(_bindgen_to'..id..'(%s,%s))'):format(state, index) end,
            check = function (state, index) return ('(_bindgen_check'..id..'(%s,%s))'):format(state, index) end,
            push = function (state, value) return ('_bindgen_push'..id..'(%s, %s);'):format(state, value) end,
            luatype = '!'..id,
        }
    end
    types[cppname..'*'] = {
        is = function (state, index) return ('(_bindgen_isref'..id..'(%s,%s))'):format(state, index) end,
        to = function (state, index) return ('(_bindgen_toref'..id..'(%s,%s))'):format(state, index) end,
        check = function (state, index) return ('(_bindgen_checkref'..id..'(%s,%s))'):format(state, index) end,
        push = function (state, value, ownedbylua) return ('_bindgen_pushref'..id..'(%s,%s,%s);'):format(state, value, ownedbylua and 'true' or 'false') end,
        luatype = '!'..id,
    }
    types[cppname..'&'] = {
        is = function (state, index) return ('(_bindgen_isref'..id..'(%s,%s))'):format(state, index) end,
        to = function (state, index) return ('(*_bindgen_toref'..id..'(%s,%s))'):format(state, index) end,
        check = function (state, index) return ('(*_bindgen_checkref'..id..'(%s,%s))'):format(state, index) end,
        push = function (state, value, ownedbylua) return ('_bindgen_pushref'..id..'(%s,&(%s),%s);'):format(state, value, ownedbylua and 'true' or 'false') end,
        luatype = '!'..id,
    }
    types[cppname..' const*'] = {
        is = function (state, index) return ('(_bindgen_isref'..id..'(%s,%s))'):format(state, index) end,
        to = function (state, index) return ('(_bindgen_toref'..id..'(%s,%s))'):format(state, index) end,
        check = function (state, index) return ('(_bindgen_checkref'..id..'(%s,%s))'):format(state, index) end,
        push = function (state, value) assert(not nocopy) return ('_bindgen_push'..id..'(%s, *(%s));'):format(state, value) end,
        luatype = '!'..id,
    }
    types[cppname..' const&'] = {
        is = function (state, index) return ('(_bindgen_isref'..id..'(%s,%s))'):format(state, index) end,
        to = function (state, index) return ('(*_bindgen_toref'..id..'(%s,%s))'):format(state, index) end,
        check = function (state, index) return ('(*_bindgen_checkref'..id..'(%s,%s))'):format(state, index) end,
        push = function (state, value) assert(not nocopy) return ('_bindgen_push'..id..'(%s, %s);'):format(state, value) end,
        luatype = '!'..id,
    }
    types[cppname..'*const&'] = types[cppname..'*']
end--}}}

-- is/to/check/push code generators
--{{{
local function gettype(name)
    if not name:match('[&*]') then
        name = (' '..name..' '):gsub('([^%w_:])const([^%w_:])', '%1 %2')
    end
    -- FIXME types written as "const Example &" instead of "Example const &"
    name = name:gsub(' ([^%w_])', '%1'):gsub('([^%w_>]) ', '%1')
    name = name:gsub('^%s*([^ ].-[^ ])%s*$', '%1')
    return types[name]
end

local function generate_is_code(typename, state, index)
    return gettype(typename).is(state, index)
end

local function generate_to_code(typename, state, index)
    return gettype(typename).to(state, index)
end

local function generate_check_code(typename, state, index)
    return gettype(typename).check(state, index)
end

local function generate_push_code(typename, state, value, ownedbylua)
    return gettype(typename).push(state, value, ownedbylua)
end
--}}}

-- iterate over a string of comma separated C++ type names (ignores commas in nested <> and () pairs)
local function comma_separated_type_iterator(str)--{{{
    local cur = 1
    return function ()
        local start = cur
        while true do
            local match = str:match('^[^,<(]()', start) or str:match('^%b<>()', start) or str:match('^%b()()', start)
            if match then
                start = match
            else
                local substr = str:sub(cur, start-1)
                cur = start
                cur = str:match(',()', cur) or cur
                return #substr~=0 and substr or nil
            end
        end
    end
end--}}}

-- substitute template args in a type name based on an arg name->type table
local function substitute_templates(template, subs)--{{{
    local out = ' '..template..' '
    for k, v in pairs(subs) do
        out = out:gsub('([<,> *&])'..k..'([<,> *&])', '%1'..v..'%2')
    end
    return out:sub(2, -2)
end--}}}

-- taken from http://ricilake.blogspot.com/2007/10/iterating-bits-in-lua.html
local function bitor(x, y)--{{{
  local p = 1
  while p < x do p = p + p end
  while p < y do p = p + p end
  local z = 0
  repeat
    if p <= x or p <= y then
      z = z + p
      if p <= x then x = x - p end
      if p <= y then y = y - p end
    end
    p = p * 0.5
  until p < 1
  return z
end--}}}

-- parses addition, bitwise or, and names of previous values for enum fields
local function parse_enum_value(str, vars)--{{{
    local a, b
    a, b = str:match('(.+)|(.+)')
    if a then
        return bitor(parse_enum_value(a, vars), parse_enum_value(b, vars))
    end
    a, b = str:match('(.+)%+(.+)')
    if a then
        return parse_enum_value(a, vars) + parse_enum_value(b, vars)
    end
    a = str:match('^%s*([%a_][%w_]*)%s*$')
    if a then
        return assert(tonumber(vars[a]))
    end
    return assert(tonumber(str), 'cannot parse enum value: '..str)
end--}}}

-- substitute special syntax with generated is/to/check/push code
local function process_wrapped_function_code(code)--{{{
    local newcode, prevend = '', 1
    for istart, str, iend in code:gmatch('()@(.-)@()') do
        newcode = newcode..code:sub(prevend, istart-1)
        if str ~= '' then
            local op, typename, argstr = str:match('^%s*([a-z]+)%s+(.-)%s*%((.*)%)%s*$')
            local args = {}
            for arg in comma_separated_type_iterator(argstr) do args[#args+1] = arg end
            newcode = newcode..gettype(typename)[op](unpack(args))
        end
        prevend = iend
    end
    newcode = newcode..code:sub(prevend)
    return newcode
end--}}}

-- convert a C++ signature (or multiple "|"-separated ones for overloading)
-- into a table representation
local function process_signature(sigs, templatesubs)--{{{
    local overloads = {}
    local match = sigs:match('^@([%w_]+)')
    if match then
        overloads.luaname = match
        sigs = sigs:sub(#match+1)
    end
    local sigstbl = {}
    for i in sigs:gmatch('[^|]+') do sigstbl[#sigstbl+1] = i end
    repeat
        local done = true
        for k, v in ipairs(sigstbl) do
            if v:match('=') then
                local name, args = v:match('^(.-)(%b())%s*$')
                local newargs = {}
                for arg in comma_separated_type_iterator(args:sub(2, -2)) do
                    if arg:match('=') then
                        local newsig1 = name..'('..table.concat(newargs, ',')..')'
                        table.insert(newargs, arg:match('^(.-)='))
                        local newsig2 = name..'('..table.concat(newargs, ',')..')'
                        sigstbl[k] = newsig2
                        table.insert(sigstbl, k, newsig1)
                    else
                        table.insert(newargs, arg)
                    end
                end
            end
            --[[local tmp1, tmp2 = v:match('[,(].-()=.-([,)].*)')
            if tmp1 then
                print(v:sub(1, tmp1-1), 'dsfdsfsdfd', v:sub(1, tmp1-1)..tmp2)
                sigstbl[k] = v:sub(1, tmp1-1)
                table.insert(sigstbl, k+1, v:sub(1, tmp1-1)..tmp2)
                done = false
                break
            end]]
        end
    until done
    local luasigs = {}
    for _, i in ipairs(sigstbl) do
        local overload = {}
        local rettypename, functype, cppname = i:match('^(.-%S)%s+([@#]?)([%w_:<,>]+)%(')
        if rettypename then
            cppname = cppname:gsub('>>', '> >'):gsub('>>', '> >')
            overload.rettypename, overload.cppname = substitute_templates(rettypename, templatesubs), substitute_templates(cppname, templatesubs)
            overload.functype = functype == '@' and 'method' or (functype == '#' and 'staticmethod' or 'normal')
            overloads.luaname = overloads.luaname or overload.cppname:match('::(.-)$'):gsub('[<,>]', '_')
            local context, basename = overload.cppname:match('(.+)::(..-)$')
            if context then
                overload.context, overload.basename = context:gsub('>>', '> >'):gsub('>>', '> >'), basename:gsub('>>', '> >'):gsub('>>', '> >')
            else
                overload.basename = cppname
            end
        elseif overloads[1] then
            overload.rettypename, overload.cppname, overload.functype, overload.context, overload.basename =
                overloads[1].rettypename, overloads[1].cppname, overloads[1].functype, overloads[1].context, overloads[1].basename
        end
        if overload.rettypename and not overload.rettypename:match('^%s*void%s*$') then
            overload.rettype = assert(gettype(overload.rettypename), 'no such registered type: '..overload.rettypename)
        end
        overload.args = {}
        overload.preference = 0
        overload.luasig = ''
        for j in comma_separated_type_iterator(i:match('(%b())%s*$'):sub(2, -2)) do
            local arg = {}
            arg.typename = substitute_templates(j, templatesubs):gsub('>>', '> >'):gsub('>>', '> >')
            arg.type = assert(gettype(arg.typename), 'no such registered type: '..arg.typename)
            overload.preference = overload.preference + (arg.type.preference or 0)
            overload.luasig = overload.luasig..arg.type.luatype..' '
            table.insert(overload.args, arg)
        end
        if luasigs[overload.luasig] then
            if luasigs[overload.luasig].preference < overload.preference then
                overload.index = luasigs[overload.luasig].index
                overloads[overload.index] = overload
                luasigs[overload.luasig] = overload
            end
        else
            luasigs[overload.luasig] = overload
            table.insert(overloads, overload)
            overload.index = #overloads
        end
    end
    return overloads
end--}}}
        
local generate

-- binding code generator functions

local function bind_enum(spec, templatesubs)--{{{
    local cppname = substitute_templates(spec.cppname, templatesubs)
    local val, vars = '-1', {}
    for name in spec.values:gmatch('[^\n]*[^\n%s][^\n]*') do
        local newname, newval = name:match('(%S+)%s*=%s*(%S+)')
        if newname then
            name = newname
            val = newval
            local success, result = pcall(parse_enum_value, val, vars)
            if success then val = tostring(result) end
        else
            val = tostring(parse_enum_value(val, vars) + 1)
        end
        writesrcbody('    '..generate_push_code(cppname, 'L', val)..'\n')
        writesrcbody('    lua_rawsetf(L, -2, "'..name..'");\n')
        vars[name] = val
    end
    writesrcbody('\n')  
end--}}}

local function bind_function(spec, templatesubs)--{{{
    local id = uniqueid()
    printdebug('function', spec)
    local overloads = process_signature(spec, templatesubs)
    writesrcheader('struct _BindgenFunctionWrapper'..id)
    local first, bases = true, {}
    for i, overload in ipairs(overloads) do
        if overload.functype ~= 'normal' and not bases[overload.context] then
            if first then
                writesrcheader(' : ')
                first = false
            else
                writesrcheader(', ')
            end
            writesrcheader('public '..overload.context)
            bases[overload.context] = true
        end
    end
    writesrcheader(' { \nstatic int _bindgen_wrapper(lua_State *L) {\n')
    for i, overload in ipairs(overloads) do
        local callcode = ''
        if overload.functype == 'method' then
            writesrcheader('    '..(i==1 and 'if' or 'else if')..' (lua_gettop(L) >= '..(#overload.args+1))
            writesrcheader(' && '..generate_is_code(overload.context..'*', 'L', 1))
            for j, arg in ipairs(overload.args) do writesrcheader(' && '..generate_is_code(arg.typename, 'L', j+1)) end
            writesrcheader(') {\n        ')
            callcode = '((_BindgenFunctionWrapper'..id..'*)'..generate_to_code(overload.context..'*', 'L', 1)..')->'..overload.basename..'('
            for j, arg in ipairs(overload.args) do callcode = callcode..((j==1 and '' or ',')..generate_to_code(arg.typename, 'L', j+1)) end
            callcode = callcode..')'
        else
            writesrcheader('    '..(i==1 and 'if' or 'else if')..' (lua_gettop(L) >= '..(#overload.args))
            for j, arg in ipairs(overload.args) do writesrcheader(' && '..generate_is_code(arg.typename, 'L', j)) end
            writesrcheader(') {\n        ')
            callcode = overload.cppname:gsub('>>', '> >'):gsub('>>', '> >')..'('
            for j, arg in ipairs(overload.args) do callcode = callcode..((j==1 and '' or ',')..generate_to_code(arg.typename, 'L', j)) end
            callcode = callcode..')'
        end
        if overload.rettype then
            callcode = generate_push_code(overload.rettypename, 'L', callcode)
            writesrcheader(callcode..'\n            return 1;}\n')
        else
            writesrcheader(callcode..';\n            return 0;}\n')
        end
    end
    writesrcheader('    else { return luaL_error(L, "improper function args");}\n')
    writesrcheader('}};\n\n')
    writesrcbody('    lua_pushcfunction(L, &_BindgenFunctionWrapper'..id..'::_bindgen_wrapper);\n')
    writesrcbody('    lua_rawsetf(L, -2, "'..overloads.luaname..'");\n\n')
end--}}}

local function bind_wrappedfunction(spec, templatesubs)--{{{
    local id = uniqueid()
    if next(templatesubs) then
        writesrcheader('template <')
        local first = true
        for k, v in pairs(templatesubs) do
            if first then first = false else writesrcheader(', ') end
            writesrcheader('typename '..k)
        end
        writesrcheader(' >\n')
    end
    writesrcheader('int _bindgen_wrappedfunction_'..id..'(lua_State *L) {\n')
    writesrcheader(process_wrapped_function_code(spec.code))
    writesrcheader('}\n\n')
    writesrcbody('    lua_pushcfunction(L, &_bindgen_wrappedfunction_'..id)
    if next(templatesubs) then
        writesrcheader('<')
        local first = true
        for k, v in pairs(templatesubs) do
            if first then first = false else writesrcheader(', ') end
            writesrcheader(v)
        end
        writesrcheader(' >\n')
    end
    writesrcbody(');\n')
    writesrcbody('    lua_rawsetf(L, -2, "'..spec.name..'");\n')
end--}}}

local function bind_class(spec, templatesubs)--{{{
    local cppname = substitute_templates(spec.cppname, templatesubs)
    printdebug('class', cppname)
    
    if spec.cppcode_pre then writesrcheader(process_wrapped_function_code(spec.cppcode_pre)) end
    if spec.cppcode_post then writesrcfooter(process_wrapped_function_code(spec.cppcode_post)) end

    if type(spec.ctor) == 'string' then
        printdebug('constructor', spec.ctor)
        local overloads = process_signature(spec.ctor, templatesubs)
        local id = uniqueid()
        writesrcheader('struct _BindgenConstructorWrapper'..id..' : public '..cppname:gsub('>>', '> >'):gsub('>>', '> >')..' {\n')
        writesrcheader('static int wrapper(lua_State *L) {\n')
        for i, overload in ipairs(overloads) do
            writesrcheader('    ')
            if i ~= 1 then writesrcheader('else ') end
            writesrcheader('if (lua_gettop(L) == '..#overload.args+1)
            for j, arg in ipairs(overload.args) do
                writesrcheader(' && '..generate_is_code(arg.typename, 'L', j+1))
            end
            writesrcheader(') {\n')
            local callcode = 'new '..cppname:gsub('>>', '> >'):gsub('>>', '> >')..'('
            for j, arg in ipairs(overload.args) do callcode = callcode..(j==1 and '' or ',')..generate_to_code(arg.typename, 'L', j+1) end
            writesrcheader('        '..generate_push_code(cppname..'*', 'L', callcode..')'), true)
            writesrcheader('\n        return 1; }\n')
        end
        writesrcheader('    return luaL_error(L, "improper constructor args");\n}};\n\n')
        writesrcbody('    lua_rawgeti(L, LUA_REGISTRYINDEX, _bindgen_classmt'..classids[cppname]..');\n')
        writesrcbody('    lua_pushcfunction(L, &_BindgenConstructorWrapper'..id..'::wrapper);\n')
        writesrcbody('    lua_rawsetf(L, -2, "__call");\n')
        writesrcbody('    lua_pop(L, 1);\n')
    elseif type(spec.ctor) == 'table' then
        printdebug('wrapped constructor')
        local id = uniqueid()
        if next(templatesubs) then
            writesrcheader('template <')
            local first = true
            for k, v in pairs(templatesubs) do
                if first then first = false else writesrcheader(', ') end
                writesrcheader('typename '..k)
            end
            writesrcheader(' >\n')
        end
        writesrcheader('int _bindgen_wrappedfunction_'..id..'(lua_State *L) {\n')
        writesrcheader(process_wrapped_function_code(spec.ctor.code))
        writesrcheader('}\n\n')
        writesrcbody('    lua_rawgeti(L, LUA_REGISTRYINDEX, _bindgen_classmt'..classids[cppname]..');\n')
        writesrcbody('    lua_pushcfunction(L, &_bindgen_wrappedfunction_'..id)
        if next(templatesubs) then
            writesrcheader('<')
            local first = true
            for k, v in pairs(templatesubs) do
                if first then first = false else writesrcheader(', ') end
                writesrcheader(v)
            end
            writesrcheader(' >\n')
        end
        writesrcbody(');\n')
        writesrcbody('    lua_rawsetf(L, -2, "__call");\n')
        writesrcbody('    lua_pop(L, 1);\n')
    end
    
    writesrcbody('    lua_rawgeti(L, LUA_REGISTRYINDEX, _bindgen_classtable'..classids[cppname]..');\n')
    
    for _, subspec in ipairs(spec) do generate(subspec, templatesubs) end

    writesrcbody('    lua_rawsetf(L, -2, "'..spec.name..'");\n')
    printdebug('end class', cppname)
end--}}}

local function bind_template(spec, templatesubs)--{{{
    printdebug('template')
    for _, subs in ipairs(spec.substitutions) do
        local newtemplatesubs = {}
        for k, v in pairs(templatesubs) do newtemplatesubs[k] = v end
        for k, v in pairs(subs) do newtemplatesubs[k] = v end
        local oldname = spec[1].name
        printdebug('template instance')
        for k, v in pairs(subs) do -- FIXME
            printdebug('template substitution', k, v)
            spec[1].name = spec[1].name..'_'..v
        end
        spec[1].name = spec[1].name..'_'
        generate(spec[1], newtemplatesubs)
        spec[1].name = oldname
        printdebug('end template instance')
    end
    printdebug('end template')
end--}}}

local function bind_customtype(spec)--{{{
    for _, typename in ipairs(type(spec.typename)=='table' and spec.typename or {spec.typename}) do
        types[typename] = {
            is = spec.is,
            to = spec.to,
            check = spec.check,
            push = spec.push,
            luatype = spec.luatype,
            preference = spec.preference,
        }
    end
end--}}}

generate = function(subspec, templatesubs)--{{{
    if type(subspec) == 'string' then
        bind_function(subspec, templatesubs)
    elseif subspec[0] == 'enum' then
        bind_enum(subspec, templatesubs)
    elseif subspec[0] == 'wrappedfunction' then
        bind_wrappedfunction(subspec, templatesubs)
    elseif subspec[0] == 'class' then
        bind_class(subspec, templatesubs)
    elseif subspec[0] == 'namespace' then
        printdebug('namespace', subspec.name)
        writesrcbody('    lua_newtable(L); // namespace '..subspec.name..'\n')
        writesrcbody('    lua_checkstack(L, 1);\n\n')
        for _, subsubspec in ipairs(subspec) do generate(subsubspec, templatesubs) end
        writesrcbody('    lua_rawsetf(L, -2, "'..subspec.name..'"); // end of namespace\n\n')
        printdebug('end namespace', subspec.name)
    elseif subspec[0] == 'template' then
        bind_template(subspec, templatesubs)
    elseif subspec[0] == 'customtype' then
        bind_customtype(subspec)
    else
        error('invalid binding type "'..tostring(subspec[0])..'"')
    end
end--}}}

-- register types found in the spec
do--{{{
    local classes, bases, nocopy = {}, {}, {}
    local function register_types(spec, templatesubs)
        if spec[0] == 'template' then
            for _, subs in ipairs(spec.substitutions) do
                local newtemplatesubs = {}
                for k, v in pairs(templatesubs) do newtemplatesubs[k] = v end
                for k, v in pairs(subs) do newtemplatesubs[k] = v end
                local oldname = spec[1].name
                for k, v in ipairs(subs) do
                    spec[1].name = spec[1].name..'_'..v
                end
                spec[1].name = spec[1].name..'_'
                register_types(spec[1], newtemplatesubs)
                spec[1].name = oldname
            end
            return
        elseif spec[0] == 'class' then
            local cppname = substitute_templates(spec.cppname, templatesubs)
            local base = spec.base and substitute_templates(spec.base, templatesubs)
            classes[cppname] = true
            bases[cppname] = base
            nocopy[cppname] = spec.ctor == false or spec.nocopy
        elseif spec[0] == 'enum' then
            local cppname = substitute_templates(spec.cppname, templatesubs)
            addnumtype(spec.cppname)
        end
        for k, v in ipairs(spec) do if type(v) == 'table' then register_types(v, templatesubs) end end
    end
    register_types(spec, {})
    local function addclass(class)
        if bases[class] and classes[bases[class]] then
            addclass(bases[class])
        end
        addclasstype(class, nocopy[class], bases[class])
        classes[class] = nil
    end
    while true do
        local class = next(classes)
        if not class then break end
        addclass(class)
    end
end--}}}

-- register type aliases
if spec.typealiases then--{{{
    for line in spec.typealiases:gmatch('[^\n]+') do
        --print(line)
        local alias, typename = line:match('^%s*(%S+)%s+(%S.-%S)%s*$')
        --print('alias', alias, typename)
        if alias then
            addtypealias(alias, typename)
        end
    end
end--}}}

writesrcbodyheader('    lua_newtable(L);\n\n')

for _, subspec in ipairs(spec) do generate(subspec, {}) end

-- write the results to the output file
--{{{
local f = assert(io.open(outsrc, "w"))
f:write [[
extern "C" {
#include <lua.h>
#include <lauxlib.h>
}

void lua_rawgetf(lua_State *L, int index, const char *key) {
    lua_getfield(L, index, key);
    //lua_pushstring(L, key);
    //lua_rawget(L, index < 0 ? index-1 : index);
}

void lua_rawsetf(lua_State *L, int index, const char *key) {
    lua_setfield(L, index, key);
    //lua_pushstring(L, key);
    //lua_pushvalue(L, -2);
    //lua_rawset(L, index < 0 ? index-2 : index);
    //lua_pop(L, 1);
}

template <typename T>
struct _BindgenUserdata {
    T *instance;
    bool ownedbylua;
};

template <typename T>
int _bindgen_instance_metatable___gc(lua_State *L) {
    _BindgenUserdata<T> *ud = (_BindgenUserdata<T>*) lua_touserdata(L, 1);
    delete ud->instance;
    return 0;
}
]]
if spec.cppcode_pre then f:write(spec.cppcode_pre) end
f:write(srcpreheader)
f:write(srcheader)
f:write('extern "C" { int luaopen_'..spec.name..'(lua_State *L); }\n\n')
f:write('int luaopen_'..spec.name..'(lua_State *L) {\n')
if spec.cppcode_prebind then f:write(spec.cppcode_prebind) end
f:write(srcbodyheader)
f:write(srcbody)
if spec.cppcode_postbind then f:write(spec.cppcode_postbind) end
f:write('    return 1;\n}\n\n')
f:write(srcfooter)
if spec.cppcode_post then f:write(spec.cppcode_post) end
f:close()
--}}}

