#!/usr/bin/env lua

--[[

Copyright (c) 2007-2009 Mauro Iazzi
Copyright (c)      2008 Peter K�mmel
Copyright (c)      2010 Michal Kottman
Copyright (c)      2012 Michael Kirsch

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

-- add the dir that this file is in to package.path
package.path = arg[0]:gsub('[^/\\]+%.lua$', '')..'/?.lua;'..package.path

-- parse command line args
local modulename, infile, outfile, ignorefile--{{{
local headers = {}
local cmdlineargs = {
    ['in']     = function (f) infile = f end,
    ['out']    = function (f) outfile = f end,
    ['ignore'] = function (f) ignorefile = f end,
    ['module'] = function (m) modulename = m end,
    ['header'] = function (h) table.insert(headers, '#include <'..h..'>') end,
}
for _, i in ipairs(arg) do
    local match
    for pattern, func in pairs(cmdlineargs) do
        match = i:match('^%-%-'..pattern..'=(.+)')
        if match then
            func(match)
            break
        end
    end
    if not match then error('invalid command line arg: '..i) end
end
assert(modulename, 'module name not specified')
assert(infile, 'input XML file not specified')
assert(outfile, 'output spec filename not specified')--}}}

-- set up the file that contains a list of ignored itend and the reasons
ignorefile = ignorefile and assert(io.open(ignorefile, 'w'))--{{{
function ignore(name, cause)
    if ignorefile then ignorefile:write(('%s %s\n'):format(name, cause)) end
end--}}}

-- iterate over a string of comma separated C++ type names (ignores commas in nested <> and () pairs)
local function comma_separated_type_iterator(str)--{{{
    local cur = 1
    return function ()
        local start = cur
        while true do
            local match = str:match('^%b<>()', start) or str:match('%b()()', start) or str:match('^[^,<>]()', start)
            if match then
                start = match
            else
                local substr = str:sub(cur, start-1)
                cur = start
                cur = str:match(',+()', cur) or cur
                return #substr~=0 and substr or nil
            end
        end
    end
end--}}}

-- parse the XML file. xmlstream is the tree, idindex is a set of all the
-- elements
local xmlstream, idindex--{{{
do
    local parsexml = require 'xml'
    local xmlfile = assert(io.open(infile))
    local xml = xmlfile:read('*a')
    xmlfile:close()
    xmlstream, idindex = parsexml(xml)
end--}}}

-- Remove duplicate entries (~4300/20000 for QtCore)
do--{{{
    local dups = {}
    local remove = {}
    for e in pairs(idindex) do
        if e.xarg and e.xarg.id and dups[e.xarg.id] then
            remove[e] = true
        end
        dups[e.xarg.id] = true
    end
    for e in pairs(remove) do
        idindex[e] = nil
    end
end--}}}

-- create a id->element table
local elementsbyid = {}--{{{
for elem in pairs(idindex) do
    elementsbyid[elem.xarg.id] = elem
end--}}}

-- get the tamplate substitutions list
local templates = require 'templates'

-- get the blacklist and convert it from an array to a set
local blacklist = require 'blacklist'--{{{
for i = 1, #blacklist do
    blacklist[blacklist[i]] = true
    blacklist[i] = nil
end--}}}

-- convert the XML tree to the spec table
local spec = {[0]='module', name=modulename, typealiases=''}--{{{
local rootspec = spec
-- set of functions already seen, if thy're seen again it's an overload and
-- not another function
local overloadedfuncs = {}
local function element2spec(elem, parentspec)
    if elem.xarg.fullname and blacklist[elem.xarg.fullname] then ignore(elem.xarg.fullname, 'blacklisted') return nil end
    local spec
    if elem.label == 'Class' then
        --{{{
        if elem.xarg.access == 'private' then
            return nil
        end
        spec = {
            [0] = 'class',
            name = elem.xarg.name,
            cppname = elem.xarg.fullname,
        }
        for id in elem.xarg.members:gmatch('[^ ]+') do
            table.insert(spec, element2spec(elementsbyid[id], spec))
        end
        -- get the base class
        if elem.xarg.bases_with_attributes then
            for i in elem.xarg.bases_with_attributes:gmatch('[^;]+') do
                local match = i:match('public (.+)')
                if match then
                    if spec.base then
                        ignore(elem.xarg.fullname, 'multiple inheritance not supported')
                        return nil
                    else
                        spec.base = match
                    end
                end
            end
        end
        --}}}
    elseif elem.label == 'Namespace' then
        --{{{
        if elem.xarg.access == 'private' then
            return nil
        end
        spec = {
            [0] = 'namespace',
            name = elem.xarg.name,
        }
        for id in elem.xarg.members:gmatch('[^ ]+') do
            table.insert(spec, element2spec(elementsbyid[id], spec))
        end
        --}}}
    elseif elem.label == 'FunctionDefinition' or elem.label == 'Function' then
        --{{{
        if elem.xarg.access == 'private' or elem.xarg.name:match('~') then
            if elem.xarg.member_of_class
               and elem.xarg.fullname == elem.xarg.member_of_class..'::'..elem.xarg.member_of_class
               and #elem == 1
               and elem[1].xarg.type_base == elem.xarg.member_of_class
               and elem[1].xarg.type_name:match('&') then
                parentspec.nocopy = true
            end
            return nil
        end
        local ismethod = elem.xarg.member_of_class and elem.xarg.static ~= '1'
        if ismethod and elem.xarg.abstract=='1' then parentspec.ctor = false end
        if elem.xarg.variadics == '1' then
            ignore(elem.xarg.fullname, 'variadic')
            return nil
        end
        if elem.xarg.name:match('^operator') then -- TODO operator support
            ignore(elem.xarg.fullname, 'operator')
            return nil
        end
        if elem.xarg.type_name:match('int%s*%&') then -- FIXME catch all num types
            ignore(elem.xarg.fullname, 'returns number reference')
            return nil
        end
        if elem.xarg.context == '' then -- FIXME a temporary workaround to avoid binding non-Qt functions
            ignore(elem.xarg.fullname, 'no context')
            return nil
        end
        if blacklist[elem.xarg.type_base] then
            ignore(elem.xarg.fullname, 'blacklisted return type')
            return nil
        end
        for _, arg in ipairs(elem) do
            if arg.label == 'Argument' then
                if arg.xarg.type_name:match('%b()') then
                    ignore(elem.xarg.fullname, 'function pointer argument')
                    return nil
                end
                if blacklist[arg.xarg.type_base] then
                    ignore(elem.xarg.fullname, 'blacklisted arg type')
                    return nil
                end
            end
        end
        if overloadedfuncs[elem.xarg.fullname] then
            local argtbl = {}
            for _, arg in ipairs(elem) do
                if arg.label == 'Argument' then
                    table.insert(argtbl, arg.xarg.type_name)
                end
            end
            if ismethod and elem.xarg.fullname == elem.xarg.member_of_class..'::'..elem.xarg.member_of_class:gsub('.*::', ''):gsub('%b<>', '') then
                if parentspec.ctor ~= false then
                    parentspec.ctor = parentspec.ctor and parentspec.ctor..' | ' or ''
                    parentspec.ctor = parentspec.ctor..'('..table.concat(argtbl, ',')..')'
                end
                return nil
            end
            local f = overloadedfuncs[elem.xarg.fullname]
            f.sig = f.sig..(' | %s %s%s(%s)'):format(elem.xarg.type_name,
                                                     ismethod and '@' or (parentspec[0]=='class' and '#' or ''),
                                                     elem.xarg.fullname,
                                                     table.concat(argtbl, ','))
            return nil
        else
            spec = {
                [0] = 'function',
                sig = elem.xarg.type_name..' '..(ismethod and '@' or (parentspec[0]=='class' and '#' or ''))..elem.xarg.fullname..'(',
            }
            local first = true
            for _, arg in ipairs(elem) do
                if arg.label == 'Argument' then
                    if first then first = false else spec.sig = spec.sig..',' end
                    spec.sig = spec.sig..arg.xarg.type_name
                end
            end
            spec.sig = spec.sig..')'
            overloadedfuncs[elem.xarg.fullname] = spec
            if ismethod and elem.xarg.fullname == elem.xarg.member_of_class..'::'..elem.xarg.member_of_class:gsub('.*::', ''):gsub('%b<>', '') then
                if parentspec.ctor ~= false then
                    parentspec.ctor = parentspec.ctor and parentspec.ctor..' | ' or ''
                    parentspec.ctor = parentspec.ctor..spec.sig:match('%(.*%)')
                end
                return nil
            end
        end
        if elem.xarg.abstract then return nil end
        --}}}
    elseif elem.label == 'Enum' then
--{{{
        if elem.xarg.access == 'private' then
            return nil
        end
        spec = {
            [0] = 'enum',
            name = elem.xarg.name,
            cppname = elem.xarg.fullname,
            values = '',
        }
        for _, arg in ipairs(elem) do
            if arg.label == 'Enumerator' then
                spec.values = spec.values..arg.xarg.name..' = '..arg.xarg.value..'\n'
            end
        end
    --}}}
    elseif elem.label == 'Variable' then
        --{{{
        return nil
        --}}}
    elseif elem.label == 'TypeAlias' then
        rootspec.typealiases = rootspec.typealiases..elem.xarg.fullname..' '..elem.xarg.type_name..'\n'--{{{
        return nil--}}}
    end
    
    assert(spec, 'unknown node type: '..elem.label)
    
    -- process templates
    if spec.name and spec.cppname and spec.cppname:match('>$') then--{{{
        local subs = templates[spec.cppname]
        if not subs then return nil end
        local args, subtable = {}, {}
        for i in comma_separated_type_iterator(spec.name:match('%b<>$'):sub(2, -2)) do args[#args+1] = i end
        for _, i in ipairs(subs) do
            local tbl, index = {}, 1
            for j in comma_separated_type_iterator(i) do
                tbl[args[index]] = j
                index = index + 1
            end
            table.insert(subtable, tbl)
        end
        local newspec = {
            [0] = 'template',
            substitutions = subtable,
            spec,
        }
        spec = newspec
    end--}}}
    
    return spec
end
for id in xmlstream[1].xarg.members:gmatch('[^ ]+') do
    table.insert(spec, element2spec(elementsbyid[id], spec))
end--}}}

local function fixfunctions(spec)--{{{
    for k, v in ipairs(spec) do
        if v[0] == 'function' then
            spec[k] = v.sig
        else
            fixfunctions(v)
        end
    end
end
fixfunctions(spec)--}}}

-- mark subclasses of abstract classes as abstract, and mark non-copyable classes
local abstractclasses = {}--{{{
local function getabstractclasses(spec)
    if spec[0] == 'class' then
        if spec.ctor == false then
            abstractclasses[spec.cppname] = true
        elseif spec.ctor then
            --[[local copyable = false
            for k, v in ipairs(spec.ctors) do
                if v:match('[^,]*'..spec.cppname:gsub('.*::', '')..'[^,]*') then
                    copyable = true
                    break
                end
            end
            if not copyable then spec.nocopy = true end]]
        end
    end
    for k, v in ipairs(spec) do
        if type(v) == 'table' then
            getabstractclasses(v)
        end
    end
end
getabstractclasses(spec)
repeat
    local found = false
    local function fixabstractclasses(spec)
        if spec[0] == 'class' and spec.ctor and spec.base and abstractclasses[spec.base] then
            spec.ctor = false
            abstractclasses[spec.cppname] = true
            found = true
        end
        for k, v in ipairs(spec) do
            if type(v) == 'table' then
                fixabstractclasses(v)
            end
        end
    end
    fixabstractclasses(spec)
until not found--}}}

-- include the headers
spec.cppcode_pre = table.concat(headers, '\n')..'\n'

local specfile = assert(io.open(outfile, 'w'))

-- convert a table to Lua source code
local function tabletofile(val, file, indentation)--{{{
    local valtype = type(val)
    if valtype == 'boolean' then
        file:write(val and 'true' or 'false')
    elseif valtype == 'number' then
        file:write(tostring(val))
    elseif valtype == 'string' then
        if val:match('\n') then
            indentation = indentation or ''
            local subindent = indentation..'  '
            file:write('[=[\n')
            for line in val:gmatch('[^\n]+') do
                file:write(subindent..line..'\n')
            end
            file:write(indentation..']=]')
        else
            file:write(('%q'):format(val))
        end
    elseif valtype == 'table' then
        indentation = indentation or ''
        local subindent = indentation..'  '
        file:write('{\n')
        if val[0] then
            file:write(subindent..'[')
            tabletofile(0, file, subindent)
            file:write('] = ')
            tabletofile(val[0], file, subindent)
            file:write(',\n')
        end
        for k, v in pairs(val) do
            if type(k) ~= 'number' then
                if type(k) == 'string' and k:match('^[%w_]+$') then
                    file:write(subindent..k..' = ')
                else
                    file:write(subindent..'[')
                    tabletofile(k, file, subindent)
                    file:write('] = ')
                end
                tabletofile(v, file, subindent)
                file:write(',\n')
            end
        end
        for k, v in ipairs(val) do
            file:write(subindent)
            tabletofile(v, file, subindent)
            file:write(',\n')
        end
        file:write(indentation..'}')
    else
        error('cannot serialize '..valtype..' value')
    end
end--}}}

specfile:write('return ')
tabletofile(spec, specfile)
specfile:close()

