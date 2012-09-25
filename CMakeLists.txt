#  Copyright (c) 2008 Peter Kuemmel 
#  Copyright (c) 2008 Mauro Iazzi
#  Copyright (c) 2011 Michal Kottman
#  Copyright (c) 2012 Michael Kirsch
#  
#  Permission is hereby granted, free of charge, to any person
#  obtaining a copy of this software and associated documentation
#  files (the "Software"), to deal in the Software without
#  restriction, including without limitation the rights to use,
#  copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the
#  Software is furnished to do so, subject to the following
#  conditions:
#  
#  The above copyright notice and this permission notice shall be
#  included in all copies or substantial portions of the Software.
#  
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
#  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
#  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
#  OTHER DEALINGS IN THE SOFTWARE.

cmake_minimum_required(VERSION 2.6)

project(lqt)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/bin) 
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/lib) 
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/lib) 

find_package(Qt4 REQUIRED)
set(QT_USE_QTCORE 1)
set(QT_USE_QTGUI 1)
set(QT_USE_QTNETWORK 1)
set(QT_USE_QTWEBKIT 1)
set(QT_USE_QTOPENGL 1)
set(QT_USE_QTSCRIPT 1)
set(QT_USE_QTSVG 1)
set(QT_USE_QTUITOOLS 1) 
set(QT_USE_QTXML 1)
set(QT_USE_QTSQL 1)
set(QT_USE_QTXMLPATTERNS 1)
include(${QT_USE_FILE})

if(noqt)
	set(QT_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/test)
	#set(nomerge TRUE)
endif()

find_package(Lua51 REQUIRED)
find_program(LUA_CMD NAMES lua lua5.1 lua51) # for some reason LuaJIT 2.0.0-beta10 segfaults with some blacklist configurations

if(WIN32)
	if(MSVC)
		set(CMAKE_SUPPRESS_REGENERATION TRUE)
		add_definitions(-D_CRT_SECURE_NO_DEPRECATE -D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_WARNINGS -DLUA_BUILD_AS_DLL /bigobj)
		set(CMAKE_DEBUG_POSTFIX)
	endif()
	if(MINGW)
		set(CMAKE_SHARED_LINKER_FLAGS "-Wl,-add-stdcall-alias")
	endif()
	set(LUA_LIBS_FOR_MODULE ${LUA_LIBRARIES})
else(WIN32)
	set(LUA_LIBS_FOR_MODULE )
endif(WIN32)

if(APPLE)
   set(CMAKE_SHARED_MODULE_CREATE_CXX_FLAGS
       "${CMAKE_SHARED_MODULE_CREATE_CXX_FLAGS} -flat_namespace -undefined dynamic_lookup")
endif(APPLE)

add_subdirectory(cpptoxml)

function(build_qt_lib_binding BINDING_HEADER)

    if(UNIX)
        set (PP_CONFIG ${CMAKE_SOURCE_DIR}/cpptoxml/parser/rpp/pp-qt-configuration)
    else(UNIX)
        set (PP_CONFIG ${CMAKE_SOURCE_DIR}/cpptoxml/parser/rpp/pp-qt-configuration-win)
    endif(UNIX)

    set(BINDING_NAME ${BINDING_HEADER})

	set(MODULE_WORK_DIR ${CMAKE_BINARY_DIR}/${BINDING_NAME}_tmp)
	file(MAKE_DIRECTORY ${MODULE_WORK_DIR})

    set (GEN_XML ${MODULE_WORK_DIR}/xml.xml)
    set (GEN_BINDSPEC ${MODULE_WORK_DIR}/bindspec.lua)
	set (GEN_SRC ${MODULE_WORK_DIR}/module.cpp)
	set_source_files_properties(${GENERATED_MODULE_SRC} PROPERTIES  GENERATED TRUE)

    add_custom_command(OUTPUT ${GEN_XML}
                      COMMAND cpptoxml
                          ${BINDING_HEADER}
                          -C ${PP_CONFIG}
                          -Q ${QT_INCLUDE_DIR} 
                          -o ${GEN_XML}
                      COMMENT "Generating XML: running cpptoxml on ${BINDING_NAME}"
                      VERBATIM)

    add_custom_command(OUTPUT ${GEN_BINDSPEC}
                       MAIN_DEPENDENCY "${CMAKE_SOURCE_DIR}/generator/xml2bindspec.lua"
                       DEPENDS ${GEN_XML}
                               "${CMAKE_SOURCE_DIR}/generator/templates.lua"
                               "${CMAKE_SOURCE_DIR}/generator/blacklist.lua"
                       COMMAND ${LUA_CMD}
                           "${CMAKE_SOURCE_DIR}/generator/xml2bindspec.lua" 
                           --module=${BINDING_NAME}
                           --in=${GEN_XML}
                           --out=${GEN_BINDSPEC}
                           --ignore=${CMAKE_BINARY_DIR}/${BINDING_NAME}-ignore.txt
                           --header=${BINDING_HEADER}
                       COMMENT "Generating bindspec file from XML"
                       VERBATIM)

    add_custom_command(OUTPUT ${GEN_SRC}
                       MAIN_DEPENDENCY "${CMAKE_SOURCE_DIR}/generator/bindgen.lua"
                       DEPENDS ${GEN_BINDSPEC}
                       COMMAND ${LUA_CMD}
                           "${CMAKE_SOURCE_DIR}/generator/bindgen.lua" 
                           ${GEN_BINDSPEC} 
                           ${GEN_SRC} 
                       COMMENT "Generating binding code from bindspec"
                       VERBATIM)

	set(COMMON COMMON_${BINDING_NAME})
	file(GLOB ${COMMON}_SRC ${CMAKE_SOURCE_DIR}/common/*.cpp)
	file(GLOB ${COMMON}_HPP ${CMAKE_SOURCE_DIR}/common/*.hpp)

	qt4_wrap_cpp(MOC_GEN_SRC ${GEN_SRC})
	
	include_directories(${CMAKE_SOURCE_DIR}/common ${LUA_INCLUDE_DIR} ${QT_INCLUDES})

	add_library(${BINDING_NAME} MODULE ${GEN_SRC} ${MOC_GEN_SRC})
											
	target_link_libraries(${BINDING_NAME} ${QT_LIBRARIES} ${LUA_LIBS_FOR_MODULE})
	set_target_properties(${BINDING_NAME} PROPERTIES PREFIX "" COMPILE_DEFINITIONS "MODULE_${BINDING_NAME}")

	if(LQT_BUILDIN_CPPTOXML)
		add_dependencies(generate_${BINDING_NAME}_xml  cpptoxml)
	endif()
endfunction()

build_qt_lib_binding(QtCore)

#if(noqt)
	#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/test/noqt)
	#build_qt_lib_binding(noqt)
#else()
	#build_qt_lib_binding(QtCore)
	#build_qt_lib_binding(QtNetwork QtCore)
	#build_qt_lib_binding(QtGui QtNetwork QtCore)
	#build_qt_lib_binding(QtScript QtGui QtNetwork QtCore)
	#build_qt_lib_binding(QtOpenGL QtGui QtNetwork QtCore)
	#build_qt_lib_binding(QtSvg QtScript QtGui QtNetwork QtCore)
	#build_qt_lib_binding(QtXml QtSvg QtScript QtGui QtNetwork QtCore)
	#build_qt_lib_binding(QtXmlPatterns QtCore QtXml)
	#build_qt_lib_binding(QtUiTools QtCore QtGui QtXml) 
	#build_qt_lib_binding(QtWebKit QtXmlPatterns QtSvg QtScript QtOpenGL QtGui QtNetwork QtCore QtXml)
	#build_qt_lib_binding(QtSql QtCore)
	## TODO
	##build_qt_lib_binding(QtScriptTools) # 4.5 only
	##build_qt_lib_binding(Phonon)
#endif()
