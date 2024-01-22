add_subdirectory(ankerl)
add_subdirectory(antlr)
add_subdirectory(fmt)

find_package(ANTLR)

set(ANTLR4_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/antlr/runtime/src/" PARENT_SCOPE)
target_include_directories(antlr4_static PRIVATE ${ANTLR4_INCLUDE_DIR})

find_package(LuaJIT)
if(LUAJIT_FOUND)
    add_library(lua STATIC IMPORTED GLOBAL)
    add_library(lua-header INTERFACE)
    set_property(TARGET lua PROPERTY IMPORTED_LOCATION ${LUA_LIBRARY})
    set_property(TARGET lua-header PROPERTY INTERFACE_INCLUDE_DIRECTORIES ${LUA_INCLUDE_DIR})
    target_link_libraries(lua-header INTERFACE lua)
    set_property(GLOBAL PROPERTY USING_LUAJIT ON)
else()
    message(STATUS "Using source-distributed Lua version instead of system. Try to install libluajit-dev.")
    add_subdirectory(lua)
    set_property(GLOBAL PROPERTY USING_LUAJIT OFF)
endif()

add_subdirectory(sol)
