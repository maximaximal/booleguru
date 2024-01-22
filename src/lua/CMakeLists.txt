# Creates C resources file from files in given directory
# https://stackoverflow.com/a/27206982
function(create_resources dir output)
  # Create empty output file
  file(WRITE ${output} "")
  # Collect input files
  file(GLOB bins ${dir}/*)
  # Iterate through input files and compile lua.
  foreach(bin ${bins})
    if(${bin} MATCHES "~$")
      continue()
    endif()
    # Get short filename
    string(REGEX MATCH "([^/]+)$" filename ${bin})
    # Add as dependency for reloading on change
    set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS ${bin})
    # Replace filename spaces & extension separator for C compatibility
    string(REGEX REPLACE "\\.| |-" "_" filename ${filename})
    get_property(using_luajit GLOBAL PROPERTY USING_LUAJIT)
    if(using_luajit AND ${bin} MATCHES ".fnl$")
        message(STATUS "Convert fnl script ${bin} to lua and then to bytecode prior to embedding using LuaJIT")
        string(REGEX REPLACE "_fnl$" "_lua" filename ${filename})
        execute_process(COMMAND bash -c "luajit -O9 ${CMAKE_CURRENT_SOURCE_DIR}/bin/fennel --compile | luajit -b /dev/stdin ${CMAKE_CURRENT_BINARY_DIR}/booleguru-luajit-bytecode.bin")
        file(READ ${CMAKE_CURRENT_BINARY_DIR}/booleguru-luajit-bytecode.bin filedata HEX)
        message(STATUS "Change require-name of fennel file ${bin} to ${filename} because the file was converted to Lua")
    elseif(using_luajit AND ${bin} MATCHES ".lua$")
        message(STATUS "Convert lua script ${bin} to bytecode prior to embedding using LuaJIT")
        execute_process(COMMAND luajit -O9 -b ${bin} ${CMAKE_CURRENT_BINARY_DIR}/booleguru-luajit-bytecode.bin)
        file(READ ${CMAKE_CURRENT_BINARY_DIR}/booleguru-luajit-bytecode.bin filedata HEX)
    else()
        file(READ ${bin} filedata HEX)
    endif()
    # Read hex data from file
    # Convert hex data for C compatibility
    string(REGEX REPLACE "([0-9a-f][0-9a-f])" "0x\\1," filedata ${filedata})
    # Append data to output file
    file(APPEND ${output} "const char embedded_script_${filename}[] = {${filedata}}; const unsigned int embedded_script_${filename}_size = sizeof(embedded_script_${filename});\n")

    set(EMBEDDED_SCRIPTS_EXTERNS "${EMBEDDED_SCRIPTS_EXTERNS}\nextern const char embedded_script_${filename}[]; extern const unsigned int embedded_script_${filename}_size;")
    set(EMBEDDED_SCRIPTS_EXTERNS "${EMBEDDED_SCRIPTS_EXTERNS}" PARENT_SCOPE)

    set(EMBEDDED_SCRIPTS "embedded_script{\"${filename}\", std::string_view(&embedded_script_${filename}[0], embedded_script_${filename}_size)}, ${EMBEDDED_SCRIPTS}")
    set(EMBEDDED_SCRIPTS "${EMBEDDED_SCRIPTS}" PARENT_SCOPE)
  endforeach()
endfunction()


set(SRCS
  src/lua-context.cpp
  src/lua-context-register-types.cpp
  src/binding-helpers.cpp
)

add_library(booleguru-lua STATIC ${SRCS})

target_include_directories(booleguru-lua PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

if(CMAKE_BUILD_TYPE MATCHES "Release" OR CMAKE_BUILD_TYPE MATCHES "RelWithDbgInfo")
  create_resources("${CMAKE_CURRENT_SOURCE_DIR}/scripts" "${CMAKE_CURRENT_BINARY_DIR}/scripts.c")
  file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/scripts.h" "#include <string_view>\n${EMBEDDED_SCRIPTS_EXTERNS}\nstruct embedded_script { std::string_view name; std::string_view data; };\nconst embedded_script embedded_scripts[] = {${EMBEDDED_SCRIPTS}};\n")
  add_library(booleguru-lua-scripts OBJECT
    ${CMAKE_CURRENT_BINARY_DIR}/scripts.c
  )
  target_link_libraries(booleguru-lua PRIVATE $<TARGET_OBJECTS:booleguru-lua-scripts>)
  target_include_directories(booleguru-lua PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
else()
  target_compile_definitions(booleguru-lua PRIVATE BOOLEGURU_LUA_AUTOSTART_DIR="${CMAKE_CURRENT_SOURCE_DIR}/scripts")
endif()

target_link_libraries(booleguru-lua PUBLIC
  booleguru-expression booleguru-transform booleguru-util booleguru-solve)
target_link_libraries(booleguru-lua PUBLIC lua lua-header sol2 ${CMAKE_DL_LIBS})
target_link_libraries(booleguru-lua PRIVATE fmt)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()

if(${CMAKE_BUILD_TYPE} MATCHES "Sanitize")
  target_link_libraries(booleguru-lua PRIVATE -fsanitize=address)
endif()
