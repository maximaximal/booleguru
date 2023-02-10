# Creates C resources file from files in given directory
# https://stackoverflow.com/a/27206982
function(create_resources dir output)
    # Create empty output file
    file(WRITE ${output} "")
    # Collect input files
    file(GLOB bins ${dir}/*)
    # Iterate through input files
    foreach(bin ${bins})
        # Get short filename
        string(REGEX MATCH "([^/]+)$" filename ${bin})
        # Replace filename spaces & extension separator for C compatibility
        string(REGEX REPLACE "\\.| |-" "_" filename ${filename})
        # Read hex data from file
        file(READ ${bin} filedata HEX)
        # Convert hex data for C compatibility
        string(REGEX REPLACE "([0-9a-f][0-9a-f])" "0x\\1," filedata ${filedata})
        # Append data to output file
        file(APPEND ${output} "const unsigned char ${filename}[] = {${filedata}};\nconst unsigned ${filename}_size = sizeof(${filename});\n")
    endforeach()
  endfunction()

create_resources("${CMAKE_CURRENT_SOURCE_DIR}/scripts" "${CMAKE_CURRENT_BINARY_DIR}/scripts.c")

set(SRCS
  src/lua-context.cpp
  src/lua-context-register-types.cpp

  # Embedded Scripts (Fennel, Lua Libraries, etc)
  ${CMAKE_CURRENT_BINARY_DIR}/scripts.c
)

add_library(booleguru-lua STATIC ${SRCS})

target_include_directories(booleguru-lua PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-lua PUBLIC
  booleguru-expression booleguru-transform booleguru-util booleguru-solve)
target_link_libraries(booleguru-lua PUBLIC lua lua-header sol2)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()

if(${CMAKE_BUILD_TYPE} MATCHES "Sanitize")
  target_link_libraries(booleguru-lua PRIVATE -fsanitize=address)
endif()
