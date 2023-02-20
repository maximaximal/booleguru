# Creates C resources file from files in given directory
# https://stackoverflow.com/a/27206982
function(create_resources dir output)
  # Create empty output file
  file(WRITE ${output} "")
  # Collect input files
  file(GLOB bins ${dir}/*)
  # Iterate through input files
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
    # Read hex data from file
    file(READ ${bin} filedata HEX)
    # Convert hex data for C compatibility
    string(REGEX REPLACE "([0-9a-f][0-9a-f])" "0x\\1," filedata ${filedata})
    # Append data to output file
    file(APPEND ${output} "const unsigned char ${filename}[] = {${filedata}};\nconst unsigned ${filename}_size = sizeof(${filename});\n")
  endforeach()
endfunction()


set(SRCS
  src/lua-context.cpp
  src/lua-context-register-types.cpp
)

add_library(booleguru-lua STATIC ${SRCS})

target_include_directories(booleguru-lua PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

if(${CMAKE_BUILD_TYPE} MATCHES "Release" OR ${CMAKE_BUILD_TYPE} MATCHES "RelWithDebugInfo")
  create_resources("${CMAKE_CURRENT_SOURCE_DIR}/scripts" "${CMAKE_CURRENT_BINARY_DIR}/scripts.c")
  add_library(booleguru-lua-scripts OBJECT
    ${CMAKE_CURRENT_BINARY_DIR}/scripts.c
  )
  target_link_libraries(booleguru-lua $<TARGET_OBJECTS:booleguru-lua-scripts>)
else()
  target_compile_definitions(booleguru-lua PRIVATE BOOLEGURU_LUA_AUTOSTART_DIR="${CMAKE_CURRENT_SOURCE_DIR}/scripts")
endif()

target_link_libraries(booleguru-lua PUBLIC
  booleguru-expression booleguru-transform booleguru-util booleguru-solve)
target_link_libraries(booleguru-lua PUBLIC lua lua-header sol2 ${CMAKE_DL_LIBS})

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()

if(${CMAKE_BUILD_TYPE} MATCHES "Sanitize")
  target_link_libraries(booleguru-lua PRIVATE -fsanitize=address)
endif()
