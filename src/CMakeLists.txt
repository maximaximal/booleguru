add_subdirectory(util)

add_subdirectory(expression)
add_subdirectory(transform)
add_subdirectory(solve)
add_subdirectory(lua)

if(NOT EMSCRIPTEN)
  if(pybind11_FOUND)
    add_subdirectory(py)
  endif()
endif()

add_subdirectory(parse)
add_subdirectory(serialize)

if(${CMAKE_BUILD_TYPE} MATCHES "Fuzz")
    add_subdirectory(fuzz)
else()
    if(NOT EMSCRIPTEN)
        add_subdirectory(cli)
    endif()
endif()


if(EMSCRIPTEN)
  add_subdirectory(js-api)
endif()
