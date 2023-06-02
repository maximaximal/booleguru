add_subdirectory(util)

add_subdirectory(expression)
add_subdirectory(transform)
add_subdirectory(solve)
add_subdirectory(lua)
add_subdirectory(parse)
add_subdirectory(serialize)

if(NOT EMSCRIPTEN)
  if(pybind11_FOUND)
    add_subdirectory(py)
  endif()

  add_subdirectory(cli)
endif()

if(EMSCRIPTEN)
  add_subdirectory(js-api)
endif()
