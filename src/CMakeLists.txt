add_subdirectory(util)

add_subdirectory(expression)
add_subdirectory(transform)
add_subdirectory(cl)
add_subdirectory(parse)
add_subdirectory(serialize)

if(NOT EMSCRIPTEN)
  add_subdirectory(cli)
endif()

if(EMSCRIPTEN)
  add_subdirectory(js-api)
endif()
