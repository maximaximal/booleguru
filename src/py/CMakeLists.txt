add_library(booleguru-py-common STATIC
  ${CMAKE_CURRENT_SOURCE_DIR}/src/py_register_booleguru_types.cpp
)

target_include_directories(booleguru-py-common PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(booleguru-py-common booleguru-parse booleguru-serialize booleguru-transform)

add_library(booleguru-py-embed STATIC
  ${CMAKE_CURRENT_SOURCE_DIR}/src/embedded_python.cpp
)

add_library(booleguru-py STATIC
  ${CMAKE_CURRENT_SOURCE_DIR}/src/python_module.cpp
)

target_link_libraries(booleguru-py-embed PUBLIC booleguru-py-common)
target_link_libraries(booleguru-py PUBLIC booleguru-py-common)
