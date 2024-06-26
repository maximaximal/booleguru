add_library(booleguru-py-common OBJECT
  ${CMAKE_CURRENT_SOURCE_DIR}/src/solver.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/model.cpp
)

target_include_directories(booleguru-py-common PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(booleguru-py-common PUBLIC
  booleguru-serialize
  booleguru-transform
  booleguru-solve
  booleguru-lua
)

target_link_libraries(booleguru-py-common PRIVATE pybind11::pybind11)

add_library(booleguru-py-embed STATIC
  ${CMAKE_CURRENT_SOURCE_DIR}/src/python-context.cpp
)

set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR})

pybind11_add_module(pybooleguru SHARED
  ${CMAKE_CURRENT_SOURCE_DIR}/src/python_module.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/py_register_booleguru_types.cpp
)

target_link_libraries(booleguru-py-embed PUBLIC booleguru-py-common)
target_link_libraries(pybooleguru PUBLIC booleguru-py-common)

target_link_libraries(booleguru-py-embed PRIVATE pybind11::embed)
target_link_libraries(pybooleguru PRIVATE pybind11::module)

set_property(TARGET pybooleguru PROPERTY ENABLE_EXPORTS ON)
set_property(TARGET booleguru-py-embed PROPERTY ENABLE_EXPORTS ON)
