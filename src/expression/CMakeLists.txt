set(SRCS
  src/op.cpp
  src/op_manager.cpp
  src/var_manager.cpp
  src/script_manager.cpp
)

add_library(booleguru-expression OBJECT ${SRCS})

set_property(TARGET booleguru-expression PROPERTY POSITION_INDEPENDENT_CODE ON)

target_include_directories(booleguru-expression PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()
