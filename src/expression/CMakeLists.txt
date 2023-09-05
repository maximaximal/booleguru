set(SRCS
  src/op.cpp
  src/op_manager.cpp
  src/var_manager.cpp
  src/bvop_manager.cpp
  src/script_manager.cpp
  src/literals.cpp
  src/quanttree.cpp
)

add_library(booleguru-expression STATIC ${SRCS})

target_include_directories(booleguru-expression PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-expression PUBLIC unordered_dense::unordered_dense booleguru-util)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()
