set(SRCS
  src/id.cpp
  src/op.cpp
  src/op_manager.cpp
  src/var_manager.cpp
  src/bvop_manager.cpp
  src/literals.cpp
  src/quanttree.cpp
  src/mutator.cpp
  src/mutation.cpp
  src/mutation_sampler.cpp
)

add_library(booleguru-expression STATIC ${SRCS})

target_include_directories(booleguru-expression PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-expression PUBLIC unordered_dense::unordered_dense booleguru-util)

set_property(TARGET booleguru-expression PROPERTY ENABLE_EXPORTS ON)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()
