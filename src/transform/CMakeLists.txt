set(SRCS
  src/variable_extend.cpp
  src/variable_rename.cpp
  src/eliminate_equivalence.cpp
  src/eliminate_implication.cpp
  src/hash_variables.cpp
  src/distribute_nots.cpp
  src/distribute_ors.cpp
  src/prenex_quantifiers.cpp
  src/prenex_quantifiers_optimal.cpp
  src/tseitin.cpp
  src/cnf.cpp
)

add_library(booleguru-transform STATIC ${SRCS})

target_include_directories(booleguru-transform PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-transform PUBLIC booleguru-expression booleguru-util)

target_link_libraries(booleguru-transform PRIVATE fmt)

if(TARGET Catch2::Catch2)
  add_subdirectory(test)
endif()

if(${CMAKE_BUILD_TYPE} MATCHES "Sanitize")
  target_link_libraries(booleguru-transform PRIVATE -fsanitize=address)
endif()
