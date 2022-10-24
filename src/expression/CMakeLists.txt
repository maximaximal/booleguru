set(SRCS
  src/expression.cpp)

add_library(booleguru-expression STATIC ${SRCS})

target_include_directories(booleguru-expression PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
