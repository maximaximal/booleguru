set(SRCS
  src/variable_extender.cpp
)

add_library(booleguru-transform STATIC ${SRCS})

target_include_directories(booleguru-transform PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-transform PUBLIC booleguru-expression)
