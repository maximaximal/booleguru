set(SRCS
  src/util.cpp
)

add_library(booleguru-util STATIC ${SRCS})

target_include_directories(booleguru-util PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-util fmt::fmt)
