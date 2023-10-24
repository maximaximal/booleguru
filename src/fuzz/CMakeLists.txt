set(SRCS
  src/source.cpp
  src/fuzz_entry_point.cpp
)

add_executable(booleguru-fuzz ${SRCS})

target_include_directories(booleguru-fuzz PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_link_libraries(booleguru-fuzz PUBLIC fmt::fmt booleguru-lua booleguru-parse)
