set(SRCS
  src/source.cpp
  src/fuzz_entry_point.cpp
)

add_executable(booleguru-fuzz ${SRCS})


target_include_directories(booleguru-fuzz PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_include_directories(booleguru-fuzz PRIVATE ${ANTLR4_INCLUDE_DIR})

target_link_libraries(booleguru-fuzz PUBLIC fmt::fmt antlr4_static booleguru-lua booleguru-parse)
target_link_libraries(booleguru-fuzz PUBLIC -fsanitize=fuzzer)

add_executable(booleguru-print-corpus src/fuzz_decode_corpus_to_ops.cpp src/source.cpp)
target_link_libraries(booleguru-print-corpus PUBLIC fmt::fmt antlr4_static booleguru-lua booleguru-parse)
target_include_directories(booleguru-print-corpus PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
