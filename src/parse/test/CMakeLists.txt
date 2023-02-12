set(TEST_SRCS
  test-parse-boole.cpp
  test-sexp-tracker.cpp
)

add_executable(booleguru-parse-test ${TEST_SRCS})

target_link_libraries(booleguru-parse-test PRIVATE Catch2::Catch2WithMain)
target_link_libraries(booleguru-parse-test PRIVATE booleguru-parse)
target_link_libraries(booleguru-parse-test PRIVATE antlr4_static)

catch_discover_tests(booleguru-parse-test)
