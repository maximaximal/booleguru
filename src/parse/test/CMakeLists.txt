set(TEST_SRCS
  test-parse-boole.cpp
)

add_executable(booleguru-parse-test ${TEST_SRCS})

target_link_libraries(booleguru-parse-test PRIVATE Catch2::Catch2WithMain)
target_link_libraries(booleguru-parse-test PRIVATE booleguru-parse)

catch_discover_tests(booleguru-parse-test)
