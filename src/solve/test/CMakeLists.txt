set(TEST_SRCS
  test-result.cpp
)

add_executable(booleguru-solve-test ${TEST_SRCS})

target_link_libraries(booleguru-solve-test PRIVATE Catch2::Catch2WithMain)
target_link_libraries(booleguru-solve-test PRIVATE booleguru-solve)

catch_discover_tests(booleguru-solve-test)
