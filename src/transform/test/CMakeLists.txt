set(TEST_SRCS
  test_prenex_quantifier.cpp
)

add_executable(booleguru-transform-test ${TEST_SRCS})

target_link_libraries(booleguru-transform-test PRIVATE Catch2::Catch2WithMain)
target_link_libraries(booleguru-transform-test PRIVATE booleguru-transform)

catch_discover_tests(booleguru-transform-test)
