set(TEST_SRCS
  test-cli-processor.cpp
)

add_executable(booleguru-cli-test ${TEST_SRCS})

target_link_libraries(booleguru-cli-test PRIVATE Catch2::Catch2WithMain)
target_link_libraries(booleguru-cli-test PRIVATE booleguru-cli)

catch_discover_tests(booleguru-cli-test)
