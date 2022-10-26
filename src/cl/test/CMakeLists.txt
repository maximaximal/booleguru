set(TEST_SRCS
  test-ecl-wrapper.cpp
)

add_executable(booleguru-cl-test ${TEST_SRCS})

target_link_libraries(booleguru-cl-test PRIVATE Catch2::Catch2WithMain)
target_link_libraries(booleguru-cl-test PRIVATE booleguru-cl)

catch_discover_tests(booleguru-cl-test)
