set(TEST_SRCS
  test_serialize_qcir.cpp
)

add_executable(booleguru-serialize-test ${TEST_SRCS})

target_link_libraries(booleguru-serialize-test PRIVATE Catch2::Catch2WithMain)
target_link_libraries(booleguru-serialize-test PRIVATE booleguru-serialize)

catch_discover_tests(booleguru-serialize-test)
