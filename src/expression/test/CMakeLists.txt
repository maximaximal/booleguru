set(TEST_SRCS
  test-expression-and-inside.cpp
  test-user-defined-literals.cpp
)

add_executable(booleguru-expression-test ${TEST_SRCS})

target_link_libraries(booleguru-expression-test PRIVATE Catch2::Catch2WithMain)
target_link_libraries(booleguru-expression-test PRIVATE booleguru-expression)

catch_discover_tests(booleguru-expression-test)

if(${CMAKE_BUILD_TYPE} MATCHES "Sanitize")
  target_link_libraries(booleguru-expression-test PRIVATE -fsanitize=address)
endif()
