set(TEST_SRCS
  test_lua_eval.cpp
  test_fennel_eval.cpp
)

add_executable(booleguru-lua-test ${TEST_SRCS})

target_link_libraries(booleguru-lua-test PRIVATE Catch2::Catch2WithMain)
target_link_libraries(booleguru-lua-test PRIVATE booleguru-lua)

catch_discover_tests(booleguru-lua-test)
